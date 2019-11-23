       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stbuono.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "rordini.sl".
           copy "tivaese.sl".
           copy "articoli.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tivaese.fd". 
           copy "articoli.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "splcrt2graf.lks".

       01  como-chiave.
         05 como-anno            pic 9(4).
         05 como-numero          pic 9(8).
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-lineseq        pic xx.
       77  status-tivaese        pic xx.
       77  status-articoli       pic xx.
       77  como-imposta          pic 9(10)v999.
       77  como-imposta-2dec     pic 9(10)v99.
       77  wstampa               pic x(256).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  prezzo                pic 9(10)v999.
       77  tot-importo           pic 9(10)v999.
       77  scelta                pic 9.
       77  tor-numero-edit       pic z(8).
       77  riga                  pic 9(3).
       77  righe                 pic 9(3).
       
       78  titolo                value "Stampa Buono di carico".
      *    luciano
      * 78  MaxRighe              value 61.
       78  MaxRighe              value 60.
      * 
       01 file-info.
           05 file-size   pic  x(8) comp-x.
           05 file-date   pic  9(8) comp-x.
           05 file-time   pic  9(8) comp-x.
                    
       01 operazione      pic  x.
           88 antep       value is "a".
           88 stampa      value is "s".
       77  filler                pic 9.
           88  TestataGiaFatta   value 1, false 0.
       77  filler                pic 9.
           88  CopiaUfficio      value 1.
           88  CopiaMagazzino    value 2.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

      * RIGHE PER LA STAMPA
       01  divisorio   pic x(78) value all "=".

       01  intestazione.
         05 filler     pic x(19) value "BUONO DI CARICO N. ".
         05 int-numero pic z(8).
         05 filler     pic x(5)  value " DEL ".
         05 int-data   pic x(10).
         05 filler     pic x(3)  value " - ".
         05 int-ora    pic x(8).

       01  testata.
         05 filler     pic x(8)  value "ARTICOLO".
         05 filler     pic x(1)  value spaces.
         05 filler     pic x(20) value "DESCRIZIONE".
         05 filler     pic x(3)  value spaces.     
         05 filler     pic x(3)  value "UDM".
         05 filler     pic x(2)  value spaces.
         05 filler     pic x(9)  value "QUANTITA'".
         05 filler     pic x(14) value "    IMP. MERCE".
         05 filler     pic x(2)  value spaces.
         05 filler     pic x(8)  value "CONSUMO".
         05 filler     pic x(1)  value spaces.
         05 filler     pic x(8)  value "IMP.COU".

       01  riga-stampa.
         05 cod-art    pic z(8).
         05 filler     pic x(1).
         05 des-art    pic x(20).
         05 filler     pic x(3).
         05 udm        pic x(3).
         05 filler     pic x(1).
         05 qta        pic z.zzz.zz9.
         05 filler     pic x(1).
         05 imp-merce  pic zzz.zzz.zz9,99.
         05 filler     pic x(1).
         05 consumo    pic z.zz9,99.
         05 filler     pic x(1).
         05 cou        pic z.zz9,99.

       01  riga-totale.
         05 filler     pic x(25).
         05 filler     pic x(11) value "=========> ".
         05 filler     pic x(29) value "TOTALE COMPLESSIVO IVATO = ".
         05 tot-edit   pic zz.zzz.zz9,99.

       01  testa-note  pic x(20) value "** NOTE DI CONSEGNA:".
       01  riga-note1.
         05 r-note1      pic x(19).
         05 filler       pic x.
         05 r-note-data  pic x(10).
         05 filler       pic x.
         05 r-note2      pic x(30).

       01  riga-note2.
         05 r-note3      pic x(19).
         05 filler       pic x.
         05 r-note4      pic x(30).

       01  riga-magazzino-1 pic x(78)
                            value "*********************************".
       01  riga-magazzino-2 pic x(78)
                            value "*** ** * || MAGAZZINO || * ** ***".
       01  riga-magazzino-3 pic x(78)
                            value "*********************************".
                                                             
       01  riga-ufficio-1   pic x(78)
                            value "*******************************".
       01  riga-ufficio-2   pic x(78)
                            value "*** ** * || UFFICIO || * ** ***".
       01  riga-ufficio-3   pic x(78)
                            value "*******************************".

       77  como-rec pic x(1000).

       LINKAGE SECTION.
       copy "link-stbuono.def".

      ******************************************************************
       PROCEDURE DIVISION using stbuono-linkage.

       DECLARATIVES.
       TORDINI SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [TORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

       LINESEQ SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

       RORDINI SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [RORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

       TIVAESE SECTION.
           use after error procedure on tivaese.
           set tutto-ok  to true.
           evaluate status-tivaese
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Tabella codici iva [TIVAESE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [TIVAESE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TIVAESE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

       ARTICOLI SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [ARTICOLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform ELABORAZIONE
              perform EXIT-PGM
           end-if.

      ***---
       INIT.
           move 0 to riga.
           move 0 to righe.
           set     tutto-ok     to true.
           set     CopiaUfficio to true.
           accept  wstampa      from environment "PATH-ST".
           accept  como-data    from century-date.
           accept  como-ora     from time.
           inspect wstampa      replacing trailing spaces by low-value.
           string wstampa       delimited by low-value
                  "stbuono"     delimited by size
                  "_"           delimited by size
                  como-data     delimited by size
                  "_"           delimited by size
                  como-ora      delimited by size
                  ".txt"        delimited by size
                  into wstampa
           end-string.
           inspect wstampa      replacing trailing low-value by spaces.

      ***---
       OPEN-FILES.              
           open input tordini rordini tivaese articoli.
           if tutto-ok 
              open output lineseq
      *    luciano
              write line-riga from space after 1
      *
           else        
              goback
           end-if.
      
      ***---
       ELABORAZIONE.
           if CopiaMagazzino
              write line-riga from spaces after page
      *    luciano
              write line-riga from x"09" after 1
      *
           end-if.
           move buono-chiave to tor-chiave.
           read tordini invalid set errori to true end-read.
           if tutto-ok
              move low-value to ror-rec
              move tor-chiave to ror-chiave
              start rordini key is >= ror-chiave
                    invalid continue
              end-start           
              set trovato         to false
              set TestataGiaFatta to false
              perform until 1 = 2
                 read rordini  next 
                      at end   subtract 1 from riga
                               exit perform
                 end-read
                 if tor-anno   not = ror-anno   or
                    tor-numero not = ror-num-ordine
                    subtract 1 from riga
                    exit perform
                 end-if
                 set trovato      to true
                 if not TestataGiaFatta
                    move 0 to tot-importo
                    move 0 to prezzo
                    perform STAMPA-TESTA
                    set TestataGiaFatta to true
                 end-if
                 move "IV"        to tbliv-codice1
                 move ror-cod-iva to tbliv-codice2
                 read tivaese no lock 
                      invalid move 0 to tbliv-percentuale
                 end-read
                 move ror-cod-articolo to cod-art art-codice
                 read articoli no lock 
                      invalid move spaces to art-descrizione 
                 end-read
                 move art-descrizione     to des-art
                 move art-unita-di-misura to udm
                 move ror-qta             to qta
                 move ror-imponib-merce   to imp-merce
                 move ror-imp-consumo     to consumo
                 move ror-imp-cou-cobat   to cou
                 write line-riga from riga-stampa after 1
                 compute prezzo = ror-imponib-merce +
                                  ror-imp-cou-cobat +
                                  ror-imp-consumo
                 compute prezzo = prezzo * ror-qta
                 compute como-imposta =
                         prezzo * tbliv-percentuale / 100
                 add 0,005 to como-imposta
                 move como-imposta to como-imposta-2dec

                 compute tot-importo =
                         tot-importo +
                         prezzo      +
                         como-imposta-2dec

                 add 1 to riga
                 if riga > ( MaxRighe - 2 )
                    write line-riga from spaces after page
      *    luciano
                    write line-riga from x"09" after 1
      *
                    write line-riga from spaces after 1   
                    write line-riga from spaces after 1
                    move 5 to riga
                 end-if
              end-perform
              if trovato            
                 perform STAMPA-TOTALE
                 perform STAMPA-NOTE
                 perform STAMPA-TIPO-COPIA
              end-if
           end-if.

      ***---
       STAMPA-TESTA.
           move tor-numero     to int-numero.
           move como-data(7:2) to int-data(1:2).
           move "/"            to int-data(3:1).
           move como-data(5:2) to int-data(4:2).
           move "/"            to int-data(6:1).
           move como-data(1:4) to int-data(7:4).
           move como-ora(1:2)  to int-ora(1:2).
           move "."            to int-ora(3:1).
           move como-ora(3:2)  to int-ora(4:2).
           move "."            to int-ora(6:1).
           move como-ora(5:2)  to int-ora(7:2).
           write line-riga   from intestazione after 2.
           write line-riga   from spaces after 1.
           write line-riga   from divisorio after 1.
           write line-riga   from testata after 1.  
           write line-riga   from divisorio after 1.
           write line-riga   from spaces after 1.
           move  10            to riga.

      ***---
       STAMPA-TOTALE.
           if riga > ( MaxRighe - 2 - 4 )
              write line-riga from spaces after page
      *    luciano
              write line-riga from x"09" after 1
      *
              write line-riga from spaces after 1
              write line-riga from spaces after 1
              move  4 to riga
           end-if.
           move  tot-importo to tot-edit.
           write line-riga from divisorio after 3.
           write line-riga from riga-totale after 1.
           add 4 to riga.

      ***---
       STAMPA-NOTE.  
           if riga > ( MaxRighe - 2 - 4 )
              write line-riga from spaces after page
      *    luciano
              write line-riga from x"09" after 1
      *
              write line-riga from spaces after 1   
              write line-riga from spaces after 1
              move  4 to riga
           end-if.
           if tor-note1      not = spaces or
              tor-note2      not = spaces or
              tor-note3      not = spaces or
              tor-note4      not = spaces or
              tor-data-note1 not = 0

              write line-riga        from testa-note after 2
              move  tor-note1          to r-note1
              move tor-data-note1(7:2) to r-note-data(1:2)
              move "/"                 to r-note-data(3:1)
              move tor-data-note1(5:2) to r-note-data(4:2)
              move "/"                 to r-note-data(6:1)
              move tor-data-note1(1:4) to r-note-data(7:4)
              move tor-note2           to r-note2
              write line-riga          from riga-note1 after 1

              move tor-note3           to r-note3
              move tor-note4           to r-note4
              write line-riga          from riga-note2 after 1
              add 4 to riga
           end-if.

      ***---
       STAMPA-TIPO-COPIA.
           compute righe = ( MaxRighe + 2 - riga ).
           if CopiaUfficio
              call "C$JUSTIFY" using riga-ufficio-1, "C"
              call "C$JUSTIFY" using riga-ufficio-2, "C"
              call "C$JUSTIFY" using riga-ufficio-3, "C"
              write line-riga   from riga-ufficio-1 after righe
              write line-riga   from riga-ufficio-2 after 1
              write line-riga   from riga-ufficio-3 after 1
              set CopiaMagazzino  to true
           else
              call "C$JUSTIFY" using riga-magazzino-1, "C"
              call "C$JUSTIFY" using riga-magazzino-2, "C"
              call "C$JUSTIFY" using riga-magazzino-3, "C"
              write line-riga   from riga-magazzino-1 after righe
              write line-riga   from riga-magazzino-2 after 1
              write line-riga   from riga-magazzino-3 after 1
           end-if.

      ***---
       EXIT-PGM.
           write LINE-RIGA from space before page.
           close tordini  lineseq  rordini articoli tivaese.
           if trovato

              initialize file-info

              call "C$FILEINFO" using wstampa, file-info

              if file-size not = 0
      *           call   "SPOOLER-A" using operazione, wstampa
      *           cancel "SPOOLER-A"
                 move wstampa            to splcrt2graf-percorso-stampa
                 set splcrt2graf-stampa     to true
                 set splcrt2graf-windows    to true
                 set splcrt2graf-verticale  to true
                 set splcrt2graf-forza-crt  to true
                 set splcrt2graf-12pt       to true
                 call "splcrt2graf" using splcrt2graf-link
                 cancel "splcrt2graf"

              end-if

              call "C$DELETE" using wstampa
           end-if.
           goback.
