       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EDI-stbozze-p.
       AUTHOR.                          Andrea.
       REMARKS. La stampante mi viene passata da st-ordine-m oppure
                richiesta. st-ordine-m sa che mi deve passare la stessa
                stampante usata per lo scontrino.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destini.sl".
           copy "EDI-mtordini.sl". 
           copy "EDI-mrordini.sl".
           copy "mtordini.sl". 
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "destini.fd".      
           copy "EDI-mtordini.fd". 
           copy "EDI-mrordini.fd".
           copy "mtordini.fd".
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "fonts.def".
           copy "acugui.def".
           copy "spooler.def".
           copy "selprint.lks".
           copy "tratta-numerico.def".
       
       78  titolo                value "Stampa Bozze EDI".
       78  78-comma              value ".".
       78  MaxRighe              value 54.
                                          
       77  status-EDI-mtordini   pic xx.
       77  status-EDI-mrordini   pic xx.
       77  status-mtordini       pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-articoli       pic xx.

       77  TimesNewRoman20B      handle of font.
       77  CourierNew10          handle of font.
       77  CourierNew10B         handle of font.
       77  CourierNew12B         handle of font.

       77  messaggio             pic x(150)  value spaces.
       77  wfont-status          pic s9(5)   value zero.
       77  font-size-dply        pic z(5)    value zero.
       77  pos-piede             pic 9(3)v99 value zero.
       77  max-righe-corpo       pic 9(3)v99 value zero.

       77  como-data              pic 9(8).
       77  como-ora               pic 9(8).
       77  riga                   pic 99.
      ***** 77  imballi-ed             pic z(4).
       77  tot-colli              pic 9(5).
       77  tot-peso-tot           pic 9(6)v999.
       77  tot-peso-utf           pic 9(6)v999.
       77  tot-peso-non-utf       pic 9(6)v999.
       77  totale                 pic 9(6)v999.
       77  codice-x               pic x(5).
                                           
       77  como-qta               pic 9(8).
       77  como-colli             pic 9(8).
       77  como-prz               pic 9(8)v9(3).
                                                 
      *
       77  filler                pic 9   value 0.
           88  testata-bold      value 1 false 0.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

      * RIGHE PER LA STAMPA
       01  divisorio   pic x(95) value all "=".

       01  t0.
         05 t0-NAB-CODBUYER      PIC  x(40).
         05 filler               pic x(8).
         05 t0-NAD-CODCONS       pic x(40).

       01  t1.
         05 t1-cli-ragsoc        pic x(40).
         05 filler               pic x(8).
         05 t1-des-ragsoc        pic x(40).

       01  t2.
         05 t2-cli-indirizzo     pic x(40).
         05 filler               pic x(8).
         05 t2-des-indirizzo     pic x(40).

       01  t3.
         05 t3-cli-cap           pic x(5).
         05 filler               pic x.
         05 t3-cli-localita      pic x(34).
         05 filler               pic x(8).
         05 t3-des-cap           pic x(5).
         05 filler               pic x.
         05 t3-des-localita      pic x(34).

       01  t4.
         05 filler               pic x(21) value "Dati Ordine GESLUX :".
         05 t4-numero            pic z(10).
         05 filler               pic x(5)  value " del ".
         05 t4-del               pic x(10).
         05 filler               pic x(3)  value " - ".
         05 t4-hh                pic 9(2).
         05 filler               pic x     value ":".
         05 t4-mm                pic 9(2). 
         05 filler               pic x(14) value " Inserita da: ".
         05 t4-utente            pic x(20).

       01  t4b.
         05 filler               pic x(21) value "Dati import EDI....:".
         05 t4b-numero           pic z(10).
         05 filler               pic x(5)  value " del ".
         05 t4b-del              pic x(10).
         05 filler               pic x(3)  value " - ".
         05 t4b-hh               pic 9(2).
         05 filler               pic x     value ":".
         05 t4b-mm               pic 9(2). 
         05 filler               pic x(14) value " Inserita da: ".
         05 t4b-utente           pic x(20).

       01  t5.
         05 filler               pic x(21) value "Dati Bozza EDI.....:".
         05 t5-num-ord-cli       pic x(10).
         05 filler               pic x(5)  value " del ".
         05 t5-del               pic x(10).
         05 filler               pic x(3).
         05 t5-ritira            pic x(15).

       01  t6.
         05 filler               pic x(21) value "Note di consegna...:".
         05 t6-note1             pic x(20).
         05 t6-data              pic x(10).
         05 filler               pic x(2)  value spaces.
         05 t6-note2             pic x(30).

       01  t7.
         05 filler               pic x(21) value spaces.
         05 t7-note3             pic x(30).
         05 filler               pic x(2)  value spaces.
         05 t7-note4             pic x(30).

       01  testata.          
         05 filler     pic x.
         05 filler     pic x(6)  value "  Art.".
         05 filler     pic x(3)  value spaces.
         05 filler     pic x(5)  value "Colli".
         05 filler     pic x(4)  value spaces.
      *****   05 filler     pic x(22) value "Imballo".
      *****   05 filler     pic x(1)  value spaces.
         05 filler     pic x(45) value "Descrizione".
         05 filler     pic x(5)  value spaces.     
         05 filler     pic x(5)  value "Q.tà".

       01  testata-prz-bold.
         05 filler     pic x(11) value "     Prezzo".
         
       01  r-riga.
         05 filler     pic x.
         05 r-art      pic z(6).
         05 filler     pic x(3).
         05 r-colli    pic z.zz9.
         05 filler     pic x(4).
      *****   05 r-imb      pic x(22).
      *****   05 filler     pic x(1).
         05 r-des      pic x(45).
         05 filler     pic x(3).
         05 r-qta      pic zz.zz9.   

       01  r-riga-prz-bold.
         05 r-prz      pic zzz.zz9,999.

       01  r-totale.
         05 filler     pic x(71).
         05 filler     pic x(8) value "TOTALE:  ".
         05 filler     pic x(2).
         05 r-tot      pic z.zzz.zz9,999.       

       01  r-data-ora.
         05 filler     pic x(30).
         05 filler     pic x(13)  value "*** COPIA ***".
         05 filler     pic x(2).
         05 filler     pic x(12) value "Stampata il ".
         05 r-data     pic x(10).
         05 filler     pic x(6) value " alle ".
         05 r-ora      pic x(5).

       01                       pic 9.
           88 primo-passaggio   value 1 false zero.

       LINKAGE SECTION.
           copy "link-EDI-stbozze.def".

      ******************************************************************
       PROCEDURE DIVISION using EDI-stb-limiti.

       DECLARATIVES.
      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set tutto-ok  to true.
           evaluate status-mtordini 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [MTORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [MTORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[MTORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Tabella Clienti [CLIENTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Tabella Destini [DESTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [DESTINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[DESTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

       END DECLARATIVES.
      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to riga.
           set tutto-ok         to true.
           set primo-passaggio  to true
           accept como-data from century-date.
           accept como-ora  from time.

      ***---
       OPEN-FILES.              
           open input mtordini clienti destini 
                      EDI-mtordini EDI-mrordini
                      articoli.
      
      ***---
       ELABORAZIONE.
           set  tutto-ok      to true.

           move EDI-stb-da-anno to mto-anno.
           move EDI-stb-da-num  to mto-numero.

           start mtordini key >= mto-chiave
                 invalid continue
             not invalid perform SCORRI-ORDINI
           end-start.

      ***---
       SCORRI-ORDINI.
           perform until 1 = 2
              read mtordini next at end exit perform end-read

              if mto-anno > EDI-stb-a-anno
                 exit perform
              end-if

              if mto-anno = EDI-stb-a-anno and 
                 mto-numero > EDI-stb-a-num
                 exit perform
              end-if

              perform VALIDA-ORDINE

              if tutto-ok
                 perform TRATTA-ORDINE
                 if spl-sta-annu
                    exit perform
                 end-if
              end-if
           end-perform.           

           if not primo-passaggio
              perform CHIUDI-STAMPA
           end-if.

      ***---
       VALIDA-ORDINE.
           set tutto-ok to true.                          

           if mto-numero < EDI-stb-da-num or 
              mto-numero > EDI-stb-a-num
              set errori to true
           end-if.

           if mto-data-creazione < EDI-stb-da-data or 
              mto-data-creazione > EDI-stb-a-data
              set errori   to true
           end-if.
                     
           move mto-ordine-EDI to emto-chiave.
           read EDI-mtordini no lock
                invalid set errori to true
           end-read.

      ***---
       TRATTA-ORDINE.
           perform STAMPA-TESTA
           if spl-sta-annu
              exit paragraph
           end-if

           move low-value   to emro-rec.
           move emto-chiave to emro-chiave.
           start EDI-mrordini key is >= emro-chiave
                 invalid continue
           end-start
           move  0 to tot-colli
           move 15 to riga
           move  0 to totale
                      tot-peso-utf 
                      tot-peso-non-utf 
                      tot-peso-tot
           perform until 1 = 2
              read EDI-mrordini next 
                 at end 
                    exit perform 
              end-read

              if emto-anno   not = emro-anno   or
                 emto-numero not = emro-numero
                 exit perform
              end-if 
              |Per le righe inserite manualmente
              if emro-02D13-LIN-CODFORTU = spaces
                 move emro-cod-articolo to r-art art-codice
                 read articoli no lock 
                      invalid move spaces to art-descrizione
                 end-read
                 move art-descrizione to r-des
                 move emro-qta        to r-qta como-qta
                 move emro-num-colli  to r-colli como-colli

              else
                 move emro-02D13-LIN-CODFORTU to NumericEdi
                 perform TRATTA-NUMERICO
                 move como-numero             to r-art
                 move emro-02D15-LIN-DESART   to r-des       

                 move emro-02D17-QTAORD       to NumericEdi
                 perform TRATTA-NUMERICO
                 move como-numero             to r-qta como-qta

                 move emro-02D22-LIN-NRCUINTU to NumericEdi
                 perform TRATTA-NUMERICO
                 move como-numero             to r-colli como-colli
              end-if

              move r-riga to spl-riga-stampa
              perform SCRIVI
              
              move CourierNew10B     to spl-hfont
              set testata-bold       to true    
                                                 
              |Per le righe inserite manualmente
              if emro-02D13-LIN-CODFORTU = spaces
                 move emro-prz        to r-prz como-prz
              else
                 move emro-02D19-LIN-PRZUNI   to NumericEdi
                 perform TRATTA-NUMERICO
                 move como-numero             to r-prz como-prz
              end-if
      
              move r-riga-prz-bold   to spl-riga-stampa
              perform SCRIVI
              subtract 1 from riga
              set testata-bold       to false
              move CourierNew10      to spl-hfont  

              compute totale = 
                      totale + ( como-qta   * 
                                 como-colli * 
                                 como-prz )

              if riga >= MaxRighe
                 perform SALTO-PAGINA
              end-if

           end-perform

           if riga < MaxRighe
              move divisorio to spl-riga-stampa
              perform SCRIVI
           end-if
                                          
           move CourierNew10B to spl-hfont
           move totale   to r-tot
           move r-totale to spl-riga-stampa
           perform SCRIVI

           if riga >= MaxRighe - 7
              perform SALTO-PAGINA
           end-if

           move CourierNew12B to spl-hfont

           move spaces to spl-riga-stampa
           perform SCRIVI          

           accept como-data from century-date
           accept como-ora  from time
           string como-data(7:2) delimited size
                  "/"            delimited size
                  como-data(5:2) delimited size
                  "/"            delimited size
                  como-data(1:4) delimited size
                  into r-data
           end-string
           string como-ora(1:2) delimited size
                  ":"           delimited size
                  como-ora(3:2) delimited size
                  into r-ora
           end-string

           move spaces to spl-riga-stampa
           perform SCRIVI

           subtract 0,2 from spl-riga
           move r-data-ora to spl-riga-stampa
           perform SCRIVI.

      ***---
       APRI-STAMPA.
           move EDI-stb-stampante to selprint-stampante.
           if selprint-stampante = spaces
              call   "selprint" using selprint-linkage
              cancel "selprint"
           end-if.

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              set spl-vertical         to true
              set spl-apertura         to true
              move 1                   to spl-margine-sinistro
              move 1                   to spl-margine-destro
              move 1                   to spl-margine-inf
              move "Stampa Bozze EDI"  to spl-nome-job
              call "spooler"        using spooler-link

              compute max-righe-corpo = spl-altezza - 10
              compute pos-piede       = spl-altezza -  8
           else
              set spl-sta-annu to true
           end-if.

           if spl-sta-annu 
              cancel "spooler"
           else
              perform SETTA-FONT
              set spl-stringa to true
              |Mi riposiziono ad inizio foglio
              move 1            to spl-colonna
              move 1            to spl-riga
              move spaces       to spl-riga-stampa
              move CourierNew10 to spl-hfont
              call "spooler" using spooler-link
           end-if.

      ***---
       CHIUDI-STAMPA.
           set spl-chiusura to true.
           cancel "spooler".

      ***---
       STAMPA-TESTA.
           if primo-passaggio
              perform APRI-STAMPA
              if spl-sta-annu
                 exit paragraph
              end-if
              set primo-passaggio  to false
           else
              perform SALTO-PAGINA
           end-if

           set spl-stringa         to true.
           move TimesNewRoman20B   to spl-hfont.
           move 0,5                to spl-riga.
           move 7,0                to spl-colonna.
           move "STAMPA BOZZE EDI" to spl-riga-stampa.
           call "spooler" using spooler-link.

           move CourierNew10 to spl-hfont.

           move 2           to spl-riga.

           set  cli-tipo-C   to true.
           move emto-cod-cli to cli-codice.
           read clienti no lock invalid continue end-read.

           if emto-prg-destino not = 0
              move emto-cod-cli     to des-codice
              move emto-prg-destino to des-prog
              read destini no lock invalid continue end-read
           else
              initialize des-indirizzo
                         des-cap
                         des-localita
                         des-prov
           end-if.     
                                                           
           move emto-01t21-nab-codbuyer to t0-nab-codbuyer.
           move emto-01t28-nad-codcons  to t0-nad-codcons.
           move t0           to spl-riga-stampa.
           perform SCRIVI.

           move cli-codice   to codice-x.
           inspect codice-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using codice-x, "L".
           inspect codice-x replacing trailing spaces by low-value.
           initialize t1-cli-ragsoc.
           string codice-x     delimited low-value
                  " - "        delimited size
                  cli-ragsoc-1 delimited size
                  into t1-cli-ragsoc
           end-string.
                                                                   
           initialize t1-des-ragsoc.
           if emto-prg-destino not = 0
              move des-prog    to codice-x
              inspect codice-x replacing leading x"30" by x"20"
              call "C$JUSTIFY" using codice-x, "L"
              inspect codice-x replacing trailing spaces by low-value
              string codice-x     delimited low-value
                     " - "        delimited size
                     des-ragsoc-1 delimited size
                     into t1-des-ragsoc
              end-string
           else
              move space  to t1-des-ragsoc
           end-if.

           move t1           to spl-riga-stampa.
           perform SCRIVI.

           move cli-indirizzo to t2-cli-indirizzo.
           move des-indirizzo to t2-des-indirizzo.
           move t2           to spl-riga-stampa.
           perform SCRIVI.

           move cli-cap      to t3-cli-cap.
           move cli-localita to t3-cli-localita.
           move des-cap      to t3-des-cap.
           move des-localita to t3-des-localita.
           move t3           to spl-riga-stampa.
           perform SCRIVI.

           add 0,3 to spl-riga.

           move mto-numero to t4-numero.
           string mto-data-creazione(7:2) delimited size
                  "/"                     delimited size
                  mto-data-creazione(5:2) delimited size
                  "/"                     delimited size
                  mto-data-creazione(1:4) delimited size
                  into t4-del
           end-string.
           move mto-ora-creazione(1:2) to t4-hh.
           move mto-ora-creazione(3:2) to t4-mm.
           move mto-utente-creazione   to t4-utente.
           move t4           to spl-riga-stampa.
           perform SCRIVI.

           move emto-numero to t4b-numero.
           string emto-data-creazione(7:2) delimited size
                  "/"                      delimited size
                  emto-data-creazione(5:2) delimited size
                  "/"                      delimited size
                  emto-data-creazione(1:4) delimited size
                  into t4b-del
           end-string.
           move emto-ora-creazione(1:2) to t4b-hh.
           move emto-ora-creazione(3:2) to t4b-mm.
           move emto-utente-creazione   to t4b-utente.
           move t4b           to spl-riga-stampa.
           perform SCRIVI.

           move emto-01T05-BGM-NUMDOC to t5-num-ord-cli.
           string emto-01T04-BGM-DATADOC(7:2) delimited size
                  "/"                         delimited size
                  emto-01T04-BGM-DATADOC(5:2) delimited size
                  "/"                         delimited size
                  emto-01T04-BGM-DATADOC(1:4) delimited size
                  into t5-del
           end-string.
           if emto-ritira-si
              move "RITIRA IN LUBEX" to t5-ritira
           else
              move spaces            to t5-ritira
           end-if.
           move t5           to spl-riga-stampa.
           perform SCRIVI.

           add 0,3 to spl-riga.

           move emto-note1 to t6-note1.
           if emto-01T11-DTM-DATACONS not = spaces
              string emto-01T11-DTM-DATACONS(7:2) delimited size
                     "/"                          delimited size
                     emto-01T11-DTM-DATACONS(5:2) delimited size
                     "/"                          delimited size
                     emto-01T11-DTM-DATACONS(1:4) delimited size
                     into t6-data
              end-string
           end-if.
           move emto-01T35-FTX-NOTE to t6-note2.
           move t6        to spl-riga-stampa.
           perform SCRIVI.

           move emto-01T68-FTX-NOTE to t7-note3.
           move emto-01T69-FTX-NOTE to t7-note4.
           move t7        to spl-riga-stampa.
           perform SCRIVI.
                          
           move divisorio to spl-riga-stampa.
           perform SCRIVI.
           
           move testata  to spl-riga-stampa.
           perform SCRIVI.
                                           
           move testata-prz-bold  to spl-riga-stampa.
           move CourierNew10B     to spl-hfont.
           set testata-bold       to true.
           perform SCRIVI.
           set testata-bold to false.

           move CourierNew10 to spl-hfont.

           move divisorio to spl-riga-stampa.
           perform SCRIVI.

      ***---
       SCRIVI.
           if testata-bold
              subtract 0,5 from spl-riga
              move 17,8      to spl-colonna
           else
              move 0,3 to spl-colonna
           end-if.
           set  spl-stringa  to true.
           call "spooler" using spooler-link.
           add  0,5 to spl-riga.
           add  1   to riga.

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina     to true.
           call "spooler"        using spooler-link.                                                            
      *    luciano
           set spl-stringa to true
           |Mi riposiziono ad inizio foglio
           move 0      to spl-riga
           move spaces to spl-riga-stampa
           call "spooler" using spooler-link.
           move spaces to spl-riga-stampa.
           perform SCRIVI.
      *    luciano
           move 0 to riga.

      ***---
       SETTA-FONT.
           set tutto-ok             to true.

           initialize WFONT-DATA.
           move 10                  to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to false.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               CourierNew10, wfont-data
                               giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize WFONT-DATA.
           move 10                  to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               CourierNew10B, wfont-data
                               giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize WFONT-DATA.
           move 12                  to wfont-size.
           move "Courier New"       to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               CourierNew12B, wfont-data
                               giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

           initialize wfont-data.
           move 20                  to wfont-size.
           move "Times New Roman"   to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to false   
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               TimesNewRoman20B, wfont-data
                               giving wfont-status.

           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

      ***---
       DISTRUGGI-FONT.
           Destroy CourierNew10.
           Destroy TimesNewRoman20B.

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.

           inspect messaggio replacing trailing SPACE by LOW-VALUE.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close mtordini clienti destini 
                 EDI-mtordini EDI-mrordini
                 articoli.

      ***---
       EXIT-PGM.
           destroy TimesNewRoman20B 
                   CourierNew10 
                   CourierNew10B
                   CourierNew12B.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "tratta-numerico.cpy".
