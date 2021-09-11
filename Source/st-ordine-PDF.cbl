       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-ordine-PDF.
       AUTHOR.                          Andrea.
       REMARKS. Stampa l'ordine appena dopo ch'esso viene salvato.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destini.sl".
           copy "check-rordini.sl".
           copy "check-rordini2.sl".
           copy "tordini.sl". 
           copy "rordini.sl".
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "destini.fd".
           copy "check-rordini.fd".
           copy "check-rordini2.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "fonts.def".
           copy "acugui.def".
           copy "spooler.def".
           copy "selprint.lks".
           copy "link-readutente.def".
       
       78  titolo                value "Stampa Ordine".
       78  MaxRighe              value 54.
                                 
       77  status-check-rordini  pic xx.
       77  status-check-rordini2 pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-articoli       pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.

       77  path-check-rordini    pic x(256).
       77  path-check-rordini2   pic x(256).

       77  TimesNewRoman20B       handle of font.
       77  CourierNew10           handle of font.
       77  CourierNew10B          handle of font.
       77  CourierNew12B          handle of font.

       77  PgmChiamante           pic x(20).
       77  messaggio              pic x(150)  value spaces.
       77  wfont-status           pic s9(5)   value zero.
       77  font-size-dply         pic z(5)    value zero.
       77  pos-piede              pic 9(3)v99 value zero.
       77  max-righe-corpo        pic 9(3)v99 value zero.
       77  num-pagina             pic 9(3)    value zero.
       77  num-edit               pic x(8).
       77  mese                   pic 99.
       77  mese-esteso            pic x(12).
       77  path-pdf-notifica      pic x(256).
       77  DestFile               pic x(256).
       77  NomeFile               pic x(256).
       77  PathCreaPDF            pic x(256).
       77  StatusCreaPDF          pic s9.
       77  parametri              pic x(200).

       77  como-data              pic 9(8).
       77  como-ora               pic 9(8).
       01  como-time.
           05 como-hh             pic 99.
           05 como-mm             pic 99.
           05 como-ss             pic 99.
           05 como-cc             pic 99.
       77  como-ss-end            pic 9(8).
       77  scelta                 pic 9.
       77  riga                   pic 99.
       77  imballi-ed             pic z(4).
       77  tot-colli              pic 9(5).
       77  tot-peso-tot           pic 9(6)v999.
       77  tot-peso-utf           pic 9(6)v999.
       77  tot-peso-non-utf       pic 9(6)v999.
       77  totale                 pic 9(6)v999.
       77  sw-aggiunte            pic 9(3).
       77  sw-modificate          pic 9(3).
       77  sw-eliminate           pic 9(3).
       77  tot-peso-aggiunte      pic 9(6)v999.
       77  tot-utf-aggiunte       pic 9(6)v999.
       77  tot-non-utf-aggiunte   pic 9(6)v999.
       77  tot-colli-aggiunte     pic 9(4).
       77  tot-peso-modificate    pic s9(6)v999.
       77  tot-utf-modificate     pic s9(6)v999.
       77  tot-non-utf-modificate pic s9(6)v999.
       77  tot-colli-modificate   pic s9(4).
       77  tot-peso-eliminate     pic 9(6)v999.
       77  tot-utf-eliminate      pic 9(6)v999.
       77  tot-non-utf-eliminate  pic 9(6)v999.
       77  tot-colli-eliminate    pic 9(4).
       77  idx                    pic 9(3) value 0.
       77  importo-singolo2       pic 9(5)v99.
       77  importo-singolo        pic 9(5)v99.
       77  comando                pic x(200).
       77  codice-x               pic x(5).

      *
       01  sw-font               pic 9.

       77  filler                pic 9   value 0.
           88  testata-bold      value 1 false 0.

       77  filler                 pic 9.
           88  ScartaSecondi      value 1, false 0.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

      * RIGHE PER LA STAMPA
       01  divisorio   pic x(90) value all "=".

       01  t1.
         05 t1-cli-ragsoc        pic x(40).
         05 filler               pic x(2).
         05 t1-des-ragsoc        pic x(40).

       01  t2.
         05 t2-cli-indirizzo     pic x(40).
         05 filler               pic x(2).
         05 t2-des-indirizzo     pic x(40).

       01  t3.
         05 t3-cli-cap           pic x(5).
         05 filler               pic x.
         05 t3-cli-localita      pic x(34).
         05 filler               pic x(2).
         05 t3-des-cap           pic x(5).
         05 filler               pic x.
         05 t3-des-localita      pic x(34).

       01  t4.
         05 filler               pic x(21) value "Dati Ordine........:".
         05 t4-numero            pic z(12).
         05 filler               pic x(5)  value " del ".
         05 t4-del               pic x(10).
         05 filler               pic x(3)  value " - ".
         05 t4-hh                pic 9(2).
         05 filler               pic x     value ":".
         05 t4-mm                pic 9(2). 
         05 filler               pic x(14) value " Inserita da: ".
         05 t4-utente            pic x(20).

       01  t5.
         05 filler               pic x(21) value "Dati Ordine Cliente:".
         05 t5-num-ord-cli       pic x(12).
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
         05 filler               pic x(5)  value spaces.
         05 t7-note4             pic x(30).

       01  testata.
         05 filler     pic x(6)  value "  Art.".
         05 filler     pic x(1)  value spaces.
         05 filler     pic x(5)  value "Colli".
         05 filler     pic x(2)  value spaces.
         05 filler     pic x(22) value "Imballo".
         05 filler     pic x(1)  value spaces.
         05 filler     pic x(30) value "Descrizione".
         05 filler     pic x(1)  value spaces.     
         05 filler     pic x(4)  value "Q.tà".
         05 filler     pic x(1)  value spaces.
         05 filler     pic x(8)  value "    PROD".

       01  testata-prz-bold.
         05 filler     pic x(8) value "  Prezzo".
         
       01  el-r-riga   occurs 999.
         05 el-r-art   pic z(6).
         05 filler     pic x(1).
         05 el-r-colli pic z.zz9.
         05 filler     pic x(2).
         05 el-r-imb   pic x(22). 
         05 filler     pic x(1).
         05 el-r-des   pic x(30).
         05 el-r-qta   pic zzzz9.
         05 el-qta-pos-neg pic x.
            88 el-qta-pos  value "+".
            88 el-qta-neg  value "-".
         05 filler     pic x(1). 
         05 el-r-prz   pic z.zz9,99 blank zero.
         05 el-tipo    pic x.
            88 el-aggiunta   value "A".
            88 el-modificata value "M".
            88 el-eliminata  value "E".

       01  r-riga.
         05 r-art      pic z(6).
         05 filler     pic x(1).
         05 r-colli    pic z.zz9.
         05 filler     pic x(2).
         05 r-imb      pic x(22).
         05 filler     pic x(1).
         05 r-des      pic x(30).         
         05 r-qta      pic zzzz9.
         05 filler     pic x(2).
         05 r-prod     pic z.zz9,99.

       01  r-riga-prz-bold.
         05 r-prz      pic z.zz9,99.

       01  r-totale.
         05 filler     pic x(71).
         05 filler     pic x(8) value "TOTALE: ".
         05 r-tot      pic zzz.zz9,99.

       01  r-colli-tot.
         05 filler     pic x(15) value "*** Tot. colli:".
         05 r-colli-t  pic ----9.

       01  r-peso-t-utf.
         05 filler         pic x(14) value "*** PESO ---> ".
         05 filler         pic x(45).
         05 filler         pic x(5)  value "UTF: ".
         05 r-peso-utf     pic ---.--9,999.

       01  r-peso-t-non-utf.
         05 filler         pic x(55).
         05 filler         pic x(9)  value "NON UTF: ".
         05 r-peso-non-utf pic ---.--9,999.

       01  r-peso-t-tot.
         05 filler         pic x(59).
         05 filler         pic x(5)  value "TOT: ".
         05 r-peso-tot     pic ---.--9,999.

       01  r-data-ora.
         05 filler     pic x(17) value "Stampata in data ".
         05 r-data     pic x(10).
         05 filler     pic x(10) value " alle ore ".
         05 r-ora      pic x(5).

       LINKAGE SECTION.
       01  link-chiave.
           05 link-anno         pic 9(4).
           05 link-numero       pic 9(8).
       77  link-path            pic x(256).
       01  tipo-ope             pic x.
           88 inserimento       value "I".
           88 modifica          value "M".

      ******************************************************************
       PROCEDURE DIVISION using link-chiave, link-path, tipo-ope.

       DECLARATIVES.

      ***---      
       CHECK-RORDINI2-ERR SECTION.
           use after error procedure on check-rordini2.
           set tutto-ok  to true.
           evaluate status-check-rordini2
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [CHECK-RORDINI2] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [CHECK-RORDINI2] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CHECK-RORDINI2] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  

      ***---
       TORDINI-ERR SECTION.
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

      ***---      
       RORDINI-ERR SECTION.
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

      ***---
       ARTICOLI-ERR SECTION.
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
           accept ru-user from environment "USER_CODI".
           call   "readutente" using ru-linkage.
           cancel "readutente".
           move 0 to riga.
           set     tutto-ok     to true.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-check-rordini2.
           accept  path-check-rordini2 from environment "PATH_ST".
           inspect path-check-rordini2 replacing trailing 
                                       spaces by low-value.
           string  path-check-rordini2 delimited low-value
                   "CHECK-RORDINI_"    delimited size
                   como-data           delimited size
                   "_"                 delimited size
                   como-ora            delimited size
                   ".tmp"              delimited size
                   into path-check-rordini2
           end-string.

      ***---
       OPEN-FILES.              
           open input tordini  rordini  clienti destini articoli.
           if modifica
              move link-path to path-check-rordini
              open  input check-rordini
              open output check-rordini2
              close check-rordini2
              open i-o check-rordini2
           end-if.
      
      ***---
       ELABORAZIONE.
           move link-chiave to tor-chiave.
           read tordini no lock invalid set errori to true end-read.
           if tutto-ok
              perform APRI-STAMPA
              if not spl-sta-annu
                 perform STAMPA-TESTA
                 move low-value  to ror-rec
                 move tor-chiave to ror-chiave
                 start rordini key is >= ror-chiave
                       invalid continue
                 end-start
                 move  0 to tot-colli
                 move 15 to riga
                 move  0 to totale sw-aggiunte 
                                   sw-modificate 
                                   sw-eliminate
                 move 0 to tot-peso-aggiunte
                           tot-utf-aggiunte
                           tot-non-utf-aggiunte
                           tot-colli-aggiunte
                           tot-peso-modificate
                           tot-utf-modificate
                           tot-non-utf-modificate
                           tot-colli-modificate
                           tot-peso-eliminate
                           tot-utf-eliminate
                           tot-non-utf-eliminate
                           tot-colli-eliminate
                 perform until 1 = 2
                    read rordini next at end exit perform end-read

                    if tor-anno   not = ror-anno   or
                       tor-numero not = ror-num-ordine
                       exit perform
                    end-if

                    if modifica
                       move ror-prg-chiave  to cror-prg-chiave2
                       move ror-des-imballo to cror-des-imballo2
                       if ror-si-blister
                          move 0 to cror-qta-imballi2
                       else
                          move ror-qta-imballi to cror-qta-imballi2
                       end-if
                       move ror-blister     to cror-blister2
                       read check-rordini2 no lock
                            invalid 
                            initialize cror-dati2
                                       replacing numeric data by zeroes
                                            alphanumeric data by spaces
                            move ror-peso-utf     to cror-peso-utf2
                            move ror-peso-non-utf to cror-peso-non-utf2
                        not invalid
                            add 1 to cror-righe2
                       end-read
                       add ror-num-colli to cror-num-colli2
                       add ror-qta to cror-qta2
                       compute cror-importo2 =
                               cror-importo2 + 
                             ( ror-qta * ror-prz-unitario )
                       write cror-rec2 
                             invalid rewrite cror-rec2 
                       end-write
                    end-if

                    move ror-cod-articolo to r-art art-codice
                    read articoli no lock 
                         invalid move spaces to art-descrizione 
                    end-read

                    move ror-num-colli   to r-colli

                    add ror-num-colli    to tot-colli

                    compute tot-peso-utf =
                            tot-peso-utf + 
                          ( ror-peso-utf * ror-qta)
                    compute tot-peso-non-utf =
                            tot-peso-non-utf + 
                          ( ror-peso-non-utf * ror-qta )
                    compute tot-peso-tot =
                            tot-peso-tot + 
                        (( ror-peso-utf + 
                       ror-peso-non-utf ) * ror-qta )

                    move art-descrizione to r-des

                    compute totale = 
                            totale + ( ror-qta * ror-prz-unitario)

                    if ror-si-blister
                       move ror-des-imballo to r-imb
                    else

                       move ror-qta-imballi to imballi-ed
                       inspect ror-des-imballo replacing trailing 
                                                spaces by low-value
                       move ror-qta-imballi to imballi-ed
                       call "C$JUSTIFY" using imballi-ed, "L"
                       initialize r-imb
                       string  ror-des-imballo delimited by low-value
                               " da "           delimited by size
                               imballi-ed       delimited by spaces
                               " x "            delimited by size
                               art-udm-imballo  delimited by size
                               into r-imb
                       end-string

                    end-if

                    move ror-qta           to r-qta
                    move ror-prz-unitario  to r-prz
                    move ror-imponib-merce to r-prod
                    move r-riga to spl-riga-stampa
                    perform SCRIVI
                    
                    move CourierNew10B     to spl-hfont
                    set testata-bold       to true
                    move ror-prz-unitario  to r-prz
                    move r-riga-prz-bold   to spl-riga-stampa
                    perform SCRIVI
                    subtract 1 from riga
                    set testata-bold       to false
                    move CourierNew10      to spl-hfont

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

                 move tot-colli   to r-colli-t
                 move r-colli-tot to spl-riga-stampa
                 perform SCRIVI

                 move tot-peso-utf     to r-peso-utf
                 move r-peso-t-utf     to spl-riga-stampa
                 perform SCRIVI

                 move tot-peso-non-utf to r-peso-non-utf
                 move r-peso-t-non-utf to spl-riga-stampa
                 perform SCRIVI

                 move tot-peso-tot     to r-peso-tot
                 move r-peso-t-tot     to spl-riga-stampa
                 perform SCRIVI

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
                 perform SCRIVI

                 if modifica
                    perform CONFRONTA-FILES
                 end-if
                                  
                 perform CHIUDI-STAMPA
              end-if
           end-if.
           
      ***---
       APRI-STAMPA.
           perform CREA-PDF.
           perform WAIT-3-SECS.
           if tutto-ok
              evaluate true
              when ru-SO-XP
                   accept selprint-stampante
                   from environment "STAMPANTE_DIRETTA_ORDINI_PDF_XP"
              when ru-SO-VISTA
                   accept selprint-stampante
                   from environment "STAMPANTE_DIRETTA_ORDINI_PDF_V"
              when ru-SO-7
                   accept selprint-stampante
                   from environment "STAMPANTE_DIRETTA_ORDINI_PDF_7"
              end-evaluate

              if selprint-stampante not = space
                 move selprint-num-copie to SPL-NUM-COPIE
                 move selprint-stampante to SPL-NOME-STAMPANTE

                 set  spl-vertical        to true
                 set  spl-apertura        to true
                 move 1                   to spl-margine-sinistro
                 move 1                   to spl-margine-destro
                 move 1                   to spl-margine-inf
                 initialize spl-nome-job
                 string link-anno delimited size
                        "_"       delimited size
                        num-edit  delimited spaces
                        into spl-nome-job
                 end-string
                 call "spooler"        using spooler-link

                 compute max-righe-corpo = spl-altezza - 10
                 compute pos-piede       = spl-altezza -  8
              else
                 set spl-sta-annu to true
              end-if
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
       CREA-PDF.
           |CONTROLLO CARTELLE PARAMETRIZZAZIONE BACKUP
           move como-data(5:2) to mese.
           evaluate mese
           when 01  move "Gennaio"   to mese-esteso
           when 02  move "Febbraio"  to mese-esteso
           when 03  move "Marzo"     to mese-esteso
           when 04  move "Aprile"    to mese-esteso
           when 05  move "Maggio"    to mese-esteso
           when 06  move "Giugno"    to mese-esteso
           when 07  move "Luglio"    to mese-esteso
           when 08  move "Agosto"    to mese-esteso
           when 09  move "Settembre" to mese-esteso
           when 10  move "Ottobre"   to mese-esteso
           when 11  move "Novembre"  to mese-esteso
           when 12  move "Dicembre"  to mese-esteso
           end-evaluate.
           inspect mese-esteso replacing trailing spaces by low-value.
           move link-numero        to num-edit.
           inspect num-edit replacing leading x"30" by x"20".
           call "C$JUSTIFY" using num-edit, "L".
           accept  path-pdf-notifica 
                   from environment "PATH_PDF_NOTIFICA".

           inspect path-pdf-notifica replacing trailing 
                                     spaces by low-value.

           string  path-pdf-notifica delimited low-value
                   link-anno         delimited size
                   "\"               delimited size
                   mese-esteso       delimited low-value
                   "\"               delimited size
                   into DestFile
           end-string.

           string  link-anno         delimited size  
                   "_"               delimited size
                   num-edit          delimited spaces
                   "_"               delimited size
                   como-data         delimited size
                   "_"               delimited size
                   como-ora          delimited size
                   into NomeFile
           end-string.

           evaluate true
           when ru-SO-XP
                accept selprint-stampante
                from environment "STAMPANTE_DIRETTA_ORDINI_PDF_XP"
           when ru-SO-VISTA
                accept selprint-stampante
                from environment "STAMPANTE_DIRETTA_ORDINI_PDF_V"
           when ru-SO-7
                accept selprint-stampante
                from environment "STAMPANTE_DIRETTA_ORDINI_PDF_7"
           end-evaluate.

           inspect NomeFile 
                   replacing trailing spaces by low-value.
           inspect DestFile 
                   replacing trailing spaces by low-value.
           inspect selprint-stampante 
                   replacing trailing spaces by low-value.

           initialize parametri.
           string NomeFile           delimited low-value
                  "§"                delimited size
                  DestFile           delimited low-value
                  "§"                delimited size
                  selprint-stampante delimited low-value
                  into parametri
           end-string.
                             
           accept  PathCreaPDF from environment "PATH_EXE_PDF".
           inspect PathCreaPDF replacing trailing spaces by low-value.

           initialize comando.
           string PathCreaPDF delimited low-value
                  " "         delimited size
                  parametri   delimited size
                  into comando
           end-string.
                             
           move 0 to StatusCreaPDF.
           call "C$SYSTEM" using comando, 129
                          giving StatusCreaPDF
           if StatusCreaPDF = -1
              display message "Archiviazione PDF fallita!"
                        title titolo
                         icon 2
           end-if.

      ***---
      * C$SLEEP ha problemi su thin-client e ritorna il controllo 
      * al menu perciò viene sostituita da questa routine
       WAIT-3-SECS.
           set ScartaSecondi to false.
           accept como-time from time.
           evaluate como-ss
           when 57    move 0 to como-ss-end
                      set ScartaSecondi to true
           when 58    move 1 to como-ss-end
                      set ScartaSecondi to true
           when 59    move 2 to como-ss-end
                      set ScartaSecondi to true
           when 60    move 3 to como-ss-end
                      set ScartaSecondi to true
           when other add  3 to como-ss giving como-ss-end
           end-evaluate.
           perform until 1 = 2
              accept como-time from time
              if como-ss >= como-ss-end
                 if not ScartaSecondi
                    exit perform
                 else
                    |Altrimenti se i secondi iniziali sono 57
                    |al 58(> di 0) uscirebbe subito senza attendere
                    if como-ss not = 57 and
                       como-ss not = 58 and
                       como-ss not = 59 and
                       como-ss not = 60
                       exit perform
                    end-if
                 end-if
              end-if
           end-perform.

      ***---
       CHIUDI-STAMPA.
           set spl-chiusura to true.
           cancel "spooler".

      ***---
       STAMPA-TESTA.
           set spl-stringa       to true.
           move TimesNewRoman20B to spl-hfont.
           move 0,5              to spl-riga.
           move 4,5              to spl-colonna.
           if inserimento
              call "C$CALLEDBY" using PgmChiamante
              if PgmChiamante = "gordc"
                 move "SCONTRINO ORDINE INSERITO"  to spl-riga-stampa
              else
                 move "NOTIFICA PREZZO MODIFICATO" to spl-riga-stampa
              end-if
           else
              move "NOTIFICA BOZZA MODIFICATA" to spl-riga-stampa
           end-if.
           call "spooler" using spooler-link.

           move CourierNew10 to spl-hfont.

           move 2           to spl-riga.
           
           set  cli-tipo-C  to true.
           move tor-cod-cli to cli-codice.
           read clienti no lock invalid continue end-read.

           if tor-prg-destino not = 0
              move tor-cod-cli     to des-codice
              move tor-prg-destino to des-prog
              read destini no lock invalid continue end-read
           end-if.

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
           if des-prog not = 0
              move des-prog    to codice-x
              inspect codice-x replacing leading x"30" by x"20"
              call "C$JUSTIFY" using codice-x, "L"
              inspect codice-x replacing trailing spaces by low-value
              string codice-x     delimited low-value
                     " - "        delimited size
                     des-ragsoc-1 delimited size
                     into t1-des-ragsoc
              end-string
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

           move tor-numero to t4-numero.
           string tor-data-creazione(7:2) delimited size
                  "/"                     delimited size
                  tor-data-creazione(5:2) delimited size
                  "/"                     delimited size
                  tor-data-creazione(1:4) delimited size
                  into t4-del
           end-string.
           move tor-ora-creazione(1:2) to t4-hh.
           move tor-ora-creazione(3:2) to t4-mm.
           move tor-utente-creazione   to t4-utente.
           move t4           to spl-riga-stampa.
           perform SCRIVI.

           move tor-num-ord-cli to t5-num-ord-cli.
           string tor-data-ordine(7:2) delimited size
                  "/"                  delimited size
                  tor-data-ordine(5:2) delimited size
                  "/"                  delimited size
                  tor-data-ordine(1:4) delimited size
                  into t5-del
           end-string.
           if tor-ritira-si
              move "RITIRA IN LUBEX" to t5-ritira
           else
              move spaces            to t5-ritira
           end-if.
           move t5           to spl-riga-stampa.
           perform SCRIVI.

           add 0,3 to spl-riga.

           move tor-note1 to t6-note1.
           string tor-data-note1(7:2) delimited size
                  "/"                 delimited size
                  tor-data-note1(5:2) delimited size
                  "/"                 delimited size
                  tor-data-note1(1:4) delimited size
                  into t6-data
           end-string.
           move tor-note2 to t6-note2.
           move t6        to spl-riga-stampa.
           perform SCRIVI.

           move tor-note3 to t7-note3.
           move tor-note4 to t7-note4.
           move t7        to spl-riga-stampa.
           perform SCRIVI.
                          
           move divisorio to spl-riga-stampa.
           perform SCRIVI.
           
           move testata  to spl-riga-stampa.
           perform SCRIVI.

           move divisorio to spl-riga-stampa.
           perform SCRIVI.

      ***---
       CONFRONTA-FILES.
           move 0 to idx.
           move low-value to cror-rec2.
           start check-rordini2 key >= cror-chiave2 end-start.
           perform until 1 = 2
              read check-rordini2 next at end exit perform end-read
              move cror-chiave2 to cror-chiave
              read check-rordini no lock
                   invalid
                   add  1 to idx

                   move cror-prg-cod-articolo2 to ror-cod-articolo
                   move cror-num-colli2        to ror-num-colli
                   move cror-blister2          to ror-blister
                   move cror-qta-imballi2      to ror-qta-imballi
                   move cror-des-imballo2      to ror-des-imballo
                   move cror-qta2              to ror-qta

                   compute ror-prz-unitario =
                           cror-importo2 / cror-qta2

                   move 1 to sw-aggiunte
                   perform ELEMENTO
                   set el-aggiunta(idx)  to true
                   set el-qta-pos(idx)   to true
                   add ror-num-colli to tot-colli-aggiunte
                   compute tot-utf-aggiunte  =
                           tot-utf-aggiunte  +
                         ( ror-qta * cror-peso-utf2)

                   compute tot-non-utf-aggiunte =
                           tot-non-utf-aggiunte +
                         ( ror-qta * cror-peso-non-utf2)

                   compute tot-peso-aggiunte =
                           tot-peso-aggiunte +
                         ( ror-qta * cror-peso-utf2 ) +
                         ( ror-qta * cror-peso-non-utf2 )
               not invalid
                   if cror-qta     not = cror-qta2 |or
      *****                cror-importo not = cror-importo2
                      add 1 to idx
                      move cror-rec2 to ror-rec
                      move 1 to sw-modificate

                      move cror-prg-cod-articolo to ror-cod-articolo
                      move cror-blister          to ror-blister
                      move cror-qta-imballi      to ror-qta-imballi
                      move cror-des-imballo      to ror-des-imballo
                      evaluate true
                      when cror-qta2 > cror-qta
                           compute ror-qta = cror-qta2 - cror-qta
                      when cror-qta2 < cror-qta
                           compute ror-qta = cror-qta - cror-qta2
                      when other
                           move cror-qta2 to ror-qta
                      end-evaluate
      
                      compute ror-prz-unitario =
                              cror-importo2 / cror-qta2

      *****                compute importo-singolo =
      *****                        cror-importo / cror-qta
      *****
      *****                compute importo-singolo2 =
      *****                        cror-importo2 / cror-qta2
      *****
      *****                evaluate true
      *****                when importo-singolo2 > importo-singolo
      *****                     compute ror-prz-unitario = 
      *****                             cror-importo2 - 
      *****                             cror-importo
      *****                when importo-singolo2 < importo-singolo
      *****                     compute ror-prz-unitario = 
      *****                             cror-importo - 
      *****                             cror-importo2
      *****                when other
      *****                     move importo-singolo to ror-prz-unitario
      *****                end-evaluate

                      evaluate true
                      when cror-num-colli2 > cror-num-colli
                           compute ror-num-colli  = 
                                  cror-num-colli2 - 
                                  cror-num-colli
                      when cror-num-colli2 < cror-num-colli
                           compute ror-num-colli = 
                                  cror-num-colli - 
                                  cror-num-colli2
                      when other
                           move cror-num-colli2 to ror-num-colli
                      end-evaluate

                      perform ELEMENTO

                      if cror-qta2 < cror-qta
                         set el-qta-neg(idx)   to true
                      else
                         set el-qta-pos(idx)   to true
                      end-if

                      set el-modificata(idx) to true

                      if el-qta-pos(idx)
                         add ror-num-colli to tot-colli-modificate
                         compute tot-utf-modificate  =
                                 tot-utf-modificate  +
                               ( ror-qta * cror-peso-utf)

                         compute tot-non-utf-modificate =
                                 tot-non-utf-modificate +
                               ( ror-qta * cror-peso-non-utf)

                         compute tot-peso-modificate =
                                 tot-peso-modificate +
                                 ( ror-qta * cror-peso-utf ) +
                                 ( ror-qta * cror-peso-non-utf)
                      else
                        subtract ror-num-colli from tot-colli-modificate
                         compute tot-utf-modificate  =
                                 tot-utf-modificate  -
                               ( ror-qta * cror-peso-utf)

                         compute tot-non-utf-modificate =
                                 tot-non-utf-modificate -
                               ( ror-qta * cror-peso-non-utf)

                         compute tot-peso-modificate =
                                 tot-peso-modificate -
                                 ( ror-qta * cror-peso-utf ) -
                                 ( ror-qta * cror-peso-non-utf)
                      end-if 
                   end-if
              end-read
           end-perform.  

           move low-value to cror-rec.
           start check-rordini key >= cror-chiave end-start.
           perform until 1 = 2
              read check-rordini next at end exit perform end-read
              move cror-chiave to cror-chiave2
              read check-rordini2 no lock
                   invalid
                   add  1 to idx
                   move cror-prg-cod-articolo to ror-cod-articolo
                   move cror-num-colli        to ror-num-colli
                   move cror-blister          to ror-blister
                   move cror-qta-imballi      to ror-qta-imballi
                   move cror-des-imballo      to ror-des-imballo
                   move cror-qta              to ror-qta

                   compute ror-prz-unitario =
                           cror-importo / cror-qta

                   move 1 to sw-eliminate
                   perform ELEMENTO
                   set el-eliminata(idx) to true
                   set el-qta-neg(idx)   to true
                   add ror-num-colli to tot-colli-eliminate
                   compute tot-utf-eliminate  =
                           tot-utf-eliminate  +
                         ( ror-qta * cror-peso-utf)

                   compute tot-non-utf-eliminate =
                           tot-non-utf-eliminate +
                         ( ror-qta * cror-peso-non-utf)

                   compute tot-peso-eliminate =
                           tot-peso-eliminate +
                         ( ror-qta * cror-peso-utf ) +
                         ( ror-qta * cror-peso-non-utf)
              end-read
           end-perform.

           if sw-aggiunte = 1
              add 2   to riga
              add 0,5 to spl-riga
              perform STAMPA-AGGIUNTE
              add 1   to riga
           end-if.

           if sw-modificate = 1
              add 2   to riga
              add 0,5 to spl-riga
              perform STAMPA-MODIFICATE
              add 1   to riga
           end-if.

           if sw-eliminate = 1
              add 2   to riga
              add 0,5 to spl-riga
              perform STAMPA-ELIMINATE
              add 1   to riga
           end-if.

      ***---
       ELEMENTO.
           move ror-cod-articolo to r-art art-codice.
           read articoli no lock 
                invalid move spaces to art-descrizione 
           end-read.

           move ror-num-colli   to r-colli.
           move art-descrizione to r-des.

           if ror-si-blister
              move ror-des-imballo to r-imb
           else
              move ror-qta-imballi to imballi-ed
              inspect ror-des-imballo replacing trailing 
                                       spaces by low-value
              call "C$JUSTIFY" using imballi-ed, "L"
              initialize r-imb
              string  ror-des-imballo delimited by low-value
                      " da "           delimited by size
                      imballi-ed       delimited by spaces
                      " x "            delimited by size
                      art-udm-imballo  delimited by size
                      into r-imb
              end-string
           end-if.
           
           move ror-qta           to r-qta.
           move ror-prz-unitario  to r-prz.

           move r-riga to el-r-riga(idx).
           move ror-prz-unitario  to el-r-prz(idx).
      *****     move 0      to el-r-prz(idx).

      ***---
       STAMPA-AGGIUNTE.
           move 0 to sw-font.
           perform varying idx from 1 by 1 
                     until idx > 999
              if el-r-des(idx) = spaces
                 exit perform
              end-if

              if el-aggiunta(idx)

                 if riga >= MaxRighe
                    perform SALTO-PAGINA
                 end-if

                 if sw-font = 0
                    move 1 to sw-font
                    move CourierNew12B    to spl-hfont
                    move "RIGHE AGGIUNTE" to spl-riga-stampa
                    perform SCRIVI
                    move CourierNew10     to spl-hfont
                 end-if

                 move spaces         to el-tipo(idx)
                 move el-r-riga(idx) to spl-riga-stampa
                 perform SCRIVI
              end-if

           end-perform.

           if riga >= MaxRighe - 6
              perform SALTO-PAGINA
           end-if.
                                                
           add 0,2                 to spl-riga.
           move CourierNew12B      to spl-hfont.
           move tot-colli-aggiunte to r-colli-t.
           move r-colli-tot        to spl-riga-stampa.
           perform SCRIVI.

           move tot-utf-aggiunte to r-peso-utf.
           move r-peso-t-utf     to spl-riga-stampa.
           perform SCRIVI.

           move tot-non-utf-aggiunte to r-peso-non-utf.
           move r-peso-t-non-utf     to spl-riga-stampa.
           perform SCRIVI.

           move tot-peso-aggiunte to r-peso-tot.
           move r-peso-t-tot      to spl-riga-stampa.
           perform SCRIVI.

      ***---
       STAMPA-MODIFICATE.   
           move 0 to sw-font.
           perform varying idx from 1 by 1 
                     until idx > 999
              if el-r-des(idx) = spaces
                 exit perform
              end-if

              if el-modificata(idx)

                 if riga >= MaxRighe
                    perform SALTO-PAGINA
                 end-if
                                   
                 if sw-font = 0
                    move 1 to sw-font
                    move CourierNew12B      to spl-hfont
                    move "RIGHE MODIFICATE" to spl-riga-stampa
                    perform SCRIVI
                    move CourierNew10     to spl-hfont
                 end-if
                                       
                 move spaces         to el-tipo(idx)
                 move el-r-riga(idx) to spl-riga-stampa
                 perform SCRIVI
              end-if

           end-perform.

           if riga >= MaxRighe - 6
              perform SALTO-PAGINA
           end-if.
                                             
           add 0,2                   to spl-riga.
           move CourierNew12B        to spl-hfont.
           move tot-colli-modificate to r-colli-t.
           move r-colli-tot          to spl-riga-stampa.
           perform SCRIVI.

           move tot-utf-modificate to r-peso-utf.
           move r-peso-t-utf       to spl-riga-stampa.
           perform SCRIVI.

           move tot-non-utf-modificate to r-peso-non-utf.
           move r-peso-t-non-utf       to spl-riga-stampa.
           perform SCRIVI.

           move tot-peso-modificate to r-peso-tot.
           move r-peso-t-tot        to spl-riga-stampa.
           perform SCRIVI.

      ***---
       STAMPA-ELIMINATE.
           move 0 to sw-font.
           perform varying idx from 1 by 1 
                     until idx > 999
              if el-r-des(idx) = spaces
                 exit perform
              end-if

              if el-eliminata(idx)

                 if riga >= MaxRighe
                    perform SALTO-PAGINA
                 end-if
                                   
                 if sw-font = 0
                    move 1 to sw-font
                    move CourierNew12B     to spl-hfont
                    move "RIGHE ELIMINATE" to spl-riga-stampa
                    perform SCRIVI
                    move CourierNew10     to spl-hfont
                 end-if
                                               
                 move spaces         to el-tipo(idx)
                 move el-r-riga(idx) to spl-riga-stampa
                 perform SCRIVI
              end-if

           end-perform.

           if riga >= MaxRighe - 6
              perform SALTO-PAGINA
           end-if.

           move CourierNew12B       to spl-hfont.
           add 0,2                  to spl-riga.
           move tot-colli-eliminate to r-colli-t.
           move r-colli-tot         to spl-riga-stampa.
           perform SCRIVI.

           move tot-utf-eliminate to r-peso-utf.
           move r-peso-t-utf      to spl-riga-stampa.
           perform SCRIVI.

           move tot-non-utf-eliminate to r-peso-non-utf.
           move r-peso-t-non-utf      to spl-riga-stampa.
           perform SCRIVI.

           move tot-peso-eliminate to r-peso-tot.
           move r-peso-t-tot       to spl-riga-stampa.
           perform SCRIVI.
           

      ***---
       SCRIVI.
           if testata-bold
              subtract 0,5 from spl-riga
              move 17,6      to spl-colonna
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

           inspect WFONT-NAME replacing trailing SPACE by LOW-VALUE.
           move WFONT-SIZE    to FONT-SIZE-DPLY.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited LOW-VALUE
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio

           inspect messaggio replacing trailing SPACE by LOW-VALUE.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close tordini rordini  articoli clienti destini.

           if modifica
              close check-rordini check-rordini2
              delete file check-rordini2
           end-if.
                           
      ***---
       EXIT-PGM.
           destroy TimesNewRoman20B 
                   CourierNew10 
                   CourierNew10B
                   CourierNew12B.

           goback.
