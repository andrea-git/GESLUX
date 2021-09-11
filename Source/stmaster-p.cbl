       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stmaster-p.
       AUTHOR.                          Luciano.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destini.sl".
           copy "mtordini.sl". 
           copy "mrordini.sl".
           copy "articoli.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "destini.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "articoli.fd".
           copy "ttipocli.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "fonts.def".
           copy "acugui.def".
           copy "spooler.def".
           copy "selprint.lks".
       
       78  titolo                value "GESLUX - Stampa Ordine".
       78  MaxRighe              value 54.
                                 
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-articoli       pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-ttipocli       pic xx.

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
       77  imballi-ed             pic z(4).
       77  tot-colli              pic 9(5).
       77  tot-peso-tot           pic 9(6)v999.
       77  tot-peso-utf           pic 9(6)v999.
       77  tot-peso-non-utf       pic 9(6)v999.
       77  totale                 pic 9(6)v999.
       77  r-prz-z                pic zz.zz9,99.
       77  codice-x               pic x(5).

      *
       77  filler                pic 9   value 0.
           88  testata-bold      value 1 false 0.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

      * RIGHE PER LA STAMPA
       01  divisorio   pic x(92) value all "=".

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
         05 filler               pic x(2)  value spaces.
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
         05 filler     pic x(9) value "   Prezzo".
         
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
         05 r-prz      pic x(10).

       01  r-totale.
         05 filler     pic x(74).
         05 filler     pic x(8) value "TOTALE: ".
         05 r-tot      pic zzz.zz9,99.

       01  r-colli-tot.
         05 filler     pic x(15) value "*** Tot. colli:".
         05 r-colli-t  pic ----9.

       01  r-peso-t-utf.
         05 filler         pic x(14) value "*** PESO ---> ".
         05 filler         pic x(47).
         05 filler         pic x(5)  value "UTF: ".
         05 r-peso-utf     pic ---.--9,999.

       01  r-peso-t-non-utf.
         05 filler         pic x(57).
         05 filler         pic x(9)  value "NON UTF: ".
         05 r-peso-non-utf pic ---.--9,999.

       01  r-peso-t-tot.
         05 filler         pic x(61).
         05 filler         pic x(5)  value "TOT: ".
         05 r-peso-tot     pic ---.--9,999.

       01  r-data-ora.
         05 filler     pic x(17) value "Stampata in data ".
         05 r-data     pic x(10).
         05 filler     pic x(10) value " alle ore ".
         05 r-ora      pic x(5).

       01                       pic 9.
           88 primo-passaggio   value 1 false zero.
       LINKAGE SECTION.
           copy "link-stmasterp.def".

      ******************************************************************
       PROCEDURE DIVISION using stmasterp-limiti.

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
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [MRORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                display message "File [MRORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[MRORDINI] Indexed file corrupt!"
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
           move 0 to riga.
           set tutto-ok         to true.
           set primo-passaggio  to true
           accept como-data from century-date.
           accept como-ora  from time.

      ***---
       OPEN-FILES.              
           open input mtordini mrordini  clienti destini articoli
                      ttipocli.
      
      ***---
       ELABORAZIONE.
           set  tutto-ok      to true.

           move stmasterp-da-anno to mto-anno.
           move stmasterp-da-num  to mto-numero.

           start mtordini key >= mto-chiave
                 invalid continue
             not invalid perform SCORRI-ORDINI
           end-start.

      ***---
       SCORRI-ORDINI.
           perform until 1 = 2
              read mtordini next at end exit perform end-read

              if mto-anno > stmasterp-a-anno
                 exit perform
              end-if

              if mto-anno = stmasterp-a-anno and 
                    mto-numero > stmasterp-a-num
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
           
           set  cli-tipo-C  to true.
           move mto-cod-cli to cli-codice.
           read clienti no lock invalid continue end-read.
           move cli-tipo to tcl-codice.
           read ttipocli no lock invalid continue end-read.

           if mto-numero < stmasterp-da-num or 
              mto-numero > stmasterp-a-num
              set errori to true
           end-if.

           evaluate true
           when stmasterp-bloccati
                if not mto-bloccato
                   set errori   to true
                end-if
           when stmasterp-tutti
                continue
           end-evaluate

           evaluate true
           when stmasterp-tipo-trad          
                if tcl-gdo-si or tcl-gdo-opz
      *****          if  mto-gdo not = spaces
                   set errori to true
                end-if
           when stmasterp-tipo-gdo
      *****          if  mto-gdo = spaces
                if tcl-gdo-no
                   set errori to true
                end-if
           when stmasterp-tipo-tutti
                continue
           end-evaluate

           if mto-data-creazione < stmasterp-da-data or 
              mto-data-creazione > stmasterp-a-data
              set errori   to true
           end-if.


      ***---
       TRATTA-ORDINE.
           perform STAMPA-TESTA
           if spl-sta-annu
              exit paragraph
           end-if

           move low-value  to mro-rec
           move mto-chiave to mro-chiave
           start mrordini key is >= mro-chiave
              invalid 
                 continue
           end-start
           move  0 to tot-colli
           move 15 to riga
           move  0 to totale
                      tot-peso-utf 
                      tot-peso-non-utf 
                      tot-peso-tot
           perform until 1 = 2
              read mrordini next 
                 at end 
                    exit perform 
              end-read

              if mto-anno   not = mro-anno   or
                 mto-numero not = mro-numero
                 exit perform
              end-if

              move mro-cod-articolo to r-art art-codice
              read articoli no lock 
                 invalid 
                    move spaces to art-descrizione 
              end-read

              move mro-num-colli   to r-colli

              add mro-num-colli    to tot-colli

              compute tot-peso-utf =
                      tot-peso-utf + 
                    ( mro-peso-utf * mro-qta)
              compute tot-peso-non-utf =
                      tot-peso-non-utf + 
                    ( mro-peso-non-utf * mro-qta )
              compute tot-peso-tot =
                      tot-peso-tot + 
                  (( mro-peso-utf + 
                 mro-peso-non-utf ) * mro-qta )

              move art-descrizione to r-des

              compute totale = 
                      totale + ( mro-qta * mro-prz-unitario)

              if mro-si-blister
                 move mro-des-imballo to r-imb
              else
                 move mro-qta-imballi to imballi-ed
                 inspect mro-des-imballo replacing trailing 
                                          spaces by low-value
                 move mro-qta-imballi to imballi-ed
                 call "C$JUSTIFY" using imballi-ed, "L"
                 initialize r-imb
                 string  mro-des-imballo delimited by low-value
                         " da "           delimited by size
                         imballi-ed       delimited by spaces
                         " x "            delimited by size
                         art-udm-imballo  delimited by size
                         into r-imb
                 end-string
              end-if

              move mro-qta           to r-qta
              move mro-imponib-merce to r-prod             

              move r-riga to spl-riga-stampa
              perform SCRIVI
              
              move CourierNew10B     to spl-hfont
              set testata-bold       to true
              move mro-prz-unitario  to r-prz-z
              move r-prz-z           to r-prz(1:9)
              if mro-prz-unitario = 9999999,99 or mro-si-prz-promo
                 move "*"    to r-prz(10:1)
              else
                 move spaces to r-prz(10:1)
              end-if

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
           call   "selprint" using selprint-linkage
           cancel "selprint"

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              set spl-vertical         to true
              set spl-apertura         to true
              move 1                   to spl-margine-sinistro
              move 1                   to spl-margine-destro
              move 1                        to spl-margine-inf
              move "GESLUX - Stampa Ordini" to spl-nome-job
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
           evaluate true
           when stmasterp-bloccati
                move 3,9           to spl-colonna
                move "STAMPA ORDINI MASTER BLOCCATI" to spl-riga-stampa
           when stmasterp-tutti
                move 4,7           to spl-colonna
                move "STAMPA ORDINI MASTER" to spl-riga-stampa
           end-evaluate.
           call "spooler" using spooler-link.

           move CourierNew10 to spl-hfont.

           move 2           to spl-riga.

           if mto-prg-destino not = 0
              move mto-cod-cli     to des-codice
              move mto-prg-destino to des-prog
              read destini no lock invalid continue end-read
           else
              initialize des-indirizzo
                         des-cap
                         des-localita
                         des-prov
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
           if mto-prg-destino not = 0
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

           move mto-num-ord-cli to t5-num-ord-cli.
           string mto-data-ordine(7:2) delimited size
                  "/"                  delimited size
                  mto-data-ordine(5:2) delimited size
                  "/"                  delimited size
                  mto-data-ordine(1:4) delimited size
                  into t5-del
           end-string.
           if mto-ritira-si
              move "RITIRA IN LUBEX" to t5-ritira
           else
              move spaces            to t5-ritira
           end-if.
           move t5           to spl-riga-stampa.
           perform SCRIVI.

           add 0,3 to spl-riga.

           move mto-note1 to t6-note1.
           string mto-data-note1(7:2) delimited size
                  "/"                 delimited size
                  mto-data-note1(5:2) delimited size
                  "/"                 delimited size
                  mto-data-note1(1:4) delimited size
                  into t6-data
           end-string.
           move mto-note2 to t6-note2.
           move t6        to spl-riga-stampa.
           perform SCRIVI.

           move mto-note3 to t7-note3.
           move mto-note4 to t7-note4.
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
           close mtordini mrordini  articoli clienti destini ttipocli.

      ***---
       EXIT-PGM.
           destroy TimesNewRoman20B 
                   CourierNew10 
                   CourierNew10B
                   CourierNew12B.
           goback.
