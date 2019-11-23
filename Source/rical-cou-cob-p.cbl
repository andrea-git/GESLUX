       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rical-cou-cob-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Ricalcolo COU e COBAT su master riga non chiusa, tutti i clienti

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "articoli.sl".
           copy "timposte.sl".
           copy "tmarche.sl".
           copy "clienti.sl".      
           copy "ttipocli.sl".
           copy "progmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "articoli.fd".
           copy "timposte.fd".
           copy "tmarche.fd".
           copy "clienti.fd".
           copy "ttipocli.fd".
           copy "progmag.fd".

       WORKING-STORAGE SECTION.
           copy "imposte.def".

      *    COSTANTI
       78  titolo            value "Ricalcolo COU".

      *    FILE STATUS
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-articoli       pic xx.
       77  status-timposte       pic xx.       
       77  status-tmarche        pic xx.
       77  status-clienti        pic xx.
       77  status-ttipocli       pic xx.
       77  status-progmag        pic xx.

      * VARIABILI
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  righe                 pic 9(7).
       77  righe-ed              pic z.zzz.zz9.

      * FLAGS
       01  controlli             pic xx.
           88 errori             value "ER".
           88 tutto-ok           value "OK".

       LINKAGE SECTION.
       77  link-handle          handle of window.

      ******************************************************************
       PROCEDURE DIVISION USING link-handle.

       DECLARATIVES.
      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "35"
                display message "File [MTORDINI] not found!"
                           title titolo
                            icon 3
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
                display message "File [MRORDINI] not found!"
                           title titolo
                            icon 3
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
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "File [ARTICOLI] not found!"
                           title titolo
                            icon 3
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
 
      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           set tutto-ok  to true.
           evaluate status-timposte
           when "35"
                display message "File [TIMPOSTE] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TIMPOSTE] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TIMPOSTE] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0       to counter counter2 righe.
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open input mtordini articoli timposte clienti ttipocli 
                      tmarche progmag.
           open i-o   mrordini.
      
      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.
           initialize mto-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           set mto-attivo to true.
           start mtordini key >= k-mto-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    if mto-chiuso
                       exit perform 
                    end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 20 line 09
                       move 0 to counter2
                    end-if 

                    set  cli-tipo-C  to true
                    move mto-cod-cli to cli-codice
                    read clienti no lock
                         invalid continue
                     not invalid
                         move cli-tipo to tcl-codice
                         read ttipocli no lock
                              invalid continue
                         end-read
                    end-read  
                    if ttipocli-gdo set TrattamentoGDO to true
                    else            set TrattamentoGDO to false
                    end-if

                    perform LOOP-RIGHE

                 end-perform
           end-start.
           move righe to righe-ed.
           display message
                     "Elaborazione terminata su ", righe-ed, " righe"
                     title titolo.

      ***---
       LOOP-RIGHE.
           move mto-chiave to mro-chiave-testa.
           move low-value  to mro-riga.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if

                    if mro-chiuso
                       continue
                    else
                       if mro-no-omaggio and mro-imponib-merce not = 0
                          move mro-prg-chiave to prg-chiave
                          read progmag no lock
                          move mro-cod-articolo to art-codice
                          read articoli no lock
                               invalid continue
                           not invalid 
                               move art-marca-prodotto to mar-codice
                               read tmarche no lock
                               perform CALCOLA-IMPOSTE

                               evaluate true   
                               when ImpostaCou
                                    move imposta-cou 
                                      to mro-imp-cou-cobat

                                    if TrattamentoGDO
           
                                       compute mro-imponib-merce = 
                                               mro-prz-unitario  -
                                               mro-imp-consumo   -
                                               mro-imp-cou-cobat -
                                               mro-add-piombo
                                    end-if

                                    rewrite mro-rec
                                            invalid continue
                                        not invalid add 1 to righe
                                    end-rewrite
                               when ImpostaCobat
                                    move imposta-cobat
                                      to mro-imp-cou-cobat

                                    if TrattamentoGDO
           
                                       compute mro-imponib-merce = 
                                               mro-prz-unitario  -
                                               mro-imp-consumo   -
                                               mro-imp-cou-cobat -
                                               mro-add-piombo
                                    end-if

                                    rewrite mro-rec
                                            invalid continue
                                        not invalid add 1 to righe
                                    end-rewrite
                               end-evaluate

                          end-read
                       end-if
                    end-if

                 end-perform
           end-start.              

      ***---
       CLOSE-FILES.
           close mtordini mrordini articoli timposte tmarche clienti
                 ttipocli progmag.

      ***---
       EXIT-PGM.
           display "                               "
              upon link-handle  at column 08 line 09.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "imposte.cpy".
