       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rical-cou-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Ricalcolo COU su:
           - master riga non chiusa, 
           - evasioni non fatturate
           - ordini fornitori registrati
           
           tutti i clienti

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
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tordforn.sl".
           copy "rordforn.sl".

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
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".

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
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.

      * VARIABILI
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  righe-ror             pic 9(7).
       77  righe-mro             pic 9(7).
       77  righe-rof             pic 9(7).

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
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                display message "File [TORDINI] not found!"
                           title titolo
                            icon 3
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
                display message "File [RORDINI] not found!"
                           title titolo
                            icon 3
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
       TORDFORN-ERR SECTION.
           use after error procedure on tordforn.
           set tutto-ok  to true.
           evaluate status-tordforn
           when "35"
                display message "File [TORDFORN] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TORDFORN] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TORDFORN] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.          

      ***---
       RORDFORN-ERR SECTION.
           use after error procedure on rordforn.
           set tutto-ok  to true.
           evaluate status-rordforn
           when "35"
                display message "File [RORDFORN] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [RORDFORN] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[RORDFORN] Indexed file corrupt!"
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
           move 0       to counter counter2 
                           righe-ror righe-mro righe-rof.
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open input mtordini articoli timposte clienti ttipocli 
                      tmarche progmag tordini tordforn.
           open i-o   mrordini rordini rordforn.
      
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

           perform ELABORAZIONE-MASTER-NON-CHIUSI.
           perform ELABORAZIONE-EVASIONI-NON-FATTURATE.
           perform ELABORAZIONE-ORDINIF-INSERITI.

           display "                               "
              upon link-handle  at column 0 line 22.

           display message
                     "Elaborazione terminata su: ", 
              x"0d0a""- Master: " righe-mro, " righe"
              x"0d0a""- Evasioni: " righe-ror, " righe"
              x"0d0a""- Ordini f: " righe-rof, " righe"
                   title titolo.

      ***---
       ELABORAZIONE-MASTER-NON-CHIUSI.
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
                          upon link-handle at column 0 line 22
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

      ***---
       ELABORAZIONE-EVASIONI-NON-FATTURATE.
           move low-value to tor-rec.
           start tordini key >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno-fattura > 0 or
                       tor-num-fattura  > 0
                       exit perform
                    end-if 

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 0 line 22
                       move 0 to counter2
                    end-if

                    if tor-data-fattura > 0 or
                       tor-num-prenot   > 0
                       exit perform cycle
                    end-if    

                    set  cli-tipo-C  to true
                    move tor-cod-cli to cli-codice
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

                    move low-value  to ror-rec
                    move tor-anno   to ror-anno
                    move tor-numero to ror-num-ordine
                    start rordini key >= ror-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read rordini next 
                               at end exit perform 
                             end-read
                             if ror-anno       not = tor-anno or
                                ror-num-ordine not = tor-numero
                                exit perform
                             end-if
                             if ror-no-omaggio and 
                                ror-imponib-merce not = 0
                                move ror-prg-chiave to prg-chiave
                                read progmag no lock
                                move ror-cod-articolo to art-codice
                                read articoli no lock
                                     invalid continue
                                 not invalid 
                                     move art-marca-prodotto 
                                       to mar-codice
                                     read tmarche no lock
                                     perform CALCOLA-IMPOSTE
                             
                                     evaluate true   
                                     when ImpostaCou
                                          move imposta-cou 
                                            to ror-imp-cou-cobat
                             
                                          if TrattamentoGDO
                             
                                             compute ror-imponib-merce = 
                                                     ror-prz-unitario  -
                                                     ror-imp-consumo   -
                                                     ror-imp-cou-cobat -
                                                     ror-add-piombo
                                          end-if
                             
                                          rewrite ror-rec
                                                  invalid continue
                                              not invalid 
                                                  add 1 to righe-ror
                                          end-rewrite
                                     end-evaluate
                             
                                end-read
                             end-if
                          end-perform
                    end-start
                 end-perform
           end-start.
        
      ***---
       ELABORAZIONE-ORDINIF-INSERITI.   
           move low-value to tof-rec.
           set tof-inserito to true.
           start tordforn key >= tof-k-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordforn next at end exit perform end-read
                    if tof-inviato or tof-in-lavorazione or tof-chiuso 
                       exit perform
                    end-if   

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 0 line 22
                       move 0 to counter2
                    end-if 

                    set  cli-tipo-C  to true
                    move tor-cod-cli to cli-codice
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

                    move low-value  to rof-rec
                    move tof-anno   to rof-anno
                    move tof-numero to rof-numero
                    start rordforn key >= rof-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read rordforn next 
                               at end exit perform 
                             end-read
                             if rof-anno   not = tof-anno or
                                rof-numero not = tof-numero
                                exit perform
                             end-if
                             if rof-imponib-merce > 0
                                move rof-prg-chiave to prg-chiave
                                read progmag no lock
                                move rof-cod-articolo to art-codice
                                read articoli no lock
                                     invalid continue
                                 not invalid 
                                     move art-marca-prodotto 
                                       to mar-codice
                                     read tmarche no lock
                                     perform CALCOLA-IMPOSTE
                             
                                     evaluate true   
                                     when ImpostaCou
                                          move imposta-cou 
                                            to rof-imp-cou-cobat
                             
                                          compute rof-imponib-merce = 
                                                  rof-prz-unitario  -
                                                  rof-imp-consumo   -
                                                  rof-imp-cou-cobat -
                                                  rof-add-piombo
                                          
                                          rewrite rof-rec
                                                  invalid continue
                                              not invalid 
                                                  add 1 to righe-rof
                                          end-rewrite
                                     end-evaluate
                             
                                end-read
                             end-if
                          end-perform
                    end-start
                 end-perform
           end-start.
           


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
                                        not invalid add 1 to righe-mro
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
                 ttipocli progmag tordini rordini.

      ***---
       EXIT-PGM.  
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "imposte.cpy".
