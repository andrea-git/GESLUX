       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rical-piombo-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Ricalcolo add. piombo su ordini con cliente trattamento
           imposte GDO e non ancora fatturati

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "ttipocli.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tpiombo.sl".
           copy "articoli.sl".
           copy "timposte.sl".
           copy "param.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "ttipocli.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tpiombo.fd".
           copy "articoli.fd".
           copy "timposte.fd".
           copy "param.fd".

       WORKING-STORAGE SECTION.
           copy "common-excel.def".
           copy "link-geslock.def".
           copy "acugui.def".
           copy "imposte.def".
           copy "trova-parametro.def".

      *    COSTANTI
       78  titolo            value "Ricalcolo add.le piombo".

      *    FILE STATUS
       77  status-clienti        pic xx.
       77  status-ttipocli       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tpiombo        pic xx.
       77  status-articoli       pic xx.
       77  status-timposte       pic xx.
       77  status-param          pic xx.

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
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message "File [TTIPOCLI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TTIPOCLI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TTIPOCLI] Indexed file corrupt!"
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
                display message "File [CLIENTI] not found!"
                           title titolo
                            icon 3
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
       TPIOMBO-ERR SECTION.
           use after error procedure on tpiombo.
           set tutto-ok  to true.
           evaluate status-tpiombo
           when "35"
                display message "File [TPIOMBO] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TPIOMBO] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TPIOMBO] Indexed file corrupt!"
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
           open input tordini articoli ttipocli tpiombo clienti 
                      timposte param.
           open i-o   rordini.
      
      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.
           move 0 to tor-anno-fattura tor-num-fattura.
           start tordini key >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno-fattura  not = 0 or
                       tor-num-fattura   not = 0
                       exit perform 
                    end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 28 line 09
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
                          not invalid
                              if ttipocli-gdo
                                 perform LOOP-RIGHE
                              end-if
                         end-read
                    end-read

                 end-perform
           end-start.
           move righe to righe-ed.
           display message 
                     "Elaborazione terminata su ", righe-ed, " righe"
                     title titolo.

      ***---
       LOOP-RIGHE.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           move low-value  to ror-num-riga.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno  or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if

                    if ror-no-omaggio and ror-imponib-merce not = 0
                       move ror-cod-articolo to art-codice
                       read articoli no lock
                            invalid continue
                        not invalid
                            perform CALCOLA-IMPOSTE
                       end-read
                    end-if

                 end-perform
           end-start.

      ***---
       CALCOLA-IMPOSTE.
           move 0 to risultato-imposte
                     imposta-cobat.

           if art-si-cobat
              move 0 to ror-imp-cou-cobat
              move 0 to ror-add-piombo

              set ImpostaCobat to true
              evaluate true
              when art-auto-cobat
                   evaluate true
                   when   art-amperaggio >= imp-cb-auto-sca-1-da and
                          art-amperaggio <= imp-cb-auto-sca-1-a
                          move imp-cb-auto-sca-1-euro
                            to risultato-imposte
                   when   art-amperaggio >= imp-cb-auto-sca-2-da and
                          art-amperaggio <= imp-cb-auto-sca-2-a
                          move imp-cb-auto-sca-2-euro
                            to risultato-imposte
                   when   art-amperaggio >= imp-cb-auto-sca-3-da and
                          art-amperaggio <= imp-cb-auto-sca-3-a
                          move imp-cb-auto-sca-3-euro
                            to risultato-imposte
                   when   art-amperaggio >= imp-cb-auto-sca-4-da and
                          art-amperaggio <= imp-cb-auto-sca-4-a
                          move imp-cb-auto-sca-4-euro
                            to risultato-imposte
                   when   art-amperaggio >= imp-cb-auto-sca-5-da and
                          art-amperaggio <= imp-cb-auto-sca-5-a
                          move imp-cb-auto-sca-5-euro
                            to risultato-imposte
                   end-evaluate

              when art-moto-cobat
                   evaluate true
                   when art-amperaggio >= imp-cb-scooter-sca-1-da and
                        art-amperaggio <= imp-cb-scooter-sca-1-a
                        move imp-cb-scooter-sca-1-euro
                          to risultato-imposte
                   when art-amperaggio >= imp-cb-scooter-sca-2-da and
                        art-amperaggio <= imp-cb-scooter-sca-2-a
                        move imp-cb-scooter-sca-2-euro
                          to risultato-imposte
                   when art-amperaggio >= imp-cb-scooter-sca-3-da and
                        art-amperaggio <= imp-cb-scooter-sca-3-a
                        move imp-cb-scooter-sca-3-euro
                          to risultato-imposte
                   end-evaluate
              end-evaluate

              add 0,005              to risultato-imposte
              move risultato-imposte to ror-imp-cou-cobat
                 
              if tcl-si-piombo
                 move art-marca-prodotto to tpb-marca
                 move tor-data-ordine    to como-data-ordine tpb-data
                 move ror-prz-unitario   to como-prz-unitario
                 move ror-imp-cou-cobat  to como-imp-cou-cobat
                 move tor-cod-cli        to como-prm-cliente
                 move tor-prg-destino    to como-prm-destino
                 perform ADDIZIONALE-PIOMBO
                 move add-piombo to ror-add-piombo
              end-if

              compute ror-imponib-merce = 
                      ror-prz-unitario  -
                      ror-imp-consumo   -
                      ror-imp-cou-cobat -
                      ror-add-piombo

              rewrite ror-rec
                      invalid continue 
                  not invalid add 1 to righe
              end-rewrite

           end-if.

      ***---
       CLOSE-FILES.
           close tordini  rordini clienti param
                 articoli tpiombo ttipocli timposte.

      ***---
       EXIT-PGM.
           display "                               "
              upon link-handle  at column 28 line 09.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "addizionale-piombo.cpy".
           copy "trova-parametro.cpy".
