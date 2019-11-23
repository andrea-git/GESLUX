       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rical-cou-cob-ev-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Ricalcolo COU e COBAT su evasioni non fatturate, tutti i clienti

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           copy "tordini.sl".
           copy "rordini.sl".
           copy "articoli.sl".
           copy "timposte.sl".
           copy "tmarche.sl".
           copy "clienti.sl".      
           copy "ttipocli.sl".
           copy "progmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "tordini.fd".
           copy "rordini.fd".
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
       77  status-tordini       pic xx.
       77  status-rordini       pic xx.
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
           open input tordini articoli timposte clienti ttipocli 
                      tmarche progmag.
           open i-o   rordini.
      
      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.    
           initialize tor-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           start tordini key >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno-fattura not = 0
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

                    perform LOOP-RIGHE

                 end-perform
           end-start.
           move righe to righe-ed.
           display message
                     "Elaborazione terminata su ", righe-ed, " righe"
                     title titolo.

      ***---
       LOOP-RIGHE.
           move tor-chiave to ror-chiave.
           move low-value  to ror-num-riga.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if

                    if ror-no-omaggio and ror-imponib-merce not = 0
                       move ror-prg-chiave to prg-chiave
                       read progmag no lock
                       move ror-cod-articolo to art-codice
                       read articoli no lock
                            invalid continue
                        not invalid 
                            move art-marca-prodotto to mar-codice
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
                                     not invalid add 1 to righe
                                 end-rewrite
                            when ImpostaCobat
                                 move imposta-cobat
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
                                     not invalid add 1 to righe
                                 end-rewrite
                            end-evaluate

                       end-read
                    end-if

                 end-perform
           end-start.              

      ***---
       CLOSE-FILES.
           close tordini rordini articoli timposte tmarche clienti
                 ttipocli progmag.

      ***---
       EXIT-PGM.
           display "                               "
              upon link-handle  at column 08 line 09.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "imposte.cpy".
