       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      aggstatmese.
       AUTHOR.                          Andrea.
       REMARKS. Crea il file statmese valorizzandone i dati
                dai documenti di vendita (fatture, note credito)
                suddividendoli per giorno.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tcaumag.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "statmese.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tcaumag.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "statmese.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Statistiche giornaliere per mese".

      * FILE-STATUS              
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-statmese       pic xx.
       77  status-tcaumag        pic xx.

      * VARIABILI
       77  mese                  pic 99.
       77  start-data            pic 9(8).
       77  como-valore           pic s9(12)v99.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

      * FLAGS
       01  controlli             pic xx.
         88 errori               value "ER".
         88 tutto-ok             value "OK".

       01  FlagTrovato           pic 9.
         88 trovato              value 1, false 0.

       LINKAGE SECTION.
       77  link-data             pic 9(8).
       77  scr-oper-handle       handle of window.

       PROCEDURE DIVISION USING link-data, scr-oper-handle.

       DECLARATIVES.
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                set errori to true
                display message "File [TCAUMAG] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TCAUMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.

       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                set errori to true
                display message "File [TMOVMAG] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate

       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                set errori to true
                display message "File [RMOVAMG] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [RMOVAMG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RMOVAMG] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.

       STATMESE-ERR SECTION.
           use after error procedure on statmese.
           set tutto-ok  to true.
           evaluate status-statmese
           when "35"
                set errori to true
                display message "File [STATMESE] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [STATMESE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STATMESE] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "statmese"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output statmese
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
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
           move 0 to counter counter2.
           move link-data      to start-data.
           move 01             to start-data(7:2).
           move link-data(5:2) to mese.
           set  trovato        to false.
           set  tutto-ok       to true.

      ***---
       OPEN-FILES.
           open input tmovmag rmovmag tcaumag.
           if tutto-ok
              open output statmese
              if tutto-ok
                 close statmese
                 open i-o statmese
              else
                 close tmovmag rmovmag tcaumag
              end-if
           end-if.

      ***---
       ELABORAZIONE.
           move low-value    to tmo-rec.
           move start-data   to tmo-data-movim.
           start tmovmag key is >= k-data
                 invalid set errori to true
           end-start.

           if tutto-ok

              |RIPULISCO LA SCREEN DAL CONTATORE
              display "                          "
                 upon scr-oper-handle at column 34
                                           line 25
              display "                          "
                 upon scr-oper-handle at column 30
                                           line 26
              ||||||||

              perform until 1 = 2
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon scr-oper-handle at column 34
                                                 line 25
                    move 0 to counter2
                    if counter = 100
                       display "STATMESE COUNTER" 
                          upon scr-oper-handle at column 30
                                                    line 26
                    end-if
                 end-if

                 read tmovmag next at end      exit perform end-read
                 if tmo-data-movim > link-data exit perform end-if

                 move tmo-causale to tca-codice
                 read tcaumag no lock invalid continue end-read
                 if tca-cliente and tca-si-stat
                    perform LOOP-RIGHE
                 end-if

              end-perform
           end-if.

      ***---
       LOOP-RIGHE.
           set  tutto-ok to true.
           move low-value  to rmo-rec.
           move tmo-chiave to rmo-chiave.
           start rmovmag key is >= rmo-chiave 
                 invalid set errori to true 
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read rmovmag next at end exit perform end-read
                 if rmo-anno  not = tmo-anno or
                    rmo-movim not = tmo-numero
                    exit perform
                 end-if

                 initialize stmese-rec replacing numeric by zeroes
                                            alphanumeric by spaces
                 move tmo-data-movim(7:2) to stmese-giorno
                 read statmese
                      invalid move 0 to stmese-fatt
                 end-read

                 if rmo-qta = 0 move 1 to rmo-qta end-if

                 compute como-valore =
                      (( rmo-netto    +
                         rmo-imp-cons +
                         rmo-coubat ) * rmo-qta )

                 if tca-imponibile-pos
                    add      como-valore to   stmese-fatt
                 else
                    subtract como-valore from stmese-fatt
                 end-if 

                 write stmese-rec invalid rewrite stmese-rec end-write
              end-perform
           end-if.

      ***---
       CLOSE-FILES.
           close tmovmag rmovmag statmese tcaumag.

      ***---
       EXIT-PGM.
           goback.
