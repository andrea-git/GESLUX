       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-saldi-f.
       AUTHOR.                          Andrea.
       REMARKS.
           Serve per valorizzare a TRUE il flag dei saldi sia su
           fornitori che su destini. I saldi saranno tenuti e l'ordine
           NON verrà chiuso alla prima evasione.
           Il flag sui FORNITORI ERA quello SALDI, che ora è diventato 
           saldi-banco su clienti/fornitori, mentre è rimasto standard 
           sui destini.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destinif.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "destinif.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Settaggio flag".

      * FILE-STATUS
       77  status-clienti          pic xx.
       77  status-destinif         pic xx.

      * VARIABILI

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

      *****************************************************************

       PROCEDURE DIVISION.

       DECLARATIVES.
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                set errori to true
                display message "File [clienti] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [clienti] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[clienti] Indexed file corrupt!"
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
                move   "clienti"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o clienti allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       DESTINIF-ERR SECTION.
           use after error procedure on destinif.
           set tutto-ok  to true.
           evaluate status-destinif
           when "35"
                set errori to true
                display message "File [DESTINIF] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [DESTINIF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DESTINIF] Indexed file corrupt!"
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
                move   "DESTINIF"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o destinif allowing readers
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
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open i-o clienti  allowing readers.
           open i-o destinif allowing readers.

      ***---
       ELABORAZIONE.
           move low-value to cli-rec.
           set cli-tipo-F to true.
           start clienti key >= cli-chiave.
           perform until 1 = 2 
              read clienti next at end exit perform end-read
              if cli-tipo-C exit perform end-if
              set cli-saldi-banco-si to true
              rewrite cli-rec invalid continue end-rewrite
           end-perform.

           move low-value to desf-rec.
           start destinif key >= desf-chiave.
           perform until 1 = 2 
              read destinif next at end exit perform end-read
              set desf-saldi-si to true
              rewrite desf-rec invalid continue end-rewrite
           end-perform.

           display message "Operazione terminata!"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close clienti destinif.

      ***---
       EXIT-PGM.
           goback.
