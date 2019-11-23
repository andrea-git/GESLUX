       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-gg-desf.
       AUTHOR.                          Andrea.
       REMARKS.
           Imposta a 7 i gg di evasione a tutti i destini f

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "destinif.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "destinif.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Settaggio flag".

      * FILE-STATUS
       77  status-destinif          pic xx.

      * VARIABILI

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

      *****************************************************************

       PROCEDURE DIVISION.

       DECLARATIVES.
      ***---
       DESTINIF-ERR SECTION.
           use after error procedure on destinif.
           set tutto-ok  to true.
           evaluate status-destinif
           when "35"
                set errori to true
                display message "File [destinif] not found!"
                          title titolo      
                           icon 3
           when "39"
                set errori to true
                display message "File [destinif] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[destinif] Indexed file corrupt!"
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
                move   "destinif"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o destinif
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
           open i-o destinif.

      ***---
       ELABORAZIONE.
           move low-value to desf-rec.
           perform until 1 = 2 
              read destinif next at end exit perform end-read
              move 7 to desf-gg-consegna
              rewrite desf-rec invalid continue end-rewrite
           end-perform.
           display message "Operazione terminata!"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close destinif.

      ***---
       EXIT-PGM.
           goback.
