       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-flag-gds.
       AUTHOR.                          Andrea.
       REMARKS.
           Serve per valorizzare il nuovo flag GDS col 
           medesimo valore di GDA (Ex-GDO)

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Settaggio flag GDS".

      * FILE-STATUS
       77  status-articoli           pic xx.

      * VARIABILI

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

      *****************************************************************

       PROCEDURE DIVISION.

       DECLARATIVES.
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                set errori to true
                display message "File [ARTICOLI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
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
                move   "articoli"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o articoli allowing readers
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
           open i-o articoli allowing readers.

      ***---
       ELABORAZIONE.
           move low-value to art-rec.
           perform until 1 = 2 
              read articoli next at end exit perform end-read
              move art-gda to art-gds
              rewrite art-rec invalid continue end-rewrite
           end-perform.
           display message "Operazione terminata!"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close articoli.

      ***---
       EXIT-PGM.
           goback.
