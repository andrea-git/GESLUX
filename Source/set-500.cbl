       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-500.
       AUTHOR.                          Andrea.
       REMARKS.
           Serve per valorizzare a TRUE lo split della bolla > 500 Kg
           per tutti i clienti/destini non GDO/esteri.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destini.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "destini.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Settaggio flag 500".

      * FILE-STATUS
       77  status-clienti          pic xx.
       77  status-destini          pic xx.

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
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                set errori to true
                display message "File [destini] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [destini] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[destini] Indexed file corrupt!"
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
                move   "destini"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o destini allowing readers
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
           open i-o clienti allowing readers.
           if tutto-ok
              open i-o destini allowing readers
           end-if.

      ***---
       ELABORAZIONE.
           move low-value to cli-rec.
           perform until 1 = 2 
              read clienti next at end exit perform end-read
              if cli-tipo-F
                 exit perform
              end-if
              if cli-tipo not = "1 " and not = "4 "
                 move "S" to cli-superamento-500
                 move cli-codice to des-codice
                 move low-value  to des-prog
                 start destini key >= des-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read destini next at end exit perform end-read
                          if des-codice not = cli-codice
                             exit perform
                          end-if
                          move "S" to des-superamento-500
                          rewrite des-rec
                       end-perform
                 end-start
                 rewrite cli-rec invalid continue end-rewrite
              end-if
           end-perform.
           display message "Operazione terminata!"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close clienti.

      ***---
       EXIT-PGM.
           goback.
