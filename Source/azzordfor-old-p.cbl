       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzordfor-old.
       AUTHOR.                          Andrea.
       REMARKS. Azzera il file ordfor-old slittando i dati 
                dell'anno corrente su quelli dell'anno passato.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "ordfor-old.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "ordfor-old.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Azzeramento file ordini a fornitori".

      * FILE-STATUS
       77  status-ordfor-old            pic xx.

      * VARIABILI
       77  counter                  pic 9(10).
       77  counter2                 pic 9(10).
       77  counter-edit             pic z(10).
       77  idx                      pic 99.

      * FLAGS
       01  controlli                pic xx.
         88 errori                  value "ER".
         88 tutto-ok                value "OK".

       LINKAGE SECTION.
       77  link-handle              handle of window.

       PROCEDURE DIVISION USING link-handle.

       DECLARATIVES.

       ordfor-old-ERR SECTION.
           use after error procedure on ordfor-old.
           set tutto-ok  to true.
           evaluate status-ordfor-old
           when "35"
                set errori to true
                display message "File [ordfor-old] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [ordfor-old] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ordfor-old] Indexed file corrupt!"
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
                move   "ordfor-old"     to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o ordfor-old allowing readers
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
           set  tutto-ok       to true.

      ***---
       OPEN-FILES.
           open i-o ordfor-old     allowing readers.

      ***---
       ELABORAZIONE.
           move low-value   to ord-rec-old.
           start ordfor-old key is >= ord-chiave-old
                 invalid set errori to true
           end-start.

           if tutto-ok

              perform until 1 = 2

                 read ordfor-old next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200
                    move counter to counter-edit 
                    display counter-edit
                       upon link-handle at column 30
                                             line 18
                    move 0 to counter2
                 end-if

                 move 0 to idx
                 perform 12 times
                    add  1  to idx
                    move ord-qta-corr-m-old(idx) 
                      to ord-qta-past-m-old(idx)
                    move 0  to ord-qta-corr-m-old(idx)
                 end-perform
                 rewrite ord-rec-old invalid continue end-rewrite

              end-perform
           end-if.  

      ***---
       CLOSE-FILES.
           close ordfor-old.

      ***---
       EXIT-PGM.
           goback.
