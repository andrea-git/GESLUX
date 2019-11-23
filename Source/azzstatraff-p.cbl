       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzstatraff-p.
       AUTHOR.                          Andrea.
       REMARKS. Questo batch verrà lanciato solo a FINE ANNO dai
                dipendenti Lubex per far "slittare" i dati valorizzando
                i campi dell'anno precedente.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "statraff.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "statraff.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Batch Azzeramento".

      * FILE-STATUS
       77  status-statraff       pic xx.

      * FLAGS
       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

      * VARIABILI
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       77  link-handle           handle of window.

       PROCEDURE DIVISION USING link-handle.

       DECLARATIVES.
       STATRAFF-ERR SECTION.
           use after error procedure on statraff.
           set tutto-ok  to true.
           evaluate status-statraff
           when "35"
                set errori to true
                display message "File [STATRAFF] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [STATRAFF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STATRAFF] Indexed file corrupt!"
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
                move   "statraff"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o statraff allowing readers
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
           perform OPEN-FILE.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILE
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.
           move 0 to counter counter2.

      ***---
       OPEN-FILE.
           open i-o statraff allowing readers.

      ***---
       ELABORAZIONE.
           move low-value  to str-rec.
           start statraff  key is >= str-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read statraff next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 35
                                             line 18
                    move 0 to counter2
                 end-if

                 move str-kg-corr  to str-kg-past
                 move str-qta-corr to str-qta-past
                 move str-vag-corr to str-vag-past
                 move str-kg-prog  to str-kg-prog-past
                 move str-qta-prog to str-qta-prog-past
                 move str-vag-prog to str-vag-prog-past
                 move 0            to str-kg-corr 
                 move 0            to str-qta-corr
                 move 0            to str-vag-corr
                 move 0            to str-kg-prog 
                 move 0            to str-qta-prog
                 move 0            to str-vag-prog
                 rewrite str-rec invalid continue end-rewrite
              end-perform
           end-if.

      ***---
       CLOSE-FILE.
           close statraff.

      ***---
       EXIT-PGM.
           goback.
