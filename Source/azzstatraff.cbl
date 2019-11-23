       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzstatsett-p.
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

       PROCEDURE DIVISION.

       DECLARATIVES.
       STATRAFF-ERR SECTION.
           use after error procedure on statraff.
           set tutto-ok  to true.
           evaluate status-statraff
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
           perform OPEN-FILE.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILE
           end-if.
           perform EXIT-PGM.

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
                 move str-qta-corr to str-qta-past
                 move str-vag-corr to str-vag-past
                 move str-qta-prog to str-qta-prog-past
                 move str-vag-prog to str-vag-prog-past
                 rewrite str-rec invalid continue end-rewrite
              end-perform
           end-if.

      ***---
       CLOSE-FILE.
           close statraff.

      ***---
       EXIT-PGM.
           goback.
