       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      del-cod-art-cli.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "assorcli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "assorcli.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Cancellazione codice articolo cliente".

      * FILE STATUS
       77  status-assorcli       pic xx.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

       77  num-rec               pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       ASSORCLI SECTION.
           use after error procedure on assorcli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-assorcli
           when "39"
                set errori to true
                display message "File [ASSORCLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ASSORCLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [ASSORCLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
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
           open i-o assorcli.

      ***---
       ELABORAZIONE.
           move low-value to asc-rec.
           start assorcli key >= asc-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read assorcli next at end exit perform end-read
                    move spaces to asc-cod-articolo-per-cliente
                    rewrite asc-rec
                    add 1 to num-rec
                 end-perform
           end-start.
           display message 
                   "Elaborazione effettuata su ", num-rec, " record"
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close assorcli.

      ***---
       EXIT-PGM.
           goback.
