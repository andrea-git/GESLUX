       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mov-peso.
       AUTHOR.                          Andrea.
       REMARKS. Controllo che il peso TOT UTF e il peso TOT non siano
           presenti entrambi nella riga. In questo caso elimino la
           valorizzazione del peso NON UTF dato che l'errore si è
           verificato solamente su articoli UTF.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rmovmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "rmovmag.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Correzione pesi TOT e TOT UTF".

      * FILE STATUS
       77  status-rmovmag        pic xx.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI
       77  num-rec                   pic 9(6).
       77  num-rec-edit              pic zzz.zz9.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "39"
                set errori to true
                display message "File [RMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RMOVMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           set tutto-ok to true.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open i-o   rmovmag.

      ***---
       ELABORAZIONE.
           move 0 to num-rec.
           move low-value to rmo-rec.
           start rmovmag key >= rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-peso-tot     not = 0 and
                       rmo-peso-tot-utf not = 0
                       move 0 to rmo-peso-tot
                       rewrite rmo-rec invalid continue end-rewrite
                       add 1 to num-rec
                    end-if
                 end-perform
           end-start.
           move num-rec to num-rec-edit.

           display message "Rettificati " num-rec-edit, " righe!"
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close rmovmag.

      ***---
       EXIT-PGM.
           goback.
