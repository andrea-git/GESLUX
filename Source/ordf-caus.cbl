       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ordf-caus.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordforn.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "tordforn.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Cambio causale ordini f".

      * FILE STATUS
       77  status-tordforn      pic xx.

      * VARIABILI 
       77  cont-n pic 9(5) value 0.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

      ******************************************************************
       PROCEDURE DIVISION.

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
           move 0 to cont-n.

      ***---
       OPEN-FILES.
           open i-o tordforn.

      ***---
       ELABORAZIONE.
           move 2019 to tof-rec.
           start tordforn key >= tof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordforn next at end exit perform end-read
                    if tof-causale = "AAAA"
                       move "ORFL" to tof-causale
                       rewrite tof-rec
                       add 1 to cont-n
                    end-if
                 end-perform
           end-start.

           display message "Operazione terminata!"
                    x"0d0a""AGGIORNATI: ",  cont-n,
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close tordforn.

      ***---
       EXIT-PGM.
           goback.
