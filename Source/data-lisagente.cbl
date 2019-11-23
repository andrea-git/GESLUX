       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      data-lisagente.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lisagente.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lisagente.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Validità listini agente".

      * FILE STATUS
       77  status-lisagente      pic xx.

      * VARIABILI 
       77  cont-n pic 9(5).
       77  cont-o pic 9(5).

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
           move 0 to cont-n cont-o.

      ***---
       OPEN-FILES.
           open i-o lisagente.

      ***---
       ELABORAZIONE.
           move low-value to lis-rec.
           move 9999      to lis-codice.
           start lisagente key >= lis-chiave.
           perform until 1 = 2
              read lisagente next at end exit perform end-read
              if lis-codice not = 9999   exit perform end-if
              if lis-data-fine-new = 20141231
                 move 20151231 to lis-data-fine-new
                 rewrite lis-rec
                 add 1 to cont-n
              end-if
              if lis-data-fine-old = 20141231
                 move 20151231 to lis-data-fine-old
                 rewrite lis-rec
                 add 1 to cont-o
              end-if

           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""AGGIORNATI NEW: ",  cont-n,
                    x"0d0a""AGGIORNATI OLD: ",  cont-o,
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close lisagente.

      ***---
       EXIT-PGM.
           goback.
