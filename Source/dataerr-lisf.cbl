       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      dataerr-lisf.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tlistini.sl".
           copy "rlistini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tlistini.fd".
           copy "rlistini.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Data errata listini F".

      * FILE STATUS
       77  status-rlistini      pic xx.
       77  status-tlistini      pic xx.

      * VARIABILI 
       77  cont-n pic 9(5).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       LINKAGE SECTION.                   
       77  data-old              pic 9(8).
       77  data-new              pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION USING data-old data-new.

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
           open input rlistini tlistini.

      ***---
       ELABORAZIONE.
           move low-value to rlis-rec.
           start rlistini key >= rlis-chiave.
           perform until 1 = 2
              read rlistini next at end exit perform end-read
              if rlis-ini-val(1:4) < 2000 and rlis-ini-val > 0
                 display message rlis-codice " - " rlis-articolo, " RI"
              end-if
              if rlis-fine-val(1:4) < 2000 and rlis-fine-val > 0 
                 display message rlis-codice " - " rlis-articolo, " RF"
              end-if
           end-perform.
           move low-value to tlis-rec.
           start tlistini key >= tlis-chiave.
           perform until 1 = 2
              read tlistini next at end exit perform end-read
              if tlis-ini-val(1:4) < 2000 and tlis-ini-val > 0
                 display message tlis-codice " TI"
              end-if
              if tlis-fine-val(1:4) < 2000 and tlis-ini-val > 0
                 display message rlis-codice" TF"
              end-if
           end-perform.

           display message "Operazione terminata!"
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close rlistini tlistini.

      ***---
       EXIT-PGM.
           goback.
