       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-scorta-3-7.
       AUTHOR.                          Andrea.
       REMARKS. Articoli da scorta 3 a scorta 7.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Batch articoli scorta 3 --> 7".

      * FILE STATUS
       77  status-articoli       pic xx.
       
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

      ***---
       OPEN-FILES.
           open i-o   articoli.

      ***---
       ELABORAZIONE.        
           move low-value to art-rec.
           start articoli key >= art-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read articoli next at end exit perform end-read
              if art-scorta = 3
                 move 7 to art-scorta
                 rewrite art-rec
              end-if
           end-perform.     

      ***---
       CLOSE-FILES.
           close articoli.

      ***---
       EXIT-PGM. 
           goback.
