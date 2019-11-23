       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-flag.
       AUTHOR.                          Andrea.
       REMARKS. CEPSA (Ex-Shark) = 1 se SPI = 1.
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
       78  titolo value "Settaggio flag articoli".

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
              if art-SPI-si
                 set art-SHARK-si to true
                 rewrite art-rec
              end-if
           end-perform.     

      ***---
       CLOSE-FILES.
           close articoli.

      ***---
       EXIT-PGM. 
           goback.
