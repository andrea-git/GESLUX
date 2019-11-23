       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      reset-ev-intera.
       AUTHOR.                          Andrea.
       REMARKS.
           Serve per valorizzare a FALSE il vecchio flag sostituito
           da quello dei parametri per le prenotazioni qta

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Settaggio flag".

      * FILE-STATUS
       77  status-mtordini           pic xx.

      *****************************************************************

       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open i-o mtordini.

      ***---
       ELABORAZIONE.
           move low-value to mto-rec.
           start mtordini key >= mto-chiave.
           perform until 1 = 2 
              read mtordini next at end exit perform end-read
              set mto-prenotazione-qta-no to true
              rewrite mto-rec invalid continue end-rewrite
           end-perform.
           display message "Operazione terminata!"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close mtordini.

      ***---
       EXIT-PGM.
           goback.
