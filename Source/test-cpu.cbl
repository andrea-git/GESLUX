       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      test-cpu.
       AUTHOR.                          Andrea.
       REMARKS. Programma creato pr far impallare il sistema
                con un ciclo di loopvolontariamente INFINITO
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tcla1art.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tcla1art.fd".

       WORKING-STORAGE SECTION.

       78  titolo    value "TEST CPU".

       77  status-tcla1art   pic xx.

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE until 1 = 2.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open input tcla1art.
      
      ***---
       ELABORAZIONE.
           move low-value to cl1-rec.
           start tcla1art key >= cl1-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tcla1art next at end exit perform end-read
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close tcla1art.

      ***---
       EXIT-PGM.
           goback.
