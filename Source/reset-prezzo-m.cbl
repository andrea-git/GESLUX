       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      reset-prezzo-m.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mrordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mrordini.fd".

       WORKING-STORAGE SECTION.
       77  status-mrordini  pic xx.

       PROCEDURE DIVISION.
      ***---
       MAIN.
           open i-o mrordini.

           move low-value to mro-rec.
           start mrordini key >= mro-chiave.
           perform until 1 = 2
              read mrordini next at end exit perform end-read
              set mro-prz-manuale-no to true
              rewrite mro-rec
           end-perform.
                 
           close mrordini.

           display message "FINE OPERAZIONE!".

           goback.
