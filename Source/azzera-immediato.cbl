       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzera-immediato.
       AUTHOR.                          Andrea.
       REMARKS. Pulizia del flag: Tutti a 0.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".

       WORKING-STORAGE SECTION.

       77  status-mtordini  pic xx.

       PROCEDURE DIVISION.
      ***---
       MAIN.
           open i-o mtordini.

           move low-value to mto-rec.
           start mtordini key >= mto-chiave.
           perform until 1 = 2
              read mtordini next at end exit perform end-read
              set mto-immediato-no to true
              rewrite mto-rec
           end-perform.
                 
           close mtordini.

           display message "FINE"

           goback.
