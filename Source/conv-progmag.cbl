       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-progmag.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Files GESLUX
           copy "progmag.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
      *    Files GESLUX
           copy "progmag.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
       77  status-progmag         pic xx.
       77  status-lineseq         pic xx.
       77  wstampa                pic x(256).
       77  counter                pic 99999.


      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           evaluate status-progmag
           when "35"
                display message "File [PROGMAG] not found!"
                           icon 3
           when "39"
                display message "File [PROGMAG] mismatch size!"
                           icon 3
           when "98"
                display message "[PROGMAG] Indexed file corrupt!"
                           icon 3 
           end-evaluate.

       END DECLARATIVES.

       MAIN-PRG.
           move 0 to counter.
           move "C:\progmag.txt" to wstampa.
           open input  lineseq.
           open output progmag.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              move line-riga to prg-rec
              set prg-attivo to true
              write prg-rec
              add 1 to counter
           end-perform.
           close lineseq.
           close progmag.
           display message "OK FINE! " counter.
