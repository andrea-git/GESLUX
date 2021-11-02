       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EDI-val-immediata.
       AUTHOR.                          Andrea.
       REMARKS.
           Valorizza con 0 il valore dell'evasione immediata EDI

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "EDI-mtordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "EDI-mtordini.fd".

       WORKING-STORAGE SECTION.
      *    COSTANTI
       78  titolo               value "Val immediata".

      *    FILE STATUS
       77  status-EDI-mtordini  pic xx.

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN.
           open i-o EDI-mtordini.
           move low-value to emto-chiave.
           start EDI-mtordini key >= emto-chiave.
           perform until 1 = 2
              read edi-mtordini next at end exit perform end-read
              set emto-ev-immediata-no to true
              rewrite emto-rec
           end-perform.

           close EDI-mtordini.
           display message "FINE".
           goback.
