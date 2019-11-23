       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-esi-i.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
       77  status-clienti    pic X(2).

      ******************************************************************
       PROCEDURE DIVISION.     
           open i-o clienti.
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave.
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F exit perform end-if
              set cli-esigibilita-iva-immediata to true
              rewrite cli-rec
           end-perform.
           close clienti.
