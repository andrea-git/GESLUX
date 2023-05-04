       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-cli-sdi-up.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
       77  status-clienti          pic xx.


      ******************************************************************
       PROCEDURE DIVISION.
       MAIN.                              
           open i-o clienti.
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti next at end exit perform end-read
                    if cli-tipo-F exit perform end-if
                    inspect cli-codice-sdi converting
                                     "abcdefghijklmnopqrstuvwxyz" to
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    rewrite cli-rec
                 end-perform
           end-start.
           close clienti.
           display message "Elaborazione terminata"
                     title "Conversione codice SDI".
           goback.
