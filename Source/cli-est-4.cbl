       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cli-est-4.
       AUTHOR.                          Andrea.
       REMARKS. IMPOSTA I CLIENTI ESTERI CON TIPOLOGIA "4".

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".

       WORKING-STORAGE SECTION.

       77  status-clienti  pic xx.
       77  n               pic 9(5) value 0.

       PROCEDURE DIVISION.
      ***---
       MAIN.
           open i-o clienti.

           set cli-tipo-C to true.
           move low-value to cli-rec.
           start clienti key >= cli-chiave.
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F exit perform end-if
              if cli-prov = "EE" or cli-nazione not = "ITA"
                 move "4 " to cli-tipo
                 rewrite cli-rec
                 add 1 to n
              end-if
           end-perform.

           close clienti.

           display message "Elaborati clienti " n
                    TITLE  "ELABORAZIONE TERMINATA!".

           goback.
