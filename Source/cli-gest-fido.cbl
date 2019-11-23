       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cli-gest-fido.
       AUTHOR.                          Andrea.
       REMARKS. IMPOSTA GESTIONE-FIDO = True a tutti i clienti.
  
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
              set cli-gestione-fido-si to true
              rewrite cli-rec
              add 1 to n
           end-perform.

           close clienti.

           display message "Elaborati clienti " n
                    TITLE  "ELABORAZIONE TERMINATA!".

           goback.
