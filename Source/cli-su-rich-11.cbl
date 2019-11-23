       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cli-su-rich-11.
       AUTHOR.                          Andrea.
       REMARKS. IMPOSTA SOSTITUZIOE ARTICOLI "SU RICHIESTA" AI CLIENTI
                CON TIPOLOGIA "11" e "12".
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".

       WORKING-STORAGE SECTION.

       77  status-clienti  pic xx.

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
              if cli-tipo = "11" or "12"
                 set cli-sost-richiesta to true
                 rewrite cli-rec
              end-if
           end-perform.

           close clienti.

           goback.
