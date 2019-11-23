       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-art-udm.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
       77  status-articoli         pic xx.
       77  status-lineseq          pic xx.
       77  wstampa                 pic x(256).

       77  como-valore             pic x(20).
       78  78-comma                value ".".

       copy "tratta-numerico.def".


      ******************************************************************
       PROCEDURE DIVISION.
       MAIN.
           move "art-udm.csv" to wstampa.
           open input lineseq.
           open i-o articoli.
           move low-value to art-chiave.
           start articoli key >= art-chiave.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              unstring line-riga  delimited by ";"
                       into art-codice
                            art-descrizione
                            como-valore
              read articoli no lock
                   invalid continue
               not invalid
                   if como-valore = spaces
                      move 0 to art-litri
                   else
                      move como-valore to art-udm-imballo
                      move como-valore to NumericEdi
                      perform TRATTA-NUMERICO
                      move como-numero to art-litri
                   end-if
                   rewrite art-rec
              end-read
           end-perform.
           display message "FINE".
           close articoli.
           close lineseq.
           goback.

      ***---
       PAR-COP.
           copy "tratta-numerico.cpy".
