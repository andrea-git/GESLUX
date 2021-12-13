       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      carcap.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "anacap.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "anacap.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
       77  status-anacap           pic xx.
       77  status-lineseq          pic xx.
       77  wstampa                 pic x(256).


      ******************************************************************
       PROCEDURE DIVISION.
       MAIN.
           move "anacap.csv" to wstampa.
           open input lineseq.
           open i-o anacap.  
           |Salto intestazione                    
           read lineseq next.

           perform until 1 = 2
              read lineseq next at end exit perform end-read
              initialize anc-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              unstring line-riga delimited by ";"
                  into anc-comune
                       anc-prov
                       anc-cap
              call "C$JUSTIFY" using anc-cap, "R"
              inspect anc-cap replacing leading x"20" by x"30"

              move "IMPORT" to anc-utente-creazione
              accept anc-ora-creazione from time
              accept anc-data-creazione from century-date

              write anc-rec invalid rewrite anc-rec end-write
           end-perform.
           display message "FINE".
           close anacap.
           close lineseq.
           goback.
