       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-art-udm.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".  

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".  

       WORKING-STORAGE SECTION.
       77  status-articoli         pic xx.
       77  statusPgm               signed-short.
                                            
       77  udm-old                 pic x(9).
       77  udm-new                 pic x(9).

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN.
           open i-o articoli.
           move low-value to art-chiave.
           start articoli key >= art-chiave.
           perform until 1 = 2
              read articoli next at end exit perform end-read
              if art-udm-imballo-old = spaces
                 exit perform cycle
              end-if           
              move art-udm-imballo-old  to udm-old
              move art-udm-imballo      to udm-new
              call   "check-udm" using udm-old
                                       udm-new
                                giving statusPgm
              cancel "check-udm"
              move udm-new to art-udm-imballo
              rewrite art-rec
           end-perform.           
           close articoli.        
           goback.                        
