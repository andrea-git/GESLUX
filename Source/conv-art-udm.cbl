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
              call   "check-udm" using art-udm-imballo-old 
                                       art-udm-imballo
                                giving statusPgm
              cancel "check-udm"
              rewrite art-rec
           end-perform.           
           close articoli.        
           goback.                        
