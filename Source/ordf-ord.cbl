       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ordf-ord.
       AUTHOR.                          Andrea.
       REMARKS. Per gli ordini passati in linkage da crea-ordfor
                riordina le righe per descrizione articolo
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rordforn.sl".
           copy "articoli.sl".
           copy "tmp-des-rordforn.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "rordforn.fd".
           copy "articoli.fd".
           copy "tmp-des-rordforn.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Ordinamento automatici".

      * FILE STATUS
       77  status-rordforn      pic xx.  
       77  status-articoli      pic xx.
       77  status-des-rordforn  pic xx.

      * VARIABILI 
       77  como-data pic 9(8).
       77  como-ora  pic 9(8).
       77  s-numero  pic 9(8) value 0.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       LINKAGE SECTION.    
       copy "link-ordf-ord.def".

      ******************************************************************
       PROCEDURE DIVISION USING link-ordf-ord.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.
           accept path-des-rordforn from environment "PATH_ST".
           accept como-data from century-date.
           accept como-ora  from time.                    
           inspect path-des-rordforn  
                   replacing trailing spaces by low-value.
           string path-des-rordforn delimited low-value
                  "des-rof_"        delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
             into path-des-rordforn
           end-string.
           inspect path-des-rordforn  
                   replacing trailing low-value by spaces.

      ***---
       OPEN-FILES.               
           open i-o    rordforn.
           open input  articoli.
           open output tmp-des-rordforn.
           close       tmp-des-rordforn.
           open i-o    tmp-des-rordforn.

      ***---
       ELABORAZIONE.
           move low-value to rof-rec.
           move loo-anno  to rof-anno.
           move loo-primo to rof-numero.
           start rordforn key >= rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn next at end exit perform end-read
                    if rof-numero > loo-ultimo
                       exit perform
                    end-if
                    move rof-dati to drof-rof-dati
                    move rof-cod-articolo to art-codice
                    read articoli no lock
                         invalid move "NON TROVATO" to art-descrizione
                    end-read         
                    move rof-chiave-testa to drof-chiave-testa
                    move art-descrizione  to drof-art-descrizione
                    move rof-cod-articolo to drof-cod-articolo
                    move rof-riga         to drof-rof-riga
                    move 0 to drof-prog
                    perform until 1 = 2
                       add 1 to drof-prog
                       write drof-rec
                             invalid continue
                         not invalid exit perform
                       end-write
                    end-perform
                    if loo-funzione = "S"
                       delete rordforn record
                    end-if
                 end-perform
           end-start.
           if loo-funzione = "O"
              move path-des-rordforn to loo-path
           else
              move low-value to drof-chiave
              start tmp-des-rordforn key >= drof-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tmp-des-rordforn next 
                         at end exit perform 
                       end-read
                       if s-numero = 0 or drof-numero not = s-numero  
                          move drof-numero to s-numero
                          move 0 to rof-riga
                       end-if
                       add 1 to rof-riga
                       move drof-chiave-testa to rof-chiave-testa
                       move drof-rof-dati     to rof-dati
                       write rof-rec
                    end-perform
              end-start
           end-if.

      ***---
       CLOSE-FILES.
           close rordforn articoli tmp-des-rordforn.
           if loo-funzione = "S"
              delete file tmp-des-rordforn
           end-if.

      ***---
       EXIT-PGM.
           goback.
