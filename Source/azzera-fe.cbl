       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzera-fe.
       AUTHOR.                          Andrea.
       REMARKS. AZZERA FIDO EXTRA DI TUTTI I CLIENTI.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
       77  como-data       pic 9(8).    
       77  status-clienti  pic xx.

       77  filler          pic 9.
         88 RecLocked      value 1, false 0.    

       77  status-lineseq    pic x(2).
       77  wstampa           pic x(200).  

       77  como-riga             pic x(100).   
       77  riga-stampa           pic x(100).

       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(2)  value "] ".
                                            
       77  como-ora                pic 9(8).  

       LINKAGE SECTION.
       copy "link-batch.def".                               

      *****************************************************************

       PROCEDURE DIVISION USING batch-linkage. 

       DECLARATIVES.
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           evaluate status-clienti              
           when "99" set RecLocked to true
           end-evaluate.
       END DECLARATIVES.

      ***---
       MAIN.                  
           move 0 to batch-status.
           initialize wstampa.
           accept como-data from century-date.
           accept como-ora  from time.
           accept  wstampa from environment "SCHEDULER_PATH_LOG".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa      delimited low-value
                   "AZZERA-FE_" delimited size
                   como-data    delimited size
                   "_"          delimited size
                   como-ora     delimited size
                   ".log"       delimited size
                   into wstampa
           end-string.
           move wstampa to batch-log.
           open output lineseq.       

           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
               
           open i-o clienti.
           if status-clienti not = "00"
              initialize como-riga
              string "ELABORAZIONE INTERROTTA: "
                     "STATUS-CLIENTI: "
                     status-clienti
                into como-riga
              end-string
              perform SETTA-RIGA-STAMPA
              move -1 to batch-status
           else
           
              set cli-tipo-C to true
              move low-value to cli-rec
              start clienti key >= cli-chiave
              perform until 1 = 2
                 read clienti next no lock at end exit perform end-read
                 if cli-tipo-F exit perform end-if
                 if como-data > cli-data-fido-extra
                    move 0 to cli-fido-extra
                    rewrite cli-rec
                 end-if
              end-perform

              close clienti 
           end-if.
                     
           move "FINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           close lineseq.

           goback.              

      ***---
       SETTA-RIGA-STAMPA.
           initialize riga-stampa.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-stampa
           end-string.   
           write line-riga of lineseq from riga-stampa.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
