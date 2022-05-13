       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      del-split.
       REMARKS. BATCH cancellazione notturna split per bolla.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "rordini.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.

      * Status Files GESLUX
       77  status-tordini    pic X(2).
       77  status-rordini    pic X(2).
       77  status-lineseq    pic x(2).
       77  wstampa           pic x(200).  

       77  como-riga             pic x(100).   
       77  riga-stampa           pic x(100).

       01  r-inizio              pic x(25).
                                            
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).

                                                                          
       78  titolo            value "Cancellazione Split". 

       LINKAGE SECTION.
       copy "link-batch.def".                               

      *****************************************************************

       PROCEDURE DIVISION USING batch-linkage. 

       DECLARATIVES.

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.

       END DECLARATIVES.

      ******************************************************************

       MAIN-PRG.
           move 0 to batch-status.
           initialize wstampa.
           accept como-data from century-date.
           accept como-ora  from time.
           accept  wstampa from environment "SCHEDULER_PATH_LOG".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa      delimited low-value
                   "DEL-SPLIT_" delimited size
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

           open i-o tordini rordini.
           if status-tordini not = "00" or status-tordini not = "00"
              initialize como-riga
              string "ELABORAZIONE INTERROTTA: "
                     "STATUS-TORDINI: "
                     status-tordini
                     " - STATUS RORDINI: "
                     status-rordini
                into como-riga
              end-string
              perform SETTA-RIGA-STAMPA
              move -1 to batch-status
           else
              move 9999 to tor-anno
              start tordini key >= tor-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tordini next at end exit perform end-read
                       if tor-anno not = 9999
                          exit perform
                       end-if
                       delete tordini record
                    end-perform
              end-start
              move 9999 to ror-anno
              start rordini key >= ror-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rordini next at end exit perform end-read
                       if ror-anno not = 9999
                          exit perform
                       end-if
                       delete rordini record
                    end-perform
              end-start                
              close tordini rordini 
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
