       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-fido-extra.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl". 
           copy "lineseq.sl".
           
       SELECT csvFile
           ASSIGN       TO path-csvFile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-csvFile.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd". 
           copy "lineseq.fd". 
           
       FD  csvFile.
       01 line-csvFile        PIC  x(1000).

       WORKING-STORAGE SECTION.
       copy "comune.def".

       78  titolo value "Importazione fido extra".

       77  status-clienti          pic x(2).
       77  path-csvFile            pic x(256).
       77  STATUS-csvFile          pic x(2).
       77  wstampa                 pic x(256).
       77  status-lineseq          pic xx.

       77  separatore              pic x.   
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).

       77  nargs                   pic 99 comp-1.      
       01  r-inizio                pic x(25).

       01  filler           pic 9.
           88 RichiamoSchedulato    value 1, false 0.

       77  r-cliente        pic 9(5).
       77  r-fido-extra     pic x(18).
       77  n-riga           pic 9(4).  
       77  como-riga        pic x(100). 
       77  idx2             pic 99.
       77  fido-int-x       pic x(18).
       77  fido-int         pic 9(18).

       LINKAGE SECTION.
           copy "link-batch.def".

       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.   
      ***---
       CSVFILE-ERR SECTION.
           use after error procedure on csvFile.

       END DECLARATIVES.

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
           CALL "C$NARG" USING NARGS.
           initialize wstampa.
           if nargs not = 0
              set RichiamoSchedulato to true
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
           else
              set RichiamoSchedulato to false
              accept  wstampa from environment "PATH_ST"
           end-if.
                                
           accept como-data from century-date.
           accept como-ora  from time.
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa           delimited low-value
                   "IMP-FIDO-EXTRA_" delimited size
                   como-data         delimited size
                   "_"               delimited size
                   como-ora          delimited size
                   ".log"            delimited size
              into wstampa
           end-string.                 
     
           if RichiamoSchedulato          
              move wstampa to batch-log
           end-if.
           
           set RecLocked   to false.

           accept separatore from environment "SEPARATORE".
                                          
           initialize path-csvFile.
           accept path-csvFile from environment "IMP_FIDO_EXTRA_FILE".
           if path-csvFile = spaces
              goback 
           end-if.

      ***---
       OPEN-FILES.     
           open output lineseq.
                   
           open input  csvFile.
           if status-csvFile not = "00"
              initialize como-riga
              string "Errore: " status-csvFile " - file: " path-csvFile
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              if RichiamoSchedulato
                 move -1 to batch-status
              end-if
              set errori to true
              close lineseq
           end-if.
              
           if tutto-ok
              open i-o clienti
           end-if.                        
      
      ***---
       ELABORAZIONE.     
           move "INIZIO ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.
           
           move "CANCELLAZIONE FIDO EXTRA SUI CLIENTI" to como-riga.
           perform SCRIVI-RIGA-LOG.

           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti next at end exit perform end-read
                    if cli-tipo-F exit perform end-if
                    move 0 to cli-fido-extra
                    rewrite cli-rec
                 end-perform
           end-start.              
           move "CANCELLAZIONE ESEGUITA" to como-riga.
           perform SCRIVI-RIGA-LOG.    
          
           move "IMPORTAZIONE FIDO EXTRA SUI CLIENTI" to como-riga.
           perform SCRIVI-RIGA-LOG.

           move 0 to n-riga
           perform until 1 = 2
              add 1 to n-riga
              initialize line-csvFile
              read csvFile next at end exit perform end-read
              unstring line-csvFile delimited by separatore
                                    into r-cliente r-fido-extra 

              move r-cliente to cli-codice
              set cli-tipo-C to true
              read clienti no lock
                   invalid
                   initialize como-riga
                   string "Riga: " n-riga " - cliente: " 
                          cli-codice " NON TROVATO"
                     into como-riga
                   end-string
                   perform SCRIVI-RIGA-LOG
               not invalid           
                   move 0      to idx2 fido-int
                   move spaces to fido-int-x
                   perform varying idx from 1 by 1 
                             until idx > 18
                      if r-fido-extra(idx:1) = "," 
                         exit perform cycle 
                      end-if
                      if r-fido-extra(idx:1) = " " 
                         exit perform 
                      end-if
                      add 1 to idx2
                      move r-fido-extra(idx:1) to fido-int-x(idx2:1)
                   end-perform
                   call "C$JUSTIFY" using fido-int-x, "R"
                   inspect fido-int-x replacing leading x"20" by x"30"
                   move fido-int-x to fido-int
                   compute cli-fido-extra = fido-int / 100
                   rewrite cli-rec 

                   initialize como-riga
                   string "Riga: " n-riga " - cliente: " 
                          cli-codice " aggiornato fido extra: " 
                          r-fido-extra
                     into como-riga
                   end-string
                   perform SCRIVI-RIGA-LOG
              end-read
           end-perform.
       
           move "FINE IMPORTAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.

           move "FINE ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.

      ***---
       CLOSE-FILES.
           close lineseq clienti csvFile.

      ***---
       SCRIVI-RIGA-LOG.
           initialize line-riga.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
             into line-riga
           end-string.   
           write line-riga.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
