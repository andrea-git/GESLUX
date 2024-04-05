       IDENTIFICATION DIVISION.
       PROGRAM-ID.  agg-datalisf.
       REMARKS. Allinea le date delle righe dei listini f con quelle
                delle testate
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.     
           copy "tlistini.sl".
           copy "rlistini.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.     
           copy "tlistini.fd".
           copy "rlistini.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.     
       77  status-tlistini   pic xx.
       77  status-rlistini   pic xx.
       77  status-lineseq    pic xx.

       77  nargs             pic 99  comp-1 value 0.  
       77  como-riga         pic x(300).
       77  r-inizio          pic x(25).

       77  como-ora          pic 9(8).
       77  como-data         pic 9(8).

       01  filler            pic xx.    
         88 tutto-ok         value "OK".
         88 errori           value "ER".

       01  filler                    pic 9 value 0.
           88 RichiamoSchedulato           value 1, false 0.

       LINKAGE SECTION.
           copy "link-batch.def".
      

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage. 

       DECLARATIVES.
     
      ***---
       TLISTINI-ERR SECTION.
           use after error procedure on tlistini.
           evaluate status-tlistini
           when "35" 
                move "ERR: File [TLISTINI] not found!" to como-riga
                perform RIGA-LOG
                set errori to true
           when "39"
                move "ERR: File [TLISTINI] mismatch size!" to como-riga
                perform RIGA-LOG  
                set errori to true 
           when "98"
                move "ERR: File [TLISTINI] corrupt!" to como-riga
                perform RIGA-LOG  
                set errori to true 
           end-evaluate.    
     
      ***---
       RLISTINI-ERR SECTION.
           use after error procedure on rlistini.
           evaluate status-rlistini
           when "35" 
                move "ERR: File [RLISTINI] not found!" to como-riga
                perform RIGA-LOG  
                set errori to true
           when "39"
                move "ERR: File [RLISTINI] mismatch size!" to como-riga
                perform RIGA-LOG  
                set errori to true 
           when "98"
                move "ERR: File [RLISTINI] corrupt!" to como-riga
                perform RIGA-LOG  
                set errori to true 
           when "93"
                move "ERR: File [RLISTINI] locked!"  to como-riga
                perform RIGA-LOG  
                set errori to true 
           when "99"
                move "ERR: Rcoed [RLISTINI] locked!" to como-riga
                perform RIGA-LOG   
           end-evaluate.    

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           evaluate status-lineseq
           when "35" 
                move "ERR: File [LINESEQ] not found!" to como-riga
                perform RIGA-LOG  
                set errori to true
           when "39"
                move "ERR: File [LINESEQ] mismatch size!" to como-riga
                perform RIGA-LOG  
                set errori to true 
           when "98"
                move "ERR: File [LINESEQ] corrupt!" to como-riga
                perform RIGA-LOG  
                set errori to true 
           end-evaluate.    

       END DECLARATIVES.

       MAIN-PRG.          
           perform INIT-PGM.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           else
              move -1 to batch-status
           end-if.
           perform EXIT-PGM.


      ***---
       INIT-PGM.
           set tutto-ok to true. 

           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true   
              move 0 to batch-status
           else
              set RichiamoSchedulato to false
           end-if.              
              
           initialize wstampa.
           accept como-data from century-date.
           accept como-ora  from time.

           if RichiamoSchedulato
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
              set RichiamoSchedulato to true
              move wstampa to batch-log
           else
              accept  wstampa from environment "PATH_ST"
           end-if.
           
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa      delimited low-value
                   "STATOCONS_" delimited size
                   como-data    delimited size
                   "_"          delimited size
                   como-ora     delimited size
                   ".log"       delimited size
              into wstampa
           end-string.
      
      ***---
       OPEN-FILES.
           open output lineseq.
           open  input tlistini.
           open    i-o rlistini.

      ***---
       ELABORAZIONE.
           move "INIZIO ELABORAZIONE" to como-riga.
           perform RIGA-LOG.

           move low-value to rlis-rec.
           start rlistini key >= rlis-codice
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rlistini next at end exit perform end-read
                    move rlis-codice to tlis-codice
                    read tlistini no lock
                         invalid
                         initialize como-riga
                         string "LISTINO: "
                                tlis-codice
                                " NON TROVATO SULLA TESTATA"
                                delimited size
                           into como-riga
                         end-string       
                         perform RIGA-LOG
                         if RichiamoSchedulato 
                            if batch-status = 0
                               move 1 to batch-status
                            end-if
                         end-if
                     not invalid
                         if rlis-ini-val  not = tlis-ini-val or
                            rlis-fine-val not = tlis-fine-val
                            initialize como-riga
                            string "LISTINO: "
                                   tlis-codice
                                   " DATE RIGA: "
                                   rlis-ini-val
                                   " - "
                                   rlis-fine-val
                                   " DATE TESTA: "
                                   tlis-ini-val
                                   " - "
                                   tlis-fine-val
                                   delimited size
                              into como-riga
                            end-string       
                            perform RIGA-LOG
                            move tlis-ini-val  to rlis-ini-val
                            move tlis-fine-val to rlis-fine-val
                            rewrite rlis-rec 
                                    invalid continue 
                            end-rewrite
                            if status-rlistini not = "00"
                               initialize como-riga
                               string "STATUS-LISTINI: "
                                      status-rlistini
                                      delimited size
                                 into como-riga
                               end-string      
                               perform RIGA-LOG
                               if RichiamoSchedulato 
                                  move -1 to batch-status
                               end-if
                            end-if
                         end-if
                   end-read
                end-perform
           end-start.    
           move "FINE ELABORAZIONE" to como-riga.
           perform RIGA-LOG.

      ***---
       CLOSE-FILES.
           close tlistini rlistini lineseq.

      ***---
       EXIT-PGM.
           goback.                      

      ***---
       RIGA-LOG.
           initialize line-riga.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
             into line-riga
           end-string.
           write line-riga.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
