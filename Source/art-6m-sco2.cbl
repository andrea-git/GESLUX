       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-6m-sco2.
       AUTHOR.                          Andrea.
       REMARKS. se il codice è creato da almeno 6 mesi e non ha nessuno 
                venduto negli ultimi 6 mesi viene messo automaticamente 
                in scorta 2.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.        
           copy "articoli.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tcaumag.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tcaumag.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Batch articoli 6 mesi".

      * FILE STATUS           
       77  status-articoli       pic xx.
       77  status-rmovmag        pic xx.
       77  status-tmovmag        pic xx.    
       77  status-tcaumag        pic xx.
       77  status-lineseq        pic xx.
                                                
       77  data-oggi             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-data             pic 9(8).
       77  como-riga             pic x(110).  
       77  r-inizio              pic x(25).    
       77  nargs                 pic 99  comp-1 value 0.
                                              
       77  filler                pic 9 value 0.
           88  nessun-errore           value 1, false 0.
       01  filler                pic 9 value 0.
         88 scorta2                    value 1, false 0.      
       77  filler                pic 9 value 0.
           88 RecLocked                value 1, false 0.   

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.

       01  como-data-6m.                     
         03 como-anno-6m         pic 9(4).
         03 como-mese-6m         pic 9(2).
         03 como-giorno-6m       pic 9(2).
       
      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
                               
      ******************************************************************
       LINKAGE SECTION.   
       copy "link-batch.def".
      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage. 
       DECLARATIVES.   

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [ARTICOLI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [ARTICOLI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[ARTICOLI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.        

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RMOVMAG] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RMOVMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[RMOVMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.        

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TMOVMAG] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TMOVMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TMOVMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TCAUMAG] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TCAUMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TCAUMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.    
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

           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.

           set tutto-ok to true.
           accept data-oggi from century-date.
           move data-oggi(5:2) to como-mese-6m.
           move data-oggi(1:4) to como-anno-6m.
           move data-oggi(7:2) to como-giorno-6m.
           evaluate como-mese-6m
           when 1 move  7 to como-mese-6m
                  subtract 1 from como-anno-6m
           when 2 move  8 to como-mese-6m
                  subtract 1 from como-anno-6m
           when 3 move  9 to como-mese-6m
                  subtract 1 from como-anno-6m
           when 4 move 10 to como-mese-6m
                  subtract 1 from como-anno-6m
           when 5 move 11 to como-mese-6m
                  subtract 1 from como-anno-6m
           when 6 move 12 to como-mese-6m
                  subtract 1 from como-anno-6m
           when other subtract 6 from como-mese-6m
           end-evaluate.  
           if RichiamoSchedulato
              initialize wstampa
              accept como-data from century-date
              accept como-ora  from time
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa         delimited low-value
                      "ART-6M-SCO2_"  delimited size
                      como-data       delimited size
                      "_"             delimited size
                      como-ora        delimited size
                      ".log"          delimited size
                      into wstampa
              end-string
              set RichiamoSchedulato to true
              move wstampa to batch-log
              open output lineseq
           end-if.
           initialize como-riga.      
           perform SETTA-INIZIO-RIGA.
           string r-inizio
                  "DATA INIZIO RICERCA: " 
                  como-data-6m(7:2)
                  "/"
                  como-data-6m(5:2)
                  "/"
                  como-data-6m(1:4)
             into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       OPEN-FILES.
           open i-o   articoli.
           open input tmovmag rmovmag tcaumag.

      ***---
       ELABORAZIONE.        
           move low-value to art-rec.
           start articoli key >= art-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read articoli next at end exit perform end-read
              move como-data-6m  to rmo-data-movim
              move art-codice to rmo-articolo 
              set scorta2 to true
              start rmovmag key >= k-art-data
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rmovmag next at end exit perform end-read
                       if rmo-articolo not = art-codice 
                          exit perform
                       end-if
                       move rmo-anno  to tmo-anno
                       move rmo-movim to tmo-numero
                       read tmovmag no lock
                            invalid continue
                        not invalid
                            move tmo-causale to tca-codice
                            read tcaumag no lock
                                 invalid continue
                             not invalid
                                 if tca-movim-ven-pos or
                                    tca-movim-ven-neg
                                    set scorta2 to false
                                    initialize como-riga        
                                    perform SETTA-INIZIO-RIGA
                                    string r-inizio
                                           "ARTICOLO: " 
                                           art-codice 
                                           ", TROVATO MOVIMENTO: "
                                           tmo-anno
                                           " - "
                                           tmo-numero
                                           ", DATA: "         
                                           rmo-data-movim(7:2)
                                           "/"
                                           rmo-data-movim(5:2)
                                           "/"
                                           rmo-data-movim(1:4)
                                           ", CAUSALE: "
                                           tmo-causale
                                      into como-riga
                                    end-string
                                    perform RIGA-LOG
                                    exit perform
                                 end-if
                            end-read
                       end-read
                    end-perform
              end-start
              if scorta2  
                 move 2 to art-scorta
                 rewrite art-rec
              end-if
           end-perform.     

      ***---
       CLOSE-FILES.
           close articoli tmovmag rmovmag tcaumag lineseq.

      ***---
       RIGA-LOG.
           if RichiamoSchedulato
              initialize line-riga of lineseq
              write line-riga of lineseq from como-riga
           else
              display como-riga upon syserr
           end-if.

      ***---
       EXIT-PGM. 
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
