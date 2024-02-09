       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      log4mas.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl". 
           copy "progmag.sl". 
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "lineseq.sl".    
           copy "log4mas.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd". 
           copy "progmag.fd". 
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "lineseq.fd".    
           copy "log4mas.fd".

       WORKING-STORAGE SECTION.
           copy "comune.def".
           copy "link-geslock.def".

       78  titolo value "Log articoli scorta 4 - master".

       77  status-articoli         pic x(2).
       77  status-progmag          pic x(2).
       77  status-mtordini         pic x(2).
       77  status-mrordini         pic x(2).
       77  status-clienti          pic x(2).
       77  status-destini          pic x(2).
       77  status-log4mas          pic x(2).
       77  status-lineseq          pic x(2).
       77  wstampa                 pic x(256).
                                            
       77  como-articolo           pic 9(6).
                                            
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).
       77  como-impegnato          pic s9(9).
       77  como-riga               pic x(100). 

       77  como-data-del           pic 9(8).
       77  como-anno               pic 9(4).
       77  num-del                 pic 9(5) value 0.

       77  nargs    pic 99 comp-1.      
       01  r-inizio                pic x(25).

       01  filler           pic 9.
           88 RichiamoSchedulato    value 1, false 0.

       LINKAGE SECTION.
           copy "link-batch.def".

       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.
      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                set errori to true
                display message "File not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TXT"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output lineseq
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
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
           string  wstampa      delimited low-value
                   "LOG4MAS_"   delimited size
                   como-data    delimited size
                   "_"          delimited size
                   como-ora     delimited size
                   ".log"       delimited size
              into wstampa
           end-string.                 
     
           if RichiamoSchedulato          
              move wstampa to batch-log
           end-if.
           
           set RecLocked   to false.

      ***---
       OPEN-FILES.                           
           open output lineseq.
           open i-o log4mas.
           if tutto-ok
              open input articoli progmag mtordini mrordini
                         clienti  destini
           end-if.                        
      
      ***---
       ELABORAZIONE.         
           move 0 to num-del.          
           move como-data(1:4) to como-anno.
           subtract 1 from como-anno.
           string como-anno      delimited size
                  como-data(5:4) delimited size
             into como-data-del
           end-string.

           initialize como-riga.
           string "CANCELLAZIONE DATI FINO AL: " 
                  como-data-del(7:2)
                  "/"
                  como-data-del(5:2)
                  "/"
                  como-data-del(1:4)
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.    

           move low-value to l4m-rec.
           start log4mas key >= l4m-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read log4mas next at end exit perform end-read
                    if l4m-data-estrazione > como-data-del
                       exit perform
                    end-if

                    add 1 to num-del
                    delete log4mas record
                 end-perform
           end-start.

           initialize como-riga.
           string "CANCELLATI RECORDS: "
                  num-del
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.    

           move 0 to num-del.
           initialize como-riga.
           string "CANCELLAZIONE DATI DEL: " 
                  como-data(7:2)
                  "/"
                  como-data(5:2)
                  "/"
                  como-data(1:4)
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.    
                                   
           move low-value to l4m-rec.
           move como-data to l4m-data-estrazione.
           start log4mas key >= l4m-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read log4mas next at end exit perform end-read
                    if l4m-data-estrazione > como-data
                       exit perform
                    end-if

                    add 1 to num-del
                    delete log4mas record
                 end-perform
           end-start.

           initialize como-riga.
           string "CANCELLATI RECORDS: "
                  num-del
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.    

           move "INIZIO ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.

           move low-value to art-rec.
           start articoli key >= art-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read articoli next at end exit perform end-read
                    if art-scorta not = 4 exit perform cycle end-if

                    initialize como-riga
                    string "TROVATO ARTICOLO A SCORTA 4: "
                           art-codice
                      into como-riga
                    end-string 
                    perform SCRIVI-RIGA-LOG

                    initialize prg-chiave 
                               replacing numeric data by zeroes
                                    alphanumeric data by spaces
                    move art-codice to prg-cod-articolo
                    read progmag no lock
                         invalid exit perform cycle
                     not invalid

                         initialize como-riga
                         string "IMPEGNATO GDO: "
                                prg-imp-gdo
                                " - IMPEGNATO TRAD: "
                                prg-imp-trad
                           into como-riga
                         end-string 
                         perform SCRIVI-RIGA-LOG

                         compute como-impegnato =
                                 prg-imp-gdo +
                                 prg-imp-trad
                         if como-impegnato <= 0
                            exit perform cycle
                         end-if
                    end-read   

                    move low-value to mro-rec
                    move art-codice to mro-cod-articolo
                    move 2021       to mro-anno
                    start mrordini key >= mro-k-articolo
                          invalid continue 
                      not invalid
                          perform until 1 = 2
                             read mrordini next 
                               at end exit perform 
                             end-read
                             if mro-cod-articolo not = art-codice
                                exit perform
                             end-if
                             move mro-chiave-testa to mto-chiave
                             read mtordini
                                  invalid continue
                              not invalid  

                                  initialize como-riga
                                  string "MASTER: "
                                         mto-anno
                                         " - "
                                         mto-numero
                                         " - STATO: "
                                         mto-stato-ordine
                                         " - QTA: "
                                         mro-qta
                                         " - QTA EVASA: "
                                         mro-qta-e
                                    into como-riga
                                  end-string 
                                  perform SCRIVI-RIGA-LOG

                                  if mto-chiuso 
                                     exit perform cycle 
                                  end-if
                                  if mro-qta <= mro-qta-e
                                     exit perform cycle
                                  end-if
                                  set cli-tipo-c to true
                                  move mto-cod-cli to cli-codice
                                  read clienti no lock 
                                       invalid 
                                       move "NON TROVATO" 
                                         to cli-ragsoc-1
                                  end-read
                                  move cli-codice to des-codice
                                  move mto-prg-destino to des-prog
                                  read destini no lock
                                       invalid
                                       move "NON TROVATO" 
                                         to des-ragsoc-1
                                       move "NON TROVATO" 
                                         to des-localita
                                  end-read
                                  perform SCRIVI-LOG4MAS
                             end-read
                          end-perform
                    end-start
                 end-perform
           end-start.   
           move "FINE ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.

      ***---
       SCRIVI-LOG4MAS.
           if como-articolo not = art-codice  
              move art-codice to como-articolo
              initialize l4m-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              move como-data       to l4m-data-estrazione
              move art-codice      to l4m-articolo
              move art-descrizione to l4m-art-des
              move como-impegnato  to l4m-pezzi
              perform WRITE-LOG4MAS
           end-if.
           initialize l4m-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           compute l4m-pezzi = mro-qta - mro-qta-e.
           
           move como-data       to l4m-data-estrazione.
           move art-codice      to l4m-articolo.
           move art-descrizione to l4m-art-des. 
           move mto-chiave      to l4m-mto-chiave.
           move cli-codice      to l4m-cli-codice.
           move cli-ragsoc-1    to l4m-cli-ragsoc.
           move des-prog        to l4m-des-prog.
           move des-ragsoc-1    to l4m-des-ragsoc.
           move des-localita    to l4m-localita.
           move mto-num-ord-cli to l4m-num-ord-cli.
           move mto-data-ordine to l4m-mto-data-ordine.
           perform WRITE-LOG4MAS.

      ***---
       WRITE-LOG4MAS.
           perform until 1 = 2
              add 1 to l4m-prog
              write l4m-rec
                    invalid continue
                not invalid exit perform
              end-write
           end-perform.


      ***---
       CLOSE-FILES.
           close lineseq articoli progmag mtordini mrordini
                 clienti destini log4mas.

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
