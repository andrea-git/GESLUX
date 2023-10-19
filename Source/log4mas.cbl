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
           
       SELECT csvFile
           ASSIGN       TO path-csvFile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-csvFile.

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

       FD  csvFile.
       01 line-csvFile        PIC  x(1000).

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
       77  status-csvFile          pic x(2).
       77  path-csvFile            pic x(256).
       77  status-lineseq          pic x(2).
       77  wstampa                 pic x(256).

       77  separatore              pic x.
       77  como-articolo           pic 9(6).
                                            
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).
       77  como-qta                pic z(8).
       77  como-impegnato          pic s9(9).
       77  como-riga               pic x(100). 

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

           accept separatore from environment "SEPARATORE".
                                          
           initialize path-csvFile.
           accept path-csvFile from environment "LOG4MAS_PATH".
           if path-csvFile = spaces
              display message 
                      "Valorizzare la variabile d'ambiente LOG4MAS_PATH"
               x"0d0a""Elaborazione interrotta"
                        title titolo
                         icon 2
              goback
           end-if.
                                          
           inspect path-csvFile replacing trailing spaces by low-value.
           string  path-csvFile    delimited low-value
                   "log4mas-path_" delimited size
                   como-data       delimited size
                   ".csv"          delimited size
              into path-csvFile
           end-string.

      ***---
       OPEN-FILES.                           
           open output lineseq csvFile.
           if tutto-ok
              open input articoli progmag mtordini mrordini
                         clienti  destini
           end-if.                        
      
      ***---
       ELABORAZIONE.     
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
                                  perform SCRIVI-CSV
                             end-read
                          end-perform
                    end-start
                 end-perform
           end-start.   
           move "FINE ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.

      ***---
       SCRIVI-CSV.
           if como-articolo = 0
              initialize line-csvFile
              string "Articolo"    delimited size
                     separatore    delimited size
                     "Descrizione" delimited size
                     separatore    delimited size
                     "Anno"        delimited size
                     separatore    delimited size
                     "Numero"      delimited size
                     separatore    delimited size
                     "Cliente"     delimited size
                     separatore    delimited size
                     "Destino"     delimited size
                     separatore    delimited size
                     "Località"    delimited size
                     separatore    delimited size
                     "Ord.Cli."    delimited size
                     separatore    delimited size
                     "Data"        delimited size
                     separatore    delimited size
                     "Pezzi"       delimited size
                     separatore    delimited size
                into line-csvFile
              end-string
              write line-csvFile
           end-if.
           if como-articolo not = art-codice
              initialize line-csvFile
              move art-codice to como-articolo    
              move como-impegnato to como-qta
              string art-codice      delimited size
                     separatore      delimited size
                     art-descrizione delimited size
                     separatore      delimited size
                     separatore      delimited size
                     separatore      delimited size
                     separatore      delimited size
                     separatore      delimited size
                     separatore      delimited size
                     separatore      delimited size
                     separatore      delimited size
                     como-qta        delimited size
                into line-csvFile
              end-string
              write line-csvFile
           end-if.                          
           initialize line-csvFile.
           compute como-qta = mro-qta - mro-qta-e.
           string separatore   delimited size
                  separatore   delimited size
                  mto-anno     delimited size
                  separatore   delimited size
                  mto-numero   delimited size
                  separatore   delimited size
                  cli-ragsoc-1 delimited size
                  separatore   delimited size
                  des-ragsoc-1 delimited size
                  separatore   delimited size
                  des-localita delimited size
                  separatore   delimited size
                  mto-num-ord-cli 
                  separatore   delimited size
                  mto-data-ordine(7:2)       
                  "/"
                  mto-data-ordine(5:2)       
                  "/"
                  mto-data-ordine(1:4)
                  separatore   delimited size
                  como-qta
                  separatore   delimited size
             into line-csvFile
           end-string.
           write line-csvFile.


      ***---
       CLOSE-FILES.
           close lineseq articoli progmag mtordini mrordini
                 clienti destini csvFile.

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
