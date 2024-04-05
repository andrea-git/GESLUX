       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      statocons.
       AUTHOR.                          Andrea.
       REMARKS. Imposta gli stati di consegna come MERCE CONSEGNATA
                per i vettori DIRETTO con fattura emessa
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "lineseq.fd".   

       WORKING-STORAGE SECTION.
       77  status-tordini        pic xx.
       77  status-lineseq        pic xx.
       77  wstampa               pic x(256).   
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).    
       77  nargs                 pic 99  comp-1 value 0.   

       77  counter               pic 9(9).
       77  counter2              pic 9(9).
       77  counter-edit          pic zzz.zzz.zz9.


       01  r-inizio              pic x(25).

       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.       
       77  tentativi             pic 99.
       77  anno-in-corso         pic 9(4) value 0.

       77  resto                 pic 9(3).
       77  diff-giorni           pic 9(5).

       77  como-ora              pic 9(8).
       77  como-data             pic 9(8).

       77  como-riga             pic x(300).

       77  filler        pic 9 value 0.
           88  nessun-errore   value 0.
           88  errore-warning  value 1.
           88  errore-error    value 2.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
      
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.   

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato          value 1, false 0.

       LINKAGE SECTION.
           copy "link-batch.def".
      

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TORDINI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TORDINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
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
           set nessun-errore to true.

           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.
           if RichiamoSchedulato
              initialize wstampa
              accept como-data from century-date
              accept como-ora  from time
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa      delimited low-value
                      "STATOCONS_" delimited size
                      como-data    delimited size
                      "_"          delimited size
                      como-ora     delimited size
                      ".log"       delimited size
                      into wstampa
              end-string
              set RichiamoSchedulato to true
              move wstampa to batch-log
              open output lineseq
           else
              display "Ricalcolo in corso..."
           end-if.

           set tutto-ok    to true.
           set prima-volta to true.
           perform SETTA-INIZIO-RIGA.

           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.

           initialize como-riga.
           string r-inizio              delimited size
                  "INIZIO ELABORAZIONE" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       OPEN-FILES.
           open i-o tordini.

      ***---
       ELABORAZIONE.
           move como-data(1:4) to anno-in-corso.
           if not RichiamoSchedulato
              display "Elaborazione ordini anno in corso..."
           end-if.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                            delimited size
                  "ELABORAZIONE ORDINI ANNO IN CORSO" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           move 0 to counter counter2.
           set tutto-ok   to true.
           move low-value to tor-rec.
           move anno-in-corso to tor-anno-fattura.
           start tordini key >= k-agfatt
                 invalid set errori to true
           end-start.                  

           if tutto-ok
              perform until 1 = 2
                 read tordini next no lock 
                      at end exit perform 
                 end-read

                 if tor-anno-fattura not = anno-in-corso
                    exit perform
                 end-if
                 if tor-esito-consegna = space and tor-vettore = 0
                    set RecLocked to false
                    read tordini lock
                    if RecLocked    
                       perform SETTA-INIZIO-RIGA
                       initialize como-riga
                       string r-inizio           delimited size
                              "EVASIONE N. "     delimited size
                              tor-anno           delimited size
                              " - "              delimited size
                              tor-numero         delimited size
                              " FATTURA: "       delimited size
                              tor-anno-fattura   delimited size
                              " - "              delimited size
                              tor-num-fattura    delimited size         
                              " *** IN USO ***"  delimited size
                              into como-riga
                       end-string
                       perform RIGA-LOG
                       set errore-warning to true
                    else
                       move "OK" to tor-esito-consegna
                       rewrite tor-rec
                       unlock tordini all records
                    end-if
                    start tordini key > k-agfatt
                          invalid exit perform
                    end-start
                 end-if

              end-perform
           end-if.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                delimited size
                  "FINE ELABORAZIONE "    delimited size
                  " ORDINI ANNO IN CORSO" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       CONTATORE-VIDEO.
           add 1 to counter counter2

           if counter2 = 100
              move counter to counter-edit
              display counter-edit 
                      upon batch-win-handle
                      line 25,00
                    column 38,00
              move 0 to counter2
           end-if.

      ***--
       CLOSE-FILES.
           close tordini.

      ***---
       EXIT-PGM.
           perform SETTA-INIZIO-RIGA.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
           compute tot-secondi = end-secondi - start-secondi.      

           if tot-secondi < 60
              if RichiamoSchedulato    
                 move tot-secondi to ss         
                 initialize line-riga of lineseq
                 string "ELABORAZIONE TERMINATA IN: ",
                        ss, " SECONDI"
                        into line-riga of lineseq
                 end-string
                 write line-riga of lineseq
              else
                 move tot-secondi to ss
                 display "ELABORAZIONE TERMINATA IN: ",
                         ss, " SECONDI"
                    upon syserr 
              end-if
           else
              divide tot-secondi by 60 giving mm remainder ss
              if RichiamoSchedulato
                 initialize line-riga of lineseq
                 string "ELABORAZIONE TERMINATA IN: ",
                         mm, " MINUTI E ", ss, " SECONDI"
                         into line-riga of lineseq
                 end-string
                 write line-riga of lineseq
              else
                 display "ELABORAZIONE TERMINATA IN: ",
                         mm, " MINUTI E ", ss, " SECONDI"
                    upon syserr
              end-if
           end-if.

           if RichiamoSchedulato
              close       lineseq
              evaluate true
              when nessun-errore  move  0 to batch-status
              when errore-warning move  1 to batch-status
              when errore-error   move -1 to batch-status
              end-evaluate
              display "                                             "
                 upon batch-win-handle
                   line 25,00
                 column 35,00
           end-if.

           goback.      

      ***---
       RIGA-LOG.
           if RichiamoSchedulato
              initialize line-riga of lineseq
              write line-riga of lineseq from como-riga
           else
              display como-riga upon syserr
           end-if.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
