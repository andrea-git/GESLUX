       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ricalfor-bat.
       AUTHOR.                          Andrea.
       REMARKS. Ricalcolo dati ordfor2:
                - promo
                - giacenza
                - costo mp
                - fabbisogno 6 mesi
                Nel caso il file non sia mai stato generato prendo 
                i dati dal file ordfor.
                SERVE PER GLI AVVISI AUTOMATICI!!!
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "tparamge.sl".
           copy "ordfor.sl".
           copy "ordfor2.sl".
           copy "tmarche.sl".
           copy "articoli.sl".
           copy "progmag.sl".
           copy "timbalqta.sl".
           copy "lineseq.sl".
           copy "rlistini.sl".
           copy "tlistini.sl".
           copy "tscorte.sl".
           copy "coperfab.sl".
           copy "clienti.sl".
           copy "destinif.sl".
           copy "tmp-forn.sl".
           copy "tpiombo.sl".
           copy "timposte.sl".
           copy "tmagaz.sl".
           copy "impforn.sl".
           copy "catart.sl".
           copy "tmp-ordfor.sl".
           copy "param.sl".
           copy "lineseq-mail.sl".

       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq1.

           copy "qta-pordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".
           copy "tparamge.fd".
           copy "ordfor.fd".
           copy "ordfor2.fd".
           copy "tmarche.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "timbalqta.fd".
           copy "lineseq.fd".
           copy "rlistini.fd".
           copy "tlistini.fd".
           copy "tscorte.fd". 
           copy "coperfab.fd".
           copy "clienti.fd".
           copy "destinif.fd".
           copy "tmp-forn.fd".
           copy "tpiombo.fd".
           copy "timposte.fd".
           copy "tmagaz.fd". 
           copy "impforn.fd".
           copy "catart.fd".
           copy "tmp-ordfor.fd".
           copy "param.fd".
           copy "lineseq-mail.fd".

       FD  lineseq1.
       01 line-riga        PIC  x(900).   

           copy "qta-pordini.fd".

       WORKING-STORAGE SECTION.
           copy "mail.def".
           copy "costo-medio.def".
           copy "ordfor2.def".
           copy "imposte.def".
           copy "prz-finito-forn.def".
           copy "trova-parametro.def".

       78  titolo              value "Batch Ricalcolo giacenze ordfor2".
       78  user-codi           value "BATCH".

       77  status-tparamge       pic xx.
       77  status-tsetinvio      pic xx.
       77  status-ordfor         pic xx.
       77  status-ordfor2        pic xx.
       77  status-progmag        pic xx.
       77  status-tmarche        pic xx.
       77  status-articoli       pic xx.
       77  status-timbalqta      pic xx.
       77  status-rlistini       pic xx.
       77  status-tlistini       pic xx.
       77  status-lineseq        pic xx.
       77  status-lineseq1       pic xx.
       77  status-tscorte        pic xx.
       77  status-coperfab       pic xx.
       77  status-clienti        pic xx.
       77  status-destinif       pic xx.
       77  status-tmp-forn       pic xx.
       77  status-tpiombo        pic xx.
       77  status-timposte       pic xx.
       77  status-tmagaz         pic xx.
       77  status-impforn        pic xx.
       77  status-catart         pic xx.
       77  status-param          pic xx.
       77  status-tmp-ordfor     pic xx.
       77  path-tmp-forn         pic x(256).
       77  path-tmp-ordfor       pic x(256).
       77  wstampa               pic x(256).
       77  status-qta-pordini    pic x(2).   
       77  status-lineseq-mail   pic xx.
       77  path-lineseq-mail     pic x(256).

      * VARIABILI
       77  idx                   pic 9(3).
       77  tot-anno              pic s9(12)v99 value 0.
       77  giacenza              pic 9(8).
       77  giac-positiva         pic 9(8).
       77  SaveImballo           pic x(3).
       77  articolo-fisso        pic 9(6).
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).

       01  r-inizio              pic x(25).
                                 
       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.       
       77  tentativi             pic 99.     
       77  nargs                 pic 99  comp-1 value 0.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8). 

       77  link-data             pic 9(8).
       77  como-riga             pic x(80).

       77  link-handle           handle of window value 0. |FITTIZIO
                                            
       77  filler                pic 9.
           88  nessun-errore     value 1, false 0.
                                            
       77  filler                pic 9 value 0.
           88  errore-venduti    value 1, false 0.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
           
       77  CosaElaborare         pic x.
           88  ElaboraGiacenza   value "G".
           88  ElaboraImpegnato  value "I".
      
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 ricalcolo          value 1, false 0.   
       01  filler                pic 9 value 0.
           88 RichiamoSchedulato          value 1, false 0.
      
       LINKAGE SECTION.
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.
       copy "mail-decl.cpy".
       
      ***---
       ORDFOR-ERR SECTION.
           use after error procedure on ordfor.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ordfor
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                     delimited size
                       "File [ORDFOR] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [ORDFOR] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "[ORDFOR] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.
       
      ***---
       ORDFOR2-ERR SECTION.
           use after error procedure on ordfor2.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ordfor2
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [ORDFOR2] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [ORDFOR2] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[ORDFOR2] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.
       
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [PROGMAG] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [PROGMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[PROGMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.
       
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
       RLISTINI-ERR SECTION.
           use after error procedure on rlistini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rlistini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [RLISTINI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [RLISTINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[RLISTINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.
       
      ***---
       tLISTINI-ERR SECTION.
           use after error procedure on tlistini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tlistini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [TLISTINI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [TLISTINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[TLISTINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.
       
      ***---
       TSCORTE-ERR SECTION.
           use after error procedure on tscorte.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tscorte
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TSCORTE] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TSCORTE] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TSCORTE] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.
       
      ***---
       TMAGAZ-ERR SECTION.
           use after error procedure on tmagaz.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmagaz
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TMAGAZ] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TMAGAZ] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TMAGAZ] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.                         
       
      ***---
       LINESEQ1-ERR SECTION.
           use after error procedure on lineseq1.
           continue.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
              if tutto-ok
                 perform ELABORAZIONE
                 perform CLOSE-FILES
              end-if
              perform EXIT-PGM
           else
              goback
           end-if.

      ***---
       INIT.
           set ricalcolo     to false.
           set nessun-errore to true.  
           set tutto-ok      to true.

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
                      "RICALFOR_"  delimited size
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
              display "Ricalcolo giacenze su ordfor2 in corso..."
              initialize LinkAddress FileOrig FileDest
              accept LinkAddress from environment "RICALFOR_ADDRESSES"
              accept FileOrig    from environment "RICALFOR_LOG"
              accept FileDest    from environment "RICALFOR_LOG_INVIO"
              if LinkAddress = spaces
                 set errori        to true
                 set nessun-errore to false
                 perform SETTA-INIZIO-RIGA

                 initialize como-riga
                 string r-inizio                         delimited size
                        "Configurare RICALFOR_ADDRESSES" delimited size
                        into como-riga
                 end-string
                 perform RIGA-LOG
                 display "Configurare RICALFOR_ADDRESSES"
              end-if
              if FileOrig = spaces
                 set errori        to true
                 set nessun-errore to false
                 perform SETTA-INIZIO-RIGA

                 initialize como-riga
                 string r-inizio                   delimited size
                        "Configurare RICALFOR_LOG" delimited size
                        into como-riga
                 end-string
                 perform RIGA-LOG
                 display "Configurare RICALFOR_LOG"
              end-if
              if FileDest = spaces
                 set errori        to true
                 set nessun-errore to false
                 perform SETTA-INIZIO-RIGA

                 initialize como-riga
                 string r-inizio                         delimited size
                        "Configurare RICALFOR_LOG_INVIO" delimited size
                        into como-riga
                 end-string
                 perform RIGA-LOG
                 display "Configurare RICALFOR_LOG_INVIO"
              end-if
           end-if.        

           if tutto-ok    
              perform SETTA-INIZIO-RIGA

              move como-ora(1:2) to hh
              move como-ora(3:2) to mm
              move como-ora(5:2) to ss
          
              compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss

              initialize como-riga
              string r-inizio              delimited size
                     "INIZIO ELABORAZIONE" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG

              open input tparamge tmagaz
              perform CALCOLA-PERIODO
              close tparamge
           end-if.

      ***---
       OPEN-FILES.
           open input 
                 progmag
                 articoli
                 tmarche
                 ordfor
                 timbalqta
                 rlistini
                 tlistini
                 tscorte
                 clienti 
                 destinif
                 impforn
                 catart
                 param.

           open i-o coperfab qta-pordini.

           if tutto-ok
              perform OPEN-ORDFOR2-LOCK
              if tutto-ok
                 perform OPEN-OUTPUT-TMP-ORDFOR
                 perform OPEN-OUTPUT-TMP-FORN
              end-if
           end-if.                             

      ***---
       OPEN-OUTPUT-TMP-FORN.
           accept como-ora  from time.
           accept como-data from time.
           initialize path-tmp-forn.
           accept  path-tmp-forn  from environment "PATH_ST".
           inspect path-tmp-forn  replacing trailing spaces 
                                         by low-value.
           string  path-tmp-forn  delimited low-value
                   "TMP-F-DISP"   delimited size
                   "_"            delimited size
                   data-calcolo   delimited size
                   "_"            delimited size
                   como-ora       delimited size
                   ".tmp"         delimited size
                   into path-tmp-forn
           end-string.
           open output tmp-forn.
           close       tmp-forn.
           open i-o    tmp-forn.

      ***---
       OPEN-ORDFOR2-LOCK.
           open exclusive i-o ordfor2.
           if RecLocked
              set errori to true
              set nessun-errore to false
              perform SETTA-INIZIO-RIGA

              initialize como-riga
              string r-inizio                     delimited size
                     "File [ORDFOR2] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
           end-if.

      ***---
       ELABORAZIONE.
           move low-value to ord2-rec.
           start ordfor2 key >= ord2-chiave
                 invalid
                 close ordfor2
                 perform SETTA-INIZIO-RIGA
                 initialize como-riga
                 string r-inizio                  delimited size
                        "INIZIO CREAZIONE RECORD" delimited size
                        into como-riga
                 end-string
                 perform RIGA-LOG
                 perform RIGENERA-QTA-VENDUTI
                 if tutto-ok
                    perform CREA-ORDFOR2
                    perform SETTA-INIZIO-RIGA
                    initialize como-riga
                    string r-inizio                delimited size
                           "FINE CREAZIONE RECORD" delimited size
                           into como-riga
                    end-string
                    perform RIGA-LOG
                 end-if
                                        

             not invalid
                 close ordfor2
                 open exclusive i-o ordfor2
           end-start.

           if errori
              set nessun-errore to false
              if not errore-venduti
                 perform SETTA-INIZIO-RIGA
                 initialize como-riga
                 string r-inizio                    delimited size
                        "ERRORE ORDFOR NON TROVATO" delimited size
                       into como-riga
                 end-string
              end-if
              perform RIGA-LOG
           else
              move low-value  to ord2-rec
              start ordfor2 key is >= ord2-chiave
                    invalid 
                    set errori        to true
                    set nessun-errore to false

                    perform SETTA-INIZIO-RIGA
                    initialize como-riga
                    string r-inizio                     delimited size
                           "ERRORE ORDFOR2 NON TROVATO" delimited size
                            into como-riga
                    end-string
                    perform RIGA-LOG

              end-start
           end-if.

           if tutto-ok
              initialize como-riga
              string r-inizio                      delimited size
                     "INIZIO ELABORAZIONE ORDFOR2" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG

              perform until 1 = 2
                 perform SETTA-INIZIO-RIGA

                 read ordfor2 next at end exit perform end-read

                 move ord2-articolo to art-codice
                 read articoli no lock
                      invalid continue
                  not invalid
                      move ord2-articolo to prg-cod-articolo
                      move spaces        to prg-cod-magazzino
                      move spaces        to prg-tipo-imballo
                      move 0             to prg-peso
                      read progmag no lock
                           invalid move 0 to costo-mp
                       not invalid perform AGGIORNA-RECORD
                      end-read
                 end-read

              end-perform
              close ordfor2

              initialize como-riga
              string r-inizio                    delimited size
                     "INIZIO ELABORAZIONE PROMO" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG

              if RichiamoSchedulato
                 move batch-win-handle to link-handle
              end-if               

              call   "sos-ordini" using como-data, 
                                        link-handle |FITTIZIO
              cancel "sos-ordini"

              initialize como-riga
              string r-inizio                  delimited size
                     "FINE ELABORAZIONE PROMO" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG

              initialize como-riga
              string r-inizio                    delimited size
                     "FINE ELABORAZIONE ORDFOR2" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG

           end-if.

      ***---
       CLOSE-FILES.
           close progmag
                 articoli
                 tmarche
                 ordfor
                 timbalqta
                 rlistini
                 tlistini
                 tscorte
                 tmagaz
                 clienti
                 destinif
                 impforn
                 catart
                 tmp-ordfor
                 param 
                 qta-pordini.
           delete file tmp-ordfor.


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
              close lineseq
              if nessun-errore
                 move  0 to batch-status
              else              
                 move -1 to batch-status
              end-if
           else
              perform INVIO-MAIL
           end-if.

           goback.

      ***---
       INVIO-MAIL.
           display "Invio mail in corso...".

           move "INVIO MAIL IN CORSO..." to como-riga.
           perform SETTA-RIGA-STAMPA.

           initialize LinkBody.
           if nessun-errore
              move "RICALCOLO ORDINI FORNITORI - OK" 
                to LinkSubject
           else
              move "RICALCOLO ORDINI FORNITORI - ATTENZIONE" 
                to LinkSubject
           end-if.

           move "In allegato dettagli funzionamento programma" 
             to LinkBody.

           accept LinkAddress from environment "RICALFOR_ADDRESSES".
           accept FileOrig    from environment "RICALFOR_LOG".
           accept FileDest    from environment "RICALFOR_LOG_INVIO".
           call "C$COPY" using FileOrig, FileDest, "S".
           move FileDest to LinkAttach.
                               
           move "ricalfor-bat" to NomeProgramma.

           move 5 to tentativi-mail.
           perform CICLO-SEND-MAIL.
               
           initialize como-riga.
           if mail-ok
              string r-inizio               delimited size
                     "INVIO MAIL RIUSCITO!" delimited size
                     into como-riga
              end-string
           else
              string r-inizio                   delimited size
                     "INVIO MAIL NON RIUSCITO!" delimited size
                     into como-riga
              end-string
           end-if.
           perform SETTA-RIGA-STAMPA.

           delete file lineseq-mail.     

      ***---
       AFTER-SEND-MAIL.
           call "C$DELETE" using FileDest.
           initialize como-riga.
           string r-inizio          delimited size
                  "TENTATIVO N. "   delimited size
                  tentativo-mail    delimited size
                  ": STATUS "       delimited size
                  StatusInvioMail   delimited size
                  " - "             delimited size
                  line-riga-mail    delimited size
             into como-riga
           end-string.
           perform SETTA-RIGA-STAMPA.

      ***---
       SETTA-RIGA-STAMPA.
           perform SETTA-INIZIO-RIGA.
           perform RIGA-LOG.

      ***---
       RIGA-LOG.
           if RichiamoSchedulato
              initialize line-riga of lineseq
              write line-riga of lineseq from como-riga
           else
              display como-riga upon syserr
           end-if.

      ***---
       RIGENERA-QTA-VENDUTI.           
           set tutto-ok to true.
           move como-data(1:4) to CurrentYear.
           subtract 1 from CurrentYear giving PastYear.  

           accept wstampa from environment "PATH_VENDUTI".
           open input lineseq1.
           if status-lineseq1 not = "00"
              set errore-venduti to true
              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio       delimited size
                     "ERRORE"       delimited size   
                     status-lineseq delimited size
                     " su: "        delimited size
                     wstampa        delimited size
                into como-riga
              end-string
              perform RIGA-LOG
           else   
              close    qta-pordini
              open i-o qta-pordini
              if status-qta-pordini not = "00"                  
                 set errore-venduti to true
                 perform SETTA-INIZIO-RIGA
                 initialize como-riga
                 string r-inizio           delimited size
                        "ERRORE"           delimited size
                        status-qta-pordini delimited size
                        " su: "            delimited size
                        "qta-pordini"      delimited size
                       into como-riga
                 end-string
                 perform RIGA-LOG
              else
                 move low-value to qp-chiave
                 start qta-pordini key >= qp-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read qta-pordini next 
                               at end exit perform 
                          end-read
                          delete qta-pordini record
                       end-perform
                 end-start
                 perform until 1 = 2
                    read lineseq1 next at end exit perform end-read
                    unstring line-riga of lineseq1 delimited by ";"
                        into r-articolo r-negativo 
                             r-qta r-anno r-mese r-note
                    end-unstring          
                    if r-articolo is numeric 
                       if r-negativo not = spaces
                          compute r-qta = r-qta * -1
                       end-if
                       move r-articolo to qp-articolo
                       move r-anno     to qp-anno
                       move r-mese     to qp-mese
                       move r-qta      to qp-qta qp-qta-ass
                       move r-note     to qp-note
                       move 1 to qp-prog
                       move como-data to qp-data-import
                       move como-ora  to qp-ora-import
                       perform until 1 = 2
                          write qp-rec
                                invalid add 1 to qp-prog
                            not invalid exit perform
                          end-write
                       end-perform
                    end-if
                 end-perform 
              end-if     
              close lineseq1
           end-if.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "costo-medio.cpy".
           copy "ordfor2.cpy".
           copy "imposte.cpy".
           copy "addizionale-piombo.cpy".
           copy "trova-parametro.cpy".
           copy "setta-inizio-riga.cpy".

      ***--- |DUMMY NON TOCCARE
       RECUPERO-ANAGRAFICA.
       COLORA-FABBISOGNO.
       CALCOLA-PRZ-FINITO.
