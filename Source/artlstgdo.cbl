       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      artlstgdo.
       AUTHOR.                          Andrea.
       REMARKS. 
           Batch con lancio in notturno.

           Per tutti gli articoli con:
           - flag GDS/DO
           - scorta con nuovo parametro flag "inserimento automatico 
             listino" (in tabella scorte)
           - listino fornitori valido alla data odierna,

           calcolo il prz. confronto e salvo il + basso 

           a questo punto per tutti i gruppi GDO con:
           - nuovo parametro flag "inserimento automatico listino" 
            (in tabella gruppi GDO)
           inserisco l'articolo a listino con data validità odierna 
           (solo se quell'articolo non è già presente), creando la 
           riga se già presente un listino o un listino se non è 
           presente nulla. 
           Inserisco come prezzo il prezzo salvato diviso un nuovo 
           parametro numerico (in tabella parametri generali)

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "tgrupgdo.sl".
           copy "tscorte.sl". 
           copy "tlistini.sl".
           copy "rlistini.sl".
           copy "tparamge.sl".  
           copy "listini.sl".   
           copy "clienti.sl".   
           copy "destinif.sl".   
           copy "lineseq.sl".   
           copy "distinteb.sl".
           copy "progmag.sl".
           copy "impforn.sl".    
           copy "timposte.sl".
           copy "tpiombo.sl".
           copy "param.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "tgrupgdo.fd".
           copy "tscorte.fd". 
           copy "tlistini.fd".
           copy "rlistini.fd".
           copy "tparamge.fd".
           copy "listini.fd".  
           copy "clienti.fd".   
           copy "destinif.fd".   
           copy "lineseq.fd".    
           copy "distinteb.fd".  
           copy "progmag.fd".
           copy "impforn.fd".
           copy "timposte.fd".   
           copy "tpiombo.fd".
           copy "param.fd".

       WORKING-STORAGE SECTION.            
       77  status-articoli       pic xx.
       77  status-tgrupgdo       pic xx.
       77  status-tscorte        pic xx.
       77  status-tlistini       pic xx.
       77  status-rlistini       pic xx.
       77  status-tparamge       pic xx.
       77  status-listini        pic xx.
       77  status-clienti        pic xx.
       77  status-destinif       pic xx.   
       77  status-progmag        pic xx.
       77  status-impforn        pic xx.
       77  status-timposte       pic xx.
       77  status-distinteb      pic xx. 
       77  status-tpiombo        pic xx.
       77  status-param          pic xx.
       77  status-lineseq        pic xx.
       77  wstampa               pic x(256).   
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).    
       77  nargs                 pic 99  comp-1 value 0.   

       77  counter               pic 9(9).
       77  counter2              pic 9(9).
       77  counter-edit          pic zzz.zzz.zz9.    
       77  s-codice              pic z(15).
       77  codice-x              pic x(15).
                                         
       77  idx                   pic 999.
       77  idx-log               pic 999.
       77  prz-confronto-ed      pic zzz.zzz.zz9,9999.
       77  tot-righe             pic 9(10) value 0.

       01  r-inizio              pic x(25).

       77  tentativi             pic 99.

       77  resto                 pic 9(3).
       77  diff-giorni           pic 9(5).

       77  como-ora              pic 9(8).
       77  como-data             pic 9(8).
                    
       |Mi salvo subito i gruppi gdo con quel flag per non doverlo rivfare ogni volta
       01  tab-gdo               occurs 999.
           05 el-gdo             pic x(5).

       01  tab-gdo-log.
         03                      occurs 999.
           05 el-gdo-log         pic x(5).
           05 el-gdo-div         pic x(3).

       77  como-riga             pic x(1000).

       77  filler        pic 9 value 0.
           88  nessun-errore   value 0.
           88  errore-warning  value 1.
           88  errore-error    value 2.
                                              
       77  controllo             pic xx.
           88 tutto-ok          value "OK".
           88 errori            value "ER".   

       77  calcolo-piombo       pic x.
           88 nuovo-calcolo-piombo value "N".

       77  filler                pic 9.
           88 trovato           value 1, false 0.
      
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.   

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato          value 1, false 0.  

       77 s-prz-confronto    PIC 9(6)v9999.    

       copy "prz-finito-forn.def".
       copy "costo-medio.def".   
       copy "imposte-fornitore.def".
       copy "trova-parametro.def".

       LINKAGE SECTION.
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.  
                                                 
      ***---
       LISTINI-ERR SECTION.
           use after error procedure on LISTINI.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-LISTINI
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [LISTINI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [LISTINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[LISTINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true   
           when "93"
           when "99" set RecLocked to true
           end-evaluate.   
                  
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on PROGMAG.
           set tutto-ok  to true.
           evaluate status-PROGMAG
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [PROGMAG] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [PROGMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[PROGMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.   
                  
      ***---
       PARAM-ERR SECTION.
           use after error procedure on PARAM.
           set tutto-ok  to true.
           evaluate status-PARAM
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [PARAM] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [PARAM] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[PARAM] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.   
                  
                  
      ***---
       TPIOMBO-ERR SECTION.
           use after error procedure on TPIOMBO.
           set tutto-ok  to true.
           evaluate status-TPIOMBO
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TPIOMBO] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TPIOMBO] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TPIOMBO] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.   
                  
      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on TIMPOSTE.
           set tutto-ok  to true.
           evaluate status-TIMPOSTE
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TIMPOSTE] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TIMPOSTE] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TIMPOSTE] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.   
                  
      ***---
       IMPFORN-ERR SECTION.
           use after error procedure on IMPFORN.
           set tutto-ok  to true.
           evaluate status-IMPFORN
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [IMPFORN] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [IMPFORN] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[IMPFORN] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.   
                  
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on CLIENTI.
           set tutto-ok  to true.
           evaluate status-CLIENTI
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [CLIENTI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [CLIENTI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[CLIENTI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.   
                  
      ***---
       DISTINTEB-ERR SECTION.
           use after error procedure on DISTINTEB.
           set tutto-ok  to true.
           evaluate status-DISTINTEB
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [DISTINTEB] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [DISTINTEB] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[DISTINTEB] indexed file corrupt!"delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.   
                  
      ***---
       DESTINIF-ERR SECTION.
           use after error procedure on DESTINIF.
           set tutto-ok  to true.
           evaluate status-DESTINIF
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [DESTINIF] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [DESTINIF] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[DESTINIF] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.   
                  
      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on TPARAMGE.     
           set tutto-ok  to true.
           evaluate status-TPARAMGE
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TPARAMGE] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TPARAMGE] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TPARAMGE] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.    
                                                 
      ***---
       RLISTINI-ERR SECTION.
           use after error procedure on RLISTINI.     
           set tutto-ok  to true.
           evaluate status-RLISTINI
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RLISTINI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RLISTINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[RLISTINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.   
                                                 
      ***---
       TLISTINI-ERR SECTION.
           use after error procedure on TLISTINI.     
           set tutto-ok  to true.
           evaluate status-TLISTINI
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TLISTINI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TLISTINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TLISTINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.     
                                 
      ***---
       TSCORTE-ERR SECTION.
           use after error procedure on TSCORTE.     
           set tutto-ok  to true.
           evaluate status-TSCORTE
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TSCORTE] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TSCORTE] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TSCORTE] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.     
         
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.     
           set tutto-ok  to true.
           evaluate status-ARTICOLI
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [ARTICOLI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [ARTICOLI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[ARTICOLI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
           end-evaluate.

      ***---
       TGRUPGDO-ERR SECTION.
           use after error procedure on TGRUPGDO.
           set tutto-ok  to true.
           evaluate status-TGRUPGDO
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TGRUPGDO] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TGRUPGDO] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TGRUPGDO] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true 
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
           set environment "STRIP_TRAILING_SPACES" to "1".
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
                      "ARTLSTGDO_" delimited size
                      como-data    delimited size
                      "_"          delimited size
                      como-ora     delimited size
                      ".log"       delimited size
                 into wstampa
              end-string
              set RichiamoSchedulato to true
              move wstampa to batch-log
              open output lineseq
           end-if.

           set tutto-ok    to true.
           set prima-volta to true.
           perform SETTA-INIZIO-RIGA.

           initialize como-riga.
           string r-inizio              delimited size
                  "INIZIO ELABORAZIONE" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       OPEN-FILES.
           open input articoli tgrupgdo tscorte tlistini rlistini
                      tparamge clienti destinif distinteb progmag
                      impforn timposte tpiombo param.
           open i-o listini.

      ***---
       ELABORAZIONE.  
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                      delimited size
                  "RICERCA GRUPPI GDO IN CORSO" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.
                            
           if el-gdo(1) = spaces
              move spaces to tge-chiave
              read tparamge no lock
                   
              move 0 to idx
              move low-value to gdo-codice
              start tgrupgdo key >= gdo-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tgrupgdo next at end exit perform end-read
                       if gdo-auto-lst-si
                          add 1 to idx
                          move gdo-codice to el-gdo(idx)
                       end-if
                    end-perform
              end-start
           end-if.
           if idx = 0
              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                      delimited size
                     "NESSUN GRUPPO GDO IMPOSTATO" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
              set errore-warning to true
              exit paragraph
           end-if.     

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                         delimited size
                  "ELABORAZIONE ARTICOLI IN CORSO" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           move 0 to counter counter2.
           set tutto-ok   to true.
           move low-value to art-rec.
           start articoli key >= art-chiave
                 invalid set errori to true
           end-start.                  

           if tutto-ok
              perform until 1 = 2
                 read articoli next no lock 
                      at end exit perform 
                 end-read
                                           
                 if art-si-do and art-attivo
                    move art-scorta to sco-codice
                    read tscorte no lock
                         invalid continue
                     not invalid
                         if sco-auto-lst-si 
                            perform TROVA-LISTINO-F
                            if trovato
                               perform CREA-LISTINI
                            end-if
                         end-if
                    end-read
                 end-if

              end-perform
           end-if.        
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                      delimited size
                  "FINE ELABORAZIONE ARTICOLI"  delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio     delimited size
                  "INSERITI "  delimited size
                  tot-righe    delimited size
                  " ARTICOLI"  delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.
                     
      ***---
       TROVA-LISTINO-F.
           set trovato to false.

           move low-value  to rlis-chiave-ricerca.
           move art-codice to rlis-articolo.  
           move 999999,9999 to s-prz-confronto.
           move 0 to s-codice.

           start rlistini key >= rlis-k-art 
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rlistini next no lock 
                      at end exit perform 
                    end-read
                    if art-codice not = rlis-articolo
                       exit perform
                    end-if

      *    controllo che sia compreso nelle data odierna
                    if como-data >= rlis-ini-val and 
                       como-data <= rlis-fine-val 
                       move rlis-fornitore     to cli-codice
                       set cli-tipo-F to true
                       read clienti no lock invalid continue end-read
                       move 0                 to prg-peso-utf
                       move 0                 to prg-peso-non-utf
                       move 0                 to prg-peso

                       move rlis-codice       to tlis-codice
                       read tlistini no lock invalid continue end-read 
                       move tlis-fornitore    to desf-codice
                       move rlis-destino      to desf-prog
                       read destinif no lock invalid continue end-read

                       move tlis-trasp-f     to como-trasporto-f
                       move tlis-trasp-c     to como-trasporto-c
                       |13012010
                       move art-peso-utf     of articoli 
                                             to prg-peso-utf
                       move art-peso-non-utf of articoli
                                             to prg-peso-non-utf
                       perform CALCOLA-PRZ-FINITO
                       add 0,0005             to prz-confronto
                       add 0,005              to prz-confronto

                       add 0,0005             to prz-reale
                       add 0,005              to prz-reale
           
                       if prz-confronto < s-prz-confronto
                          move prz-confronto to s-prz-confronto
                          set trovato to true
                          move rlis-codice to s-codice
                          exit perform
                       end-if
                    end-if
                 end-perform
           end-start.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           if trovato     
              move s-codice to codice-x                  
              inspect codice-x replacing leading x"30" by x"20"
              call "C$JUSTIFY" using codice-x , "L"
              inspect codice-x replacing trailing spaces by low-value
              move s-prz-confronto to prz-confronto-ed
              string r-inizio         delimited size
                     "ART: "          delimited size
                     art-codice       delimited size 
                     " - TROVATO L: " delimited size
                     codice-x         delimited low-value
                     " PREZZO: "      delimited size
                     prz-confronto-ed delimited size
                     into como-riga
              end-string
           else
              string r-inizio                     delimited size 
                     "ARTICOLO: "                 delimited size
                     art-codice                   delimited size
                     " - NESSUN LISTINO TROVATO " delimited size
                into como-riga
              end-string
           end-if.
           perform RIGA-LOG.

      ***---
       CALCOLA-TRASPORTO.
           move 0 to costo-trasporto.
           move spaces to tge-chiave.
           read tparamge no lock. 
           if como-trasporto-f = 1
              compute costo-trasporto = 
                      prg-peso * tge-trasp-f
           end-if.
           if como-trasporto-c = 1
              compute costo-trasporto = costo-trasporto +
                    (prg-peso * tge-trasp-c)
           end-if. 

      ***---
       CREA-LISTINI.
           move 0 to idx idx-log.
           initialize tab-gdo-log.

           perform varying idx from 1 by 1 
                     until idx > 999
              if el-gdo(idx) = spaces
                 exit perform
              end-if         
              move low-value   to lst-rec
              move el-gdo(idx) to lst-gdo
              move art-codice  to lst-articolo
              move como-data   to lst-data
              set trovato to false
              start listini key <= lst-k-gdo-articolo   
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read listini previous no lock
                         at end exit perform 
                       end-read
                       if lst-gdo      not = el-gdo(idx) or
                          lst-articolo not = art-codice
                          exit perform
                       end-if
                       |Se il più recente non è FA
                       if lst-prezzo = 99999999,99
                          set trovato to false
                       else                       
                          set trovato to true
                       end-if
                       exit perform
                    end-perform
              end-start                                  
              if not trovato                
                 initialize lst-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 add 0,005 to s-prz-confronto giving lst-prezzo
                 if tge-divisore-lstgdo not = 0
                    compute lst-prezzo = lst-prezzo / 
                                         tge-divisore-lstgdo
                 end-if

                 add 1 to idx-log
                 move el-gdo(idx) to el-gdo-log(idx-log)
                 move " - "       to el-gdo-div(idx-log)

                 move el-gdo(idx)     to lst-gdo
                 move como-data       to lst-data
                 move art-codice      to lst-articolo
                 accept lst-data-creazione from century-date
                 accept lst-ora-creazione  from time
                 move "AUTO"          to lst-utente-creazione
                 add 1 to tot-righe
                 write lst-rec    
              end-if    
           end-perform.  
                            
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           if idx-log > 0
              move spaces to el-gdo-div(idx-log)
              inspect tab-gdo-log replacing trailing spaces by low-value
              move lst-prezzo to prz-confronto-ed
              string r-inizio                           delimited size
                     "**CREATO ARTICOLO A LISTINO PER:" delimited size 
                     tab-gdo-log                   delimited low-value
                     " CON PREZZO: "                    delimited size
                     prz-confronto-ed                   delimited size
                into como-riga
              end-string
              perform RIGA-LOG
           end-if.

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
           close articoli tgrupgdo tscorte tlistini rlistini progmag
                 tparamge listini clienti destinif distinteb timposte
                 impforn tpiombo param.

      ***---
       EXIT-PGM.
           perform SETTA-INIZIO-RIGA.   

           initialize como-riga.
           string r-inizio            delimited size
                  "FINE ELABORAZIONE" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

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
               
           set environment "STRIP_TRAILING_SPACES" to " ".

           goback.                  

      ***---
       RIGA-LOG.
           if RichiamoSchedulato
              initialize line-riga 
              write line-riga from como-riga
           else
              display como-riga upon syserr
           end-if.

      ***---
       RECUPERO-ANAGRAFICA.

      ***---
       PARAGRAFO-COPY.
           copy "prz-finito-forn.cpy".
           copy "costo-medio.cpy".
           copy "imposte-fornitore.cpy".
           copy "addizionale-piombo-fornitore.cpy".
           copy "trova-parametro.cpy".
           copy "setta-inizio-riga.cpy".
