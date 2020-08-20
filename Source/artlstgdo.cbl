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

       77  idx                   pic 999.
       77  prz-confronto-ed      pic zzz.zzz.zz9,9999.
       77  tot-righe             pic 9(10) value 0.

       01  r-inizio.
         05 filler              pic x(2)  value " [".
         05 r-data.
            10 r-gg             pic xx.
            10 filler           pic x     value "/".
            10 r-mm             pic xx.
            10 filler           pic x     value "/".
            10 r-aa             pic xx.
         05 filler              pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh             pic xx.
            10 filler           pic x     value X"22".
            10 r-min            pic xx.
            10 filler           pic x     value "'".
            10 r-sec            pic xx.
         05 filler              pic x(2)  value "] ".

       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.       
       77  tentativi             pic 99.

       77  resto                 pic 9(3).
       77  diff-giorni           pic 9(5).

       77  como-ora              pic 9(8).
       77  como-data             pic 9(8).

       |Mi salvo subito i gruppi gdo con quel flag per non doverlo rivfare ogni volta
       01  tab-gdo               occurs 999.
           05 el-gdo             pic x(5).

       77  como-riga             pic x(300).

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
       SETTA-INIZIO-RIGA.
           accept como-ora  from time.
           accept como-data from century-date.

           move como-data(3:2) to r-aa. 
           move como-data(5:2) to r-mm.
           move como-data(7:2) to r-gg.

           move como-ora(1:2) to r-hh.
           move como-ora(3:2) to r-min.
           move como-ora(5:2) to r-sec.

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
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                         delimited size
                  "RICERCA LISTINI PER ARTICOLO: " delimited size
                  art-codice                       delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           move low-value  to rlis-chiave-ricerca.
           move art-codice to rlis-articolo.  
           move 999999,9999 to s-prz-confronto.
           move 0 to s-codice.

           start rlistini key >= rlis-k-art 
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rlistini next at end exit perform end-read
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

                       move rlis-codice       to tlis-codice
                       read tlistini no lock invalid continue end-read 
                       move tlis-fornitore    to desf-codice
                       move rlis-destino      to desf-prog
                       read destinif no lock invalid continue end-read

                       move tlis-trasp       to como-trasporto
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
                       end-if
                    end-if
                 end-perform
           end-start.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           if trovato                                  
              move s-prz-confronto to prz-confronto-ed
              string r-inizio            delimited size
                     "TROVATO LISTINO: " delimited size
                     s-codice            delimited size
                     " CON PREZZO: "     delimited size
                     prz-confronto-ed    delimited size
                     into como-riga
              end-string
           else
              string r-inizio                         delimited size
                     "NESSUN LISTINO TROVATO "        delimited size
                     art-codice                       delimited size
                     into como-riga
              end-string
           end-if.
           perform RIGA-LOG.

      ***---
       CALCOLA-TRASPORTO.
           move 0 to costo-trasporto.
           move spaces to tge-chiave.
           read tparamge no lock.
           if desf-nazione = "ITA"
              compute costo-trasporto = 
                      prg-peso * tge-trasp-italy
           else
              compute costo-trasporto = 
                      prg-peso * tge-trasp-estero
           end-if.     

      ***---
       CREA-LISTINI.   
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                           delimited size
                  "CREAZIONE LISTINI PER ARTICOLO: " delimited size
                  art-codice                         delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           move 0 to idx.
           perform varying idx from 1 by 1 
                     until idx > 999
              if el-gdo(idx) = spaces
                 exit perform
              end-if         
              move low-value to lst-rec
              move el-gdo(idx) to lst-gdo
              move art-codice  to lst-articolo
              set trovato to false
              start listini key >= lst-k-articolo   
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read listini next at end exit perform end-read
                       if lst-gdo      not = el-gdo(idx) or
                          lst-articolo not = art-codice
                          exit perform
                       end-if
                       if lst-data <= como-data
                          set trovato to true
                          exit perform
                       end-if
                    end-perform
              end-start 
              perform SETTA-INIZIO-RIGA
              initialize como-riga                             
              if trovato
                 string r-inizio                          delimited size
                        "TROVATO ARTICOLO A LISTINO PER: "delimited size
                        el-gdo(idx)                       delimited size
                        into como-riga
                 end-string
              else         
                 initialize lst-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 add 0,005 to s-prz-confronto giving lst-prezzo
                 if tge-divisore-lstgdo not = 0
                    compute lst-prezzo = lst-prezzo / 
                                         tge-divisore-lstgdo
                 end-if
                 move lst-prezzo to prz-confronto-ed
                 string r-inizio                          delimited size
                        "**CREATO ARTICOLO A LISTINO PER:"delimited size 
                        el-gdo(idx)                       delimited size
                        " CON PREZZO: "                   delimited size
                        prz-confronto-ed                  delimited size
                        into como-riga
                 end-string
                 move el-gdo(idx)     to lst-gdo
                 move como-data       to lst-data
                 move art-codice      to lst-articolo
                 accept lst-data-creazione from century-date
                 accept lst-ora-creazione  from time
                 move "AUTO"          to lst-utente-creazione
                 add 1 to tot-righe
                 write lst-rec
              end-if    
              perform RIGA-LOG
           end-perform.

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
       RECUPERO-ANAGRAFICA.

      ***---
       PARAGRAFO-COPY.
           copy "prz-finito-forn.cpy".
           copy "costo-medio.cpy".
           copy "imposte-fornitore.cpy".
           copy "addizionale-piombo-fornitore.cpy".
           copy "trova-parametro.cpy".
