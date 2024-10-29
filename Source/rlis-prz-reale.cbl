       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rlis-prz-reale.
       AUTHOR.                          Andrea.
       REMARKS. Per estrazioni Walter:
                - calcola il prz reale e lo salva su file
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.        
           copy "tlistini.sl". 
           copy "rlistini.sl".
           copy "articoli.sl".
           copy "timposte.sl".
           copy "progmag.sl".
           copy "impforn.sl".   
           copy "tscorte.sl".   
           copy "destinif.sl".  
           copy "distinteb.sl".
           copy "tpiombo.sl".
           copy "param.sl".
           copy "clienti.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tlistini.fd".
           copy "rlistini.fd".
           copy "articoli.fd". 
           copy "timposte.fd".
           copy "progmag.fd".
           copy "impforn.fd".    
           copy "tscorte.fd".   
           copy "destinif.fd".
           copy "distinteb.fd". 
           copy "tpiombo.fd".
           copy "param.fd".  
           copy "clienti.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Salvataggio prz reale".

      * FILE STATUS           
       77  status-tlistini       pic xx.   
       77  status-rlistini       pic xx.
       77  status-articoli       pic xx.  
       77  status-timposte       pic xx.
       77  status-progmag        pic xx. 
       77  status-impforn        pic xx.     
       77  status-tscorte        pic xx.
       77  status-destinif       pic xx.  
       77  status-distinteb      pic xx.
       77  status-tpiombo        pic xx.   
       77  status-param          pic xx.
       77  status-clienti        pic xx.
       77  status-lineseq        pic xx.
                                           
       77  idx                   pic 9(5).   
       77  como-data             pic 9(8).   
       77  como-ora              pic 9(8).
       77  como-riga             pic x(80).
       77  r-inizio              pic x(25).   
       77  nargs                 pic 99  comp-1 value 0.      
       77  filler                pic 9 value 0.
           88 RecLocked                value 1, false 0.   

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.
       77 calcolo-piombo   PIC  x.
           88 nuovo-calcolo-piombo VALUE IS "N". 
       
      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
                                   
       copy "prz-finito-forn.def".    
       copy "costo-medio.def".
       copy "imposte-fornitore.def".           
       copy "trova-parametro.def".
                               
      ******************************************************************
       LINKAGE SECTION.   
       copy "link-batch.def".
      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage. 
       DECLARATIVES.               

      ***---
       RLISTINI-ERR SECTION.
           use after error procedure on rlistini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rlistini
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       FILE-ERR SECTION.
           use after error procedure on tlistini articoli  timposte
                                        progmag  impforn   tscorte
                                        destinif distinteb tpiombo
                                        param    clienti   lineseq.
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
           initialize wstampa
           if RichiamoSchedulato        
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
           else                                                    
              accept  wstampa from environment "PATH_ST"
           end-if.
                                
           accept como-data from century-date
           accept como-ora  from time
           inspect wstampa replacing trailing spaces by low-value
           string  wstampa           delimited low-value
                   "RLIS-PRZ-REALE_" delimited size
                   como-data         delimited size
                   "_"               delimited size
                   como-ora          delimited size
                   ".log"            delimited size
                   into wstampa
           end-string
           if RichiamoSchedulato
              move wstampa to batch-log
           end-if.

      ***---
       OPEN-FILES.  
           open output lineseq.
           open i-o   rlistini.
           open input tlistini articoli timposte  progmag impforn 
                      tscorte  destinif distinteb tpiombo param
                      clienti.

      ***---
       ELABORAZIONE.                             
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio            delimited size
                  "INIZIO PROGRAMMA"  delimited size
             into como-riga
           end-string.
           perform RIGA-LOG.
    
           move low-value to tlis-rec.
           start tlistini key >= tlis-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read tlistini next at end exit perform end-read
              move low-value   to rlis-rec
              move tlis-chiave to rlis-chiave
              start rlistini key >= rlis-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2       
                       read rlistini next no lock
                         at end exit perform 
                       end-read
                       if rlis-codice not = tlis-codice
                          exit perform
                       end-if

                       move 0 to prg-peso-utf prg-peso-non-utf          
                       move tlis-trasp-f to como-trasporto-f
                       move tlis-trasp-c to como-trasporto-c
                       perform CALCOLA-PRZ-FINITO
                       move prz-reale to rlis-prz-reale
                       set RecLocked to false
                       rewrite rlis-rec
                       if RecLocked    
                          initialize como-riga
                          perform SETTA-INIZIO-RIGA
                          string r-inizio              delimited size
                                 "RLIS LOCK. CODICE: " delimited size
                                 rlis-codice           delimited size
                                 " ARTICOLO: "         delimited size
                                 rlis-articolo         delimited size
                            into como-riga
                          end-string
                          perform RIGA-LOG
                          set errori to true
                          if RichiamoSchedulato
                             move 1 to batch-status
                          end-if
                       end-if
                    end-perform
              end-start
           end-perform. 

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio          delimited size
                  "FINE PROGRAMMA"  delimited size
             into como-riga
           end-string.
           perform RIGA-LOG.    

      ***---
       CALCOLA-TRASPORTO.
           move 0 to costo-trasporto.

           move art-scorta to sco-codice.
           read tscorte no lock invalid continue end-read.
           
           move tlis-fornitore to desf-codice.
           move tlis-destino   to desf-prog.
           read destinif no lock invalid continue end-read.
           move rlis-articolo to art-codice.
           read articoli no lock.
           if como-trasporto-f = 1
              compute costo-trasporto = 
                    ( art-peso-utf + art-peso-non-utf ) * sco-trasp-f
           end-if.
           if como-trasporto-c = 1
              compute costo-trasporto = costo-trasporto +
                    (( art-peso-utf + art-peso-non-utf ) * sco-trasp-c)
           end-if. 

      ***---
       CLOSE-FILES.
           close tlistini rlistini articoli  timposte progmag impforn
                 tscorte  destinif distinteb param    clienti lineseq.  

      ***---
       RIGA-LOG.
           initialize line-riga of lineseq.
           write line-riga of lineseq from como-riga.

      ***---
       EXIT-PGM. 
           goback.        

      ***--- DUMMY PER COSTO MP
       RECUPERO-ANAGRAFICA.

      ***---
       PARAGRAFO-COPY.                  
       copy "prz-finito-forn.cpy".     
       copy "costo-medio.cpy".   
       copy "imposte-fornitore.cpy".
       copy "addizionale-piombo-fornitore.cpy".
       copy "trova-parametro.cpy".
       copy "setta-inizio-riga.cpy".
