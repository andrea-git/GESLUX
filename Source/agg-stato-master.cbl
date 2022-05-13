       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      agg-stato-master.
       AUTHOR.                          Andrea.
       SPECIAL-NAMES. decimal-point is comma.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "rordini.sl".
           copy "tordini.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tagli.sl".
           copy "lineseq.sl".
           copy "tparamge.sl".
           copy "tscorte.sl".
           copy "articoli.sl".
           copy "tcaumag.sl".
           copy "ttipocli.sl".
      *****     copy "evaclides.sl".
           copy "progmag.sl". 
           copy "tmarche.sl".
           copy "timposte.sl".
           copy "param.sl".    

       SELECT logfile
           ASSIGN       TO  path-logfile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "rordini.fd".
           copy "tordini.fd".
           copy "tpromo.fd".
           copy "rpromo.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "tagli.fd".
           copy "lineseq.fd".
           copy "tparamge.fd".
           copy "tscorte.fd".
           copy "articoli.fd".
           copy "tcaumag.fd".
           copy "ttipocli.fd".
      *****     copy "evaclides.fd".
           copy "progmag.fd".
           copy "tmarche.fd".
           copy "timposte.fd". 
           copy "param.fd".    

       FD  logfile.
       01 log-riga        PIC  x(900). 

       WORKING-STORAGE SECTION.
           copy "aggiorna-stato-master.def".
           copy "link-geslock.def".
           copy "link-wprogmag.def".
           copy "versione-evasione.def".
           copy "trova-parametro.def".

       77  status-tsetinvio pic xx.
       77  status-mtordini  pic xx.
       77  status-mrordini  pic xx.
       77  status-rordini   pic xx.
       77  status-tordini   pic xx.
       77  status-tpromo    pic xx.
       77  status-rpromo    pic xx.
       77  status-clienti   pic xx.
       77  status-destini   pic xx.
       77  status-tagli     pic xx.
       77  status-tparamge  pic xx.
       77  status-tscorte   pic xx.
       77  status-articoli  pic xx.
       77  status-tcaumag   pic xx.
       77  status-ttipocli  pic xx.
      ***** 77  status-evaclides pic xx.
       77  status-progmag   pic xx.
       77  status-tmarche   pic xx.
       77  status-timposte  pic xx.
       77  status-param     pic xx.
       77  status-lineseq   pic xx.
       77  status-logfile   pic xx.
       77  wstampa          pic x(256).
       77  path-logfile     pic x(200).
       77  path-log         pic x(200).
                                      
       77  como-data        pic 9(8).
       77  como-anno        pic 9(4).
       77  como-ora         pic 9(8).
       77  nargs            pic 99 comp-1.
                  
      * DUMMY: NON SERVE, NON SARA' MAI VALORIZZATO. 
       77  user-codi       pic x(15) value spaces.  
       77  counter               pic 9(9) value 0.
       77  counter2              pic 9(9) value 0.
       77  counter-edit          pic zzz.zzz.zz9. 
       77  como-riga        pic x(200).     

       01  r-inizio         pic x(25).

       01  errori          pic xx.
           88 tutto-ok     value "OK".
           88 errori       value "ER".

       01  filler          pic 9.
           88 record-ok    value 1, false 0.

       01  filler          pic 9.
           88 RecLocked    value 1, false 0.

       01  filler          pic 9.
           88 RichiamoSchedulato    value 1, false 0.

       01  GdoInUsoFlag          pic x.
           88 GdoInUso           value "S". 
           88 GdoNonInUso        value " ".

       LINKAGE SECTION.
       copy "link-batch.def".

       PROCEDURE DIVISION USING batch-linkage.
       DECLARATIVES.
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false
           if status-mtordini = "93"
              set RecLocked to true
           end-if.
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false
           if status-mrordini = "93"
              set RecLocked to true
           end-if.            
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false
           if status-tordini = "93"
              set RecLocked to true
           end-if.
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false
           if status-rordini = "93"
              set RecLocked to true
           end-if.    
       TPROMO-ERR SECTION.
           use after error procedure on tpromo.
           set RecLocked to false
           if status-tpromo = "93"
              set RecLocked to true
           end-if.    
       RPROMO-ERR SECTION.
           use after error procedure on rpromo.
           set RecLocked to false
           if status-rpromo = "93"
              set RecLocked to true
           end-if.

       END DECLARATIVES.

      ***---
       MAIN.
           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              accept  path-log from environment "SCHEDULER_PATH_LOG"
              set RichiamoSchedulato to true 
              move  0 to batch-status
           else
              accept  path-log from environment "PATH_ST"
              set RichiamoSchedulato to false
           end-if.
           perform OPEN-LOGFILE   
           move "INIZIO PROGRAMMA" to como-riga
           perform SCRIVI-RIGA-LOG
           move "APERTURA FILES" to como-riga
           perform SCRIVI-RIGA-LOG

           accept versione-evasione from environment "VERSIONE_EVASIONE"
           open input clienti destini tparamge tscorte 
                      articoli tcaumag ttipocli  progmag tmarche |evaclides
                      timposte param.

           set RecLocked to false.     
           move "APERTURA MTORDINI" to como-riga.
           perform SCRIVI-RIGA-LOG.
           perform until 1 = 2
              open i-o mtordini |allowing readers
              if not RecLocked 
                 exit perform
              end-if
           end-perform.              
           set RecLocked to false.   
           move "APERTURA MRORDINI" to como-riga.
           perform SCRIVI-RIGA-LOG.  
           perform until 1 = 2
              open i-o mrordini |allowing readers
              if not RecLocked 
                 exit perform
              end-if
           end-perform.              
           set RecLocked to false.   
           move "APERTURA TPROMO" to como-riga.
           perform SCRIVI-RIGA-LOG.
           perform until 1 = 2
              open i-o tpromo |allowing readers
              if not RecLocked 
                 exit perform
              end-if
           end-perform.              
           set RecLocked to false.   
           move "APERTURA RPROMO" to como-riga.
           perform SCRIVI-RIGA-LOG.
           perform until 1 = 2
              open i-o rpromo |allowing readers
              if not RecLocked 
                 exit perform
              end-if
           end-perform.              
           set RecLocked to false.    
           move "APERTURA TORDINI" to como-riga.
           perform SCRIVI-RIGA-LOG.
           perform until 1 = 2
              open i-o tordini |allowing readers
              if not RecLocked 
                 exit perform
              end-if
           end-perform.              
           set RecLocked to false.   
           move "APERTURA RORDINI" to como-riga.
           perform SCRIVI-RIGA-LOG.
           perform until 1 = 2
              open i-o rordini |allowing readers
              if not RecLocked 
                 exit perform
              end-if
           end-perform.     
                                        
           move "APERTURA FILES RIUSCITA" to como-riga.
           perform SCRIVI-RIGA-LOG.
           move "INIZIO ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.
                                    
           move low-value to mto-rec.
           move como-data(1:4) to como-anno.
           subtract 3 from como-anno.
           move como-anno to mto-anno.
           start mtordini key >= mto-chiave.
           perform until 1 = 2
              read mtordini next at end exit perform end-read  
              if RichiamoSchedulato
                 add 1 to counter counter2
                 if counter2 = 300
                    move counter to counter-edit
                    display counter-edit 
                            upon batch-win-handle
                            line 25,00
                          column 38,00
                    move 0 to counter2
                 end-if
              end-if         
              initialize como-riga
              string "ELABORAZIONE MASTER: " 
                     mto-anno
                     " - "
                     mto-numero
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              perform AGGIORNA-STATO-MASTER
              if no-cli
                 string "MASTER: " 
                        mto-anno
                        " - "
                        mto-numero
                        " CLIENTE NON TROVATO"
                   into como-riga
                 end-string                  
                 move 1 to batch-status
                 perform SCRIVI-RIGA-LOG
              end-if
                 
           end-perform.
                 
           unlock mtordini all records.
           unlock tordini  all records.
           unlock rordini  all records.
           unlock mrordini all records.
           unlock tpromo   all records.
           unlock rpromo   all records.

           close mtordini tordini rordini mrordini articoli tcaumag
                 tpromo rpromo clienti destini tparamge tscorte ttipocli
                 progmag tmarche timposte param |evaclides .  
                                     
           move "CHIUSURA FILES" to como-riga.
           perform SCRIVI-RIGA-LOG.
           move "FINE PROGRAMMA" to como-riga.
           perform SCRIVI-RIGA-LOG.
           close logfile.

           if RichiamoSchedulato    
              move  0 to batch-status
              display "                                             "
                 upon batch-win-handle
                   line 25,00
                 column 35,00
           end-if.

           goback.      

      ***---
       SCRIVI-RIGA-LOG.
           if path-logfile = spaces exit paragraph end-if.
           inspect como-riga replacing trailing spaces by low-value.
           perform SETTA-INIZIO-RIGA.
           initialize log-riga.
           string r-inizio  delimited size
                  como-riga delimited low-value
             into log-riga
           end-string.
           write log-riga.       

      ***---
       OPEN-LOGFILE.
           accept como-ora  from time.
           accept como-data from century-date.
           inspect path-log replacing trailing spaces by low-value.
           initialize path-logfile.
           string path-log         delimited low-value
                  "LOG_AGG-STATO-MASTER_"   delimited size
                  como-data        delimited size
                  "_"              delimited size
                  como-ora         delimited size
                  ".log"           delimited size
                  into path-logfile
           end-string.
           open output logfile.

      ***---
       PARAGRAFO-COPY.
           copy "aggiorna-stato-master.cpy".
           copy "direziona-impegnato-common.cpy".
           copy "trova-parametro.cpy".
           copy "setta-inizio-riga.cpy".

      ***---
       AGGIORNA-IMPEGNATO-MASTER.
