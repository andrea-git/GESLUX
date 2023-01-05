       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EDI-impord-p.
       AUTHOR.                          Andrea.
       REMARKS.
           1. Importazione ordini EDI presenti in una cartella predefinita
           2. Controlli e scrittura file EDI con i dati di origine
              salvati all'interno del record
           3. Copia del file in una cartella di backup 

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
       SELECT lineseq
           ASSIGN       TO  wstampa
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq.

       SELECT lineseq-bckp
           ASSIGN       TO  wstampa-bckp
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq-bckp.

       SELECT lineseq2
           ASSIGN       TO  wstampa2
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-lineseq2.

       SELECT logfile
           ASSIGN       TO  path-logfile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.

       copy "EDI-clides.sl".
       copy "clienti.sl".
       copy "destini.sl".
       copy "tparamge.sl".
       copy "EDI-mtordini.sl".
       copy "EDI-mrordini.sl".
       copy "note.sl".
       copy "param.sl".
       copy "articoli.sl".
       copy "progmag.sl".
       copy "timballi.sl".
       copy "timbalqta.sl".
       copy "ttipocli.sl".
       copy "tpromo.sl".
       copy "rpromo.sl".
       copy "listini.sl".
       copy "rmovmag.sl".
       copy "locali.sl".
       copy "blister.sl".
       copy "cli-prg.sl".
       copy "tprov.sl".
       copy "anacap.sl".
       copy "lockfile.sl".   
       copy "log-macrobatch.sl".
       copy "tregioni.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                        
       FD  lineseq.
       01 line-riga       PIC  x(20000).

       FD  lineseq-bckp.
       01 line-riga-bckp  PIC  x(20000).

       FD  lineseq2.
       01 line-riga2      PIC  x(2000).

       FD  logfile.
       01 log-riga        PIC  x(900).    

       copy "EDI-clides.fd".
       copy "clienti.fd".
       copy "destini.fd".
       copy "tparamge.fd".
       copy "EDI-mtordini.fd".
       copy "EDI-mrordini.fd".
       copy "note.fd".
       copy "param.fd".  
       copy "articoli.fd".
       copy "progmag.fd".
       copy "timballi.fd".
       copy "timbalqta.fd". 
       copy "ttipocli.fd".
       copy "tpromo.fd".
       copy "rpromo.fd".
       copy "listini.fd".
       copy "rmovmag.fd".
       copy "locali.fd".
       copy "blister.fd".
       copy "cli-prg.fd". 
       copy "tprov.fd".
       copy "lockfile.fd".      
       copy "log-macrobatch.fd".
       copy "tregioni.fd".        
       copy "anacap.fd".

       WORKING-STORAGE SECTION.
      *    COPY
           copy "link-geslock.def".
           copy "acucobol.def".
           copy "trova-parametro.def".
           copy "link-chk-ord-cli.def".
           copy "tratta-numerico.def".
           copy "recupero-prezzi-tradizionale.def".
           copy "log-macrobatch.def".

      *    COSTANTI
       78  titolo                value "Importazione ordini EDI". 
       78  78-comma              value ".".
       78  78-EDI-impord-p       value "EDI-impord-p".
                                          
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

      *    FILE-STATUS
       77  status-lineseq        pic xx.
       77  status-lineseq-bckp   pic xx.
       77  status-lineseq2       pic xx.
       77  status-logfile        pic xx.
       77  status-edi-clides     pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tparamge       pic xx.
       77  status-EDi-mtordini   pic xx.
       77  status-EDi-mrordini   pic xx.
       77  status-note           pic xx.
       77  status-param          pic xx.
       77  status-articoli       pic xx.
       77  status-progmag        pic xx.
       77  status-timballi       pic xx.
       77  status-timbalqta      pic xx. 
       77  status-ttipocli       pic xx.
       77  status-tpromo         pic xx.
       77  status-rpromo         pic xx.
       77  status-listini        pic xx.
       77  status-rmovmag        pic xx.
       77  status-locali         pic xx.
       77  status-blister        pic xx.
       77  status-cli-prg        pic xx. 
       77  status-tprov          pic xx.
       77  status-lockfile       pic xx.
       77  status-log-macrobatch pic xx.
       77  status-tregioni       pic xx.
       77  status-anacap         pic xx.
                                            
       77  path-logfile          pic x(256).
       77  path-log-macrobatch   pic x(256).
       77  wstampa               pic x(256).
       77  wstampa-bckp          pic x(256).
       77  wstampa2              pic x(256).

       77  save-ecd-chiave          pic x(10).
       77  save-ecd-import-articoli pic 9.
         88 save-ecd-import-articoli-si value 1.
         88 save-ecd-import-articoli-no value 0.
         
       77  save-ecd-import-importi  pic 9.
         88 save-ecd-import-importi-si value 1.
         88 save-ecd-import-importi-no value 0.
          
       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.  
       77  num-riga              pic 9(6).
       77  oggi-piu-7            pic 9(8).
       77  oggi-meno-1           pic 9(8).
       77  ultimo-numero         pic 9(8) value 0.
       77  primo-numero          pic 9(8) value 0.
       77  riga-dettaglio        pic 9(5).
       77  como-div              pic 9(5).
       77  como-qta              pic 9(8).
       77  resto                 pic 9(5).
       77  qta-ord-EDI           pic 9(8).
       77  qta-imb-EDI           pic 9(7).
       77  riga                  pic 9(5).
       77  como-prov             pic xx.
                                         
       77  tot-file-ko           pic 999 value 0.
       77  tot-file-ok           pic 999 value 0.
       77  tot-file              pic 999 value 0.

       77  tot-ordini            pic 9(5) value 0.
       77  tot-ordini-err        pic 9(5) value 0.

       77  counter               pic 9(9) value 0.
       77  counter2              pic 9(9) value 0.
       77  counter-edit          pic zzz.zzz.zz9.

       77  tprz-codice           pic 9(15).
       77  tprz-prz-acq          pic 9(9)v99.   
       77  Sum                   pic s9(10)v999.
       77  como-prezzo2          pic s9(10)v999.   
       77  como-prezzo           pic s9(10)v999.
       77  TotPrzBlister         pic s9(10)v999.
       77  idx                   pic 9(3).
       77  LastIdx               pic 9(3).
       77  sav-vettore           pic 9(3).
       77  sav-regione           pic 9(3).

       77  path-import           pic x(256).    
       77  path-backup           pic x(256).
       77  path-log              pic x(256).
       77  nome-file             pic x(256).
       77  file-backup           pic x(256).
       77  file-import           pic x(256).
       77  cmd                   pic x(600).
       77  como-riga             pic x(200).

       77  dir-import-handle     handle.
       77  dir-backup-handle     handle.
       77  dir-log-handle        handle.    

       01  record-GENERICO.
         05 01G-filler              pic x.
         05 02G-filler              pic x.
         05 03G-filler              pic x.
         05 04G-filler              pic x.
         05 05G-filler              pic x.
         05 06G-tipo-record         pic x(3).

       01 como-prg-chiave.
          05 como-prg-cod-articolo   pic 9(6).
          05 como-prg-cod-magazzino  pic x(3).
          05 como-prg-tipo-imballo   pic x(3).
          05 como-prg-peso           pic 9(5)v9(3).

       01 giacenza                 pic s9(8).
       01 giacenzakey.
           05 gia-cod-articolo     PIC 9(6).
           05 gia-cod-magazzino    PIC x(3).
           05 gia-tipo-imballo     PIC x(3).
           05 gia-peso             PIC 9(5)V9(3).

       01 controlli              pic xx.
           88 tutto-ok           value "OK".
           88 errori             value "ER".      

       01 filler                 pic 9 value 0.
           88 prezzo-sp                value 1, false 0.
                     
       01 filler                 pic 9 value 0.
           88 trovato                  value 1, false 0. 
       01 filler                 pic 9 value 0.
           88 trovato-progressivo      value 1, false 0.
       01 filler                 pic 9 value 0.
           88 blocco-prezzo            value 1, false 0.

       01 filler                 pic 9 value 0.
           88 trovato-promo            value 1, false 0.
       01 filler                 pic 9 value 0.
           88 promo-future             value 1, false 0.
                                
       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.

       01  filler                pic 9 value 0.
           88 RichiamoBatch      value 1, false 0.

       01  filler                pic 9 value 0.
           88 trovato-movim            value 1, false 0. 

       01  filler                pic 9 value 0.
           88 OrdineTradizionale       value 1, false 0. 

       01  filler                pic 9 value 0.
           88 record-ok                value 1, false 0.

       01  filler                pic 9 value 0.
           88 RecuperaArticolo   value 1, false 0.  

       01  filler                pic 9 value 0.
           88 VerificaPrezziTradizionale value 1, false 0.

       01  filler                pic 9 value 0.
           88 RecLocked          value 1, false 0.
    
       01  filler                pic x.
           88 si-prg-listino     value "S". 
           88 no-prg-listino     value "N". 

       01  filler                pic 9.
           88 prima-volta        value 1, false 0.   
       01  r-inizio              pic x(25).
                                                        
       77  nargs                 pic 99  comp-1 value 0.
       77  como-giacenza         pic s9(9).
       77  ricerca               pic 9.
       77  02D-fillerx           pic x.
       77  user-codi             pic x(20).
       77  CallingPgm            pic x(10).

       LINKAGE SECTION.
       copy "link-batch.def".
       77  link-user      pic x(10).
       77  lk-mb-logfile  pic x(256). |LOG DI MACROBATCH

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage link-user lk-mb-logfile.

       DECLARATIVES.   
      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           evaluate status-lineseq
           when "00"
           when other continue
           end-evaluate. 

      ***---
       LINESEQ-BCKP-ERR SECTION.
           use after error procedure on lineseq-bckp.
           evaluate status-lineseq-bckp
           when "00"
           when other continue
           end-evaluate. 

      ***---
       LOG-MACROBATCH-ERR SECTION.
           use after error procedure on log-macrobatch.
           evaluate status-log-macrobatch
           when "00"
           when other continue
           end-evaluate.  

      ***---
       LOGFILE-ERR SECTION.
           use after error procedure on logfile.
           evaluate status-logfile
           when "00"
           when other continue
           end-evaluate. 

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           evaluate status-clienti
           when "00"
           when other continue
           end-evaluate. 

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           evaluate status-destini
           when "00"
           when other continue
           end-evaluate.           
           
      ***---
       EDI-MTORDINI-ERR SECTION.
           use after error procedure on EDI-mtordini.
           evaluate status-EDI-mtordini
           when "00"
           when other continue
           end-evaluate.     

      ***---
       EDI-MRORDINI-ERR SECTION.
           use after error procedure on EDI-mrordini.
           evaluate status-EDI-mrordini
           when "00"
           when other continue
           end-evaluate.     

      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           evaluate status-tparamge
           when "00"
           when other continue
           end-evaluate.

      ***---
       PARAM-ERR SECTION.
           use after error procedure on param.
           evaluate status-param
           when "00"
           when other continue
           end-evaluate.      

      ***---
       EDI-CLIDES-ERR SECTION.
           use after error procedure on EDI-clides.
           evaluate status-EDI-clides
           when "00"
           when other continue
           end-evaluate.   

      ***---
       NOTE-ERR SECTION.
           use after error procedure on note.
           evaluate status-note
           when "00"
           when other continue
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           evaluate status-articoli
           when "00"
           when other continue
           end-evaluate.           
                        
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           evaluate status-progmag
           when "00"
           when other continue
           end-evaluate.

      ***---
       TIMBALLI-ERR SECTION.
           use after error procedure on timballi.
           evaluate status-timballi
           when "00"
           when other continue
           end-evaluate.
                                   
      ***---
       TIMBALQTA-ERR SECTION.
           use after error procedure on timbalqta.
           evaluate status-timbalqta
           when "00"
           when other continue
           end-evaluate.
                                   
      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           evaluate status-ttipocli
           when "00"
           when other continue
           end-evaluate.
                                   
      ***---
       TPROMO-ERR SECTION.
           use after error procedure on tpromo.
           evaluate status-tpromo
           when "00"
           when other continue
           end-evaluate.
                                   
      ***---
       RPROMO-ERR SECTION.
           use after error procedure on rpromo.
           evaluate status-rpromo
           when "00"
           when other continue
           end-evaluate. 
                                   
      ***---
       LISTINI-ERR SECTION.
           use after error procedure on listini.
           evaluate status-listini
           when "00"
           when other continue
           end-evaluate.   
                                   
      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           evaluate status-rmovmag
           when "00"
           when other continue
           end-evaluate.
                                   
                                   
      ***---
       LOCALI-ERR SECTION.
           use after error procedure on locali.
           evaluate status-locali
           when "00"
           when other continue
           end-evaluate.  

      ***---
       BLISTER-ERR SECTION.
           use after error procedure on blister.
           evaluate status-blister
           when "00"
           when other continue
           end-evaluate.

      ***---
       CLI-PRG-ERR SECTION.
           use after error procedure on cli-prg.
           evaluate status-cli-prg
           when "00"
           when other continue
           end-evaluate.

      ***---
       LOCKFILE-ERR SECTION.
           use after error procedure on lockfile.
           evaluate status-lockfile
           when "99" set RecLocked to true
           when "00"
           when other continue
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.      
           perform INIT.
           if tutto-ok
              perform OPEN-FILES
              if tutto-ok
                 perform VERIFICA-ESEGUIBILITA
                 if tutto-ok
                    perform ELABORAZIONE
                    perform DELETE-LOCKFILE
                    perform CLOSE-FILES
                 end-if
              end-if
           end-if.
           perform EXIT-PGM.

      ***---
       VERIFICA-ESEGUIBILITA.
           set RecLocked to false.
           set tutto-ok  to true.
           perform DELETE-LOCKFILE.
           if RecLocked
              move 78-EDI-impord-p to lck-nome-pgm
              read lockfile  no lock invalid continue end-read
              
              initialize como-riga
              string "FUNZIONE GIA' IN USO DA: " delimited size
                     lck-utente-creazione        delimited low-value
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              set errori to true
              if CallingPgm = "edi-impord"
                 if RichiamoBatch
                    call   "set-ini-log" using r-output
                    cancel "set-ini-log"
                    initialize lm-riga
                    string r-output                    delimited size
                           "FUNZIONE GIA' IN USO DA: " delimited size
                           lck-utente-creazione        delimited size
                      into lm-riga
                    end-string
                    write lm-riga             
                 else
                    display message "Funzione già in uso da: "
                                    lck-utente-creazione
                              title titolo
                               icon 2
                 end-if
              end-if   
           else
              perform SCRIVI-LOCKFILE
           end-if.

      ***---
       SCRIVI-LOCKFILE.
           move 78-EDI-impord-p to lck-nome-pgm.
           accept lck-data-creazione from century-date.
           accept lck-ora-creazione  from time.
           move user-codi to lck-utente-creazione.
           write lck-rec invalid rewrite lck-rec end-write.
           read lockfile lock.

      ***---
       DELETE-LOCKFILE.
           move 78-EDI-impord-p to lck-nome-pgm.
           delete lockfile invalid continue end-delete.
           if status-lockfile = "49"
              open i-o lockfile
              move 78-EDI-impord-p to lck-nome-pgm
              delete lockfile invalid continue end-delete
           end-if.

      ***---
       INIT. 
           set VerificaPrezziTradizionale to false.
           initialize path-import path-backup path-log.
           call "C$NARG" using nargs.
           if nargs not = 0
              set RichiamoSchedulato to true
              move -1 to batch-status  
              accept path-log from environment "SCHEDULER_PATH_LOG"
              call "C$CALLEDBY" using CallingPgm
              if CallingPgm = "edi-impord"
                 move link-user to user-codi
                 if user-codi = "MACROBATCH"
                    set RichiamoBatch to true
                    move lk-mb-logfile to path-log-macrobatch
                    open extend log-macrobatch
                 end-if
              else
                 move "IMPORT EDI" to user-codi
              end-if
           else
              set RichiamoSchedulato to false
              accept path-log from environment "PATH_ST"
           end-if.
           set tutto-ok     to true.
           set prima-volta  to true.
           accept como-data from century-date.
           accept como-ora  from time.

           perform VERIFICA-CARTELLE.

           if errori
              close logfile
           end-if.

            
      ***---
       VERIFICA-CARTELLE.
           if path-log = spaces exit paragraph end-if. 
           inspect path-log replacing trailing spaces by low-value.   
      *    cartella di log
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         path-log,
                                         "*.*"
         
           move RETURN-CODE        to Dir-log-Handle
           if dir-log-handle = 0   
              initialize como-riga
              string "ELABORAZIONE IMPOSSIBILE. "
                     "PERCORSO LOG NON VALIDO: "
                     path-log delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              set errori to true
              exit paragraph
           end-if.

           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, 
                                         Dir-log-Handle.

           initialize path-logfile.
           string path-log          delimited low-value
                  "LOG_EDI_IMPORD_" delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".log"            delimited size
                  into path-logfile
           end-string.
           open output logfile.
                                 
           accept  path-import from environment "EDI_IMPORD_PATH".
           accept  path-backup from environment "EDI_IMPORD_PATH_BACKUP"
           if path-import = spaces
              initialize como-riga
              string "ELABORAZIONE IMPOSSIBILE. "
                     "VALORIZZARE EDI_IMPORD_PATH"
                     delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              set errori to true
              exit paragraph
           end-if.
           if path-backup = spaces
              initialize como-riga
              string "ELABORAZIONE IMPOSSIBILE. "
                     "VALORIZZARE EDI_IMPORD_PATH_BACKUP"
                     delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              set errori to true
              exit paragraph
           end-if.
              
      *    CONTROLLO L'ESISTENZA DELLA CARTELLA
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         path-import,
                                         "*.*"

           move RETURN-CODE        to Dir-import-Handle

           if Dir-import-Handle = 0
              initialize como-riga
              string "ELABORAZIONE IMPOSSIBILE. "
                     "PERCORSO IMPORT NON VALIDO: "
                     path-import delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              set errori to true
              exit paragraph
           end-if
           
      *    cartella di backup
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         path-backup,
                                         "*.*"
         
           move RETURN-CODE        to Dir-backup-Handle
           if dir-backup-handle = 0
              initialize como-riga
              string "ELABORAZIONE IMPOSSIBILE. "
                     "PERCORSO BACKUP NON VALIDO: "
                     path-backup delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              set errori to true
              exit paragraph
           end-if

           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, 
                                         Dir-backup-Handle
           inspect path-import replacing trailing spaces by low-value.
           inspect path-backup replacing trailing spaces by low-value.

      ***---
       OPEN-FILES.                                
           accept como-ora  from time.  

           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.

           move "INIZIO ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.           

           move "CONTROLLO ESISTENZA CARTELLE OK" to como-riga.
           perform SCRIVI-RIGA-LOG.

           open i-o EDI-mtordini.
           if status-EDI-mtordini = "35"
              open output EDI-mtordini
              close       EDI-mtordini
              open i-o    EDI-mtordini
           end-if.
           if status-EDI-mtordini not = "00"
              initialize como-riga
              string "ERRORE: "                     delimited size
                     status-EDI-mtordini            delimited size
                     " IN ACCESSO A EDI-MTORDINI: " delimited size
                 into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              set errori to true
           else
              open i-o EDI-mrordini
              if status-EDI-mrordini = "35"
                 open output EDI-mrordini
                 close       EDI-mrordini
                 open i-o    EDI-mrordini
              end-if
              if status-EDI-mrordini not = "00"
                 initialize como-riga
                 string "ERRORE: "                     delimited size
                        status-EDI-mrordini            delimited size
                        " IN ACCESSO A EDI-MRORDINI: " delimited size
                    into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
                 set errori to true
                 close EDI-mtordini
              else
                 open i-o destini edi-clides lockfile
                 open input clienti tparamge note param articoli progmag 
                            timballi timbalqta ttipocli  tpromo rpromo 
                            listini rmovmag locali blister cli-prg tprov
                            tregioni anacap
              end-if
           end-if.

      ***---
       ELABORAZIONE.
           if RichiamoSchedulato 
              move 0 to batch-status
           end-if.
           move spaces to tge-chiave.
           read tparamge no lock.
           move tge-anno to emto-anno.
           move high-value to emto-numero.
           start EDI-mtordini key < emto-chiave
                 invalid 
                 move 0 to emto-numero
             not invalid 
                 read EDI-mtordini previous
                      at end move 0 to emto-numero
                 end-read
           end-start.
           move emto-numero to ultimo-numero primo-numero.

           perform until 1 = 2
              |Scorro la cartella in cerca di file
              |da copia scartando Backup e .Ds_Store
              call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
                                            dir-import-handle,
                                            nome-file

              if nome-file = spaces exit perform end-if

              if nome-file not = "."      and 
                           not = ".."     and 
                           not = "Backup" and not = ".DS_Store"

                 initialize wstampa
                 inspect nome-file  replacing trailing spaces
                                    by low-value
                 string path-import delimited low-value
                        nome-file   delimited low-value
                   into wstampa
                 end-string

                 initialize como-riga
                 perform SCRIVI-RIGA-LOG
                 string "ELABORAZIONE FILE "   delimited size
                        wstampa                delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG

                 add 1 to tot-file

                 |Verifico che il file sia accessibile
                 |OPEN L1
                 open input lineseq
                 if status-lineseq = "00"
                    |Lo ricreo in backup
                    initialize wstampa-bckp
                    string  path-backup delimited low-value
                            nome-file   delimited low-value
                            "_"         delimited size
                            como-data   delimited size
                            "_"         delimited size
                            como-ora    delimited size
                       into wstampa-bckp
                    end-string

                    inspect wstampa-bckp
                            replacing trailing spaces by low-value
                    initialize como-riga
                    string "CREAZIONE FILE BACKUP : " 
                                           delimited size
                           wstampa-bckp    delimited low-value
                      into como-riga
                    end-string     
                    perform SCRIVI-RIGA-LOG
                    inspect wstampa-bckp
                            replacing trailing low-value by spaces

                    |OPEN B1
                    open output lineseq-bckp
                    if status-lineseq-bckp = "00"
                       perform until 1 = 2
                          read lineseq next at end exit perform end-read
                          move line-riga to line-riga-bckp
                          write line-riga-bckp
                       end-perform
                       |CLOSE B1
                       close      lineseq-bckp
                       |Verifico che il nuovo file creato sia accessibile
                       |OPEN B2
                       open input lineseq-bckp       
                       if status-lineseq-bckp = "00"
                          
                          move "GENERAZIONE BACKUP RIUSCITA" 
                            to como-riga
                          perform SCRIVI-RIGA-LOG  

                          inspect wstampa
                                  replacing trailing spaces by low-value
                          initialize como-riga
                          string "CANCELLAZIONE FILE: " 
                                           delimited size
                                 wstampa   delimited low-value
                            into como-riga
                          end-string
                          perform SCRIVI-RIGA-LOG  
                          inspect wstampa
                                  replacing trailing low-value by spaces
                          |CLOSE L1
                          close       lineseq
                          delete file lineseq        
                          |OPEN L2
                          open input  lineseq
                          if status-lineseq = "35"
                             move "CANCELLAZIONE RIUSCITA" to como-riga
                             perform SCRIVI-RIGA-LOG

                             initialize como-riga
                             string "LAVORAZIONE EFFETTIVA FILE: "
                                    wstampa-bckp delimited size
                               into como-riga
                             end-string
                             perform SCRIVI-RIGA-LOG

                             |Lo elaboro
                             add 1 to tot-file-ok
                             perform ELABORA-FILE
                             |CLOSE B2
                             close lineseq-bckp
                          else              
                             |CLOSE L2
                             close lineseq
                             |CLOSE B2
                             close lineseq-bckp
                             add 1 to tot-file-ko
                             initialize como-riga
                             string "CANCELLAZIONE FALLITA. ERR: " 
                                    status-lineseq delimited size
                               into como-riga
                             end-string
                             perform SCRIVI-RIGA-LOG
                             if RichiamoSchedulato
                                move 1 to batch-status  
                             end-if
                          end-if
                       else
                          |CLOSE L1
                          close       lineseq

                          inspect wstampa-bckp
                                  replacing trailing spaces by low-value
                          initialize como-riga
                          string "ACCESSO A FILE BACKUP " delimited size
                                 " NON RIUSCITA. ERR: "   delimited size
                                 status-lineseq-bckp      delimited size
                            into como-riga
                          end-string     
                          add  1 to tot-file-ko
                          perform SCRIVI-RIGA-LOG
                          if RichiamoSchedulato
                             move 1 to batch-status
                          end-if
                       end-if
                    else
                       |CLOSE L1
                       close lineseq
                       initialize como-riga
                       string "GENERAZIONE BACKUP " delimited size
                              "NON RIUSCITA. ERR: " delimited size
                              status-lineseq-bckp   delimited size
                         into como-riga
                       end-string
                       perform SCRIVI-RIGA-LOG  
                       add  1 to tot-file-ko
                       if RichiamoSchedulato
                          move 1 to batch-status
                       end-if
                    end-if
                 else
                    initialize como-riga
                    string "ELABORAZIONE FILE"    delimited size
                           " NON RIUSCITA. ERR: " delimited size
                           status-lineseq         delimited size
                      into como-riga
                    end-string
                    perform SCRIVI-RIGA-LOG
                    add  1 to tot-file-ko
                    if RichiamoSchedulato
                       move 1 to batch-status
                    end-if
                 end-if
              end-if
           end-perform.   

      **********     perform until 1 = 2
      **********        |Scorro la cartella in cerca di file
      **********        |da copia scartando Backup e .Ds_Store
      **********        call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
      **********                                      dir-import-handle,
      **********                                      nome-file
      **********
      **********        if nome-file = spaces exit perform end-if
      **********
      **********        if nome-file not = "."      and 
      **********                     not = ".."     and 
      **********                     not = "Backup" and not = ".DS_Store"
      **********
      **********           initialize wstampa
      **********           inspect nome-file  replacing trailing spaces
      **********                              by low-value
      **********           string path-import delimited low-value
      **********                  nome-file   delimited low-value
      **********             into wstampa
      **********           end-string
      **********
      **********           initialize como-riga
      **********           perform SCRIVI-RIGA-LOG
      **********           string "ELABORAZIONE FILE "   delimited size
      **********                  wstampa                delimited size
      **********             into como-riga
      **********           end-string
      **********           perform SCRIVI-RIGA-LOG
      **********
      **********           add 1 to tot-file
      **********
      **********           |Verifico che il file sia accessibile
      **********           open input lineseq
      **********           if status-lineseq not = "00"
      **********              initialize como-riga
      **********              string "ELABORAZIONE FILE"    delimited size
      ***************                     nome-file              delimited low-value
      **********                     " NON RIUSCITA. ERR: " delimited size
      **********                     status-lineseq         delimited size
      **********                into como-riga
      **********              end-string
      **********              perform SCRIVI-RIGA-LOG
      **********              add  1 to tot-file-ko
      **********              if RichiamoSchedulato
      **********                 move 1 to batch-status
      **********              end-if
      **********           else                  
      **********              |Lo chiudo e lo sposto in backup
      **********              close lineseq       
      **********              initialize file-backup
      **********              string  path-backup delimited low-value
      **********                      nome-file   delimited low-value
      **********                      "_"         delimited size
      **********                      como-data   delimited size
      **********                      "_"         delimited size
      **********                      como-ora    delimited size
      **********                 into file-backup
      **********              end-string
      **********              inspect wstampa   replacing trailing spaces 
      **********                                by low-value
      **********              initialize cmd
      **********              string "move "     delimited size
      **********                     wstampa     delimited low-value
      **********                     " "         delimited size
      **********                     file-backup delimited size
      **********                into cmd
      **********              end-string      
      **********              move wstampa to file-import
      **********              move 0 to status-call
      **********              call "C$SYSTEM" using cmd, 225
      **********                             giving status-call
      **********              if status-call not = 0
      **********                 move "COMANDO BACKUP NON RIUSCITO" to como-riga
      **********                 perform SCRIVI-RIGA-LOG
      **********                 if RichiamoSchedulato
      **********                    move 1 to batch-status
      **********                 end-if
      **********              else                   
      **********                 initialize como-riga
      **********                 string "COMANDO BACKUP RIUSCITO: " delimited size
      **********                        file-backup            delimited low-value
      **********                   into como-riga
      **********                 end-string
      **********                 perform SCRIVI-RIGA-LOG
      **********
      **********                 |Attendo che il file sia presente realmente
      **********                 |provando ad aprirlo
      **********                 move file-backup to wstampa 
      **********                 perform 5 times
      **********                    call "C$SLEEP" using 2
      **********                    open input lineseq
      **********                    if status-lineseq = "00"
      **********                       |Lo elaboro
      **********                       add 1 to tot-file-ok
      **********                       perform ELABORA-FILE
      **********                       close lineseq
      **********                       exit perform
      **********                    end-if                      
      **********                 end-perform
      **********                 if status-lineseq not = "00" 
      **********                    |Se il file non è accessibile lo ripristino
      **********                    |alla sua posizione originaria
      **********                    initialize como-riga
      **********                    string "ELABORAZIONE BACKUP"  delimited size
      ***************                           wstampa      delimited low-value
      **********                           " NON RIUSCITA. ERR: " delimited size
      **********                           status-lineseq         delimited size
      **********                      into como-riga
      **********                    end-string
      **********                    perform SCRIVI-RIGA-LOG
      **********                    initialize cmd
      **********                    inspect file-backup 
      **********                            replacing trailing spaces by low-value
      **********                    string "copy "     delimited size
      **********                           file-backup delimited low-value
      **********                           " "         delimited size
      **********                           file-import delimited size
      **********                      into cmd
      **********                    end-string            
      **********                    initialize como-riga
      **********                    string "RIPRISTINO FILE IMPORT: " 
      **********                                                  delimited size
      **********                           cmd                    delimited size
      **********                      into como-riga
      **********                    end-string
      **********                    perform SCRIVI-RIGA-LOG 
      **********                    move 0 to status-call
      **********                    call "C$SYSTEM" using cmd, 225
      **********                                   giving status-call
      **********                    if status-call = 0
      **********                       move "COMANDO RIPRISTINO OK" to como-riga
      **********                       perform SCRIVI-RIGA-LOG
      **********
      **********                       move file-import to wstampa 
      **********                       perform 5 times
      **********                          call "C$SLEEP" using 5
      **********                          open input lineseq
      **********                          if status-lineseq = "00"
      **********                             close lineseq
      **********                             exit perform
      **********                          end-if                      
      **********                       end-perform
      **********                       if status-lineseq not = "00"
      **********                          move 
      **********                          "*** RIPRISTINO NON RIUSCITO ***" 
      **********                            to como-riga          
      **********                          perform SCRIVI-RIGA-LOG
      **********                       end-if
      **********
      **********                    else
      **********                       move 
      **********                       "RIPRISTINO KO, ELABORAZIONE INTERROTTA" 
      **********                         to como-riga          
      **********                       perform SCRIVI-RIGA-LOG
      **********                       move -1 to batch-status  
      **********                       exit perform
      **********                    end-if                              
      **********
      **********                    add  1 to tot-file-ko
      **********                    if RichiamoSchedulato
      **********                       move 1 to batch-status
      **********                    end-if
      **********                 end-if   
      **********              end-if
      **********           end-if
      **********        end-if
      **********     end-perform.   
           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, 
                                         Dir-import-Handle.   

           move 0 to tot-ordini tot-ordini-err.
           if primo-numero < ultimo-numero
              move tge-anno     to emto-anno
              move primo-numero to emto-numero
              start EDI-mtordini key >= emto-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read EDI-mtordini next 
                            at end exit perform 
                       end-read
                       |Non dovrebbe mai succedere ma in caso di lanci multipli...
                       if emto-anno  not = tge-anno or
                          emto-numero > ultimo-numero 
                          exit perform
                       end-if
                       add 1 to tot-ordini
                       if emto-bloccato
                          add 1 to tot-ordini-err
                       end-if
                    end-perform
               end-start
           end-if. 

      ***---
       ELABORA-FILE.
           move 0 to num-riga.
           perform until 1 = 2
              read lineseq-bckp next at end exit perform end-read
              move line-riga-bckp to line-riga
              add 1 to num-riga
              unstring line-riga delimited by ";"
                  into 01G-filler
                       02G-filler
                       03G-filler
                       04G-filler
                       05G-filler
                       06G-tipo-record
              end-unstring
              evaluate 06G-tipo-record
              when "01T" perform ELABORA-TESTA
              when "02D" perform ELABORA-RIGA
              end-evaluate
           end-perform.
           if VerificaPrezziTradizionale
              perform RECUPERA-APPLICA-VERIFICA-PREZZI-TRADIZIONALE 
           end-if.

      ***---
       ELABORA-TESTA.
           if VerificaPrezziTradizionale
              perform RECUPERA-APPLICA-VERIFICA-PREZZI-TRADIZIONALE 
           end-if.
           add  1 to tot-ordini.
           move 0 to riga-dettaglio.
           initialize ecd-rec 
                      cli-rec 
                      des-rec 
                     emto-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           perform UNSTRING-TESTA.

           set emto-righe-non-presenti  to true.
           set emto-bloccato            to true.  

           |In caso trovassi il valore LBX devo cercare direttamente 
           |sui file GESLUX
           if emto-01T22-NAB-QCODBUYER = "LBX" or 
              emto-01T22-NAB-QCODBUYER = "CLBX" 

              |Li forzo a negativi in quanto non saranno mai
              |recuperati dalla scheda 
              set ecd-import-articoli-no to true
              set ecd-import-importi-no  to true
              move ecd-import-articoli   to save-ecd-import-articoli
              move ecd-import-importi    to save-ecd-import-importi  
              
              move emto-01T21-NAB-CODBUYER to NumericEdi

              perform TRATTA-NUMERICO
              move como-numero to des-codice cli-codice
              move emto-01T28-NAD-CODCONS to NumericEdi
              perform TRATTA-NUMERICO
              move como-numero to des-prog

              set cli-tipo-C to true
              read clienti no lock
                    invalid
                    set emto-cliente-non-valido to true
                not invalid
                    if cli-bloccato or cli-disattivo
                       set emto-cliente-non-attivo to true
                    end-if
                    |Se il fido extra è sufficiente verrà deciso dopo
                    |dal programma di conferma
                    if cli-fuori-fido |and cli-fido-extra = 0
                       set emto-cliente-fuori-fido to true
                       set emto-cliente-non-attivo to true
                    end-if

                    if des-prog not = 0
                       read destini no lock       
                            invalid 
           
                            if cli-destino-auto-EDI-no
                               set emto-destino-non-valido to true
                            else
                               if emto-01T30-NAD-RAGSOCD not = spaces
                                  perform AGGIUNGI-DESTINO
                               end-if
                            end-if
                        not invalid
                            if des-bloccato or des-disattivo
                               set emto-destino-non-attivo to true
                            end-if
                       end-read
                    else            
      *****                 if cli-destino-auto-EDI-no
      *****                    set emto-destino-non-valido to true
      *****                 else
                          if emto-01T30-NAD-RAGSOCD not = spaces
                             perform AGGIUNGI-DESTINO
                          end-if
      *****                 end-if
                    end-if
              end-read

      *     move emto-01T28-NAD-CODCONS  to ecd-cod-consegna.
           else                                           
              |METRO PASSA IL VALORE FISSO DEL BUYER A POSIZIONE 41
              if emto-01T41-NAI-CODFATT = "8026924990908"
                 move emto-01T41-NAI-CODFATT  to ecd-cod-dest
              else
                 move emto-01T21-NAB-CODBUYER to ecd-cod-dest
              end-if

              move emto-01T28-NAD-CODCONS  to ecd-cod-consegna
              read edi-clides no lock key ecd-k-orders
                   invalid
                   |CERCO IL CLIENTE PER RECUPERARE IL CODICE
                   move spaces to ecd-cod-consegna
                   read edi-clides no lock key ecd-k-orders
                        invalid
                        set emto-clides-non-valido to true
                    not invalid
                        set cli-tipo-C to true
                        move ecd-cli-codice to cli-codice
                        read clienti no lock
                             invalid 
                             set emto-cliente-non-valido to true
                         not invalid        
                             perform RECUPERA-DATI-CLIENTE

                             if cli-bloccato or cli-disattivo
                                set emto-cliente-non-attivo to true
                             end-if
                             |Se il fido extra è sufficiente verrà deciso dopo
                             |dal programma di conferma
                             if cli-fuori-fido |and cli-fido-extra = 0
                                set emto-cliente-fuori-fido to true
                                set emto-cliente-non-attivo to true
                             end-if
        
                             | Questi ordini devono SEMPRE creare un nuovo destino
                             if emto-01T22-NAB-QCODBUYER = 14 and
                                emto-01T21-NAB-CODBUYER = 8001120009005
                                perform AGGIUNGI-DESTINO
                             else
                                if cli-destino-auto-EDI-no
                                   set emto-clides-non-valido to true
                                else
                                   if emto-01T30-NAD-RAGSOCD 
                                      not = spaces
                                      perform AGGIUNGI-DESTINO
                                   end-if
                                end-if
                             end-if
                        end-read
                   end-read
               not invalid
                   set cli-tipo-C to true
                   move ecd-cli-codice to cli-codice
                   read clienti no lock
                        invalid
                        set emto-cliente-non-valido to true
                    not invalid
                        perform RECUPERA-DATI-CLIENTE 
                     
                        if cli-bloccato or cli-disattivo
                           set emto-cliente-non-attivo to true
                        end-if
                        |Se il fido extra è sufficiente verrà
                        |deciso dopo dal programma di conferma
                        if cli-fuori-fido |and cli-fido-extra = 0
                           set emto-cliente-fuori-fido to true
                           set emto-cliente-non-attivo to true
                        end-if        
                                                           
                        if ecd-prg-destino not = 0
                           move ecd-cli-codice  to des-codice
                           move ecd-prg-destino to des-prog
                           read destini no lock
                                invalid 
                                set emto-destino-non-valido to true
                            not invalid
                                if des-bloccato or des-disattivo
                                  set emto-destino-non-attivo to true
                                end-if
                           end-read
                        end-if
                   end-read
              end-read
           end-if.                                 

           move tge-anno         to emto-anno.
           move tge-causale-omag to emto-causale.
           move cli-codice       to emto-cod-cli.
           move des-prog         to emto-prg-destino.
           move cli-gdo          to emto-gdo.
           move emto-01T05-BGM-NUMDOC to emto-num-ord-cli.
           if emto-01T04-BGM-DATADOC = spaces
              accept emto-data-ordine from century-date
           else
              move emto-01T04-BGM-DATADOC to emto-data-ordine
           end-if.
           accept emto-data-passaggio-ordine from century-date.
           move cli-agente  to emto-cod-agente.
           move cli-pag     to emto-cod-pagamento.
           move cli-iva-ese to emto-cod-ese-iva.
           if emto-01T163-TOD_CODCOST = "CC"
              set emto-ritira-si to true
           else                     
              move 0 to emto-vettore
              if des-vettore not = 0
                 move des-prov to como-prov
                 perform VETTORE-PROV-REG
                 if emto-vettore = 0
                    move des-vettore to emto-vettore
                 end-if
              else                         
                 move cli-prov to como-prov
                 perform VETTORE-PROV-REG
                 if emto-vettore = 0
                    move cli-vettore to emto-vettore
                 end-if
              end-if
           end-if.                       
           move emto-01T70-FTX-note to emto-note.
                                           
           initialize not-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move cli-codice to not-codice
           move des-prog   to not-prog
           read note invalid continue end-read.
           move not-note-1 to emto-note1.
                                   
           |18.09.2014 Bisogna utilizzare le nostre note, quelle collegate ai clienti.
           |Le note importate in EDI servono solo nella stampa bozza ordine.
      *****     if emto-01T35-FTX-NOTE = spaces and
      *****        emto-01T68-FTX-note = spaces and
      *****        emto-01T69-FTX-note = spaces 
      *****        move not-note-2 to emto-note2
      *****        move not-note-3 to emto-note3
      *****        move not-note-4 to emto-note4
      *****     else
      *****        move emto-01T35-FTX-NOTE to emto-note2
      *****        move emto-01T68-FTX-note to emto-note3
      *****        move emto-01T69-FTX-note to emto-note4
      *****     end-if.                                  
           move not-note-2 to emto-note2.
           move not-note-3 to emto-note3.
           move not-note-4 to emto-note4.

           move emto-01T11-DTM-DATACONS to emto-data-note1.
           compute  oggi-piu-7 = function integer-of-date (como-data).
           add 7 to oggi-piu-7.
           if emto-data-note1 = 0
              compute  oggi-meno-1 = 
                       function integer-of-date (como-data)
              subtract 1 from oggi-meno-1      
              compute emto-data-note1 =
                      function date-of-integer (oggi-meno-1)
      *****     else
      *****        compute emto-data-note1 =
      *****                function integer-of-date (emto-data-note1)
      *****        if emto-data-note1 < oggi-piu-7
      *****           move oggi-piu-7 to emto-data-note1
      *****        end-if
      *****        compute emto-data-note1 =
      *****                function date-of-integer (emto-data-note1)
           end-if.

           move emto-cod-cli     to como-prm-cliente.
           move emto-prg-destino to como-prm-destino.
           perform TROVA-PARAMETRO.
           move prm-saldi-banco      to emto-saldi-banco.
           move prm-saldi-promo      to emto-saldi-promo.
           move prm-prenotazione-qta to emto-prenotazione-qta.

           accept emto-data-creazione from century-date.
           accept emto-ora-creazione  from time.
           move "IMPORT EDI"   to emto-utente-creazione.
           move num-riga  to emto-riga-file.
           move nome-file to emto-nome-file.
           accept emto-evadi-dal from century-date.

           move ultimo-numero to emto-numero.
           perform until 1 = 2
              add 1 to emto-numero ultimo-numero
              write emto-rec
                    invalid continue
                not invalid exit perform
              end-write
           end-perform.

           initialize como-riga.
           string "CREATO ORDINE: " delimited size
                  emto-anno         delimited size
                  " - "             delimited size
                  emto-numero       delimited size
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           initialize chk-ord-cli-linkage
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.

           move emto-chiave      to chk-ord-chiave  chk-ord-chiave-EDI.
           move emto-cod-cli     to chk-cod-cli     chk-cod-cli-EDI.
           move emto-prg-destino to chk-prg-destino chk-prg-destino-EDI.
           move emto-num-ord-cli to chk-num-ord-cli chk-num-ord-cli-EDI.

           call   "check-ord-cli" using chk-ord-cli-linkage.
           cancel "check-ord-cli".

           if chk-ord-status = -1 or chk-ord-status-EDI = -1
              set emto-esistente-si to true
              set emto-bloccato     to true
           else
              set emto-esistente-no to true
           end-if.

           set RecuperaArticolo to false.
           move cli-tipo to tcl-codice
           read ttipocli no lock 
                invalid continue 
            not invalid
                if tcl-gdo-si
                   set RecuperaArticolo to true
                end-if
           end-read.

           set tcl-gdo-si      to true.
           set ttipocli-gdo    to true.
           set tcl-si-recupero to true.
           if not ( emto-cliente-non-valido )
              move cli-tipo to tcl-codice
              read ttipocli no lock invalid continue end-read
           end-if.
           if tcl-gdo-no set OrdineTradizionale to true
           else          set OrdineTradizionale to false
           end-if.
                                                       
           set emto-inversione-imposte-no to true.
           |Se dal file di import mi arriva la X e sono su un 
           |tradizionale setto l'inversione
           if emto-01T60-inversione-imposte = "X" or
              emto-01T60-inversione-imposte = "x"
              if ttipocli-standard
                 set emto-inversione-imposte-si to true
              end-if
           end-if.
                                                       
           set emto-ev-immediata-no to true.
           if emto-01T61-ev-immediata = "X" or
              emto-01T61-ev-immediata = "x"
              set emto-ev-immediata-si to true
           end-if.
           rewrite emto-rec.


      *****     if tcl-manuale-si
      *****        set OrdineTradizionale to false
      *****     end-if.

      *****     if OrdineTradizionale and              
      *****        |Un ordine con LBX ha già i prezzi corretti
      *****        emto-01T22-NAB-QCODBUYER not = "LBX"
      *****        set VerificaPrezziTradizionale to true
      *****     end-if.  

           if emto-01T37-BGM-CODAZION = "BLO"
              set emto-bloc-forzato-si to true
           end-if.

      ***---
       RECUPERA-DATI-CLIENTE.
           move ecd-chiave to save-ecd-chiave.

           move cli-codice to ecd-cli-codice
           move 0          to ecd-prg-destino
           read edi-clides
                invalid
                set ecd-import-articoli-no  to true
                set ecd-import-importi-no   to true
           end-read
           move ecd-import-articoli  to save-ecd-import-articoli.
           move ecd-import-importi   to save-ecd-import-importi.
           move save-ecd-chiave to ecd-chiave.
           read edi-clides.

      ***---
       AGGIUNGI-DESTINO.  
           if emto-01T30-NAD-RAGSOCD = spaces      
              set emto-destino-non-valido to true
              exit paragraph 
           end-if.
           initialize des-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           |LO CERCO NELLA STESSA PROVINCIA
           move cli-codice             to des-codice.
           move 0 to sav-vettore.
           start destini key >= des-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read destini next at end exit perform end-read
                    if des-codice   not = cli-codice
                       exit perform
                    end-if
                    if des-prov = emto-01T33-NAD-PROVD and des-prog > 0
                       move des-vettore to sav-vettore
                       exit perform
                    end-if
                 end-perform
           end-start.
           |SE NON LO TROVO LO CERCO PER LA STESSA REGIONE
           if sav-vettore = 0
              move emto-01T33-NAD-PROVD to prv-codice
              read tprov no lock
                   invalid continue
               not invalid                           
                   move cli-codice  to des-codice
                   move low-value   to des-prog
                   move prv-regione to sav-regione
                   start destini key >= des-chiave
                         invalid continue
                     not invalid
                         perform until 1 = 2
                            read destini next 
                                 at end exit perform 
                            end-read
                            if des-codice   not = cli-codice
                               exit perform
                            end-if
                            if des-prog > 0
                               move des-prov to prv-codice
                               read tprov no lock
                                    invalid continue

                                not invalid
                                    if prv-regione = sav-regione
                                       move des-vettore to sav-vettore
                                       exit perform
                                    end-if
                               end-read
                            end-if
                         end-perform
                   end-start
              end-read
           end-if.          

           initialize des-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move cli-codice to des-codice.

           move high-value to des-prog
           start destini key <= des-chiave
                 invalid move 0 to des-prog
             not invalid
                 read destini previous
                 if des-codice not = cli-codice
                    move 0 to des-prog
                 end-if
           end-start.
                                                               
           move cli-codice to des-codice.
           add 1           to des-prog
           initialize des-dati replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           move emto-01T30-NAD-RAGSOCD to des-ragsoc-1.       
           move emto-01T31-NAD-INDIRD  to des-indirizzo.
           move emto-01T32-NAD-CITTAD  to des-localita.
           move emto-01T33-NAD-PROVD   to des-prov.
           move emto-01T34-NAD-CAPD    to des-cap.
           if des-prov = "EE"
              move cli-nazione to des-nazione
           else
              move "ITA"       to des-nazione
                    
              move des-prov to prv-codice
              read tprov no lock
                   invalid 
                   set emto-destino-non-valido to true
                   set emto-clides-non-valido  to true
              end-read
              move des-cap to anc-cap
              read anacap no lock
                   invalid 
                   set emto-destino-non-valido to true
                   set emto-clides-non-valido  to true
              end-read

           end-if.
           move sav-vettore to des-vettore.
           move "N"   to des-deposito-UTF.
           set des-attivo   to true.
           set des-no-invio to true.
           accept des-data-creazione from century-date.
           accept des-ora-creazione  from time.
           move "IMPORT EDI" to des-utente-creazione  

           write des-rec.

           initialize ecd-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move cli-codice              to ecd-cli-codice.
           move des-prog                to ecd-prg-destino.

           |METRO PASSA IL VALORE DEL BUYER A POSIZIONE 41
           if emto-01T41-NAI-CODFATT not = spaces
              move emto-01T41-NAI-CODFATT  to ecd-cod-dest
           else
              move emto-01T21-NAB-CODBUYER to ecd-cod-dest
           end-if.

           move "91"                    to ecd-q-cod-dest.
           move emto-01T28-NAD-CODCONS  to ecd-cod-consegna.
           move "92"                    to ecd-q-cod-consegna.
           move emto-01T30-NAD-RAGSOCD  to ecd-ragsoc-d.
           move emto-01T31-NAD-INDIRD   to ecd-indirizzo-d.
           move emto-01T32-NAD-CITTAD   to ecd-citta-d.
           move emto-01T33-NAD-PROVD    to ecd-prov-d.
           move emto-01T34-NAD-CAPD     to ecd-cap-d.
           write ecd-rec.


      ***---
       ELABORA-RIGA.
           if emto-righe-non-presenti
              if emto-cliente-valido and
                 emto-destino-valido and
                 emto-esistente-no   and
                 emto-bloc-forzato-no
                 set emto-attivo to true
              end-if
              set emto-righe-presenti to true
              rewrite emto-rec
           end-if.

           add 1 to riga-dettaglio.
           perform CONTATORE-VIDEO.
           initialize emro-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           perform UNSTRING-RIGA.

           move emto-chiave    to emro-chiave-testa.
           move riga-dettaglio to emro-riga.
           set emro-attivo to true.

           move emro-02D13-LIN-CODFORTU to NumericEdi.
           perform TRATTA-NUMERICO.
           move como-numero        to emro-cod-articolo.
                                             

           if como-numero = 0  and 
              RecuperaArticolo and 
              emro-02D14-LIN-CODDISTU not = spaces
              move high-value to lst-chiave
              move cli-gdo    to lst-gdo
              move emro-02D14-LIN-CODDISTU to lst-cod-art-cli
              start listini key <= lst-k-cod-art-cli
                    invalid continue
                not invalid
                    read listini previous
                    if lst-gdo         = cli-gdo and
                       lst-cod-art-cli = emro-02D14-LIN-CODDISTU
                       set NotNumericFound to false
                       move lst-articolo to como-numero 
                                            emro-cod-articolo
                    end-if
              end-start
           end-if.     

           if como-numero > 999999 or NotNumericFound
              move 0 to emro-cod-articolo

              set emro-bloccato            to true
              set emro-articolo-non-valido to true

              set emto-bloccato  to true
              set emto-art-ko    to true
              rewrite emto-rec end-rewrite
           else
              if emro-cod-articolo > 99999
                 set emro-si-blister to true
                 move emro-cod-articolo  to bli-codice
                 read blister no lock
                      invalid
                      set emro-bloccato            to true
                      set emro-articolo-non-valido to true

                      set emto-bloccato  to true
                      set emto-art-ko    to true
                      rewrite emto-rec end-rewrite
                  not invalid
                      if bli-bloccato or bli-disattivo
                         set emro-bloccato            to true
                         set emro-articolo-non-attivo to true
     
                         set emto-bloccato  to true
                         set emto-art-ko    to true
                         rewrite emto-rec end-rewrite
                      else
                         perform CONTROLLA-VALIDITA-COMPONENTI
                         if bli-bloccato
                            set emro-bloccato            to true
                            set emro-articolo-non-attivo to true
     
                            set emto-bloccato  to true
                            set emto-art-ko    to true
                            rewrite emto-rec end-rewrite
                         end-if
                      end-if
                 end-read
              else
                 move emro-cod-articolo  to art-codice
                 read articoli no lock
                      invalid
                      set emro-bloccato            to true
                      set emro-articolo-non-valido to true

                      set emto-bloccato  to true
                      set emto-art-ko    to true
                      rewrite emto-rec end-rewrite
                  not invalid
                      move art-peso-utf     to prg-peso-utf
                      move art-peso-non-utf to prg-peso-non-utf
                      if art-bloccato or art-disattivo
                         set emro-bloccato            to true
                         set emro-articolo-non-attivo to true
     
                         set emto-bloccato  to true
                         set emto-art-ko    to true
                         rewrite emto-rec end-rewrite
                      end-if
                 end-read
              end-if
           end-if.
                                     
           move emro-02D17-QTAORD to NumericEDI.
           perform TRATTA-NUMERICO.
           if not NotNumericFound and como-numero > 0
              move como-numero to qta-ord-EDI
              move emro-02D22-LIN-NRCUINTU to NumericEDI
              perform TRATTA-NUMERICO
              if not NotNumericFound and como-numero > 0
                 move como-numero to qta-imb-EDI
                 |ho la quantità in imballi (ma non la devo moltiplicare
                 |per la qta imballi che ricevo)
                 if save-ecd-import-articoli-si or 
                    save-ecd-import-importi-si          
                    move qta-ord-EDI to emro-qta-GESLUX emro-qta-EDI
                 else                         
                    compute emro-qta-GESLUX = qta-ord-EDI * qta-imb-EDI
                    move emro-qta-GESLUX to emro-qta-EDI
                 end-if
              else
                 move emro-02D222-QTAPZ to NumericEDI
                 perform TRATTA-NUMERICO
                 if not NotNumericFound and como-numero > 0
                    move como-numero to emro-qta-GESLUX emro-qta-EDI
                 else
                    move qta-ord-EDI to emro-qta-GESLUX emro-qta-EDI
                 end-if
              end-if
           end-if.

           if emro-qta-EDI = 0 or NotNumericFound
              set emro-bloccato          to true
              set emro-qtac-non-presente to true
              set emro-progressivo-non-trovato to true

              set emto-bloccato to true
              set emto-qta-ko   to true
              set emto-prg-ko   to true
              rewrite emto-rec
           else
              if not emro-articolo-non-valido
                 perform ASSEGNA-PROGRESSIVO 

                 if emro-progressivo-non-attivo  or
                    emro-progressivo-non-trovato or
                    emro-progressivo-non-forzato
                    set emro-bloccato to true

                    set emto-bloccato to true
                    set emto-prg-ko   to true
                 end-if
                 add emro-qta-GESLUX to emto-pz-tot
                 rewrite emto-rec
              end-if
           end-if.

           set trovato-movim to false.
           move emro-02D19-LIN-PRZUNI to NumericEDI.
           perform TRATTA-NUMERICO.
           compute emro-prz-EDI rounded = como-numero.  
           if emro-prz-EDI = 0 or NotNumericFound
              set emro-prezzo-non-valido to true
              set emro-bloccato          to true

              set emto-bloccato to true
              set emto-prz-ko   to true
              rewrite emto-rec

      *****        set emro-si-omaggio to true
      *****        move tge-cod-iva-omag to emro-cod-iva
           else                                       
              if save-ecd-import-importi-si
                 compute como-numero = emro-prz-EDI / imq-qta-imb
                 move como-numero to emro-prz-EDI
              end-if

              if emro-articolo-valido or emro-articolo-non-attivo
                 move emro-prz-EDI to emro-prz-GESLUX
              end-if

      *****        if emto-cod-ese-iva not = spaces
      *****           move emto-cod-ese-iva to emro-cod-iva
      *****        else
      *****           move tge-cod-iva-std  to emro-cod-iva
      *****        end-if
      *****        set emro-no-omaggio to true
              move tge-causale-ordini-std to emto-causale
              rewrite emto-rec
           end-if.

           if not emro-si-blister
              if emro-articolo-non-valido or OrdineTradizionale
                |Un ordine con LBX ha già i prezzi corretti
                 if not ( emto-01T22-NAB-QCODBUYER = "LBX" or
                          emto-01T22-NAB-QCODBUYER = "PLBX" )
                    move 9999999,99  to emro-prz-GESLUX
                    set  emro-bloccato-prezzo-si to true
                 end-if
              else
                 perform RECUPERA-DATI-FROM-RMOVMAG
              end-if
           end-if.

           if emro-articolo-valido or emro-articolo-non-attivo and 
              not OrdineTradizionale
              |Recupero il prezzo da listino, se ordine tradizionale lo
              |recupero e lo verifico dopo quando ho l'ordine intero
                                                     
             |Un ordine con LBX ha già i prezzi corretti
              if emto-01T22-NAB-QCODBUYER = "LBX" or
                 emto-01T22-NAB-QCODBUYER = "PLBX"
                 if tcl-gdo-si                                  
                    perform RECUPERA-PREZZO
                 else
                    perform RECUPERA-SOLO-PROMO
                 end-if
              else                          
                 perform RECUPERA-PREZZO
              end-if

           end-if.
                                                    
           if emro-qta-EDI not = 0
              if emro-qta-EDI not = emro-qta-GESLUX and not 
                 emro-si-blister
                 set emro-bloccato      to true
                 set emro-qtac-adattata to true
                 set emto-bloccato      to true
                 set emto-qta-ko        to true
                 rewrite emto-rec end-rewrite
              else
                 set emro-qtac-ok to true
                 move emro-qta-GESLUX to emro-qta
              end-if
              if not emro-si-blister
                 move imq-qta-imb to emro-qta-imballi
              else
                 move 1 to emro-qta-imballi 
              end-if
              compute emro-num-colli = 
                      emro-qta-GESLUX / emro-qta-imballi
           end-if.
           
           if emro-si-blister
              if not trovato-promo               
                 set  emro-bloccato-prezzo-si to true
                 move 99999999,99 to TotPrzBlister
              else                                      
                 set  emro-bloccato-prezzo-no to true
                 move emro-prz-GESLUX to TotPrzBlister
              end-if
      *****        if emro-articolo-valido
      *****           perform PREZZO-COMPONENTE-BLISTER
      *****        end-if
           end-if.

           if emro-prz-GESLUX not = emro-prz-EDI
              set emro-prezzo-non-valido to true
              set emro-bloccato to true

              set emto-bloccato to true
              set emto-prz-ko   to true
              rewrite emto-rec
           else
              move emro-prz-GESLUX to emro-prz
           end-if.

           perform DATI-COMUNI.
           write emro-rec.

      *****     if  emro-si-blister and
      *****       ( emro-articolo-valido or emro-articolo-non-attivo )
      *****        perform varying idx from 2 by 1
      *****                  until idx > LastIdx
      *****           perform ARTICOLO-COMPONENTE-BLISTER
      *****           perform ASSEGNA-PROGRESSIVO
      *****           perform PREZZO-COMPONENTE-BLISTER
      *****           perform DATI-COMUNI
      *****           add 1 to emro-riga
      *****           write emro-rec
      *****        end-perform
      *****     end-if.
           |BLISTER

      ***---
       CONTROLLA-VALIDITA-COMPONENTI.
           move emro-cod-articolo to bli-codice art-codice.
           read blister no lock invalid continue end-read.

           perform varying idx from 1 by 1
                     until idx > 50
              if bli-el-articolo(idx) = 0 exit perform end-if
              move bli-el-articolo(idx) to art-codice
              read articoli no lock
                   invalid continue
              end-read
              if art-attivo
                 set bli-bloccato  to true
                 move low-value    to prg-chiave
                 move art-codice   to prg-cod-articolo
                 move "LBX"        to prg-cod-magazzino
                 start progmag key >= prg-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read progmag next at end exit perform end-read
                          if prg-cod-articolo  not = art-codice or
                             prg-cod-magazzino not = "LBX" |Sarà sempre questo il magazzino sia per OMOM che AAAA
                             exit perform
                          end-if
                          if prg-attivo
                             set bli-attivo to true
                             exit perform
                          end-if
                       end-perform
                 end-start
                 if bli-bloccato
                    exit perform
                 end-if
              else
                 set bli-bloccato to true
                 exit perform
              end-if
           end-perform.

      ***---
       DATI-COMUNI.
           accept emro-evadi-dal      from century-date.
           accept emro-data-creazione from century-date.
           accept emro-ora-creazione  from time.
           move "IMPORT EDI"   to emro-utente-creazione.
           move num-riga   to emro-riga-file.
           move nome-file  to emro-nome-file.

      ***---
       ARTICOLO-COMPONENTE-BLISTER.
           move bli-el-articolo(idx) to art-codice emro-cod-articolo.
           read articoli no lock.
           if bli-bloccato or bli-disattivo or
              art-bloccato or art-disattivo

              set emro-bloccato            to true
              set emro-articolo-non-attivo to true

              set emto-bloccato  to true
              set emto-art-ko    to true
              rewrite emto-rec end-rewrite
           end-if.

      ********---
      ***** PREZZO-COMPONENTE-BLISTER.
      *****     compute emro-prz-GESLUX =
      *****             TotPrzBlister * 
      *****             bli-el-perce(idx)/ 100 / 
      *****             bli-el-qta(idx) .
      *****     perform ARROTONDA-PRZ-BLISTER. 

      *****     if emro-prz-GESLUX = 0
      *****        set emro-si-omaggio to true
      *****        move tge-cod-iva-omag to emro-cod-iva
      *****     else
      *****        if emto-cod-ese-iva not = spaces
      *****           move emto-cod-ese-iva to emro-cod-iva
      *****        else
      *****           move tge-cod-iva-std  to emro-cod-iva
      *****        end-if
      *****        set emro-no-omaggio to true
      *****     end-if.
   
      ***---
       ASSEGNA-PROGRESSIVO.
           set emro-progressivo-non-trovato to true.
           if emro-si-blister
              set emro-progressivo-valido to true
           else
              perform TROVA-LISTINO
              if no-prg-listino
                 perform TROVA-CLI-PRG
              end-if
              if como-prg-cod-articolo not = 0
                 move como-prg-chiave  to emro-prg-forzato
                 move prg-tipo-imballo to imq-codice
                 read timbalqta no lock
                      invalid continue
                  not invalid                                 
                      set trovato-progressivo to true
                      if save-ecd-import-articoli-si
      *****                   compute emro-qta-GESLUX = 
      *****                           emro-qta-GESLUX * imq-qta-imb
                         perform VALORIZZA-PROGRESSIVO
                         set emro-progressivo-valido     to true
                      else
                         |Se ho un progressivo forzato prendo quello fisso
                         |se attivo e adatto alla quantità se minore
                         if emro-qta-GESLUX < imq-qta-imb
                            move imq-qta-imb to emro-qta-GESLUX
                         end-if

      *****             if imq-qta-imb <= emro-qta
                            move 0 to como-div
                            compute como-div = emro-qta-GESLUX /
                                               imq-qta-imb
                            compute como-qta = como-div * imq-qta-imb
                            if emro-qta-GESLUX not = como-qta
                               add 1 to como-div
                               compute emro-qta-GESLUX = 
                                       como-div * imq-qta-imb
                            end-if
                            perform VALORIZZA-PROGRESSIVO
                            set emro-progressivo-valido     to true
                      end-if   
      *****             end-if
                 end-read
              end-if
           end-if.

           |Se trova un forzato ed è attivo lo assegna in automatico
           |quindi non entrerà nemmeno nella ricerca
           if not emro-progressivo-valido
              perform PROGRESSIVO-ARTICOLO
           end-if.

      ***---
       PROGRESSIVO-ARTICOLO.
      *    18/11/2014 (richiesta di Walter):
      *    prima cerco un progressivo a maggior giacenza (NON assoluta)
      *    con lo stesso imballo, se non lo trovo assegno al progressivo
      *    con maggior giacenza (NON assoluta) e adatto la quantità
           set trovato-progressivo to false.
           perform varying ricerca from 1 by 1 until ricerca > 8
              perform RICERCA-PROGRESSIVO
              if trovato-progressivo
                 exit perform
              end-if
           end-perform.

           if trovato-progressivo
              move como-prg-chiave to prg-chiave
              read progmag no lock
              move prg-tipo-imballo to imq-codice
              read timbalqta no lock
              if save-ecd-import-articoli-si
                 continue
      *****           compute emro-qta-GESLUX = emro-qta-GESLUX * imq-qta-imb
              else          
                 if qta-imb-edi = 0
                    if emro-qta-GESLUX < imq-qta-imb
                       move imq-qta-imb to emro-qta-GESLUX
                    else
                       compute como-div = emro-qta-GESLUX /
                                          imq-qta-imb
                       compute como-qta = como-div * imq-qta-imb
                       if emro-qta-GESLUX not = como-qta
                          add 1 to como-div
                       end-if                                          
                       compute emro-qta-GESLUX = como-div * imq-qta-imb
                  
                    end-if
                 else                                     
                    if emro-qta-GESLUX < imq-qta-imb 
                       set emro-qtac-adattata to true
                       move imq-qta-imb to emro-qta-GESLUX
                    else
                       compute emro-qta-GESLUX = 
                               qta-ord-EDI * qta-imb-edi
                       |Faccio comunque un controllo 
                       |che la qta EDI sia corretta  
                       move 0 to resto
                       divide emro-qta-GESLUX by imq-qta-imb
                              giving como-qta 
                           remainder resto
                       if resto > 0
                          add 1 to como-qta
                          compute emro-qta-GESLUX = 
                                  como-qta * imq-qta-imb
                       end-if       
                    end-if                      
                 end-if
              end-if
              perform VALORIZZA-PROGRESSIVO
              if prg-attivo
                 set emro-progressivo-valido to true
              else
                 set emro-progressivo-non-attivo to true
              end-if
           else
              set emro-progressivo-non-trovato to true
           end-if.

      ***---
       RICERCA-PROGRESSIVO.
           evaluate ricerca
      *     cerco progressivo attivo, con giacenza e con = imballo
           when 1
                move 0 to como-giacenza
      *     cerco progressivo attivo, con giacenza
           when 2
                move 0 to como-giacenza

      *     cerco progressivo attivo con = imballo
           when 3
                move -999999999 to como-giacenza
      *     cerco progressivo attivo
           when 4
                move -999999999 to como-giacenza

      *     cerco progressivo con giacenza e con = imballo
           when 5
                move 0 to como-giacenza
      *     cerco progressivo con giacenza
           when 6
                move 0 to como-giacenza

      *     cerco progressivo con = imballo
           when 7
                move -999999999 to como-giacenza
      *     cerco progressivo
           when 8
                move -999999999 to como-giacenza
           end-evaluate.

           initialize como-prg-chiave
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.
           move low-value  to prg-chiave.
           move art-codice to prg-cod-articolo.
           move "LBX"      to prg-cod-magazzino.
           start progmag key >= prg-chiave
                 invalid
                 rewrite emto-rec end-rewrite
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo  not = art-codice or
                       prg-cod-magazzino not = "LBX"
                       exit perform
                    end-if
                    move prg-tipo-imballo to imq-codice
                    read timbalqta no lock
                         invalid  continue
                     not invalid
                         if qta-imb-EDI = 0
                            evaluate ricerca
                            when 1
                            when 3
                                 move 0 to como-qta
                                 if imq-qta-imb <= emro-qta-EDI
                                    compute como-div = 
                                            emro-qta-EDI / imq-qta-imb
                                    compute como-qta = 
                                            como-div * imq-qta-imb
                                 end-if        
                                 if como-qta = emro-qta-EDI      and
                                    prg-giacenza > como-giacenza and
                                    prg-attivo
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                            when 2
                            when 4
                                 if prg-giacenza > como-giacenza and
                                    prg-attivo
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                            when 5
                            when 7                                  
                                 move 0 to como-qta
                                 if imq-qta-imb <= emro-qta-EDI
                                    compute como-div = 
                                            emro-qta-EDI / imq-qta-imb
                                    compute como-qta = 
                                            como-div * imq-qta-imb
                                 end-if        
                                 if como-qta = emro-qta-EDI  and
                                    prg-giacenza > como-giacenza
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                            when 6
                            when 8
                                 if prg-giacenza > como-giacenza
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                            end-evaluate
                         else
                            if save-ecd-import-articoli-si
                               evaluate ricerca
                               when 2
                               when 4
                                 if prg-giacenza > como-giacenza and
                                    prg-attivo
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                               when 6
                               when 8
                                 if prg-giacenza > como-giacenza
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                               end-evaluate
                            else
                               evaluate ricerca
                               when 1
                               when 3 
                                 if qta-imb-EDI = imq-qta-imb    and
                                    prg-giacenza > como-giacenza and
                                    prg-attivo
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                               when 2
                               when 4
                                 if prg-giacenza > como-giacenza and
                                    prg-attivo
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                               when 5
                               when 7
                                 if qta-imb-EDI = imq-qta-imb    and
                                    prg-giacenza > como-giacenza
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                               when 6
                               when 8
                                 if prg-giacenza > como-giacenza
                                    move prg-giacenza to como-giacenza
                                    move prg-chiave   to como-prg-chiave
                                    set trovato-progressivo to true
                                 end-if
                               end-evaluate
                            end-if
                         end-if
                    end-read
                 end-perform
           end-start.

      *****     set trovato-progressivo to false.
      *****     move low-value  to prg-chiave.
      *****     move art-codice to prg-cod-articolo.
      *****     move "LBX"      to prg-cod-magazzino.
      *****     start progmag key >= prg-chiave
      *****           invalid                            
      *****           rewrite emto-rec end-rewrite
      *****       not invalid
      *****           perform until 1 = 2
      *****              read progmag next at end exit perform end-read
      *****              if prg-cod-articolo  not = art-codice or
      *****                 prg-cod-magazzino not = "LBX"
      *****                 exit perform
      *****              end-if
      *****              move prg-tipo-imballo to imq-codice
      *****              read timbalqta no lock
      *****                   invalid continue
      *****               not invalid
      *****                   if imq-qta-imb <= emro-qta-GESLUX
      *****                      move 0 to como-div
      *****                      compute como-div = 
      *****                              emro-qta-GESLUX / imq-qta-imb
      *****                      compute como-qta = como-div * imq-qta-imb
      *****                      if emro-qta-GESLUX = como-qta
      *****                         set trovato-progressivo to true
      *****                         perform VALORIZZA-PROGRESSIVO
      *****                         if prg-attivo
      *****                            set emro-progressivo-valido to true
      *****                                                        
      ******                            Do la precedenza all'imballo uguale a 
      ******                            quello impostato in EDI infatti se lo 
      ******                            trovo esco dal ciclo e tengo quello
      ******                            altrimenti è valido l'ULTIMO BUONO TROVATO
      *****                            if imq-qta-imb = qta-imb-EDI or
      *****                               qta-imb-EDI = 0
      *****                               exit perform
      *****                            end-if
      *****                         else
      *****                            set emro-progressivo-non-attivo 
      *****                             to true
      *****                         end-if
      *****                      end-if
      *****                   end-if
      *****              end-read
      *****           end-perform
      *****     end-start.  
      *****
      *****     |Modifico io la quantità
      *****     if not trovato-progressivo
      *****        move low-value  to prg-chiave
      *****        move art-codice to prg-cod-articolo
      *****        move "LBX"      to prg-cod-magazzino
      *****        start progmag key >= prg-chiave
      *****              invalid                            
      *****              rewrite emto-rec end-rewrite
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read progmag next at end exit perform end-read
      *****                 if prg-cod-articolo  not = art-codice or
      *****                    prg-cod-magazzino not = "LBX"
      *****                    exit perform
      *****                 end-if
      *****                 move prg-tipo-imballo to imq-codice
      *****                 read timbalqta no lock
      *****                      invalid continue
      *****                  not invalid
      *****                      if imq-qta-imb <= emro-qta-GESLUX
      *****                         move 0 to como-div
      *****                         compute como-div = 
      *****                                 emro-qta-GESLUX / imq-qta-imb
      *****                         add 1 to como-div
      *****                         compute emro-qta-GESLUX = 
      *****                                 como-div * imq-qta-imb
      *****                         set trovato-progressivo to true
      *****                         perform VALORIZZA-PROGRESSIVO
      *****                         if prg-attivo
      *****                            set emro-progressivo-valido to true
      *****                            exit perform
      *****                         else
      *****                            set emro-progressivo-non-attivo 
      *****                             to true
      *****                         end-if
      *****                      else
      *****                         move imq-qta-imb to emro-qta-GESLUX
      *****                         set trovato-progressivo to true
      *****                         perform VALORIZZA-PROGRESSIVO
      *****                         if prg-attivo
      *****                            set emro-progressivo-valido to true
      *****                            exit perform
      *****                         else
      *****                            set emro-progressivo-non-attivo 
      *****                             to true
      *****                         end-if
      *****                      end-if
      *****                 end-read
      *****              end-perform
      *****        end-start
      *****     end-if.

      ***---
       VALORIZZA-PROGRESSIVO.
           move prg-chiave       to emro-prg-chiave.
           move prg-peso-utf     to emro-peso-utf.
           move prg-peso-non-utf to emro-peso-non-utf.

           move prg-tipo-imballo to imq-codice.
           read timbalqta no lock
                invalid continue
            not invalid
                move imq-tipo to imb-codice
                read timballi no lock
                     invalid continue
                 not invalid
                     move imb-descrizione to emro-des-imballo
                end-read
           end-read.

      ********---
      ***** PROGRESSIVO-BLISTER.
      *****     move bli-codice           to emro-bli-codice.
      *****     move bli-el-qta(idx)      to emro-bli-qta.
      *****     move bli-el-perce(idx)    to emro-bli-perce.
      *****     |Valorizzo prg-chiave con il record avente > giacenza
      *****     move 0 to giacenza.
      *****     set  trovato              to false.
      *****     move low-value            to prg-chiave.
      *****     move bli-el-articolo(idx) to prg-cod-articolo art-codice.
      *****     move bli-magazzino        to prg-cod-magazzino.
      *****     perform TROVA-LISTINO.
      *****     if no-prg-listino
      *****        perform TROVA-CLI-PRG
      *****     end-if.
      *****     if como-prg-cod-articolo not = 0
      *****        move como-prg-chiave  to emro-prg-forzato GiacenzaKey
      *****     else
      *****        start progmag key >= prg-chiave
      *****              invalid continue
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read progmag next at end exit perform end-read
      *****                 if prg-cod-articolo  not = bli-el-articolo(idx)
      *****                 or prg-cod-magazzino not = bli-magazzino
      *****                    exit perform
      *****                 end-if
      *****                 if prg-attivo    
      *****                    set emro-progressivo-valido to true
      *****                    if not trovato
      *****                       set trovato to true
      *****                       |Se la prima ed unica volta ho una giacenza di -1
      *****                       move prg-giacenza to giacenza
      *****                       move prg-chiave   to GiacenzaKey
      *****                    end-if
      *****                    if prg-giacenza > giacenza
      *****                       move prg-giacenza to giacenza
      *****                       move prg-chiave to GiacenzaKey
      *****                    end-if
      *****                 end-if
      *****              end-perform
      *****        end-start
      *****     end-if.
      *****     move GiacenzaKey to prg-chiave.
      *****     read progmag no lock. 
      *****     move prg-chiave       to emro-prg-chiave.
      *****     move prg-peso-utf     to emro-peso-utf.
      *****     move prg-peso-non-utf to emro-peso-non-utf.
      *****
      *****     move "BLISTER" to emro-des-imballo.
      *****     compute emro-qta-GESLUX = emro-qta-EDI * bli-el-qta(idx).
      *****     if idx = 1
      *****        compute emro-qta-imballi = 
      *****                emro-qta-GESLUX / bli-el-qta(idx)
      *****        move emro-qta-imballi to emro-num-colli
      *****     else
      *****        move 0 to emro-qta-imballi emro-num-colli
      *****     end-if.
      *****     move emro-qta-GESLUX to emro-qta.
      *****
      ********---
      ***** ARROTONDA-PRZ-BLISTER.
      *****     add 0,005 to como-numero giving como-prezzo.
      ******    devo moltiplicare il prezzo singolo per la qta presente 
      ******    all'interno del blister
      *****     compute como-prezzo2 = como-prezzo * bli-el-qta(idx)
      *****     add como-prezzo2 to Sum.
      *****
      *****     if idx = LastIdx|sono sull'ultimo
      *****        if Sum not = TotPrzBlister
      *****           if Sum > TotPrzBlister
      *****              compute como-prezzo2 = 
      *****                      como-prezzo2 - (Sum - TotPrzBlister)
      *****           else
      *****
      *****              compute como-prezzo2 = 
      *****                      como-prezzo2 + (TotPrzBlister - Sum)
      *****           end-if
      *****
      ******    devo dividere il prezzo ottenuto per la qta dell'articolo
      ******    all'interno del blister
      *****           compute como-prezzo = como-prezzo2 / bli-el-qta(idx)
      ******    Luciano fine
      *****        end-if
      *****     end-if.

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
           inspect como-riga replacing trailing low-value by spaces.

           if como-riga not = spaces
              move log-riga to lm-riga
              write lm-riga
           end-if.     

      ***---
       CONTATORE-VIDEO.
           if RichiamoBatch          exit paragraph end-if.
           if not RichiamoSchedulato exit paragraph end-if.
           add 1 to counter counter2.

           if counter2 = 50
              move counter to counter-edit
              display counter-edit
                      upon batch-win-handle
                      line 25,00
                    column 38,00
              move 0 to counter2
           end-if.

      ***--
       CLOSE-FILES.
           close EDI-clides clienti destini tparamge EDI-mtordini note
                 param EDI-mrordini articoli progmag timballi timbalqta
                 rpromo tpromo listini ttipocli rmovmag tprov locali 
                 blister cli-prg lockfile tregioni anacap.

      ***---
       EXIT-PGM.
           if path-logfile not = spaces
              if RichiamoSchedulato
                 move path-logfile to batch-log
              end-if

              move spaces to como-riga
              perform SCRIVI-RIGA-LOG

              initialize como-riga
              string "FILES TRATTATI: "   delimited size
                     tot-file             delimited size
                     " - OK: "            delimited size
                     tot-file-ok          delimited size
                     " - NON ELABORATI: " delimited size
                     tot-file-ko          delimited size
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

              initialize como-riga
              string "*** ORDINI TRATTATI: " delimited size
                    tot-ordini               delimited size
                    " di cui "               delimited size
                    tot-ordini-err           delimited size
                    " da verificare ****"    delimited size
               into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

              move spaces to como-riga
              perform SCRIVI-RIGA-LOG

              move 0 to tot-secondi
              accept como-ora from time
              move como-ora(1:2) to hh
              move como-ora(3:2) to mm
              move como-ora(5:2) to ss

              compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss
              compute tot-secondi = end-secondi - start-secondi

              if tot-secondi < 60
                 move tot-secondi to ss
                 initialize como-riga
                 string "ELABORAZIONE TERMINATA IN: ",
                        ss, " SECONDI" delimited size
                        into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              else
                 divide tot-secondi by 60 giving mm remainder ss
                 initialize como-riga
                 string "ELABORAZIONE TERMINATA IN: ",
                         mm, " MINUTI E ", ss, " SECONDI" delimited size
                         into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              end-if
              close logfile

              if RichiamoBatch
                 close log-macrobatch
              end-if

           end-if.

           goback.

      ***---
       UNSTRING-TESTA.
           unstring line-riga delimited by ";"
               into emto-01T-filler        
                    emto-02T-filler        
                    emto-03T-filler        
                    emto-01T04-BGM-DATADOC        
                    emto-01T05-BGM-NUMDOC
                    emto-06T-filler        
                    emto-07T-filler        
                    emto-08T-filler        
                    emto-09T-filler        
                    emto-10T-filler        
                    emto-01T11-DTM-DATACONS
                    emto-12T-filler
                    emto-13T-filler
                    emto-14T-filler        
                    emto-15T-filler        
                    emto-16T-filler        
                    emto-17T-filler        
                    emto-18T-filler        
                    emto-19T-filler        
                    emto-20T-filler        
                    emto-01T21-NAB-CODBUYER
                    emto-01T22-NAB-QCODBUYER
                    emto-01T23-NAB-RAGSOCB        
                    emto-01T24-NAB-INDIRB        
                    emto-01T25-NAB-CITTAB        
                    emto-01T26-NAB-PROVB        
                    emto-01T27-NAB-CAPB        
                    emto-01T28-NAD-CODCONS 
                    emto-29T-filler        
                    emto-01T30-NAD-RAGSOCD        
                    emto-01T31-NAD-INDIRD        
                    emto-01T32-NAD-CITTAD        
                    emto-01T33-NAD-PROVD        
                    emto-01T34-NAD-CAPD        
                    emto-01T35-FTX-NOTE    
                    emto-36T-filler        
                    emto-01T37-BGM-CODAZION        
                    emto-38T-filler        
                    emto-39T-filler        
                    emto-40T-filler        
                    emto-01T41-NAI-CODFATT
                    emto-42T-filler        
                    emto-43T-filler        
                    emto-44T-filler        
                    emto-45T-filler        
                    emto-46T-filler        
                    emto-47T-filler        
                    emto-48T-filler        
                    emto-49T-filler        
                    emto-50T-filler        
                    emto-51T-filler        
                    emto-52T-filler        
                    emto-53T-filler        
                    emto-54T-filler        
                    emto-55T-filler        
                    emto-56T-filler        
                    emto-57T-filler        
                    emto-58T-filler        
                    emto-59T-filler
                    emto-01T60-inversione-imposte
                    emto-01T61-ev-immediata
                    emto-62T-filler        
                    emto-63T-filler        
                    emto-64T-filler        
                    emto-65T-filler        
                    emto-66T-filler        
                    emto-67T-filler        
                    emto-01T68-FTX-NOTE    
                    emto-01T69-FTX-NOTE    
                    emto-01T70-FTX-NOTE    
                    emto-71T-filler        
                    emto-72T-filler        
                    emto-73T-filler        
                    emto-74T-filler        
                    emto-75T-filler        
                    emto-76T-filler        
                    emto-77T-filler        
                    emto-78T-filler        
                    emto-79T-filler        
                    emto-80T-filler        
                    emto-81T-filler        
                    emto-82T-filler        
                    emto-83T-filler        
                    emto-84T-filler        
                    emto-85T-filler        
                    emto-86T-filler        
                    emto-87T-filler        
                    emto-88T-filler        
                    emto-89T-filler        
                    emto-90T-filler        
                    emto-91T-filler        
                    emto-92T-filler        
                    emto-93T-filler        
                    emto-94T-filler        
                    emto-95T-filler        
                    emto-96T-filler        
                    emto-97T-filler        
                    emto-98T-filler        
                    emto-99T-filler        
                    emto-100T-filler       
                    emto-101T-filler       
                    emto-102T-filler       
                    emto-103T-filler       
                    emto-104T-filler       
                    emto-105T-filler       
                    emto-106T-filler       
                    emto-107T-filler       
                    emto-108T-filler       
                    emto-109T-filler       
                    emto-110T-filler       
                    emto-111T-filler       
                    emto-112T-filler       
                    emto-113T-filler       
                    emto-114T-filler       
                    emto-115T-filler       
                    emto-116T-filler       
                    emto-117T-filler       
                    emto-118T-filler       
                    emto-119T-filler       
                    emto-120T-filler       
                    emto-121T-filler       
                    emto-122T-filler       
                    emto-123T-filler       
                    emto-124T-filler       
                    emto-125T-filler       
                    emto-126T-filler       
                    emto-127T-filler       
                    emto-128T-filler       
                    emto-129T-filler       
                    emto-130T-filler       
                    emto-131T-filler       
                    emto-132T-filler       
                    emto-133T-filler       
                    emto-134T-filler       
                    emto-135T-filler       
                    emto-136T-filler       
                    emto-137T-filler       
                    emto-138T-filler       
                    emto-139T-filler       
                    emto-140T-filler       
                    emto-141T-filler       
                    emto-142T-filler       
                    emto-143T-filler       
                    emto-144T-filler       
                    emto-145T-filler       
                    emto-146T-filler       
                    emto-147T-filler       
                    emto-148T-filler       
                    emto-149T-filler       
                    emto-150T-filler       
                    emto-151T-filler       
                    emto-152T-filler       
                    emto-153T-filler       
                    emto-154T-filler       
                    emto-155T-filler       
                    emto-156T-filler       
                    emto-157T-filler       
                    emto-158T-filler       
                    emto-159T-filler       
                    emto-160T-filler       
                    emto-161T-filler       
                    emto-162T-filler       
                    emto-01T163-TOD-CODCOST 
           end-unstring.

      ***---
       UNSTRING-RIGA.
           unstring line-riga delimited by ";"
               into emro-01D-filler
                    emro-02D-filler
                    emro-03D-filler
                    emro-04D-filler
                    emro-05D-filler
                    emro-06D-filler
                    emro-07D-filler
                    emro-08D-filler
                    emro-09D-filler
                    emro-10D-filler
                    emro-11D-filler
                    emro-12D-filler
                    emro-02D13-LIN-CODFORTU
                    emro-02D14-LIN-CODDISTU
                    emro-02D15-LIN-DESART
                    emro-16D-filler
                    emro-02D17-QTAORD
                    emro-18D-filler
                    emro-02D19-LIN-PRZUNI
                    emro-20D-filler
                    emro-21D-filler
                    emro-02D22-LIN-NRCUINTU
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |3
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |4
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |5
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |6
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |7
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |8
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |9
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |0
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |1
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |2
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |3
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |4
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |5
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |6
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |7
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |8
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |9
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |0
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |1
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx
                    02D-fillerx        |2
                    02D-fillerx     
                    emro-02D222-QTAPZ
           end-unstring.

      ***---
       RECUPERA-DATI-FROM-RMOVMAG.
           set  trovato-movim  to false.
           |DEVO FARE POI IL RECUPERO DA LISTINO PER
           |CUI NON M'INTERESSA L'ULTIMO MOVIMENTO
LABLAB     if tcl-si-recupero  exit paragraph end-if.
           move low-value      to rmo-rec.
           set  rmo-cliente    to true.
           move cli-codice     to rmo-cod-clifor.
           move "AAAA"         to rmo-causale.
           move art-codice     to rmo-articolo.
           move high-value     to rmo-data-movim.
           start rmovmag key   is < rmo-chiave-ricerca
                 invalid continue
             not invalid
                 read rmovmag previous at end continue end-read
                 if rmo-cod-clifor = cli-codice  and
                    rmo-causale    = "AAAA"      and
                    rmo-articolo   = art-codice  and
LUBEXX              rmo-qta    not = 0
                    set trovato-movim to true
LUBEXX              if ttipocli-gdo
LUBEXX                 compute emro-prz-GESLUX = 
LUBEXX                         rmo-netto + rmo-coubat + rmo-imp-cons
LUBEXX              else
LUBEXX                 move rmo-netto    to emro-prz-GESLUX
LUBEXX              end-if
                 end-if
           end-start.

      ***---
       RECUPERA-SOLO-PROMO. 
           if emro-si-blister
              move bli-codice to art-codice
           end-if.
           set emro-bloccato-prezzo-no to true.
           set prezzo-sp    to false.
LABLAB     if tcl-si-recupero
              set promo-future to false  
              perform CERCA-PROMO-LISTINO
              if trovato
                 if not prezzo-sp
                    move rpr-codice      to emro-promo
                 end-if
              end-if
              if trovato set trovato-promo to true
              else       set trovato-promo to false
              end-if
           end-if.

      ***---
       RECUPERA-PREZZO. 
           if emro-si-blister
              move bli-codice to art-codice
           end-if.
           set emro-bloccato-prezzo-no to true.
           set prezzo-sp    to false.
LABLAB     if tcl-si-recupero
              set promo-future to false  
              perform CERCA-PROMO-LISTINO
              if not trovato
                 move 0 to emro-prz-commle
                 move 9999999,99              to emro-prz-GESLUX 
                 set  emro-bloccato-prezzo-si to true
              else
                 if not prezzo-sp
                    move rpr-codice      to emro-promo
                 end-if
                 move rpr-prz-acq        to emro-prz-GESLUX
                                            emro-prz-commle
                 move lst-cod-art-cli    to emro-cod-art-cli
              end-if
              if trovato set trovato-promo to true
              else       set trovato-promo to false
              end-if
           else
              if trovato-movim
LUBEXX           if ttipocli-gdo
LUBEXX              compute emro-prz-GESLUX = 
LUBEXX                      rmo-netto + rmo-coubat + rmo-imp-cons
LUBEXX           else
LUBEXX              move rmo-netto    to emro-prz-GESLUX
LUBEXX           end-if
              else
                 move 9999999,99 to emro-prz-GESLUX
                 set  emro-bloccato-prezzo-si to true
              end-if
           end-if.

LABLAB***---
       CERCA-PROMO-LISTINO.
           set trovato      to false.

           |Provo con la promo locale
           if emto-prg-destino not = 0
              if promo-future
                 move cli-gdo          to loc-gdo
                 move cli-codice       to loc-cliente
                 move emto-prg-destino to loc-destino
                 move 0                to loc-ini-dpo
                 move low-value        to loc-fine-dpo
                 start locali key >= loc-chiave-ricerca
                       invalid set record-ok to false
                   not invalid set record-ok to true
                 end-start
              else
                 move cli-gdo          to loc-gdo
                 move cli-codice       to loc-cliente
                 move emto-prg-destino to loc-destino
                 move emto-data-ordine to loc-fine-dpo
                 move low-value        to loc-ini-dpo
                 start locali key >= loc-chiave-gdo-fine
                       invalid set record-ok to false
                   not invalid set record-ok to true
                 end-start
              end-if
              if record-ok
                 perform until 1 = 2
                    read locali next at end exit perform end-read
                    if loc-gdo     not = cli-gdo    or
                       loc-cliente not = cli-codice or
                       loc-destino not = emto-prg-destino
                       exit perform
                    end-if

                    if ( loc-fine-dpo >= emto-data-ordine  and
                         loc-ini-dpo  <= emto-data-ordine ) or
                       ( promo-future and 
                         loc-fine-dpo >= emto-data-ordine )

                       move loc-codice to tpr-codice
                       read tpromo no lock
                            invalid continue
                        not invalid
                            if tpr-gdo      = loc-gdo     and
                               tpr-ini-dpo  = loc-ini-dpo and
                               tpr-fine-dpo = loc-fine-dpo
                               move tpr-codice to rpr-codice
                               move art-codice to rpr-articolo
                               read rpromo no lock
                                    invalid continue
                                not invalid
                                    set trovato to true
                                    move rpr-codice  to tprz-codice
                                    move rpr-prz-acq to tprz-prz-acq
                               end-read
                            end-if
                       end-read
                    end-if
                 end-perform
              end-if
           end-if.

           |PROMO GDO
           if not trovato
              if promo-future
                 move cli-gdo         to tpr-gdo
                 move 0               to tpr-ini-dpo
                 move low-value       to tpr-fine-dpo
                 start tpromo  key >= tpr-chiave-ricerca
                       invalid set record-ok to false
                   not invalid set record-ok to true
                 end-start
              else
                 move cli-gdo          to tpr-gdo
                 move emto-data-ordine to tpr-fine-dpo
                 move low-value        to tpr-ini-dpo
                 start tpromo  key >= tpr-chiave-gdo-fine
                       invalid set record-ok to false
                   not invalid set record-ok to true
                 end-start
              end-if

              if record-ok
                 perform until 1 = 2
                    read tpromo next at end exit perform end-read
                    if tpr-gdo not = cli-gdo
                       exit perform
                    end-if

                    if ( tpr-fine-dpo >= emto-data-ordine  and
                         tpr-ini-dpo  <= emto-data-ordine ) or
                       ( promo-future and 
                         tpr-fine-dpo >= emto-data-ordine )

                       if tpr-nazionale
                          move tpr-codice to rpr-codice
                          move art-codice to rpr-articolo
                          read rpromo no lock 
                               invalid continue
                           not invalid
                               set trovato to true
                               move rpr-codice  to tprz-codice
                               move rpr-prz-acq to tprz-prz-acq
                          end-read
                       end-if
                    end-if
                 end-perform
              end-if

           end-if.

           if not trovato 
              if not promo-future
                 move 0                to rpr-codice
                 move cli-gdo          to lst-gdo
                 move emto-data-ordine to lst-data
                 move art-codice       to lst-articolo
                 start listini key <= lst-k-articolo
                       invalid continue
                   not invalid
                       read listini previous
                       if lst-gdo      = cli-gdo          and
                          lst-data    <= emto-data-ordine and
                          lst-articolo = art-codice
                          |In caso di "FA" non cerco promo 
                          |né successive né precedenti
                          if lst-prezzo >= 999999,99
                             set emro-bloccato-prezzo-si to true
                             set trovato      to false
                          else
                             if lst-prezzo not = 0
                                set trovato to true
                                move lst-prezzo   to rpr-prz-acq
                                set emro-bloccato-prezzo-no to true
                             else
                                set prezzo-sp to true
                                set emro-bloccato-prezzo-si to true
                                |CASO "SP"
                                |1. Cerco la promo immediatamente dopo
                                perform CERCA-PROMO-DOPO
                                if not trovato
                                   |2. Cerco la promo immediatamente prima
                                   perform CERCA-PROMO-PRIMA
                                end-if
                             end-if
                          end-if
                       end-if
                 end-start
              end-if
           else
              move tprz-codice  to rpr-codice
              move tprz-prz-acq to rpr-prz-acq
              set trovato to true
           end-if.

      ***---
       CERCA-PROMO-DOPO.
           if emto-prg-destino not = 0
              move cli-gdo         to loc-gdo
              move cli-codice      to loc-cliente
              move emto-prg-destino to loc-destino
              move emto-data-ordine to loc-ini-dpo
              move low-value       to loc-fine-dpo
              start locali key >= loc-chiave-ricerca
                    invalid set record-ok to false
                not invalid set record-ok to true
              end-start

              if record-ok
                 perform until 1 = 2
                    read locali next at end exit perform end-read
                    if loc-gdo      not =  cli-gdo         or
                       loc-cliente  not =  cli-codice      or
                       loc-destino  not =  emto-prg-destino
                       exit perform
                    end-if

                    if loc-ini-dpo  >= emto-data-ordine and
                       loc-fine-dpo >= emto-data-ordine
                       move loc-codice to tpr-codice
                       read tpromo no lock
                            invalid continue
                        not invalid
                            if tpr-gdo      = loc-gdo     and
                               tpr-ini-dpo  = loc-ini-dpo and
                               tpr-fine-dpo = loc-fine-dpo
                               move tpr-codice to rpr-codice
                               move art-codice to rpr-articolo
                               read rpromo no lock 
                                    invalid continue
                                not invalid set trovato to true
                                            exit perform
                               end-read
                            end-if
                       end-read
                    end-if

                 end-perform
              end-if
           end-if.

           if not trovato
              move cli-gdo          to tpr-gdo
              move emto-data-ordine to tpr-ini-dpo
              move low-value        to tpr-fine-dpo
              start tpromo key >= tpr-chiave-ricerca
                    invalid set record-ok to false
                not invalid set record-ok to true
              end-start

              if record-ok
                 perform until 1 = 2
                    read tpromo next at end exit perform end-read
                    if tpr-gdo not = cli-gdo
                       exit perform
                    end-if

                    if tpr-fine-dpo > emto-data-ordine and
                       tpr-ini-dpo  > emto-data-ordine
                       move tpr-codice to rpr-codice
                       move art-codice to rpr-articolo
                       read rpromo no lock 
                            invalid continue
                        not invalid set trovato to true
                                    exit perform
                       end-read
                    end-if
                 end-perform
              end-if
           end-if.                   

      ***---
       CERCA-PROMO-PRIMA.
           if emto-prg-destino not = 0
              move cli-gdo          to loc-gdo
              move cli-codice       to loc-cliente
              move emto-prg-destino to loc-destino
              move emto-data-ordine to loc-fine-dpo
              move low-value        to loc-ini-dpo
              start locali key <= loc-chiave-gdo-fine
                    invalid set record-ok to false
                not invalid set record-ok to true
              end-start

              if record-ok
                 perform until 1 = 2
                    read locali previous at end exit perform end-read
                    if loc-gdo     not =  cli-gdo      or
                       loc-cliente not =  cli-codice   or
                       loc-destino not =  emto-prg-destino
                       exit perform
                    end-if

                    if loc-ini-dpo  >= emto-data-ordine and
                       loc-fine-dpo >= emto-data-ordine
                       move loc-codice to tpr-codice
                       read tpromo no lock
                            invalid continue
                        not invalid
                            if tpr-gdo      = loc-gdo     and
                               tpr-ini-dpo  = loc-ini-dpo and
                               tpr-fine-dpo = loc-fine-dpo
                               move tpr-codice to rpr-codice
                               move art-codice to rpr-articolo
                               read rpromo no lock 
                                    invalid continue
                                not invalid set trovato to true
                                            exit perform
                               end-read
                            end-if
                       end-read
                    end-if
                 end-perform
              end-if
           end-if.

           if not trovato
              move cli-gdo          to tpr-gdo
              move emto-data-ordine to tpr-fine-dpo
              move low-value        to tpr-ini-dpo
              start tpromo key <= tpr-chiave-gdo-fine
                    invalid set record-ok to false
                not invalid set record-ok to true
              end-start

              if record-ok
                 perform until 1 = 2
                    read tpromo previous at end exit perform end-read
                    if tpr-gdo not = cli-gdo
                       exit perform
                    end-if
                    if tpr-fine-dpo < emto-data-ordine and
                       tpr-ini-dpo  < emto-data-ordine
                       move tpr-codice to rpr-codice
                       move art-codice to rpr-articolo
                       read rpromo no lock 
                            invalid continue
                        not invalid set trovato to true
                                    exit perform
                       end-read
                    end-if
                 end-perform
              end-if

           end-if.                 
            
      ***---
       TROVA-LISTINO.
           initialize como-prg-chiave replacing numeric data by zeroes
                                           alphanumeric data by spaces.
           set no-prg-listino to true.
      *****     if cli-gdo = space
           if tcl-gdo-no
              exit paragraph
           end-if.

           move cli-gdo          to lst-gdo.
           move emto-data-ordine to lst-data.
           move art-codice       to lst-articolo.
           start listini key <= lst-k-articolo
                 invalid continue
             not invalid
                 read listini previous
                 if lst-gdo      = cli-gdo          and
                    lst-data    <= emto-data-ordine and
                    lst-articolo = art-codice

                    if lst-prg-cod-articolo = space
                       move zero   to lst-prg-cod-articolo
                    end-if

                    if lst-prg-cod-articolo not = zero
                       set si-prg-listino   to true
                    end-if

                 end-if
           end-start.

           if si-prg-listino
              move lst-prg-chiave  to prg-chiave
              read progmag no lock
                   invalid set emro-progressivo-non-forzato to true
               not invalid
                   if prg-bloccato or prg-disattivo or
                      prg-cod-magazzino not = "LBX"
                      move 0 to como-prg-cod-articolo
                   else
                      move lst-prg-chiave  to como-prg-chiave
                   end-if
              end-read
           end-if.

      ***---
       TROVA-CLI-PRG.
           set  cp-tipo-C        to true.
           move cli-codice       to cp-clifor.
           move art-codice       to cp-articolo.
           read cli-prg no lock
                invalid continue
            not invalid
                set si-prg-listino   to true
                read progmag no lock
                     invalid set emro-progressivo-non-forzato to true
                 not invalid
                     if prg-bloccato or prg-disattivo or
                        prg-cod-magazzino not = "LBX"
                        move 0 to como-prg-cod-articolo
                     else
                        move cp-prg-chiave  to como-prg-chiave
                     end-if
                end-read
           end-read.

      ***---
       RECUPERA-APPLICA-VERIFICA-PREZZI-TRADIZIONALE.
           if emto-righe-non-presenti
              exit paragraph
           end-if.

           set VerificaPrezziTradizionale to false.
      *****     move ef-cau-buf to tca-codice.
      *****     read tcaumag no lock.
      *****     if tca-si-zero
      *****        exit paragraph 
      *****     end-if.
           read EDI-mtordini.
           |SOLO PER AVERE IL TOTALE DEI COLLI
           move 0 to tot-colli.
           move emto-chiave to emro-chiave-testa.
           move low-value to emro-riga.
           start edi-mrordini key >= emro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read edi-mrordini next at end exit perform end-read
                    if not emro-si-blister
                       compute como-qta = emro-qta / emro-qta-imballi
BLISTR              else
BLISTR                 move emro-qta-imballi to como-qta
BLISTR              end-if
                    add como-qta to tot-colli
                 end-perform
           end-start.

           accept wstampa2   from environment "PATH_RECUPERO".
           accept como-data  from century-date.
           accept como-ora   from time.
           inspect wstampa2  replacing trailing spaces by low-value.
           string  wstampa2  delimited low-value
                   como-data delimited size
                   "_"       delimited size
                   como-ora  delimited size
                   ".txt"    delimited size
                   into wstampa2
           end-string.
           inspect wstampa2  replacing trailing low-value by spaces.
           open output lineseq2.

           move emto-chiave to emro-chiave-testa.
           move low-value to emro-riga.
           start edi-mrordini key >= emro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read edi-mrordini next at end exit perform end-read
                    initialize r-rec replacing numeric data by zeroes
                                          alphanumeric data by spaces
                    move emro-prg-chiave to prg-chiave
BLISTR              if not emro-si-blister
                       compute como-qta = emro-qta / emro-qta-imballi
                       move prg-cod-articolo to r-blister
BLISTR              else
BLISTR                 move emro-qta-imballi to como-qta
                       move emro-bli-codice  to r-blister
                       move emro-bli-perce   to r-perce-bli
BLISTR              end-if
                    inspect r-blister replacing leading x"30" by x"20"

                    move prg-cod-articolo to r-articolo
                    inspect r-articolo replacing leading x"30" by x"20"
                    move prg-peso to r-peso
                    inspect r-peso replacing leading x"30" by x"20"
                    move como-qta      to r-colli

                    move emto-cod-cli  to r-cod-cli
                    move emto-prg-destino  to r-destino
                    inspect r-cod-cli  replacing leading x"30" by x"20"
                    inspect r-destino  replacing leading x"30" by x"20"
                    move "X" to r-filler
                    move emto-data-ordine  to r-data
                    move "N" to r-ritiro
              
                    move tot-colli to r-tot-colli

                    move "X" to r-prz-promo
              
                    move r-rec     to line-riga2 of lineseq2
                    write line-riga2             of lineseq2
                 end-perform
           end-start.
           |DEMARCATORE EOF RICHIESTO DA WALTER
           move "XXX" to line-riga2 of lineseq2.
           write line-riga2         of lineseq2.
           close lineseq2.

           initialize comando.
           accept  comando  from environment "RECUPERO_EXE".
           inspect comando  replacing trailing spaces by low-value.
           string  comando  delimited low-value
                   " "      delimited size
                   wstampa2 delimited size
                   into comando
           end-string.

           move 0 to status-call.
           call "C$SYSTEM" using comando, 32
                          giving status-call.

           if status-call = 0
              perform APPLICA-PREZZI
           end-if.
        
      ***---
       APPLICA-PREZZI.
           move 1 to riga.
           open input lineseq2.
           set blocco-prezzo to false.
           perform until 1 = 2
              read lineseq2 next at end exit perform end-read
              |DEMARCATORE EOF RICHIESTO DA WALTER
              if line-riga2 of lineseq2 = "XXX"
                 exit perform
              end-if

              move line-riga2 of lineseq2 to r-rec

              inspect r-cod-cli     replacing leading x"20" by x"30"
              inspect r-destino     replacing leading x"20" by x"30"
              inspect r-blister     replacing leading x"20" by x"30"
              inspect r-articolo    replacing leading x"20" by x"30"
              inspect r-peso        replacing leading x"20" by x"30"
              inspect r-colli       replacing leading x"20" by x"30"
              inspect r-tot-colli   replacing leading x"20" by x"30"
              inspect r-i-consumo   replacing leading x"20" by x"30"
              inspect r-i-cou-cobat replacing leading x"20" by x"30"
              inspect r-perce-pb    replacing leading x"20" by x"30"
              inspect r-perce-bli   replacing leading x"20" by x"30"
              inspect r-prezzo      replacing leading x"20" by x"30"

              move emto-chiave to emro-chiave-testa
              move riga        to emro-riga
              read edi-mrordini

              move r-prezzo     to emro-prz-GESLUX
              move r-prz-promo  to emro-prz-promo convert

              if emro-prz-GESLUX > 9999999
                 set emro-bloccato-prezzo-si to true
              else
                 set emro-bloccato-prezzo-no to true
              end-if

              if emro-prz-GESLUX not = emro-prz-EDI
                 set emro-prezzo-non-valido to true
                 set emro-bloccato to true

                 set blocco-prezzo to true
              else                                 
                 set emro-prezzo-valido to true        
                 move emro-prz-GESLUX to emro-prz
                 |se era bloccata SOLO per prezzo la attivo
                 if emro-articolo-valido and
                    emro-qtac-ok         and
                    emro-progressivo-valido
                    set emro-attivo to true
                 end-if
              end-if

              rewrite emro-rec
              add 1 to riga
           end-perform.
           close lineseq2.
           delete file lineseq2.

           |se i prezzi sono tutti corretti e l'ordine era
           |bloccato SOLO per il prezzo riattivo l'ordine
           if not blocco-prezzo
              if emto-cliente-valido  and 
                 emto-cliente-fido-ok and
                 emto-destino-valido  and
                 emto-righe-presenti  and
                 emto-qta-ok          and 
                 emto-art-ok          and
                 emto-prg-ok          and
                 emto-esistente-si
                 set emto-attivo to true
                 set emto-prz-ok to true
                 rewrite emto-rec
              end-if
           end-if.

      ***---
       VETTORE-PROV-REG.
           |CERCO PER PROVINCIA
           move como-prov to prv-codice.
           read tprov no lock
                invalid continue
            not invalid
                if prv-vet-5 > 0
                   move prv-vet-5 to emto-vettore
                end-if
                if prv-vet-4 > 0
                   move prv-vet-4 to emto-vettore
                end-if
                if prv-vet-3 > 0
                   move prv-vet-3 to emto-vettore
                end-if
                if prv-vet-2 > 0
                   move prv-vet-2 to emto-vettore
                end-if
                if prv-vet-1 > 0
                   move prv-vet-1 to emto-vettore
                end-if
                if emto-vettore = 0
                  |CERCO PER REGIONE
                   move prv-regione to reg-codice
                   read tregioni no lock
                        invalid continue
                    not invalid
                        if reg-vet-5 > 0
                           move reg-vet-5 to emto-vettore
                        end-if
                        if reg-vet-4 > 0
                           move reg-vet-4 to emto-vettore
                        end-if
                        if reg-vet-3 > 0
                           move reg-vet-3 to emto-vettore
                        end-if
                        if reg-vet-2 > 0
                           move reg-vet-2 to emto-vettore
                        end-if
                        if reg-vet-1 > 0
                           move reg-vet-1 to emto-vettore
                        end-if
                   end-read
                end-if
           end-read.

      ***---
       PARAGRAFO-COPY.
           copy "trova-parametro.cpy".
           copy "tratta-numerico.cpy".
           copy "setta-inizio-riga.cpy".
