       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mail-giacenze-new.
       AUTHOR.                          Andrea.
       REMARKS. Programma batch da lanciare la notte senza linkage che
                genera un csv contenente i progmag LBX con giacenza < 0.
                NUOVA VERSIONE CON SCORTA DI SICUREZZA

                NON PIU USATO!!!!!

                RIPRISTINATO!!
                parametrizzati i magazzini

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "ordfor.sl".
           copy "ordfor2.sl".
           copy "lineseq.sl".
           copy "progmag.sl".
           copy "articoli.sl".
           copy "timballi.sl".
           copy "timbalqta.sl".
           copy "tmagaz.sl".
           copy "tmp-progmag.sl".
           copy "tmarche.sl".
           copy "timposte.sl".
           copy "tparamge.sl".
           copy "tscorte.sl".
           copy "tordforn.sl".
           copy "rordforn.sl".
           copy "sordforn.sl".
           copy "tmp-arrivi-prese.sl".
           copy "clienti.sl".
           copy "promoeva.sl".
           copy "lineseq-mail.sl".

       SELECT logfile
           ASSIGN       TO  path-logfile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.      
           
      * contiene i soli progmag che hanno LBX come articolo STD
       SELECT tmplbx-progmag
           ASSIGN       TO path-tmplbx-progmag
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmplbx-progmag
           RECORD KEY   IS tmplbx-prg-chiave
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.         
           copy "tsetinvio.fd".
           copy "ordfor.fd".
           copy "ordfor2.fd".
           copy "lineseq.fd".
           copy "progmag.fd".
           copy "articoli.fd".
           copy "timballi.fd".
           copy "timbalqta.fd".
           copy "tmagaz.fd".
           copy "tmp-progmag.fd".
           copy "tmarche.fd".
           copy "timposte.fd".
           copy "tparamge.fd".
           copy "tscorte.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "sordforn.fd".
           copy "tmp-arrivi-prese.fd".
           copy "clienti.fd".
           copy "promoeva.fd". 
           copy "lineseq-mail.fd".
           
       FD  tmplbx-progmag.
       01 tmplbx-rec.
           05 tmplbx-prg-chiave.
               10 tmplbx-prg-cod-articolo PIC  9(6).
               10 tmplbx-prg-cod-magazzino            PIC  X(3).
               10 tmplbx-prg-tipo-imballo PIC  X(3).
               10 tmplbx-prg-peso         PIC  9(5)v9(3).

       FD  logfile.
       01 log-riga        PIC  x(900).  

       WORKING-STORAGE SECTION.
           copy "mail.def".
           copy "costo-medio.def".
           copy "imposte.def".

      * FILE STATUS
       77  path-tmplbx-progmag        pic x(256).
       77  status-tmplbx-progmag      pic xx.
       77  status-logfile             pic xx.
       77  status-ordfor              pic xx.
       77  status-ordfor2             pic xx.
       77  status-lineseq             pic xx.
       77  status-tsetinvio           pic xx.
       77  status-progmag             pic xx.
       77  status-articoli            pic xx.
       77  status-timballi            pic xx.
       77  status-timbalqta           pic xx.
       77  status-tmagaz              pic xx.
       77  status-tmp-progmag         pic xx.
       77  status-tmarche             pic xx.
       77  status-timposte            pic xx.
       77  status-tparamge            pic xx.
       77  status-tscorte             pic xx.
       77  status-tordforn            pic xx.
       77  status-rordforn            pic xx.
       77  status-sordforn            pic xx.
       77  status-tmp-arrivi-prese    pic xx.
       77  status-clienti             pic xx.
       77  status-promoeva            pic xx.
       77  status-lineseq-mail        pic xx.
       77  path-lineseq-mail          pic x(256).

       77  wstampa                    pic x(256).
       77  path-tmp-progmag           pic x(256).
       77  path-tmp-arrivi-prese      pic x(256).
       77  path-logfile               pic x(256).

      * VARIABILI                        
              
       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.  

       77  counter               pic 9(9) value 0.
       77  counter2              pic 9(9) value 0.
       77  counter-edit          pic zzz.zzz.zz9.

       77  tit-scorte                 pic x(50).
       01  r-inizio              pic x(25).

       77  como-riga                  pic x(80).
       77  riga-comodo                pic x(1000).

       77  path-backup                pic x(256).
       77  path-ini                   pic x(256).
       77  path-csv                   pic x(256).
       77  path-log-source            pic x(256).
       77  path-log-dest              pic x(256).

       77  copy-status                signed-short.

       77  start-char                 pic 9(3).
       77  idx-tot-scorte             pic 9(3) value 0.
       01  tab-scorte.
           05 el-sco                  pic 9(2)
                                      occurs 100 times
                                      indexed  idx-sco.
                                                       
       77  como-idx-mag               pic 9(3) value 0.
       77  idx-tot-mag                pic 9(3) value 0.
       01  tab-mag.
         03 el-mag occurs 100 times indexed by idx-mag.
           05 el-CodiceMag               pic x(3).
           05 el-GiacenzaMag             pic s9(8). 
           05 el-GiacenzaMag-ed          pic --.---.--9.
           05 el-ImpegnatoMag            pic s9(8).
           05 el-OrdinatoMag             pic s9(8).     
           05 el-OrdinatoMag-ed          pic --.---.--9.
           05 el-PalletMag               pic s9(8).
           05 el-PalletMag-ed            pic --.---.--9.

       77  giacenza                   pic s9(8).
       77  como-data                  pic 9(8).
       77  como-ora                   pic 9(8).
       77  qta-edit                   pic z.zzz.
       77  giacenza-ed                pic --.---.--9.
       77  imballi-ed                 pic --.---.--9,9.
       77  peso-ed                    pic zz9,999.
       77  start-data                 pic 9(8).
       77  end-data                   pic 9(8).
       77  media-anno                 pic S9(9).
       77  mese-start                 pic 99.
       77  mese-end                   pic 99.
       77  anno-corr                  pic 9(4).
       77  anno-past                  pic 9(4).
       77  mese                       pic 99.
       77  idx                        pic 9(3).
       77  como-qta                   pic 9(8).
       77  num-forn                   pic 9(3).
       77  codice-x                   pic x(6).
       77  fornitore-ed               pic x(50).
       77  data-ed                    pic x(10).
       01  occurs-qta.
         03 el-qta                    pic s9(9) occurs 12.
       77  wk-campo                   pic s9(12)v99.
       77  mesi-utili                 pic 99.
       77  SS1                        pic s9(10).
      ***** 77  SS2                        pic s9(10).

       77  qta-prenotata              pic s9(8).
       77  giac-utile                 pic s9(8).
       77  giac-utile-ed              pic ---.---.--9.
       77  mancante-ed                pic ---.---.--9.
       77  comodo                     pic s9(8)v99.
       77  resto                      pic 9(3).
       77  tot-giac-altri             pic s9(8).  

       77  MagazzinoPrincipale        pic x(3). 
       77  nargs                      pic 99  comp-1 value 0.

      * FLAGS                                

       01  filler                     pic 9 value 0.
           88 RichiamoSchedulato      value 1, false 0.

       01  controlli                  pic xx.
         88 errori                    value "ER".
         88 tutto-ok                  value "OK".

       01  filler                     pic 9.
         88 RecLocked                 value 1, false 0.

       01  filler                     pic 9.
         88 trovato                   value 1, false 0.

       01  filler                     pic 9.
         88 trovato-giac              value 1, false 0.

       01  filler                     pic 9.
         88 trovato-prese             value 1, false 0.

       01  filler                     pic 9.
         88 CreatoFile                value 1, false 0.

       01  filler                     pic 9.
         88 prima-volta               value 1, false 0.
                                                       
       01  filler                     pic 9.
         88 ExitPerform               value 1, false 0.
                                                       
       01  filler                     pic 9.
         88 InVariMagazzini           value 1, false 0.

       LINKAGE SECTION.
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.

      ***---
       TMPLBX-PROGMAG-ERR SECTION.
           use after error procedure on tmplbx-progmag.
           set tutto-ok  to true.
           evaluate status-tmplbx-progmag
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                   delimited size
                       "[TMPLBX-PROGMAG] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                       delimited size
                       "[TMPLBX-PROGMAG] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "[TMPLBX-PROGMAG] Indexed corrupt!"delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.  

      ***---
       TMP-PROGMAG-ERR SECTION.
           use after error procedure on tmp-progmag.
           set tutto-ok  to true.
           evaluate status-tmp-progmag
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                   delimited size
                       "[TMP-PROGMAG] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                       delimited size
                       "[TMP-PROGMAG] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "[TMP-PROGMAG] Indexed corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.    

      ***---
       LOGFILE-ERR SECTION.
           use after error procedure on logfile.
           evaluate status-logfile
           when "00"
           when other continue
           end-evaluate. 

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                    delimited size
                       "File [LINESEQ] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                        delimited size
                       "File [LINESEQ] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                          delimited size
                       "[LINESEQ] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [PROGMAG] not found!"  delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                        delimited size
                       "File [PROGMAG] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                          delimited size
                       "[PROGMAG] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.
 
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [ARTICOLI] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "File [ARTICOLI] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                           delimited size
                       "[ARTICOLI] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.
 
      ***---
       TIMBALLI-ERR SECTION.
           use after error procedure on timballi.
           set tutto-ok  to true.
           evaluate status-timballi
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [TIMBALLI] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "File [TIMBALLI] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                           delimited size
                       "[TIMBALLI] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.

      ***---
       TIMBALQTA-ERR SECTION.
           use after error procedure on timbalqta.
           set tutto-ok  to true.
           evaluate status-timbalqta
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                      delimited size
                       "File [TIMBALQTA] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                          delimited size
                       "File [TIMBALQTA] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                           delimited size
                       "[TIMBALQTA] Indexed file corrupt!"delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.
 
      ***---
       TMAGAZ-ERR SECTION.
           use after error procedure on tmagaz.
           set tutto-ok  to true.
           evaluate status-tmagaz
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                   delimited size
                       "File [TMAGAZ] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                       delimited size
                       "File [TMAGAZ] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                         delimited size
                       "[TMAGAZ] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.

      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                    delimited size
                       "File [TMARCHE] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                        delimited size
                       "File [TMARCHE] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                          delimited size
                       "[TMARCHE] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.

      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           set tutto-ok  to true.
           evaluate status-timposte
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [TIMPOSTE] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "File [TIMPOSTE] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                           delimited size
                       "[TIMPOSTE] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.
 
      ***---
       TSCORTE-ERR SECTION.
           use after error procedure on tscorte.
           set tutto-ok  to true.
           evaluate status-tscorte
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [TSCORTE] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "File [TSCORTE] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                          delimited size
                       "[TSCORTE] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.

      ***---
       TORDFORN-ERR SECTION.
           use after error procedure on tordforn.
           set tutto-ok  to true.
           evaluate status-tordforn
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [TORDFORN] not found!"  delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                        delimited size
                       "File [TORDFORN] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                          delimited size
                       "[TORDFORN] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.

      ***---
       RORDFORN-ERR SECTION.
           use after error procedure on rordforn.
           set tutto-ok  to true.
           evaluate status-rordforn
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [RORDFORN] not found!"  delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                        delimited size
                       "File [RORDFORN] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                          delimited size
                       "[RORDFORN] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.

      ***---
       SORDFORN-ERR SECTION.
           use after error procedure on sordforn.
           set tutto-ok  to true.
           evaluate status-sordforn
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [SORDFORN] not found!"  delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                        delimited size
                       "File [SORDFORN] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                          delimited size
                       "[SORDFORN] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           end-evaluate.
 
      ***---
       PROMOEVA-ERR SECTION.
           use after error procedure on promoeva.
           set tutto-ok  to true.
           evaluate status-promoeva
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [PROMOEVA] not found!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "File [PROMOEVA] Mismatch size!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                           delimited size
                       "[PROMOEVA] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                set errori to true
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
       CONTATORE-VIDEO.
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

      ***---
       INIT.
           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.
           if RichiamoSchedulato    
              move -1 to batch-status  
              accept como-ora  from time

              move como-ora(1:2) to hh
              move como-ora(3:2) to mm
              move como-ora(5:2) to ss
     
              compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss

              initialize path-logfile
              accept como-data from century-date
              accept como-ora  from time
              accept  path-logfile from environment "SCHEDULER_PATH_LOG"
              inspect path-logfile 
                      replacing trailing spaces by low-value
              string  path-logfile      delimited low-value
                      "mail-GiaCENZE-NEW_"  delimited size
                      como-data             delimited size
                      "_"                   delimited size
                      como-ora              delimited size
                      ".log"                delimited size
                      into path-logfile
              end-string
              open output logfile
           end-if.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio              delimited size
                  "INIZIO ELABORAZIONE" delimited size
                  into como-riga
           end-string.      
           perform SCRIVI-RIGA-LOG.

           set CreatoFile  to false.
           set prima-volta to true.
           set tutto-ok    to true.
           initialize wstampa.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  wstampa   from environment "PATH_ST".
           inspect wstampa   replacing trailing spaces by low-value.
           string  wstampa          delimited low-value
                   "giacenze_"      delimited size
                   como-data        delimited size
                   ".csv"           delimited size
                   into wstampa
           end-string.

           initialize path-tmp-progmag.
           accept  path-tmp-progmag from environment "PATH_ST".
           inspect path-tmp-progmag replacing trailing
                                    spaces by low-value.
           string  path-tmp-progmag delimited low-value
                   "tmp-progmag_"   delimited size
                   como-data        delimited size
                   "_"              delimited size
                   como-ora         delimited size
                   ".tmp"           delimited size
                   into path-tmp-progmag
           end-string.

           initialize path-tmp-arrivi-prese.
           accept  path-tmp-arrivi-prese from environment "PATH_ST".
           inspect path-tmp-arrivi-prese replacing trailing
                                         spaces by low-value.
           string  path-tmp-arrivi-prese delimited low-value
                   "tmp-arrivi-prese_"   delimited size
                   como-data             delimited size
                   "_"                   delimited size
                   como-ora              delimited size
                   ".tmp"                delimited size
                   into path-tmp-arrivi-prese
           end-string.

      ***---
       OPEN-FILES.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio         delimited size
                  "APERTURA FILES" delimited size
                  into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           open output lineseq.
           if tutto-ok
              open output tmp-progmag tmp-arrivi-prese
              if tutto-ok    
                 accept como-data from century-date
                 accept como-ora  from time
                 initialize path-tmplbx-progmag
                 accept path-tmplbx-progmag from environment "PATH_ST"
                 inspect path-tmplbx-progmag 
                         replacing trailing spaces by low-value
                 string path-tmplbx-progmag delimited low-value
                        "TMPLBX-PROGMAG_"   delimited size
                        como-data           delimited size
                        "_"                 delimited size
                        como-ora            delimited size
                   into path-tmplbx-progmag
                 end-string
                 inspect path-tmplbx-progmag 
                         replacing trailing low-value by spaces
                 open output tmplbx-progmag
                 close       tmplbx-progmag
                 open i-o    tmplbx-progmag

                 if errori
                    close       lineseq tmp-progmag tmp-arrivi-prese
                    delete file lineseq tmp-progmag tmp-arrivi-prese
                 else
                    close      tmp-progmag
                    open i-o   tmp-progmag
         
                    close      tmp-arrivi-prese
                    open i-o   tmp-arrivi-prese

                    open input progmag articoli timballi ordfor ordfor2
                               timbalqta tmagaz tmarche timposte 
                               clienti tparamge tscorte tordforn
                               rordforn sordforn promoeva
                                  
                    if errori
                       close       lineseq tmp-progmag tmp-arrivi-prese
                                   tmplbx-progmag
                       delete file lineseq tmp-progmag tmp-arrivi-prese
                                   tmplbx-progmag
                    end-if
                 end-if
              else
                 close       lineseq
                 delete file lineseq
              end-if
           end-if.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           if tutto-ok
              string r-inizio                  delimited size
                     "APERTURA FILES RIUSCITA" delimited size
                     into como-riga
              end-string
           else
              string r-inizio                      delimited size
                     "APERTURA FILES NON RIUSCITA" delimited size
                     into como-riga
              end-string
           end-if.
           perform SCRIVI-RIGA-LOG.

      ***---
       ELABORAZIONE.
           if RichiamoSchedulato
              move 0 to batch-status  
           end-if.
           |Cerco il magazzino principale
           move low-value to mag-rec.
           start tmagaz key >= mag-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read tmagaz next at end exit perform end-read
              if si-mag-principale
                 move mag-codice to MagazzinoPrincipale
              end-if
              if mag-priorita-mag not = 0
                 if mag-priorita-mag > idx-tot-mag
                    move mag-priorita-mag to idx-tot-mag
                 end-if
                 move mag-codice to el-CodiceMag(mag-priorita-mag)
              end-if
           end-perform.

           |Riempio il file dei soli progressivi presenti 
           |che hanno LBX in art STD
           move low-value to prg-rec.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    move prg-cod-articolo to art-codice
                    read articoli no lock
                    if art-mag-std = "LBX"
                       move prg-chiave      to tmplbx-prg-chiave
                       write tmplbx-rec
                    end-if
                 end-perform
           end-start.

           |Salvo la data di consolidamento
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           compute start-data =
                   function INTEGER-OF-DATE(tge-data-consolid-progmag).
           subtract 364 from start-data.
           move tge-data-consolid-progmag to end-data.
           compute start-data =
                   function DATE-OF-INTEGER(start-data).

           move start-data(5:2) to mese-start.
           move end-data(5:2)   to mese-end.
           move end-data(1:4)   to anno-corr.
           move start-data(1:4) to anno-past.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                           delimited size
                  "INIZIO ELABORAZIONE RIFORNIMENTI" delimited size
                  into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           set tutto-ok      to true.
           set trovato       to false.
           set trovato-prese to false.
           set prima-volta   to true.               
           set ExitPerform   to false.
           move low-value to tmplbx-rec.
           start tmplbx-progmag key >= tmplbx-prg-chiave
                 invalid set errori to true
           end-start.

      *****     move 0 to GiacenzaMTN.
      *****     move 0 to GiacenzaGIC.
      *****     move 0 to GiacenzaSLI.
      *****     move 0 to GiacenzaSHI.
      *****     move 0 to GiacenzaGET.
      *****     move 0 to OrdinatoLBX.
      *****     move 0 to OrdinatoMTN.
      *****     move 0 to OrdinatoGIC.
      *****     move 0 to OrdinatoSLI.
      *****     move 0 to OrdinatoSHI.
      *****     move 0 to OrdinatoGET.
                           
           move 0 to giac-utile.
           move 0 to tot-giac-altri.
           move 0 to SS1.
           move 0 to qta-prenotata. 

           if tutto-ok
              perform until 1 = 2
                 read tmplbx-progmag next
                      at end                
                      set ExitPerform to true
                 end-read              
                 if ExitPerform
                    move spaces to prg-cod-magazzino
                 else
                    move tmplbx-prg-chiave to prg-chiave
                    read progmag no lock
                 end-if
                 perform CONTATORE-VIDEO
                 if prg-cod-magazzino = spaces |Sono sul padre
      *****              if impegnato > ( GiacenzaLBX + OrdinatoLBX )
      *****              if impegnato > GiacenzaLBX
                 
      *****              if ( GiacenzaLBX - impegnato ) < SS2
                    add qta-prenotata to el-ImpegnatoMag(1)
                    if (el-GiacenzaMag(1) - el-ImpegnatoMag(1)) < SS1
                       move 0 to como-idx-mag
                       set trovato-giac to false
                       perform varying idx-mag from 2 by 1 
                                 until idx-mag > idx-tot-mag
                          if el-GiacenzaMag(idx-mag) > 0
                             set trovato-giac to true
                          end-if
                          if como-idx-mag = 0
                             if el-GiacenzaMag(idx-mag) < 0
                                move idx-mag to como-idx-mag
                             end-if
                          end-if
                       end-perform
      ****                 if GiacenzaSLI > 0 or
      ****                    GiacenzaMTN > 0 or
      ****                    GiacenzaGIC > 0 or
      ****                    GiacenzaSHI > 0 or
      ****                    GiacenzaGET > 0
                       if trovato-giac
                          perform SOMMA-MEDIA-ANNO
                          compute giac-utile = el-ImpegnatoMag(1) -
                                               el-GiacenzaMag(1)  +
                                               media-anno  |-
                                               |OrdinatoLBX
                          move 0 to tot-giac-altri
                          perform varying idx-mag from 2 by 1
                                    until idx-mag > idx-tot-mag
                             if idx-mag not = como-idx-mag or
                                como-idx-mag = 0
                                add el-GiacenzaMag(idx-mag) 
                                 to tot-giac-altri
                             end-if
                          end-perform
      *****                    evaluate true
      *****                    when GiacenzaSLI < 0
      *****                         compute tot-giac-altri =
      *****                                 GiacenzaGIC + 
      *****                                 GiacenzaMTN +
      *****                                 GiacenzaSHI +
      *****                                 GiacenzaGET
      *****                    when GiacenzaMTN < 0
      *****                         compute tot-giac-altri =
      *****                                 GiacenzaSLI + 
      *****                                 GiacenzaGIC +
      *****                                 GiacenzaSHI +
      *****                                 GiacenzaGET
      *****                    when GiacenzaGIC < 0
      *****                         compute tot-giac-altri =
      *****                                 GiacenzaSLI + 
      *****                                 GiacenzaMTN +
      *****                                 GiacenzaSHI +
      *****                                 GiacenzaGET
      *****                    when GiacenzaSHI < 0
      *****                         compute tot-giac-altri =
      *****                                 GiacenzaSLI + 
      *****                                 GiacenzaMTN +
      *****                                 GiacenzaGIC +
      *****                                 GiacenzaGET
      *****                    when GiacenzaGET < 0
      *****                         compute tot-giac-altri =
      *****                                 GiacenzaSLI + 
      *****                                 GiacenzaMTN +
      *****                                 GiacenzaGIC +
      *****                                 GiacenzaSHI
      *****                    when other
      *****                         compute tot-giac-altri =
      *****                                 GiacenzaSLI +
      *****                                 GiacenzaMTN +
      *****                                 GiacenzaGIC +
      *****                                 GiacenzaSHI +
      *****                                 GiacenzaGET
      *****                    end-evaluate    
                          perform VALUTA-TOTALI
                       end-if
                    end-if    

                    if ExitPerform
                       exit perform
                    end-if                         
      *****              move 0 to el-GiacenzaMag(1)
      *****              move 0 to GiacenzaMTN
      *****              move 0 to GiacenzaSLI
      *****              move 0 to GiacenzaGIC
      *****              move 0 to GiacenzaSHI
      *****              move 0 to GiacenzaGET
      *****              move 0 to OrdinatoLBX
      *****              move 0 to OrdinatoSLI
      *****              move 0 to OrdinatoGIC
      *****              move 0 to OrdinatoMTN
      *****              move 0 to OrdinatoSHI
      *****              move 0 to OrdinatoGET   
                    move prg-cod-articolo to art-codice
                    read articoli no lock
                    perform INIT-MAG
                    move 0 to SS1
                    move 0 to qta-prenotata
                    perform CALCOLA-COSTO-MP
                    perform CALCOLA-COSTO-MP-WHEN-ZERO
                    add 0,005 to costo-mp giving costo-mp-2dec
                    
                 else                                  
                    if qta-prenotata = 0
                       move prg-cod-articolo to pev-articolo
                       perform CALCOLA-QTA-PRENOTATA
                    end-if
                 
                    if SS1 = 0
                       perform CALCOLA-SCORTA-SICUREZZA
                    end-if
                 
                    set idx-mag to 1
                    search el-mag
                      at end continue
                    when el-CodiceMag(idx-mag) = prg-cod-magazzino
                        add prg-giacenza  to el-GiacenzaMag(idx-mag)
                        add prg-impegnato to el-ImpegnatoMag(idx-mag)
                        add prg-ordinato-1 to el-OrdinatoMag(idx-mag)
                    end-search
                 
      *****              evaluate prg-cod-magazzino
      *****              when 78-LBX add prg-giacenza   to GiacenzaLBX
      *****                          add prg-impegnato  to impegnato
      *****                          add prg-ordinato-1 to OrdinatoLBX
      *****              when 78-SLI add prg-giacenza   to GiacenzaSLI  
      *****                          add prg-ordinato-1 to OrdinatoSLI
      *****              when 78-MTN add prg-giacenza   to GiacenzaMTN  
      *****                          add prg-ordinato-1 to OrdinatoMTN
      *****              when 78-GIC add prg-giacenza   to GiacenzaGIC  
      *****                          add prg-ordinato-1 to OrdinatoGIC
      *****              when 78-SHI add prg-giacenza   to GiacenzaSHI  
      *****                          add prg-ordinato-1 to OrdinatoSHI
      *****              when 78-GET add prg-giacenza   to GiacenzaGET
      *****                          add prg-ordinato-1 to OrdinatoGET
      *****              end-evaluate
                    
                 end-if
              end-perform
           end-if.

           if trovato-prese
      *****        write line-riga of lineseq from spaces after 4
              set prima-volta to false
              initialize line-riga of lineseq
              string ";"                                delimited size
                     "** RIFORNIMENTI DA DEPOSITI AL: " delimited size
                     como-data(7:2)                     delimited size
                     "/"                                delimited size
                     como-data(5:2)                     delimited size
                     "/"                                delimited size
                     como-data(1:4)                     delimited size
                     " **"                              delimited size
                     into line-riga of lineseq
              end-string
              write line-riga of lineseq

              set CreatoFile  to true

              write      line-riga of lineseq from spaces
              initialize riga-comodo
              string "Codice"             delimited size
                     ";"                  delimited size
                     "Descrizione"        delimited size
                     ";"                  delimited size
                     "Q.ta rifornimento"  delimited size
                     ";"                  delimited size
                     "in arrivo LBX (PZ)" delimited size
                     ";"                  delimited size
      *****               "da MTN"             delimited size
      *****               ";"                  delimited size
      *****               "(in pallet)"        delimited size
      *****               ";"                  delimited size
      *****               "in arrivo (PZ)"     delimited size
      *****               ";"                  delimited size
      *****               "da SLI"             delimited size
      *****               ";"                  delimited size
      *****               "(in pallet)"        delimited size
      *****               ";"                  delimited size
      *****               "in arrivo (PZ)"     delimited size
      *****               ";"                  delimited size
      *****               "da GIC"             delimited size
      *****               ";"                  delimited size
      *****               "(in pallet)"        delimited size
      *****               ";"                  delimited size
      *****               "in arrivo (PZ)"     delimited size
      *****               ";"                  delimited size
      *****               "da SHI"             delimited size
      *****               ";"                  delimited size
      *****               "(in pallet)"        delimited size
      *****               ";"                  delimited size
      *****               "in arrivo (PZ)"     delimited size
      *****               ";"                  delimited size
      *****               "da GET"             delimited size
      *****               ";"                  delimited size
      *****               "(in pallet)"        delimited size
      *****               ";"                  delimited size
      *****               "in arrivo (PZ)"     delimited size
      *****               ";"                  delimited size
      *****               "Mancante"           delimited size
                     into riga-comodo
              end-string
              perform varying idx-mag from 2 by 1 
                        until idx-mag > idx-tot-mag
                  inspect riga-comodo replacing trailing 
                                      spaces by low-value
                  string riga-comodo           delimited low-value
                         "da "                 delimited size
                         el-CodiceMag(idx-mag) delimited size
                         ";"                   delimited size
                         "(in pallet)"         delimited size
                         ";"                   delimited size
                         "in arrivo (PZ)"      delimited size
                         ";"                   delimited size
                    into riga-comodo
                  end-string
              end-perform

              inspect riga-comodo replacing trailing 
                                  spaces by low-value
              string riga-comodo  delimited low-value
                     "Mancante"   delimited size
                into riga-comodo
              end-string

              move riga-comodo to line-riga of lineseq
              inspect line-riga of lineseq 
                      replacing trailing low-value by spaces
              write line-riga of lineseq

              close      tmp-progmag
              open input tmp-progmag
              move high-value to tmp-prg-rec
              start tmp-progmag key <= key-costo
                    invalid continue
              end-start

              perform until 1 = 2
                 read tmp-progmag previous at end exit perform end-read

                 perform CONTATORE-VIDEO

                 move tmp-prg-cod-articolo to art-codice 
                 move tmp-prg-giacenza     to giac-utile-ed
                 move tmp-prg-ord(1)       to el-OrdinatoMag-ed(1)

                 |Gli altri li sottraggo dopo
                 compute comodo = tmp-prg-giacenza - tmp-prg-ord(1)
                 
                 perform varying idx-mag from 2 by 1 
                           until idx-mag > idx-tot-mag
                    move tmp-prg-gia(idx-mag) 
                      to el-GiacenzaMag-ed(idx-mag)
                    move tmp-prg-ord(idx-mag)
                      to el-OrdinatoMag-ed(idx-mag)
                    subtract tmp-prg-gia(idx-mag) from comodo
                    subtract tmp-prg-ord(idx-mag) from comodo
                 end-perform        

                 if comodo > 0
                    move comodo to mancante-ed
                 else
                    move 0      to mancante-ed
                 end-if
      *****           move tmp-prg-giac-MTN     to GiacenzaMTN-ed
      *****           move tmp-prg-giac-SLI     to GiacenzaSLI-ed
      *****           move tmp-prg-giac-GIC     to GiacenzaGIC-ed
      *****           move tmp-prg-giac-SHI     to GiacenzaSHI-ed
      *****           move tmp-prg-giac-GET     to GiacenzaGET-ed
      *****
      *****           move tmp-prg-ord-LBX      to OrdinatoLBX-ed
      *****           move tmp-prg-ord-MTN      to OrdinatoMTN-ed
      *****           move tmp-prg-ord-SLI      to OrdinatoSLI-ed
      *****           move tmp-prg-ord-GIC      to OrdinatoGIC-ed
      *****           move tmp-prg-ord-SHI      to OrdinatoSHI-ed
      *****           move tmp-prg-ord-GET      to OrdinatoGET-ed

                 read articoli no lock invalid continue end-read
                 perform CALCOLA-PALLET

      *****           move PalletMTN            to PalletMTN-ed
      *****           move PalletSLI            to PalletSLI-ed
      *****           move PalletGIC            to PalletGIC-ed
      *****           move PalletSHI            to PalletSHI-ed
      *****           move PalletGET            to PalletGET-ed

      *****           compute comodo = tmp-prg-giacenza  -
      *****                            tmp-prg-giac-MTN  -
      *****                            tmp-prg-giac-SLI  -
      *****                            tmp-prg-giac-GIC  -
      *****                            tmp-prg-giac-SHI  -
      *****                            tmp-prg-giac-GET  -
      *****                            tmp-prg-ord-MTN   -
      *****                            tmp-prg-ord-LBX   -
      *****                            tmp-prg-ord-SLI   -
      *****                            tmp-prg-ord-GIC   -
      *****                            tmp-prg-ord-SHI   -
      *****                            tmp-prg-ord-GET         

      *****           if comodo > 0
      *****              move comodo to mancante-ed
      *****           else
      *****              move 0      to mancante-ed
      *****           end-if

      *****           string art-codice            delimited size
      *****                  ";"                   delimited size
      *****                  art-descrizione(1:30) delimited size
      *****                  ";"                   delimited size
      *****                  giac-utile-ed         delimited size
      *****                  ";"                   delimited size
      *****                  OrdinatoLBX-ed        delimited size
      *****                  ";"                   delimited size
      *****                  GiacenzaMTN-ed        delimited size
      *****                  ";"                   delimited size
      *****                  PalletMTN-ed          delimited size
      *****                  ";"                   delimited size
      *****                  OrdinatoMTN-ed        delimited size
      *****                  ";"                   delimited size
      *****                  GiacenzaSLI-ed        delimited size
      *****                  ";"                   delimited size
      *****                  PalletSLI-ed          delimited size
      *****                  ";"                   delimited size
      *****                  OrdinatoSLI-ed        delimited size
      *****                  ";"                   delimited size
      *****                  GiacenzaGIC-ed        delimited size
      *****                  ";"                   delimited size
      *****                  PalletGIC-ed          delimited size
      *****                  ";"                   delimited size
      *****                  OrdinatoGIC-ed        delimited size
      *****                  ";"                   delimited size
      *****                  GiacenzaSHI-ed        delimited size
      *****                  ";"                   delimited size
      *****                  PalletSHI-ed          delimited size
      *****                  ";"                   delimited size
      *****                  OrdinatoSHI-ed        delimited size
      *****                  ";"                   delimited size
      *****                  GiacenzaGET-ed        delimited size
      *****                  ";"                   delimited size
      *****                  PalletGET-ed          delimited size
      *****                  ";"                   delimited size
      *****                  OrdinatoGET-ed        delimited size
      *****                  ";"                   delimited size
      *****                  mancante-ed           delimited size
      *****                  into line-riga of lineseq
      *****           end-string
      *****           write line-riga of lineseq         
                 initialize riga-comodo    
                 string art-codice            delimited size
                        ";"                   delimited size
                        art-descrizione(1:30) delimited size
                        ";"                   delimited size
                        giac-utile-ed         delimited size
                        ";"                   delimited size
                        el-OrdinatoMag-ed(1)  delimited size
                        ";"                   delimited size    
                        into riga-comodo
                 end-string
                 perform varying idx-mag from 2 by 1 
                           until idx-mag > idx-tot-mag
                     inspect riga-comodo replacing trailing 
                                         spaces by low-value
                     string riga-comodo             delimited low-value
                            el-GiacenzaMag-ed(idx-mag) delimited size
                            ";"                        delimited size
                            el-PalletMag-ed(idx-mag)   delimited size
                            ";"                        delimited size
                            el-OrdinatoMag-ed(idx-mag) delimited size
                            ";"                        delimited size
                       into riga-comodo
                     end-string
                 end-perform

                 inspect riga-comodo replacing trailing 
                                     spaces by low-value
                 string riga-comodo  delimited low-value
                        mancante-ed  delimited size
                   into riga-comodo
                 end-string

                 move riga-comodo to line-riga of lineseq
                 inspect line-riga of lineseq 
                         replacing trailing low-value by spaces
                 write line-riga of lineseq
              end-perform

              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                           delimited size
                     "AVVISO PER RIFORNIMENTI GENERATO" delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

           else

              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                         delimited size
                     "NESSUN AVVISO PER RIFORNIMENTI" delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

           end-if.  

      ***---
       INIT-MAG.
           perform varying idx-mag from 1 by 1
                     until idx-mag > 100
              move 0 to el-GiacenzaMag(idx-mag)
                        el-GiacenzaMag-ed(idx-mag)
                        el-ImpegnatoMag(idx-mag)
                        el-OrdinatoMag(idx-mag)
                        el-OrdinatoMag-ed(idx-mag)
                        el-PalletMag(idx-mag)
                        el-PalletMag-ed(idx-mag)
           end-perform.

      ***---
       CALCOLA-QTA-PRENOTATA.
           move low-value to pev-prog.
           start promoeva key >= pev-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read promoeva next at end exit perform end-read
                    if pev-articolo not = prg-cod-articolo
                       exit perform
                    end-if
                    add pev-giac-utile to qta-prenotata
                 end-perform
           end-start.

      ***---
       CALCOLA-SCORTA-SICUREZZA.
           |IN CASO DI MODIFICA ALLA FORMULA
           |ALLINEARE CON ordfor2.cpy
           move "LBX"      to ord2-mag.
           move art-codice to ord2-articolo.
           read ordfor2 no lock
                invalid
                move ord2-chiave to ord-chiave
                read ordfor no lock
                     invalid move 0 to ord-riordino
                             move 0 to ord-consegna
                end-read
                move ord-riordino to ord2-riordino
                move ord-consegna to ord2-consegna
           end-read.
           compute SS1 = ord2-riordino - ord2-consegna.
      *****     read articoli no lock 
      *****          invalid move 0 to SS2
      *****      not invalid
      *****          move art-scorta to sco-codice
      *****          read tscorte no lock 
      *****               invalid move 0 to SS2
      *****           not invalid
      *****               compute SS2 = 
      *****                       SS1 * sco-liv-scorta / 100
      *****          end-read
      *****     end-read.

      ***---
       VALORIZZA-ARRIVI.
           move high-value to tmp-prg-rec
           start tmp-progmag key <= key-costo
                 invalid continue
           end-start.

           perform until 1 = 2
              read tmp-progmag previous at end exit perform end-read
              move low-value to rof-rec
              move tmp-prg-cod-articolo to rof-cod-articolo
              start rordforn key >= rof-k-articolo
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rordforn next at end exit perform end-read
                       if rof-cod-articolo not = tmp-prg-cod-articolo
                          exit perform
                       end-if
                       if rof-qta-ord > rof-qta-evasa
                          move rof-chiave-testa to tof-chiave
                          read tordforn invalid continue end-read
                          if tof-chiuso
                             continue
                          else

                             |IDENTIFICO LA DATA DI ARRIVO 
                             |E LA QUANTITA DA UTILIZZARE
                             move tof-chiave  to sof-chiave
                             move 0           to sof-prog
                             move 0           to sof-data-arr
                             read sordforn no lock
                                  invalid
                                  move rof-chiave to sof-chiave
                                  read sordforn no lock
                                       invalid continue
                                  end-read
                              not invalid
                                  if sof-data-arr = 0
                                     move rof-chiave to sof-chiave
                                     read sordforn no lock
                                          invalid continue
                                     end-read
                                  else
                                     move 0 to sof-qta
                                  end-if
                             end-read

                             |Se trovo la data nelle note di testa la uso
                             |e come qta prendo quella dell'ordine
                             if sof-data-arr not = 0
                                move sof-data-arr to tap-data
                                if sof-qta = 0
                                   compute como-qta = rof-qta-ord -
                                                      rof-qta-evasa
                                else
                                   move sof-qta to como-qta
                                end-if

                                |Non c'è la qta sulla nota per la riga 
                                |relativa oppure non c'è la nota
                                if sof-qta = 0
                                   compute como-qta = rof-qta-ord -
                                                      rof-qta-evasa
                                else
                                   move sof-qta to como-qta
                                end-if

                                move rof-cod-articolo to tap-articolo
                                move tof-cod-forn     to tap-fornitore
                                move sof-data-arr to tap-data
                                read tmp-arrivi-prese 
                                     invalid move 0 to tap-qta
                                end-read
                                add como-qta to tap-qta
                                write tap-rec 
                                      invalid rewrite tap-rec 
                                end-write
                             end-if

                          end-if
                       end-if
                    end-perform
              end-start
           end-perform.

      ***---
       SOMMA-MEDIA-ANNO.
           move 0 to media-anno mesi-utili.
           move MagazzinoPrincipale to ord-mag.
           move art-codice          to ord-articolo.
           read ordfor no lock 
                invalid continue
            not invalid
                move 1 to idx
                move 0 to wk-campo
                initialize occurs-qta
                move mese-start to mese
                perform 12 times
                   if mese > 12
                      move 1 to mese
                   end-if

                   if mese-end = 12
                      move ord-qta-past-m(mese) to el-qta(mese)
                   else
                      |Superando il mese finale significa
                      |che siamo nell'anno precedente
                      if mese > mese-end
                         move ord-qta-past-m(mese) to el-qta(mese)
                      else
                         move ord-qta-corr-m(mese) to el-qta(mese)
                      end-if
                   end-if

                   add el-qta(mese) to wk-campo
                   add 1 to mese idx

                   |SBLOCCARE QUI SE VIENE RICHIESTA LA MEDIA SOLO
                   |SUI MESI IN CUI SONO STATI FATTI RITIRI
      *****             if el-qta(mese) not = 0
      *****                add 1 to mesi-utili
      *****             end-if
                end-perform
                move 12 to mesi-utili
                move 0 to resto
                divide wk-campo by mesi-utili 
                            giving media-anno
                         remainder resto
                if resto not = 0
                   add 1 to media-anno
                end-if
           end-read.

      ***---
       CALCOLA-PALLET.
           move 0 to comodo.
      *****     move 0 to PalletMTN.
      *****     move 0 to PalletSLI.
      *****     move 0 to PalletGIC.
      *****     move 0 to PalletSHI.
      *****     move 0 to PalletGET.
           if art-qta-epal not = 0
              move art-qta-epal to comodo
           end-if.
           if art-qta-std  not = 0
              move art-qta-std to comodo
           end-if.     
           |Nel caso non sia valorizzato evito 
           |di mandare in errore il run-time
           if comodo not = 0
              perform varying idx-mag from 2 by 1 
                        until idx-mag > idx-tot-mag
                 if tmp-prg-gia(idx-mag) not = 0
                    move 0 to resto
                    divide tmp-prg-gia(idx-mag) by comodo 
                           giving el-PalletMag(idx-mag)
                           remainder resto
                    if resto not = 0 
                       add 1 to el-PalletMag(idx-mag)
                    end-if
                 else
                    move 0 to el-PalletMag(idx-mag)
                 end-if
              end-perform
      *****        if tmp-prg-giac-MTN not = 0
      *****           move 0 to resto
      *****           divide tmp-prg-giac-MTN by comodo giving PalletMTN
      *****                                          remainder resto
      *****           if resto not = 0 
      *****              add 1 to PalletMTN 
      *****           end-if
      *****        end-if
      *****        if tmp-prg-giac-SLI not = 0
      *****           move 0 to resto
      *****           divide tmp-prg-giac-SLI by comodo giving PalletSLI
      *****                                          remainder resto
      *****           if resto not = 0 
      *****              add 1 to PalletSLI
      *****           end-if
      *****        end-if
      *****        if tmp-prg-giac-GIC not = 0
      *****           move 0 to resto
      *****           divide tmp-prg-giac-GIC by comodo giving PalletGIC
      *****                                          remainder resto
      *****           if resto not = 0 
      *****              add 1 to PalletGIC
      *****           end-if
      *****        end-if
      *****        if tmp-prg-giac-SHI not = 0
      *****           move 0 to resto
      *****           divide tmp-prg-giac-SHI by comodo giving PalletSHI
      *****                                          remainder resto
      *****           if resto not = 0 
      *****              add 1 to PalletSHI
      *****           end-if
      *****        end-if
      *****        if tmp-prg-giac-GET not = 0
      *****           move 0 to resto
      *****           divide tmp-prg-giac-GET by comodo giving PalletGET
      *****                                          remainder resto
      *****           if resto not = 0 
      *****              add 1 to PalletGET
      *****           end-if
      *****        end-if 
           end-if.  

           perform varying idx-mag from 2 by 1 
                     until idx-mag > idx-tot-mag
              move el-PalletMag(idx-mag) to el-PalletMag-ed(idx-mag)        
           end-perform.

      ***---
       VALUTA-TOTALI.
           |In questo prf valuto solamente quanta merce devo PRENDERE 
           |dai vari magazzini per coprire la quantità utile.

           |Insieme non coprono il dislivello per cui li prendo TUTTI
           if giac-utile > tot-giac-altri
              |Se però un magazzino ha giacenza < 0
              |non prendo nulla da quel magazzino
              perform varying idx-mag from 2 by 1
                        until idx-mag > idx-tot-mag
                 if el-GiacenzaMag(idx-mag) > 0
                    set trovato to true
                 else
                    move 0 to el-GiacenzaMag(idx-mag)
                 end-if
              end-perform
           else
              set trovato to true
              |Se la giacenza di un solo magazzino mi
              |copre l'utile prendo TUTTO da quel 
              |magazzino e niente dagli altri
              move 0 to como-idx-mag
              perform varying idx-mag from 2 by 1 
                        until idx-mag > idx-tot-mag
                 if el-GiacenzaMag(idx-mag) > giac-utile
                    move idx-mag to como-idx-mag
                    exit perform
                 end-if
              end-perform
              if como-idx-mag not = 0
                 perform varying idx-mag from 2 by 1 
                           until idx-mag > idx-tot-mag
                    if idx-mag = como-idx-mag
                       move giac-utile to el-GiacenzaMag(idx-mag)
                    else
                       move 0 to el-GiacenzaMag(idx-mag)
                    end-if
                 end-perform
              else
                 perform DA-QUALE-MAGAZZINO
              end-if
           end-if.

           if trovato
              if prima-volta
                 set prima-volta to false
                 close tmp-progmag
                 open output tmp-progmag
                 set trovato-prese to true
              end-if
              initialize tmp-prg-rec replacing numeric data by zeroes
                                          alphanumeric data by spaces

              move art-codice      to tmp-prg-cod-articolo
              move art-descrizione to tmp-prg-art-des
              move giac-utile      to tmp-prg-giacenza
              perform varying idx-mag from 1 by 1 
                        until idx-mag > idx-tot-mag
                 if el-CodiceMag(idx-mag) not = spaces
                    move el-GiacenzaMag(idx-mag) 
                      to tmp-prg-gia(idx-mag)
                    move el-OrdinatoMag(idx-mag) 
                      to tmp-prg-ord(idx-mag)
                 end-if
              end-perform
      *****        move GiacenzaMTN     to tmp-prg-giac-MTN
      *****        move GiacenzaSLI     to tmp-prg-giac-SLI
      *****        move GiacenzaGIC     to tmp-prg-giac-GIC
      *****        move GiacenzaSHI     to tmp-prg-giac-SHI
      *****        move GiacenzaGET     to tmp-prg-giac-GET
      *****        move OrdinatoMTN     to tmp-prg-ord-MTN
      *****        move OrdinatoSLI     to tmp-prg-ord-SLI
      *****        move OrdinatoGIC     to tmp-prg-ord-GIC
      *****        move OrdinatoLBX     to tmp-prg-ord-LBX
      *****        move OrdinatoSHI     to tmp-prg-ord-SHI
      *****        move OrdinatoGET     to tmp-prg-ord-GET
              compute tmp-prg-costo-medio =
                      costo-mp-2dec * giac-utile
              if tmp-prg-giacenza not = 0
                 write tmp-prg-rec invalid continue end-write
              end-if
           end-if.

      ***---
       DA-QUALE-MAGAZZINO.
           |Da qui in poi la giacenza è coperta da più magazzini
           |Mi è stata data una scala d'importanza (MTN, SLI, GIC, SHI, GET)

           |Uso come priorità quella dei parametri (EX MTN...) 
           compute comodo = giac-utile - el-GiacenzaMag(2).
                                             
           perform varying idx-mag from 3 by 1 
                     until idx-mag > idx-tot-mag
              |Sono sull'ultimo
              if idx-mag = idx-tot-mag
                 move comodo to el-GiacenzaMag(idx-mag)
              else
                 if el-GiacenzaMag(idx-mag) > comodo
                    move comodo to el-GiacenzaMag(idx-mag) 
                    add 1 to idx-mag giving como-idx-mag
                    perform varying como-idx-mag from como-idx-mag by 1 
                              until como-idx-mag > idx-tot-mag
                       move 0 to el-GiacenzaMag(como-idx-mag)
                    end-perform
                    exit perform
                 else  
                    compute comodo = comodo - el-GiacenzaMag(idx-mag)
                 end-if
              end-if
           end-perform.

      *****     compute comodo = giac-utile - GiacenzaMTN.
      *****     |La parte mancante la prendo da SLI (se copre)...
      *****     if GiacenzaSLI > comodo
      *****        move comodo to GiacenzaSLI
      *****        move 0      to GiacenzaGIC
      *****        move 0      to GiacenzaSHI
      *****        move 0      to GiacenzaGET
      *****     else
      *****        |...altrimenti la sottraggo dal totale
      *****        |e il restante la prendo da GIC
      *****        compute comodo = comodo - GiacenzaSLI
      *****        if GiacenzaGIC > comodo
      *****           move comodo to GiacenzaGIC
      *****           move 0      to GiacenzaSHI
      *****           move 0      to GiacenzaGET
      *****        else
      *****           |...altrimenti la sottraggo dal totale
      *****           |e il restante la prendo da SHI
      *****           compute comodo = comodo - GiacenzaGIC
      *****           if GiacenzaSHI > comodo
      *****              move comodo to GiacenzaSHI
      *****              move 0      to GiacenzaGET
      *****           else
      *****              |...altrimenti la sottraggo dal totale
      *****              |e il restante la prendo da GET
      *****              compute comodo = comodo - GiacenzaSHI
      *****              move comodo to GiacenzaGET
      *****           end-if
      *****        end-if
      *****     end-if.    

      ***---
       CLOSE-FILES.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio         delimited size
                  "CHIUSURA FILES" delimited size
                  into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           close lineseq articoli progmag tordforn rordforn sordforn
                 timballi timbalqta tmagaz clienti ordfor2
                 tmp-progmag tmarche timposte tmp-arrivi-prese
                 ordfor tparamge tscorte promoeva tmplbx-progmag

           delete file tmplbx-progmag.     

           if not CreatoFile
              delete file lineseq

              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio               delimited size
                     "NESSUNA MAIL INVIATA" delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

           else
              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                 delimited size
                     "INVIO MAIL IN CORSO..." delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

              move "Invio automatico GESLUX prese giornaliere" 
                to LinkSubject
              initialize LinkAddress
              accept LinkAddress from environment "NIGHT_ADDRESSES_NEW"
              if LinkAddress = spaces
                 accept LinkAddress from environment "NIGHT_ADDRESSES"
              end-if
              move "In allegato dettaglio giacenze."    to LinkBody
              move wstampa                              to LinkAttach
              move 5 to tentativi-mail
              perform CICLO-SEND-MAIL 
                  
              perform SETTA-INIZIO-RIGA
              initialize como-riga
              if mail-ok 
                 string r-inizio               delimited size
                        "INVIO MAIL RIUSCITO!" delimited size
                        into como-riga
                 end-string      
              else            
                 if RichiamoSchedulato
                    move 1 to batch-status  
                 end-if
                 string r-inizio                   delimited size
                        "INVIO MAIL NON RIUSCITO!" delimited size
                        into como-riga
                 end-string
              end-if
              perform SCRIVI-RIGA-LOG

              |FACCIO LA COPIA DEI FILE (INI, CSV)
              perform COPIA-FILES

              delete file lineseq-mail

              |Così cancella anche il csv
              move LinkAttach to wstampa

           end-if.
           delete file tmp-progmag lineseq tmp-arrivi-prese.
 
           if RichiamoSchedulato
              move path-logfile to batch-log

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

           end-if.  
          
      ***---
       AFTER-SEND-MAIL.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio         delimited size
                  "TENTATIVO N. "  delimited size
                  tentativo-mail   delimited size
                  ": STATUS "      delimited size
                  StatusInvioMail  delimited size
                  " - "            delimited size
                  line-riga-mail   delimited size
             into como-riga
           end-string
           perform SCRIVI-RIGA-LOG.

      ***---
       COPIA-FILES.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                delimited size
                  "INIZIO COPIA FILES..." delimited size
                  into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-backup.
           accept  path-backup from environment "PATH_BACKUP".
           inspect path-backup replacing trailing spaces by low-value.

           |FILE INI
           initialize path-ini.
           string  path-backup delimited low-value
                   "InvioMail" delimited size
                   "_"         delimited size
                   como-data   delimited size
                   "_"         delimited size
                   como-ora    delimited size
                   ".ini"      delimited size
                   into path-ini
           end-string.
           move 0 to copy-status.
           call "C$COPY" using path-lineseq-mail, path-ini, "S"
                        giving copy-status.
                                 
           perform SETTA-INIZIO-RIGA.
           if copy-status not = 0 
              if RichiamoSchedulato
                 move 1 to batch-status  
              end-if
              initialize como-riga
              string r-inizio                      delimited size
                     "COPIA FILE INI NON RIUSCITA" delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

              initialize como-riga
              string r-inizio            delimited size
                     "FILE DI PARTENZA:" delimited size
                     wstampa             delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

              initialize como-riga
              string r-inizio                delimited size
                     "FILE DI DESTINAZIONE:" delimited size
                     path-ini                delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
           else
              initialize como-riga
              string r-inizio                  delimited size
                     "COPIA FILE INI RIUSCITA" delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
           end-if.

           |FILE CSV
           initialize path-csv.
           string  path-backup delimited low-value
                   "giacenze"  delimited size
                   "_"         delimited size
                   como-data   delimited size
                   "_"         delimited size
                   como-ora    delimited size
                   ".csv"      delimited size
                   into path-csv
           end-string.
           move 0 to copy-status.
           call "C$COPY" using LinkAttach, path-csv, "S"
                        giving copy-status.
                              
           perform SETTA-INIZIO-RIGA.
           if copy-status not = 0 
              if RichiamoSchedulato
                 move 1 to batch-status  
              end-if
              initialize como-riga
              string r-inizio                      delimited size
                     "COPIA FILE CSV NON RIUSCITA" delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

              initialize como-riga
              string r-inizio            delimited size
                     "FILE DI PARTENZA:" delimited size
                     LinkAttach          delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

              initialize como-riga
              string r-inizio                delimited size
                     "FILE DI DESTINAZIONE:" delimited size
                     path-csv                delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
           else     
              initialize como-riga
              string r-inizio                  delimited size
                     "COPIA FILE CSV RIUSCITA" delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
           end-if.  

           initialize path-log-source.
           accept path-log-source from environment "PATH_LOG_SOURCE".

           initialize path-log-dest.
           string  path-backup delimited low-value
                   "InvioMail" delimited size
                   "_"         delimited size
                   como-data   delimited size
                   "_"         delimited size
                   como-ora    delimited size
                   ".log"      delimited size
                   into path-log-dest
           end-string.

           move 0 to copy-status.
           call "C$COPY" using path-log-source, path-log-dest, "S"
                        giving copy-status.
                      
           perform SETTA-INIZIO-RIGA.
           if copy-status not = 0 
              if RichiamoSchedulato
                 move 1 to batch-status  
              end-if
              initialize como-riga
              string r-inizio                      delimited size
                     "COPIA FILE LOG NON RIUSCITA" delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

              initialize como-riga
              string r-inizio            delimited size
                     "FILE DI PARTENZA:" delimited size
                     path-log-source     delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

              initialize como-riga
              string r-inizio                delimited size
                     "FILE DI DESTINAZIONE:" delimited size
                     path-log-dest           delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
           else
              initialize como-riga
              string r-inizio                  delimited size
                     "COPIA FILE LOG RIUSCITA" delimited size
                     into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
           end-if.     

           |FILE LOG
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                 delimited size
                  "ELABORAZIONE TERMINATA" delimited size
                  into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

      ***---
       SCRIVI-RIGA-LOG.
           if RichiamoSchedulato
              move como-riga to log-riga
              write log-riga
           else
              display como-riga upon syserr
           end-if.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "costo-medio.cpy".
           copy "calcola-costo-mp-when-zero.cpy".
           copy "setta-inizio-riga.cpy".
