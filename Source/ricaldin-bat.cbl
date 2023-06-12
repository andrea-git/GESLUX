       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ricaldin-bat.
       AUTHOR.                          Andrea.
       REMARKS. - COPIA PROGRESSIVI
                Ricalcolo dati dinamici da:
                - note credito non fatturate
                - ordini non fatturati ma bolla emessa
                - movimenti di magazzino successivi al consolidamento

                - ordinato su ordini fornitori inviati e in lavorazione
                  in aumento per ordinata - evasa (aggiornamento stato e 
                  pezzi evasi)
                - giacenza su bozze aperte in aumento

                - aumento impegnato da ordini master (maggiore tra ord e eva) 
                  non chiusi (aggiornamento stato, pezzi e prezzi)

                da lanciare ogni notte come batch previo azzeramento
                dei valori dinamici su progmag.
                La data di consolidamento viene passata in linkage.

                - SE IL PROGRAMMA NON VA A BUON FINE RIPRENDO I PROGRRESSIVI DA BACKUP
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".  
           copy "progmag.sl".

       SELECT progmagc
           ASSIGN       TO  "progmagc"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-progmagc
           RECORD KEY   IS prgc-chiave
           ALTERNATE RECORD KEY IS key01 = prgc-cod-magazzino, 
                                           prgc-cod-articolo, 
                                           prgc-tipo-imballo,
                                           prgc-peso
           WITH DUPLICATES .

           copy "tparamge.sl".
           copy "tordforn.sl". 
           copy "rordforn.sl".
           copy "teva.sl".
           copy "reva.sl".
           copy "tpromo.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tagli.sl".
           copy "ttipocli.sl".
           copy "tscorte.sl".
           copy "articoli.sl".
           copy "tcaumag.sl".
      *****     copy "evaclides.sl".
           copy "timposte.sl".
           copy "tmarche.sl".
           copy "param.sl".
           copy "lineseq-mail.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.         
           copy "tsetinvio.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "tordini.fd".
           copy "rordini.fd".     
           copy "progmag.fd".     
           copy "progmag.fd"
                replacing ==progmag== by ==progmagc==    
                  leading "prg" by "prgc".

           copy "tparamge.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "teva.fd".
           copy "reva.fd".
           copy "tpromo.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "tagli.fd".
           copy "ttipocli.fd".
           copy "tscorte.fd". 
           copy "articoli.fd".
           copy "tcaumag.fd".
      *****     copy "evaclides.fd".   
           copy "timposte.fd".
           copy "tmarche.fd".
           copy "param.fd".
           copy "lineseq-mail.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           copy "link-wprogmag.def".
           copy "mail.def".
           copy "aggiorna-stato-ordf.def".
           copy "aggiorna-stato-master.def".
           copy "link-geslock.def".
           copy "versione-evasione.def".
           copy "trova-parametro.def".
           copy "link-tprev-p.def".  
       01  r-inizio              pic x(25).

       78  user-codi             value "BATCH".
       78  titolo                value"Batch Ricalcolo valori dinamici". 
       77  status-tsetinvio      pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-progmag        pic xx.
       77  status-progmagc       pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tparamge       pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.
       77  status-teva           pic xx.
       77  status-reva           pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tpromo         pic xx.
       77  status-tagli          pic xx.
       77  status-ttipocli       pic xx.
       77  status-tscorte        pic xx.
       77  status-articoli       pic xx.
       77  status-tcaumag        pic xx.
      ***** 77  status-evaclides      pic xx.
       77  status-timposte       pic xx.
       77  status-tmarche        pic xx.
       77  status-param          pic xx.
       77  status-lineseq        pic xx.
       77  wstampa               pic x(256).   
       77  status-lineseq-mail   pic xx.
       77  path-lineseq-mail     pic x(256).
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).
       77  macrobatch            pic x.
       
       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99999.
       77  ss                    pic 99.       
       77  tentativi             pic 99.

       77  counter               pic 9(9).
       77  counter2              pic 9(9).
       77  counter-edit          pic zzz.zzz.zz9.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8). 

       77  mese-oggi             pic 99.
       77  mese-consegna         pic 99.

       77  como-data-1           pic 9(8).
       77  como-data-2           pic 9(8).
       77  resto                 pic 9(3).
       77  diff-giorni           pic 9(5).

       77  link-data             pic 9(8).
       77  como-riga             pic x(80).  
       77  nargs                 pic 99  comp-1 value 0.

       01  como-rec              pic x(5000).
             
       77  filler                pic 9.
           88  nessun-errore     value 1, false 0.

       77  filler                pic 9 value 0.
           88  ripristino        value 1, false 0.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
           
       77  CosaElaborare         pic x.
           88  ElaboraGiacenza   value "G".
           88  ElaboraImpegnato  value "I".

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato          value 1, false 0.
      
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.
       01  GdoInUsoFlag          pic x.
           88 GdoInUso           value "S". 
           88 GdoNonInUso        value " ".  
   
       LINKAGE SECTION.
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.    
       copy "mail-decl.cpy".

      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [MTORDINI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [MTORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[MTORDINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [MRORDINI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [MRORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[MRORDINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TORDFORN-ERR SECTION.
           use after error procedure on tordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordforn
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [TORDFORN] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [TORDFORN] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[TORDFORN] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       RORDFORN-ERR SECTION.
           use after error procedure on rordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordforn
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [RORDFORN] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [RORDFORN] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[RORDFORN] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag 
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TMOVAMG] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TMOVMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TMOVMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag 
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RMOVAMG] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RMOVMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[RMOVMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TEVA-ERR SECTION.
           use after error procedure on teva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-teva
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                   delimited size
                       "File [TEVA] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                     delimited size
                       "File [TEVA] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "[TEVA] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       REVA-ERR SECTION.
           use after error procedure on reva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-reva
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                   delimited size
                       "File [REVA] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                     delimited size
                       "File [REVA] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "[REVA] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

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
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TORDINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini 
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RORDINI] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[RORDINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TNTOACR-ERR SECTION.
           use after error procedure on tnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TNOTACR] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TNOTACR] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TNOTACR] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       RNTOACR-ERR SECTION.
           use after error procedure on rnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rnotacr
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RNOTACR] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RNOTACR] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[RNOTACR] indexed file corrupt!" delimited size
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
       PROGMAGC-ERR SECTION.
           use after error procedure on progmagc.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmagc
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [PROGMAGC] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [PROGMAGC] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[PROGMAGC] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [TPARAMGE] inesistente!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [TPARAMGE] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[TPARAMGE] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true
                set nessun-errore to false
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
           accept versione-evasione from environment "VERSIONE_EVASIONE"
           
           set no-mail to true.
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
              accept macrobatch from environment "MACROBATCH"
              if macrobatch = "S"
                 set environment "MACROBATCH" to " "                                   
                 accept  wstampa from environment "PATH_MACROBATCH_LOG"
              else
                 accept  wstampa from environment "SCHEDULER_PATH_LOG"
              end-if
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa      delimited low-value
                      "RICALDIN_"  delimited size
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
              display "Ricalcolo valori dinamici "
                      "progressivi di magazzino in corso..."
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
           |Quando si consolida il magazzino NESSUNO deve essere
           |dentro al file dei movimenti (tranne che in visua) e mi
           |sembra il minimo dato che devo fare operazioni in massa

LUBEXX     |I files devono essere aperti in input. Corriamo il rischio
LUBEXX     |di elaborare intanto che ci lavorano, ma non importa
           open input tmovmag rmovmag tordini rordini clienti destini
                      tpromo tparamge tnotacr rnotacr teva reva 
                      ttipocli tscorte articoli tcaumag |evaclides
                      timposte tmarche param
           if tutto-ok
              perform OPEN-PROGMAG-LOCK
              if tutto-ok
                 perform OPEN-TORDFORN-LOCK
                 if tutto-ok
                    perform OPEN-RORDFORN-LOCK
                    if tutto-ok
                       perform OPEN-MTORDINI-LOCK
                       if tutto-ok
                          perform OPEN-MRORDINI-LOCK
                          if tutto-ok
                             perform OPEN-OUTPUT-PROGMAGC
                          end-if
                       end-if
                    end-if
                 end-if
              end-if
           end-if.

      ***---
       OPEN-PROGMAG-LOCK.
           open i-o progmag |allowing readers.
           if RecLocked
              set errori to true
              set nessun-errore to false
              perform SETTA-INIZIO-RIGA

              initialize como-riga
              string r-inizio                     delimited size
                     "File [PROGMAG] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
           end-if.

      ***---
       OPEN-TORDFORN-LOCK.
           open i-o tordforn |allowing readers.
           if RecLocked
              set errori to true
              set nessun-errore to false
              perform SETTA-INIZIO-RIGA

              initialize como-riga
              string r-inizio                      delimited size
                     "File [TORDFORN] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
           end-if.

      ***---
       OPEN-RORDFORN-LOCK.
           open i-o rordforn |allowing readers.
           if RecLocked
              set errori to true
              set nessun-errore to false
              perform SETTA-INIZIO-RIGA

              initialize como-riga
              string r-inizio                      delimited size
                     "File [RORDFORN] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
           end-if.

      ***---
       OPEN-MTORDINI-LOCK.
           open i-o mtordini |allowing readers.
           if RecLocked
              set errori to true
              set nessun-errore to false
              perform SETTA-INIZIO-RIGA

              initialize como-riga           
              string r-inizio                      delimited size
                     "File [MTORDINI] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
           end-if. 

      ***---
       OPEN-MRORDINI-LOCK.
           open i-o mrordini |allowing readers.
           if RecLocked
              set errori to true
              set nessun-errore to false
              perform SETTA-INIZIO-RIGA

              initialize como-riga
              string r-inizio                      delimited size
                     "File [MRORDINI] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
           end-if.

      ***---
       OPEN-OUTPUT-PROGMAGC.
           open output progmagc.
           if status-progmagc not = "00"
              set errori to true
           else
              close    progmagc
              open i-o progmagc
           end-if.

      ***---
       ELABORAZIONE.
           move spaces to tge-codice.
           read tparamge no lock invalid continue end-read.
           move tge-data-consolid-progmag to link-data.

           perform BACKUP-PROGMAG.
           perform AZZERA-PROGMAG.
           if ripristino
              perform RESTORE-PROGMAG
              exit paragraph
           end-if.
           perform ELABORA-ORDINI-MASTER.
           if ripristino
              perform RESTORE-PROGMAG
              exit paragraph
           end-if.
           perform ELABORA-NOTE-CREDITO-NON-FATTURATI.          
           if ripristino
              perform RESTORE-PROGMAG
              exit paragraph
           end-if.
           perform ELABORA-INEVASI-E-BOLLA-EMESSA-NON-FATTURATI.
           if ripristino
              perform RESTORE-PROGMAG
              exit paragraph
           end-if.
           perform ELABORA-MOVIMENTI-DI-MAGAZZINO.
           if ripristino
              perform RESTORE-PROGMAG
              exit paragraph
           end-if.
           perform ELABORA-ORDINATO-ORDF.      
           if ripristino
              perform RESTORE-PROGMAG
              exit paragraph
           end-if.
           perform ELABORA-ORDINATO-EVASIONI-F.
           if ripristino
              perform RESTORE-PROGMAG
              exit paragraph
           end-if.

      ***---
       BACKUP-PROGMAG.          
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                    delimited size
                  "INIZIO BACKUP PROGRESSIVI" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.
           move low-value to prg-rec.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    move prg-rec to prgc-rec
                    write prgc-rec
                 end-perform
           end-start.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                  delimited size
                  "FINE BACKUP PROGRESSIVI" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       RESTORE-PROGMAG.
           close       progmag.
           open output progmag |allowing readers.
           if RecLocked
              set errori to true
              set nessun-errore to false
              perform SETTA-INIZIO-RIGA

              initialize como-riga
              string r-inizio                     delimited size
                     "File [PROGMAG] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
              exit paragraph
           end-if.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                     delimited size
                  "INIZIO RESTORE PROGRESSIVI" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.
           move low-value to prgc-rec.
           start progmagc key >= prgc-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmagc next at end exit perform end-read
                    move prgc-rec to prg-rec
                    write prg-rec
                 end-perform
           end-start.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                   delimited size
                  "FINE RESTORE PROGRESSIVI" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.


      ***---
       AZZERA-PROGMAG.
           set tutto-ok to true.
           if not RichiamoSchedulato
              display "Azzeramento in corso..."
           end-if.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                  delimited size
                  "AZZERAMENTO PROGRESSIVI" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           move 0 to counter counter2.

           move low-value to prg-rec.
           start progmag key is >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2 

                    read progmag next at end exit perform end-read                 

                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if 
                    |Azzeramento dei valori dinamici:
                    |GIACENZA - IMPEGNATO - ORDINATO
                    |L'ordinato sarà poi ricalcolato dalla nuova
                    |funzione richiamata immediatamente dopo
                    move 0 to prg-giacenza
                    move 0 to prg-impegnato
                    move 0 to prg-imp-master
                    move 0 to prg-imp-TRAD
                    move 0 to prg-imp-GDO
                    move 0 to prg-ordinato-1
                    move 0 to prg-ordinato-2
                    move 0 to prg-ordinato-3
                    move 0 to prg-ordinato-4
                    move 0 to prg-ordinato-5
                    move 0 to prg-ordinato-6
                    move 0 to prg-giacenza-bloc
                    |Metto in giacenza dinamica quella del periodo
                    move prg-giacenza-udm to prg-giacenza
                    |Aggiornamento del record 
                    rewrite prg-rec invalid continue end-rewrite
                 end-perform
           end-start.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                       delimited size
                  "FINE AZZERAMENTO PROGRESSIVI" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       ELABORA-ORDINI-MASTER.
           move 0 to counter counter2.
           set tutto-ok to true.
           if not RichiamoSchedulato
              display "Elaborazione impegnato master in corso..."
           end-if.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                     delimited size
                  "ELABORAZIONE ORDINI MASTER" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           move low-value to mto-rec.
           set mto-registrato to true.
           start mtordini key >= k-mto-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    move mto-rec to como-rec
                    if mto-chiuso
                       exit perform
                    end-if
                    perform LOOP-RIGHE-MRORDINI
                    if errori
                       set ripristino to true
                       exit perform
                    end-if
      *****              set ricalcolo to true
      *****              perform AGGIORNA-STATO-MASTER
      *****              move como-rec to mto-rec
      *****              start mtordini key > k-mto-stato
      *****                    invalid  exit perform
      *****              end-start

                 end-perform
           end-start.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                          delimited size
                  "FINE ELABORAZIONE ORDINI MASTER" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       LOOP-RIGHE-MRORDINI.
           move mto-chiave to mro-chiave-testa.
           move low-value  to mro-riga.
           start mrordini  key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if        
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if        
                    if mro-chiuso
                       continue
                    else                                
                       perform AGGIORNA-IMPEGNATO-MASTER
                       if errori
                          exit perform
                       end-if
                    end-if        
                 end-perform
           end-start.

      ***---
       ELABORA-NOTE-CREDITO-NON-FATTURATI.
           if not RichiamoSchedulato
              display "Elaborazione NCRE "
                      "non fatturati in corso..."
           end-if.

           move 0 to counter counter2.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio             delimited size
                  "ELABORAZIONE NCRE " delimited size
                  "NON FATTURATI"      delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           move 0 to tno-anno-fattura.
           move 0 to tno-num-fattura.

           start tnotacr key is >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tnotacr next at end exit perform end-read

                    if tno-data-fattura not = 0 or
                       tno-num-fattura  not = 0
                       exit perform
                    end-if

                    move tno-causale to tca-codice
                    read tcaumag no lock invalid continue end-read
                    if tca-nota-credito-si and tca-tipo-nota-reso
                       perform LOOP-RIGHE-RNOTACR
                    end-if             

                    if errori 
                       set ripristino to true
                       exit perform 
                    end-if

                 end-perform
           end-start.
           perform SETTA-INIZIO-RIGA.       
      *****     display counter upon syserr.
           initialize como-riga.
           string r-inizio                  delimited size
                  "FINE ELABORAZIONE NCRE " delimited size
                  "NON FATTURATI"           delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       LOOP-RIGHE-RNOTACR.
           move tno-anno   to rno-anno.
           move tno-numero to rno-numero.
           move low-values to rno-num-riga.
           start rnotacr key is >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rnotacr  next at end exit perform end-read
                    if tno-anno   not = rno-anno    or
                       tno-numero not = rno-numero
                       exit perform
                    end-if       
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if
                    move "BATCH"               to link-user
                    move "1000000000000010"    to link-array
                    move rno-qta               to link-valore
                    move tno-causale           to link-causale
                    move rno-prg-chiave        to link-key
                    set  link-update           to true
                    set  link-open-with-lock   to true
                    set  link-update-um        to true
                    set  link-update-peso      to false
                    set  link-update-valore    to false
                    set  link-chiamata-batch   to true
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    evaluate link-wprogmag-status
                    when -1
                         perform SETTA-INIZIO-RIGA
                         initialize como-riga
                         string r-inizio             delimited size
                                link-msg-err-ritorno delimited size
                                into como-riga
                         end-string
                         perform RIGA-LOG
                         set nessun-errore to false      
                         set errori     to true
                         exit perform
                    when -2
                         perform SETTA-INIZIO-RIGA
                         inspect link-msg-err-ritorno replacing trailing
                                                     spaces by low-value
                         initialize como-riga
                         string r-inizio             delimited size
                                link-msg-err-ritorno delimited low-value
                                " --> IMPOSSIBILE "  delimited size
                                "PROSEGUIRE!"        delimited size
                                into como-riga
                         end-string                
                         set errori     to true
                         perform RIGA-LOG
                         set nessun-errore to false
                         exit perform
                    end-evaluate
                 end-perform
           end-start.
                 
      ***---     
       ELABORA-INEVASI-E-BOLLA-EMESSA-NON-FATTURATI.
           if not RichiamoSchedulato
              display "Elaborazione inevasi e bolla emessa "
                      "non fatturati in corso..."
           end-if.

           move 0 to counter counter2.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                         delimited size
                  "ELABORAZIONE INEVASI E BOLLA "  delimited size
                  "EMESSA NON FATTURATI"           delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           move 0 to tor-anno-fattura.
           move 0 to tor-num-fattura.

           start tordini key is >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-num-fattura  not = 0 or
                       tor-anno-fattura not = 0
                       exit perform
                    end-if    
       
                    |09042018:
                    |Le bolle con data fattura non devono essere considerate,
                    |ma non possono essere usate come rottura di chiave
                    if tor-data-fattura = 0
                       if tor-num-bolla  not = 0 and
                          tor-data-bolla not = 0
                          set ElaboraGiacenza  to true
                       else
                          set ElaboraImpegnato to true
                       end-if
                    end-if
                    ||||

                    perform LOOP-RIGHE-RORDINI
                    if errori
                       set ripristino to true
                       exit perform 
                    end-if

                 end-perform
           end-start.
           perform SETTA-INIZIO-RIGA.
      *****     display counter upon syserr.
           initialize como-riga.
           string r-inizio                   delimited size
                  "FINE ELABORAZIONE BOLLA " delimited size
                  "EMESSA NON FATTURATI"     delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       LOOP-RIGHE-RORDINI.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           move low-values to ror-num-riga.
           start rordini key is >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini  next at end exit perform end-read
                    if tor-anno   not = ror-anno    or
                       tor-numero not = ror-num-ordine
                       exit perform
                    end-if 
                    move 0                  to link-impegnato
                    move "BATCH"            to link-user
                    move ror-prg-chiave     to link-key
                    evaluate true
                    when ElaboraGiacenza
                         |AGISCO SULLA GIACENZA (NORMALE E BLOCCATA)
                         move "1000000000000010"  to link-array
                    when ElaboraImpegnato
                         |AGISCO SULL'IMPEGNATO
                         move "0100000000000000"  to link-array
                    end-evaluate    
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if
                    move ror-qta               to link-valore
                    move tor-causale           to link-causale
                    move ror-prg-chiave        to link-key
                    set  link-update           to true
                    set  link-open-with-lock   to true
                    set  link-update-um        to true
                    set  link-update-peso      to false
                    set  link-update-valore    to false
                    set  link-chiamata-batch   to true
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    evaluate link-wprogmag-status
                    when -1
                         perform SETTA-INIZIO-RIGA
                         initialize como-riga
                         string r-inizio             delimited size
                                link-msg-err-ritorno delimited size
                                into como-riga
                         end-string
                         perform RIGA-LOG
                         set nessun-errore to false
                         set errori to true
                         exit perform
                    when -2
                         perform SETTA-INIZIO-RIGA
                         inspect link-msg-err-ritorno replacing trailing
                                                     spaces by low-value
                         initialize como-riga
                         string r-inizio             delimited size
                                link-msg-err-ritorno delimited low-value
                                " --> IMPOSSIBILE "  delimited size
                                "PROSEGUIRE!"        delimited size
                                into como-riga
                         end-string
                         perform RIGA-LOG
                         set nessun-errore to false
                         set errori to true
                         exit perform
                    end-evaluate   
                 end-perform
           end-start.

      ***---
       ELABORA-MOVIMENTI-DI-MAGAZZINO.
           if not RichiamoSchedulato
              display "Elaborazione movimenti "
                      "di magazzino in corso..."
           end-if.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                              delimited size
                  "ELABORAZIONE MOVIMENTI DI MAGAZZINO" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.
                              
           move 0 to counter counter2.
           set tutto-ok   to true.
           move low-value to tmo-rec.
           |Non importa anche se diventa 20051032. L'importante è che
           |la start successiva si posizioni sul giorno superiore al 31
           add 1 to link-data.
           move link-data  to tmo-data-movim.
           start tmovmag key is > k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag  next at end exit perform end-read

                    if tmo-data-movim <= link-data
                       exit perform
                    end-if
                    perform LOOP-RIGHE-MAGAZZINO
                    if errori
                       set ripristino to true
                       exit perform
                    end-if
                 end-perform
           end-start.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                  delimited size
                  "FINE ELABORAZIONE "      delimited size
                  " MOVIMENTI DI MAGAZZINO" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       LOOP-RIGHE-MAGAZZINO.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           move low-value  to rmo-riga.
           start rmovmag key is >= rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno   not =  tmo-anno or
                       rmo-movim  not =  tmo-numero
                       exit perform
                    end-if
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if
                    move "BATCH"             to link-user
                    |AGISCO SULLA GIACENZA (NORMALE E BLOCCATA)
                    move "1000000000000010"  to link-array
                    move rmo-qta             to link-valore
                    move rmo-causale         to link-causale
                    move rmo-chiave-progmag  to link-key
                    set  link-update         to true
                    set  link-open-with-lock to true
                    set  link-update-um      to true
                    set  link-update-peso    to false
                    set  link-update-valore  to false
                    set  link-chiamata-batch to true
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    evaluate link-wprogmag-status
                    when -1
                         perform SETTA-INIZIO-RIGA
                         initialize como-riga
                         string r-inizio             delimited size
                                link-msg-err-ritorno delimited size
                                into como-riga
                         end-string
                         perform RIGA-LOG
                         set nessun-errore to false   
                         set errori to true
                         exit perform
                    when -2
                         perform SETTA-INIZIO-RIGA
                         inspect link-msg-err-ritorno replacing trailing
                                                     spaces by low-value
                         initialize como-riga
                         string r-inizio             delimited size
                                link-msg-err-ritorno delimited low-value
                                " --> IMPOSSIBILE "  delimited size
                                "PROSEGUIRE!"        delimited size
                                into como-riga
                         end-string
                         perform RIGA-LOG
                         set nessun-errore to false 
                         set errori to true
                         exit perform
                    end-evaluate   
                 end-perform
           end-start.

      ***---
       ELABORA-ORDINATO-ORDF.
           if not RichiamoSchedulato
              display "Elaborazione ordinato evasioni in corso..."
           end-if.
           
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                 delimited size
                  "ELABORAZIONE ORDINATO " delimited size
                  "EVASIONI"               delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           move 0 to counter counter2.
           set tutto-ok    to true.
           move high-value  to tof-rec.
           set tof-inserito to true.
           start tordforn key > tof-k-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordforn next at end exit perform end-read
                    move tof-rec to como-rec

                    |Gli ordini chiusi manualmente e quelli inseriti
                    |non vanno proprio considerati
                    if tof-chiusura-man or tof-inserito| or tof-accettato
                       continue
                    else
                       accept como-data from century-date
                       if como-data >= tof-data-consegna
                          move 1 to tof-mese-rif
                       else
                          if tof-urgente
                             move 1 to tof-mese-rif
                          else
                             move tof-data-consegna(5:2)to mese-consegna
                             move como-data(5:2)        to mese-oggi
                             |Data di consegna nell'anno successivo: il mese
                             |è di valore minore, ma la data INTERA no
                             if mese-oggi > mese-consegna
                                add 12 to mese-consegna
                             end-if
                             compute tof-mese-rif = 
                                   ( mese-consegna - mese-oggi ) + 1
                          end-if
                          
      *****                    compute como-data-1 = 
      *****                            function INTEGER-OF-DATE(como-data)
      *****                    compute como-data-2 = 
      *****                       function INTEGER-OF-DATE(tof-data-consegna)
      *****                   compute diff-giorni = como-data-2 - como-data-1
      *****                    if diff-giorni <= 30
      *****                       move 1 to tof-mese-rif
      *****                    else
      *****                       move 0 to resto
      *****                       divide diff-giorni by 30 
      *****                                      giving tof-mese-rif
      *****                                   remainder resto
      *****                       if resto not = 0
      *****                          add 1 to tof-mese-rif
      *****                       end-if
      *****                    end-if
                       end-if

                       perform LOOP-RIGHE-RORDFORN 
                       if errori
                          set ripristino to true
                          exit perform
                       end-if
                       perform AGGIORNA-STATO-ORDF
                       move como-rec to tof-rec
                       start tordforn key > tof-k-stato
                             invalid  exit perform
                       end-start

                    end-if
                 end-perform
           end-start.
           
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                      delimited size
                  "FINE ELABORAZIONE ORDINATO " delimited size
                  "EVASIONI"                    delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       LOOP-RIGHE-RORDFORN.
           move tof-anno   to rof-anno.
           move tof-numero to rof-numero.
           move low-value  to rof-riga.
           start rordforn key is >= rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn next at end exit perform end-read
                    if rof-chiave-testa not = tof-chiave
                       exit perform
                    end-if       
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if
                                    
                    |1. AGISCO SULL'ORDINATO col mese attuale
                    |(l'ordinato è già stato azzerato in precedenza)
                    move "0010000000000000"  to link-array
                    move tof-mese-rif        to link-mese-rif
                    move tof-chiave          to link-chiave-origine
                    move tof-causale         to link-causale
                    move rof-prg-chiave      to link-key
                    move rof-qta-ord         to link-valore
                    set  link-update         to true
                    set  link-open-with-lock to true
                    set  link-update-um      to true
                    set  link-update-peso    to false
                    set  link-update-valore  to false
                    set  link-chiamata-batch to true
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    if link-wprogmag-status = -1
                       set errori to true
                       exit perform
                    end-if

                    move 0 to tof-pz-arrivati
                    set tof-inevaso to true
      
                    |2. Controllo se ci evasioni relativi
                    |alla riga con qta da evadere ed aggiorno
                    |la qta evasa come somme delle evasioni
                    move 0 to rof-qta-evasa
                    move rof-chiave to reva-chiave-ordf
                    start reva key >= reva-chiave-ordf
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read reva next 
                                  at end exit perform 
                             end-read
                             if reva-chiave-ordf not = rof-chiave
                                exit perform
                             end-if
                             add reva-qta to rof-qta-evasa
                          end-perform
                    end-start

                    if rof-qta-evasa > 0
                       if rof-qta-evasa < rof-qta-ord
                          move rof-qta-evasa to link-valore
                       else
                          move rof-qta-ord   to link-valore
                       end-if
                       |Storno l'ordinato col mese attuale
                       |per la quantità evasa
                       move "0000000000000000"  to link-array
                       move -1                  to multiplyer(3)
                       move tof-mese-rif        to link-mese-rif
                       move tof-chiave          to link-chiave-origine
                       move tof-causale         to link-causale
                       move rof-prg-chiave      to link-key
                       set  link-update         to true
                       set  link-open-with-lock to true
                       set  link-update-um      to true
                       set  link-update-peso    to false
                       set  link-update-valore  to false
                       set  link-chiamata-batch to true
                       if prima-volta
                          set prima-volta to false
                          close progmag |wprogmag aprirà con lock il file!!!
                       end-if
                       call "wprogmag" using link-wprogmag
                       if link-wprogmag-status = -1
                          set errori to true
                          exit perform
                       end-if
                       rewrite rof-rec invalid continue end-rewrite
                    end-if
                 end-perform
           end-start.

      ***---
       ELABORA-ORDINATO-EVASIONI-F.   
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                 delimited size
                  "ELABORAZIONE GIACENZA " delimited size
                  "EVASIONI F"             delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

           set tutto-ok   to true.
           move low-value to tof-rec.
           set teva-aperta to true.
           start teva key is > teva-stato
                 invalid  continue
             not invalid
                 perform until 1 = 2
                    read teva next at end exit perform end-read

                    if teva-chiusa
                       exit perform
                    end-if
                    perform LOOP-RIGHE-REVA
                    if errori
                       set ripristino to true
                       exit perform
                    end-if
                 end-perform
           end-start.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                      delimited size
                  "FINE ELABORAZIONE GIACENZA " delimited size
                  "EVASIONI F"                  delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       LOOP-RIGHE-REVA.
           move teva-anno   to reva-anno.
           move teva-numero to reva-numero.
           move low-value  to reva-riga.
           start reva key is >= reva-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read reva next at end exit perform end-read
                    if reva-chiave-testa not = teva-chiave
                       exit perform
                    end-if    
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if
      
                    move reva-chiave-testa-ordf to tof-chiave
                    read tordforn no lock invalid continue end-read
                    |1. AGISCO SULLA GIACENZA (NORMALE E BLOCCATA)
                    move "1000000000000010"  to link-array
                    move reva-qta            to link-valore
                    move tof-causale         to link-causale
                    move reva-chiave-progmag to link-key
                    set  link-update         to true
                    set  link-open-with-lock to true
                    set  link-update-um      to true
                    set  link-update-peso    to false
                    set  link-update-valore  to false
                    set  link-chiamata-batch to true
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    if link-wprogmag-status = -1
                       set errori to true
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***----
       AGGIORNA-IMPEGNATO-MASTER.
           set  cli-tipo-C  to true.
           move mto-cod-cli to cli-codice.
           read clienti     no lock.

           |Ad aumentare l'impegnato sulle qta evase 
           |ci penseranno poi le evasioni
           if mro-qta > mro-qta-e
              compute link-valore = mro-qta - mro-qta-e
           else
              move 0       to link-valore
           end-if.
           move link-valore to link-impegnato.
           perform DIREZIONA-IMPEGNATO.

           move "BATCH"               to link-user.
           move "0100000000000000"    to link-array.
           move mto-causale           to link-causale.
           move mro-prg-chiave        to link-key.
           set  link-update           to true.
           set  link-open-with-lock   to true.
           set  link-update-um        to true.
           set  link-update-peso      to false.
           set  link-update-valore    to false.
           set  link-chiamata-batch   to true.
           if prima-volta
              set prima-volta to false
              close progmag |wprogmag aprirà con lock il file!!!
           end-if.
           call "wprogmag" using link-wprogmag.
           evaluate link-wprogmag-status
           when -1
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio             delimited size
                       link-msg-err-ritorno delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set nessun-errore to false
                set errori to true
           when -2
                perform SETTA-INIZIO-RIGA
                inspect link-msg-err-ritorno replacing trailing
                                             spaces by low-value
                initialize como-riga
                string r-inizio             delimited size
                       link-msg-err-ritorno delimited low-value
                       " --> IMPOSSIBILE "  delimited size
                       "PROSEGUIRE!"        delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set nessun-errore to false
                set errori to true
           end-evaluate.

      ***---
       CONTATORE-VIDEO.
           if batch-win-handle <= 0 exit paragraph end-if.
           add 1 to counter counter2

           if counter2 = 300
              move counter to counter-edit
              display counter-edit 
                      upon batch-win-handle
                      line 25,00
                    column 38,00
              move 0 to counter2
           end-if.

      ***--
       CLOSE-FILES.
           close tmovmag rmovmag tordini rordini teva reva  mtordini
                 tparamge tnotacr rnotacr tordforn rordforn mrordini
                 tpromo clienti destini ttipocli tscorte articoli
                 tcaumag timposte tmarche param. |evaclides 

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
           else
              perform INVIO-MAIL
           end-if.

           move user-codi to link-tprev-user.
           move spaces    to link-tprev-handle.
           call   "tprev-p" using tprev-linkage
           cancel "tprev-p".

           if RichiamoSchedulato and batch-win-handle > 0
              if nessun-errore
                 move  0 to batch-status
              else              
                 move -1 to batch-status
              end-if
              display "                                             "
                 upon batch-win-handle
                   line 25,00
                 column 35,00
           end-if.
                  
           goback.

      ***---
       INVIO-MAIL.
           display "Invio mail in corso...".

           move "INVIO MAIL IN CORSO..." to como-riga.
           perform SETTA-RIGA-STAMPA.

           initialize LinkBody.
           if nessun-errore
              move "RICALCOLO GIACENZE DINAMICHE - OK" 
                to LinkSubject
           else
              move "RICALCOLO GIACENZE DINAMICHE - ATTENZIONE" 
                to LinkSubject
           end-if.

           move "In allegato dettagli funzionamento programma" 
             to LinkBody.

           accept LinkAddress from environment "RICALDIN_ADDRESSES".


           accept FileOrig    from environment "RICALDIN_LOG".
           accept FileDest    from environment "RICALDIN_LOG_INVIO".
           call "C$COPY" using FileOrig, FileDest, "S".
           move FileDest to LinkAttach.

           move "ricaldin-bat" to NomeProgramma.

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
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "aggiorna-stato-ordf.cpy".
      *****     copy "aggiornaX-stato-master.cpy".
           copy "direziona-impegnato-common.cpy".
           copy "trova-parametro.cpy".
           copy "setta-inizio-riga.cpy".
