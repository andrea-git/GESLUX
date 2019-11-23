       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-qp.
       AUTHOR.                          Andrea.
       REMARKS. Controllo notturno con correzione automatica
                e segnalazione nel caso in cui:
                - il progressivo di magazzino sia lo stesso su
                  movimento/evasione/ordini f
                - le qta di un movimento di magazzino (che deriva da
                  un'evasione) sia diversa dall'evasione
                - il progressivo di magazzino sia lo stesso su
                  evasioni aperte/ordini f

                per entrambi i casi verrà tenuto buono il 
                dato sul movimento di magazzino. 
                Verrano presi i movimenti di magazzino non consolidati
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tordforn.sl". 
           copy "rordforn.sl".
           copy "tparamge.sl".
           copy "teva.sl".
           copy "reva.sl".
           copy "lineseq.sl".

       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "tparamge.fd".
           copy "teva.fd".
           copy "reva.fd".
           copy "lineseq.fd".   

       FD  lineseq1.
       01 line-riga        PIC  x(900).

       WORKING-STORAGE SECTION.
           copy "mail.def".

       77  status-tmovmag        pic xx.
       77  status-tsetinvio      pic xx.
       77  status-rmovmag        pic xx.
       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.
       77  status-tparamge       pic xx.
       77  status-teva           pic xx.
       77  status-reva           pic xx.
       77  status-lineseq        pic xx.
       77  wstampa               pic x(256).   
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).    
       77  nargs                 pic 99  comp-1 value 0.   

       77  counter               pic 9(9).
       77  counter2              pic 9(9).
       77  counter-edit          pic zzz.zzz.zz9.


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

       77  como-riga             pic x(300).

       77  filler        pic 9 value 0.
           88  nessun-errore   value 0.
           88  errore-warning  value 1.
           88  errore-error    value 2.

       77  filler            pic 9.
           88  corretto                value 0.
           88  ordf-not-found          value 1.
           88  teva-not-found          value 2.
           88  prog-rmo-rof-diversi    value 3.
           88  prog-rmo-reva-diversi   value 4.
           88  prog-reva-rof-diversi   value 5.
           88  qta-rmo-reva-diversi    value 6.

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
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [TORDFORN] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[TORDFORN] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
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
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [RORDFORN] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[RORDFORN] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
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
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TMOVMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TMOVMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
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
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RMOVMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[RMOVMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
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
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                     delimited size
                       "File [TEVA] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "[TEVA] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true      
                set errore-error to true
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
                set errore-error to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                     delimited size
                       "File [REVA] mismatch size!" delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
                set errori to true        
                set errore-error to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "[REVA] indexed file corrupt!" delimited size
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
                      "CHECK-QP_"  delimited size
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
           open input tmovmag rmovmag tparamge.
           if tutto-ok
              perform OPEN-TEVA-LOCK
              if tutto-ok
                 perform OPEN-REVA-LOCK
                 if tutto-ok
                    perform OPEN-TORDFORN-LOCK
                    if tutto-ok
                       perform OPEN-RORDFORN-LOCK
                    end-if
                 end-if
              end-if
           end-if.

      ***---
       OPEN-TEVA-LOCK.
           open i-o teva allowing readers.
           if RecLocked
              set errori to true        
              set errore-error to true
              perform SETTA-INIZIO-RIGA

              initialize como-riga
              string r-inizio                  delimited size
                     "File [TEVA] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
           end-if.

      ***---
       OPEN-REVA-LOCK.
           open i-o reva allowing readers.
           if RecLocked
              set errori to true
              set errore-error to true
              perform SETTA-INIZIO-RIGA

              initialize como-riga
              string r-inizio                  delimited size
                     "File [REVA] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
           end-if.

      ***---
       OPEN-TORDFORN-LOCK.
           open i-o tordforn allowing readers.
           if RecLocked
              set errori to true
              set errore-error to true
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
           open i-o rordforn allowing readers.
           if RecLocked
              set errori to true
              set errore-error to true
              perform SETTA-INIZIO-RIGA

              initialize como-riga
              string r-inizio                      delimited size
                     "File [RORDFORN] già in uso!" delimited size
                     into como-riga
              end-string
              perform RIGA-LOG
           end-if.

      ***---
       ELABORAZIONE.
           move spaces to tge-codice.
           read tparamge no lock invalid continue end-read.

           perform ELABORA-MOVIMENTI-DI-MAGAZZINO.
           perform ELABORA-EVASIONI-APERTE.

      ***---
       ELABORA-MOVIMENTI-DI-MAGAZZINO.
           if not RichiamoSchedulato
              display "Elaborazione movimenti di magazzino in corso..."
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
           add 1 to tge-data-consolid-progmag giving tmo-data-movim.
           start tmovmag key is > k-data
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tmovmag  next at end exit perform end-read

                 if tmo-data-movim <= tge-data-consolid-progmag
                    exit perform
                 end-if
                 set corretto to true
                 if tmo-teva-anno   not = 0 and
                    tmo-teva-numero not = 0
                    move tmo-teva-anno   to teva-anno
                    move tmo-teva-numero to teva-numero
                    read teva no lock
                         invalid
                         set teva-not-found to true
                         perform ERRORI
                     not invalid
                         perform LOOP-RIGHE-MAGAZZINO
                    end-read
                 end-if
              end-perform
           end-if.
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
           move low-value to reva-rec.
           move tmo-teva-anno   to reva-anno.
           move tmo-teva-numero to reva-numero.
           start reva key >= reva-chiave
                 invalid continue
           end-start.

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

                    set corretto to true

                    read reva next
                    move reva-chiave-ordf to rof-chiave
                    read rordforn no lock 
                         invalid
                         set ordf-not-found to true
                         perform ERRORI
                     not invalid
                         if rmo-chiave-progmag not = rof-prg-chiave  
                            set prog-rmo-rof-diversi to true
                            perform ERRORI
                            move rmo-chiave-progmag to rof-prg-chiave 
                            rewrite rof-rec invalid continue end-rewrite
                         end-if
                    end-read

                    if rmo-chiave-progmag not = reva-chiave-progmag
                       set prog-rmo-reva-diversi to true
                       perform ERRORI
                       move rmo-chiave-progmag to reva-chiave-progmag
                       rewrite reva-rec invalid continue end-rewrite
                    end-if

                    if rmo-qta not = reva-qta
                       set qta-rmo-reva-diversi to true
                       perform ERRORI
                       move rmo-qta to reva-qta
                       rewrite reva-rec invalid continue end-rewrite
                    end-if
                    
                 end-perform
           end-start.

      ***---
       ELABORA-EVASIONI-APERTE.
           if not RichiamoSchedulato
              display "Elaborazione evasioni aperte in corso..."
           end-if.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                       delimited size
                  "ELABORAZIONE EVASIONI APERTE" delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.
                              
           move 0 to counter counter2.
           move low-value to teva-rec.
           set teva-aperta to true.
           start teva key >= teva-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read teva next at end exit perform end-read
                    if teva-chiusa exit perform end-if                   

                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if

                    set corretto to true
                    move reva-chiave-ordf to rof-chiave
                    read rordforn no lock
                         invalid
                         set ordf-not-found to true
                         perform ERRORI
                     not invalid
                         if reva-chiave-progmag not = rof-prg-chiave  
                            set prog-reva-rof-diversi to true
                            perform ERRORI
                            move reva-chiave-progmag to rof-prg-chiave
                            rewrite rof-rec invalid continue end-rewrite
                         end-if
                    end-read
                 end-perform
           end-start.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio              delimited size
                  "FINE ELABORAZIONE "  delimited size
                  " EVASIONI APERTE"    delimited size
                  into como-riga
           end-string.
           perform RIGA-LOG.

      ***---
       ERRORI.
           evaluate true
           when teva-not-found
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio            delimited size
                       "EVASIONE "         delimited size
                       teva-anno           delimited size
                       " - "               delimited size
                       teva-numero         delimited size
                       " NON TROVATA SU"   delimited size
                       " MOVIMENTO MAG "   delimited size
                       tmo-anno            delimited size
                       " - "               delimited size
                       tmo-numero          delimited size
                       into como-riga
                end-string
                perform RIGA-LOG

           when ordf-not-found
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio            delimited size
                       "ORDINE FORNITORE " delimited size
                       rof-anno            delimited size
                       " - "               delimited size
                       rof-numero          delimited size
                       " - "               delimited size
                       rof-riga            delimited size
                       " NON TROVATO SU"   delimited size
                       " EVASIONE "        delimited size
                       reva-anno           delimited size
                       " - "               delimited size
                       reva-numero         delimited size
                       into como-riga
                end-string
                perform RIGA-LOG

           when prog-rmo-rof-diversi
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                 delimited size
                       "ORDINE F "              delimited size
                       rof-anno                 delimited size
                       " - "                    delimited size
                       rof-numero               delimited size
                       " - "                    delimited size
                       rof-riga                 delimited size
                       " PROGRESSIVO"           delimited size
                       rof-prg-chiave           delimited size
                       " DIVERSA DA MOVIMENTO " delimited size
                       rmo-anno                 delimited size
                       " - "                    delimited size
                       rmo-movim                delimited size 
                       " - "                    delimited size
                       rmo-riga                 delimited size
                       ". PROGRESSIVO "         delimited size
                       rmo-chiave-progmag       delimited size
                       into como-riga
                end-string
                perform RIGA-LOG

           when prog-rmo-reva-diversi
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                 delimited size
                       "EVASIONE "              delimited size
                       reva-anno                delimited size
                       " - "                    delimited size
                       reva-numero              delimited size
                       " - "                    delimited size
                       reva-riga                delimited size
                       " PROGRESSIVO"           delimited size
                       reva-chiave-progmag      delimited size
                       " DIVERSA DA MOVIMENTO " delimited size
                       rmo-anno                 delimited size
                       " - "                    delimited size
                       rmo-movim                delimited size 
                       " - "                    delimited size
                       rmo-riga                 delimited size
                       ". PROGRESSIVO "         delimited size
                       rmo-chiave-progmag       delimited size
                       into como-riga
                end-string
                perform RIGA-LOG

           when prog-reva-rof-diversi
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                 delimited size
                       "ORDINE F "              delimited size
                       rof-anno                 delimited size
                       " - "                    delimited size
                       rof-numero               delimited size
                       " - "                    delimited size
                       rof-riga                 delimited size
                       " PROGRESSIVO"           delimited size
                       rof-prg-chiave           delimited size
                       " DIVERSA DA EVASIONE "  delimited size
                       reva-anno                delimited size
                       " - "                    delimited size
                       reva-numero              delimited size 
                       " - "                    delimited size
                       reva-riga                delimited size
                       ". PROGRESSIVO "         delimited size
                       reva-chiave-progmag      delimited size
                       into como-riga
                end-string
                perform RIGA-LOG

           when qta-rmo-reva-diversi
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                 delimited size
                       "EVASIONE "              delimited size
                       reva-anno                delimited size
                       " - "                    delimited size
                       reva-numero              delimited size
                       " - "                    delimited size
                       reva-riga                delimited size
                       " QUANTITA'"             delimited size
                       reva-qta                 delimited size
                       " DIVERSA DA MOVIMENTO " delimited size
                       rmo-anno                 delimited size
                       " - "                    delimited size
                       rmo-movim                delimited size 
                       " - "                    delimited size
                       rmo-riga                 delimited size
                       ". QUANTITA' "           delimited size
                       rmo-qta                  delimited size
                       into como-riga
                end-string
                perform RIGA-LOG
           end-evaluate.
           set errore-warning to true.   

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
           close tmovmag rmovmag teva reva tparamge tordforn rordforn.

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
              move "CONTROLLO CONGRUENZA MOVMAG/EVASIONI/ORDINI F - OK" 
                to LinkSubject
           else
              move 
            "CONTROLLO CONGRUENZA MOVMAG/EVASIONI/ORDINI F - ATTENZIONE" 
                to LinkSubject
           end-if.

           move "In allegato dettagli funzionamento programma" 
             to LinkBody.

           accept LinkAddress from environment "CHECK_QP_ADDRESSES".
           accept FileOrig    from environment "CHECK_QP_LOG".
           accept FileDest    from environment "CHECK_QP_LOG_INVIO".
           call "C$COPY" using FileOrig, FileDest, "S".
           move FileDest to LinkAttach.

           set errori to true.
           move 0 to tentativi.
           move "check-qp" to NomeProgramma.

           perform 10 times
              add 1 to tentativi
              perform SEND-MAIL
              
              initialize como-riga
              if StatusInvioMail = -1
                 string r-inizio                      delimited size
                        "TENTATIVO N. "               delimited size
                        tentativi                     delimited size
                        ": "                          delimited size
                        "Chiamata InvioMail fallita!" delimited size
                        " STATUS -1"                  delimited size
                        into como-riga
                 end-string
              else
                 string r-inizio                       delimited size
                        "TENTATIVO N. "                delimited size
                        tentativi                      delimited size
                        ": "                           delimited size
                        "Chiamata InvioMail riuscita!" delimited size
                        into como-riga
                 end-string
              end-if
              perform SETTA-RIGA-STAMPA
                            
              call "C$DELETE" using FileDest
              open input lineseq1
              read  lineseq1 next
              if line-riga of lineseq1 = "True"
                 set tutto-ok to true
                 close lineseq1
                 exit perform
              end-if
              close lineseq1

              initialize como-riga
              string r-inizio              delimited size
                     "TENTATIVO N. "       delimited size
                     tentativi             delimited size
                     ": "                  delimited size
                     line-riga of lineseq1 delimited size
                     into como-riga
              end-string
              perform SETTA-RIGA-STAMPA

           end-perform
               
           initialize como-riga.
           if tutto-ok
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

           delete file lineseq.

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
