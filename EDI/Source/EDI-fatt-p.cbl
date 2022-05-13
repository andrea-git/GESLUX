       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      edi-fatt-p.
       AUTHOR.                          Andrea.
       REMARKS. Aggiornamento del numero di ultimo documento trattato
                nella tabella dei contatori TCONTAT.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.            

       SELECT logfile
           ASSIGN       TO  path-logfile
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-logfile.

           copy "tparamge.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "tcontat.sl".
           copy "articoli.sl".
           copy "tivaese.sl".
           copy "lineseq.sl".
           copy "timbalqta.sl".
           copy "EDI-param.sl".
           copy "EDI-tiva.sl".
           copy "EDI-clides.sl".
           copy "tcaumag.sl".
           copy "tcodpag.sl".
           copy "clienti.sl".
           copy "recapiti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          

       FD  logfile.
       01 log-riga        PIC  x(900). 

           copy "tparamge.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "tcontat.fd".
           copy "articoli.fd".
           copy "tivaese.fd".
           copy "lineseq.fd".  
           copy "timbalqta.fd".
           copy "EDI-param.fd".
           copy "EDI-tiva.fd".
           copy "EDI-clides.fd".
           copy "tcaumag.fd". 
           copy "tcodpag.fd".
           copy "clienti.fd".
           copy "recapiti.fd".

       WORKING-STORAGE SECTION.                                     
      *    COPY
           copy "edi.def".
           copy "link-geslock.def".
           copy "comune.def".
           copy "varsca".
           copy "link-stfatt.def".

      *    COSTANTI
       78  titolo                value "Generazione file invio EDI".
                                          
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

      *    FILE-STATUS
       77  status-tparamge       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-tcontat        pic xx.
       77  status-tivaese        pic xx.
       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.
       77  status-EDI-param      pic xx.
       77  status-EDI-tiva       pic xx.
       77  status-EDI-clides     pic xx.
       77  status-tcaumag        pic xx.
       77  status-timbalqta      pic xx.
       77  status-tcodpag        pic xx.
       77  status-clienti        pic xx.
       77  status-recapiti       pic xx.
       77  status-logfile        pic xx.
       77  wstampa               pic x(256).   
                                               
       77  copy-status           signed-short.
       77  fatt-status           signed-short.
       77  nc-status             signed-short.
       77  path-backup           pic x(256).    
       77  CallingPgm            pic x(20).   

       77  path-logfile          pic x(256).  
       77  path-log              pic x(256).
       77  como-riga             pic x(200).    
       
       77  importo-netto         pic 9(9)v99.
       77  imponibile-merce      pic 9(9)v99.
       77  tot-imponibile        pic 9(9)v99.
       77  tot-consumo           pic 9(9)v99.
       77  tot-cou               pic 9(9)v99.
       77  tot-iva               pic 9(9)v99.
       77  tot-merce             pic 9(9)v99.
       77  tot-fattura           pic 9(9)v99.
       77  tot-solo-cou          pic 9(9)v99.
       77  tot-cobat             pic 9(9)v99.
       77  tot-piombo            pic 9(9)v99.
       77  causale-omaggio       pic x(3).
       77  tipo-nome-file        pic x(50).
       77  save-ecd-export-imposte pic 9.
         88 save-ecd-export-imposte-si value 1.
         88 save-ecd-export-imposte-no value 0.

       01  tabella-iva           occurs 3. 
         05 cod-iva              pic x(3).            
         05 imponibile-iva       pic 9(9)v99.
         05 importo-iva          pic 9(15)v99.  

       01  filler                pic 9 value 0.
         88 RichiamoStFatt       value 1, false 0.
                   
       77  prz-tot-riga          pic 9(12)v9(3).
       77  prz-lordo             pic 9(12)v9(3).
       77  como-iva              pic 9(9)v999.
       77  como-iva-2dec         pic 9(9)v99.   

       77  como-numero           pic z(12)vz(5).
       01  como-numero-x.
           05 como-int           pic x(12).
           05 como-dec           pic x(5).
       77  NumericEDI            pic x(20).       
       
       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  whh                   pic 99.
       77  wmm                   pic 9(5).
       77  wss                   pic 99.  

       01  r-inizio              pic x(25).

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  nargs                 pic 99  comp-1 value 0.
                         
       01  filler                pic 9.
           88 FileCreato         value 1, false 0.
       01  filler                pic 9.
           88 prima-volta        value 1, false 0.
       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.

       77  filler                 pic xx.
           88  nota-credito-qta   value "NQ".
           88  nota-credito-prz   value "NP".
           88  nota-credito-reso  value "NR".
           88  accredito-generico value "NG".  
           88  fattura            value "FA".     
           88  fattura-manuale    value "FM".
           88  addebito-generico  value "FG".
                                                 
       77  filler                pic 9.
           88  no-cou            value 0.
           88  si-cou            value 1.
       77  filler                pic 9.
           88  no-cobat          value 0.
           88  si-cobat          value 1.
       77  filler                pic 9.
           88  no-piombo         value 0.
           88  si-piombo         value 1.

       77  filler                pic 9.
           88 record-ok          value 1, false 0.

       77  filler                pic 9.
           88 trovataIVA         value 1, false 0.
           
      *
       01 edi-work.
           03 edi-tipo-doc       pic 9.
           03 edi-num-a          pic 9(8).
           03 edi-num-da         pic 9(8).
           03 edi-anno           pic 9(4).       
           03 edi-tot-doc        pic 9(8).
           03 edi-tot-pdf        pic 9(8).
           03 edi-doc-da         pic 9(8).
           03 edi-doc-a          pic 9(8).
           03 edi-user           pic x(20).
           03 edi-handle         usage handle.

       LINKAGE SECTION.
       copy "link-batch.def".
       copy "link-edi.def".

      ******************************************************************
       PROCEDURE DIVISION using batch-linkage edi-linkage.

       DECLARATIVES.                                        
      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                if not RichiamoSchedulato
                   display message "File [TORDINI] not found!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "39"
                if not RichiamoSchedulato
                   display message "File [TORDINI] Mismatch size!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "98"
                if not RichiamoSchedulato
                   display message "[TORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           end-evaluate.
 
      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                if not RichiamoSchedulato
                   display message "File [RORDINI] not found!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "39"
                if not RichiamoSchedulato
                   display message "File [RORDINI] Mismatch size!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "98"
                if not RichiamoSchedulato
                   display message "[RORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           end-evaluate.
 
      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "35"
                if not RichiamoSchedulato
                   display message "File [TNOTACR] not found!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "39"
                if not RichiamoSchedulato
                   display message "File [TNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "98"
                if not RichiamoSchedulato
                   display message "[TNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           end-evaluate.
 
      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           set tutto-ok  to true.
           evaluate status-rnotacr
           when "35"
                if not RichiamoSchedulato
                   display message "File [RNOTACR] not found!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "39"
                if not RichiamoSchedulato
                   display message "File [RNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "98"
                if not RichiamoSchedulato
                   display message "[RNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           end-evaluate.
 
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                if not RichiamoSchedulato
                   display message "File [ARTICOLI] not found!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "39"
                if not RichiamoSchedulato
                   display message "File [ARTICOLI] Mismatch size!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "98"
                if not RichiamoSchedulato
                   display message "[ARTICOLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           end-evaluate.
 
      ***---
       TIVAESE-ERR SECTION.
           use after error procedure on tivaese.
           set tutto-ok  to true.
           evaluate status-tivaese
           when "35"
                if not RichiamoSchedulato
                   display message "File [TIVAESE] not found!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "39"
                if not RichiamoSchedulato
                   display message "File [TIVAESE] Mismatch size!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "98"
                if not RichiamoSchedulato
                   display message "[TIVAESE] Indexed file corrupt!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           end-evaluate. 

      ***---
       TCONTAT-ERR SECTION.
           use after error procedure on tcontat.
           set tutto-ok  to true.
           evaluate status-tcontat
           when "35"
                if not RichiamoSchedulato
                   display message "File [TCONTAT] not found!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "39"
                if not RichiamoSchedulato
                   display message "File [TCONTAT] Mismatch size!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "98"
                if not RichiamoSchedulato
                   display message "[TCONTAT] Indexed file corrupt!"
                           title titolo
                            icon 3
                end-if
                set errori to true
           when "93"
                if RichiamoSchedulato
                   set errori to true
                else
                   initialize geslock-messaggio
                   string   "File già in uso!"
                     x"0d0a""Impossibile procedere!" delimited size
                         into geslock-messaggio
                   end-string
                   move 1 to geslock-v-riprova
                   move 0 to geslock-v-ignora
                   move 1 to geslock-v-termina
                   move   "tcontat"    to geslock-nome-file
                   call   "geslock" using geslock-linkage
                   cancel "geslock"
                   evaluate true
                   when riprova
                        perform OPEN-IO-TCONTAT
                   when termina
                        set errori to true
                        display message "Operazione interrotta!"
                                  title titolo
                                   icon 2
                   end-evaluate
                end-if
           when "99"
                if RichiamoSchedulato 
                   set errori to true
                else
                   initialize geslock-messaggio
                   string   "Il record per l'anno: " con-anno
           x"0d0a""risulta in uso su altro terminale."    delimited size
           x"0d0a""Questo comporta l'impossibiltà ad"     delimited size
           x"0d0a""aggiornare la tabella dei contatori."  delimited size
                         into geslock-messaggio
                   end-string
                   move 1 to geslock-v-riprova
                   move 0 to geslock-v-ignora
                   move 1 to geslock-v-termina
                   move   "tcontat"    to geslock-nome-file
                   call   "geslock" using geslock-linkage
                   cancel "geslock"
                   evaluate true
                   when riprova
                        perform READ-TCONTAT-LOCK
                   when termina
                        set errori to true
                        display message "Operazione interrotta!"
                                  title titolo
                                   icon 2
                   end-evaluate
                end-if
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35" set errori to true
           when "93" 
                if RichiamoSchedulato 
                   set errori to true
                else
                   initialize geslock-messaggio
                   string   "File già in uso!"
                     x"0d0a""Impossibile procedere!" delimited size
                         into geslock-messaggio
                   end-string
                   move 1 to geslock-v-riprova
                   move 0 to geslock-v-ignora
                   move 1 to geslock-v-termina
                   move   "File TXT"   to geslock-nome-file
                   call   "geslock" using geslock-linkage
                   cancel "geslock"
                   evaluate true
                   when riprova
                        open output lineseq
                   when termina
                        set errori to true
                        display message "Operazione interrotta!"
                                  title titolo
                                   icon 2
                   end-evaluate
                end-if
           end-evaluate.

       END DECLARATIVES.

      ***---
       START-PRG.
           call "C$NARG" using nargs.
           evaluate nargs
           when 0 continue
           when 1
                set RichiamoSchedulato to true    

                accept como-data from century-date
                move como-data(1:4) to esercizio
                set environment "ESERCIZIO" to esercizio

                move -1 to fatt-status nc-status  
                accept  path-log from environment "SCHEDULER_PATH_LOG"
                perform OPEN-LOGFILE
                                                      
                accept esercizio-x from environment "ESERCIZIO"
                move   esercizio-x to esercizio
                initialize como-riga     
                string "RICERCA CONTATORE FATTURE PER L'ANNO: " 
                       esercizio delimited size
                  into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                perform TROVA-CONTATORE-ANNO-ESERCIZIO
                if tutto-ok
                   move 1 to edi-tipo-doc
                   move esercizio to edi-anno
                   add 1 to con-ult-num-EDI giving edi-num-da
                   move 99999999  to edi-num-a
                   move "ELABORAZIONE FATTURE" to como-riga
                   perform SCRIVI-RIGA-LOG
                   perform MAIN-PRG
                end-if
                initialize tor-rec replacing numeric data by zeroes
                                        alphanumeric data by spaces
                initialize como-riga     
                string "RICERCA CONTATORE NC PER L'ANNO: " 
                       esercizio delimited size
                  into como-riga
                end-string
                perform SCRIVI-RIGA-LOG
                perform TROVA-CONTATORE-ANNO-ESERCIZIO
                if tutto-ok
                   move 2 to edi-tipo-doc
                   move esercizio to edi-anno
                   add 1 to con-ult-num-nc-EDI giving edi-num-da
                   move 99999999  to edi-num-a
                   move "ELABORAZIONE NOTE CR" to como-riga
                   perform SCRIVI-RIGA-LOG
                   perform MAIN-PRG
                end-if

                move 0 to tot-secondi
                accept como-ora from time
                move como-ora(1:2) to whh
                move como-ora(3:2) to wmm
                move como-ora(5:2) to wss

                compute end-secondi = (whh * 3600) + (wmm * 60) + wss
                compute tot-secondi = end-secondi - start-secondi

                if tot-secondi < 60
                   move tot-secondi to wss
                   initialize como-riga
                   string "ELABORAZIONE TERMINATA IN: ",
                          wss, " SECONDI" delimited size
                     into como-riga
                   end-string
                   perform SCRIVI-RIGA-LOG
                else
                   divide tot-secondi by 60 giving wmm remainder wss
                   initialize como-riga
                   string "ELABORAZIONE TERMINATA IN: ",
                           wmm, " MINUTI E ", wss, " SECONDI" 
                           delimited size
                      into como-riga
                   end-string
                   perform SCRIVI-RIGA-LOG
                end-if

                close logfile

                evaluate fatt-status nc-status
                when  0 -1  move -1 to batch-status
                when  0  0  move  0 to batch-status
                when  0  1  move  1 to batch-status
                when -1 -1  
                when -1  0
                when -1  1  
                when  1 -1  move -1 to batch-status
                when  1  0  
                when  1  1  move  1 to batch-status
                end-evaluate

               |Se per il resto è andato tutto a buon fine
               |controllo lo stato  del programma stfatt (se chiamato)
                if batch-status = 0
                   if RichiamoStFatt
                      move stfatt-status to batch-status
                   end-if
                end-if


           when other
                set RichiamoSchedulato to false
                move edi-linkage to edi-work
                perform MAIN-PRG
           end-evaluate.

           perform EXIT-PGM.


      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok 
              initialize stfatt-linkage 
                         replacing numeric data by zeroes
                              alphanumeric data by spaces        
              move spaces to epa-codice
              read edi-param no lock
                   invalid
                   set errori to true
                   if RichiamoSchedulato
                      move "Mancano parametri di importazione EDI-PARAM"
                        to como-riga
                      perform SCRIVI-RIGA-LOG
                   else
                      display message
                      "Impostare parametri di importazione"
                                title titolo
                                 icon 2
                   end-if
              end-read
              if tutto-ok
                 move spaces to tge-codice
                 read tparamge no lock 
                      invalid continue
                 end-read
                 move tge-causale-omag to tca-codice
                 read tcaumag no lock 
                      invalid 
                      set errori to true 
                      if RichiamoSchedulato
                         move "CAUSALE OMAGGIO NON TROVATA" to como-riga
                         perform SCRIVI-RIGA-LOG
                      else
                         display message "Causale omaggio non trovata"
                                    title titolo
                                     type 2
                      end-if
                 end-read
                 move tca-causale-edi to causale-omaggio
              end-if
           end-if.
           if tutto-ok
              if edi-tipo-doc = 1 perform ELABORAZIONE-FATTURE
              else                perform ELABORAZIONE-NC
              end-if
              perform CLOSE-FILES
                      
              if edi-tipo-doc = 1 move 0 to fatt-status
              else                move 0 to nc-status
              end-if             

              if edi-tot-pdf not = 0
                 if RichiamoSchedulato
                    move "Stampa fatture" to como-riga
                    perform SCRIVI-RIGA-LOG
                    set SoloPDFBatch to true
                 else
                    set SoloPDF to true
                 end-if
                 set RichiamoStFatt to true
                 move 1 to LinkElab
                 move 1 to num-copie
                 if edi-tipo-doc = 1 set Fatture     to true
                 else                set NoteCredito to true
                 end-if
                 move edi-anno to LinkAnno
                 call   "stfatt-p" using stfatt-linkage
                 cancel "stfatt-p"
      *****           if link-tipo-stampa = 9 
                 if stfatt-status = 1
                    subtract edi-tot-pdf from edi-tot-doc
                    move 0 to edi-tot-pdf
                    |Viene scritta già nel pgm chiamato
      *****              move "INVIO PDF NON RIUSCITO" to como-riga
      *****              perform SCRIVI-RIGA-LOG
                    if edi-tipo-doc = 1 move -1 to fatt-status
                    else                move -1 to nc-status
                    end-if
                 end-if
              end-if

              if FileCreato
                 initialize path-backup
                 accept  path-backup 
                         from environment "EDI_FATT_PATH_BACKUP"
                 inspect path-backup 
                         replacing trailing spaces by low-value

                 string  path-backup    delimited low-value
                         tipo-nome-file delimited low-value
                         como-data      delimited size
                         "_"            delimited size
                         como-ora       delimited size
                         ".txt"         delimited size
                         into path-backup
                  end-string
                  
                  call "C$COPY" using wstampa, path-backup, "S"
                               giving copy-status
                  if copy-status = 0
                     move "BACKUP EFFETTUATO CORRETTAMENTE" to como-riga
                     perform SCRIVI-RIGA-LOG
                  else                      
                     move "BACKUP NON RIUSCITO" to como-riga
                     perform SCRIVI-RIGA-LOG
                     if edi-tipo-doc = 1 move 1 to fatt-status
                     else                move 1 to nc-status
                     end-if
                  end-if
              end-if

           end-if.

      ***---
       INIT.
           move 0 to edi-tot-doc edi-tot-pdf.
           move 0 to edi-doc-da.
           move 0 to edi-doc-a.
           move 0 to counter counter2.
           set tutto-ok     to true.
           set prima-volta  to true.
           set FileCreato   to false.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa.
           accept  wstampa from environment "EDI_FATT_PATH".
           inspect wstampa replacing trailing spaces by low-value.
           if edi-tipo-doc = 1 
              move "LUBEX_fatture_EDI_" to tipo-nome-file
           else
              move "LUBEX_note-cr_EDI_" to tipo-nome-file
           end-if.
           inspect tipo-nome-file replacing trailing spaces by low-value
           string  wstampa        delimited low-value
                   tipo-nome-file delimited low-value
                   como-data      delimited size
                   "_"            delimited size
                   como-ora       delimited size
                   ".txt"         delimited size
                   into wstampa
           end-string.

      ***---
       OPEN-LOGFILE.
           accept como-ora  from time.
           inspect path-log replacing trailing spaces by low-value.
           initialize path-logfile.
           string path-log         delimited low-value
                  "LOG_EDI_FATT_"  delimited size
                  como-data        delimited size
                  "_"              delimited size
                  como-ora         delimited size
                  ".log"           delimited size
                  into path-logfile
           end-string.
           open output logfile.
                                            
           move   como-ora(1:2) to whh.
           move   como-ora(3:2) to wmm.
           move   como-ora(5:2) to wss.

           compute start-secondi = ( whh * 3600 ) + ( wmm * 60 ) + wss.

      ***---
       OPEN-FILES.
           move "APERTURA FILES" to como-riga.
           perform SCRIVI-RIGA-LOG.
           perform OPEN-IO-TCONTAT.
           if tutto-ok
              perform READ-TCONTAT-LOCK
              if tutto-ok
                 if edi-tipo-doc = 1
                    open input tordini rordini
                 else
                    open input tnotacr rnotacr
                 end-if
                 open input tivaese articoli  tparamge timbalqta 
                            tcodpag EDI-param EDI-tiva EDI-clides 
                            tcaumag clienti recapiti
                 if errori
                    close tcontat
                 end-if
              else
                 close tcontat
              end-if
           end-if.  
           if errori
              move "APERTURA FILES KO" to como-riga
           else
              move "APERTURA FILES OK" to como-riga
           end-if.
           perform SCRIVI-RIGA-LOG.

      ***---
       OPEN-IO-TCONTAT.
           open i-o tcontat.

      ***---
       READ-TCONTAT-LOCK.
           move edi-anno  to con-anno.
           read tcontat lock invalid set errori to true end-read.

      ***---
       ELABORAZIONE-FATTURE.
           if RichiamoSchedulato
              move "INIZIO ELABORAZIONE FATTURE" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           move edi-anno   to tor-anno-fattura.
           move edi-num-da to tor-num-fattura.
           start tordini key  is >= k-fattura
                 invalid
                 if not RichiamoSchedulato
                    display message "Nessun documento da inviare!"
                              title titolo
                               icon 2
                 else
                    move "NESSUNA FATTURA ELABORATA" to como-riga
                    perform SCRIVI-RIGA-LOG
                 end-if
                 set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tordini next at end exit perform end-read
                 if tor-num-fattura > edi-num-a
                    exit perform
                 end-if
                 if tor-anno-fattura not = edi-anno
                    exit perform
                 end-if
                 perform COUNTER-VIDEO

                 set record-ok to false

                 |13/12/2016: guardare sul cliente non sull'ordine
                 set cli-tipo-C   to true
                 move tor-cod-cli to cli-codice
                 read clienti no lock
                      invalid move spaces to rec-invio
                  not invalid
                      move cli-codice to rec-codice
                      read recapiti no lock
                           invalid move spaces to rec-invio
                      end-read
                 end-read
                 move rec-invio to tor-invio

                 if tor-invio-edi
                    move tor-causale to tca-codice
                    read tcaumag no lock
                         invalid continue
                     not invalid
                         if tca-causale-EDI not = spaces
                            set record-ok to true
                         end-if
                    end-read
                 end-if

                 if record-ok
                    if tor-ordine
                       set fattura to true          
                    else
                       if tca-causale-edi = "XX"
                          set addebito-generico to true
                       else
                          set fattura-manuale to true
                       end-if
                    end-if

                    if addebito-generico
                       initialize como-riga
                       string "ELABORATA FATTURA N. " delimited size
                              tor-num-fattura         delimited size
                              " ADDEBITO GENERICO"    delimited size
                              into como-riga
                       end-string        
                       perform SCRIVI-RIGA-LOG
                       if num-da = 0
                          move tor-num-fattura to num-da
                       end-if
                       move tor-num-fattura to num-a
                       add 1 to edi-tot-pdf
                    else                      
                       initialize como-riga
                       string "ELABORATA FATTURA N. " delimited size
                              tor-num-fattura         delimited size
                              into como-riga
                       end-string
                       perform SCRIVI-RIGA-LOG
                       perform VALORIZZA-EDI-01H-01T
                       perform LOOP-RIGHE-RORDINI
                       |NON SONO RIUSCITO AD APRIRE IL FILE DI TESTO
                       if errori exit perform end-if    
                       perform VALORIZZA-EDI-PIEDE 
                    end-if

                    if edi-doc-da = 0
                       move tor-num-fattura to edi-doc-da
                    end-if
                    if edi-doc-a = 0
                       move tor-num-fattura to edi-doc-a
                    end-if        
                    add 1 to edi-tot-doc
                    if tor-num-fattura < edi-doc-da
                       move tor-num-fattura to edi-doc-da
                    end-if
                    if tor-num-fattura > edi-doc-a
                       move tor-num-fattura to edi-doc-a
                    end-if
                 end-if
                 if errori exit perform end-if
              end-perform
           end-if.

           if tutto-ok
              if edi-tot-doc > 0

                 call "C$CALLEDBY" using CallingPgm
                 if CallingPgm not = "edi-stdoc"
                    move edi-doc-a to con-ult-num-edi
                    rewrite con-rec invalid continue end-rewrite
                 end-if

                 initialize como-riga
                 string "ELABORATE "   delimited size
                        edi-tot-doc    delimited size 
                        " FATTURE. "   delimited size
                        "DA N. "       delimited size
                        edi-doc-da     delimited size
                        " A N. "       delimited size
                        edi-doc-a      delimited size
                        " DI CUI PDF " delimited size
                        edi-tot-pdf    delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
              else
                 if not RichiamoSchedulato
                    display message "Nessun documento da inviare!"
                              title titolo
                               icon 2
                 else
                    move "NESSUNA FATTURA ELABORATA" to como-riga
                    perform SCRIVI-RIGA-LOG
                 end-if
              end-if
           end-if.

      ***---
       COUNTER-VIDEO.
           add 1 to counter.
           add 1 to counter2.
           if counter2 = 100 and batch-win-handle not = 0
              move counter to counter-edit

              if RichiamoSchedulato
                 display counter-edit
                    upon batch-win-handle
                    line 25,00
                  column 38,00
              else
                 display counter-edit
                    upon edi-handle at column 15
                                         line 03
              end-if
              move 0 to counter2
           end-if.

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
       LOOP-RIGHE-RORDINI.
      *****     if visualizza-totali = "S"
      *****        |Guardo i totali delle imposte in maniera preventiva
      *****        |x' devo sapere prima uante righe di totalizzatori
      *****        |finali dovrò poi stampare
      *****        move low-values   to ror-rec
      *****        move tor-anno     to ror-anno
      *****        move tor-numero   to ror-num-ordine
      *****        start rordini key is >= ror-chiave
      *****              invalid set errori to true
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read rordini next at end exit perform end-read
      *****                 if ror-anno       not = tor-anno or
      *****                    ror-num-ordine not = tor-numero
      *****                    exit perform
      *****                 end-if
      *****                 move ror-cod-articolo to art-codice
      *****                 read articoli no lock invalid continue end-read
      *****                 if ror-add-piombo > 0
      *****                    set si-piombo to true
      *****                    compute tot-piombo = 
      *****                            tot-piombo +
      *****                            ror-add-piombo  * 
      *****                          ( ror-qta - ror-qta-omaggi )
      *****                 end-if
      *****                 if ror-imp-cou-cobat > 0
      *****                    if art-si-cobat
      *****                       set si-cobat to true
      *****                       compute tot-cobat = 
      *****                               tot-cobat +
      *****                               ror-imp-cou-cobat * 
      *****                             ( ror-qta - ror-qta-omaggi )
      *****                    else
      *****                       set si-cou   to true
      *****                       compute tot-solo-cou = 
      *****                               tot-solo-cou +
      *****                               ror-imp-cou-cobat * 
      *****                             ( ror-qta - ror-qta-omaggi )
      *****                    end-if
      *****                 end-if
      *****              end-perform
      *****        end-start
      *****        if si-piombo add 1 to righe-finali end-if
      *****        if si-cou    add 1 to righe-finali end-if
      *****        if si-cobat  add 1 to righe-finali end-if
      *****     else
      *****        move 0 to righe-finali
      *****     end-if.

           move 0 to tot-iva imponibile-iva(1) imponibile-iva(2)
                     imponibile-iva(3) importo-iva(1) importo-iva(2)
                     importo-iva(3) tot-fattura tot-imponibile tot-merce
           move low-values   to ror-rec.
           move tor-anno     to ror-anno.
           move tor-numero   to ror-num-ordine.
           start rordini key is >= ror-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rordini next at end exit perform end-read
                 if tor-anno   not = ror-anno  or
                    tor-numero not = ror-num-ordine
                    exit perform
                 end-if          
                 move 0 to tno-num-fattura
                 move 0 to tno-data-fattura
                 perform VALORIZZA-EDI-02D
              end-perform
           end-if.                              

      ***---
       VALORIZZA-EDI-02D.
           add ror-qta to tot-merce.
           move 01T1-tipo-doc      to 02D1-tipo-doc
           move 01T2-id-edi        to 02D2-id-edi
           move 01T3-q-id-edi      to 02D3-q-id-edi
           move 01T4-data-doc      to 02D4-data-doc
           move 01T5-num-doc       to 02D5-num-doc 
           move ecd-ragsoc-d       to 02D9-ragsoc-dest       
           move ecd-indirizzo-d    to 02D10-indirizzo-dest    
           move ecd-citta-d        to 02D11-citta-dest    
           move ecd-prov-d         to 02D12-prov-dest     
           move ecd-cap-d          to 02D13-cap-dest      
           move ecd-cod-consegna   to 02D7-cod-dest  
           move ecd-q-cod-consegna to 02D8-q-cod-dest
           if tor-num-bolla not = 0
              move tor-num-bolla   to 02D14-num-bolla
              inspect 02D14-num-bolla replacing leading x"30" by x"20"
              call "C$JUSTIFY" using 02D14-num-bolla, "L"
           else
              move spaces          to 02D14-num-bolla
           end-if.                                    
           if tor-data-bolla not = 0
              move tor-data-bolla  to 02D15-data-bolla
           else
              move spaces          to 02D15-data-bolla
           end-if.     
           if tor-num-ord-cli not = spaces
              move tor-num-ord-cli   to 02D16-num-ordine
              inspect 02D16-num-ordine replacing leading x"30" by x"20"
              call "C$JUSTIFY" using 02D16-num-ordine, "L"
           else
              move spaces          to 02D16-num-ordine
           end-if.                                   
           if tor-data-ordine not = 0
              move tor-data-ordine to 02D17-data-ordine
           else
              move spaces          to 02D17-data-ordine
           end-if.     
           
           initialize 02D18-num-fattura 02D19-data-fattura
                      02D113-num-bolla  02D114-data-bolla.
           evaluate true
           when nota-credito-qta
           when nota-credito-prz 
           when nota-credito-reso
                if tno-bolla-from-data   not = 0 and
                   tno-bolla-from-numero not = spaces
                   move tno-bolla-from-numero to 02D113-num-bolla
                   move tno-bolla-from-data   to 02D114-data-bolla
                else                                              
                   if tno-fattura-from-data   not = 0 and
                      tno-fattura-from-numero not = 0 
                      move tno-fattura-from-numero to 02D18-num-fattura
                      inspect 02D18-num-fattura 
                              replacing leading x"30" by x"20"
                      call "C$JUSTIFY" using 02D18-num-fattura, "L"
                      move tno-fattura-from-data to 02D19-data-fattura
                   end-if
                end-if
           when fattura-manuale
                |Il numero di fattura essendo alfa, non richiede conversione
                move tor-fattura-from-numero to 02D18-num-fattura
                move tor-fattura-from-data   to 02D19-data-fattura
           end-evaluate.                                        
                                                             

           multiply ror-num-riga by 100 giving ror-num-riga.
           move ror-num-riga       to 02D20-prog-riga

           move ror-cod-articolo   to art-codice 
           read articoli no lock invalid continue end-read

           move art-codice to 02d21-codfortu.
           move "SA"       to 02d22-codfortu-q.

           move spaces to 02D24-q-cod-ean 02D23-cod-ean.
           if art-codice-ean-1 not = spaces
              move art-codice-ean-1 to 02D23-cod-ean
           else
              if art-codice-ean-2 not = spaces      
                 move art-codice-ean-2 to 02D23-cod-ean
              else
                 if art-codice-ean-3 not = spaces
                    move art-codice-ean-3 to 02D23-cod-ean
                 else
                    if art-codice-ean-4 not = spaces
                       move art-codice-ean-4 to 02D23-cod-ean
                    else
                       if art-codice-ean-5 not = spaces
                          move art-codice-ean-4 to 02D23-cod-ean
                       end-if
                    end-if
                 end-if
              end-if
           end-if.
           if 02D23-cod-ean not = spaces
              move "EN" to 02D24-q-cod-ean 
           end-if.
           move art-descrizione    to 02D27-art-descrizione.

      * I campi vanno sempre valorizzati in positivo, sarà Carrefour 
      * a gestirli correttamente in base alla causale come da
      * indicazione del 10/09/2013

      *****     if tca-imponibile-pos             
              move "+"             to 02D31-qta-segno     

      ******  Non valorizzaato come da indicazione Carrefour 05/09/2013
      ******  ABILITATO PER ALLINEARE (24/10/2014)
      *****        if cli-gdo = "CAREF"
      *****           move spaces       to 02D37-prz-segno
      *****        else
                 move "+"          to 02D37-prz-segno
      *****        end-if

              move "+"             to 02D34-CUTU-segno
              move "+"             to 02D40-prz-segno2
              move "+"             to 02D45-prz-tot-segno     
              move "+"             to 02D48-perce-iva-segno
      *****     else                                               
      *****        move "-"             to 02D31-qta-segno     
      ******  Non valorizzaato come da indicazione Carrefour 05/09/2013
      **********        move "-"             to 02D37-prz-segno
      *****        move "-"             to 02D40-prz-segno2
      *****        move "-"             to 02D45-prz-tot-segno     
      *****        move "-"             to 02D48-perce-iva-segno
      *****        move "-"             to 02D121-cat-imp-oli-segno
      *****     end-if   
           move ror-qta     to como-numero.
           perform TRATTA-NUMERICO.
           move NumericEDI  to 02D32-qta. 

           move 0 to imq-qta-imb.
           move ror-prg-tipo-imballo to imq-codice.
           read timbalqta no lock invalid continue end-read.
           move imq-qta-imb  to como-numero.
           perform TRATTA-NUMERICO.
           move NumericEDI   to 02D35-CUTU. 

           move "IV"        to tbliv-codice1
           move ror-cod-iva to tbliv-codice2
           read tivaese  no lock invalid continue end-read
           move ror-cod-iva to eiv-collegato
           read edi-tiva no lock key eiv-collegato
                invalid 
                move "S"                to eiv-codice
                move tbliv-descrizione1 to eiv-descrizione
           end-read.
           move eiv-descrizione to 02D52-des-iva            
           if ror-qta = 0
              if fattura-manuale
                 compute prz-lordo = ror-imp-consumo   + 
                                     ror-imp-cou-cobat +
                                     ror-add-piombo    +
                                     ror-imponib-merce
              else
                 compute prz-lordo = ror-prz-unitario
              end-if
              move prz-lordo to prz-tot-riga
           else
              subtract ror-qta-omaggi from ror-qta   
              if fattura-manuale
                 compute prz-lordo = ror-imp-consumo   + 
                                     ror-imp-cou-cobat +
                                     ror-add-piombo    +
                                     ror-imponib-merce
              else
                 compute prz-lordo = ror-prz-unitario
              end-if
              compute prz-tot-riga = prz-lordo * ror-qta
           end-if.
           if prz-tot-riga = 0
              move causale-omaggio to 02D30-causale   
           else
              move tca-causale-edi to 02D30-causale
           end-if.                  
                                                     
           if ( ror-imp-consumo + ror-imp-cou-cobat + ror-add-piombo )
              not = 0
              if save-ecd-export-imposte-si
                                                       
                 if ror-imp-consumo not = 0               
                    compute como-numero = ror-imp-consumo
                    perform TRATTA-NUMERICO  
                    move NumericEdi     to 02D122-cat-imp-oli-importo
                    move "I01"          to 02D117-imposta-oli
                    move eiv-codice     to 02D118-cat-imp-oli
                    move "+"            to 02D121-cat-imp-oli-segno
                    move "IMPOSTA CON." to 02D123-cat-descrizione 
                 else
                    move spaces to 02D117-imposta-oli 
                                   02D118-cat-imp-oli
                                   02D121-cat-imp-oli-segno
                                   02D122-cat-imp-oli-importo
                                   02D123-cat-descrizione                                              
                 end-if
                    
                 if ( ror-imp-cou-cobat + ror-add-piombo ) not = 0
                    move "VEJ"      to 02D53-tipo-sconto-1   
                    move "+"        to 02D56-segno-sconto-1                   
                    compute como-numero = ror-imp-cou-cobat +
                                          ror-add-piombo   
                    perform TRATTA-NUMERICO
                    move NumericEdi to 02D57-importo-sconto-1
                    move "IMPOSTA COU/COBAT/ADD.PIOMBO" 
                      to 02D58-descr-sconto-1 
                    move "001" to 02D138-indicatore-sconto-1
                 else     
                    move spaces to 02D53-tipo-sconto-1
                                   02D56-segno-sconto-1                   
                                   02D57-importo-sconto-1
                                   02D58-descr-sconto-1 
                                   02D138-indicatore-sconto-1 
                 end-if
              else         
                 move spaces to 02D53-tipo-sconto-1
                                02D56-segno-sconto-1                   
                                02D57-importo-sconto-1
                                02D58-descr-sconto-1 
                                02D138-indicatore-sconto-1

                 move "+"             to 02D121-cat-imp-oli-segno
                 move "I01"           to 02D117-imposta-oli
                 move eiv-codice      to 02D118-cat-imp-oli
                 compute como-numero = ror-imp-consumo   + 
                                       ror-imp-cou-cobat +
                                       ror-add-piombo  
                 perform TRATTA-NUMERICO
                 move NumericEDI to 02D122-cat-imp-oli-importo
              end-if

           else
              move spaces to 02D53-tipo-sconto-1
                             02D56-segno-sconto-1                   
                             02D57-importo-sconto-1
                             02D58-descr-sconto-1 
                             02D138-indicatore-sconto-1
              
              move spaces to 02D117-imposta-oli 
                             02D118-cat-imp-oli
                             02D121-cat-imp-oli-segno
                             02D122-cat-imp-oli-importo
                             02D123-cat-descrizione
           end-if.

           move prz-tot-riga to como-numero
           perform TRATTA-NUMERICO
           move NumericEDI   to 02D46-prz-tot-valore
           add  prz-tot-riga to tot-imponibile.
                                                        
      ******  Non valorizzaato come da indicazione Carrefour 05/09/2013
      ******  ABILITATO PER ALLINEARE (24/10/2014)
      *****     if cli-gdo = "CAREF"                  
      *****        move spaces to 02D38-prz-valore
      *****        move spaces to 02D39-q-prz
      *****        move spaces to 02D44-qtafatt
      *****        move spaces to 02D164-unimisfatt
      *****     else
              move ror-imponib-merce  to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 02D38-prz-valore
              move "AAA"      to 02D39-q-prz
              move "PCE"      to 02D164-unimisfatt
              move ror-qta    to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 02D44-qtafatt
      *****     end-if.                      


      *    02D41 con il prezzo al lordo di sconti, addebiti ed 
      *    accise varie (il campo 02D40 è il relativo segno) 24/10/2014
      *****     move ror-imponib-merce  to como-numero
           move prz-lordo  to como-numero
           perform TRATTA-NUMERICO
           move NumericEDI to 02D41-prz-valore2

           move tbliv-percentuale to como-numero
           perform TRATTA-NUMERICO
           move NumericEDI to 02D49-perce-iva

           move eiv-codice to 02D47-cod-iva.
           perform VALUTA-IVA.   

           perform SCRIVI-DETTAGLIO.                      
           
OMAGGI     if ror-qta-omaggi not = 0
              move causale-omaggio to 02D30-causale

              move "IV"             to tbliv-codice1
              move tge-cod-iva-omag to tbliv-codice2 ror-cod-iva
                                       
              read tivaese  no lock invalid continue end-read
              move tge-cod-iva-omag to eiv-collegato
              read edi-tiva key eiv-collegato
                   invalid 
                   move "S"                to eiv-codice
                   move tbliv-descrizione1 to eiv-descrizione
              end-read
              move eiv-codice      to 02D47-cod-iva
              move eiv-descrizione to 02D52-des-iva      
              move 0               to prz-tot-riga prz-lordo
              perform VALUTA-IVA
                                                    
              move ror-qta-omaggi     to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI  to 02D32-qta

              move ror-qta-omaggi     to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI  to 02D44-qtafatt

              move 0              to 02D38-prz-valore
              move 0              to 02D41-prz-valore2
              move 0              to 02D46-prz-tot-valore
              move 0              to 02D49-perce-iva
              move 0              to 02D122-cat-imp-oli-importo

              perform SCRIVI-DETTAGLIO
OMAGGI     end-if.

      ***---
       SCRIVI-DETTAGLIO.
           inspect 02D1-tipo-doc                          
                   replacing trailing spaces by low-value
           inspect 02D2-id-edi                            
                   replacing trailing spaces by low-value
           inspect 02D3-q-id-edi                          
                   replacing trailing spaces by low-value
           inspect 02D4-data-doc                          
                   replacing trailing spaces by low-value
           inspect 02D5-num-doc                           
                   replacing trailing spaces by low-value
           inspect 02D6-tipo-rec                          
                   replacing trailing spaces by low-value
           inspect 02D7-cod-dest                          
                   replacing trailing spaces by low-value
           inspect 02D8-q-cod-dest                        
                   replacing trailing spaces by low-value
           inspect 02D9-ragsoc-dest                       
                   replacing trailing spaces by low-value
           inspect 02D10-indirizzo-dest                    
                   replacing trailing spaces by low-value
           inspect 02D11-citta-dest                        
                   replacing trailing spaces by low-value
           inspect 02D12-prov-dest                         
                   replacing trailing spaces by low-value
           inspect 02D13-cap-dest                          
                   replacing trailing spaces by low-value
           inspect 02D14-num-bolla                         
                   replacing trailing spaces by low-value
           inspect 02D15-data-bolla                        
                   replacing trailing spaces by low-value
           inspect 02D16-num-ordine
                   replacing trailing spaces by low-value
           inspect 02D17-data-ordine
                   replacing trailing spaces by low-value
           inspect 02D18-num-fattura                         
                   replacing trailing spaces by low-value
           inspect 02D19-data-fattura                        
                   replacing trailing spaces by low-value 
           inspect 02D20-prog-riga                         
                   replacing trailing spaces by low-value
           inspect 02d21-codfortu                         
                   replacing trailing spaces by low-value
           inspect 02D22-codfortu-q                      
                   replacing trailing spaces by low-value
           inspect 02D23-cod-ean                         
                   replacing trailing spaces by low-value
           inspect 02D24-q-cod-ean                         
                   replacing trailing spaces by low-value
           inspect 02D27-art-descrizione                   
                   replacing trailing spaces by low-value
           inspect 02D30-causale                           
                   replacing trailing spaces by low-value
           inspect 02D31-qta-segno                         
                   replacing trailing spaces by low-value
           inspect 02D32-qta                               
                   replacing trailing spaces by low-value
           inspect 02D33-unimis                         
                   replacing trailing spaces by low-value
           inspect 02D34-CUTU-segno
                   replacing trailing spaces by low-value
           inspect 02D35-CUTU
                   replacing trailing spaces by low-value
           inspect 02D37-prz-segno                         
                   replacing trailing spaces by low-value
           inspect 02D38-prz-valore                        
                   replacing trailing spaces by low-value
           inspect 02D39-q-prz                             
                   replacing trailing spaces by low-value
           inspect 02D40-prz-segno2                         
                   replacing trailing spaces by low-value
           inspect 02D41-prz-valore2                        
                   replacing trailing spaces by low-value
           inspect 02D42-q-prz2
                   replacing trailing spaces by low-value
           inspect 02D43-unimis                            
                   replacing trailing spaces by low-value
           inspect 02D44-qtafatt
                   replacing trailing spaces by low-value
           inspect 02D45-prz-tot-segno                     
                   replacing trailing spaces by low-value
           inspect 02D46-prz-tot-valore                    
                   replacing trailing spaces by low-value
           inspect 02D47-cod-iva                           
                   replacing trailing spaces by low-value
           inspect 02D48-perce-iva-segno                   
                   replacing trailing spaces by low-value
           inspect 02D49-perce-iva                         
                   replacing trailing spaces by low-value
           inspect 02D52-des-iva                           
                   replacing trailing spaces by low-value.
           inspect 02D53-tipo-sconto-1        
                   replacing trailing spaces by low-value.
           inspect 02D56-segno-sconto-1       
                   replacing trailing spaces by low-value.
           inspect 02D57-importo-sconto-1     
                   replacing trailing spaces by low-value.
           inspect 02D58-descr-sconto-1       
                   replacing trailing spaces by low-value.
           inspect 02D113-num-bolla
                   replacing trailing spaces by low-value.
           inspect 02D114-data-bolla
                   replacing trailing spaces by low-value.
           inspect 02D117-imposta-oli                       
                   replacing trailing spaces by low-value.
           inspect 02D118-cat-imp-oli                       
                   replacing trailing spaces by low-value.
           inspect 02D121-cat-imp-oli-segno                 
                   replacing trailing spaces by low-value.
           inspect 02D122-cat-imp-oli-importo 
                   replacing trailing spaces by low-value.
           inspect 02D123-cat-descrizione
                   replacing trailing spaces by low-value.  
           inspect 02D164-unimisfatt  
                   replacing trailing spaces by low-value.
           inspect 02D138-indicatore-sconto-1
                   replacing trailing spaces by low-value.
           initialize line-riga.
           string 02D1-tipo-doc              delimited low-value
                  separatore                 delimited size
                  02D2-id-edi                delimited low-value
                  separatore                 delimited size
                  02D3-q-id-edi              delimited low-value
                  separatore                 delimited size
                  02D4-data-doc              delimited low-value
                  separatore                 delimited size
                  02D5-num-doc               delimited low-value
                  separatore                 delimited size
                  02D6-tipo-rec              delimited low-value
                  separatore                 delimited size
                  02D7-cod-dest              delimited low-value
                  separatore                 delimited size
                  02D8-q-cod-dest            delimited low-value
                  separatore                 delimited size
                  02D9-ragsoc-dest           delimited low-value
                  separatore                 delimited size
                  02D10-indirizzo-dest       delimited low-value
                  separatore                 delimited size
                  02D11-citta-dest           delimited low-value
                  separatore                 delimited size
                  02D12-prov-dest            delimited low-value
                  separatore                 delimited size
                  02D13-cap-dest             delimited low-value
                  separatore                 delimited size
                  02D14-num-bolla            delimited low-value
                  separatore                 delimited size
                  02D15-data-bolla           delimited low-value
                  separatore                 delimited size     
                  02D16-num-ordine           delimited low-value
                  separatore                 delimited size     
                  02D17-data-ordine          delimited low-value
                  separatore                 delimited size
                  02D18-num-fattura          delimited low-value
                  separatore                 delimited size     
                  02D19-data-fattura         delimited low-value
                  separatore                 delimited size
                  02D20-prog-riga            delimited low-value
                  separatore                 delimited size     
                  02d21-codfortu             delimited low-value
                  separatore                 delimited size     
                  02d22-codfortu-q           delimited low-value
                  separatore                 delimited size
                  02D23-cod-ean              delimited low-value
                  separatore                 delimited size
                  02D24-q-cod-ean            delimited low-value
                  separatore                 delimited size     
                  separatore                 delimited size
                  separatore                 delimited size
                  02D27-art-descrizione      delimited low-value
                  separatore                 delimited size
                  separatore                 delimited size
                  separatore                 delimited size
                  02D30-causale              delimited low-value
                  separatore                 delimited size
                  02D31-qta-segno            delimited low-value
                  separatore                 delimited size
                  02D32-qta                  delimited low-value
                  separatore                 delimited size
                  02D33-unimis               delimited size
                  separatore                 delimited size
                  02D34-CUTU-segno           delimited low-value
                  separatore                 delimited size
                  02D35-CUTU                 delimited low-value
                  separatore                 delimited size
                  separatore                 delimited size
                  02D37-prz-segno            delimited low-value
                  separatore                 delimited size
                  02D38-prz-valore           delimited low-value
                  separatore                 delimited size
                  02D39-q-prz                delimited low-value
                  separatore                 delimited size     
                  02D40-prz-segno2           delimited low-value
                  separatore                 delimited size     
                  02D41-prz-valore2          delimited low-value
                  separatore                 delimited size     
                  02D42-q-prz2               delimited low-value
                  separatore                 delimited size
                  02D43-unimis               delimited low-value
                  separatore                 delimited size     
                  02D44-qtafatt              delimited low-value
                  separatore                 delimited size
                  02D45-prz-tot-segno        delimited low-value
                  separatore                 delimited size
                  02D46-prz-tot-valore       delimited low-value
                  separatore                 delimited size
                  02D47-cod-iva              delimited low-value
                  separatore                 delimited size
                  02D48-perce-iva-segno      delimited low-value
                  separatore                 delimited size
                  02D49-perce-iva            delimited low-value
                  separatore                 delimited size
                  separatore                 delimited size
                  separatore                 delimited size
                  02D52-des-iva              delimited low-value
                  separatore                 delimited size
                  02D53-tipo-sconto-1        delimited low-value
                  separatore                 delimited size
                  separatore                 delimited size
                  separatore                 delimited size
                  02D56-segno-sconto-1       delimited low-value
                  separatore                 delimited size
                  02D57-importo-sconto-1     delimited low-value
                  separatore                 delimited size
                  02D58-descr-sconto-1       delimited low-value
                  separatore                 delimited size
                  02D-middle1                delimited size 
                  02D113-num-bolla           delimited low-value
                  separatore                 delimited size
                  02D114-data-bolla          delimited low-value
                  separatore                 delimited size
                  02D115-umcons              delimited size
                  separatore                 delimited size
                  separatore                 delimited size
                  02D117-imposta-oli         delimited low-value                   
                  separatore                 delimited size   
                  02D118-cat-imp-oli         delimited low-value                   
                  separatore                 delimited size   
                  separatore                 delimited size   
                  separatore                 delimited size   
                  02D121-cat-imp-oli-segno   delimited low-value                   
                  separatore                 delimited size   
                  02D122-cat-imp-oli-importo delimited low-value
                  separatore                 delimited size   
                  02D123-cat-descrizione     delimited low-value
                  separatore                 delimited size
                  02D-middle2                delimited size
                  02D138-indicatore-sconto-1 delimited low-value
                  separatore                 delimited size
                  02D-middle3                delimited size
                  02D164-unimisfatt          delimited low-value
                  separatore                 delimited size
                  end-02D                    delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       TRATTA-NUMERICO.
           initialize NumericEDI.
           if como-numero = 0 or = spaces
              move "0" to NumericEDI
              inspect NumericEDI replacing trailing spaces by low-value
              exit paragraph
           end-if.
           move como-numero to como-numero-x.
           inspect como-int replacing leading x"30" by x"20".
           call "C$JUSTIFY" using como-int, "L".
           if como-int = spaces
              move "0" to como-int
           end-if.
           inspect como-int replacing trailing spaces by low-value.

           if como-dec not = "00000" and not = spaces
              inspect como-dec replacing trailing x"30" by low-value
              string como-int delimited low-value
                     "."      delimited size
                     como-dec delimited low-value
                     into NumericEDI
              end-string
           else
              move como-int to NumericEDI
           end-if.           
           inspect NumericEDI replacing trailing spaces by low-value.

      ***---
       ELABORAZIONE-NC.                     
           if RichiamoSchedulato
              move "INIZIO ELABORAZIONE NOTE CREDITO" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.   

           move edi-anno   to tno-anno-fattura.
           move edi-num-da to tno-num-fattura.
           start tnotacr key  is >= k-fattura
                 invalid
                 if not RichiamoSchedulato
                    display message "Nessun documento da inviare!"
                              title titolo
                               icon 2
                 else
                    move "NESSUNA NOTA CR. ELABORATA" to como-riga
                    perform SCRIVI-RIGA-LOG
                 end-if
                 set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tnotacr next at end exit perform end-read
                 if tno-num-fattura > edi-num-a
                    exit perform
                 end-if
                 if tno-anno-fattura not = edi-anno
                    exit perform
                 end-if      
                 set record-ok to false 

                 |13/12/2016: guardare sul cliente non sull'ordine
                 set cli-tipo-C   to true
                 move tno-cod-cli to cli-codice
                 read clienti no lock
                      invalid move spaces to rec-invio
                  not invalid
                      move cli-codice to rec-codice
                      read recapiti no lock
                           invalid move spaces to rec-invio
                      end-read
                 end-read
                 move rec-invio to tno-invio

                 if tno-invio-edi
                    move tno-causale to tca-codice
                    read tcaumag no lock
                         invalid continue
                     not invalid
                         if tca-causale-EDI not = spaces
                            set record-ok to true 
                         end-if
                    end-read
                 end-if
                 if record-ok                                    
                    move tno-cod-cli       to tor-cod-cli
                    move tno-data-fattura  to tor-data-fattura
                    move tno-num-fattura   to tor-num-fattura
                    move tno-cod-pagamento to tor-cod-pagamento
                    evaluate tca-causale-edi
                    when "L07"
                         set nota-credito-qta   to true
                    when "L08"
                         set nota-credito-prz   to true 
                    when "L02"
                         set nota-credito-reso  to true
                    when "XX"
                         set accredito-generico to true
                    end-evaluate
                    move tno-prg-destino to tor-prg-destino

                    if accredito-generico
                       if num-da = 0
                          move tno-num-fattura to num-da
                       end-if
                       move tno-num-fattura to num-a
                       add 1 to edi-tot-pdf 
                       initialize como-riga
                       string "ELABORATA NOTA CRE N. " delimited size
                              tor-num-fattura          delimited size
                              " ADDEBITO GENERICO"     delimited size
                              into como-riga
                       end-string        
                       perform SCRIVI-RIGA-LOG
                    else                      
                       initialize como-riga
                       string "ELABORATA NOTA CRE N. " delimited size
                              tor-num-fattura          delimited size
                              into como-riga
                       end-string
                       perform SCRIVI-RIGA-LOG
      
                       move tno-cod-cli to tor-cod-cli
                       move 0 to tor-data-ordine
                       perform VALORIZZA-EDI-01H-01T
                       perform LOOP-RIGHE-RNOTACR
                       |NON SONO RIUSCITO AD APRIRE IL FILE DI TESTO
                       if errori exit perform end-if  
                       perform VALORIZZA-EDI-PIEDE 
                    end-if

                    if edi-doc-da = 0
                       move tno-num-fattura to edi-doc-da
                    end-if
                    if edi-doc-a = 0
                       move tno-num-fattura to edi-doc-a
                    end-if               
                    add 1 to edi-tot-doc
                    if tno-num-fattura < edi-doc-da
                       move tno-num-fattura to edi-doc-da
                    end-if
                    if tno-num-fattura > edi-doc-a
                       move tno-num-fattura to edi-doc-a
                    end-if                        
                 end-if
                 if errori exit perform end-if
              end-perform
           end-if.

           if tutto-ok
              if edi-tot-doc > 0

                 call "C$CALLEDBY" using CallingPgm
                 if CallingPgm not = "edi-stdoc"
                    move edi-doc-a to con-ult-num-nc-edi
                    rewrite con-rec invalid continue end-rewrite
                 end-if

                 initialize como-riga
                 string "INVIATE "     delimited size
                        edi-tot-doc    delimited size 
                        " NOTE CR. "   delimited size
                        "DA N. "       delimited size
                        edi-doc-da     delimited size
                        " A N. "       delimited size
                        edi-doc-a      delimited size
                        " DI CUI PDF " delimited size
                        edi-tot-pdf    delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG

              else
                 if not RichiamoSchedulato
                    display message "Nessun documento da inviare!"
                              title titolo
                               icon 2
                 else
                    move "NESSUNA NOTA CR. ELABORATA" to como-riga
                    perform SCRIVI-RIGA-LOG
                 end-if
              end-if
           end-if.

      ***---
       LOOP-RIGHE-RNOTACR.
      *****     if visualizza-totali = "S" and righe-finali > 1
      *****        |Guardo i totali delle imposte in maniera preventiva
      *****        |x' devo sapere prima uante righe di totalizzatori
      *****        |finali dovrò poi stampare
      *****        move low-values   to rno-rec
      *****        move tno-anno     to rno-anno
      *****        move tno-numero   to rno-numero
      *****        start rnotacr key is >= rno-chiave
      *****              invalid set errori to true
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read rnotacr  next at end exit perform end-read
      *****                 if rno-anno   not = tno-anno or
      *****                    rno-numero not = tno-numero
      *****                    exit perform
      *****                 end-if
      *****                 move rno-cod-articolo to art-codice
      *****                 read articoli no lock invalid continue end-read
      *****                 if rno-add-piombo > 0
      *****                    set si-piombo to true
      *****                    compute tot-piombo = 
      *****                            tot-piombo +
      *****                            rno-add-piombo  * rno-qta
      *****                 end-if
      *****                 if rno-imp-cou-cobat > 0
      *****                    if art-si-cobat
      *****                       set si-cobat to true
      *****                       compute tot-cobat = 
      *****                               tot-cobat +
      *****                               rno-imp-cou-cobat * rno-qta
      *****                    else
      *****                       set si-cou   to true
      *****                       compute tot-solo-cou = 
      *****                               tot-solo-cou +
      *****                               rno-imp-cou-cobat * rno-qta
      *****                    end-if
      *****                 end-if
      *****              end-perform
      *****        end-start
      *****        if si-piombo add 1 to righe-finali end-if
      *****        if si-cou    add 1 to righe-finali end-if
      *****        if si-cobat  add 1 to righe-finali end-if
      *****     else
      *****        move 0 to righe-finali
      *****     end-if.
      *****
      *****     move 0            to RowCounter.              
           move 0 to tot-iva imponibile-iva(1) imponibile-iva(2)
                     imponibile-iva(3) importo-iva(1) importo-iva(2)
                     importo-iva(3) tot-fattura tot-imponibile tot-merce
           move low-values   to rno-rec.
           move tno-anno     to rno-anno.
           move tno-numero   to rno-numero.
           start rnotacr key is >= rno-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok          
              perform until 1 = 2
                 read rnotacr next at end exit perform end-read
                 if tno-anno   not = rno-anno  or
                    tno-numero not = rno-numero
                    exit perform
                 end-if                       
                 move rno-num-riga      to ror-num-riga
                 move rno-cod-articolo  to ror-cod-articolo 
                 move rno-qta           to ror-qta
                 move rno-imp-consumo   to ror-imp-consumo
                 move rno-imp-cou-cobat to ror-imp-cou-cobat
                 move rno-prz-unitario  to ror-imponib-merce
                 move rno-add-piombo    to ror-add-piombo
                 compute ror-prz-unitario = ror-imponib-merce +
                                            ror-imp-consumo   +
                                            ror-imp-cou-cobat +
                                            ror-add-piombo
                 move rno-qta           to ror-qta  
                 move rno-cod-iva       to ror-cod-iva     
                 move rno-prg-chiave    to ror-prg-chiave
                 move 0 to ror-qta-omaggi                               
                 perform VALORIZZA-EDI-02D
              end-perform
           end-if.

      ********---
      ***** VALORIZZA-POSTEL.
      *****     if not RelazioneTestaGiaFatte
      *****        move 0 to num-righe-note
      *****
      *****        move 0 to  tot-imponibile tot-consumo
      *****                   tot-cou        tot-fattura
      *****                   tot-iva        imponibile-merce
      *****
      *****
      *****        set RelazioneTestaGiaFatte to true
      *****        move como-cod-cli     to cli-codice rec-codice des-codice
      *****        move como-prg-destino to des-prog
      *****        set  cli-tipo-C    to true
      *****        read clienti  no lock invalid continue end-read
      *****
      *****        move cli-codice to cli-codice-G2
      *****        read CLI no lock invalid continue end-read
      *****
      *****        set des-no-invio to true
      *****
      *****        read destini  no lock
      *****             invalid  set EsisteDestino to false
      *****         not invalid  set EsisteDestino to true
      *****        end-read
      *****
      *****        if des-si-invio
      *****           set EsisteRecapito to true
      *****           move des-ragsoc-1  to rec-ragsoc-1
      *****           move des-indirizzo to rec-indirizzo
      *****           move des-cap       to rec-cap
      *****           move des-localita  to rec-localita
      *****           move des-prov      to rec-provincia
      *****        else
      *****           read recapiti no lock
      *****                invalid  set EsisteRecapito to false
      *****            not invalid  
      *****                if rec-ragsoc-1       = spaces and
      *****                   rec-indirizzo      = spaces and
      *****                   rec-cap            = spaces and
      *****                   rec-localita       = spaces and
      *****                   rec-provincia      = spaces
      *****                   set EsisteRecapito to false
      *****                else
      *****                   set EsisteRecapito to true
      *****                end-if
      *****           end-read
      *****        end-if
      *****
      *****        move "PA"               to tblpa-codice1
      *****        move como-cod-pagamento to tblpa-codice2
      *****        read tcodpag no lock invalid continue end-read
      *****
      *****        move como-vettore to vet-codice
      *****        read tvettori     no lock invalid continue end-read
      *****
      *****        move como-cod-agente to age-codice
      *****        read agenti          no lock invalid continue end-read
      *****
      *****        if EsisteRecapito
      *****           move rec-ragsoc-1       to ragio-dx
      *****           move rec-indirizzo      to indi-dx
      *****           move rec-cap            to cap-dx
      *****           move rec-localita       to citta-dx
      *****           move rec-provincia      to prov-dx
      *****           move cli-ragsoc-1       to ragio-sx
      *****           move cli-indirizzo      to indi-sx
      *****           move cli-cap of clienti to cap-sx
      *****           move cli-localita       to citta-sx
      *****        else
      *****           move cli-ragsoc-1       to ragio-dx
      *****           move cli-indirizzo      to indi-dx
      *****           move cli-cap of clienti to cap-dx
      *****           move cli-localita       to citta-dx
      *****           move cli-prov           to prov-dx
      *****           move spaces             to ragio-sx
      *****           move spaces             to indi-sx
      *****           move spaces             to cap-sx
      *****           move spaces             to citta-sx
      *****        end-if
      *****
      *****        move como-num-fattura       to nr-fatt
      *****        move como-data-fattura(3:2) to como-data6(5:2)
      *****        move como-data-fattura(5:2) to como-data6(3:2)
      *****        move como-data-fattura(7:2) to como-data6(1:2)
      *****        move como-data6             to t-data
      *****        move tblpa-descrizione1     to despaga
      *****        move cli-abi                to t-abi
      *****        move cli-cab                to t-cab
      *****        move como-num-ord-cli       to numord-cli
      *****        move como4-data-doc(3:2)     to como-data6(5:2)
      *****        move como4-data-doc(5:2)     to como-data6(3:2)
      *****        move como4-data-doc(7:2)     to como-data6(1:2)
      *****        move como-data6             to dataord-cli
      *****        move como-num-bolla         to numerobolla
      *****        move como-data-bolla(3:2)   to como-data6(5:2)
      *****        move como-data-bolla(5:2)   to como-data6(3:2)
      *****        move como-data-bolla(7:2)   to como-data6(1:2)
      *****        move como-data6             to databolla
      *****        move como-cod-cli           to codcliente
      *****
      *****        move como-cod-agente        to codice-ed
      *****        call "C$JUSTIFY"         using codice-ed, "L"
      *****        move codice-ed              to agente
      *****
      *****        if cli-fisica
      *****           move cli-codfis to clfisc
      *****        else
      *****           move cli-piva   to clfisc
      *****        end-if
      *****
      *****        if EsisteDestino
      *****           move des-ragsoc-1  to dest-1
      *****           move des-indirizzo to dest-2
      *****           move des-localita  to dest-3
      *****        else
      *****           move spaces to dest-1
      *****           move spaces to dest-2
      *****           move spaces to dest-3
      *****        end-if
      *****                                              
      *****                                             
      *****     end-if.
      *****
      *****     initialize record-tbliv
      *****                art-rec 
      *****                replacing numeric by zeroes
      *****                             alphanumeric by spaces.
      *****
      *****     move spaces to rec-mer1.
      *****
      *****     move como-cod-articolo to art-codice.
      *****     read articoli no lock invalid continue end-read.
      *****
      *****     move "IV"          to tbliv-codice1.
      *****     move como-cod-iva  to tbliv-codice2.
      *****     read tivaese no lock invalid continue end-read.
      *****
      *****     if como-cod-articolo = 0
      *****        move como-des-libera to desarticolo
      *****        move spaces          to unimis
      *****     else
      *****        move art-codice      to codiceart
      *****        if edi1-tipo-doc not = 1
      *****           move rno-cod-articolo to ror-cod-articolo
      *****           move tno-prg-destino  to tor-prg-destino
      *****        end-if
      *****        perform TROVA-CODICE-ARTICOLO-ON-ASSORCLI
      *****        if trovato-assorcli
      *****           move art-descrizione              to dex-a
      *****           move "-"                          to dex-s
      *****           move asc-cod-articolo-per-cliente to dex-b
      *****        else
      *****           move art-descrizione              to desarticolo
      *****        end-if
      *****     end-if.
      *****     move como-qta             to quantita.
      *****
      ********---
      ***** VALORIZZA-RIGA.
      *****     move como-imponib-merce   to prezzo.
      *****     move como-imp-consumo     to r-imposta.
      *****     move como-imp-cou-cobat   to r-cou.
      *****
      *****     if como-qta = 0 move 1 to como-qta end-if.
      *****
      *****     compute importo-netto = como-qta *
      *****     (como-imponib-merce + como-imp-consumo + como-imp-cou-cobat).
      *****
      *****     move importo-netto to importo.
      *****
      *****     compute imponibile-merce = imponibile-merce + 
      *****                              ( como-qta * como-imponib-merce ).
      *****     compute tot-consumo      = tot-consumo      + 
      *****                              ( como-qta * como-imp-consumo ).
      *****     compute tot-cou          = tot-cou          + 
      *****                              ( como-qta * como-imp-cou-cobat ). 
      *****     compute tot-imponibile   = tot-imponibile   + importo-netto.
      *****
      *****     move 0 to como-iva.
      *****
      *****     if tbliv-percentuale not = 0
      *****        move tbliv-percentuale to perce-iva-9di3
      *****        move perce-iva-9di3    to perce-iva-x
      *****        call "C$JUSTIFY" using perce-iva-x, "R"
      *****        inspect perce-iva-x replacing leading x"30" by x"20"
      *****     else
      *****        move tbliv-codice2     to perce-iva-x
      *****     end-if.
      *****     
      *****     move         1 to idx.
      *****     set TrovataIVA to false.
      *****     perform until idx > 3
      *****        if cod-iva(idx) = perce-iva-x
      *****           set TrovataIVA to true
      *****           exit perform
      *****        end-if
      *****        add  1 to idx
      *****     end-perform.
      *****
      *****     if not TrovataIVA
      *****        evaluate true
      *****        when cod-iva(1) = spaces move perce-iva-x to cod-iva(1)
      *****                                 move 1 to idx
      *****        when cod-iva(2) = spaces move perce-iva-x to cod-iva(2)
      *****                                 move 2 to idx
      *****        when cod-iva(3) = spaces move perce-iva-x to cod-iva(3)
      *****                                 move 3 to idx
      *****        end-evaluate
      *****        if tbliv-percentuale = 0 set iva-sigla(idx) to true
      *****        else                     set iva-sigla(idx) to false
      *****        end-if
      *****     end-if.
      *****
      *****     compute imponibile-iva(idx) = 
      *****             imponibile-iva(idx) + importo-netto.
      *****
      *****     move tbliv-percentuale to iva-z.
      *****
      *****     move iva-z to iva.
      *****
      ********---
      ***** VALORIZZA-edi-TOTALI.
      *****     set RigheFinali to true.
      *****     if si-cou
      *****        move 0                           to art-codice
      *****                                            como-cod-articolo
      *****        move "T O T A L E  C. O. U. "    to como-des-libera
      *****        move tot-solo-cou                to como-valore
      *****        perform VALORIZZA-POSTEL
      *****     end-if.
      *****
      *****     if si-cobat
      *****        move 0                           to art-codice
      *****                                            como-cod-articolo
      *****        move "T O T A L E  C O B A T"    to como-des-libera
      *****        move tot-cobat                   to como-valore
      *****        perform VALORIZZA-POSTEL
      *****     end-if.
      *****
      *****     if si-piombo
      *****        move 0                           to art-codice
      *****                                            como-cod-articolo
      *****        move "T O T A L E  P I O M B O"  to como-des-libera
      *****        move tot-piombo                  to como-valore
      *****        perform VALORIZZA-POSTEL
      *****     end-if.
      *****     set RigheFinali to false.        
      *****
      ********---
      ***** VALORIZZA-edi-PIEDE.
      *****     perform CALCOLA-IVA.
      *****
      *****     move imponibile-merce to st-importo.
      *****     perform VALUTA-IVA.
      *****     move tot-imponibile   to st-importo-totale st-importo-totale2
      *****
      *****     if UsaPrimaRiga
      *****        move st-importo        to impo-gdo
      *****        move st-aliquota-tot   to iva1
      *****        move st-imponibile     to impon1
      *****        move st-importo-iva    to impor1
      *****        move st-importo-totale to totfat
      *****        write line-riga from rec-pie1 after 2
      *****     else
      *****        move st-importo2        to ximpo-gdo
      *****        move st-aliquota-tot2   to xiva1
      *****        move st-imponibile2     to ximpon1
      *****        move articolo-iva(1)    to dex-iva
      *****        move st-importo-totale2 to xtotfat
      *****        write line-riga from rec-pie1-x after 2
      *****     end-if.
      *****
      *****     add tot-imponibile  to tot-iva giving tot-fattura.
      *****     move tot-consumo    to st-importo.
      *****     perform VALUTA-IVA.
      *****     move tot-iva        to st-importo-totale st-importo-totale2.
      *****
      *****     if UsaPrimaRiga
      *****        move st-importo        to impo-gdo
      *****        move st-aliquota-tot   to iva1
      *****        move st-imponibile     to impon1
      *****        move st-importo-iva    to impor1
      *****        move st-importo-totale to totfat
      *****        write line-riga from rec-pie1 after 2
      *****     else
      *****        move st-importo2        to ximpo-gdo
      *****        move st-aliquota-tot2   to xiva1
      *****        move st-imponibile2     to ximpon1
      *****        move articolo-iva(2)    to dex-iva
      *****        move st-importo-totale2 to xtotfat
      *****        write line-riga from rec-pie1-x after 2
      *****     end-if.
      *****     
      *****     add tot-imponibile  to tot-iva giving tot-fattura.
      *****     move tot-cou        to st-importo.
      *****     perform VALUTA-IVA.
      *****     move tot-fattura    to st-importo-totale st-importo-totale2.
      *****
      *****     if UsaPrimaRiga
      *****        move st-importo        to impo-gdo
      *****        move st-aliquota-tot   to iva1
      *****        move st-imponibile     to impon1
      *****        move st-importo-iva    to impor1
      *****        move st-importo-totale to totfat
      *****        write line-riga from rec-pie1 after 2
      *****     else
      *****        move st-importo2        to ximpo-gdo
      *****        move st-aliquota-tot2   to xiva1
      *****        move st-imponibile2     to ximpon1
      *****        move articolo-iva(3)    to dex-iva
      *****        move st-importo-totale2 to xtotfat
      *****        write line-riga from rec-pie1-x after 2
      *****     end-if.

      ***---
       VALORIZZA-EDI-PIEDE.                
           initialize 03P16-cod-iva-1      03P17-aliq-iva-segno-1
                      03P18-aliq-iva-1     03P19-impon-segno-1
                      03P20-impon-valore-1 03P21-iva-segno-1
                      03P22-iva-valore-1   03P23-iva-descr-1
                      03P15-tipo-tassa-1.
           initialize 03P25-cod-iva-2      03P26-aliq-iva-segno-2
                      03P27-aliq-iva-2     03P28-impon-segno-2
                      03P29-impon-valore-2 03P30-iva-segno-2
                      03P31-iva-valore-2   03P32-iva-descr-2
                      03P24-tipo-tassa-2.
           initialize 03P34-cod-iva-3      03P35-aliq-iva-segno-3
                      03P36-aliq-iva-3     03P37-impon-segno-3
                      03P38-impon-valore-3 03P39-iva-segno-3
                      03P40-iva-valore-3   03P41-iva-descr-3
                      03P33-tipo-tassa-3.
      
           move    1 to idx.
           perform 3 times
              move "IV"         to tbliv-codice1
              move cod-iva(idx) to tbliv-codice2
              read tivaese no lock 
                   invalid move 0 to tbliv-percentuale
              end-read
              move 0 to como-iva
              compute como-iva = 
                ( ( imponibile-iva(idx) * tbliv-percentuale ) / 100 )
              add 0,005          to como-iva
              move como-iva      to como-iva-2dec
              move como-iva-2dec to importo-iva(idx)
              add 1 to idx
           end-perform.

           compute tot-iva = importo-iva(1) +
                             importo-iva(2) +
                             importo-iva(3).

           compute tot-fattura = tot-iva + tot-imponibile.
                                                 
           move 01T1-tipo-doc       to 03P1-tipo-doc
           move 01T2-id-edi         to 03P2-id-edi
           move 01T3-q-id-edi       to 03P3-q-id-edi
           move 01T4-data-doc       to 03P4-data-doc
           move 01T5-num-doc        to 03P5-num-doc.

      * I campi vanno sempre valorizzati in positivo, sarà Carrefour 
      * a gestirli correttamente in base alla causale come da
      * indicazione del 10/09/2013
      *****     if tca-imponibile-pos             
              move "+"             to 03P7-tot-doc-segno
              move "+"             to 03P9-tot-imp-segno 
              move "+"             to 03P11-tot-iva-segno
              move "+"             to 03P13-tot-merce-segno
      *****     else                                        
      *****        move "-"             to 03P7-tot-doc-segno
      *****        move "-"             to 03P9-tot-imp-segno
      *****        move "-"             to 03P11-tot-iva-segno
      *****        move "-"             to 03P13-tot-merce-segno
      *****     end-if.
           
           move tot-fattura to como-numero.
           perform TRATTA-NUMERICO.
           move NumericEDI to 03P8-tot-doc-valore.

           move tot-imponibile to como-numero.
           perform TRATTA-NUMERICO.
           move NumericEDI to 03P10-tot-imp-valore.

           move tot-iva to como-numero.
           perform TRATTA-NUMERICO.
           move NumericEDI to 03P12-tot-iva-valore.

      *****     move tot-merce to como-numero.
           move tot-imponibile to como-numero.
           perform TRATTA-NUMERICO.
           move NumericEDI to 03P14-tot-merce-valore.

           if cod-iva(1) not = spaces
              move "VAT"      to 03P15-tipo-tassa-1
              move "IV"       to tbliv-codice1
              move cod-iva(1) to tbliv-codice2
              read tivaese no lock      
              
      * I campi vanno sempre valorizzati in positivo, sarà Carrefour 
      * a gestirli correttamente in base alla causale come da
      * indicazione del 10/09/2013
      *****        if tca-imponibile-pos
                 move "+"  to 03P17-aliq-iva-segno-1
                 move "+"  to 03P19-impon-segno-1
                 move "+"  to 03P21-iva-segno-1
      *****        else
      *****           move "-"  to 03P17-aliq-iva-segno-1
      *****           move "-"  to 03P19-impon-segno-1
      *****           move "-"  to 03P21-iva-segno-1
      *****        end-if
              move tbliv-percentuale to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 03P18-aliq-iva-1
              move imponibile-iva(1) to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 03P20-impon-valore-1
              move importo-iva(1) to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 03P22-iva-valore-1        
              move cod-iva(1) to eiv-collegato   
              read edi-tiva key  eiv-collegato
                   invalid
                   move "S"                to eiv-codice
                   move tbliv-descrizione1 to eiv-descrizione
              end-read
              move eiv-descrizione to 03P23-iva-descr-1
              move eiv-codice      to 03P16-cod-iva-1
           end-if.                                

           if cod-iva(2) not = spaces            
              move "VAT"      to 03P24-tipo-tassa-2    
              move "IV"       to tbliv-codice1
              move cod-iva(2) to tbliv-codice2
              read tivaese no lock      
              
      * I campi vanno sempre valorizzati in positivo, sarà Carrefour 
      * a gestirli correttamente in base alla causale come da
      * indicazione del 10/09/2013
      *****        if tca-imponibile-pos
                 move "+"  to 03P26-aliq-iva-segno-2
                 move "+"  to 03P28-impon-segno-2
                 move "+"  to 03P30-iva-segno-2
      *****        else
      *****           move "-"  to 03P26-aliq-iva-segno-2
      *****           move "-"  to 03P28-impon-segno-2
      *****           move "-"  to 03P30-iva-segno-2
      *****        end-if
              move tbliv-percentuale to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 03P27-aliq-iva-2
              move imponibile-iva(2) to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 03P29-impon-valore-2
              move importo-iva(2) to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 03P31-iva-valore-2
              move cod-iva(2) to eiv-collegato
              read edi-tiva key  eiv-collegato               
                   invalid
                   move "S"                to eiv-codice
                   move tbliv-descrizione1 to eiv-descrizione
              end-read
              move eiv-descrizione to 03P32-iva-descr-2        
              move eiv-codice      to 03P25-cod-iva-2
           end-if.                                   

           if cod-iva(3) not = spaces            
              move "VAT"      to 03P33-tipo-tassa-3
              move "IV"       to tbliv-codice1
              move cod-iva(3) to tbliv-codice2
              read tivaese no lock

      * I campi vanno sempre valorizzati in positivo, sarà Carrefour 
      * a gestirli correttamente in base alla causale come da
      * indicazione del 10/09/2013
      *****        if tca-imponibile-pos
                 move "+"  to 03P35-aliq-iva-segno-3
                 move "+"  to 03P37-impon-segno-3
                 move "+"  to 03P39-iva-segno-3
      *****        else
      *****           move "-"  to 03P35-aliq-iva-segno-3
      *****           move "-"  to 03P37-impon-segno-3
      *****           move "-"  to 03P39-iva-segno-3
      *****        end-if
              move tbliv-percentuale to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 03P36-aliq-iva-3
              move imponibile-iva(3) to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 03P38-impon-valore-3
              move importo-iva(3) to como-numero
              perform TRATTA-NUMERICO
              move NumericEDI to 03P40-iva-valore-3
              move cod-iva(3) to eiv-collegato
              read edi-tiva key  eiv-collegato
                   invalid
                   move "S"                to eiv-codice
                   move tbliv-descrizione1 to eiv-descrizione
              end-read
              move eiv-descrizione to 03P41-iva-descr-3
              move eiv-codice      to 03P34-cod-iva-3
           end-if.         
           inspect 03P7-tot-doc-segno    
                   replacing trailing spaces by low-value.
           inspect 03P8-tot-doc-valore                     
                   replacing trailing spaces by low-value.
           inspect 03P9-tot-imp-segno                      
                   replacing trailing spaces by low-value.
           inspect 03P10-tot-imp-valore                     
                   replacing trailing spaces by low-value.
           inspect 03P11-tot-iva-segno                      
                   replacing trailing spaces by low-value.
           inspect 03P12-tot-iva-valore                     
                   replacing trailing spaces by low-value.
           inspect 03P13-tot-merce-segno                      
                   replacing trailing spaces by low-value.
           inspect 03P14-tot-merce-valore                     
                   replacing trailing spaces by low-value.
           inspect 03P15-tipo-tassa-1                          
                   replacing trailing spaces by low-value.
           inspect 03P16-cod-iva-1                          
                   replacing trailing spaces by low-value.
           inspect 03P17-aliq-iva-segno-1                   
                   replacing trailing spaces by low-value.
           inspect 03P18-aliq-iva-1                         
                   replacing trailing spaces by low-value.
           inspect 03P19-impon-segno-1                      
                   replacing trailing spaces by low-value.
           inspect 03P20-impon-valore-1                     
                   replacing trailing spaces by low-value.
           inspect 03P21-iva-segno-1                        
                   replacing trailing spaces by low-value.
           inspect 03P22-iva-valore-1                       
                   replacing trailing spaces by low-value.
           inspect 03P23-iva-descr-1                        
                   replacing trailing spaces by low-value.
           inspect 03P24-tipo-tassa-2                          
                   replacing trailing spaces by low-value.
           inspect 03P25-cod-iva-2                          
                   replacing trailing spaces by low-value.
           inspect 03P26-aliq-iva-segno-2                   
                   replacing trailing spaces by low-value.
           inspect 03P27-aliq-iva-2                         
                   replacing trailing spaces by low-value.
           inspect 03P28-impon-segno-2                      
                   replacing trailing spaces by low-value.
           inspect 03P29-impon-valore-2                     
                   replacing trailing spaces by low-value.
           inspect 03P30-iva-segno-2                        
                   replacing trailing spaces by low-value.
           inspect 03P31-iva-valore-2                       
                   replacing trailing spaces by low-value.
           inspect 03P32-iva-descr-2                        
                   replacing trailing spaces by low-value.
           inspect 03P33-tipo-tassa-3                          
                   replacing trailing spaces by low-value.
           inspect 03P34-cod-iva-3                          
                   replacing trailing spaces by low-value.
           inspect 03P35-aliq-iva-segno-3                   
                   replacing trailing spaces by low-value.
           inspect 03P36-aliq-iva-3                         
                   replacing trailing spaces by low-value.
           inspect 03P37-impon-segno-3                      
                   replacing trailing spaces by low-value.
           inspect 03P38-impon-valore-3                     
                   replacing trailing spaces by low-value.
           inspect 03P39-iva-segno-3                        
                   replacing trailing spaces by low-value.
           inspect 03P40-iva-valore-3                       
                   replacing trailing spaces by low-value.
           inspect 03P41-iva-descr-3                        
                   replacing trailing spaces by low-value.

           initialize line-riga.
           string 03P1-tipo-doc          delimited low-value
                  separatore             delimited size
                  03P2-id-edi            delimited low-value
                  separatore             delimited size
                  03P3-q-id-edi          delimited low-value
                  separatore             delimited size
                  03P4-data-doc          delimited low-value
                  separatore             delimited size
                  03P5-num-doc           delimited low-value
                  separatore             delimited size
                  03P6-tipo-rec          delimited low-value
                  separatore             delimited size
                  03P7-tot-doc-segno     delimited low-value
                  separatore             delimited size
                  03P8-tot-doc-valore    delimited low-value
                  separatore             delimited size
                  03P9-tot-imp-segno     delimited low-value
                  separatore             delimited size
                  03P10-tot-imp-valore   delimited low-value
                  separatore             delimited size
                  03P11-tot-iva-segno    delimited low-value
                  separatore             delimited size
                  03P12-tot-iva-valore   delimited low-value
                  separatore             delimited size     
                  03P13-tot-merce-segno  delimited low-value
                  separatore             delimited size
                  03P14-tot-merce-valore delimited low-value
                  separatore             delimited size
                  03P15-tipo-tassa-1     delimited low-value
                  separatore             delimited size
                  03P16-cod-iva-1        delimited low-value
                  separatore             delimited size
                  03P17-aliq-iva-segno-1 delimited low-value
                  separatore             delimited size
                  03P18-aliq-iva-1       delimited low-value
                  separatore             delimited size
                  03P19-impon-segno-1    delimited low-value
                  separatore             delimited size
                  03P20-impon-valore-1   delimited low-value
                  separatore             delimited size
                  03P21-iva-segno-1      delimited low-value
                  separatore             delimited size
                  03P22-iva-valore-1     delimited low-value
                  separatore             delimited size
                  03P23-iva-descr-1      delimited low-value
                  separatore             delimited size
                  03P24-tipo-tassa-2     delimited low-value
                  separatore             delimited size
                  03P25-cod-iva-2        delimited low-value
                  separatore             delimited size
                  03P26-aliq-iva-segno-2 delimited low-value
                  separatore             delimited size
                  03P27-aliq-iva-2       delimited low-value
                  separatore             delimited size
                  03P28-impon-segno-2    delimited low-value
                  separatore             delimited size
                  03P29-impon-valore-2   delimited low-value
                  separatore             delimited size
                  03P30-iva-segno-2      delimited low-value
                  separatore             delimited size
                  03P31-iva-valore-2     delimited low-value
                  separatore             delimited size
                  03P32-iva-descr-2      delimited low-value
                  separatore             delimited size
                  03P33-tipo-tassa-3     delimited low-value
                  separatore             delimited size
                  03P34-cod-iva-3        delimited low-value
                  separatore             delimited size
                  03P35-aliq-iva-segno-3 delimited low-value
                  separatore             delimited size
                  03P36-aliq-iva-3       delimited low-value
                  separatore             delimited size
                  03P37-impon-segno-3    delimited low-value
                  separatore             delimited size
                  03P38-impon-valore-3   delimited low-value
                  separatore             delimited size
                  03P39-iva-segno-3      delimited low-value
                  separatore             delimited size
                  03P40-iva-valore-3     delimited low-value
                  separatore             delimited size
                  03P41-iva-descr-3      delimited low-value
                  separatore             delimited size     
                  03P-middle             delimited size
                  03P90-note             delimited low-value
                  03P-end-riga           delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       VALUTA-IVA. 
           set TrovataIVA to false.
           if cod-iva(1) = ror-cod-iva
              set TrovataIVA to true
              move ror-cod-iva to cod-iva(1)
              add prz-tot-riga to imponibile-iva(1)
           else
              if cod-iva(2) = ror-cod-iva   
                 set TrovataIVA to true
                 move ror-cod-iva to cod-iva(2)
                 add prz-tot-riga to imponibile-iva(2)
              else                                    
                 if cod-iva(3) = ror-cod-iva
                    set TrovataIVA to true
                    move ror-cod-iva to cod-iva(3)
                    add prz-tot-riga to imponibile-iva(3)
                 end-if
              end-if
           end-if
          
           if not TrovataIVA
              if cod-iva(1) = spaces
                 move ror-cod-iva to cod-iva(1)
                 add prz-tot-riga to imponibile-iva(1)
              else
                 if cod-iva(2) = spaces               
                    move ror-cod-iva to cod-iva(2)
                    add prz-tot-riga to imponibile-iva(2)
                 else                                    
                    if cod-iva(3) = spaces               
                       move ror-cod-iva to cod-iva(3)
                       add prz-tot-riga to imponibile-iva(3)
                    end-if
                 end-if
              end-if
           end-if.
    
      ***---
       VALORIZZA-EDI-01H-01T.
           set cli-tipo-C to true.
           move tor-cod-cli to cli-codice.
           read clienti no lock invalid initialize cli-rec end-read.

           if prima-volta
              set  prima-volta to false
              open output lineseq
              if errori 
                 if RichiamoSchedulato
                    move "ERRORE CREAZIONE FILE EXPORT" to como-riga
                    perform SCRIVI-RIGA-LOG
                 end-if
                 exit paragraph 
              end-if
              set FileCreato to true
           end-if.

           move "PA"              to tblpa-codice1.
           move tor-cod-pagamento to tblpa-codice2.
           read tcodpag no lock
                invalid move spaces to tblpa-descrizione1
           end-read.

           move tor-cod-cli     to ecd-cli-codice.
           move 0               to ecd-prg-destino.
           read edi-clides no lock 
                invalid initialize ecd-rec
           end-read.
           |Devo considerare quello del cliente quindi lo salvo
           move ecd-export-imposte to save-ecd-export-imposte.
           evaluate true
           when nota-credito-qta
           when nota-credito-prz
           when nota-credito-reso
                move "381" to 01H1-tipo-doc 01T1-tipo-doc
           when fattura
                move "380" to 01H1-tipo-doc 01T1-tipo-doc
           when fattura-manuale
                move "384" to 01H1-tipo-doc 01T1-tipo-doc
                move 0 to tor-data-bolla tor-num-bolla
           end-evaluate.
           move epa-id-edi       to 01H2-id-edi
           move epa-q-id-edi     to 01H3-q-id-edi           
           move tor-data-fattura to 01H4-data-doc
           move tor-num-fattura  to 01H5-num-doc
           inspect 01H5-num-doc replacing leading x"30" by x"20"
           call "C$JUSTIFY" using 01H5-num-doc, "L"
           move ecd-id-edi   to 01H7-id-edi-cli
           move ecd-q-id-edi to 01H8-q-id-ed-cli

           inspect 01H1-tipo-doc 
                   replacing trailing spaces by low-value
           inspect 01H2-id-edi                            
                   replacing trailing spaces by low-value
           inspect 01H3-q-id-edi                          
                   replacing trailing spaces by low-value
           inspect 01H4-data-doc                          
                   replacing trailing spaces by low-value
           inspect 01H5-num-doc                           
                   replacing trailing spaces by low-value
           inspect 01H6-tipo-rec                          
                   replacing trailing spaces by low-value
           inspect 01H7-id-edi-cli                        
                   replacing trailing spaces by low-value
           inspect 01H8-q-id-ed-cli                       
                   replacing trailing spaces by low-value
           initialize line-riga
           string 01H1-tipo-doc     delimited low-value
                  separatore        delimited size
                  01H2-id-edi       delimited low-value
                  separatore        delimited size
                  01H3-q-id-edi     delimited low-value
                  separatore        delimited size
                  01H4-data-doc     delimited low-value
                  separatore        delimited size
                  01H5-num-doc      delimited low-value
                  separatore        delimited size
                  01H6-tipo-rec     delimited low-value
                  separatore        delimited size
                  01H7-id-edi-cli   delimited low-value
                  separatore        delimited size
                  01H8-q-id-ed-cli  delimited low-value
                  separatore        delimited size
                  separatore        delimited size
                  separatore        delimited size
                  separatore        delimited size
                  separatore        delimited size
                  separatore        delimited size
                  into line-riga
           end-string.
           write line-riga.
                       
           move epa-id-edi       to 01T2-id-edi
           move epa-q-id-edi     to 01T3-q-id-edi
           move tor-data-fattura to 01T4-data-doc
           move tor-num-fattura  to 01T5-num-doc  
           inspect 01T5-num-doc  replacing leading x"30" by x"20"
           call "C$JUSTIFY"      using 01T5-num-doc, "L" 
           move ecd-id-edi       to 01T7-id-edi-cli
           move ecd-q-id-edi     to 01T8-q-id-edi-cli
           move ecd-codforn      to 01T9-cod-forn
           move ecd-q-codforn    to 01T10-q-cod-forn
           move epa-codfis       to 01T11-codfis
           move epa-capsoc       to 01T12-capsoc
           move epa-rea-cciaa    to 01T13-rea-cciaa
           move epa-tribunale    to 01T14-tribunale
           move epa-piva         to 01T15-piva
           move epa-ragsoc       to 01T16-ragsoc
           move epa-indirizzo    to 01T17-indirizzo
           move epa-citta        to 01T18-citta
           move epa-prov         to 01T19-prov
           move epa-cap          to 01T20-cap
           
           move ecd-piva           to 01T23-piva-cli
           move ecd-ragsoc-c       to 01T24-ragsoc-cli
           move ecd-indirizzo-c    to 01T25-indirizzo-cli
           move ecd-citta-c        to 01T26-citta-cli
           move ecd-prov-c         to 01T27-prov-cli 
           move ecd-cap-c          to 01T28-cap-cli 
           move tblpa-descrizione1 to 01T30-pag-des.
           move ecd-cod-dest       to 01T34-cod-cli
           move ecd-q-cod-dest     to 01T35-q-cod-cli

           inspect 01T1-tipo-doc     
                   replacing trailing spaces by low-value
           inspect 01T2-id-edi       
                   replacing trailing spaces by low-value
           inspect 01T3-q-id-edi     
                   replacing trailing spaces by low-value
           inspect 01T4-data-doc     
                   replacing trailing spaces by low-value
           inspect 01T5-num-doc      
                   replacing trailing spaces by low-value
           inspect 01T6-tipo-rec     
                   replacing trailing spaces by low-value
           inspect 01T7-id-edi-cli   
                   replacing trailing spaces by low-value
           inspect 01T8-q-id-edi-cli 
                   replacing trailing spaces by low-value
           inspect 01T9-cod-forn     
                   replacing trailing spaces by low-value
           inspect 01T10-q-cod-forn   
                   replacing trailing spaces by low-value
           inspect 01T11-codfis         
                   replacing trailing spaces by low-value
           inspect 01T12-capsoc         
                   replacing trailing spaces by low-value
           inspect 01T13-rea-cciaa         
                   replacing trailing spaces by low-value
           inspect 01T14-tribunale
                   replacing trailing spaces by low-value
           inspect 01T15-piva         
                   replacing trailing spaces by low-value
           inspect 01T16-ragsoc       
                   replacing trailing spaces by low-value
           inspect 01T17-indirizzo    
                   replacing trailing spaces by low-value
           inspect 01T18-citta        
                   replacing trailing spaces by low-value
           inspect 01T19-prov         
                   replacing trailing spaces by low-value
           inspect 01T20-cap          
                   replacing trailing spaces by low-value
           inspect 01T23-piva-cli     
                   replacing trailing spaces by low-value
           inspect 01T24-ragsoc-cli   
                   replacing trailing spaces by low-value
           inspect 01T25-indirizzo-cli
                   replacing trailing spaces by low-value
           inspect 01T26-citta-cli    
                   replacing trailing spaces by low-value
           inspect 01T27-prov-cli     
                   replacing trailing spaces by low-value
           inspect 01T28-cap-cli      
                   replacing trailing spaces by low-value
           inspect 01T30-pag-des
                   replacing trailing spaces by low-value
           inspect 01T34-cod-cli                           
                   replacing trailing spaces by low-value
           inspect 01T35-q-cod-cli    
                   replacing trailing spaces by low-value

           initialize line-riga
           string 01T1-tipo-doc       delimited low-value              
                  separatore          delimited size
                  01T2-id-edi         delimited low-value              
                  separatore          delimited size
                  01T3-q-id-edi       delimited low-value              
                  separatore          delimited size
                  01T4-data-doc       delimited low-value              
                  separatore          delimited size
                  01T5-num-doc        delimited low-value              
                  separatore          delimited size
                  01T6-tipo-rec       delimited low-value              
                  separatore          delimited size
                  01T7-id-edi-cli     delimited low-value              
                  separatore          delimited size
                  01T8-q-id-edi-cli   delimited low-value              
                  separatore          delimited size
                  01T9-cod-forn       delimited low-value              
                  separatore          delimited size
                  01T10-q-cod-forn    delimited low-value              
                  separatore          delimited size
                  01T11-codfis        delimited low-value
                  separatore          delimited size
                  01T12-capsoc        delimited low-value
                  separatore          delimited size
                  01T13-rea-cciaa     delimited low-value
                  separatore          delimited size
                  01T14-tribunale     delimited low-value
                  separatore          delimited size
                  01T15-piva          delimited low-value              
                  separatore          delimited size
                  01T16-ragsoc        delimited low-value              
                  separatore          delimited size
                  01T17-indirizzo     delimited low-value              
                  separatore          delimited size
                  01T18-citta         delimited low-value              
                  separatore          delimited size
                  01T19-prov          delimited low-value              
                  separatore          delimited size
                  01T20-cap           delimited low-value              
                  separatore          delimited size                 
                  separatore          delimited size
                  separatore          delimited size
                  01T23-piva-cli      delimited low-value              
                  separatore          delimited size
                  01T24-ragsoc-cli    delimited low-value              
                  separatore          delimited size
                  01T25-indirizzo-cli delimited low-value              
                  separatore          delimited size
                  01T26-citta-cli     delimited low-value              
                  separatore          delimited size
                  01T27-prov-cli      delimited low-value              
                  separatore          delimited size
                  01T28-cap-cli       delimited low-value              
                  separatore          delimited size    
                  separatore          delimited size
                  01T30-pag-des       delimited low-value              
                  separatore          delimited size
                  separatore          delimited size
                  separatore          delimited size
                  separatore          delimited size
                  01T34-cod-cli       delimited low-value              
                  separatore          delimited size    
                  01T35-q-cod-cli     delimited low-value              
                  separatore          delimited size    
                  01T-middle1         delimited size
                  separatore          delimited low-value
                  separatore          delimited size     
                  separatore          delimited low-value
                  separatore          delimited size    
                  end-01T             delimited size
                  into line-riga
           end-string.
           write line-riga.
                                                      
           if tor-prg-destino = 0
              move ecd-ragsoc-c     to ecd-ragsoc-d
              move ecd-indirizzo-c  to ecd-indirizzo-d
              move ecd-citta-c      to ecd-citta-d
              move ecd-prov-c       to ecd-prov-d
              move ecd-cap-c        to ecd-cap-d
              move ecd-cod-dest     to ecd-cod-consegna
              move ecd-q-cod-dest   to ecd-q-cod-consegna
           else                                       
              move tor-prg-destino to ecd-prg-destino
              read edi-clides no lock 
                   invalid
                   move ecd-ragsoc-c     to ecd-ragsoc-d
                   move ecd-indirizzo-c  to ecd-indirizzo-d
                   move ecd-citta-c      to ecd-citta-d
                   move ecd-prov-c       to ecd-prov-d
                   move ecd-cap-c        to ecd-cap-d
                   move ecd-cod-dest     to ecd-cod-consegna
                   move ecd-q-cod-dest   to ecd-q-cod-consegna
              end-read
           end-if.
             
      ***--
       CLOSE-FILES.
           if edi-tipo-doc = 1 close tordini rordini
           else                close tnotacr rnotacr
           end-if.
           close tivaese tparamge clienti recapiti
                 articoli tcodpag tcontat
                 EDI-param EDI-tiva EDI-clides tcaumag timbalqta.

           if tutto-ok 
              close lineseq 
           end-if.

      ***---
       EXIT-PGM.
           if not RichiamoSchedulato and nargs not = 0
              move edi-work to edi-linkage
           end-if.
           goback.
                                         
      ***---
       TROVA-CONTATORE-ANNO-ESERCIZIO.
           set tutto-ok to true.
           open input tcontat.
           move esercizio to con-anno
           read tcontat no lock
                invalid
                if RichiamoSchedulato
                   initialize como-riga
                   string "IMPOSSIBILE PROCEDERE: "  delimited size
                          "Contatori per l'anno "    delimited size
                          "d'esericzio NON trovati!" delimited size
                     into como-riga
                   end-string
                   perform SCRIVI-RIGA-LOG
                else
                   display message "Contatori per l'anno "
                                   "d'esericzio NON trovati!"
                            x"0d0a""IMPOSSIBILE PROCEDERE!!"
                             title tit-err
                              icon 3
                end-if
                set errori to true
           end-read.
           close tcontat.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
