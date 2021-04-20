       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      edi-bolle-p.
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
           copy "tcontat.sl".
           copy "articoli.sl".
           copy "lineseq.sl". 
           copy "EDI-clides.sl".
           copy "EDI-param.sl".
           copy "tcaumag.sl". 
           copy "clienti.sl".
           copy "tsetinvio.sl".
           copy "progmag.sl".

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.          

       FD  logfile.
       01 log-riga        PIC  x(900). 

           copy "tparamge.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tcontat.fd".
           copy "articoli.fd".
           copy "lineseq.fd".  
           copy "EDI-clides.fd".
           copy "EDI-param.fd". 
           copy "tcaumag.fd". 
           copy "clienti.fd".
           copy "tsetinvio.fd".
           copy "progmag.fd".   

       WORKING-STORAGE SECTION.                                     
      *    COPY
           copy "edi-bolle.def".

      *    COSTANTI
       78  titolo                value "Generazione bolle invio EDI".
                                          
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

      *    FILE-STATUS               
       77  status-lineseq1       pic xx.
       77  status-tparamge       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tcontat        pic xx.
       77  status-tivaese        pic xx.
       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.
       77  status-EDI-clides     pic xx.
       77  status-EDI-param      pic xx.
       77  status-tcaumag        pic xx.
       77  status-timbalqta      pic xx.
       77  status-tcodpag        pic xx.
       77  status-clienti        pic xx.
       77  status-tsetinvio      pic xx.
       77  status-progmag        pic xx.
       77  status-logfile        pic xx.
       77  wstampa               pic x(256).

       77  tot-doc               pic 9(8) value 0.
       77  bolle-tot             pic z(8).
       77  bolla-da              pic z(8).        
       77  bolla-a               pic z(8).
       
       77  path-logfile          pic x(256).  
       77  como-riga             pic x(200).    
       
       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 9(5).
       77  ss                    pic 99.  

       77  CallingPgm            pic x(20).
       77  path-log              pic x(256).
       77  como-numero           pic z(12)vz(5).
       01  como-numero-x.
           05 como-int           pic x(12).
           05 como-dec           pic x(5).
       77  NumericEDI            pic x(20).
       77  SerieBolle            pic 9.

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       01  r-inizio.
         05 filler              pic x(2)  value " [".
         05 r-data-i.
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
         05 filler           pic x(2)     value "] ".

       77  copy-status           signed-short.
       77  path-backup           pic x(256).       
       77  nargs                 pic 99  comp-1 value 0.

       01  filler                pic xx.     
           88 errori             value "ER".
           88 tutto-ok           value "OK".
                                                  
       01  filler                pic 9.
           88 prima-volta        value 1, false 0.
       01  filler                pic 9.
           88 FileCreato         value 1, false 0. 
       77  filler                pic 9.
           88 trovato            value 1, false 0.  
       77  filler                pic 9.
           88 record-ok          value 1, false 0.   
       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.


      ******************************************************************
       LINKAGE SECTION.   
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.
      ***---
       TCONTAT-ERR SECTION.
           use after error procedure on tcontat.
           set tutto-ok  to true.
           evaluate status-tcontat
           when "35"
      *****          display message "File [TCONTAT] not found!"
      *****                     title titolo
      *****                      icon 3
                set errori to true
           when "39"
      *****          display message "File [TCONTAT] Mismatch size!"
      *****                     title titolo
      *****                      icon 3
                set errori to true
           when "98"
      *****          display message "[TCONTAT] Indexed file corrupt!"
      *****                     title titolo
      *****                      icon 3
                set errori to true
           when "93"
      *****          initialize geslock-messaggio
      *****          string   "File già in uso!"
      *****            x"0d0a""Impossibile procedere!" delimited size
      *****                into geslock-messaggio
      *****          end-string
      *****          move 1 to geslock-v-riprova
      *****          move 0 to geslock-v-ignora
      *****          move 1 to geslock-v-termina
      *****          move   "tcontat"    to geslock-nome-file
      *****          call   "geslock" using geslock-linkage
      *****          cancel "geslock"
      *****          evaluate true
      *****          when riprova
                     perform OPEN-IO-TCONTAT
      *****          when termina
      *****               set errori to true
      *****               display message "Operazione interrotta!"
      *****                         title titolo
      *****                          icon 2
      *****          end-evaluate
           when "99"
      *****          initialize geslock-messaggio
      *****          string   "Il record per l'anno: " con-anno
      *****     x"0d0a""risulta in uso su altro terminale."    delimited size
      *****     x"0d0a""Questo comporta l'impossibiltà ad"     delimited size
      *****     x"0d0a""aggiornare la tabella dei contatori."  delimited size
      *****                into geslock-messaggio
      *****          end-string
      *****          move 1 to geslock-v-riprova
      *****          move 0 to geslock-v-ignora
      *****          move 1 to geslock-v-termina
      *****          move   "tcontat"    to geslock-nome-file
      *****          call   "geslock" using geslock-linkage
      *****          cancel "geslock"
      *****          evaluate true
      *****          when riprova
                     perform READ-TCONTAT-LOCK
      *****          when termina
      *****               set errori to true
      *****               display message "Operazione interrotta!"
      *****                         title titolo
      *****                          icon 2
      *****          end-evaluate
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "93"
      *****          initialize geslock-messaggio
      *****          string   "File già in uso!"
      *****            x"0d0a""Impossibile procedere!" delimited size
      *****                into geslock-messaggio
      *****          end-string
      *****          move 1 to geslock-v-riprova
      *****          move 0 to geslock-v-ignora
      *****          move 1 to geslock-v-termina
      *****          move   "File TXT"   to geslock-nome-file
      *****          call   "geslock" using geslock-linkage
      *****          cancel "geslock"
      *****          evaluate true
      *****          when riprova
                     open output lineseq
      *****          when termina
      *****               set errori to true
      *****               display message "Operazione interrotta!"
      *****                         title titolo
      *****                          icon 2
      *****          end-evaluate
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok                                           
              move spaces to epa-codice
              read edi-param no lock 
                   invalid
                   set errori to true
                   move "Impostare parametri di importazione" 
                     to como-riga
                   perform SCRIVI-RIGA-LOG
              end-read
              move tge-causale-omag to tca-codice
              read tcaumag no lock
           end-if.
           if tutto-ok     
              if RichiamoSchedulato
                 move 0 to batch-status
              end-if
              call "C$CALLEDBY" using CallingPgm
              if CallingPgm = "edi-stdoc"
                 move batch-log      to tor-bolla
                 read tordini key k-bolla
                      invalid continue
                  not invalid perform EXPORT-BOLLA
                 end-read
              else
                 move 1 to SerieBolle
                 perform ELABORAZIONE-BOLLE
                 move 2 to SerieBolle
                 perform ELABORAZIONE-BOLLE
                 perform CLOSE-FILES 
              end-if
           else
              if RichiamoSchedulato
                 move -1 to batch-status
              end-if
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.                                  
           CALL "C$NARG" USING NARGS.                                      
           if nargs not = 0
              set RichiamoSchedulato to true
              move -1 to batch-status  
              accept path-log from environment "SCHEDULER_PATH_LOG"
           else                             
              set RichiamoSchedulato to false
              accept path-log from environment "PATH_ST"
           end-if.
           inspect path-log replacing trailing spaces by low-value.

           move 0 to counter counter2.
           set tutto-ok     to true.
           set prima-volta  to true.
           set FileCreato   to false.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa.
           accept  wstampa from environment "EDI_BOLLE_PATH".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa delimited low-value
                   "LUBEX_bolle_EDI_" delimited size
                   como-data          delimited size
                   "_"                delimited size
                   como-ora           delimited size
                   ".txt"             delimited size
                   into wstampa
           end-string.           

      ***---
       OPEN-FILES.
           initialize path-logfile.
           string path-log          delimited low-value
                  "LOG_EDI_BOLLE_"  delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".log"            delimited size
                  into path-logfile
           end-string.
           open output logfile.
                                               
           accept como-ora  from time.
           move   como-ora(1:2) to hh.
           move   como-ora(3:2) to mm.
           move   como-ora(5:2) to ss.

           compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.

           move "INIZIO ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.

           perform OPEN-IO-TCONTAT.
           if tutto-ok
              open input tparamge
              move spaces to tge-codice
              read tparamge no lock 
                   invalid continue
              end-read
              perform READ-TCONTAT-LOCK
              if tutto-ok
                 open input tordini rordini articoli tsetinvio
                            EDI-clides tcaumag clienti EDI-param progmag
                 if errori
                    close tcontat tparamge
                 end-if
              else
                 close tcontat tparamge
              end-if
           end-if.

      ***---
       OPEN-IO-TCONTAT.
           open i-o tcontat.

      ***---
       READ-TCONTAT-LOCK.
           move tge-anno  to con-anno.
           read tcontat lock invalid set errori to true end-read.

      ***---
       ELABORAZIONE-BOLLE.
           set tutto-ok  to true.
           move tge-anno to tor-anno-bolla
           if SerieBolle = 1
              add 1 to con-ult-num-bolle-edi-1 giving tor-num-bolla
              if tor-num-bolla = 0
                 move 99999999 to tor-num-bolla
              end-if
            else
              add 1 to con-ult-num-bolle-edi-2 giving tor-num-bolla
              if tor-num-bolla = 0
                 move 99999999 to tor-num-bolla
              end-if
              if tor-num-bolla < 900000
                 move 900000 to tor-num-bolla
              end-if
           end-if.
           start tordini key  is >= k-bolla
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tordini next at end exit perform end-read
                 if tor-anno-bolla not = con-anno
                    exit perform
                 end-if   
                 if SerieBOlle = 1
                    if tor-num-bolla >= 500000
                       exit perform
                    end-if
                 end-if            
                 perform CONTATORE-VIDEO
                 perform EXPORT-BOLLA
                 if errori exit perform end-if
              end-perform
           end-if.

           move tot-doc to bolle-tot.
           if tutto-ok
              if tot-doc > 0
                 rewrite con-rec invalid continue end-rewrite
              else
                 if SerieBolle = 1
                    move "Nessuna bolla serie 1 da inviare!"to como-riga
                 else                                                   
                    move "Nessuna bolla serie 2 da inviare!"to como-riga
                 end-if
                 perform SCRIVI-RIGA-LOG
              end-if
           end-if.

      ***---
       EXPORT-BOLLA.
           set record-ok to false.
               
      *****     if tor-invio-edi  |FLAG PER LE FATTURE
              set cli-tipo-C to true
              move tor-cod-cli to cli-codice
              read clienti no lock 
                   invalid continue
               not invalid
                   if cli-invio-bolle-EDI-si
                      move tor-causale to tca-codice
                      read tcaumag no lock
                           invalid continue
                       not invalid
                           if tca-causale-EDI not = spaces
                              set record-ok to true
                           end-if
                      end-read
                   end-if
              end-read
      *****     end-if

           if record-ok                       
              initialize como-riga
              string "ELABORAZIONE BOLLA N. " delimited size
                     tor-num-bolla            delimited size
                 into como-riga
              end-string
              perform SCRIVI-RIGA-LOG
              perform VALORIZZA-EDI-01T
              perform LOOP-RIGHE-RORDINI
              |SONO RIUSCITO AD APRIRE IL FILE DI TESTO
              if tutto-ok 
                 perform VALORIZZA-EDI-PIEDE 
              
                 if tot-doc = 0
                    move tor-num-bolla to bolla-da
                 end-if
                 move tor-num-bolla to bolla-a
                 add 1 to tot-doc
                 if SerieBolle = 1
                    move tor-num-bolla to con-ult-num-bolle-edi-1
                 else                                            
                    move tor-num-bolla to con-ult-num-bolle-edi-2
                 end-if
              end-if
           end-if.

      ***---
       CONTATORE-VIDEO.
           if not RichiamoSchedulato exit paragraph end-if.
           add 1 to counter counter2.

           if counter2 = 50 and batch-win-handle not = 0
              move counter to counter-edit
              display counter-edit
                      upon batch-win-handle
                      line 25,00
                    column 38,00
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
                 perform VALORIZZA-EDI-02D
              end-perform
           end-if.                              

      ***---
       VALORIZZA-EDI-02D.
           move 01T1-BGM-TIPODOC      to 02D1-BGM-TIPODOC.
           move 01T2-BGM-ID-EDI-MIT1  to 02D2-BGM-ID-EDI-MIT1.
           move 01T3-BGM-ID-EDI-MIT2  to 02D3-BGM-ID-EDI-MIT2.
           move 01T4-BGM-DATADOC      to 02D4-BGM-DATADOC.
           move 01T5-BGM-NUMDOC       to 02D5-BGM-NUMDOC.
           if ror-progr-master not = 0
              move ror-progr-master to 02D7-LIN-NUMRIGA
           else
              move ror-num-riga     to 02D7-LIN-NUMRIGA
           end-if.
           move ror-cod-articolo      to art-codice
           read articoli no lock invalid continue end-read
           move spaces to 02D14-LIN-CODEANCU.
           if art-codice-ean-1 not = spaces
              move art-codice-ean-1 to 02D14-LIN-CODEANCU
           else
              if art-codice-ean-2 not = spaces      
                 move art-codice-ean-2 to 02D14-LIN-CODEANCU
              else
                 if art-codice-ean-3 not = spaces
                    move art-codice-ean-3 to 02D14-LIN-CODEANCU
                 else
                    if art-codice-ean-4 not = spaces
                       move art-codice-ean-4 to 02D14-LIN-CODEANCU
                    else
                       if art-codice-ean-5 not = spaces
                          move art-codice-ean-4 to 02D14-LIN-CODEANCU
                       end-if
                    end-if
                 end-if
              end-if
           end-if.
           move ror-cod-articolo   to 02D12-LIN-CODFORTU.
           move 02D14-LIN-CODEANCU to 02D16-LIN-CODEANTU.
           if 02D14-LIN-CODEANCU not = spaces
              move "EN" to 02D15-LIN-TIPCODCU 
           end-if.
           move art-descrizione    to 02D19-LIN-DESCR.
           move ror-qta     to como-numero.
           perform TRATTA-NUMERICO.
           move NumericEDI  to 02D22-LIN-QTAORD.

           move ror-prg-chiave to prg-chiave.
           read progmag no lock 
                invalid move 0 to prg-peso-utf prg-peso-non-utf
           end-read.
           compute como-numero = 
                 ( prg-peso-utf + prg-peso-non-utf ) * ror-qta.
           perform TRATTA-NUMERICO.
           move NumericEDI to 02D43-MEA-VALORE. 

           inspect 02D1-BGM-TIPODOC                          
                   replacing trailing spaces by low-value
           inspect 02D2-BGM-ID-EDI-MIT1                            
                   replacing trailing spaces by low-value
           inspect 02D3-BGM-ID-EDI-MIT2                          
                   replacing trailing spaces by low-value
           inspect 02D4-BGM-DATADOC                          
                   replacing trailing spaces by low-value
           inspect 02D5-BGM-NUMDOC                           
                   replacing trailing spaces by low-value
           inspect 02D6-TIPOREC                          
                   replacing trailing spaces by low-value
           inspect 02D7-LIN-NUMRIGA                          
                   replacing trailing spaces by low-value
           inspect 02D12-LIN-CODFORTU
                   replacing trailing spaces by low-value
           inspect 02D14-LIN-CODEANCU                       
                   replacing trailing spaces by low-value
           inspect 02D15-LIN-TIPCODCU                    
                   replacing trailing spaces by low-value
           inspect 02D16-LIN-CODEANTU                        
                   replacing trailing spaces by low-value
           inspect 02D19-LIN-DESCR                          
                   replacing trailing spaces by low-value
           inspect 02D21-LIN-QTAORD-S                         
                   replacing trailing spaces by low-value
           inspect 02D22-LIN-QTAORD                        
                   replacing trailing spaces by low-value 
           inspect 02D27-LIN-UDMQORD                         
                   replacing trailing spaces by low-value
           inspect 02D43-MEA-VALORE
                   replacing trailing spaces by low-value
           initialize line-riga of lineseq.
           string 02D1-BGM-TIPODOC      delimited low-value
                  separatore            delimited size
                  02D2-BGM-ID-EDI-MIT1  delimited low-value
                  separatore            delimited size
                  02D3-BGM-ID-EDI-MIT2  delimited low-value
                  separatore            delimited size
                  02D4-BGM-DATADOC      delimited low-value
                  separatore            delimited size
                  02D5-BGM-NUMDOC       delimited low-value
                  separatore            delimited size
                  02D6-TIPOREC          delimited low-value
                  separatore            delimited size
                  02D7-LIN-NUMRIGA      delimited low-value
                  separatore            delimited size     
                  02D-filler1           delimited size
                  02D12-LIN-CODFORTU    delimited low-value
                  separatore            delimited size
                  02D13-LIN-CODFORTU-Q  delimited size
                  separatore            delimited size     
                  02D14-LIN-CODEANCU    delimited low-value
                  separatore            delimited size
                  02D15-LIN-TIPCODCU    delimited low-value
                  separatore            delimited size
                  02D16-LIN-CODEANTU    delimited low-value
                  separatore            delimited size
                  02D-filler3           delimited size 
                  02D19-LIN-DESCR       delimited low-value
                  separatore            delimited size
                  separatore            delimited size
                  02D21-LIN-QTAORD-S    delimited low-value
                  separatore            delimited size
                  02D22-LIN-QTAORD      delimited low-value
                  separatore            delimited size
                  02D-filler4           delimited size     
                  02D27-LIN-UDMQORD     delimited low-value
                  separatore            delimited size
                  02D-filler5           delimited size
                  02D41-MEA-IDMISURA    delimited low-value
                  separatore            delimited size
                  02D-filler6           delimited size
                  02D43-MEA-VALORE      delimited low-value
                  separatore            delimited size
                  02D-filler7           delimited size
                  02D46-MEA-UNIMIS      delimited low-value
                  separatore            delimited size
                  end-02D               delimited size
                  into line-riga of lineseq
           end-string.
           write line-riga of lineseq.

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
       VALORIZZA-EDI-01T.
           if prima-volta
              set  prima-volta to false
              open output lineseq
              if errori exit paragraph end-if
              set FileCreato to true
           end-if.

           move tor-cod-cli     to ecd-cli-codice.
           move 0               to ecd-prg-destino.
           read edi-clides no lock 
                invalid initialize ecd-rec
           end-read.          
                       
           move epa-id-edi       to 01T2-BGM-ID-EDI-MIT1.
           move epa-q-id-edi     to 01T3-BGM-ID-EDI-MIT2.
           move tor-data-bolla   to 01T4-BGM-DATADOC.
           move tor-num-bolla    to 01T5-BGM-NUMDOC.
           inspect 01T5-BGM-NUMDOC  replacing leading x"30" by x"20".
           call "C$JUSTIFY"      using 01T5-BGM-NUMDOC, "L".
           move ecd-id-edi       to 01T7-BGM-ID-EDI-DES1.
           move ecd-q-id-edi     to 01T8-BGM-ID-EDI-DES2.
           move ecd-codforn      to 01T9-NAD-CODNAD-SU.
           move ecd-q-codforn    to 01T10-NAD-QCODNAD-SU.
           move ecd-cod-dest     to 01T19-NAD-CODNAD-BY.
           move ecd-q-cod-dest   to 01T20-NAD-QCODNAD-BY.
                                                      
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
           move ecd-cod-consegna   to 01T29-NAD-CODNAD-DP.
           move ecd-q-cod-consegna to 01T30-NAD-QCODNAD-DP.
           move tor-num-ord-cli    to 01T78-RFF-NUMRIF.
           move tor-data-bolla     to 01T100-RFF-AAS.
           move tor-num-bolla      to 01T101-RFF-AAS.

           inspect 01T1-BGM-TIPODOC     
                   replacing trailing spaces by low-value
           inspect 01T2-BGM-ID-EDI-MIT1       
                   replacing trailing spaces by low-value
           inspect 01T3-BGM-ID-EDI-MIT2     
                   replacing trailing spaces by low-value
           inspect 01T4-BGM-DATADOC     
                   replacing trailing spaces by low-value
           inspect 01T5-BGM-NUMDOC      
                   replacing trailing spaces by low-value
           inspect 01T6-TIPOREC     
                   replacing trailing spaces by low-value
           inspect 01T7-BGM-ID-EDI-DES1   
                   replacing trailing spaces by low-value
           inspect 01T8-BGM-ID-EDI-DES2 
                   replacing trailing spaces by low-value
           inspect 01T9-NAD-CODNAD-SU     
                   replacing trailing spaces by low-value
           inspect 01T10-NAD-QCODNAD-SU   
                   replacing trailing spaces by low-value
           inspect 01T19-NAD-CODNAD-BY         
                   replacing trailing spaces by low-value
           inspect 01T20-NAD-QCODNAD-BY         
                   replacing trailing spaces by low-value
           inspect 01T29-NAD-CODNAD-DP         
                   replacing trailing spaces by low-value
           inspect 01T30-NAD-QCODNAD-DP
                   replacing trailing spaces by low-value.
           inspect 01T78-RFF-NUMRIF
                   replacing trailing spaces by low-value.
           inspect 01T101-RFF-AAS
                   replacing trailing spaces by low-value.

           initialize line-riga of lineseq
           string 01T1-BGM-TIPODOC      delimited low-value              
                  separatore            delimited size
                  01T2-BGM-ID-EDI-MIT1  delimited low-value              
                  separatore            delimited size
                  01T3-BGM-ID-EDI-MIT2  delimited low-value              
                  separatore            delimited size
                  01T4-BGM-DATADOC      delimited low-value              
                  separatore            delimited size
                  01T5-BGM-NUMDOC       delimited low-value              
                  separatore            delimited size
                  01T6-TIPOREC          delimited low-value              
                  separatore            delimited size
                  01T7-BGM-ID-EDI-DES1  delimited low-value              
                  separatore            delimited size
                  01T8-BGM-ID-EDI-DES2  delimited low-value              
                  separatore            delimited size
                  01T9-NAD-CODNAD-SU    delimited low-value              
                  separatore            delimited size
                  01T10-NAD-QCODNAD-SU  delimited low-value              
                  separatore            delimited size
                  01Tfiller1            delimited size
                  01T19-NAD-CODNAD-BY   delimited low-value
                  separatore            delimited size
                  01T20-NAD-QCODNAD-BY  delimited low-value
                  separatore            delimited size
                  01Tfiller2            delimited size 
                  01T29-NAD-CODNAD-DP   delimited low-value
                  separatore            delimited size
                  01T30-NAD-QCODNAD-DP  delimited low-value              
                  separatore            delimited size
                  01T-filler3           delimited size
                  01T78-RFF-NUMRIF      delimited low-value
                  separatore            delimited size
                  01T-filler4           delimited size
                  01T100-RFF-AAS        delimited size
                  separatore            delimited size
                  01T101-RFF-AAS        delimited low-value
                  separatore            delimited size
                  end-01T               delimited size              
                  into line-riga of lineseq
           end-string.
           write line-riga of lineseq. 
                                                   
      ***---
       VALORIZZA-EDI-PIEDE.                      
           move epa-id-edi       to 03P2-BGM-ID-EDI-MIT1.
           move epa-q-id-edi     to 03P3-BGM-ID-EDI-MIT2.
           move tor-data-bolla   to 03P4-BGM-DATADOC.
           move tor-num-bolla    to 03P5-BGM-NUMDOC.
           inspect 03P5-BGM-NUMDOC  replacing leading x"30" by x"20".
           call "C$JUSTIFY"      using 03P5-BGM-NUMDOC, "L".

           inspect 03P2-BGM-ID-EDI-MIT1 
                   replacing trailing spaces by low-value.
           inspect 03P3-BGM-ID-EDI-MIT2 
                   replacing trailing spaces by low-value.
           inspect 03P4-BGM-DATADOC 
                   replacing trailing spaces by low-value.
           inspect 03P5-BGM-NUMDOC 
                   replacing trailing spaces by low-value.

           initialize line-riga of lineseq.
           string 03P1-BGM-TIPODOC     delimited low-value
                  separatore           delimited size
                  03P2-BGM-ID-EDI-MIT1 delimited low-value
                  separatore           delimited size
                  03P3-BGM-ID-EDI-MIT2 delimited low-value
                  separatore           delimited size
                  03P4-BGM-DATADOC     delimited low-value
                  separatore           delimited size
                  03P5-BGM-NUMDOC      delimited low-value
                  separatore           delimited size
                  03P6-TIPOREC         delimited low-value
                  separatore           delimited size
                  end-03P              delimited size
             into line-riga of lineseq
           end-string.
           write line-riga of lineseq.
             
      ***--
       CLOSE-FILES.
           close tordini rordini tparamge clienti articoli tsetinvio
                 EDI-param EDI-clides tcaumag progmag.

           if tutto-ok close lineseq end-if.

      ***---
       EXIT-PGM.
           if tot-doc > 0                 
              if FileCreato
                 initialize como-riga
                 string "ELABORATE " delimited size
                        bolle-tot    delimited size
                        " BOLLE."    delimited size
                        " DAL: "     delimited size
                        bolla-da     delimited size
                        " - AL: "    delimited size
                        bolla-a      delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG

                 initialize path-backup
                 accept  path-backup 
                         from environment "EDI_BOLLE_PATH_BACKUP"
                 inspect path-backup 
                         replacing trailing spaces by low-value
                 string  path-backup delimited low-value
                         "LUBEX_bolle_EDI_" delimited size
                         como-data          delimited size
                         "_"                delimited size
                         como-ora           delimited size
                         ".txt"             delimited size
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
                     if RichiamoSchedulato
                        move 1 to batch-status
                     end-if
                  end-if

              else
                 
                 initialize como-riga
                 string "ELABORATE " delimited size
                        bolle-tot    delimited size
                        " BOLLE."    delimited size
                        " DAL: "     delimited size
                        bolla-da     delimited size
                        " - AL: "    delimited size
                        bolla-a      delimited size
                   into como-riga
                 end-string
                 perform SCRIVI-RIGA-LOG
                 move "ATTENZIONE! File non generato. " to como-riga
                 perform SCRIVI-RIGA-LOG
              end-if
           end-if.

           move 0 to tot-secondi.
           accept como-ora from time.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
           compute tot-secondi = end-secondi - start-secondi.

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
           end-if.

           close logfile.

           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
