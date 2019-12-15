       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mail-fido.
       AUTHOR.                          Luciano.
       REMARKS. programma di invio mail dei fidi. Batch 
                schedulato sul server. Il programma scorre tutti i 
                clienti. Nel log di SYSERR metto il log generale. Per 
                ogni cliente con il flag cli-gestione-fido-si scrivo le
                relative informazioni in un PDF che a fine elaborazione
                invio via mail.
                Nel log generale viene riportato ora di inzio di ogni 
                cliente.
      ******************************************************************
                                                                              
       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.         
       SELECT seqlog
           ASSIGN       TO path-seqlog
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-seqlog.

           copy "tsetinvio.sl".
           copy "lineseq.sl".
           copy "clienti.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "sitfin.sl".
           copy "tparamge.sl".
           copy "tgrupgdo.sl".
           copy "tmp-sitfin.sl".
           copy "ttipocli.sl".


       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                      
       FD  seqlog.
       01 riga-log        PIC  x(10000).

           copy "tsetinvio.fd".
      *     copy "lineseq.fd".
       FD  lineseq.
       01 line-riga        PIC  x(10000).
           copy "clienti.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "sitfin.fd".
           copy "tparamge.fd".
           copy "tgrupgdo.fd".
           copy "tmp-sitfin.fd".
           copy "ttipocli.fd".

       FD  lineseq1.
       01 line-riga        PIC  x(900).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
           copy "mail.def".
           copy "link-sitfin-p.def".
           copy "link-calfido.def".
           copy "wait-3-secs.def".

           copy "fonts.def".
           copy "spooler.def".
           copy "selprint.lks".

           copy "link-settaPDF.def".
           copy "link-readutente.def".
                             

       78  titolo value  "Situazione Finanziaria Clienti".
       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(2)  value "] ".

       77  status-clienti    pic xx.
       77  status-tsetinvio  pic xx.
       77  status-lineseq    pic xx.
       77  status-seqlog     pic xx.
       77  status-fileseq    pic xx.
       77  status-tordini    pic xx.
       77  status-rordini    pic xx.
       77  status-tmp-sitfin pic xx. 
       77  status-sitfin     pic xx. 
       77  status-tgrupgdo   pic xx.
       77  status-tparamge   pic xx.
       77  status-ttipocli   pic xx.

       77  wstampa           pic x(256).
       77  path-tmp-Sitfin   pic x(256).
       77  path-pdf          pic x(256).

       77  totale            pic s9(12)v99.
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-riga             pic x(400).
       77  riga-stampa           pic x(400).
       77  tentativi             pic 99.    
       77  segn                  pic x.

       77  counter               pic 9(9).
       77  counter2              pic 9(9).
       77  counter-edit          pic zzz.zzz.zz9.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.

       77  DestFile               pic x(256).
       77  NomeFile               pic x(256).
       77  PathCreaPDF            pic x(256).
       77  StatusCreaPDF          pic s9.
       77  num-edit               pic x(8).
       77  comando                pic x(200).
       77  parametri              pic x(200).
       77  nargs                  pic 99  comp-1 value 0.

       01  tipo-riga.
           05 tr-num1 pic x(14).
           05 tr-num2 pic x(14).

       77  ed-num      pic ---.---.--9,99.
       77  ed-num-gg   pic zz9.

       77  Arial14BI     handle of font.
       77  Arial8        handle of font.
       77  Arial6        handle of font.
       77  Arial20BI     handle of font.

       77  messaggio             pic x(150)  value spaces.
       77  wfont-status          pic s9(5)   value zero.
       77  font-size-dply        pic z(5)    value zero.
       77  num-pagina            pic 9(3)    value zero.
       77  num-pag-ed            pic z(3)    value zero.

       77  num-giorni  pic 9(3).
       78  max-giorni  value 28.

       77  stato-ok PIC  S9(9) USAGE IS COMP-4 VALUE IS 0.
       77  stato-ko PIC  S9(9) USAGE IS COMP-4 VALUE IS 0.

       77  save-spl-riga                   PIC 9(7)V99.

       77  calling-program          pic x(20).

       01                    pic 9.
           88 on-line  value 1 false zero.

       77  cont     pic 9(5).
       77  cont-ed  pic z(5).


       01  FILE-INFO.
           02  FILE-SIZE    PIC X(8) COMP-X.
           02  FILE-DATE    PIC 9(8) COMP-X.
           02  FILE-TIME    PIC 9(8) COMP-X.

       77  old-SIZE         PIC X(8) COMP-X.


       77  status-code       pic 9.

       01                    pic 9.
           88 TIME-OUT-EXIT  value 1 false zero.

       77  MINUTI-PARTENZA   pic 99.
       77  MINUTI-arrivo     pic 99.

       77  como-nome-file         pic x(256).

       77  filler                 pic 9.
           88  trovato            value 1, false 0.

       77  como-verifica-1   pic x(10).
       77  como-verifica-2   pic x(10).
      ***** 77  como-verifica-3   pic x(10).

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato          value 1, false 0.   

       77  filler                pic 9 value 0.
           88  nessun-errore     value 1, false 0.
      

       LINKAGE SECTION.   
       copy "link-batch.def".                       

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                move "File [CLIENTI] inesistente" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG
                   set nessun-errore to false
                end-if
                set errori to true
           when "39"
                move "File [CLIENTI] mismatch size!" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG      
                   set nessun-errore to false
                end-if
                set errori to true
           when "98"
                move "[CLIENTI] Indexed file corrupt!" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG  
                   set nessun-errore to false
                end-if
                set errori to true
           when "93"
           when "99" 
                set RecLocked to true
           end-evaluate.

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                move "File [TORDINI] inesistente" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG  
                   set nessun-errore to false
                end-if
                set errori to true
           when "39"
                move "File [TORDINI] mismatch size!" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG  
                   set nessun-errore to false
                end-if
                set errori to true
           when "98"
                move "[TORDINI] Indexed file corrupt!" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG  
                   set nessun-errore to false
                end-if
                set errori to true
           when "93"
           when "99" 
                set RecLocked to true
           end-evaluate.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                move "File [RORDINI] inesistente" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG  
                   set nessun-errore to false
                end-if
                set errori to true
           when "39"
                move "File [RORDINI] mismatch size!" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG   
                   set nessun-errore to false
                end-if
                set errori to true
           when "98"
                move "[RORDINI] Indexed file corrupt!" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG   
                   set nessun-errore to false
                end-if
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                move "File [TTIPOCLI] inesistente" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG  
                   set nessun-errore to false
                end-if
                set errori to true
           when "39"
                move "File [TTIPOCLI] mismatch size!" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG  
                   set nessun-errore to false
                end-if
                set errori to true
           when "98"
                move "[TTIPOCLI] Indexed file corrupt!" to como-riga
                if on-line
                   inspect como-riga 
                                replacing trailing space by low-value
                   display message box como-riga
                          title titolo
                else
                   perform SETTA-RIGA-LOG 
                   set nessun-errore to false
                end-if
                set errori to true
           when "93"
           when "99" 
                set RecLocked to true
           end-evaluate.


       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set nessun-errore to true.
           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.                

           initialize path-seqlog
           if RichiamoSchedulato                                       
              accept  path-seqlog from environment "SCHEDULER_PATH_LOG"
           else                                                        
              accept  path-seqlog from environment "PATH_ST"
           end-if.
                                  
           accept como-data from century-date
           accept como-ora  from time                               
           inspect path-seqlog replacing trailing spaces by low-value
           string  path-seqlog  delimited low-value
                   "FIDO_"      delimited size
                   como-data    delimited size
                   "_"          delimited size
                   como-ora     delimited size
                   ".log"       delimited size
                   into path-seqlog
           end-string.

           if RichiamoSchedulato
              move path-seqlog to batch-log
           end-if.

           open output seqlog.

           move "BOSS"            to ru-user.
           call   "readutente" using ru-linkage.
           cancel "readutente".
           call "C$CALLEDBY"  using calling-program.
           if calling-program = "mail-fido-m"
              set on-line to true
           else
              set on-line to false
           end-if.

           move zero   to cont

           move "INIZIO PROGRAMMA" to como-riga
           perform SETTA-RIGA-LOG

           set tutto-ok      to true.
           set prima-volta   to true.

           accept como-data from century-date
           accept como-ora  from century-date
           accept  path-tmp-sitfin from environment "PATH_ST".
           inspect path-tmp-sitfin replacing trailing space by low-value
           string  path-tmp-sitfin delimited by low-value
                   "tmp_sitfin"    delimited size
                   "_"             delimited size
                   como-data       delimited size
                   "_"             delimited size
                   como-ora        delimited size
                   into path-tmp-sitfin.
           inspect path-tmp-sitfin 
                                replacing trailing low-value by space.

           COPY RESOURCE "stato-ok.bmp".
           CALL "w$bitmap" USING WBITMAP-LOAD "stato-ok.bmp", 
                   GIVING stato-ok.

           COPY RESOURCE "stato-ko.bmp".
           CALL "w$bitmap" USING WBITMAP-LOAD "stato-ko.bmp", 
                   GIVING stato-ko.

      ***---
       OPEN-FILES.
           move "APERTURA FILES" to como-riga.
           perform SETTA-RIGA-LOG.

      *****     open output sitfin.
      *****     close       sitfin.
           open i-o    sitfin.

           open input tparamge tgrupgdo ttipocli tordini rordini.

           |Lanciando di notte non devo fare
           |particolari controlli sul lock
           if tutto-ok
              open i-o clienti 
           end-if.

           if tutto-ok
              open output tmp-sitfin
              close       tmp-sitfin
              open i-o    tmp-sitfin
           end-if.

           if errori
              move "APERTURA FILES NON RIUSCITA" to como-riga
              if on-line
                 inspect como-riga replacing trailing space by low-value
                 display message como-riga
                           title titolo
              else
                 perform SETTA-RIGA-LOG
              end-if
           else
              move "APERTURA FILES RIUSCITA" to como-riga
              perform SETTA-RIGA-LOG
           end-if.

      ***---
       ELABORAZIONE.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.

           move "AZZERAMENTO SITUAZIONE FINANZIARIA"
             to como-riga.
           perform SETTA-RIGA-LOG.           

           set cli-tipo-C to true.
           move low-value to cli-codice.
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti next at end exit perform end-read
                    if cli-tipo-F exit perform end-if
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if

                    move cli-piva to sf-piva
                    delete sitfin record invalid continue end-delete

                 end-perform
           end-start.                        

           move "SCANSIONE DEI CLIENTI CON GESTIONE FIDO ABILITATA"
             to como-riga.
           perform SETTA-RIGA-LOG.           

           set cli-tipo-C to true.
           move low-value to cli-codice.
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti next at end exit perform end-read
                    if cli-tipo-F exit perform end-if
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if

      *****              move cli-piva to sf-piva
      *****              delete sitfin record invalid continue end-delete

                    if cli-gestione-fido-si |and cli-attivo 0508:Richiesta di Mori: elaborare cmq ma non mettere in report mail
                       perform TRATTA-CLIENTE
                       perform SCRIVI-ORDINI

                       move cli-piva to sf-piva
                       read sitfin no lock 
                            invalid move 0 to sf-lince
                       end-read
                       if sf-lince < 0
                          move "-" to segn
                       else
                          move " " to segn
                       end-if
                       initialize como-riga
                       string "P.IVA: "      delimited size
                              cli-piva       delimited size
                              " - FIDO: "    delimited size
                              segn           delimited size
                              sf-lince(1:9)  delimited size
                              ","            delimited size   
                              sf-lince(10:2) delimited size
                         into como-riga
                       end-string
                       perform SETTA-RIGA-LOG

                    end-if              
                 end-perform
           end-start.                        

           move "INIZIO CONTROLLO CLIENTI / GDO" to como-riga.
           perform SETTA-RIGA-LOG.

           move low-value to sf-rec.
           start sitfin key >= sf-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sitfin next 
                         at end 
                         exit perform 
                    end-read  
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if
      *    Luciano
                    set cli-tipo-C    to true
                    move sf-cod-cli   to cli-codice
                    read clienti no lock 
                         invalid move 0 to cli-fido-extra
                    end-read
      *    Luciano fine

                    |VERIFICA 1
                    compute sf-scoperto-tot = sf-ordini-in-essere +
                                              sf-saldo-scaduto    +
                                              sf-saldo-scadenza   +
                                              sf-effetti-rischio
                    compute sf-saldo-scheda = sf-saldo-scaduto    +
                                              sf-saldo-scadenza

                    if sf-scoperto-tot = 0 and
                       sf-lince        = 0
                       set sf-verifica-1-ok to true
                    else
                       if sf-scoperto-tot <= sf-lince
                          set sf-verifica-1-ok to true
                       else
                          set sf-verifica-1-ok to false
                       end-if
                    end-if

                    |VERIFICA 2
                    if sf-den = 0 |or cli-fido-extra >= sf-den
                       move 0 to sf-perce
                    else
      *    Luciano
      *                 compute sf-perce = sf-den / sf-num * 100
                       
                       compute sf-perce = sf-den / sf-num * 100
      *****                                    (sf-den - cli-fido-extra)
      *****                                                  / sf-num * 100
      *    Luciano
                    end-if

                    if sf-perce <= tge-perce-fido
                       set sf-verifica-2-ok to true
                    else
                       set sf-verifica-2-ok to false
                    end-if

      *****              |VERIFICA 3
      *****              compute sf-fido-max = 
      *****                    ( sf-fatturato / 365 ) * 
      *****                    ( sf-gg-dilazione + 40 )
      *****
      *****              if sf-scoperto-tot = 0 and
      *****                 sf-fido-max     = 0
      *****                 set sf-verifica-3-ok to true
      *****              else
      *****                 if sf-scoperto-tot <= sf-fido-max
      *****                    set sf-verifica-3-ok to true
      *****                 else
      *****                    set sf-verifica-3-ok to false
      *****                 end-if
      *****              end-if

                    rewrite sf-rec

      *****              if sf-gdo not = spaces
      *****                 move sf-gdo to gdo-codice
      *****                 read tgrupgdo no lock invalid continue end-read
      *****                 move gdo-intestazione to cli-ragsoc-1
      *****              else
      *****                 set cli-tipo-C  to true
      *****                 move sf-cliente to cli-codice
      *****                 read clienti no lock invalid continue end-read
      *****              end-if

                    |0508: Richiesta di Mori: effetuare il calcolo ma non inviare mail
                    |16/11: Richiesta di Mori: creare mail anche 
                    |per i clienti bloccati in precedenza
      *    Luciano
      *    ho spostato la lettura prima perchè mi serve anche per la
      *    verifica 2
      *              set cli-tipo-C    to true
      *              move sf-cod-cli   to cli-codice
      *              read clienti no lock invalid continue end-read
      *    Luciano fine
                    if cli-attivo or cli-fuori-fido 

      *                se una delle prime due verifiche è errata
                       if ( sf-verifica-1 +
                            sf-verifica-2) <= 1 | +
      *                      sf-verifica-3 ) <= 1 

      *                   preparo il testo della mail
                          perform VAL-LINKBODY

                          if prima-volta
                             set prima-volta to false
                             perform OPEN-STAMPA
                          else
                             perform SALTO-PAGINA
                          end-if

                          perform SCRIVI-VERIFICA-1
                          perform SCRIVI-VERIFICA-2
      *****                    perform SCRIVI-VERIFICA-3
      *****                    perform SCRIVI-FATTURE
                       end-if
                          
                    end-if

                 end-perform
           end-start.

           move low-value to cli-rec.
           set cli-tipo-C to true.    
           start clienti key >= cli-chiave invalid continue end-start.
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F exit perform end-if    
              if RichiamoSchedulato
                 perform CONTATORE-VIDEO
              end-if
              move cli-tipo to tcl-codice
              read ttipocli no lock invalid continue end-read
                              
              if tcl-bloc-auto-si and cli-gestione-fido-si
                 move cli-piva to sf-piva
                 read sitfin no lock invalid continue end-read
                 |11/11:Richiesta di Mori: se la PRIMA verifica 
                 |non ha esito positivo bloccarlo (solo se attivo)
                 if not sf-verifica-1-ok or
                    not sf-verifica-2-ok
                    if cli-escludi-fido-no
                       if cli-attivo or cli-fuori-fido
                          set cli-bloccato to true
                          |23/05/2012
                          if not sf-verifica-1-ok
                             set cli-fuori-fido to true
                          end-if
                          if not sf-verifica-2-ok
                             set cli-prob-pag   to true
                          end-if
                          |23/05/2012
                          rewrite cli-rec invalid continue end-rewrite
                       end-if
                    end-if
                 else
                    if cli-fuori-fido
                       set cli-attivo to true
                       move spaces to cli-cau-blocco
                       rewrite cli-rec invalid continue end-rewrite
                    end-if
                 end-if
              else
                 if cli-fuori-fido
                    set cli-attivo to true
                    move spaces to cli-cau-blocco
                    rewrite cli-rec invalid continue end-rewrite
                 end-if
              end-if

              |Il cliente bloccato per problematiche pagamento viene
              |impostato se la verifica 2 non è BF. In ogni caso se
              |trovo il blocco e la verifica 2 OK, riattivo il cliente
              if cli-prob-pag and sf-verifica-2-ok
                 set cli-attivo to true
                 move spaces to cli-cau-blocco
                 rewrite cli-rec invalid continue end-rewrite
              end-if

           end-perform.

           move "FINE CONTROLLO CLIENTI / GDO" to como-riga.
           perform SETTA-RIGA-LOG.

                                          
           move "SCANSIONE CLIENTI SCADUTI" to como-riga.
           perform SETTA-RIGA-LOG.
      
           set cli-tipo-C to true.
           move low-value to cli-codice
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti next at end exit perform end-read
                    if cli-tipo-F exit perform end-if
                    move cli-tipo to tcl-codice
                    read ttipocli no lock
                         invalid
                     not invalid
                         if tcl-fido-nuovo-si
                            initialize calfido-linkage 
                                       replacing numeric data by zeroes
                                            alphanumeric data by spaces
                            move cli-codice to link-cli-codice
                            call "C$JUSTIFY"  using link-cli-codice, "R"
                            inspect link-cli-codice 
                                    replacing leading x"20" by x"30"
                            call   "calfido"  using calfido-linkage
                            cancel "calfido"
                            if saldo-scaduto > 0
                               initialize como-riga
                               string "CLIENTE: "         delimited size
                                      cli-codice          delimited size
                                      " - SCADUTO: "      delimited size
                                      saldo-scaduto(1:13) delimited size
                                      ","                 delimited size
                                      saldo-scaduto(14:2) delimited size
                                 into como-riga
                               end-string          
                               perform SETTA-RIGA-LOG
                               set cli-prob-pag to true
                               set cli-bloccato to true
                               rewrite cli-rec
                            end-if
                         end-if
                    end-read
                 end-perform
           end-start. 
      
           move "FINE SCANSIONE CLIENTI SCADUTI" to como-riga.
           perform SETTA-RIGA-LOG.


           move "FINE SCANSIONE CLIENTI" to como-riga.
           perform SETTA-RIGA-LOG.

           if not prima-volta
              set spl-chiusura to true
              call   "spooler"
              cancel "spooler"
           end-if.       

      ***---
       CONTATORE-VIDEO.
           add 1 to counter counter2

           if counter2 = 300
              move counter to counter-edit
              display counter-edit 
                      upon batch-win-handle
                      line 25,00
                    column 38,00
              move 0 to counter2
           end-if.

      ***---
       TRATTA-CLIENTE.
           set tutto-ok   to true
           initialize como-riga
           string "INTERROGAZIONE CLIENTE: " delimited size
                  cli-codice                 delimited size
                  " - "                      delimited size
                  cli-piva                   delimited size
                  " - "                      delimited size
                  cli-ragsoc-1               delimited size
                  into como-riga
           perform SETTA-RIGA-LOG.
                                  

           initialize calfido-linkage 
                      sitfin-p-linkage replacing numeric data by zeroes
                                            alphanumeric data by spaces.

           close seqlog.
           if RichiamoSchedulato               
              move batch-log        to calfido-path-log
           else
              move path-seqlog      to calfido-path-log
           end-if.
           |La setto anche da qua in quanto permette
           |a calfido di scrivere il logo
           set environment "BATCH_NOTTURNO" to "S".
           move cli-codice       to link-cli-codice.
           call   "sitfin-p"  using sitfin-p-linkage
                                    calfido-linkage.
           cancel "sitfin-p".                      
           set environment "BATCH_NOTTURNO" to " ".

           open extend seqlog.

           if calfido-status = -1
              initialize como-riga
              string "** FINE INTERROGAZIONE CLIENTE (ERR): " 
                                  delimited size
                     cli-codice   delimited size
                     " - "        delimited size
                     cli-ragsoc-1 delimited size
                     into como-riga  
              set nessun-errore to false
           else
              initialize como-riga
              string "FINE INTERROGAZIONE CLIENTE (OK): " 
                                   delimited size
                     cli-codice    delimited size
                     " - "         delimited size
                     cli-ragsoc-1  delimited size
                     into como-riga
           end-if.
           perform SETTA-RIGA-LOG.

      ***---
       SCRIVI-VERIFICA-1.
           set spl-stringa   to true.

           move zero   to spl-tipo-colonna.
           add  0,8    to spl-riga.
           move 0,5    to spl-colonna
           move Arial14BI to spl-hfont.
           set SPL-BLU to true
           move "1"    to spl-riga-stampa
           call "spooler" using spooler-link.

           move Arial8       to spl-hfont.
           add 0,2           to spl-riga.
           move 1   to spl-colonna

           move "Verifica 1:"   to spl-riga-stampa
           if sf-verifica-1-ok
              move "OK"      to spl-riga-stampa(13:)
              set spl-verde  to true
              move stato-ok  to spl-hbitmap
           else
              move "FALLITA" to spl-riga-stampa(13:)
              set spl-rosso  to true
              move stato-ko  to spl-hbitmap
           end-if
           call "spooler" using spooler-link.
      

      *    logo 
           move spl-riga  to save-spl-riga

           set  spl-bitmap  to true
           move 9   to spl-riga
           move 115 to spl-colonna
           move 0,8 to spl-bitmap-height
           move 0,7 to spl-bitmap-width
           call "spooler" using spooler-link.

           move save-spl-riga  to spl-riga

           set spl-stringa to true.
           set spl-nero    to true
           add 0,5         to spl-riga.

           move 1   to spl-colonna
           initialize spl-riga-stampa
           string "Fido:" delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link.

           move 13  to spl-colonna
           move "Esposizione:"   to spl-riga-stampa
           call "spooler" using spooler-link.

           move 2   to spl-tipo-colonna
           move sf-lince           to ed-num.
           move ed-num             to tr-num1.
           move sf-scoperto-tot    to ed-num.
           move ed-num             to tr-num2.
           move tipo-riga          to spl-riga-stampa.

           call "spooler" using spooler-link.
      
           perform LINEA-ORIZZONTALE.

      ***---
       SCRIVI-VERIFICA-2.
           set spl-stringa   to true.

           move zero   to spl-tipo-colonna.
           add 0,8           to spl-riga.
           move 0,5 to spl-colonna
           move Arial14BI to spl-hfont.
           set SPL-BLU to true
           move "2"   to spl-riga-stampa
           call "spooler" using spooler-link.

           move Arial8       to spl-hfont.
           add 0,2           to spl-riga.
           move 1   to spl-colonna

           move "Verifica 2:"   to spl-riga-stampa
           if sf-verifica-2-ok
              move "OK"      to spl-riga-stampa(13:)
              set spl-verde   to true
              move stato-ok  to spl-hbitmap
           else
              move "FALLITA" to spl-riga-stampa(13:)
              set spl-rosso  to true
              move stato-ko  to spl-hbitmap
           end-if
           call "spooler" using spooler-link.

      *    logo 
           move spl-riga  to save-spl-riga

           set  spl-bitmap  to true
           move 15  to spl-riga
           move 115 to spl-colonna
           move 0,8 to spl-bitmap-height
           move 0,7 to spl-bitmap-width
           call "spooler" using spooler-link.

           move save-spl-riga  to spl-riga

           set spl-stringa   to true
      
           set spl-nero   to true
           add 0,5        to spl-riga.
           move 1   to spl-colonna
           initialize spl-riga-stampa
           move sf-scaduto-al   to spl-riga-stampa
           call "spooler" using spooler-link.

           move 2                  to spl-tipo-colonna
           move space              to tr-num1
           move sf-num             to ed-num
           move ed-num             to tr-num2.
           move tipo-riga          to spl-riga-stampa

           call "spooler" using spooler-link.

           add 0,5        to spl-riga.
           move zero   to spl-tipo-colonna
           move 1      to spl-colonna
           initialize spl-riga-stampa
           move sf-scadenze   to spl-riga-stampa
           call "spooler" using spooler-link.

           move 2   to spl-tipo-colonna
           move space              to tr-num1
           move sf-den             to ed-num
           move ed-num             to tr-num2.
           move tipo-riga          to spl-riga-stampa

           call "spooler" using spooler-link.

      *    Luciano

      *****     move 0   to spl-tipo-colonna.
      *****     move 17  to spl-colonna.
      *****     initialize  spl-riga-stampa.
      *****     move "-" to spl-riga-stampa.
      *****     call "spooler" using spooler-link.

      *****     add 0,5        to spl-riga.
      *****     move zero   to spl-tipo-colonna
      *****     move 1      to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "Fido Extra"  to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 2   to spl-tipo-colonna
      *****     move space              to tr-num1
      *****     move cli-fido-extra     to ed-num
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa
      *****     call "spooler" using spooler-link.

      *****     move 0   to spl-tipo-colonna.
      *****     move 17  to spl-colonna.
      *****     initialize  spl-riga-stampa.
      *****     move "=" to spl-riga-stampa.
      *****     call "spooler" using spooler-link.
      *****
      *****
      *****     add 0,4        to spl-riga.
      *****     move spl-riga  to spl-riga-fine.
      *****     move 14,8      to spl-colonna.
      *****     move 17,2      to spl-colonna-fine.
      *****     set  spl-oggetto        to true.
      *****     set  spl-linea          to true.
      *****     set  spl-pen-solid      to true.
      *****     call "spooler"       using spooler-link.
      *****     add 0,1        to spl-riga.
      *****     move spl-riga  to spl-riga-fine.
      *****     call "spooler"       using spooler-link.

      *****     set spl-stringa  to true.
      *****     add 0,2          to spl-riga.
      ******     move zero      to spl-tipo-colonna
      ******     set spl-nero   to true
      ******     move 3,1      to spl-colonna
      ******     initialize spl-riga-stampa
      ******     move "Totale scoperto" to spl-riga-stampa
      ******     call "spooler" using spooler-link.
      *****
      *****     move 2                  to spl-tipo-colonna.
      *****     set spl-nero            to true.
      *****     move space              to tr-num1.
      *****     |Il fido extra copre la differenza
      **********     compute sf-den = sf-den - cli-fido-extra.
      *****     if sf-den < 0
      *****        move 0 to sf-den
      *****     end-if.   
      *****     move sf-den             to ed-num.
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa.

      *****     call "spooler" using spooler-link.

      *    Luciano fine

           add 0,5     to spl-riga.
           move zero   to spl-tipo-colonna.
           move 13     to spl-colonna.
           initialize spl-riga-stampa.
           move sf-perce-max  to spl-riga-stampa.
           call "spooler"  using spooler-link.

           if sf-verifica-2-ok
              set spl-nero   to true
           else
              set spl-rosso  to true
           end-if.

           move 2            to spl-tipo-colonna.
           move space        to tr-num1.
           move sf-perce     to ed-num.
           move ed-num       to tr-num2.
           move tipo-riga    to spl-riga-stampa.

           call "spooler" using spooler-link.

           move zero   to spl-tipo-colonna
           move 17   to spl-colonna
           initialize spl-riga-stampa
           move "%" to spl-riga-stampa
           call "spooler" using spooler-link.
           set spl-nero   to true

           perform LINEA-ORIZZONTALE.

      ********---
      ***** SCRIVI-VERIFICA-3.
      *****     set spl-stringa   to true.
      *****     move Arial8       to spl-hfont.
      *****     add 0,8           to spl-riga.
      *****     move zero   to spl-tipo-colonna.
      *****
      *****     move 0,5 to spl-colonna
      *****     move Arial14BI to spl-hfont.
      *****     set SPL-BLU to true
      *****     move "3"   to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 1   to spl-colonna
      *****     add 0,2           to spl-riga.
      *****
      *****     set spl-nero      to true
      *****     move Arial8       to spl-hfont.
      *****     move "Verifica 3:"   to spl-riga-stampa
      *****     if sf-verifica-3-ok
      *****        move "OK"      to spl-riga-stampa(13:)
      *****        set spl-verde  to true
      *****        move stato-ok  to spl-hbitmap
      *****     else
      *****        move "FALLITA" to spl-riga-stampa(13:)
      *****        set spl-rosso  to true
      *****        move stato-ko  to spl-hbitmap
      *****     end-if
      *****     call "spooler" using spooler-link.
      *****
      ******    logo 
      *****     move spl-riga  to save-spl-riga
      *****
      *****     set  spl-bitmap  to true
      *****     move 27  to spl-riga
      *****     move 115 to spl-colonna
      *****     move 0,8 to spl-bitmap-height
      *****     move 0,7 to spl-bitmap-width
      *****     call "spooler" using spooler-link.
      *****
      *****     move save-spl-riga  to spl-riga
      *****
      *****     set spl-stringa   to true
      *****     set spl-nero   to true
      *****     add 0,5        to spl-riga.
      *****     move zero   to spl-tipo-colonna
      *****     move 1      to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move sf-fat-per   to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****     add 0,3        to spl-riga.
      ******     move zero   to spl-tipo-colonna
      *****     move 1      to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "(IVA compresa)"  to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 2   to spl-tipo-colonna
      *****     move space              to tr-num1
      *****     move sf-fatturato       to ed-num
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa
      *****
      *****     call "spooler" using spooler-link.
      *****
      *****     add 0,5        to spl-riga.
      *****     move zero   to spl-tipo-colonna
      *****     move 1      to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "Fido Massimo accordabile:" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 2   to spl-tipo-colonna
      *****     move space              to tr-num1
      *****     move sf-fido-max        to ed-num
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa
      *****
      *****     call "spooler" using spooler-link.
      *****
      *****     add 0,3               to spl-riga.
      *****     move zero             to spl-tipo-colonna.
      *****     move 1                to spl-colonna.
      *****     move sf-gg-dilazione  to ed-num-gg.
      *****     initialize spl-riga-stampa.
      *****     string "Fatturato/365 * (gg 40 + " delimited size
      *****            "gg dilazione concessi "    delimited size
      *****            ed-num-gg                   delimited size
      *****            ")"                         delimited size
      *****            into spl-riga-stampa
      *****     end-string.
      *****     call "spooler" using spooler-link.
      *****
      *****     add 0,3           to spl-riga.
      *****     move zero         to spl-tipo-colonna.
      *****     move 3,5          to spl-colonna.
      *****     move spaces       to spl-riga-stampa.
      *****     call "spooler" using spooler-link.
      *****
      *****     add 0,5        to spl-riga.
      *****     move zero   to spl-tipo-colonna
      *****     move 1      to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "Scoperto totale:" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 3,1      to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "Saldo Scheda:" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 2   to spl-tipo-colonna
      *****     set spl-rosso  to true
      *****     move space              to tr-num1
      *****     move sf-saldo-scheda    to ed-num
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 0   to spl-tipo-colonna
      *****     move 17  to spl-colonna
      *****     initialize  spl-riga-stampa
      *****     move "+" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****     
      *****
      *****     move Arial6    to spl-hfont.            
      *****     add  0,4       to spl-riga.
      *****     move 0         to spl-tipo-colonna
      *****     set spl-nero   to true
      *****     move 3,1       to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "di cui scaduti" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 2   to spl-tipo-colonna
      *****     set spl-rosso  to true
      *****     move space              to tr-num1
      *****     move sf-saldo-scaduto   to ed-num
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****                            
      *****     add 0,3        to spl-riga.
      *****     move 0         to spl-tipo-colonna
      *****     set spl-nero   to true
      *****     move 3,1       to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "e a scadenza" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 2   to spl-tipo-colonna
      *****     set spl-rosso  to true
      *****     move space              to tr-num1
      *****     move sf-saldo-scadenza  to ed-num
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move Arial8       to spl-hfont.
      *****
      *****     add 0,4        to spl-riga.
      *****     move zero      to spl-tipo-colonna
      *****     set spl-nero   to true
      *****     move 3,1      to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "RB a scadere" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 2   to spl-tipo-colonna
      *****     set spl-rosso  to true
      *****     move space              to tr-num1
      *****     move sf-effetti-rischio to ed-num
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move zero   to spl-tipo-colonna
      *****     move 17   to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "+" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     add 0,5        to spl-riga.
      *****     move zero      to spl-tipo-colonna
      *****     set spl-nero   to true
      *****     move 3,1      to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "Ordini in essere" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 2   to spl-tipo-colonna
      *****     set spl-rosso  to true
      *****     move space              to tr-num1
      *****     move sf-ordini-in-essere to ed-num
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move zero   to spl-tipo-colonna
      *****     move 17   to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "=" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     add 0,4        to spl-riga
      *****     move spl-riga  to spl-riga-fine
      *****     move 14,8      to spl-colonna
      *****     move 17,2      to spl-colonna-fine
      *****     set  spl-oggetto        to true.
      *****     set  spl-linea          to true.
      *****     set  spl-pen-solid      to true.
      *****     call "spooler"       using spooler-link.
      *****     add 0,1        to spl-riga
      *****     move spl-riga  to spl-riga-fine
      *****     call "spooler"       using spooler-link.
      *****
      *****     set spl-stringa   to true
      *****     add 0,2        to spl-riga.
      *****     move zero      to spl-tipo-colonna
      *****     set spl-nero   to true
      *****     move 3,1      to spl-colonna
      *****     initialize spl-riga-stampa
      *****     move "Totale scoperto" to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     move 2   to spl-tipo-colonna
      *****     set spl-rosso  to true
      *****     move space              to tr-num1
      *****     move sf-scoperto-tot    to ed-num
      *****     move ed-num             to tr-num2.
      *****     move tipo-riga          to spl-riga-stampa
      *****     call "spooler" using spooler-link.
      *****
      *****     set spl-nero   to true
      *****     perform LINEA-ORIZZONTALE.

      ***---
       SETTA-RIGA-LOG.
           initialize riga-log.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-log
           end-string.
           write riga-log.

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

      ********---
      ***** INVIO-MAIL.
      ******     perform WAIT-3-SECS.
      ******     perform WAIT-3-SECS.
      ******     perform WAIT-3-SECS.
      ******     perform WAIT-3-SECS.
      *****     perform ASPETTA-PDF.
      *****
      *****
      *****
      *****     move "INVIO MAIL IN CORSO..." to como-riga.
      *****     perform SETTA-RIGA-LOG.
      *****
      ******     initialize LinkBody.
      *****
      *****     move "SITUAZIONE FINANZIARIA CLIENTI" 
      *****            to LinkSubject
      ******     initialize linkBody
      *****
      ******     perform VAL-LINKBODY
      *****
      *****     accept LinkAddress from environment "MAIL_FIDO_ADDRESSES".
      *****     move path-pdf  to LinkAttach
      *****
      *****     set errori to true.
      *****     move 0 to tentativi.
      *****     perform 10 times
      *****        add 1 to tentativi
      *****        perform SEND-MAIL
      *****        
      *****        initialize como-riga
      *****        if StatusInvioMail = -1
      *****           string |r-inizio                      delimited size
      *****                  "TENTATIVO N. "               delimited size
      *****                  tentativi                     delimited size
      *****                  ": "                          delimited size
      *****                  "Chiamata InvioMail fallita!" delimited size
      *****                  " STATUS -1"                  delimited size
      *****                  into como-riga
      *****           end-string
      *****        else
      *****           string |r-inizio                       delimited size
      *****                  "TENTATIVO N. "                delimited size
      *****                  tentativi                      delimited size
      *****                  ": "                           delimited size
      *****                  "Chiamata InvioMail riuscita!" delimited size
      *****                  into como-riga
      *****           end-string
      *****        end-if
      *****        perform SETTA-RIGA-LOG
      *****                      
      *****        call "C$DELETE" using FileDest
      *****        open input lineseq1
      *****        read  lineseq1 next
      *****        if line-riga of lineseq1 = "True"
      *****           set tutto-ok to true
      *****           close lineseq1
      *****           exit perform
      *****        end-if
      *****        close lineseq1
      *****
      *****
      *****        initialize como-riga
      *****        string |r-inizio              delimited size
      *****               "TENTATIVO N. "       delimited size
      *****               tentativi             delimited size
      *****               ": "                  delimited size
      *****               line-riga of lineseq1 delimited size
      *****               into como-riga
      *****        end-string
      *****        perform SETTA-RIGA-LOG
      *****     end-perform
      *****         
      *****     initialize como-riga.
      *****     if tutto-ok
      *****        move "INVIO MAIL RIUSCITO!"   to como-riga
      *****     else
      *****        string "INVIO MAIL NON RIUSCITO! " delimited size
      *****               line-riga of lineseq1       delimited by size
      *****               into como-riga
      *****        end-string
      *****     end-if.
      *****     if on-line
      *****        inspect como-riga replacing trailing space by low-value
      *****        display message box como-riga
      *****               title titolo
      *****
      *****     else
      *****        perform SETTA-RIGA-LOG
      *****     end-if
      ******     perform SETTA-RIGA-LOG.
      *****
      *****     delete file lineseq.

      ***---
       OPEN-STAMPA.
           perform CREA-PDF.
           if settaPDF-OK
              accept selprint-stampante 
              from environment "STAMPANTE_SITFIN_PDF"
           else
              move spaces to selprint-stampante
           end-if

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE
      
              set spl-vertical        to true
              set spl-apertura        to true
              move 1                  to spl-margine-sinistro
              move 1                  to spl-margine-destro
              move 1                  to spl-margine-inf
              move "Stampa situazione finanziaria clienti"   
                                      to spl-nome-job
              call "spooler"        using spooler-link
           else
              set spl-sta-annu to true
           end-if.
      
           if spl-sta-annu    
              move "ERRORE APERTURA STAMPANTE" to como-riga
              perform SETTA-RIGA-LOG 
              set nessun-errore to false
           else
              perform SETTA-FONT
              set spl-stringa to true
              |Mi riposiziono ad inizio foglio
              move 1            to spl-colonna
              move 1            to spl-riga
              move spaces       to spl-riga-stampa
              move arial8 to spl-hfont
              call "spooler" using spooler-link
           end-if.

           perform STAMPA-TESTA.

      ***---
       STAMPA-TESTA.
           add 1 to num-pagina

           perform FINCATURA.

           set spl-stringa   to true.
           move Arial14BI    to spl-hfont.
           add 0,5           to spl-riga.

           move 1   to spl-tipo-colonna.
           initialize spl-riga-stampa.
           
      *****     if sf-gdo = spaces
      *****        move sf-cliente to spl-riga-stampa
      *****     else
      *****        move sf-gdo     to spl-riga-stampa
      *****     end-if.

           inspect spl-riga-stampa replacing leading x"30" by x"20".
           call "C$JUSTIFY" using spl-riga-stampa, "L".
           inspect spl-riga-stampa 
                                replacing trailing spaces by low-value.
           inspect sf-ragsoc    replacing trailing spaces by low-value.
           string  spl-riga-stampa delimited low-value
                   sf-ragsoc       delimited low-value
                   " - "           delimited size
                   sf-piva         delimited size
                   into spl-riga-stampa
           end-string.
           inspect spl-riga-stampa 
                                replacing trailing low-value by spaces.

           call "spooler" using spooler-link.

      ***---
       CREA-PDF.
           accept como-ora  from time.
           accept como-data from century-date.
           accept DestFile from environment "PATH_ST_CLIENT".

      *****     inspect DestFile replacing trailing 
      *****                               spaces by low-value.

           string "Situazione_finanziaria_clienti"   delimited size  
                  "_("             delimited size
                  como-data(7:2)   delimited by size
                  "-"              delimited size
                  como-data(5:2)   delimited by size
                  "-"              delimited size
                  como-data(1:4)   delimited by size
                  "_-_"            delimited size
                  como-ora(1:2)    delimited size
                  "."              delimited by size
                  como-ora(3:2)    delimited size
                  ")"                delimited size
                  into NomeFile
           end-string.
                  

           set settaPDF-setta   to true

           move NomeFile  to settaPDF-nome-file
           move DestFile  to settaPDF-percorso
           call   "settaPDF2" using settaPDF-linkage
           cancel "settaPDF2".

      *****     inspect NomeFile 
      *****             replacing trailing spaces by low-value.
      *****     string NomeFile   delimited low-value
      *****            ".pdf"     delimited size
      *****            into NomeFile
      *****
      *****     inspect DestFile 
      *****             replacing trailing spaces by low-value.

      *****     if not settaPDF-OK       
      *****        display message "Archiviazione PDF fallita!"
      *****                  title titolo
      *****                   icon 2
      *****     end-if.
      *****     evaluate true
      *****     when ru-SO-XP
      *****          accept selprint-stampante from environment
      *****                 "STAMPANTE_SITFIN_PDF_XP"
      *****     when ru-SO-VISTA
      *****          accept selprint-stampante from environment
      *****                 "STAMPANTE_SITFIN_PDF_V"
      *****     when ru-SO-7
      *****          accept selprint-stampante from environment
      *****                 "STAMPANTE_SITFIN_PDF_7"
      *****     end-evaluate.
      *****
      *****     inspect NomeFile 
      *****             replacing trailing spaces by low-value.
      *****     inspect DestFile 
      *****             replacing trailing spaces by low-value.
      *****     inspect selprint-stampante 
      *****             replacing trailing spaces by low-value.

      *     initialize parametri.
      *     string NomeFile           delimited low-value
      *            "§"                delimited size
      *            DestFile           delimited low-value
      *            "§"                delimited size
      *            selprint-stampante delimited low-value
      *            into parametri
      *     end-string.

                             
      *****     initialize path-pdf
      *****     accept path-pdf from environment "PATH_ST".
      *****     inspect path-pdf replacing trailing space by low-value
      *****           
      *****     string path-pdf   delimited by low-value
      *****            NomeFile   delimited by low-value
      *****            into path-pdf
      *****     inspect path-pdf replacing trailing low-value by space

      *     accept  PathCreaPDF from environment "PATH_EXE_PDF".
      *     inspect PathCreaPDF replacing trailing spaces by low-value.
      *
      *     initialize comando.
      *     string PathCreaPDF delimited low-value
      *            " "         delimited size
      *            parametri   delimited size
      *            into comando
      *     end-string.
                             
      *     move 0 to StatusCreaPDF.
      *     call "C$SYSTEM" using comando, 129
      *                    giving StatusCreaPDF
      *     if StatusCreaPDF = -1
      *        display message "Archiviazione PDF fallita!"
      *                  |title titolo
      *                   icon 2
      *     end-if.
      *     perform WAIT-3-SECS.

      *****     if not settaPDF-OK       
      *****        display message "Archiviazione PDF fallita!"
      *****                  title titolo
      *****                   icon 2
      *****     end-if.

      ***---
       SETTA-FONT.
           set tutto-ok             to true.
           initialize wfont-data.
           move 14                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to true
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial14BI, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           set tutto-ok             to true.
           initialize wfont-data.
           move 8                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfcharset-dont-care  to true    
           set wfont-bold           to false.
           set wfont-italic         to false
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial8, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

           set tutto-ok             to true.
           initialize wfont-data.
           move 6                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfcharset-dont-care  to true    
           set wfont-bold           to false.
           set wfont-italic         to false
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial6, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.


           initialize wfont-data.
           move 20                  to wfont-size.
           move "Arial"             to wfont-name.
           set wfont-bold           to true.
           set wfcharset-dont-care  to true    
           set wfont-italic         to true
           set wfont-underline      to false   
           set wfont-strikeout      to false   
           set wfont-fixed-pitch    to false   
           move zero                to wfont-char-set
           set wfdevice-win-printer to true.
           CALL "W$FONT" USING wfont-get-closest-font, 
                               Arial20BI, wfont-data
                               giving wfont-status.
      
           if wfont-status = wfonterr-font-not-found
              set errori to true
              perform MESSAGGIO-ERR-FONT
           end-if.

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina     to true.
           call "spooler"        using spooler-link.                                                            
           set spl-stringa to true
           move 0      to spl-riga
           move spaces to spl-riga-stampa
           call "spooler" using spooler-link.
           perform STAMPA-TESTA.

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.
      
           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.
      
           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.
      
           inspect messaggio replacing trailing SPACE by LOW-VALUE.
      
           display message messaggio.

      ***---
       FINCATURA.
      *    riga Ordine
           set spl-stringa       to true.
           move Arial14BI to spl-hfont.
           move 0,2              to spl-riga.
           move 1   to spl-tipo-colonna
           move "SITUAZIONE FINANZIARIA CLIENTI"  to spl-riga-stampa
           call "spooler" using spooler-link.

           move zero   to spl-tipo-colonna.

           add 0,6  to spl-riga
           move 0,5 to spl-colonna

           move Arial8 to spl-hfont.
           initialize spl-riga-stampa
           string "stampata il "   delimited by size
                  como-data(7:2)   delimited by size
                  "/"              delimited by size
                  como-data(5:2)   delimited by size
                  "/"              delimited by size
                  como-data(1:4)   delimited by size
                  " alle "         delimited by size
                  como-ora(1:2)    delimited by size
                  ":"              delimited by size
                  como-ora(3:2)    delimited by size
                  into spl-riga-stampa

           call "spooler" using spooler-link.

           move 18  to spl-colonna
           move num-pagina   to num-pag-ed
           initialize spl-riga-stampa
           string "Pag. "    delimited by size
                  num-pag-ed delimited by size
                  into spl-riga-stampa

           call "spooler" using spooler-link.

           perform LINEA-ORIZZONTALE
           set spl-stringa   to true.

      ***---
       LINEA-ORIZZONTALE.
           add 0,4        to spl-riga
           move spl-riga  to spl-riga-fine
           move 0,1       to spl-colonna
           move 19,1      to spl-colonna-fine
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

      ***---
       SCRIVI-ORDINI.
      *****     if cli-gdo = spaces
      *****        move cli-codice     to tsf-cliente
      *****        move spaces         to tsf-gdo
      *****     else
      *****        move spaces         to tsf-cliente
      *****        move cli-gdo        to tsf-gdo
      *****     end-if.
           move cli-piva to tsf-piva.

           read tmp-sitfin no lock invalid move 0 to tsf-valore end-read

           move cli-codice        to tor-cod-cli
           set  tor-no-agg-contab to true
           move low-value         to tor-data-fattura
           start tordini key >= k-andamento-cliente 
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if cli-codice not = tor-cod-cli
                       exit perform
                    end-if
                    if not tor-no-agg-contab
                       exit perform
                    end-if
                    if tor-data-fattura not = 0
                       exit perform
                    end-if
      *             Valorizzo il tmp dei valori suddivisi per giorno
                    move tor-data-ordine to tsf-data
                    move tor-anno        to ror-anno
                    move tor-numero      to ror-num-ordine
                    move low-value       to ror-num-riga
                    move 0               to tsf-valore

                    start rordini key >= ror-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read rordini next 
                                  at end exit perform 
                             end-read
                             if tor-anno   not = ror-anno or
                                tor-numero not = ror-num-ordine
                                exit perform

                             end-if

                             compute tsf-valore = tsf-valore        +
                                                ( ror-imp-consumo   +
                                                  ror-imp-cou-cobat +
                                                  ror-imponib-merce +
                                                  ror-add-piombo )  *
                                                  ror-qta
                          end-perform
                    end-start
                    if tsf-valore not = 0
                       write tsf-rec invalid rewrite tsf-rec end-write
                    end-if
                 end-perform
           end-start.

      ***---
       SCRIVI-FATTURE.
           set spl-stringa   to true.
           add 0,5           to spl-riga.
           set spl-nero      to true.
           move zero         to spl-tipo-colonna.
           move 1            to spl-colonna.
           initialize spl-riga-stampa.
           move "Ordini non fatturati suddivisi per giorno" 
                                                  to spl-riga-stampa.
           call "spooler" using spooler-link.

      *    scorro il tmp
           move 0           to num-giorni.
           move cli-piva    to tsf-piva.
           move low-value   to tsf-data.
           start tmp-sitfin key >= tsf-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-sitfin next
                         at end exit perform
                    end-read
                    if tsf-piva not = sf-piva
                       exit perform
                    end-if
                    perform SCRIVI-GIORNO
                 end-perform
           end-start.

      ***---
       SCRIVI-GIORNO.
           add 1 to num-giorni
           if num-giorni > max-giorni
              perform SALTO-PAGINA
              add 0,5  to spl-riga
              perform LINEA-ORIZZONTALE
              set spl-stringa   to true

              move Arial8       to spl-hfont

              move 1   to num-giorni
           end-if.

           set spl-stringa   to true

           add 0,5           to spl-riga
           set spl-nero      to true
           move zero         to spl-tipo-colonna
           move 1            to spl-colonna
           initialize spl-riga-stampa
           string tsf-data(7:2) delimited by size
                  "/"           delimited by size
                  tsf-data(5:2) delimited by size
                  "/"           delimited by size
                  tsf-data(1:4) delimited by size
                  into spl-riga-stampa
           call "spooler" using spooler-link
      
           move 2   to spl-tipo-colonna
      *     set spl-rosso           to true
           move space              to tr-num2
           move tsf-valore         to ed-num
           move ed-num             to tr-num1
           move tipo-riga          to spl-riga-stampa
           call "spooler" using spooler-link.

      ***--
       CLOSE-FILES.
           close clienti tordini rordini sitfin tparamge tmp-sitfin
                 tgrupgdo ttipocli.
           delete file tmp-sitfin.

      ***---
       EXIT-PGM.
           move "TERMINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-LOG.

           CALL "w$bitmap" USING WBITMAP-DESTROY, stato-ok
           CALL "w$bitmap" USING WBITMAP-DESTROY, stato-ko

           destroy Arial14BI.
           destroy Arial8.
           destroy Arial6.

           destroy Arial20BI.
           if RichiamoSchedulato
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
           close seqlog.

           goback.

      ********---
      ***** ASPETTA-PDF.
      *****     move path-pdf to como-nome-file.
      *****     move 0 to cont.
      *****     inspect como-nome-file 
      *****             tallying cont for characters before ")".
      *****     move ".pdf " to como-nome-file(cont + 1: 5).
      *****
      *****     set trovato to false.
      *****     perform 60 times
      *****        CALL "C$FILEINFO" USING path-pdf,
      *****                                file-info, 
      *****                         GIVING status-code
      *****        if status-code = 0
      *****           set trovato to true
      *****           exit perform
      *****        else
      *****           CALL "C$FILEINFO" USING como-nome-file,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****           if status-code = 0
      *****              move como-nome-file to path-pdf
      *****              set trovato to true
      *****              exit perform
      *****           end-if
      *****        end-if
      *****        call "c$sleep" using 1
      *****     end-perform.
      *****
      *****     if trovato
      *****        move 0  to old-size
      ******       aspetto finchè non esiste fisicamente il file
      *****        move 99 to minuti-partenza
      *****        perform until 1 = 2
      *****           CALL "C$FILEINFO" USING path-pdf,
      *****                                   file-info, 
      *****                            GIVING status-code
      *****        
      *****           if status-code = 0
      *****              if FILE-SIZE not = 0
      *****                 if FILE-SIZE = old-size
      *****                    exit perform
      *****                 else
      *****                    move FILE-SIZE to old-size
      *****                    call "c$sleep" using 1
      *****                 end-if
      *****              end-if
      *****           else
      *****              perform TIME-OUT
      *****              if time-out-exit
      *****                 exit perform
      *****              end-if
      *****           end-if
      *****        
      *****        end-perform
      *****     end-if.

           set settaPDF-resetta   to true.
           call   "settaPDF" using settaPDF-linkage
           cancel "settaPDF".
       
      ***---
       TIME-OUT.
      *    tengo 2 minuti di time-out
           set time-out-exit to false.
           accept como-time from time.

           if minuti-partenza = 99
              move como-time(3:2)  to minuti-partenza
           end-if.

           move como-time(3:2)  to minuti-arrivo.

           if minuti-arrivo < minuti-partenza
              add 60 to minuti-arrivo
           end-if.
           subtract minuti-partenza from minuti-arrivo.

           if minuti-arrivo >= 3
              set time-out-exit to true
           else
              call "c$sleep" using 1
           end-if.

      ***---
       VAL-LINKBODY.
           if prima-volta
              initialize linkbody
              string "In allegato la situazione completa." 
                                delimited by size
                     x"0D0A"    delimited by size
                     "Di seguito una sintesi dei clienti con problemi"
                                delimited by size
                     into LinkBody
           end-if.


           if sf-verifica-1-ok
              move "OK"      to como-verifica-1
           else
              move "FALLITA" to como-verifica-1
           end-if
           if sf-verifica-2-ok
              move "OK"      to como-verifica-2
           else
              move "FALLITA" to como-verifica-2
           end-if
      *****     if sf-verifica-3-ok
      *****        move "OK"      to como-verifica-3
      *****     else
      *****        move "FALLITA" to como-verifica-3
      *****     end-if


           inspect sf-ragsoc    replacing trailing spaces by low-value.

           inspect LinkBody replacing trailing space by low-value.
           string LinkBody         delimited by low-value
                  x"0D0A"          delimited by size
                  sf-ragsoc        delimited low-value
                  " - "            delimited size
                  sf-piva          delimited size
                  x"0D0A20202020"  delimited by size
                  "Verifica 1: "   delimited by size
                  como-verifica-1  delimited by size
                  " "              delimited by size
                  "Verifica 2: "   delimited by size
                  como-verifica-2  delimited by size
      *****            " "              delimited by size
      *****            "Verifica 3: "   delimited by size
      *****            como-verifica-3  delimited by size
                  into LinkBody

           inspect LinkBody replacing trailing low-value by space.

      ***---
       PARAGRAFO-COPY.
      *****     copy "mail.cpy".
