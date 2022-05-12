       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-esiti.
       AUTHOR.                          Luciano.
       REMARKS. programma di importazione esiti di consegna. Batch 
                schedulato sul server. Il programma scorre tutti i 
                vettori. Nel log di SYSER metto il log generale. Per 
                ogni vettore con abilitato l'import chiamo l'apposito
                programma che genera il log specifico dell'importazione.
                nel log generale viene riportato ora di inzio singola
                importazione ed esito importazione.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "tvettori.sl".
           copy "vettel.sl". 
           COPY "lineseq-mail.sl".

      * select rep-recupero
      *     assign       to path-rep-recupero
      *     organization is line sequential
      *     access mode  is sequential
      *     file status  is status-rep-recupero.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
           copy "tsetinvio.fd".
           copy "tvettori.fd".
           copy "vettel.fd".
           COPY "lineseq-mail.fd".

      * FD  rep-recupero.
      * 01 riga-recupero    pic x(100).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "mail.def".
           copy "link-imp-esiti-p.def".


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

       01 r-stampa.
         05 r-tipo         pic x.
         05 r-codice       pic z(6).
         05 filler         pic x(3) value " - ".
         05 r-descrizione  pic x(30).
         05 filler         pic x(2). 
         05 r-marca        pic x(25).
         05 filler         pic x(2).
         05 r-prz          pic ----.--9,99.
         05 filler         pic x(2).
         05 r-mese         pic x.

       77  status-tvettori         pic xx.
       77  status-tsetinvio        pic xx.
       77  status-vettel           pic xx.
       77  status-lineseq-mail     pic xx.
       77  path-lineseq-mail       pic x(256).

       77  RENAME-STATUS           pic 9(9)            comp-4.

      * 77  path-rep-recupero     pic x(256) value spaces.
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).
       77  link-mese             pic 9(2) value 0.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-riga             pic x(100).
       77  riga-stampa           pic x(100).
       77  var1                  pic 9(4).
       77  anno-verifica         pic 9(4).
       01  fine-mese.
           05 anno               pic 9999.
           05 mese               pic 99.
           05 giorno             pic 99.
       77  data-rical            pic 9(8).
       77  tentativi             pic 99.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.
       77  filler                pic 9.
           88 trovato-movim      value 1, false 0.
       77  stato                 pic 9.
           88 nessun-errore      value 1.
           88 ok-recupero        value 2.
           88 errore-ko          value 3.

       78  barra value "\".

       77  Daemon-Main-Dir          pic X(256)          value SPACES.
       77  Daemon-Sub-IN            pic X(256)          value SPACES.
       77  Daemon-Sub-PROC          pic X(256)          value SPACES.
       77  Daemon-Sub-ERR           pic X(256)          value SPACES.
       77  Daemon-Sub-TMP           pic X(256)          value SPACES.
       77  Daemon-Sub-ORIG          pic X(256)          value SPACES.
       77  Daemon-Sub-LOG           pic X(256)          value SPACES.

       77  pattern                 pic X(50)   value SPACES.
       77  file-name               pic X(50)   value SPACES.
       77  dir-Handle              handle.
       77  MSG-Folder-Name         pic X(256)  value SPACES.
       77  origine                 pic X(256)  value SPACES.
       77  destinazione            pic X(256)  value SPACES.
       77  num-ed                  pic z(9)9.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       copy "mail-decl.cpy".

      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tvettori 
           when "35"
                move "File [TVETTORI] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [TVETTORI] mismatch size!" to como-riga
                set errori to true
           when "98"
                move "[TVETTORI] Indexed file corrupt!" to como-riga
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       VETTEL-ERR SECTION.
           use after error procedure on vettel.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-vettel 
           when "35"
                move "File [VETTEL] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [VETTEL] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[VETTEL] Indexed file corrupt!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ****---
      * PROGMAG-ERR SECTION.
      *     use after error procedure on progmag.
      *     set RecLocked to false.
      *     set tutto-ok  to true.
      *     evaluate status-progmag 
      *     when "35"
      *          move "File [PROGMAG] inesistente" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "39"
      *          move "File [PROGMAG] mismatch size!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "98"
      *          move "[PROGMAG] Indexed file corrupt!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "93"
      *     when "99" set RecLocked to true
      *     end-evaluate.
      *
      ****---
      * PROGMAGRIC-ERR SECTION.
      *     use after error procedure on progmagric.
      *     set RecLocked to false.
      *     set tutto-ok  to true.
      *     evaluate status-progmagric 
      *     when "35"
      *          move "File [PROGMAGRIC] inesistente" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "39"
      *          move "File [PROGMAGRIC] mismatch size!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "98"
      *          move "[PROGMAGRIC] Indexed file corrupt!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "93"
      *     when "99" set RecLocked to true
      *     end-evaluate.
      *
      ****---
      * TPARAMGE-ERR SECTION.
      *     use after error procedure on tparamge.
      *     set RecLocked to false.
      *     set tutto-ok  to true.
      *     evaluate status-tparamge 
      *     when "35"
      *          move "File [TPARAMGE] inesistente" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "39"
      *          move "File [TPARAMGE] mismatch size!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "98"
      *          move "[TPARAMGE] Indexed file corrupt!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "93"
      *     when "99" set RecLocked to true
      *     end-evaluate.
      *
      ****---
      * ARTICOLI-ERR SECTION.
      *     use after error procedure on articoli.
      *     set RecLocked to false.
      *     set tutto-ok  to true.
      *     evaluate status-articoli
      *     when "35"
      *          move "File [ARTICOLI] inesistente" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "39"
      *          move "File [ARTICOLI] mismatch size!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "98"
      *          move "[ARTICOLI] Indexed file corrupt!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "93"
      *     when "99" set RecLocked to true
      *     end-evaluate.
      *
      ****---
      * TIMPOSTE-ERR SECTION.
      *     use after error procedure on timposte.
      *     set RecLocked to false.
      *     set tutto-ok  to true.
      *     evaluate status-articoli
      *     when "35"
      *          move "File [TIMPOSTE] inesistente" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "39"
      *          move "File [TIMPOSTE] mismatch size!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "98"
      *          move "[TIMPOSTE] Indexed file corrupt!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "93"
      *     when "99" set RecLocked to true
      *     end-evaluate.
      *
      ****---
      * TMARCHE-ERR SECTION.
      *     use after error procedure on tmarche.
      *     set RecLocked to false.
      *     set tutto-ok  to true.
      *     evaluate status-tmarche
      *     when "35"
      *          move "File [TMARCHE] inesistente" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "39"
      *          move "File [TMARCHE] mismatch size!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "98"
      *          move "[TMARCHE] Indexed file corrupt!" to como-riga
      *          perform SETTA-RIGA-STAMPA
      *          set errori to true
      *     when "93"
      *     when "99" set RecLocked to true
      *     end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
              perform INVIO-MAIL-LOG
           else
              set errore-ko to true
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           set tutto-ok      to true.
           set nessun-errore to true.
           set prima-volta   to true.

           accept imp-esiti-p-user from environment "USER_IMPORT_ESITI".

      ***---
       OPEN-FILES.
           move "APERTURA FILES" to como-riga.
           perform SETTA-RIGA-STAMPA.

           |Lanciando di notte non devo farte 
           |particolari controlli sul lock
           if tutto-ok
              open input tvettori 
                         vettel
           end-if.
           if errori
              move "APERTURA FILES NON RIUSCITA" to como-riga
           else
              move "APERTURA FILES RIUSCITA" to como-riga
           end-if.
           perform SETTA-RIGA-STAMPA.

      ****---
      * OPEN-OUTPUT-PROGMAGRIC-LOCK.
      *     |Lo apro in lock per verificare che attualmente non ci 
      *     |sia dentro nessuno. Il controllo dell'apertura con
      *     |lock passa poi direttamente al pgm. "wprogmagric".
      *     open output progmagric.
      *     if RecLocked
      *        set errori to true
      *        move "[PROGMAGRIC] GIA' IN USO!!!" to como-riga
      *        perform SETTA-RIGA-STAMPA
      *     end-if.

      ***---
       ELABORAZIONE.
           move "SCANSIONE DEI VETTORI CON IMPORT ABILITATO"
             to como-riga.
           perform SETTA-RIGA-STAMPA.

           move low-value to vet-chiave
           start tvettori key not < vet-chiave
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read tvettori next 
                       at end 
                          exit perform 
                    end-read

                    move vet-codice   to vtt-codice
                    read vettel
                       invalid
                          initialize vtt-nome-flusso-imp
                    end-read
                    if vtt-nome-flusso-imp not = space
                       perform IMPORT-VETTORE
                    end-if
                 end-perform
           end-start.

           move "FINE SCANSIONE VETTORI" to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       IMPORT-VETTORE.
           set tutto-ok   to true
           initialize como-riga
           string "IMPORTAZIONE VETTORE: "  delimited by size
                  vet-sigla                 delimited by size
                  " - "                     delimited by size
                  vet-descrizione           delimited by size
                  into como-riga
           perform SETTA-RIGA-STAMPA.
           
      *    controllo l'esistenza

           perform CHECK-CARTELLE

           if tutto-ok
              perform COPIA-FILE-RETE
           end-if.

           if tutto-ok
              perform IMPORTA-FILE
           end-if.

           string "FINE IMPORTAZIONE VETTORE: "  delimited by size
                  vet-sigla                 delimited by size
                  " - "                     delimited by size
                  vet-descrizione           delimited by size
                  into como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       IMPORTA-FILE.
           move "INIZIO IMPORTAZIONE FILE" to como-riga
           perform SETTA-RIGA-STAMPA

      *    apro la cartella di origine rete con il filtro sul nome del file
           move vtt-nome-flusso-imp   to pattern

           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         Daemon-Sub-orig,
                                         pattern.
           move RETURN-CODE        to Dir-Handle.

           if dir-handle = 0       
              initialize como-riga
              string "NESSUN FILE PRESENTE CON "  delimited by size
                     pattern   delimited by size
                     into como-riga
              perform SETTA-RIGA-STAMPA
           else
              inspect Daemon-Sub-orig
                                replacing trailing space by low-value
              perform until 1 = 2
      *          CONTROLLO L'ESISTENZA DEI FLUSSI
               
                 call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
                                               Dir-Handle,
                                               File-Name
               
                 if File-Name not = "." or ".."
                    if File-Name = SPACES
                       exit perform
                    end-if
                 end-if
                 set trovato to true     
                 move Daemon-Sub-orig to imp-esiti-p-path-orig
                 move file-name       to imp-esiti-p-file-name
                 move vet-codice      to imp-esiti-p-vettore
                 call   "imp-esiti-p" using imp-esiti-p-linkage
                 cancel "imp-esiti-p"                       
                 inspect file-name replacing trailing space by low-value
                 if imp-esiti-p-status = zero
                    string "FILE: "   delimited by size
                           file-name  delimited by low-value
                           " IMPORTATO CORRETTAMENTE"  delimited by size
                           into como-riga
                 else
                    string "FILE: "   delimited by size
                           file-name  delimited by low-value
                           " IMPORTATO CON ERRORI"  delimited by size
                           into como-riga
                 end-if
                 perform SETTA-RIGA-STAMPA
                 move imp-esiti-p-num-imp   to num-ed
                 initialize como-riga
                 string "IMPORTATI "  delimited by size
                        num-ed        delimited by size
                        " ESITI"      delimited by size
                        into como-riga
                 perform SETTA-RIGA-STAMPA
                 if imp-esiti-p-status not = zero              
                    perform INVIO-MAIL
                 end-if
                 perform ARCHIVIAZIONE-FILE
              end-perform
           end-if

           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle.


           move "FINE IMPORTAZIONE FILE" to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       ARCHIVIAZIONE-FILE.
           initialize como-riga
           string "ARCHIVIAZIONE FILE "  delimited by size
                  file-name              delimited by size
                  into como-riga
           perform SETTA-RIGA-STAMPA

           accept como-data from century-date
           accept como-ora  from time
           initialize origine
                      destinazione

           string Daemon-Sub-orig  delimited LOW-VALUE,
                  Barra            delimited size,
                  File-Name        delimited size
                  into origine
                  
           inspect File-Name       replacing trailing space by low-value
      *     inspect vtt-path-suff-dest 
      *                             replacing trailing space by low-value
           string Daemon-Sub-PROC  delimited LOW-VALUE,
                  Barra            delimited size,
                  File-Name        delimited low-value
                  "_"              delimited by size        
                  como-data        delimited by size        
                  "_"              delimited by size        
                  como-ora         delimited by size        
                  into Destinazione
           inspect File-Name replacing trailing low-value by space
           call "RENAME" using origine, 
                               Destinazione, 
                               RENAME-STATUS

           initialize como-riga
           inspect File-Name replacing trailing space by low-value
           if RENAME-STATUS = ZERO or 2
              move "FILE ARCHIVIATO" to como-riga
           else
              move "ARCHIVIAZIONE FALLITA" to como-riga
           end-if
           perform SETTA-RIGA-STAMPA.


      ***---
       COPIA-FILE-RETE.
           move "INIZIO COPIA FILE" to como-riga
           perform SETTA-RIGA-STAMPA
           set trovato to false
      *    apro la cartella di origine rete con il filtro sul nome del file
           move vtt-nome-flusso-imp   to pattern

           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         Daemon-Sub-IN,
                                         pattern.
           move RETURN-CODE        to Dir-Handle.

           if dir-handle = 0
              initialize como-riga
              string "NESSUN FILE PRESENTE CON "  delimited by size
                     pattern   delimited by size
                     into como-riga
              perform SETTA-RIGA-STAMPA
           else
              perform until 1 = 2
      *          CONTROLLO L'ESISTENZA DEI FLUSSI
                 inspect Daemon-Sub-IN 
                                   replacing trailing space by low-value
               
                 call "C$LIST-DIRECTORY" using LISTDIR-NEXT,
                                               Dir-Handle,
                                               File-Name
              
                 if File-Name not = "." or ".."
                    if File-Name = SPACES
                       exit perform
                    end-if
                 end-if
                 set trovato to true
              
                 initialize origine
                            Destinazione
                 string Daemon-Sub-IN    delimited LOW-VALUE,
                        Barra            delimited size,
                        File-Name        delimited size,
                        into origine
              
                 string Daemon-Sub-orig  delimited LOW-VALUE,
                        Barra            delimited size,
                        File-Name        delimited size
                        into Destinazione
              
                 call "RENAME" using origine, 
                                     Destinazione, 
                                     RENAME-STATUS
              
                 initialize como-riga
                 inspect File-Name replacing trailing space by low-value
                 if RENAME-STATUS = ZERO or 2
                    string "FILE: "   delimited by size
                           file-name  delimited by low-value
                           " COPIATO NELLA CARTELLA ORIG" delimited size
                           into como-riga
                 else
                    string "FILE: "   delimited by size
                           file-name  delimited by low-value
                      " IMPOSSIBILE COPIARE IL FILE NELLA CARTELLA ORIG"
                                      delimited by size
                           into como-riga
                 end-if
                 perform SETTA-RIGA-STAMPA
              end-perform
           end-if.

           call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle.

           if not trovato
              set errori  to true
              move "NESSUN FILE DA IMPORTARE" to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

           move "FINE COPIA FILE" to como-riga
           perform SETTA-RIGA-STAMPA.

      ***---
       CHECK-CARTELLE.
           initialize Daemon-Sub-PROC
                     Daemon-Sub-ERR  
                     Daemon-Sub-TMP  
                     Daemon-Sub-ORIG 
                     Daemon-Sub-IN   
                     Daemon-Sub-LOG.

           move "CONTROLLO ESITENZA CARTELLE VETTORE" to como-riga
           perform SETTA-RIGA-STAMPA.

           move vtt-path-environment to Daemon-Main-Dir.

           inspect Daemon-Main-Dir replacing trailing SPACE by LOW-VALUE
      *     inspect vtt-path-suff-dest
      *             replacing trailing SPACE by LOW-VALUE.
      *     inspect vtt-path-suff-err
      *             replacing trailing SPACE by LOW-VALUE.

           string  Daemon-Main-Dir                   delimited LOW-VALUE
                   Barra                             delimited size,
                   vtt-path-suff-dest                delimited LOW-VALUE
              into Daemon-Sub-PROC.
                               
           string  Daemon-Main-Dir                   delimited LOW-VALUE
                   Barra                             delimited size,
                   vtt-path-suff-err                 delimited LOW-VALUE
              into Daemon-Sub-ERR.

           string  Daemon-Main-Dir                   delimited LOW-VALUE
                   Barra                             delimited size,
                   vtt-path-suff-tmp                 delimited LOW-VALUE
              into Daemon-Sub-TMP.

           string  Daemon-Main-Dir                   delimited LOW-VALUE
                   Barra                             delimited size,
                   vtt-path-suff-orig                delimited LOW-VALUE
              into Daemon-Sub-ORIG.

      *     string  Daemon-Main-Dir                   delimited LOW-VALUE
      *             Barra                             delimited size,
      *             vtt-path-suff-orig-rete           delimited LOW-VALUE
      *        into Daemon-Sub-IN.
           move vtt-path-orig-rete  to Daemon-Sub-IN

           string  Daemon-Main-Dir                   delimited LOW-VALUE
                   Barra                             delimited size,
                   vtt-path-suff-log                 delimited LOW-VALUE
              into Daemon-Sub-LOG.

           inspect Daemon-Sub-PROC replacing trailing SPACE by LOW-VALUE
           inspect Daemon-Sub-ERR  replacing trailing SPACE by LOW-VALUE
           inspect Daemon-Sub-TMP  replacing trailing SPACE by LOW-VALUE
           inspect Daemon-Sub-ORIG replacing trailing SPACE by LOW-VALUE
           inspect Daemon-Sub-IN   replacing trailing SPACE by LOW-VALUE
           inspect Daemon-Sub-LOG  replacing trailing SPACE by LOW-VALUE

      * CONTROLLO L'ESISTENZA DELLE CARTELLE 
           move "*.*"  to pattern

           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         Daemon-Main-Dir,
                                         pattern.

           move RETURN-CODE        to Dir-Handle.

           if Dir-Handle = ZERO
              set errori             to true
              move Daemon-Main-Dir  to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.
         

      *    CARTELLA MONITORATA DI CONTINUO (ORIG-RETE)
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         Daemon-Sub-IN,
                                         pattern.

           move RETURN-CODE        to Dir-Handle.

           if Dir-Handle =   ZERO
              set errori             to true
              move Daemon-Sub-IN  to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.

      *    SOTTOCARTELLA (PROCESSED)
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         Daemon-Sub-PROC,
                                         pattern.

           move RETURN-CODE          to Dir-Handle.

           if Dir-Handle = ZERO
              set errori             to true
              move Daemon-Sub-PROC   to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.

      *    SOTTOCARTELLA (ERR)
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         Daemon-Sub-ERR,
                                         pattern.

           move return-code          to Dir-Handle.

           if Dir-Handle  = ZERO
              set errori             to true
              move Daemon-Sub-ERR    to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.            

      *    SOTTOCARTELLA (TMP)
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         Daemon-Sub-TMP,
                                         pattern.

           move return-code          to Dir-Handle.

           if Dir-Handle  = ZERO
              set errori             to true
              move Daemon-Sub-TMP    to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.
                             

      *    SOTTOCARTELLA (ORIG) DOVE VENGONO SPOSTATI I FILE TROVATI
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         Daemon-Sub-ORIG,
                                         pattern.

           move return-code          to Dir-Handle.

           if Dir-Handle  = ZERO
              set errori             to true
              move Daemon-Sub-ORIG   to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.

      *    SOTTOCARTELLA DEL FILE DI LOG
           call "C$LIST-DIRECTORY" using LISTDIR-OPEN,
                                         Daemon-Sub-LOG,
                                         pattern.

           move return-code          to Dir-Handle.

           if Dir-Handle  = ZERO
              set errori             to true
              move Daemon-Sub-LOG    to MSG-Folder-Name
              perform MSG-DIR-ERR
           else
              call "C$LIST-DIRECTORY" using LISTDIR-CLOSE, Dir-Handle
           end-if.

           if tutto-ok
              move "CONTROLLO ESITENZA CARTELLE VETTORE: OK" 
                                                        to como-riga
           else
              move "CONTROLLO ESITENZA CARTELLE TERMINATO CON ERRORI" 
                                                        to como-riga
           end-if
           perform SETTA-RIGA-STAMPA.


      ***--
       CLOSE-FILES.
           close tvettori 
                 vettel.

      ***---
       SETTA-RIGA-STAMPA.
           initialize riga-stampa.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-stampa
           end-string.
           display riga-stampa upon syserr.

      ***---
       INVIO-MAIL.
           move "INVIO MAIL IN CORSO..." to como-riga.
           perform SETTA-RIGA-STAMPA.

           initialize LinkBody.


           move "IMPORTAZIONE ESITI CONSEGNA EFFETTUATO - ERRORI" 
                  to LinkSubject
           initialize linkBody

           move 
           "In Allegato il il dettaglio degli errori"
                          to LinkBody     
           accept LinkAddress from environment "IMP_ESITI_ADDRESSES".
           move imp-esiti-p-path-err  to LinkAttach

           initialize LinkAttach
           inspect imp-esiti-p-path-err 
                          replacing trailing space by low-value
      *     inspect Daemon-Sub-orig 
      *                    replacing trailing space by low-value
           string imp-esiti-p-path-err   delimited by low-value
                  ";"                    delimited by size
                  Daemon-Sub-orig        delimited by low-value
                  "\"                    delimited by size
                  file-name              delimited by low-value
                  into LinkAttach

           inspect imp-esiti-p-path-err 
                          replacing trailing low-value by space
      *     inspect Daemon-Sub-orig 
      *                    replacing trailing low-value by space 

           set errori to true.
           move 0 to tentativi. 
           move "imp-esiti" to NomeProgramma.
           perform 5 times
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
              open input lineseq-mail
              read  lineseq-mail next
              if line-riga-mail = "True"
                 set tutto-ok to true
                 close lineseq-mail
                 exit perform
              end-if
              close lineseq-mail

              initialize como-riga
              string r-inizio        delimited size
                     "TENTATIVO N. " delimited size
                     tentativi       delimited size
                     ": "            delimited size
                     line-riga-mail  delimited size
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

           delete file lineseq-mail.

      ***---
       EXIT-PGM.
           move "TERMINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           
           goback.


       MSG-DIR-ERR.
           inspect MSG-Folder-Name, 
                   replacing trailing SPACES by LOW-VALUES.
           initialize como-riga.
           string  "IMPOSSIBILE APRIRE DIRECTORY: "  delimited size,
                   MSG-Folder-Name                  delimited LOW-VALUE,
              into como-riga.


      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "setta-inizio-riga.cpy".


      ***---
       INVIO-MAIL-LOG.

           move "IMPORTAZIONE ESITI DI CONSEGNA" 
                  to LinkSubject

           initialize linkBody

           move "In allegato dettaglio funzionamento programma" 
                  to LinkBody     

           accept LinkAddress from environment "IMP_ESITI_ADDRESSES".
           accept LinkAttach from environment "IMP_ESITI_LOG".

           set errori to true.
           move 0 to tentativi.
           move "imp-esiti" to NomeProgramma.
           perform 10 times
              add 1 to tentativi
              perform SEND-MAIL
              
              call "C$DELETE" using FileDest
              open input lineseq-mail
              read  lineseq-mail next
              if line-riga-mail = "True"
                 set tutto-ok to true
                 close lineseq-mail
                 exit perform
              end-if
              close lineseq-mail

      *        initialize como-riga
      *        string r-inizio              delimited size
      *               "TENTATIVO N. "       delimited size
      *               tentativi             delimited size
      *               ": "                  delimited size
      *               line-riga of lineseq1 delimited size
      *               into como-riga
      *        end-string
      *        perform SETTA-RIGA-STAMPA

           end-perform
               
      *     initialize como-riga.
      *     if tutto-ok
      *        string r-inizio               delimited size
      *               "INVIO MAIL RIUSCITO!" delimited size
      *               into como-riga
      *        end-string
      *     else
      *        string r-inizio                   delimited size
      *               "INVIO MAIL NON RIUSCITO!" delimited size
      *               into como-riga
      *
      *        end-string
      *     end-if.
      *     perform SETTA-RIGA-STAMPA.

           delete file lineseq-mail.

