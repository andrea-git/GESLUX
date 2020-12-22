       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ricalimp-art.
       AUTHOR.                          Andrea.
       REMARKS. Ricalcolo impegnato da:
                - note credito non fatturate
                - ordini inevasi

                - master (maggiore tra ord e eva) 
                  non chiusi (aggiornamento stato, pezzi e prezzi)

           Viene chiamato quando modifico un'evasione che proviene da un
           progressivo diverso sul master.
           VIENE RICHIAMATO SOLAMENTE DA GORDCVAR ED ESEGUITO SE 
           VALORIZZATO UN FLAG IN CBLCONFI COME "S".

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tordini.sl".
           copy "rordini.sl". 
           copy "progmag.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "ttipocli.sl".
           copy "tcaumag.sl".
      *****     copy "tmp-ricalimp.sl".  
           copy "log-progmag.sl".    
           
       SELECT ra-log
           ASSIGN       TO DISK path-ra-log
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS STATUS-ra-log.

       SELECT ra-semaforo |Usato al posto di lockfile
           ASSIGN       TO  "ra-semaforo"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-ra-semaforo
           RECORD KEY   IS ra-chiave
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "tordini.fd".
           copy "rordini.fd". 
           copy "progmag.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "ttipocli.fd".
           copy "tcaumag.fd". 
      *****     copy "tmp-ricalimp.fd".           
       FD ra-semaforo.
       01  ra-rec.
         03 ra-chiave        pic x.
         03 ra-utente-in-uso pic x(20).
         03 ra-data          pic 9(8).
         03 ra-ora           pic 9(8).
       
           copy "log-progmag.fd".

       FD  ra-log.
       01 riga-ra-log PIC  x(200).

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".  
           copy "comune.def".
           copy "fonts.def".
           copy "link-wprogmag.def".

      ***** 01  link-master           pic x.
      *****   88 link-imp-master      value "M". 
      *****   88 link-imp-trad        value "T". 
      *****   88 link-imp-GDO         value "G". 
                                                                    
       77  status-progmag        pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-ttipocli       pic xx.
       77  status-tcaumag        pic xx.     
       77  status-ra-semaforo    pic xx.
       77  status-ra-log         pic xx.
      ***** 77  status-tmp-ricalimp   pic xx.
      ***** 77  path-tmp-ricalimp     pic x(256).
       77  status-log-progmag    pic xx.    
       77  path-log-progmag      pic x(256).
       77  path-ra-log           pic x(256).

       77  como-valore           pic s9(8).
       77  como-impegnato        pic s9(8).
       01  como-impegnati.
           03 como-imp-master    pic s9(8).
           03 como-imp-gdo       pic s9(8).
           03 como-imp-trad      pic s9(8).

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).    
       77  r-output              pic x(25).
       77  como-riga             pic x(50).

       77  peso-ed               PIC zz9,999.
       77  codice-ed             PIC z(5).

       77  como-articolo         pic 9(6).
       77  sw-esegui             pic x.

       01  filler                pic 9 value 0.
         88 no-screen                  value 0.
         88 si-screen                  value 1.

       01  GdoInUsoFlag          pic x.
           88 GdoInUso           value "S". 
           88 GdoNonInUso        value " ".

       77  form1-handle          handle of window.

       01 rl-progmag.
           05 FILLER           PIC  x(13)
                      VALUE IS "PROGRESSIVO: ".
           05 rl-prg-cod-articolo    PIC  9(6).
           05 FILLER           PIC  xx.
           05 rl-prg-cod-magazzino   PIC  x(3).
           05 FILLER           PIC  xx.
           05 rl-prg-tipo-imballo    PIC  x(3).
           05 FILLER           PIC  xx.
           05 rl-prg-peso            PIC  9(5)v999.
           05 FILLER           PIC  xx.
           05 FILLER           PIC  x(11)
                      VALUE IS "IMPEGNATO: ".
           05 rl-prg-impegnato       PIC  s9(8).
           05 FILLER           PIC  xx.
           05 FILLER           PIC  x(10)
                      VALUE IS "I.MASTER: ".
           05 rl-prg-imp-master      PIC  s9(8).
           05 FILLER           PIC  xx.
           05 FILLER           PIC  x(7)
                      VALUE IS "I.GDO: ".
           05 rl-prg-imp-gdo         PIC  s9(8).
           05 FILLER           PIC  xx.
           05 FILLER           PIC  x(8)
                      VALUE IS "I.TRAD: ".
           05 rl-prg-imp-trad        PIC  s9(8).

       LINKAGE SECTION.
       copy "link-ricalimp-art.def".

      ******************************************************************
       PROCEDURE DIVISION USING ra-linkage.

       DECLARATIVES.  
      ***---
       RA-LOG SECTION.
           use after error procedure on ra-log.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ra-log
           when "35" open output ra-log
           end-evaluate.  

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       RA-SEMAFORO-ERR SECTION.
           use after error procedure on ra-semaforo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ra-semaforo
           when "93" 
                set RecLocked to true
                if no-screen and ra-utente-in-uso not = spaces
                   inspect ra-utente-in-uso 
                           replacing trailing spaces by low-value
                   set OtherXX to true
                   initialize AccessType
                   string "IN USO DA: "    delimited size
                          ra-utente-in-uso delimited low-value
                          " - "            delimited size
                          ra-data(7:2)     delimited size
                          "/"              delimited size
                          ra-data(5:2)     delimited size
                          "/"              delimited size
                          ra-data(1:4)     delimited size
                          "|"              delimited size
                          ra-ora(1:2)      delimited size
                          ":"              delimited size
                          ra-ora(3:2)      delimited size
                     into AccessType
                   end-string
                   perform ACCESSOXX
                   set si-screen to true
                end-if
           end-evaluate. 

       END DECLARATIVES.

      ***---
       MAIN-PRG.                 
           accept sw-esegui from environment "ESEGUI_RICALIMP_ART".
           if sw-esegui not = "S"
              goback
           end-if.

           perform until 1 = 2
              open i-o ra-semaforo allowing readers
              if status-ra-semaforo not = "00"
                 if ra-utente-in-uso = spaces                      
                   |per dare tempo eventualmente all'altro processo
                   |di arrivare alla write immediatamente dopo la open
                   |altrimenti può verificarsi che ancora non ha aggiornato
                   |e troverebbe l'utente della sessione precedente
                    call "C$SLEEP" using "1"                     
                    open input ra-semaforo
                    move spaces to ra-chiave
                    read  ra-semaforo
                    close ra-semaforo
                    exit perform cycle
                 end-if
              else
                 exit perform
              end-if
           end-perform.          

           accept  path-ra-log from environment "PROGMAG_LOG_PATH"
           inspect path-ra-log replacing trailing spaces by low-value
           string  path-ra-log   delimited low-value
                   "LOG-RA.log"  delimited size
              into path-ra-log
           end-string
           inspect path-ra-log replacing trailing low-value by spaces.
           open extend ra-log.

                                 
           initialize como-riga.
           string "** INIZIO PROGRAMMA: " delimited size
                  ra-anno                 delimited size
                  " - "                   delimited size
                  ra-numero               delimited size
             into como-riga
           end-string.
           perform SCRIVI-RA-LOG.

           move spaces  to ra-chiave.
           move ra-user to ra-utente-in-uso
           accept ra-data from century-date.
           accept ra-ora  from time.
           write ra-rec invalid rewrite ra-rec end-write.
                                 
           move "   SCRITTO SEMAFORO" to como-riga.
           perform SCRIVI-RA-LOG.
                        
           if si-screen
              perform DESTROYXX
           end-if.   

           set RicalcoloXX to true.
           perform ACCESSOXX. 

           perform INIT.
           perform OPEN-FILES.

           if tutto-ok         
              move "   INIZIO ELABORAZIONE" to como-riga
              perform SCRIVI-RA-LOG

              perform ELABORAZIONE 

              move "   FINE ELABORAZIONE" to como-riga
              perform SCRIVI-RA-LOG

              move "   CHIUSURA FILES" to como-riga
              perform SCRIVI-RA-LOG
              perform CLOSE-FILES  
              move "   CHIUSI FILES" to como-riga
              perform SCRIVI-RA-LOG
           end-if.        
           move "** FINE PROGRAMMA" to como-riga
           perform SCRIVI-RA-LOG.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok     to true.
           set tutto-ok     to true.
           accept como-data from century-date.
           accept como-ora  from time.
           move ra-form-handle to form1-handle.          

      ***---
       OPEN-FILES.
           |Per i test
           if ra-path-log = spaces
              accept  ra-path-log from environment "PROGMAG_LOG_PATH"
              inspect ra-path-log replacing trailing spaces by low-value
              string  ra-path-log delimited low-value
                      "LOG-TEST_" delimited size
                      como-data   delimited size
                      "_"         delimited size
                      como-ora    delimited size
                      ".log"      delimited size
                 into ra-path-log
              end-string
              inspect ra-path-log replacing trailing low-value by spaces
           end-if.
           move ra-path-log to path-log-progmag.
           inspect path-log-progmag replacing all "C.log" by "R.log".
           inspect path-log-progmag replacing all "M.log" by "R.log".
           open output log-progmag.

           |Quando si consolida il magazzino NESSUNO deve essere
           |dentro al file dei movimenti (tranne che in visua) e mi
           |sembra il minimo dato che devo fare operazioni in massa

LUBEXX     |I files devono essere aperti in input. Corriamo il rischio
LUBEXX     |di elaborare intanto che ci lavorano, ma non importa
           open input tordini rordini clienti destini
                      ttipocli tcaumag mtordini mrordini.
           open i-o progmag.
      *****     accept  path-tmp-ricalimp from environment "PATH_ST".
      *****     inspect path-tmp-ricalimp 
      *****             replacing trailing spaces by low-value.
      *****     string  path-tmp-ricalimp  delimited low-value
      *****             "TMP-RICALIMP_"    delimited size
      *****             como-data          delimited size
      *****             "_"                delimited size
      *****             como-ora           delimited size
      *****             ".tmp"             delimited size
      *****        into path-tmp-ricalimp
      *****     end-string.
      *****     open output tmp-ricalimp.
      *****     close       tmp-ricalimp.
      *****     open i-o    tmp-ricalimp.

      ***---
       ELABORAZIONE.                   
           move "** SITUAZIONE PRIMA **" 
             to riga-log-progmag.
           perform WRITE-PROGMAG-LOG.

           perform AZZERA-PROGMAG.     
           move "** SITUAZIONE POST AZZERAMENTO **" 
             to riga-log-progmag.
           perform WRITE-PROGMAG-LOG.

           perform ELABORA-ORDINI-MASTER.
           move "** SITUAZIONE POST ORDINI MASTER **" 
             to riga-log-progmag.
           perform WRITE-PROGMAG-LOG.

           perform ELABORA-INEVASI-E-BOLLA-NON-EMESSA.
      *****     perform TMP-TO-PROGMAG.             

           move "** SITUAZIONE DOPO (INEVASI E BOLLA NON EMESSA)**" 
             to riga-log-progmag.
           perform WRITE-PROGMAG-LOG.

      ***---
       WRITE-PROGMAG-LOG.
           write riga-log-progmag. 
           perform varying idx from 1 by 1 
                     until idx > 999
              if ra-articolo(idx) = 0
                 exit perform
              end-if
              move ra-articolo(idx) to prg-cod-articolo
              move spaces           to prg-cod-magazzino
              move spaces           to prg-tipo-imballo
              move 0                to prg-peso
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read progmag next no lock 
                            at end exit perform 
                       end-read
                       if prg-cod-articolo not = 
                          ra-articolo(idx)
                          write riga-log-progmag from spaces
                          exit perform
                       end-if
                       move prg-cod-articolo  to rl-prg-cod-articolo
                       move prg-cod-magazzino to rl-prg-cod-magazzino
                       move prg-tipo-imballo  to rl-prg-tipo-imballo
                       move prg-peso          to rl-prg-peso     
                       move prg-impegnato     to rl-prg-impegnato
                       move prg-imp-master    to rl-prg-imp-master
                       move prg-imp-GDO       to rl-prg-imp-GDO
                       move prg-imp-TRAD      to rl-prg-imp-TRAD
                       write riga-log-progmag from rl-progmag
                    end-perform
              end-start
           end-perform.     

      ***---
       AZZERA-PROGMAG.
           set tutto-ok to true.
      
           perform varying ra-idx from 1 by 1 until 1 = 2
              if ra-articolo(ra-idx) = 0
                 exit perform
              end-if
              move low-value to prg-rec
              move ra-articolo(ra-idx) to prg-cod-articolo
              start progmag key is >= prg-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2 
             
                       read progmag next no lock
                            at end exit perform 
                       end-read
      
                       if prg-cod-articolo not = ra-articolo(ra-idx)
                          exit perform
                       end-if

                       perform READ-PROGMAG-LOCK
      
                       move 0 to prg-impegnato
                       move 0 to prg-imp-master
                       move 0 to prg-imp-TRAD
                       move 0 to prg-imp-GDO
                       rewrite prg-rec invalid continue end-rewrite
                    end-perform                                    
                    unlock progmag all records
              end-start
           end-perform.

      ********---
      ***** TMP-TO-PROGMAG.
      *****     move "** SITUAZIONE PRE **" to riga-log-progmag.
      *****     write riga-log-progmag.  
      *****     perform varying idx from 1 by 1 
      *****               until idx > 999
      *****        if ra-articolo(idx) = 0
      *****           exit perform
      *****        end-if
      *****        move ra-articolo(idx) to tric-prg-cod-articolo
      *****        move spaces           to tric-prg-cod-magazzino
      *****        move spaces           to tric-prg-tipo-imballo
      *****        move 0                to tric-prg-peso
      *****        start tmp-ricalimp key >= tric-prg-chiave
      *****              invalid continue
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read tmp-ricalimp next 
      *****                   at end exit perform 
      *****                 end-read
      *****                 if tric-prg-cod-articolo not = 
      *****                    ra-articolo(idx)
      *****                    write riga-log-progmag from spaces
      *****                    exit perform
      *****                 end-if
      *****                 move tric-prg-cod-articolo  
      *****                   to rl-prg-cod-articolo
      *****                 move tric-prg-cod-magazzino 
      *****                   to rl-prg-cod-magazzino
      *****                 move tric-prg-tipo-imballo  
      *****                   to rl-prg-tipo-imballo
      *****                 move tric-prg-peso          
      *****                   to rl-prg-peso     
      *****                 move tric-prg-impegnato     
      *****                   to rl-prg-impegnato
      *****                 move tric-prg-imp-master    
      *****                   to rl-prg-imp-master
      *****                 move tric-prg-imp-GDO       
      *****                   to rl-prg-imp-GDO
      *****                 move tric-prg-imp-TRAD      
      *****                   to rl-prg-imp-TRAD
      *****                 write riga-log-progmag from rl-progmag
      *****              end-perform
      *****        end-start
      *****     end-perform.  
      *****
      *****     move low-value to tric-rec.
      *****     start tmp-ricalimp key >= tric-prg-chiave
      *****           invalid continue
      *****       not invalid
      *****           perform until 1 = 2
      *****              read tmp-ricalimp next at end exit perform end-read
      *****              move tric-prg-chiave to prg-chiave
      *****              perform READ-PROGMAG-LOCK
      *****              if not RecLocked
      *****                 move tric-prg-impegnato to prg-impegnato
      *****                 move tric-prg-impegnati to prg-impegnati
      *****                 rewrite prg-rec
      *****                 unlock progmag all records
      *****                 perform ALLINEA-PROGMAG-PADRE
      *****              end-if
      *****           end-perform
      *****     end-start.
      *****
      ********---
      ***** ALLINEA-PROGMAG-PADRE.
      *****     move spaces to prg-cod-magazzino
      *****     move spaces to prg-tipo-imballo
      *****     move 0      to prg-peso
      *****     start progmag key >= prg-chiave
      *****           invalid continue
      *****       not invalid
      *****           move 0 to como-impegnato como-imp-master como-imp-gdo 
      *****                     como-imp-trad
      *****           perform until 1 = 2
      *****              read progmag next at end exit perform end-read
      *****              if prg-peso = 0 exit perform cycle end-if
      *****              if prg-cod-articolo not = tric-prg-cod-articolo
      *****                 exit perform
      *****              end-if                             
      *****              add prg-impegnato  to como-impegnato
      *****              add prg-imp-master to como-imp-master
      *****              add prg-imp-gdo    to como-imp-gdo
      *****              add prg-imp-trad   to como-imp-trad
      *****           end-perform
      *****           move tric-prg-cod-articolo to prg-cod-articolo
      *****           move spaces to prg-cod-magazzino
      *****           move spaces to prg-tipo-imballo
      *****           move 0      to prg-peso
      *****           perform READ-PROGMAG-LOCK
      *****           if not RecLocked
      *****              move como-impegnato  to prg-impegnato
      *****              move como-imp-master to prg-imp-master
      *****              move como-imp-gdo    to prg-imp-gdo
      *****              move como-imp-trad   to prg-imp-trad
      *****              rewrite prg-rec
      *****              unlock progmag all records
      *****           end-if
      *****     end-start.         

      ***---
       READ-PROGMAG-LOCK.           
           perform until 1 = 2
              set RecLocked to false
              read progmag lock
              if RecLocked
                 move prg-cod-articolo to codice-ed
                 move prg-peso         to peso-ed
                 move "progmag"        to geslock-nome-file
                 initialize geslock-messaggio
                 string "Articolo:       ", codice-ed
                 x"0d0a""Magazzino:  ",     prg-cod-magazzino
                 x"0d0a""Imballo:       ",  prg-tipo-imballo
                 x"0d0a""Peso:           ", peso-ed 
                 x"0d0a""Record in uso su "    delimited size
                        "altro terminale (R)." delimited size
                        into geslock-messaggio
                 end-string
                 set errori to true
                 move 1     to geslock-v-riprova
                 move 0     to geslock-v-termina
                 move 0     to geslock-v-ignora
                 call   "geslock" using geslock-linkage
                 cancel "geslock"
              else
                 exit perform
LUBEXX        end-if
           end-perform.

      ***---
       ELABORA-ORDINI-MASTER.
           set tutto-ok to true.
           move low-value to mto-rec.
           set mto-registrato to true.
           start mtordini key >= k-mto-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    if mto-chiuso exit perform end-if
                    perform LOOP-RIGHE-MRORDINI
                 end-perform
           end-start.

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
                    if mro-chiuso
                       continue
                    else
                       move mro-prg-cod-articolo to como-articolo
      *****                 move mro-prg-chiave       to tric-prg-chiave
                       perform FIND-ARTICOLO        
                       if trovato
                          perform AGGIORNA-IMPEGNATO-MASTER
                       end-if
                    end-if
                 end-perform
           end-start.
                 
      ***---     
       ELABORA-INEVASI-E-BOLLA-NON-EMESSA.
           move 0 to tor-anno-fattura.
           move 0 to tor-num-fattura.

           start tordini key is >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-data-fattura not = 0 or
                       tor-num-fattura  not = 0
                       exit perform
                    end-if

                    if tor-num-bolla  = 0 and
                       tor-data-bolla = 0
                       perform LOOP-RIGHE-RORDINI
                    end-if
                    
                    if errori exit perform end-if
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-RORDINI.
      *****     move tor-anno   to ror-anno.
      *****     move tor-numero to ror-num-ordine.
      *****     move low-values to ror-num-riga.
      *****     start rordini key is >= ror-chiave
      *****           invalid continue
      *****       not invalid
      *****           perform until 1 = 2
      *****              read rordini  next at end exit perform end-read
      *****              if tor-anno   not = ror-anno    or
      *****                 tor-numero not = ror-num-ordine
      *****                 exit perform
      *****              end-if
      *****              move ror-prg-cod-articolo  to como-articolo
      *****              move ror-prg-chiave        to tric-prg-chiave
      *****              perform FIND-ARTICOLO                
      *****              if trovato
      *****                 move 0           to como-impegnato
      *****                 move ror-qta     to como-valore
      *****                 move tor-causale to tca-codice
      *****                 perform AGGIORNA-RICALIMP-CAUSALE
      *****              end-if
      *****           end-perform
      *****     end-start.     
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
                    move ror-prg-cod-articolo  to como-articolo
                    perform FIND-ARTICOLO
                    if trovato
                       move 0                     to link-impegnato
                       move ra-user               to link-user
                       move ror-prg-chiave        to link-key
                       move "0100000000000000"    to link-array
                       move ror-qta               to link-valore
                       move tor-causale           to link-causale
                       move ror-prg-chiave        to link-key
                       set  link-update           to true
                       set  link-open-with-lock   to false
                       set  link-update-um        to true
                       set  link-update-peso      to false
                       set  link-update-valore    to false
                       call   "wprogmag" using link-wprogmag
                       cancel "wprogmag"
                    end-if
                 end-perform
           end-start.

      ********---
      ***** AGGIORNA-RICALIMP-CAUSALE.
      *****     initialize prg-rec.   
      *****     read tcaumag no lock 
      *****          invalid continue
      *****      not invalid 
      *****          perform AGGIORNA-VALORI-UM 
      *****          rewrite tric-rec
      *****     end-read.

      ********---
      ***** AGGIORNA-VALORI-UM.
      *****     evaluate true
      *****     when tca-movim-imp-pos 
      *****          add como-valore to tric-prg-impegnato
      *****          if como-impegnato not = 0
      *****             evaluate true
      *****             when link-imp-MASTER
      *****                  add como-impegnato to tric-prg-imp-master
      *****             when link-imp-GDO
      *****                  add como-impegnato to tric-prg-imp-GDO
      *****             when link-imp-TRAD
      *****                  add como-impegnato to tric-prg-imp-TRAD
      *****             end-evaluate
      *****          end-if
      *****     when tca-movim-imp-neg 
      *****          subtract como-valore from tric-prg-impegnato
      *****          if como-impegnato not = 0
      *****             evaluate true
      *****             when link-imp-MASTER
      *****                  subtract como-impegnato from tric-prg-imp-master
      *****             when link-imp-GDO
      *****                  subtract como-impegnato from tric-prg-imp-GDO
      *****             when link-imp-TRAD
      *****                  subtract como-impegnato from tric-prg-imp-TRAD
      *****             end-evaluate
      *****          end-if
      *****     end-evaluate.        

      ***----
       AGGIORNA-IMPEGNATO-MASTER.
      *****     set  cli-tipo-C  to true.
      *****     move mto-cod-cli to cli-codice.
      *****     read clienti     no lock.
      *****
      *****     |Ad aumentare l'impegnato sulle qta evase 
      *****     |ci penseranno poi le evasioni
      *****     if mro-qta > mro-qta-e
      *****        compute como-valore = mro-qta - mro-qta-e
      *****     else
      *****        move 0       to como-valore
      *****     end-if.
      *****     move como-valore to como-impegnato.
      *****     perform DIREZIONA-IMPEGNATO.
      *****     move mto-causale to tca-codice.
      *****     perform AGGIORNA-RICALIMP-CAUSALE.
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

           move ra-user               to link-user.
           move "0100000000000000"    to link-array.
           move mto-causale           to link-causale.
           move mro-prg-chiave        to link-key.
           set  link-update           to true.
           set  link-open-with-lock   to false.
           set  link-update-um        to true.
           set  link-update-peso      to false.
           set  link-update-valore    to false.
           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".

      ***---
       FIND-ARTICOLO.
      *****     set trovato to false.
      *****     set ra-idx to 1.
      *****     search ra-articoli
      *****     when ra-articolo(ra-idx) = como-articolo
      *****          move tric-prg-chiave to prg-chiave
      *****          read progmag no lock
      *****               invalid continue
      *****           not invalid
      *****               set trovato to true
      *****               read tmp-ricalimp key tric-prg-chiave
      *****                    invalid 
      *****                    initialize tric-dati 
      *****                               replacing numeric data by zeroes
      *****                                    alphanumeric data by spaces
      *****                    write tric-rec
      *****               end-read
      *****          end-read
      *****     end-search. 
           set trovato to false.
           set ra-idx to 1.
           search ra-articoli
           when ra-articolo(ra-idx) = como-articolo
                set trovato to true
           end-search.

      ***--
       CLOSE-FILES.
           close mtordini mrordini clienti destini tcaumag ra-semaforo.
           delete file ra-semaforo.
      *****     close       tmp-ricalimp.
      *****     delete file tmp-ricalimp.
           close log-progmag.

      ***---
       EXIT-PGM.
           close ra-log.        
           perform DESTROYXX.
           goback.           

      ***---
       SCRIVI-RA-LOG.
           call   "set-ini-log" using r-output.
           cancel "set-ini-log".
           initialize riga-ra-log.
           string r-output      delimited size
                  " - UTENTE: " delimited size
                  ra-user       delimited size
                  como-riga     delimited size
             into riga-ra-log
           end-string.
           write riga-ra-log.

      ***---
       PARAGRAFO-COPY.
           copy "direziona-impegnato-common.cpy".
           copy "accessoxx.cpy".
