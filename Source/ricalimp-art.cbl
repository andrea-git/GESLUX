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
           copy "tmp-ricalimp.sl".  
           copy "lockfile.sl".

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
           copy "tmp-ricalimp.fd".           
           copy "lockfile.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".         

       01  link-master           pic x.
         88 link-imp-master      value "M". 
         88 link-imp-trad        value "T". 
         88 link-imp-GDO         value "G". 
                                                                    
       77  status-progmag        pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-ttipocli       pic xx.
       77  status-tcaumag        pic xx.   
       77  status-lockfile       pic xx.
       77  status-tmp-ricalimp   pic xx.
       77  path-tmp-ricalimp     pic x(256).

       77  como-valore           pic s9(8).
       77  como-impegnato        pic s9(8).
       01  como-impegnati.
           03 como-imp-master    pic s9(8).
           03 como-imp-gdo       pic s9(8).
           03 como-imp-trad      pic s9(8).

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).    

       77  peso-ed               PIC zz9,999.
       77  codice-ed             PIC z(5).

       77  como-articolo         pic 9(6).
       77  sw-esegui             pic x.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
                                                  
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       01  GdoInUsoFlag          pic x.
           88 GdoInUso           value "S". 
           88 GdoNonInUso        value " ".

       LINKAGE SECTION.
       copy "link-ricalimp-art.def".

      ******************************************************************
       PROCEDURE DIVISION USING ra-linkage.

       DECLARATIVES.
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
       LOCKFILE-ERR SECTION.
           use after error procedure on lockfile.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-lockfile
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           accept sw-esegui from environment "ESEGUI_RICALIMP_ART".
           if sw-esegui not = "S"
              goback
           end-if.

           perform INIT.
           perform OPEN-FILES.
           move "ricalimp-art" to lck-chiave.
           read lockfile no lock
                invalid         
                move "C" to lck-operazione
                accept lck-data-creazione from century-date
                accept lck-ora-creazione  from century-date
                move ra-user to lck-utente-creazione
                write lck-rec
                read lockfile lock end-read
            not invalid
                perform until 1 = 2
                   set RecLocked to false
                   read lockfile lock end-read
                   if not RecLocked
                      exit perform
                   end-if
                end-perform
           end-read.
                
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok     to true.
           set tutto-ok     to true.
           accept como-data from century-date.
           accept como-ora  from time.

      ***---
       OPEN-FILES.
           |Quando si consolida il magazzino NESSUNO deve essere
           |dentro al file dei movimenti (tranne che in visua) e mi
           |sembra il minimo dato che devo fare operazioni in massa

LUBEXX     |I files devono essere aperti in input. Corriamo il rischio
LUBEXX     |di elaborare intanto che ci lavorano, ma non importa
           open input tordini rordini clienti destini
                      ttipocli tcaumag mtordini mrordini.
           open i-o progmag.
           accept  path-tmp-ricalimp from environment "PATH_ST".
           inspect path-tmp-ricalimp 
                   replacing trailing spaces by low-value.
           string  path-tmp-ricalimp  delimited low-value
                   "TMP-RICALIMP_"    delimited size
                   como-data          delimited size
                   "_"                delimited size
                   como-ora           delimited size
                   ".tmp"             delimited size
              into path-tmp-ricalimp
           end-string.
           open output tmp-ricalimp.
           close       tmp-ricalimp.
           open i-o    tmp-ricalimp.
           open i-o    lockfile.

      ***---
       ELABORAZIONE.
      *****     perform AZZERA-PROGMAG.
           perform ELABORA-ORDINI-MASTER.
           perform ELABORA-INEVASI-E-BOLLA-NON-EMESSA.
           perform TMP-TO-PROGMAG.

      ********---
      ***** AZZERA-PROGMAG.
      *****     set tutto-ok to true.
      *****
      *****     perform varying ra-idx from 1 by 1 until 1 = 2
      *****        if ra-articolo(ra-idx) = 0
      *****           exit perform
      *****        end-if
      *****        move low-value to prg-rec
      *****        move ra-articolo(ra-idx) to prg-cod-articolo
      *****        start progmag key is >= prg-chiave
      *****              invalid continue
      *****          not invalid
      *****              perform until 1 = 2 
      *****       
      *****                 read progmag next at end exit perform end-read
      *****
      *****                 if prg-cod-articolo not = ra-articolo(ra-idx)
      *****                    exit perform
      *****                 end-if
      *****
      *****                 move 0 to prg-impegnato
      *****                 move 0 to prg-imp-master
      *****                 move 0 to prg-imp-TRAD
      *****                 move 0 to prg-imp-GDO
      *****                 rewrite prg-rec invalid continue end-rewrite
      *****              end-perform                                    
      *****              unlock progmag all records
      *****        end-start
      *****     end-perform.

      ***---
       TMP-TO-PROGMAG.
           move low-value to tric-rec.
           start tmp-ricalimp key >= tric-prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-ricalimp next at end exit perform end-read
                    move tric-prg-chiave to prg-chiave
                    perform READ-PROGMAG-LOCK
                    if not RecLocked
                       move tric-prg-impegnato to prg-impegnato
                       move tric-prg-impegnati to prg-impegnati
                       rewrite prg-rec
                       unlock progmag all records
                       perform ALLINEA-PROGMAG-PADRE
                    end-if
                 end-perform
           end-start.

      ***---
       ALLINEA-PROGMAG-PADRE.
           move spaces to prg-cod-magazzino
           move spaces to prg-tipo-imballo
           move 0      to prg-peso
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 move 0 to como-impegnato como-imp-master como-imp-gdo 
                           como-imp-trad
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-peso = 0 exit perform cycle end-if
                    if prg-cod-articolo not = tric-prg-cod-articolo
                       exit perform
                    end-if                             
                    add prg-impegnato  to como-impegnato
                    add prg-imp-master to como-imp-master
                    add prg-imp-gdo    to como-imp-gdo
                    add prg-imp-trad   to como-imp-trad
                 end-perform
                 move tric-prg-cod-articolo to prg-cod-articolo
                 move spaces to prg-cod-magazzino
                 move spaces to prg-tipo-imballo
                 move 0      to prg-peso
                 perform READ-PROGMAG-LOCK
                 if not RecLocked
                    move como-impegnato  to prg-impegnato
                    move como-imp-master to prg-imp-master
                    move como-imp-gdo    to prg-imp-gdo
                    move como-imp-trad   to prg-imp-trad
                    rewrite prg-rec
                    unlock progmag all records
                 end-if
           end-start.         

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
                 x"0d0a""Record già in uso su altro terminale." 
                             delimited size
                        into geslock-messaggio
                 end-string
                 set errori to true
                 move 1     to geslock-v-riprova
                 move 0     to geslock-v-termina
                 move 1     to geslock-v-ignora
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
                       move mro-prg-chiave       to tric-prg-chiave
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
                    move ror-prg-chiave        to tric-prg-chiave
                    perform FIND-ARTICOLO                
                    if trovato
                       move 0           to como-impegnato
                       move ror-qta     to como-valore
                       move tor-causale to tca-codice
                       perform AGGIORNA-RICALIMP-CAUSALE
                    end-if
                 end-perform
           end-start.

      ***---
       AGGIORNA-RICALIMP-CAUSALE.
           initialize prg-rec.   
           read tcaumag no lock 
                invalid continue
            not invalid 
                perform AGGIORNA-VALORI-UM 
                rewrite tric-rec
           end-read.

      ***---
       AGGIORNA-VALORI-UM.
           evaluate true
           when tca-movim-imp-pos 
                add como-valore to tric-prg-impegnato
                if como-impegnato not = 0
                   evaluate true
                   when link-imp-MASTER
                        add como-impegnato to tric-prg-imp-master
                   when link-imp-GDO
                        add como-impegnato to tric-prg-imp-GDO
                   when link-imp-TRAD
                        add como-impegnato to tric-prg-imp-TRAD
                   end-evaluate
                end-if
           when tca-movim-imp-neg 
                subtract como-valore from tric-prg-impegnato
                if como-impegnato not = 0
                   evaluate true
                   when link-imp-MASTER
                        subtract como-impegnato from tric-prg-imp-master
                   when link-imp-GDO
                        subtract como-impegnato from tric-prg-imp-GDO
                   when link-imp-TRAD
                        subtract como-impegnato from tric-prg-imp-TRAD
                   end-evaluate
                end-if
           end-evaluate.        

      ***----
       AGGIORNA-IMPEGNATO-MASTER.
           set  cli-tipo-C  to true.
           move mto-cod-cli to cli-codice.
           read clienti     no lock.

           |Ad aumentare l'impegnato sulle qta evase 
           |ci penseranno poi le evasioni
           if mro-qta > mro-qta-e
              compute como-valore = mro-qta - mro-qta-e
           else
              move 0       to como-valore
           end-if.
           move como-valore to como-impegnato.
           perform DIREZIONA-IMPEGNATO.
           move mto-causale to tca-codice.
           perform AGGIORNA-RICALIMP-CAUSALE.

      ***---
       FIND-ARTICOLO.
           set trovato to false.
           set ra-idx to 1.
           search ra-articoli
           when ra-articolo(ra-idx) = como-articolo
                move tric-prg-chiave to prg-chiave
                read progmag no lock
                     invalid continue
                 not invalid
                     set trovato to true
                     read tmp-ricalimp key tric-prg-chiave
                          invalid 
                          initialize tric-dati 
                                     replacing numeric data by zeroes
                                          alphanumeric data by spaces
                          write tric-rec
                     end-read
                end-read
           end-search.

      ***--
       CLOSE-FILES.
           unlock lockfile all records.
           close mtordini mrordini clienti destini tcaumag lockfile.
           close       tmp-ricalimp.
           delete file tmp-ricalimp.

      ***---
       EXIT-PGM.
           goback.           

      ***---
       PARAGRAFO-COPY.
           copy "direziona-impegnato-common.cpy".
