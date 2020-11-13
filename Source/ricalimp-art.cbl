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
           Differisce da "ricalimp-bat" solo perchè lavora solo su 
           articoli di un'evasione che mi vengono passati in linkage.
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
           copy "articoli.sl".
           copy "tcaumag.sl".

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
           copy "articoli.fd".
           copy "tcaumag.fd".

       WORKING-STORAGE SECTION.
           copy "link-wprogmag.def".
                                                                    
       77  status-progmag        pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-ttipocli       pic xx.
       77  status-tscorte        pic xx.
       77  status-articoli       pic xx.
       77  status-tcaumag        pic xx.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).    

       77  como-articolo         pic 9(6).
       77  sw-esegui             pic x.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
                                                  
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.
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

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           accept sw-esegui from environment "ESEGUI_RICALIMP_ART".
           if sw-esegui not = "S"
              goback
           end-if.

           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok    to true.
           set prima-volta to true.
           set tutto-ok  to true.

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

      ***---
       ELABORAZIONE.
           perform AZZERA-PROGMAG.
           perform ELABORA-ORDINI-MASTER.
           perform ELABORA-INEVASI-E-BOLLA-NON-EMESSA.

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
             
                       read progmag next at end exit perform end-read

                       if prg-cod-articolo not = ra-articolo(ra-idx)
                          exit perform
                       end-if

                       move 0 to prg-impegnato
                       move 0 to prg-imp-master
                       move 0 to prg-imp-TRAD
                       move 0 to prg-imp-GDO
                       rewrite prg-rec invalid continue end-rewrite
                    end-perform                                    
                    unlock progmag all records
              end-start
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
                    if mto-chiuso
                       exit perform
                    end-if
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
           set trovato to false.
           set ra-idx to 1.
           search ra-articoli
           when ra-articolo(ra-idx) = como-articolo
                set trovato to true
           end-search.

      ***--
       CLOSE-FILES.
           close mtordini mrordini clienti destini tcaumag.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "direziona-impegnato-common.cpy".
