       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      aggstatraff.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".
           copy "statraff.sl".
           copy "articoli.sl".
           copy "tmp-marca.sl".
           copy "tmarche.sl".
           copy "timposte.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd".
           copy "statraff.fd".
           copy "articoli.fd".
           copy "tmp-marca.fd".
           copy "tmarche.fd".
           copy "timposte.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "costo-medio.def".
       copy "imposte.def".

      * COSTANTI
       78  titolo            value "Statistiche con raffronti per mese".

      * FILE-STATUS
       77  status-progmag        pic xx.
       77  status-statraff       pic xx.
       77  status-articoli       pic xx.
       77  status-tmp-marca      pic xx.
       77  status-tmarche        pic xx.
       77  status-timposte       pic xx.

      * VARIABILI
       77  mese                  pic 99.
       77  anno                  pic 9(4).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  path-tmp              pic x(256).
       77  finali                pic s9(12)v99.
       77  margine               pic s9(12)v99.
       77  vag-mese-scorso       pic s9(12)v99.
       77  qta-mese-scorso       pic s9(9).
       77   kg-mese-scorso       pic s9(15)v999.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  como-peso             pic s9(12)v999.
       77  como-udm              pic s9(12).
       77  como-vendite          pic s9(12)v999.
       77  como-acquisti         pic s9(12)v999.

      * FLAGS
       01  controlli             pic xx.
         88 errori               value "ER".
         88 tutto-ok             value "OK".

       01  FlagTrovato           pic 9.
         88 trovato              value 1, false 0.

       LINKAGE SECTION.
       77  link-user             pic x(10).
       77  link-data             pic 9(8).
       77  link-result           pic 9.
       77  scr-oper-handle       handle of window.

       PROCEDURE DIVISION USING link-user, 
                                link-data, 
                                link-result,
                                scr-oper-handle.

       DECLARATIVES.
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                set errori to true
                display message "File [PROGMAG] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [PROGMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "progmag"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input progmag
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                set errori to true
                display message "File [ARTICOLI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "articoli"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input articoli
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                set errori to true
                display message "File [TMARCHE] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TMARCHE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMARCHE] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "TMARCHE"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input tmarche
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           set tutto-ok  to true.
           evaluate status-timposte
           when "35"
                set errori to true
                display message "File [TIMPOSTE] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TIMPOSTE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMPOSTE] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "TIMPOSTE"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input timposte
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       STATRAFF-ERR SECTION.
           use after error procedure on statraff.
           set tutto-ok  to true.
           evaluate status-statraff
           when "35"
                set errori to true
                display message "File [STATRAFF] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [STATRAFF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STATRAFF] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "statraff"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o statraff allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       TMP-MARCA-ERR SECTION.
           use after error procedure on tmp-marca.
           set tutto-ok  to true.
           evaluate status-tmp-marca
           when "35"
                set errori to true
                display message "File [TMP-MARCA] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TMP-MARCA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-MARCA] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output tmp-marca
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
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
           move 0 to counter counter2.
           move link-data(1:4) to anno.
           move link-data(5:2) to mese.
           initialize path-tmp.
           set trovato         to false.
           set tutto-ok        to true.
           set TrattamentoGDO  to true.
           accept path-tmp   from environment "PATH-ST".
           accept como-data  from century-date.
           accept como-ora   from time.
           inspect path-tmp  replacing trailing spaces by low-value.
           string  path-tmp  delimited by low-value
                   "aggstatraff"  delimited size
                   "_"            delimited size
                   como-data      delimited size
                   "_"            delimited size
                   como-ora       delimited size
                   ".tmp"         delimited size
                   into path-tmp
           end-string.

      ***---
       OPEN-FILES.
           open i-o statraff allowing readers.
           if tutto-ok
              open output tmp-marca
              if tutto-ok
                 close    tmp-marca
                 open i-o tmp-marca
                 open input progmag articoli tmarche timposte
              else        
                 close statraff
              end-if
           end-if.

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move low-value to prg-rec.
           start progmag key is >= prg-chiave 
                 invalid set errori to true
           end-start.
           if tutto-ok

              |RIPULISCO LA SCREEN DAL CONTATORE
              display "                          "
                 upon scr-oper-handle at column 34
                                           line 25
              display "                          "
                 upon scr-oper-handle at column 30
                                           line 26
              ||||||||

              perform until 1 = 2
                 read progmag next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon scr-oper-handle at column 34
                                                 line 25
                    move 0 to counter2
                    if counter = 100
                       display "STATRAFF COUNTER"
                          upon scr-oper-handle at column 30
                                                    line 26
                    end-if
                 end-if

                 |Lo calcolo comunque sul padre come da
                 |richiesta di Trivella in data 04/01/06
                 if prg-cod-magazzino = spaces and
                    prg-tipo-imballo  = spaces and
                    prg-peso          = 0
                    set TrattamentoGDO to true
                    perform CALCOLA-COSTO-MP-COMPLETO
                    ||| IN ATTESA!!!
      *****              if costo-mp = 0
      *****                 perform CALCOLA-COSTO-MP-WHEN-ZERO
      *****              end-if
                 end-if

                 if prg-cod-magazzino not = spaces and
                    prg-tipo-imballo  not = spaces and
                    prg-peso          not = 0
                    perform INIT-REC-ARTICOLI
                    move prg-cod-articolo to art-codice
                    read articoli no lock invalid continue end-read
                    perform CALCOLA-VALORI
                    perform SOMMA-VALORI
                 end-if
              end-perform

              if trovato
                 move 0 to counter counter2
                 move 1 to link-result
                 close      tmp-marca
                 open input tmp-marca
                 move low-value to tma-rec
                 start tmp-marca key is >= tma-chiave 
                       invalid continue
                 end-start
                 perform until 1 = 2
                    read tmp-marca next at end exit perform end-read
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon scr-oper-handle at column 34
                                                    line 25
                       move 0 to counter2
                       if counter = 100
                          display "STATSETT  ON TMP"
                             upon scr-oper-handle at column 30
                                                       line 26
                       end-if
                    end-if
                    perform AGGIORNA-STATISTICHE
                 end-perform
              else
                 move 0 to link-result
              end-if
           end-if.

      ***---
       CALCOLA-VALORI.
           compute como-vendite =
                   prg-ven-valore - prg-resi-da-cli-valore.

           compute como-acquisti =
                   prg-acq-valore - prg-resi-fornitori-valore +
                   prg-valore-el  - prg-valore-ul             +
                   prg-var-inv-valore.

           add 0,005 to costo-mp giving costo-mp-2dec.

           compute finali  = prg-giacenza-udm * costo-mp-2dec.

           compute margine = como-vendite   +
                             finali         -
                             prg-ini-valore -
                             como-acquisti.

      ***---
       SOMMA-VALORI.
           initialize tma-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move art-marca-prodotto to tma-marca.
           read tmp-marca invalid continue end-read.

           compute como-peso = prg-ven-kg  - prg-resi-da-cli-kg.
           compute como-udm  = prg-ven-udm - prg-resi-da-cli-udm.
           add  como-peso to tma-kg.
           add  como-udm  to tma-qta.
           add  margine   to tma-vag.
           write tma-rec  invalid rewrite tma-rec end-write.
           set trovato to true.

      ***---
       INIT-REC-ARTICOLI.
           initialize art-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

      ***---
       AGGIORNA-STATISTICHE.
           if mese = 1
              move 0 to qta-mese-scorso
              move 0 to vag-mese-scorso
              move 0 to  kg-mese-scorso
           else
              subtract 1 from mese giving str-mese
      *****        move anno      to str-anno
              move tma-marca to str-marca
              read statraff
                   invalid
                   move 0 to qta-mese-scorso
                             vag-mese-scorso
                              kg-mese-scorso
               not invalid
                   move str-qta-prog to qta-mese-scorso
                   move str-vag-prog to vag-mese-scorso
                   move str-kg-prog  to  kg-mese-scorso
              end-read
           end-if.
           
           initialize str-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

      ****     move anno      to str-anno.
           move mese      to str-mese.
           move tma-marca to str-marca.

           read statraff 
                invalid set trovato to false
            not invalid set trovato to true
           end-read.

           compute str-kg-corr  = ( tma-kg  -  kg-mese-scorso ).
           compute str-qta-corr = ( tma-qta - qta-mese-scorso ).
           compute str-vag-corr = ( tma-vag - vag-mese-scorso ).
           move tma-qta to str-qta-prog.
           move tma-vag to str-vag-prog.
           move tma-kg  to str-kg-prog.

           if not trovato
              move link-user to str-utente-creazione
              accept str-data-creazione from century-date
              accept str-ora-creazione  from time
              write str-rec invalid continue end-write
           else
              move link-user to str-utente-ultima-modifica
              accept str-data-ultima-modifica from century-date
              accept str-ora-ultima-modifica  from time
              rewrite str-rec invalid continue end-rewrite
           end-if.

      ***---
       CLOSE-FILES.
           close progmag statraff articoli tmp-marca tmarche timposte.
           delete file tmp-marca.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "costo-medio.cpy".
       copy "recupero-anagrafica.cpy".
