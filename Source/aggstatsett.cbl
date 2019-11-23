       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      aggstatsett.
       AUTHOR.                          Andrea.
       REMARKS. Aggiorna il file delle statistiche partendo
                dai movimenti di magazzino.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
LUBEXX*****Inserito solo per il controllo dei movimenti
LUBEXX*****presenti con la causale "speciale"
LUBEXX     copy "lineseq.sl".
           copy "progmag.sl".
           copy "statraff.sl".
           copy "statsett.sl".
           copy "articoli.sl".
           copy "tmp-settore.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tcaumag.sl".
           copy "clienti.sl".
           copy "tmarche.sl".
           copy "timposte.sl".
           copy "ttipocli.sl".
           copy "progmagric.sl".

       select rep-recupero
           assign       to path-rep-recupero
           organization is line sequential
           access mode  is sequential
           file status  is status-rep-recupero.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
LUBEXX     copy "lineseq.fd".
           copy "progmag.fd". 
           copy "statraff.fd".
           copy "statsett.fd".
           copy "articoli.fd".
           copy "tmp-settore.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tcaumag.fd".
           copy "clienti.fd". 
           copy "tmarche.fd".
           copy "timposte.fd".
           copy "ttipocli.fd".
           copy "progmagric.fd".

       FD  rep-recupero.
       01 riga-recupero    pic x(100).

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "costo-medio.def".
       copy "imposte.def".

      * COSTANTI
       78  titolo value "Statistiche settoriale con raffronti per mese".

      * FILE-STATUS
       77  status-lineseq        pic xx.
       77  status-progmag        pic xx.
       77  status-statraff       pic xx.
       77  status-statsett       pic xx.
       77  status-articoli       pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-tmp-settore    pic xx.
       77  status-tcaumag        pic xx.
       77  status-clienti        pic xx.
       77  status-timposte       pic xx.
       77  status-ttipocli       pic xx.
       77  status-tmarche        pic xx.
       77  status-rep-recupero   pic xx.
       77  status-progmagric     pic xx.

       77  wstampa               pic x(256) value spaces.
       77  path-rep-recupero     pic x(256) value spaces.

      * VARIABILI

       01 r-stampa.
         05 r-tipo         pic x.
         05 r-codice       pic z(6).
         05 filler         pic x(3) value " - ".
         05 r-descrizione  pic x(30).
         05 filler         pic x(2). 
         05 r-marca        pic x(25).
         05 filler         pic x(2).
         05 r-prz          pic ----.--9,99.

       77  start-data            pic 9(8).
       01  save-chiave.
           10 save-tipocli       pic x(2).
           10 save-mese          pic 9(2).
           10 save-marca         pic 9(4).
       77  mese                  pic 99.
       77  mese-prec             pic 99.
       77  anno                  pic 9999.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  path-tmp              pic x(256).
       77  csm-mese-scorso       pic s9(12)v99.
       77  fat-mese-scorso       pic s9(12)v99.
       77   kg-mese-scorso       pic s9(15)v999.
       77  como-valore           pic s9(12)v99.
       77  como-valore-pos       pic 9(12)v99.
       77  como-peso             pic s9(12)v999.
       77  como-costo            pic s9(12)v999.
       77  tot-resa-corr-marca   pic s9(12)v99.
       77  tot-resa-prog-marca   pic s9(12)v99.
       77  tot-resa-prog-marca-pos   pic 9(12)v99.
       77  diff-prog             pic s9(12)v99.
       77  diff-corr             pic s9(12)v99.
       77  qta-mese-scorso       pic s9(9).
       77  SaveMarca             pic 9(4).
       77  CallingProgram        pic x(20).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       77  tot-resa-marca        pic s9(12)v99.

       77  save-sts-kg-prog      pic s9(12)v999.
       77  save-sts-qta-prog     pic s9(12)v99.
       77  save-sts-fat-prog     pic s9(12)v99.
       77  save-sts-csm-prog     pic s9(12)v99.
       77  save-sts-adeguam-prog pic s9(12)v99.

       77  como-adeguamento-corr pic s9(12)v99.
       77  como-adeguamento-prog pic s9(12)v99.

LUBEXX 01  titolo-1             pic x(128) 
           value "--- ELENCO MOVIMENTI CON CAUSALE DA CONVERTIRE ---".

LUBEXX 01  t-riga.
           05 t-anno            pic x(4)  value "Anno".
           05 filler            pic x(1).
           05 t-numero          pic z(8)  value "  Numero".
           05 filler            pic x(1).
           05 t-data-movim      pic x(8)  value "  Data  ".
           05 filler            pic x(2).
           05 t-causale         pic x(4)  value "Caus".
           05 filler            pic x(2).
           05 t-des-caus        pic x(30) value "Descrizione causale".
           05 filler            pic x(2).
           05 t-bolla           pic z(8)  value "N. Bolla".
           05 filler            pic x(1).
           05 t-data-bolla      pic x(8)  value "Dt Bolla".
           05 filler            pic x(2).
           05 t-cli             pic z(5)  value "Cli.".
           05 filler            pic x(2).
           05 t-ragsoc          pic x(40) value "Ragione Sociale".

LUBEXX 01  r-riga.
           05 r-anno            pic x(4).
           05 filler            pic x(1).
           05 r-numero          pic z(8).
           05 filler            pic x(1).
           05 r-data-movim      pic x(8).
           05 filler            pic x(2).
           05 r-causale         pic x(4).
           05 filler            pic x(2).
           05 r-des-caus        pic x(30).
           05 filler            pic x(2).
           05 r-bolla           pic z(8).
           05 filler            pic x(1).
           05 r-data-bolla      pic x(8).
           05 filler            pic x(2).
           05 r-cli             pic z(5).
           05 filler            pic x(2).
           05 r-ragsoc          pic x(40).

      * FLAGS
LUBEXX 01  filler                pic 9.
           88 PrimaVolta         value 1, false 0.
       
       01  controlli             pic xx.
         88 errori               value "ER".
         88 tutto-ok             value "OK".

       01  FlagTrovato           pic 9.
         88 trovato              value 1, false 0.

       01  filler                pic 9.
         88 ExitPerform          value 1, false 0.

       01  filler                pic 9.
         88 FaseDiContabilizzazione value 1, false 0.

LUBEXX 01  filler                pic 9.
LUBEXX   88 trovato-mese-scorso  value 1, false 0.

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
      ***---
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
                move   "articoli"    to geslock-nome-file
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

      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                set errori to true
                display message "File [TCAUMAG] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TCAUMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCAUMAG] Indexed file corrupt!"
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
                move   "tcaumag"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input tcaumag
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                set errori to true
                display message "File [CLIENTI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [CLIENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CLIENTI] Indexed file corrupt!"
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
                move   "clienti"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input clienti
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
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

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                set errori to true
                display message "File [TMOVMAG] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVMAG] Indexed file corrupt!"
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
                move   "tmovmag"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input tmovmag
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                set errori to true
                display message "File [RMOVMAG] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [RMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RMOVMAG] Indexed file corrupt!"
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
                move   "rmovmag"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input rmovmag
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       STATSETT-ERR SECTION.
           use after error procedure on statsett.
           set tutto-ok  to true.
           evaluate status-statsett
           when "35"
                set errori to true
                display message "File [STATSETT] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [STATSETT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STATSETT] Indexed file corrupt!"
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
                move   "statsett"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o statsett allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
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
                     open input statraff
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
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
                move   "tmarche"    to geslock-nome-file
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

      ***---
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
                move   "timposte"   to geslock-nome-file
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

      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                set errori to true
                display message "File [TTIPOCLI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TTIPOCLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TTIPOCLI] Indexed file corrupt!"
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
                move   "ttipocli"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input ttipocli
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       TMP-SETTORE-ERR SECTION.
           use after error procedure on tmp-settore.
           set tutto-ok  to true.
           evaluate status-tmp-settore
           when "35"
                set errori to true
                display message "File [TMP-SETTORE] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TMP-SETTORE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-SETTORE] Indexed file corrupt!"
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
                     open output tmp-settore
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
           move link-data      to start-data.
           move 01             to start-data(7:2).
           move link-data(5:2) to mese.
           move link-data(1:4) to anno.
           initialize path-tmp.
LUBEXX     set PrimaVolta      to true.
           set trovato         to false.
           set tutto-ok        to true.
           accept path-tmp   from environment "PATH-ST".
           accept como-data  from century-date.
           accept como-ora   from time.
           inspect path-tmp  replacing trailing spaces by low-value.
           string  path-tmp  delimited by low-value
                   "aggstatsett"  delimited by size
                   "_"            delimited by size
                   como-data      delimited by size
                   "_"            delimited by size
                   como-ora       delimited by size
                   ".tmp"         delimited by size
                   into path-tmp
           end-string.
           move spaces to CallingProgram.
           call "C$CALLEDBY" using CallingProgram.
           if CallingProgram = "contab"
              set FaseDiContabilizzazione to true
           else
              set FaseDiContabilizzazione to false
           end-if.

      ***---
       OPEN-FILES.
           open i-o statsett allowing readers.
           if tutto-ok
              open output tmp-settore
              if tutto-ok
                 close tmp-settore
                 open i-o tmp-settore
                 open input progmag
                            articoli
                            tmovmag
                            rmovmag
                            tcaumag
                            clienti
                            statraff
                            tmarche
                            timposte
                            ttipocli
                            progmagric
              else
                 close statsett
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

           move  low-value   to tmo-rec.
           move  start-data  to tmo-data-movim.

           if not FaseDiContabilizzazione
              perform CREA-RECORD-DUMMY
           end-if.

           start tmovmag key is >= k-data
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

                 read tmovmag next at end exit perform end-read
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon scr-oper-handle at column 34
                                                 line 25
                    move 0 to counter2
                    if counter = 100
                       display "STATSETT COUNTER"
                          upon scr-oper-handle at column 30
                                                    line 26
                    end-if
                 end-if

                 if tmo-data-movim > link-data exit perform end-if

                 if tmo-cliente

                    move tmo-causale to tca-codice
                    read tcaumag no lock invalid continue end-read

LUBEXX              perform CONTROLLA-CAUSALE-SPECIALE

                    if tca-cliente and tca-si-stat
                       set  cli-tipo-C     to true
                       move tmo-cod-clifor to cli-codice
                       read clienti no lock
                            invalid continue
                        not invalid
                            move cli-tipo to tcl-codice
                            read ttipocli no lock
                                 invalid continue
                             not invalid perform LOOP-RIGHE-MOVMAG
                            end-read
                       end-read
                    end-if
                 end-if

              end-perform

              if trovato
                 move 1 to link-result
                 close    tmp-settore
                 open i-o tmp-settore
                 if FaseDiContabilizzazione
                    perform AGGIORNA-STATISTICHE-CONTAB
                 else
                    perform AGGIORNA-STATISTICHE-AGGMESE
                 end-if
              else
                 move 0 to link-result
              end-if
           end-if.

LUBEXX***---
LUBEXX*Se la causale prevede un movimento spaciale
LUBEXX*scrivo un record di un txt che verrà emesso 
LUBEXX*come avviso a fine elaborazione
LUBEXX CONTROLLA-CAUSALE-SPECIALE.
           if tca-si-speciale
              if PrimaVolta
                 set PrimaVolta to false
                 initialize wstampa
                 accept  wstampa from environment "PATH_ST"
                 inspect wstampa replacing trailing spaces by low-value
                 string  wstampa                 delimited low-value
                         "ELENCO_MOVIM_SPECIALI" delimited size
                         "_"                     delimited size
                         como-data               delimited size
                         "_"                     delimited size
                         como-ora                delimited size
                         ".txt"                  delimited size
                         into wstampa
                 end-string
                 open output lineseq
                 call "C$JUSTIFY" using titolo-1, "C"
                 move titolo-1         to line-riga
                 write line-riga from titolo-1 after 2
                 write line-riga from spaces
                 write line-riga from t-riga
                 move all "-" to t-riga
                 write line-riga from t-riga
              end-if

              initialize r-riga
              move tmo-anno            to r-anno
              move tmo-numero          to r-numero
              move tmo-data-movim(3:2) to r-data-movim(7:2)
              move "/"                 to r-data-movim(6:1)
              move tmo-data-movim(5:2) to r-data-movim(4:2)
              move "/"                 to r-data-movim(3:1)
              move tmo-data-movim(7:2) to r-data-movim(1:2)
              move tmo-causale         to r-causale
              move tca-descrizione     to r-des-caus
              move tmo-numdoc-clifor   to r-bolla
              move tmo-data-doc(3:2)   to r-data-bolla(7:2)
              move "/"                 to r-data-bolla(6:1)
              move tmo-data-doc(5:2)   to r-data-bolla(4:2)
              move "/"                 to r-data-bolla(3:1)
              move tmo-data-doc(7:2)   to r-data-bolla(1:2)
              move tmo-cod-clifor      to r-cli
              set  cli-tipo-C          to true
              move tmo-cod-clifor      to cli-codice
              read clienti no lock
                   invalid move "*** NON TROVATO ***" to r-ragsoc
               not invalid move cli-ragsoc-1          to r-ragsoc
              end-read
              write line-riga from r-riga after 1

           end-if.

      ***---
       LOOP-RIGHE-MOVMAG.
           set  tutto-ok     to true.
           move low-value    to rmo-rec.
           move tmo-chiave   to    rmo-chiave.
           start rmovmag key is >= rmo-chiave 
                 invalid set errori to true 
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rmovmag  next at end exit perform end-read
                 if rmo-anno   not = tmo-anno or
                    rmo-movim  not = tmo-numero
                    exit perform
                 end-if
                 initialize tms-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move spaces to prg-cod-magazzino
                 move spaces to prg-tipo-imballo
                 move 0      to prg-peso
                 move rmo-articolo to prg-cod-articolo
                 read progmag no lock invalid continue end-read
                 move rmo-articolo to art-codice
                 read articoli no  lock
                      invalid  continue
                  not invalid
                      move cli-tipo           to tms-tipo
LUBEXX*****           GDO ESTERA: Risolta dando la prevalenza al
LUBEXX*****           gruppo GDO forzando "1" (GDO) nel tipo
LUBEXX*****           Se cli-tipo non è 4 allora è GDS o GDO x' sono
LUBEXX*****           gli unici gruppi a poter avere un indicazione
LUBEXX*****           del gruppo GDO all'interno della scheda cliente
                                 
LUBEXX*****                if cli-gdo not = spaces and cli-tipo = "4 "
LUBEXX*****                   move "1" to tms-tipo
LUBEXX*****                end-if

LUBEXX                if tcl-gdo-si or tcl-gdo-opz
                         if cli-tipo = "4 "
LUBEXX                      move "1" to tms-tipo
                         end-if
LUBEXX                end-if

                      move art-marca-prodotto to tms-marca
                      read tmp-settore no lock invalid continue 
                      end-read
                      |NUOVA PROCEDURA
                                                
                      set TrattamentoGDO to true
                      if FaseDiContabilizzazione
                         perform COSTO-MP-NOTTURNO
                         add 0,005 to costo-mp giving costo-mp-2dec
                      else
                         |Per il consolidamento uso i progressivi
                         |effettivi altrimenti perderei le modifiche
                         |fatte il giorno stesso
                         perform CALCOLA-COSTO-MP-COMPLETO
                         add 0,005 to costo-mp giving costo-mp-2dec
                      end-if

                      |Dato che i casi di recupero del costo
                      |al di fuori del medio sono ridotti al minimo
                      |e comunque sono gli stessi del ricalcolo notturno
                      |non conviene creare il file txt
      *****                if recupero-anagrafica or 
      *****                   recupero-iniziale   or
      *****                   recupero-ultimo
      *****                   if FaseDiContabilizzazione
      *****                      if path-rep-recupero = spaces
      *****                         perform CREA-REPORT-RECUPERI
      *****                      end-if
      *****
      *****                      evaluate true
      *****                      when recupero-iniziale   move "I" to r-tipo
      *****                      when recupero-ultimo     move "U" to r-tipo
      *****                      when recupero-anagrafica move "A" to r-tipo
      *****                      end-evaluate
      *****
      *****                      move art-codice       to r-codice
      *****                      move art-descrizione  to r-descrizione
      *****                      move mar-descrizione  to r-marca
      *****                      move costo-mp         to r-prz
      *****                      write riga-recupero from r-stampa
      *****                   end-if
      *****                end-if

                      |FINE NUOVA PROCEDURA
                      |VECCHIA PROCEDURA
      *****                if FaseDiContabilizzazione
      *****                   perform CALCOLA-COSTO-MP
      *****                else
      *****                   set TrattamentoGDO to true
      *****                   perform CALCOLA-COSTO-MP-COMPLETO
      *****                end-if
                      |FINE VECCHIA PROCEDURA
                      perform SOMMA-VALORI
                 end-read
              end-perform
           end-if.

      ***---
       COSTO-MP-NOTTURNO.
           move prg-chiave to prr-chiave.
           read progmagric no lock invalid continue end-read.

           move prr-costo-medio to costo-mp.

           if costo-mp = 0
              if prr-acq-udm = 0 |VERIFICO ACQUISTI
                 if prr-ini-udm = 0 |VERIFICO RIMANENZE INIZIALI
                    if prr-costo-ultimo      not = 0
                       move prr-costo-ultimo to costo-mp
                       set recupero-ultimo   to true
                    else
                       if prr-costo-inizio not = 0
                          move prr-costo-inizio to costo-mp
                          set recupero-iniziale to true
                       else
                          move prr-prz-anagrafica to costo-mp
                          set recupero-anagrafica to true
                       end-if
                    end-if
                 else
                    set recupero-ini to true
                 end-if
              else
                 set recupero-acq to true
              end-if
           else
              set recupero-normale to true
           end-if.
           read progmagric no lock invalid continue end-read.

      ***---
       SOMMA-VALORI.
           |NCPZ ad esempio non devono incidere sul peso, perciò
           |se non ho quantità, non ho movimentazioni.
           |Come da richiesta di Mori (28/04), in caso di qta 0,
           |scaricare SOLAMENTE il valore, non il peso, né il costo
           if rmo-qta = 0
      *****        compute como-costo  = costo-mp
              move 0 to como-peso
              compute como-valore = rmo-netto    +
                                    rmo-imp-cons + 
                                    rmo-coubat
              move 0 to como-costo

      *****        |Richiesta di Mori 25/11. Se non ci sono movimentazioni
      *****        |né di quantità, né di valore per quella riga devono
      *****        |essere scaricate a costo 0.
      *****        if como-valore = 0
      *****           move 0        to como-costo
      *****        else
      *****           move costo-mp to como-costo
      *****        end-if
           else
              compute como-costo  = rmo-qta * costo-mp-2dec
              compute como-peso   = rmo-qta * rmo-peso
              compute como-valore = rmo-qta *
                    ( rmo-netto + rmo-imp-cons + rmo-coubat)
           end-if.

      *     if FaseDiContabilizzazione
              if tca-imponibile-pos
                 add como-valore to tms-fat-corr
                 add como-peso   to tms-kg-corr
                 add rmo-qta     to tms-qta-corr
                 add como-costo  to tms-csm-corr
              else
                 subtract como-valore from tms-fat-corr
                 subtract como-peso   from tms-kg-corr
                 subtract rmo-qta     from tms-qta-corr
                 subtract como-costo  from tms-csm-corr
              end-if
      *     else
      *        if tca-imponibile-pos
      *           add como-valore to tms-fat-prog
      *           add como-peso   to tms-kg-prog
      *           add rmo-qta     to tms-qta-prog
      *           add como-costo  to tms-csm-prog
      *        else
      *           subtract como-valore from tms-fat-prog
      *           subtract como-peso   from tms-kg-prog
      *           subtract rmo-qta     from tms-qta-prog
      *           subtract como-costo  from tms-csm-prog
      *        end-if
      *     end-if.
           write tms-rec invalid rewrite tms-rec end-write.
           set trovato to true. 

      ***---
       AGGIORNA-STATISTICHE-CONTAB.
           |AZZERO LE QUANTITA' CORRENTI PER IL MESE IN CORSO
           move low-value to sts-chiave.
           move mese      to sts-mese.
           start statsett key is >= k-ord
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read statsett next at end exit perform end-read
                    if sts-mese not = mese    exit perform end-if
                    move 0 to sts-kg-corr
                    move 0 to sts-qta-corr
                    move 0 to sts-fat-corr
                    move 0 to sts-csm-corr
                    move 0 to sts-adeguam-corr
                    rewrite sts-rec invalid continue end-rewrite
                 end-perform
           end-start.

           move low-value to tms-chiave.
           start tmp-settore key is >= tms-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 initialize sts-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 read tmp-settore next at end exit perform end-read
                 move tms-tipo     to sts-tipocli
                 move tms-marca    to sts-marca
                 move mese         to sts-mese
                 read statsett no lock invalid continue end-read
                 move tms-fat-corr to sts-fat-corr
                 move tms-kg-corr  to sts-kg-corr
                 move tms-qta-corr to sts-qta-corr
                 move tms-csm-corr to sts-csm-corr
                 rewrite sts-rec invalid write sts-rec end-rewrite
              end-perform
           end-if.

      ***---
       AGGIORNA-STATISTICHE-AGGMESE.
           move low-value to tms-rec.
           start tmp-settore key is >= tms-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read tmp-settore next at end exit perform end-read
              perform AGGIORNA-STATISTICHE
           end-perform.
           |DA CONTAB SERVE SOLTANTO AGGIORNARE 
           |I DATI CORRENTI CON A+B+C * QTA
           perform ADEGUAMENTO.
LUBEXX     perform RIPORTA-MARCHE-MESI-PRECEDENTI.

      ***---
       AGGIORNA-STATISTICHE.
           if mese = 1
              move 0 to fat-mese-scorso
              move 0 to qta-mese-scorso
              move 0 to csm-mese-scorso
              move 0 to  kg-mese-scorso
           else
              subtract 1 from mese giving sts-mese
      *****        move anno      to sts-anno
              move tms-tipo  to sts-tipocli
              move tms-marca to sts-marca
              read statsett
                   invalid
                   move 0 to qta-mese-scorso
                             csm-mese-scorso
                             fat-mese-scorso
                              kg-mese-scorso
               not invalid
                   move sts-qta-prog to qta-mese-scorso
                   move sts-fat-prog to fat-mese-scorso
                   move sts-csm-prog to csm-mese-scorso
                   move sts-kg-prog  to  kg-mese-scorso
              end-read
           end-if.

           initialize sts-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move mese       to sts-mese.
      *****     move anno       to sts-anno.
           move tms-marca  to sts-marca.
           move tms-tipo   to sts-tipocli.

           read statsett
                invalid set trovato to false
            not invalid set trovato to true
           end-read.

LUBEXX     compute sts-qta-prog
LUBEXX             tms-qta-prog = ( tms-qta-corr + qta-mese-scorso).
LUBEXX     compute sts-fat-prog
LUBEXX             tms-fat-prog = ( tms-fat-corr + fat-mese-scorso).
LUBEXX     compute sts-csm-prog
LUBEXX             tms-csm-prog = ( tms-csm-corr + csm-mese-scorso).
LUBEXX     compute sts-kg-prog
LUBEXX             tms-kg-prog  = ( tms-kg-corr  + kg-mese-scorso).

LUBEXX*****           compute sts-qta-corr 
LUBEXX*****                   tms-qta-corr = ( tms-qta-prog - qta-mese-scorso).
LUBEXX*****           compute sts-fat-corr
LUBEXX*****                   tms-fat-corr = ( tms-fat-prog - fat-mese-scorso).
LUBEXX*****           compute sts-csm-corr
LUBEXX*****                   tms-csm-corr = ( tms-csm-prog - csm-mese-scorso).
LUBEXX*****           compute sts-kg-corr  
LUBEXX*****                   tms-kg-corr  = ( tms-kg-prog  - kg-mese-scorso).

           move tms-qta-corr to sts-qta-corr.
           move tms-fat-corr to sts-fat-corr.
           move tms-csm-corr to sts-csm-corr.
           move tms-kg-corr  to sts-kg-corr.

           rewrite tms-rec invalid continue end-rewrite.

           if not trovato
              move link-user to sts-utente-creazione
              accept sts-data-creazione from century-date
              accept sts-ora-creazione  from time
              write sts-rec invalid continue end-write
           else
              move link-user to sts-utente-ultima-modifica
              accept sts-data-ultima-modifica from century-date
              accept sts-ora-ultima-modifica  from time
              rewrite sts-rec invalid continue end-rewrite
           end-if.

      ***---
       CREA-RECORD-DUMMY.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                          "
              upon scr-oper-handle at column 34
                                        line 25
           display "                          "
              upon scr-oper-handle at column 30
                                        line 26
           ||||||||

           move 0 to counter counter2.
           move low-value to tcl-chiave.
           start ttipocli key >= tcl-chiave invalid continue end-start.
           perform until 1 = 2
              read ttipocli next at end exit perform end-read
              move low-value to mar-chiave
              start tmarche key >= mar-chiave invalid continue end-start
              perform until 1 = 2

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 10
                    move counter to counter-edit
                    display counter-edit
                       upon scr-oper-handle at column 34
                                                 line 25
                    move 0 to counter2
                    if counter = 10               
                       display "DUMMY COUNTER"
                          upon scr-oper-handle at column 30
                                                    line 26
                    end-if
                 end-if

                 read tmarche next at end exit perform end-read
                 initialize tms-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move tcl-codice to tms-tipo
                 move mar-codice to tms-marca
                 write tms-rec invalid rewrite tms-rec end-write
              end-perform
           end-perform.

      ***---
       ADEGUAMENTO.
           move 0 to SaveMarca.
           set ExitPerform to false.
           perform until 1 = 2
              move low-value to tms-rec
              move SaveMarca to tms-marca
              start tmp-settore key is >= k-marca
                    invalid continue
              end-start

              perform until 1 = 2
                 read tmp-settore next
                      at end perform CALCOLA-PERCENTUALI
                             set ExitPerform to true
                             exit perform
                 end-read

                 if SaveMarca not = tms-marca
                    if SaveMarca = 0
                       move tms-marca to SaveMarca
                    else
                       perform CALCOLA-PERCENTUALI
                       move tms-marca to SaveMarca
                       exit perform
                    end-if
                 end-if

                 compute como-valore = tms-fat-prog - tms-csm-prog
                 add como-valore  to tot-resa-prog-marca 
                 
                 |Calcolo la resa in valore assoluto
                 |solamente per le % di incidenza
                 move como-valore    to como-valore-pos
                 add como-valore-pos to tot-resa-prog-marca-pos


                 compute como-valore = tms-fat-corr - tms-csm-corr
                 add como-valore to tot-resa-corr-marca

              end-perform
              if ExitPerform exit perform end-if
           end-perform.

      ***---
       CALCOLA-PERCENTUALI.
           move low-value to tms-rec.
           move SaveMarca to tms-marca.
           start tmp-settore key is >= k-marca
                 invalid continue
           end-start.

           perform until 1 = 2
              read tmp-settore next no lock at end exit perform end-read
              if tms-marca not = SaveMarca         exit perform end-if

LUBEXX*****              compute como-valore = tms-fat-prog - tms-csm-prog
LUBEXX*****              compute tms-perce-adeguam-prog rounded =
LUBEXX*****                    ( 100 * como-valore ) / tot-resa-prog-marca

              |Come richiesta di Mori 15/01/08: utilizzare il margine
              |e prendere come punto di riferimento i valori progressivi
      *****        compute como-valore = tms-fat-corr - tms-csm-corr
      *****        compute tms-perce-adeguam-corr rounded =
      *****              ( 100 * como-valore ) / tot-resa-corr-marca

              compute como-valore = tms-fat-prog - tms-csm-prog
      *****        compute tms-perce-adeguam-corr rounded =
      *****              ( 100 * como-valore ) / tot-resa-prog-marca

              |Come richiesta di Mori:
              |La % di incidenza in valore assoluto
              move como-valore to como-valore-pos
              compute tms-perce-adeguam-corr rounded =
                    ( 100 * como-valore-pos ) / tot-resa-prog-marca-pos

              rewrite tms-rec invalid continue end-rewrite
           end-perform.

           initialize str-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move SaveMarca to str-marca.
           move mese      to str-mese.
           read statraff  no lock
                invalid   move tot-resa-corr-marca to str-vag-corr
                          move tot-resa-prog-marca to str-vag-prog
           end-read.

           compute diff-corr = str-vag-corr - tot-resa-corr-marca.
      *****     compute diff-prog = str-vag-prog - tot-resa-prog-marca.

           move low-value to tms-rec.
           move SaveMarca to tms-marca.
           start tmp-settore key is >= k-marca
                 invalid continue
           end-start.
           perform until 1 = 2
              read tmp-settore next at end exit perform end-read
              if tms-marca not = SaveMarca exit perform end-if

LUBEXX        move SaveMarca to sts-marca                     
LUBEXX        move tms-tipo  to sts-tipocli
LUBEXX        subtract 1 from mese giving sts-mese
LUBEXX        read statsett 
LUBEXX             invalid
LUBEXX             set trovato-mese-scorso to false
LUBEXX         not invalid
LUBEXX             set trovato-mese-scorso to true
LUBEXX             move sts-adeguam-prog   to como-adeguamento-prog
LUBEXX        end-read

LUBEXX        compute como-adeguamento-corr rounded =
LUBEXX              ( diff-corr * tms-perce-adeguam-corr ) / 100

LUBEXX        move SaveMarca to sts-marca
LUBEXX        move tms-tipo  to sts-tipocli
LUBEXX        move mese      to sts-mese
LUBEXX        read statsett
LUBEXX             invalid continue
LUBEXX         not invalid

LUBEXX             move como-adeguamento-corr to sts-adeguam-corr
LUBEXX             if trovato-mese-scorso
LUBEXX                compute sts-adeguam-prog =
LUBEXX                        sts-adeguam-corr +
LUBEXX                        como-adeguamento-prog
LUBEXX             else
LUBEXX                move como-adeguamento-corr to sts-adeguam-prog
LUBEXX             end-if

LUBEXX             rewrite sts-rec invalid continue end-rewrite
LUBEXX        end-read

           end-perform.
           move 0 to tot-resa-corr-marca 
                     tot-resa-prog-marca
                     tot-resa-prog-marca-pos.

LUBEXX* Aggiorno valori prog del mese corrente per tutte
LUBEXX* le eventuali marche che non sono state registrate
LUBEXX* ma che lo sono state nel mese precedente.
LUBEXX* Ad esempio se ho venduto 10 per Marzo su AGIP, gli
LUBEXX* stessi 10 devono essere presenti "prog" su Aprile.
      ***---
       RIPORTA-MARCHE-MESI-PRECEDENTI.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                          "
              upon scr-oper-handle at column 34
                                        line 25
           display "                          "
              upon scr-oper-handle at column 30
                                        line 26
           ||||||||

           move 0 to counter counter2.

           move low-value to sts-rec.
           subtract 1 from mese giving mese-prec.
           move mese-prec to sts-mese.
           start statsett key is >= k-ord
                 invalid continue
             not invalid
                 perform until 1 = 2

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 10
                       move counter to counter-edit
                       display counter-edit
                          upon scr-oper-handle at column 34
                                                    line 25
                       move 0 to counter2
                       if counter = 10               
                          display "RECUPERO COUNTER"
                             upon scr-oper-handle at column 30
                                                       line 26
                       end-if
                    end-if

                    read statsett next at end   exit perform end-read
                    if sts-mese not = mese-prec exit perform end-if
                    move sts-marca   to tms-marca
                    move sts-tipocli to tms-tipo
                    read tmp-settore 
                         invalid
                         move sts-chiave to save-chiave
                         move mese to sts-mese
                         move 0 to sts-kg-corr
                         move 0 to sts-qta-corr
                         move 0 to sts-fat-corr
                         move 0 to sts-csm-corr
                         move 0 to sts-adeguam-corr

                         move sts-kg-prog      to save-sts-kg-prog
                         move sts-qta-prog     to save-sts-qta-prog
                         move sts-fat-prog     to save-sts-fat-prog
                         move sts-csm-prog     to save-sts-csm-prog
                         move sts-adeguam-prog to save-sts-adeguam-prog

                         move link-user to sts-utente-creazione
                         accept sts-data-creazione from century-date
                         accept sts-ora-creazione  from time
                         write  sts-rec 
                                invalid
                                |Se lo trovo significa che non ho
                                |movimentato, ma che ho dentro dei dati
                                |dell'anno precedente. In questo caso
                                |aggiorno solamente i dati prog
                                read statsett no lock
                                     invalid continue |-->NON DEVE MAI CAPITARE!!!
                                 not invalid
                                     move save-sts-kg-prog
                                       to sts-kg-prog
                                     move save-sts-qta-prog
                                       to sts-qta-prog
                                     move save-sts-fat-prog
                                       to sts-fat-prog
                                     move save-sts-csm-prog
                                       to sts-csm-prog
                                     move save-sts-adeguam-prog
                                       to sts-adeguam-prog
                                     rewrite sts-rec 
                                             invalid continue 
                                     end-rewrite
                                end-read
                         end-write
                         move save-chiave  to sts-chiave
                         start statsett key > k-ord
                               invalid exit perform
                         end-start
                    end-read
                 end-perform
           end-start.

      ***---
       CREA-REPORT-RECUPERI.
           accept path-rep-recupero from environment "PATH_ST".
           accept como-data from century-date.
           accept como-ora  from century-date.
           inspect path-rep-recupero replacing trailing 
                                     spaces by low-value.
           string  path-rep-recupero            delimited low-value
                   "RECUPERO_PRZ_CONTAB_" delimited size
                   como-data              delimited size
                   "_"                    delimited size
                   como-ora               delimited size
                   ".txt"                 delimited size
                   into path-rep-recupero
           end-string.

           open output rep-recupero.
           initialize riga-recupero.
           string "CONTABILIZZAZIONE DEL " delimited size
                  como-data(7:2)           delimited size
                  "/"                      delimited size
                  como-data(5:2)           delimited size
                  "/"                      delimited size
                  como-data(1:4)           delimited size
                  " ALLE ORE: "            delimited size
                  como-ora(5:2)            delimited size
                  ":"                      delimited size
                  como-ora(1:4)            delimited size
                  into riga-recupero
           end-string.
           write riga-recupero.
           write riga-recupero from spaces.

      ***---
       CLOSE-FILES.
           close progmag
                 statsett
                 articoli
                 tmp-settore
                 tcaumag
                 clienti
                 statraff
                 tmarche
                 timposte
                 ttipocli
                 progmagric.
           delete file tmp-settore.

LUBEXX     if not PrimaVolta
LUBEXX        close lineseq
LUBEXX        call   "spooler-A"  using "A", wstampa, "O"
LUBEXX        cancel "spooler-A"
LUBEXX        delete file lineseq
LUBEXX     end-if.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "costo-medio.cpy".
       copy "calcola-costo-mp-when-zero.cpy".
