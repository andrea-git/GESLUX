       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      fordfor-p.
       AUTHOR.                          Andrea.
       REMARKS. Motore per il calcolo sul file ORDFOR. In linkage mi 
                vengono passate due date del periodo di riferimento.
                LA FASE DI AGGIORNAMENTO HA LAVORATO SOLAMENTE SU LBX
                COME DA RICHIESTA DI M. VINCENZI!!!

               ************************************************
               * ATTENZIONE!!! ALLINEARE SEMPRE CON ORDINI!!! *
               * E CON RICALFOR-BAT!!!                        *
               ************************************************

               NON PIU USATO!!!!!

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "ordfor.sl".
           copy "tmarche.sl".
           copy "articoli.sl".
           copy "progmag.sl".
           copy "timbalqta.sl".
           copy "lineseq.sl".
           copy "rlistini.sl".
           copy "tmagaz.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "ordfor.fd".
           copy "tmarche.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "timbalqta.fd".
           copy "lineseq.fd".
           copy "rlistini.fd".
           copy "tmagaz.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "common-excel.def".
       copy "acugui.def".
       copy "costo-medio.def".

      * COSTANTI
       78  titolo value "Aggiornamento file ordini a fornitori".

      * FILE-STATUS
       77  status-ordfor            pic xx.
       77  status-tmarche           pic xx.
       77  status-articoli          pic xx.
       77  status-progmag           pic xx.
       77  status-timbalqta         pic xx.
       77  status-rlistini          pic xx.
       77  status-tmagaz            pic xx.
       77  status-lineseq           pic xx.
       77  wstampa                  pic x(256).

      * VARIABILI
       77  como-giacenza            pic s9(8).
       77  mese                     pic 99.
       77  mese-start               pic 99.
       77  mese-end                 pic 99.
       77  anno                     pic 9(4).
       77  anno-corr                pic 9(4).
       77  anno-past                pic 9(4).
       77  wk-campo                 pic s9(12)v99.
       77  idx                      pic 9(3).
       01  occurs-qta.
         03 el-qta                  pic s9(9) occurs 12.
       77  tit-mese                 pic x(6)  occurs 12.
       77  tot-anno                 pic s9(12)v99 value 0.
       77  giacenza                 pic 9(8).
       77  giac-positiva            pic 9(8).
       77  SaveImballo              pic x(3).
       77  counter                  pic 9(10).
       77  counter2                 pic 9(10).
       77  counter-edit             pic z(10).
       77  data-calcolo             pic 9(8).

      * FLAGS
       01  controlli                pic xx.
         88 errori                  value "ER".
         88 tutto-ok                value "OK".

       01  YearType                 pic x.
         88 PreviousYear            value "P".
         88 CurrentYear             value "C".

       01  FlagTrovato              pic 9.
         88 trovato                 value 1, false 0.

       01  filler                   pic 9.
         88 ExitPerform             value 1, false 0.

       01  filler                   pic 9.
         88 record-ok               value 1, false 0.

       01  filler                   pic 9.
         88 prima-volta             value 1, false 0.

       01  filler                   pic 9.
         88 aggiorna-mese-corrente  value 1.
         88 aggiorna-anno-mese      value 2.

       01  filler                   pic 9.
         88 GeneraFileExcel         value 1, false 0.

      * RIGHE PER LA STAMPA
       01 r-riga.
           05 r-articolo         pic z(6).
           05 r-des-art          pic x(40).
           05 r-qta-imb          pic z(4).
           05 r-marca            pic z(4).
           05 r-scorta           pic ---.---.--9.
           05 r-scost            pic zz9,99.
           05 r-tab-mesi.
              10 r-qta           pic ----.---.--9   occurs 12.
           05 r-tot-anno         pic --.---.---.--9.
           05 r-media            pic ----.---.--9,99.
           05 r-consegna         pic ----.---.--9,99.
           05 r-riordino         pic ----.---.--9,99.
           05 r-giacenza         pic ---.---.--9.
           05 r-tab-fabb.
              10 r-fabb          pic ----.---.--9   occurs 6.
           05 r-listino          pic zzz.zzz.zz9,99.
           05 r-ul-costo         pic ----.---.--9,99.
           05 r-prz-medio        pic ----.---.--9,99.
           05 r-altezza          pic zzz.zz9,99.
           05 r-larghezza        pic zzz.zz9,99.
           05 r-profondita       pic zzz.zz9,99.
           05 r-qta-epal         pic zz.zzz.zz9.
           05 r-qta-std          pic zz.zzz.zz9.
           05 r-codice-ean-1     pic x(13).
           05 r-codice-ean-2     pic x(13).
           05 r-codice-ean-3     pic x(13).
           05 r-codice-ean-4     pic x(13).
           05 r-codice-ean-5     pic x(13).
           05 r-alter            pic x(40).
           05 r-cod-forn         pic x(15).

       LINKAGE SECTION.
       77  user-codi                pic x(10).
       77  start-data               pic 9(8).
       77  end-data                 pic 9(8).
       77  link-magazzino           pic x(3).
       77  link-result              pic 9.
       77  link-handle              handle of window.

       PROCEDURE DIVISION USING user-codi, 
                                start-data, 
                                end-data, 
                                link-magazzino,
                                link-result,
                                link-handle.

       DECLARATIVES.
      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "File TXT not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File TXT mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "TXT Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "Chiudere file Excel!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File CSV"   to geslock-nome-file
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
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "File [ARTICOLI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed [ARTICOLI] file corrupt!"
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

      ***---
       ORDFOR-ERR SECTION.
           use after error procedure on ordfor.
           set tutto-ok  to true.
           evaluate status-ordfor
           when "35"
                display message "File [ORDFOR] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [ORDFOR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ORDFOR] Indexed file corrupt!"
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
                move   "ordfor"     to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o ordfor allowing readers
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
                display message "File [PROGMAG] not found!"
                          title titolo
                           icon 3
                set errori to true
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
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message "File [TMARCHE] not found!"
                          title titolo
                           icon 3
                set errori to true
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
       TIMBALQTA-ERR SECTION.
           use after error procedure on timbalqta.
           set tutto-ok  to true.
           evaluate status-timbalqta
           when "35"
                display message "File [TIMBALQTA] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [TIMBALQTA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMBALQTA] Indexed file corrupt!"
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
                move   "timbalqta"  to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input timbalqta
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
           set  GeneraFileExcel to false.
           set  trovato         to false.
           set  tutto-ok        to true.
           set  prima-volta     to true.
           move start-data(5:2) to mese-start.
           move end-data(5:2)   to mese-end.
           move end-data(1:4)   to anno-corr.
           move start-data(1:4) to anno-past.
           move 0 to counter counter2.
           initialize wstampa.
           accept  wstampa   from environment "PATH-ST".
           inspect wstampa   replacing trailing spaces by low-value.
           inspect user-codi replacing trailing spaces by low-value.
           |INIZIO NUOVA PROCEDURA
           string  wstampa        delimited low-value
                   "verifica.csv" delimited size
                   into wstampa
           end-string.
           |FINE NUOVA PROCEDURA
           |INIZIO VECCHIA PROCEDURA
      *****     string  wstampa   delimited low-value
      *****             "ordfor"  delimited size
      *****             "_"       delimited size
      *****             user-codi delimited low-value
      *****             ".csv"    delimited low-value
      *****             into wstampa
      *****     end-string.
           |FINE VECCHIA PROCEDURA

      ***---
       OPEN-FILES.
           open i-o ordfor allowing readers.
           if tutto-ok
              open input progmag
                         articoli
                         tmarche
                         timbalqta
                         rlistini
                         tmagaz
              if errori
                 close ordfor
              end-if

           end-if.

           if errori goback end-if.

      ***---
       ELABORAZIONE.
           move low-value  to ord-rec.
           start ordfor key is >= ord-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2

                 read ordfor next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 22
                                             line 03
                    move 0 to counter2
                 end-if

                 move ord-articolo        to art-codice
                 read articoli no lock
                      invalid continue
                  not invalid
                      move ord-articolo to prg-cod-articolo
                      move spaces       to prg-cod-magazzino
                      move spaces       to prg-tipo-imballo
                      move 0            to prg-peso
                      read progmag no lock
                           invalid move 0 to costo-mp
                       not invalid perform AGGIORNA-RECORD
                      end-read
                 end-read

              end-perform
           end-if.
           if not trovato
              move 0 to link-result
           else
              move 1 to link-result
              if GeneraFileExcel
                 perform GENERA-FILE-EXCEL
              end-if
           end-if.

      ***---
       AGGIORNA-RECORD.
           accept data-calcolo from century-date.
           move 0 to ord-lead-time.
           if art-cod-fornitore = 0
              move art-marca-prodotto  to mar-codice
              read tmarche no lock
                   invalid move 0 to mar-scostamento mar-lead-time
               not invalid move mar-lead-time to ord-lead-time
              end-read
           else
              move low-value         to rlis-rec
              move art-cod-fornitore to rlis-fornitore
              move art-cod-desf-forn to rlis-destino
              move art-codice        to rlis-articolo
              start rlistini key >= rlis-k-art of rlistini
                    invalid
                not invalid
                    perform until 1 = 2
                       read rlistini next at end exit perform end-read
                       if rlis-articolo  not = art-codice        or
                          rlis-fornitore not = art-cod-fornitore or
                          rlis-destino   not = art-cod-desf-forn
                          exit perform
                       end-if
                       if data-calcolo >= rlis-ini-val and
                          data-calcolo <= rlis-fine-val
                          move rlis-lead-time to ord-lead-time
                       end-if
                    end-perform
              end-start
              move art-marca-prodotto  to mar-codice
              read tmarche no lock
                   invalid move 0 to mar-scostamento mar-lead-time
               not invalid move mar-lead-time to ord-lead-time
              end-read
              if ord-lead-time = 0
                 move mar-lead-time to ord-lead-time
              end-if
           end-if.

           |17/12/2007 Come da richiesta di Walter: invece di usare
           |l'imballo standard prendo quello con > giacenza assoluta
           perform IMBALLO-MAGGIOR-GIACENZA.
      ****     move art-imballo-standard to imq-codice.
           move prg-tipo-imballo to imq-codice.

           read timbalqta no lock
                invalid move 0 to imq-qta-imb
           end-read.

           perform TROVA-GIACENZA.
           |Poi mi riposiziono sul padre per il calcolo del costo MP
           move ord-articolo to prg-cod-articolo.
           move spaces       to prg-cod-magazzino.
           move spaces       to prg-tipo-imballo.
           move 0            to prg-peso.
           read progmag no lock invalid continue end-read.
           
           move art-descrizione     to ord-art-descrizione.|SOLO PER L'ORDINAMENTO
           move art-marca-prodotto  to ord-marca.
           move imq-qta-imb         to ord-qta-imb.
LUBEXX     compute ord-giac = prg-giacenza - prg-impegnato.
LUBEXX*****           move prg-scorta          to ord-scorta.
LUBEXX     move art-scorta          to ord-scorta.
           move prg-costo-ultimo    to ord-ultimo-costo.
           perform CALCOLA-COSTO-MP.
LUBEXX*****           add 0,5 to costo-mp  giving costo-mp-2dec.
LUBEXX*****           move 0  to costo-mp-2dec(13:2).
LUBEX******           move costo-mp-2dec       to ord-prz-medio.
LUBEXX     move costo-mp            to ord-prz-medio.
           move art-prezzo-acquisto to ord-prz-acq.
           move mar-scostamento     to ord-scost.
           perform VALORIZZA-OCCURS.
           perform CALCOLA-CONSEGNA-PERIODO.
           perform CALCOLA-PUNTO-RIORDINO.
           perform CALCOLA-FABBISOGNO.
           move user-codi           to ord-utente-ultima-modifica.
           accept ord-ora-ultima-modifica   from time.
           accept ord-data-ultima-modifica  from century-date.
           rewrite ord-rec invalid continue end-rewrite.
           set trovato to true.
LUBEXX*****           if ord-scorta > 0
              set GeneraFileExcel to true.
LUBEXX*****           end-if.

      ***---
       IMBALLO-MAGGIOR-GIACENZA.
           move 0 to giacenza.
           initialize prg-chiave replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move art-codice       to prg-cod-articolo.
           start progmag key >= prg-chiave
                 invalid move art-imballo-standard to SaveImballo
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo  not = art-codice
                       exit perform
                    end-if
                    if prg-cod-magazzino     = "LBX"  and
                       prg-tipo-imballo  not = spaces and
                       prg-peso          not = 0
                       move prg-giacenza to giac-positiva
                       if giac-positiva >= giacenza
                          move prg-giacenza     to giacenza
                          move prg-tipo-imballo to SaveImballo
                       end-if
                    end-if
                 end-perform
           end-start.
           move SaveImballo to prg-tipo-imballo.

      ***---
       VALORIZZA-OCCURS.
           move 1 to idx.
           move 0 to wk-campo.
           initialize occurs-qta.
           move mese-start to mese.
           perform 12 times
              if mese > 12
                 move 1 to mese
              end-if

LUBEXX        if mese-end = 12
                 move ord-qta-past-m(mese) to el-qta(mese)
              else
                 |Superando il mese finale significa
                 |che siamo nell'anno precedente
                 if mese > mese-end
                    move ord-qta-past-m(mese) to el-qta(mese)
                 else
                    move ord-qta-corr-m(mese) to el-qta(mese)
                 end-if
              end-if

              add el-qta(mese) to wk-campo
              perform COMPONI-TITOLO-MESI
              add 1 to mese idx
           end-perform.
           move 0 to ord-media-vend.
           compute ord-media-vend = wk-campo / 12.

      ***---
       CALCOLA-CONSEGNA-PERIODO.
           move 0 to ord-consegna.
           compute wk-campo =
            ( ( ord-media-vend * ord-scost ) / 100 ).
           compute ord-consegna = ( wk-campo / 20 )  * ord-lead-time.
           |ARROTONDAMENTO ALL'UNITA: SONO PEZZI!!!
           add 0,5 to ord-consegna.
           move  0 to ord-consegna(10:2).

      ***---
       CALCOLA-PUNTO-RIORDINO.
           compute wk-campo = 12   *(el-qta(1)  ** 2 + el-qta(2)  ** 2 
                 + el-qta(3)  ** 2 + el-qta(4)  ** 2 + el-qta(5)  ** 2 
                 + el-qta(6)  ** 2 + el-qta(7)  ** 2 + el-qta(8)  ** 2 
                 + el-qta(9)  ** 2 + el-qta(10) ** 2 + el-qta(11) ** 2 
                 + el-qta(12) ** 2) -
                 ( el-qta(1) + el-qta(2)  + el-qta(3) + el-qta(4) + 
                   el-qta(5) + el-qta(6)  + el-qta(7) + el-qta(8) + 
                   el-qta(9) + el-qta(10) + el-qta(11) + el-qta(12) ) 
                   ** 2.

           if wk-campo not > 0
              move 0 to ord-riordino
           else
              compute wk-campo = wk-campo / 132
              compute wk-campo = wk-campo ** ,5
              add ord-consegna   to wk-campo
              move wk-campo      to ord-riordino
              |ARROTONDAMENTO ALL'UNITA: SONO PEZZI!!!
              add 0,5            to ord-riordino
              move  0            to ord-riordino(10:2)
              move art-mag-std   to mag-codice
              read tmagaz no lock invalid continue end-read
              compute ord-riordino = ord-riordino +
                    ( ord-riordino * mag-perce-riordino / 100 )
           end-if.

      ***---
       CALCOLA-FABBISOGNO.
           move 0 to idx.
           initialize ord-fabbisogno-prox-6-mesi.
           |Calcolo il fabbisogno dei sei mesi prossimi, al di là del 
           |mese teorico di scorta (se siamo a Maggio, sarà quindi 
           |da Luglio a Dicembre).
           add 2 to mese-end giving mese.
           perform varying idx from 1 by 1
                     until idx > 6
              if mese > 12 subtract 12 from mese end-if
              move 0 to wk-campo
              if idx = 1
                 if el-qta(mese) > 0
                    compute wk-campo =
                           ord-riordino - ord-giac  +
                         ( el-qta(mese) * ord-scost / 100 )
                 else
                    compute wk-campo = ord-riordino - ord-giac
                 end-if
              else
                 if el-qta(mese) > 0
                    compute wk-campo =
                          ( el-qta(mese) * ord-scost / 100 ) +
                            ord-fabb-qta(idx - 1)
                 else
                    compute wk-campo = ord-fabb-qta(idx - 1)
                 end-if
              end-if
              move wk-campo to ord-fabb-qta(idx)

              add 1 to mese
           end-perform.

      ***---
       GENERA-FILE-EXCEL.
           set tutto-ok to true.
           perform ACCETTA-SEPARATORE.
           open output lineseq.
           if tutto-ok
              move low-value to ord-rec
              start ordfor key is >= k-ord
                    invalid set errori to true
              end-start
              if tutto-ok
                 perform until 1 = 2
                    read ordfor next at end exit perform end-read
                    if prima-volta
                       perform SCRIVI-INTESTAZIONE
                       set prima-volta to false
                    end-if
LUBEXX*****                    if ord-scorta > 0
                       perform SCRIVI-RIGA
LUBEXX*****                    end-if
                 end-perform
LUBEXX*****      Aggiungo TUTTI gli articoli di anagrafica
LUBEXX*****      anche quelli non movimentati come richiesta di M.V.
LUBEXX           perform AGGIUNGI-ARTICOLI-ANAGRAFICA
                 perform CALL-EXCEL
              end-if
           end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           initialize line-riga.
           string "Codice"      delimited size
                  separatore    delimited size
                  "Descrizione" delimited size
                  separatore    delimited size
                  "Imballo"     delimited size
                  separatore    delimited size
                  "Marca"       delimited size
                  separatore    delimited size
                  "Scorta"      delimited size
                  separatore    delimited size
                  "Scostamento" delimited size
                  separatore    delimited size
                  tit-mese(1)   delimited size
                  separatore    delimited size
                  tit-mese(2)   delimited size
                  separatore    delimited size
                  tit-mese(3)   delimited size
                  separatore    delimited size
                  tit-mese(4)   delimited size
                  separatore    delimited size
                  tit-mese(5)   delimited size
                  separatore    delimited size
                  tit-mese(6)   delimited size
                  separatore    delimited size
                  tit-mese(7)   delimited size
                  separatore    delimited size
                  tit-mese(8)   delimited size
                  separatore    delimited size
                  tit-mese(9)   delimited size
                  separatore    delimited size
                  tit-mese(10)  delimited size
                  separatore    delimited size
                  tit-mese(11)  delimited size
                  separatore    delimited size
                  tit-mese(12)  delimited size
                  separatore    delimited size
                  "Tot. anno"   delimited size
                  separatore    delimited size
                  "Media"       delimited size
                  separatore    delimited size
                  "Consegna"    delimited size
                  separatore    delimited size
                  "P. Riordino" delimited size
                  separatore    delimited size
                  "Giacenza"    delimited size
                  separatore    delimited size
                  "1° Mese"     delimited size
                  separatore    delimited size
                  "2° Mese"     delimited size
                  separatore    delimited size
                  "3° Mese"     delimited size
                  separatore    delimited size
                  "4° Mese"     delimited size
                  separatore    delimited size
                  "5° Mese"     delimited size
                  separatore    delimited size
                  "6° Mese"     delimited size
                  separatore    delimited size
                  "Listino"     delimited size
                  separatore    delimited size
                  "Ulimo Costo" delimited size
                  separatore    delimited size
                  "Prz. Medio"  delimited size
                  separatore    delimited size
                  "Altezza"     delimited size
                  separatore    delimited size
                  "Larghezza"   delimited size
                  separatore    delimited size
                  "Profondita"  delimited size
                  separatore    delimited size
                  "Qta EPAL"    delimited size
                  separatore    delimited size
                  "Qta STD"     delimited size
                  separatore    delimited size
                  "Codice EAN 1"delimited size
                  separatore    delimited size
                  "Codice EAN 2"delimited size
                  separatore    delimited size
                  "Codice EAN 3"delimited size
                  separatore    delimited size
                  "Codice EAN 4"delimited size
                  separatore    delimited size
                  "Codice EAN 5"delimited size
                  separatore    delimited size
                  "Descrizione alternativa" delimited size
                  separatore    delimited size
                  "Cod. Art. fornitore"delimited size
                  into line-riga
           end-string.
           write line-riga.
   
      ***---
       SCRIVI-RIGA.
           move 0 to tot-anno.
           initialize line-riga r-riga.
           move ord-articolo to r-articolo art-codice.
           read articoli no lock 
                invalid continue
            not invalid 
                move art-descrizione      to r-des-art
                move art-descrizione-2    to r-alter
                move art-cod-art-frn      to r-cod-forn
                                                
                |17/12/2007 Come da richiesta di Walter: invece di usare
                |l'imballo standard prendo quello con > giacenza assoluta
                perform IMBALLO-MAGGIOR-GIACENZA
      *****          move art-imballo-standard to imq-codice.
                move prg-tipo-imballo to imq-codice

                read timbalqta no lock
                     invalid continue
                 not invalid move imq-qta-imb to r-qta-imb
                end-read
           end-read.

           move ord-marca  to r-marca.
           move ord-scorta to r-scorta.
           move ord-scost  to r-scost.
           move 1          to idx.
           move mese-start to mese.
           perform 12 times
              if mese > 12
                 move 1 to mese
              end-if
LUBEXX        if mese-end = 12
                 move ord-qta-past-m(mese) to r-qta(idx)
                 add  ord-qta-past-m(mese) to tot-anno
              else
                 if mese > mese-end
                    move ord-qta-past-m(mese) to r-qta(idx)
                    add  ord-qta-past-m(mese) to tot-anno
                 else
                    move ord-qta-corr-m(mese) to r-qta(idx)
                    add  ord-qta-corr-m(mese) to tot-anno
                 end-if
              end-if
              add 1 to mese idx
           end-perform.
           move tot-anno         to r-tot-anno.
           move ord-media-vend   to r-media.
           move ord-consegna     to r-consegna.
           move ord-riordino     to r-riordino.
           move ord-giac         to r-giacenza.
           move ord-fabb-qta(1)  to r-fabb(1).
           move ord-fabb-qta(2)  to r-fabb(2).
           move ord-fabb-qta(3)  to r-fabb(3).
           move ord-fabb-qta(4)  to r-fabb(4).
           move ord-fabb-qta(5)  to r-fabb(5).
           move ord-fabb-qta(6)  to r-fabb(6).
           move ord-prz-acq      to r-listino.
           move ord-ultimo-costo to r-ul-costo.
           move ord-prz-medio    to r-prz-medio.
           move art-altezza      to r-altezza.
           move art-larghezza    to r-larghezza.
           move art-profondita   to r-profondita.
           move art-qta-epal     to r-qta-epal.
           move art-qta-std      to r-qta-std.
           if art-codice-ean-1 not = 0
              move art-codice-ean-1 to r-codice-ean-1
           end-if.
           if art-codice-ean-2 not = 0
              move art-codice-ean-2 to r-codice-ean-2
           end-if.
           if art-codice-ean-3 not = 0
              move art-codice-ean-3 to r-codice-ean-3
           end-if.
           if art-codice-ean-4 not = 0
              move art-codice-ean-4 to r-codice-ean-4
           end-if.
           if art-codice-ean-5 not = 0
              move art-codice-ean-5 to r-codice-ean-5
           end-if.
           perform COMPONI-RIGA.

      ***---
       COMPONI-RIGA.
           string r-articolo       delimited size
                  separatore       delimited size
                  r-des-art        delimited size
                  separatore       delimited size
                  r-qta-imb        delimited size
                  separatore       delimited size
                  r-marca          delimited size
                  separatore       delimited size
                  r-scorta         delimited size
                  separatore       delimited size
                  r-scost          delimited size
                  separatore       delimited size
                  r-qta(1)         delimited size
                  separatore       delimited size
                  r-qta(2)         delimited size
                  separatore       delimited size
                  r-qta(3)         delimited size
                  separatore       delimited size
                  r-qta(4)         delimited size
                  separatore       delimited size
                  r-qta(5)         delimited size
                  separatore       delimited size
                  r-qta(6)         delimited size
                  separatore       delimited size
                  r-qta(7)         delimited size
                  separatore       delimited size
                  r-qta(8)         delimited size
                  separatore       delimited size
                  r-qta(9)         delimited size
                  separatore       delimited size
                  r-qta(10)        delimited size
                  separatore       delimited size
                  r-qta(11)        delimited size
                  separatore       delimited size
                  r-qta(12)        delimited size
                  separatore       delimited size
                  r-tot-anno       delimited size
                  separatore       delimited size
                  r-media          delimited size
                  separatore       delimited size
                  r-consegna       delimited size
                  separatore       delimited size
                  r-riordino       delimited size
                  separatore       delimited size
                  r-giacenza       delimited size
                  separatore       delimited size
                  r-fabb(1)        delimited size
                  separatore       delimited size
                  r-fabb(2)        delimited size
                  separatore       delimited size
                  r-fabb(3)        delimited size
                  separatore       delimited size
                  r-fabb(4)        delimited size
                  separatore       delimited size
                  r-fabb(5)        delimited size
                  separatore       delimited size
                  r-fabb(6)        delimited size
                  separatore       delimited size
                  r-listino        delimited size
                  separatore       delimited size
                  r-ul-costo       delimited size
                  separatore       delimited size
                  r-prz-medio      delimited size
                  separatore       delimited size
                  r-altezza        delimited size
                  separatore       delimited size
                  r-larghezza      delimited size
                  separatore       delimited size
                  r-profondita     delimited size
                  separatore       delimited size
                  r-qta-epal       delimited size
                  separatore       delimited size
                  r-qta-std        delimited size
                  separatore       delimited size
                  r-codice-ean-1   delimited size
                  separatore       delimited size
                  r-codice-ean-2   delimited size
                  separatore       delimited size
                  r-codice-ean-3   delimited size
                  separatore       delimited size
                  r-codice-ean-4   delimited size
                  separatore       delimited size
                  r-codice-ean-5   delimited size
                  separatore       delimited size
                  r-alter          delimited size
                  separatore       delimited size
                  r-cod-forn       delimited size
                  into line-riga
           end-string.
           write line-riga.

LUBEXX***---
       AGGIUNGI-ARTICOLI-ANAGRAFICA.
           move 0 to counter counter2.
           display "                              "
              upon link-handle at column 22 line 03.
           move low-value to art-rec.
           start articoli key >= art-chiave
                 invalid continue
           end-start.
           perform until 1 = 2

              add 1 to counter
              add 1 to counter2
              if counter2 = 100
                 move counter to counter-edit
                 display counter-edit
                    upon link-handle at column 22
                                          line 03
                 move 0 to counter2
              end-if

              read articoli next  at end exit perform end-read
              move link-magazzino to ord-mag
              move art-codice     to ord-articolo
              read ordfor no lock 
                   invalid 
                   initialize line-riga 
                   initialize r-riga replacing numeric data by zeroes
                                          alphanumeric data by spaces
                   move art-codice           to r-articolo
                   move art-descrizione      to r-des-art
                   move art-descrizione-2    to r-alter
                   move art-cod-art-frn      to r-cod-forn
                                                   
                   |17/12/2007 Come da richiesta di Walter: invece di usare
                   |l'imballo standard prendo quello con > giacenza assoluta
                   perform IMBALLO-MAGGIOR-GIACENZA
      *****             move art-imballo-standard to imq-codice.
                   move prg-tipo-imballo to imq-codice

                   read timbalqta no lock
                        invalid continue
                    not invalid move imq-qta-imb to r-qta-imb
                   end-read
                   move art-marca-prodotto to mar-codice
                   read tmarche no lock
                        invalid continue
                    not invalid
                        move mar-scostamento to r-scost
                   end-read
                   move art-codice to ord-articolo
                   perform TROVA-GIACENZA
                   move ord-articolo to prg-cod-articolo
                   move spaces       to prg-cod-magazzino
                   move spaces       to prg-tipo-imballo
                   move 0            to prg-peso
                   read progmag no lock 
                        invalid continue
                    not invalid
                        compute ord-giac = prg-giacenza - prg-impegnato
                        move prg-costo-ultimo to r-ul-costo
                        perform CALCOLA-COSTO-MP
                        move costo-mp to r-prz-medio
                   end-read
                   move art-marca-prodotto  to r-marca
                   move art-scorta          to r-scorta
                   move ord-giac            to r-giacenza
                   move art-prezzo-acquisto to r-listino
                   move art-altezza         to r-altezza
                   move art-larghezza       to r-larghezza
                   move art-profondita      to r-profondita
                   move art-qta-epal        to r-qta-epal
                   move art-qta-std         to r-qta-std
                   if art-codice-ean-1 not = 0
                      move art-codice-ean-1 to r-codice-ean-1
                   end-if
                   if art-codice-ean-2 not = 0
                      move art-codice-ean-2 to r-codice-ean-2
                   end-if
                   if art-codice-ean-3 not = 0
                      move art-codice-ean-3 to r-codice-ean-3
                   end-if
                   if art-codice-ean-4 not = 0
                      move art-codice-ean-4 to r-codice-ean-4
                   end-if
                   if art-codice-ean-5 not = 0
                      move art-codice-ean-5 to r-codice-ean-5
                   end-if
                   move 0 to r-qta(1)  
                   move 0 to r-qta(2)
                   move 0 to r-qta(3)
                   move 0 to r-qta(4)
                   move 0 to r-qta(5)
                   move 0 to r-qta(6)
                   move 0 to r-qta(7)
                   move 0 to r-qta(8)
                   move 0 to r-qta(9)
                   move 0 to r-qta(10)
                   move 0 to r-qta(11)
                   move 0 to r-qta(12)
                   move 0 to r-tot-anno
                   move 0 to r-media
                   move 0 to r-consegna
                   move 0 to r-riordino
                   move 0 to r-fabb(1) 
                   move 0 to r-fabb(2)
                   move 0 to r-fabb(3)
                   move 0 to r-fabb(4)
                   move 0 to r-fabb(5)
                   move 0 to r-fabb(6)
                   perform COMPONI-RIGA
              end-read
           end-perform.

      ***---
       COMPONI-TITOLO-MESI.
      *****     evaluate mese
      *****     when 1   move "Gen" to tit-mese(idx)
      *****     when 2   move "Feb" to tit-mese(idx)
      *****     when 3   move "Mar" to tit-mese(idx)
      *****     when 4   move "Apr" to tit-mese(idx)
      *****     when 5   move "Mag" to tit-mese(idx)
      *****     when 6   move "Giu" to tit-mese(idx)
      *****     when 7   move "Lug" to tit-mese(idx)
      *****     when 8   move "Ago" to tit-mese(idx)
      *****     when 9   move "Set" to tit-mese(idx)
      *****     when 10  move "Ott" to tit-mese(idx)
      *****     when 11  move "Nov" to tit-mese(idx)
      *****     when 12  move "Dic" to tit-mese(idx)
      *****     end-evaluate.

           initialize tit-mese(idx).
           evaluate mese
           when 1   move "Mese1"  to tit-mese(idx)
           when 2   move "Mese2"  to tit-mese(idx)
           when 3   move "Mese3"  to tit-mese(idx)
           when 4   move "Mese4"  to tit-mese(idx)
           when 5   move "Mese5"  to tit-mese(idx)
           when 6   move "Mese6"  to tit-mese(idx)
           when 7   move "Mese7"  to tit-mese(idx)
           when 8   move "Mese8"  to tit-mese(idx)
           when 9   move "Mese9"  to tit-mese(idx)
           when 10  move "Mese10" to tit-mese(idx)
           when 11  move "Mese11" to tit-mese(idx)
           when 12  move "Mese12" to tit-mese(idx)
           end-evaluate.

      ***---
       CLOSE-FILES.
           close progmag
                 articoli
                 tmarche
                 ordfor
                 timbalqta
                 rlistini
                 tmagaz.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
       copy "costo-medio.cpy".

      ***--- |DUMMY NON TOCCARE
       RECUPERO-ANAGRAFICA.

      ***---
       TROVA-GIACENZA.
           move 0 to como-giacenza.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo not = ord-articolo
                       exit perform 
                    end-if
                    if prg-cod-magazzino not = spaces
                       move prg-cod-magazzino to mag-codice
                       read tmagaz no lock
                       if mag-per-promo-si
                          add prg-giacenza to como-giacenza
                       end-if
                    end-if
                 end-perform
           end-start.
