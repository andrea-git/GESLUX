       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      aggordfor-old-p.
       AUTHOR.                          Andrea.
       REMARKS. Motore per la lavorazione sul file ordfor-OLD. Se in linkage
                vengono passate DUE date allora significa che devo
                CONSOLIDARE, altrimenti lavoro sottraendo 1 all'anno
                che mi viene passato nella data e parto dal 1° Gennaio
                di quell'anno fino alla data stessa.
                Se l'anno che leggo è = all'anno - 1 aggiorno i dati
                "past" altrimenti quelli "corr".
                SOLAMENTE PER MAGAZZINO LBX COME RICHIESTA DI M. VINCENZI!!!

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "ordfor-old.sl".
           copy "tmarche.sl".
           copy "articoli.sl".
           copy "progmag.sl".
           copy "tcaumag.sl".
           copy "timbalqta.sl".


      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "ordfor-old.fd".
           copy "tmarche.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "tcaumag.fd".
           copy "timbalqta.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "recupero-addizionale.def".

      * COSTANTI
       78  titolo value "Aggiornamento file ordini a fornitori".

      * FILE-STATUS
       77  status-tmovmag           pic xx.
       77  status-rmovmag           pic xx.
       77  status-ordfor-old        pic xx.
       77  status-tmarche           pic xx.
       77  status-articoli          pic xx.
       77  status-progmag           pic xx.
       77  status-tcaumag           pic xx.
       77  status-timbalqta         pic xx.

      * VARIABILI
       77  mese                     pic 99.
       77  anno                     pic 9(4).
       77  anno-past                pic 9(4).
       77  counter                  pic 9(10).
       77  counter2                 pic 9(10).
       77  counter-edit             pic z(10).
       77  PgmChiamante             pic x(20).
       77  idx                      pic 9(3).
       77  como-qta                 pic 9(12).

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
         88 aggiorna-mese-corrente  value 1.
         88 aggiorna-anno-mese      value 2.

      *****************************************************************

       LINKAGE SECTION.
       77  link-user                pic x(10).
      * Data di consolidamento che, nel caso di consolidamento
      * è già aggiornata al mese successivo dal pgm. chiamante
       77  link-data                pic 9(8). 
       77  start-data               pic 9(8).
       77  link-magazzino           pic x(3).
       77  link-result              pic 9.
       77  scr-oper-handle          handle of window.

      *****************************************************************

       PROCEDURE DIVISION USING link-user,
                                link-data,
                                start-data,
                                link-magazzino,
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
       ordfor-old-ERR SECTION.
           use after error procedure on ordfor-old.
           set tutto-ok  to true.
           evaluate status-ordfor-old
           when "35"
                set errori to true
                display message "File [ordfor-old] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [ordfor-old] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ordfor-old] Indexed file corrupt!"
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
                move   "ordfor-old"     to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o ordfor-old allowing readers
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
       TIMBALQTA-ERR SECTION.
           use after error procedure on timbalqta.
           set tutto-ok  to true.
           evaluate status-timbalqta
           when "35"
                set errori to true
                display message "File [TIMBALQTA] not found!"
                          title titolo
                           icon 3
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
              if aggiorna-mese-corrente
                 add 1 to start-data
                 perform ELABORAZIONE
              else
                 perform RIGENERA-FILE
              end-if
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           call "C$CALLEDBY" using PgmChiamante.
           move 0 to counter counter2.
           set  trovato        to false.
           set  tutto-ok       to true.
           move link-data(1:4) to anno.
           if   start-data not = 0 set aggiorna-mese-corrente to true
           else                    set aggiorna-anno-mese     to true
           end-if.
           move 0 to anno-past.

      ***---
       OPEN-FILES.
           open i-o ordfor-old allowing readers.
           if tutto-ok
              open input progmag
                         articoli
                         tmarche
                         tmovmag
                         rmovmag
                         tcaumag
                         timbalqta
              if tutto-ok
                 if aggiorna-anno-mese
                    close       ordfor-old
                    open output ordfor-old
                    close       ordfor-old
                    open i-o    ordfor-old allowing readers
                 end-if
                 if errori
                    close progmag
                          articoli
                          tmarche
                          tmovmag
                          rmovmag
                          tcaumag
                          timbalqta
                 end-if
              else
                 close ordfor-old
              end-if

           end-if.

           if errori goback end-if.

      ***---
       ELABORAZIONE.
           move low-value  to tmo-rec.
           move start-data to tmo-data-movim.
           start tmovmag key is >= k-data
                 invalid set errori to true
           end-start.

           if tutto-ok

              if PgmChiamante = "aggmese"
                 |RIPULISCO LA SCREEN DAL CONTATORE
                 display "                          "
                    upon scr-oper-handle at column 34
                                              line 25
                 display "                          "
                    upon scr-oper-handle at column 30
                                              line 26
                 ||||||||
              end-if

              perform until 1 = 2

                 read tmovmag next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 50
                    move counter to counter-edit 
                    if PgmChiamante = "aggmese"
                       display counter-edit
                          upon scr-oper-handle at column 34
                                                    line 25
                       if counter = 50
                          display "AGGORDFO COUNTER"
                             upon scr-oper-handle at column 30
                                                       line 26
                       end-if
                    else
                       display counter-edit
                          upon scr-oper-handle at column 23
                                                    line 03
                    end-if
                    move 0 to counter2
                 end-if

                 if tmo-data-movim > link-data exit perform end-if

                 if aggiorna-anno-mese
                    if tmo-data-movim(1:4) = anno-past 
                       set PreviousYear to true
                    else
                       set CurrentYear  to true
                    end-if
                 else
                    set CurrentYear to true
                 end-if

                 move tmo-data-movim(5:2) to mese

                 if tmo-cliente
                    move tmo-causale to tca-codice
                    read tcaumag no lock invalid continue end-read
                    if tca-cliente 
LUBEXX                 if tca-si-stat
                          if tca-movim-ven-pos      or
                             tca-movim-ven-neg      or
                             tca-movim-resi-for-pos or
                             tca-movim-resi-for-neg
LUBEXX                       |Richiesta di Vincenzi M. SOLO MAGAZZINO LUBEX
LUBEXX                       |Dal pgm. chiamante valorizzo la variabile
LUBEXX                       |di linkage col codice del magazzino principale
LUBEXX                       if tca-cod-magaz = link-magazzino
                                perform LOOP-RIGHE-ORDINE
LUBEXX                       end-if
                          end-if
LUBEXX                 end-if
                    end-if
                 end-if

              end-perform
           end-if.

           if not trovato move 0 to link-result
           else           move 1 to link-result
           end-if.

      ***---
       LOOP-RIGHE-ORDINE.
           set  tutto-ok   to true.
           move low-value  to rmo-rec.
           move tmo-chiave to rmo-chiave.
           start rmovmag key is >= rmo-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rmovmag next at end exit perform end-read
                 if rmo-anno  not = tmo-anno or
                    rmo-movim not = tmo-numero
                    exit perform
                 end-if

                 |Serve per non considerare le NCPZ 
                 |e le variazioni di valore
                 if rmo-qta not = 0
                    set record-ok     to false
                    move rmo-articolo to art-codice

                    move rmo-qta to como-qta

                    perform AGGIORNA-RECORD
                 end-if


              end-perform
           end-if.

      ****---
       AGGIORNA-RECORD.
           read articoli no  lock
                invalid  continue
            not invalid
                move rmo-chiave-progmag to prg-chiave
                read progmag no lock
                     invalid continue
                 not invalid
                     set record-ok to true
                     move art-imballo-standard to imq-codice
                     read timbalqta no lock
                          invalid move spaces to imq-qta-imb
                     end-read
                end-read
           end-read.
           if record-ok
              move rmo-codmag to ord-mag-old
              move art-codice to ord-articolo-old
              read ordfor-old 
                   invalid
                   initialize ord-dati-old
                              replacing numeric data by zeroes
                                   alphanumeric data by spaces
                   move art-marca-prodotto to ord-marca-old
                   move imq-qta-imb        to ord-qta-imb-old
                   move link-user          to ord-utente-creazione-old
                   accept ord-ora-creazione-old  from time
                   accept ord-data-creazione-old from century-date
               not invalid
                   move link-user    to ord-utente-ultima-modifica-old
                   accept ord-ora-ultima-modifica-old  from time
                   accept ord-data-ultima-modifica-old
                     from century-date
              end-read
              if CurrentYear  add como-qta to ord-qta-corr-m-old(mese)
              else            add como-qta to ord-qta-past-m-old(mese)
              end-if
              write ord-rec-old invalid rewrite ord-rec-old end-write
              set trovato to true
           end-if.

      ***---
       RIGENERA-FILE.
           move low-value  to tmo-rec.
           subtract 1 from anno giving anno-past.
           string anno-past delimited size
                  "01"      delimited size
                  "01"      delimited size
                  into start-data
           end-string.
           perform ELABORAZIONE.

      ***---
       CLOSE-FILES.
           close progmag
                 tmovmag
                 rmovmag
                 articoli
                 tcaumag
                 tmarche
                 ordfor-old
                 timbalqta.

      ***---
       EXIT-PGM.
           goback.
