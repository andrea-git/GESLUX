       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      inveday-p.
       AUTHOR.                          Andrea.
       REMARKS. 
           Partendo dalla giacenza CONSOLIDATA, vengono sommate le
           quantità dei movimenti dalla data di consolidamento a 
           quella indicata. Questo valore è la giacenza DAY che sarà
           utilizzata per l'esposizione.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "articoli.sl".
           copy "progmag.sl".
           copy "tmp-inveday.sl".
           copy "tmarche.sl".
           copy "tcaumag.sl".
           copy "timbalqta.sl".
           copy "timballi.sl".
           copy "timposte.sl".
           copy "rlistini.sl".
           copy "tlistini.sl".
           copy "tscorte.sl".
           copy "impforn.sl".
           copy "distinteb.sl".
           copy "clienti.sl".
           copy "destinif.sl".
           copy "tpiombo.sl".
           copy "param.sl".
           copy "tparamge.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "tmp-inveday.fd".
           copy "tmarche.fd".
           copy "tcaumag.fd".
           copy "timbalqta.fd".
           copy "timballi.fd".
           copy "timposte.fd".
           copy "rlistini.fd".
           copy "tlistini.fd".
           copy "tscorte.fd".
           copy "impforn.fd".  
           copy "distinteb.fd".  
           copy "clienti.fd".
           copy "destinif.fd".
           copy "tpiombo.fd". 
           copy "param.fd".   
           copy "tparamge.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".
       copy "costo-medio.def".       
       copy "imposte.def".  
       copy "prz-finito-forn.def".              
       copy "trova-parametro.def".

       77 imposta-cobat-conf    PIC  9(10)v9999.
       77 imposta-cou-conf      PIC  9(10)v9999.
       77 imposta-consumo-conf  PIC  9(10)v9999.
       77 add-piombo-conf       PIC  9(10)v9999.
       77 add-piombo-5dec       PIC  9(10)v99999.

       78  titolo                value "Stampa Inventario DAY".
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  scelta                pic 9.
       77  idx                   pic 999 value 0.
       77  save-data             pic 9(8) value 0.

       77  status-articoli       pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-lineseq        pic xx.
       77  status-progmag        pic xx.
       77  status-tmp-inveday    pic xx.
       77  status-tmarche        pic xx.
       77  status-tcaumag        pic xx.
       77  status-timbalqta      pic xx.
       77  status-timballi       pic xx.
       77  status-timposte       pic xx.
       77  status-rlistini       pic xx.
       77  status-tlistini       pic xx.
       77  status-tscorte        pic xx.
       77  status-impforn        pic xx.
       77  status-distinteb      pic xx.
       77  status-clienti        pic xx.
       77  status-destinif       pic xx.
       77  status-tpiombo        pic xx.
       77  status-param          pic xx.
       77  status-tparamge       pic xx.

       77  path-csv              pic x(256).
       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).
       77  tipo-costo            pic x(9).
       77  SaveMarca             pic 9(5) value 0.
       77  SaveMagazzino         pic x(3) value spaces.
       77  prg-cod-articolo-edit pic z(6).
       77  prg-peso-edit         pic zz9,999.
       77  imballi-ed            pic z.zz9.
       77  user-codi             pic x(10).

       77  tinv-qta-edit         pic ---.---.--9.
       77  tinv-prezzo-edit      pic ----.---.---.--9,99.
       77  tinv-valore-edit      pic ----.---.---.--9,99.
       77  tinv-kg-edit          pic ----.---.---.--9,999.

       77  TotValore             pic s9(12)v99 value 0.
       77  TotValoreEdit         pic ----.---.---.--9,99.
      
       77  TotValoreMag          pic s9(12)v99 value 0.
       77  TotValoreMagEdit      pic ----.---.---.--9,99.
      
       77  TotUtf                pic s9(9)v999 value 0.
       77  TotUtfEdit            pic ----.---.--9,999.
      
       77  TotUtfMag             pic s9(9)v999 value 0.
       77  TotUtfMagEdit         pic ----.---.--9,999.
      
       77  TotUtfGen             pic s9(9)v999 value 0.
       77  TotUtfGenEdit         pic ----.---.--9,999.

       77  TotNonUtf             pic s9(9)v999 value 0.
       77  TotNonUtfEdit         pic ----.---.--9,999.

       77  TotNonUtfMag          pic s9(9)v999 value 0.
       77  TotNonUtfMagEdit      pic ----.---.--9,999.

       77  TotNonUtfGen          pic s9(9)v999 value 0.
       77  TotNonUtfGenEdit      pic ----.---.--9,999.

       77  TotKg                 pic s9(9)v999 value 0.
       77  TotKgEdit             pic ----.---.--9,999.

       77  TotKgMag              pic s9(9)v999 value 0.
       77  TotKgMagEdit          pic ----.---.--9,999.

       77  TotValoreGen          pic s9(12)v99 value 0.
       77  TotValoreGenEdit      pic ----.---.---.--9,99.

       77  tipo-elab             pic 9.

       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88  prima-volta       value 1, false 0.
       77  filler                pic 9.
           88  record-ok         value 1, false 0.
       77  filler                pic 9.
           88  trovato-movim     value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       LINKAGE SECTION.
       copy "link-inveday.def".

      ******************************************************************
       PROCEDURE DIVISION using inveday-linkage.

       DECLARATIVES.
      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                display message "File [TMOVMAG] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TMOVMAG] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TMOVMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message "File [RMOVMAG] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [RMOVMAG] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[RMOVMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
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
                display message "File [ARTICOLI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
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
                display message "File [TMARCHE] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TMARCHE] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "File [TCAUMAG] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TCAUMAG] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TCAUMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
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
                display message "File [TIMBALQTA] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TIMBALQTA] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TIMBALLI-ERR SECTION.
           use after error procedure on timballi.
           set tutto-ok  to true.
           evaluate status-timballi
           when "35"
                display message "File [TIMBALLI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TIMBALLI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TIMBALLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
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
                display message "File [PROGMAG] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[PROGMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TMP-INVEDAY-ERR SECTION.
           use after error procedure on tmp-inveday.
           set tutto-ok  to true.
           evaluate status-tmp-inveday
           when "39"
                set errori to true
                display message "File mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
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
                     perform OPEN-OUTPUT-TMP-INVEDAY
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "39"
                set errori to true
                display message "File mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
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
           end-evaluate
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
      *-
           move inveday-user to user-codi.
           evaluate true                                  
           when inveday-ultimo    move "Ultimo"    to tipo-costo
           when inveday-medio     move "Medio"     to tipo-costo
           when inveday-confronto move "Confronto" to tipo-costo
           end-evaluate.

           accept como-data from century-date.
           accept como-ora  from time.
           perform ACCETTA-SEPARATORE.
           initialize path-tmp wstampa.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  inveday-path from environment "PATH-ST".
           inspect inveday-path replacing trailing spaces by low-value.
           inspect user-codi    replacing trailing spaces by low-value.
           string  inveday-path delimited low-value
                   "inveday"    delimited size
                   "_"          delimited size
                   user-codi    delimited low-value
                   "_"          delimited size
                   como-data    delimited size
                   "_"          delimited size
                   como-ora     delimited size
                   ".tmp"       delimited size
                   into path-tmp
           end-string.
           string  inveday-path delimited low-value
                   "inveday"    delimited size
                   "_"          delimited size
                   user-codi    delimited low-value
                   ".csv"       delimited size
                   into path-csv
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-TMP-INVEDAY.
           if tutto-ok
              perform OPEN-IO-PROGMAG
              if tutto-ok
                 open input tmovmag
                            rmovmag
                            articoli
                            tmarche
                            tcaumag
                            timbalqta
                            timballi
                            timposte
                            rlistini
                            impforn
                            tscorte
                            distinteb
                            clienti 
                            destinif
                            tpiombo
                            param
                            tparamge
                            tlistini
              else
                 close tmp-inveday
                 delete file tmp-inveday
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-TMP-INVEDAY.
           open output tmp-inveday.
           if tutto-ok
              close tmp-inveday
              open i-o tmp-inveday
           end-if.
           
      ***---
       OPEN-IO-PROGMAG.
           initialize geslock-messaggio.
           string   "Il file dei progressivi di magazzino"
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità"
             x"0d0a""ad aggiornarne la giacenza." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           open i-o progmag.
           if RecLocked
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move   "progmag"    to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-PROGMAG
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           perform AGGIORNA-GIAC-DAY-ON-PROGMAG.
           if tutto-ok
              perform LOOP-TMOVMAG

              close tmp-inveday
              open input tmp-inveday
              perform OPEN-OUTPUT-LINESEQ
              if tutto-ok perform GENERA-FILE-EXCEL end-if

           else
              close       tmp-inveday
              delete file tmp-inveday
           end-if.

      ***---
       AGGIORNA-GIAC-DAY-ON-PROGMAG.
           move low-value    to    prg-rec.
           start progmag key is >= prg-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 set record-ok to false
                 read progmag next no lock at end exit perform end-read

                 |Lo calcolo comunque sul padre come da
                 |richiesta di Trivella in data 04/01/06
                 if prg-cod-magazzino = spaces and
                    prg-tipo-imballo  = spaces and
                    prg-peso          = 0
                    perform CALCOLA-COSTO-MP-COMPLETO
                 end-if

                 evaluate true
                 when inveday-figlio
                      if prg-cod-magazzino not = spaces and
                         prg-tipo-imballo  not = spaces and
                         prg-peso          not = 0
                         set record-ok to true
                      end-if

                      if record-ok
                         if inveday-mag          not = spaces
                            if prg-cod-magazzino not = inveday-mag
                               set record-ok to false
                            end-if
                         end-if
                      end-if

                 when inveday-padre
                      if prg-cod-magazzino = spaces and
                         prg-tipo-imballo  = spaces and
                         prg-peso          = 0
                         set record-ok to true
                      end-if
                 end-evaluate

                 if record-ok
                    move prg-cod-articolo to art-codice
                    read articoli no lock
                         invalid
                         display message "** GRAVE ERRORE **"
                         x"0d0a""Articolo: ", art-codice " non trovato"
                                   title titolo
                         set errori to true
                         exit perform
                    end-read

                    if inveday-marca not = 0
                       if art-marca-prodotto not = inveday-marca
                          set record-ok to false
                       end-if
                    end-if

                 end-if

                 if record-ok
                    move 1 to tipo-elab
                    perform READ-PROGMAG-LOCK-UPDATE
                    if errori exit perform end-if

                    initialize tinv-rec replacing numeric data by zeroes
                                             alphanumeric data by spaces
                    move prg-chiave          to tinv-chiave
                    move art-unita-di-misura to tinv-um
                    move art-marca-prodotto  to tinv-marca
                    move prg-giac-day        to tinv-qta

                    if inveday-figlio
                       perform DESCRIZIONE-IMBALLO
                       move prg-peso to tinv-peso tinv-peso-articolo

LUBEXX                 if prg-peso-utf = 0
LUBEXX                    move "*" to tinv-asterisk
LUBEXX                 end-if

                    else
                       move art-descrizione to tinv-art-descrizione
                       |Se lavoro coi padri non devo valorizzare il peso

LUBEXX                 |Prendo il peso dall-articolo
LUBEXX                 compute tinv-peso-articolo = 
LUBEXX                         prg-peso-utf + prg-peso-non-utf

LUBEXX                 if prg-peso-utf = 0
LUBEXX                    move "*" to tinv-asterisk
LUBEXX                 end-if

                    end-if

                    evaluate true
                    when inveday-ultimo
                         move art-prezzo-vendita to tinv-prezzo
                    when inveday-medio
      *****                 perform CALCOLA-COSTO-MP
                         add 0,005 to costo-mp giving costo-mp-2dec
                         move costo-mp-2dec        to tinv-prezzo
                    when inveday-confronto 
                         if costo-mp-2dec not = 0
                            move costo-mp-2dec to tinv-prezzo
                         else
                            perform CERCA-LISTINO
                            move prz-confronto to tinv-prezzo
                         end-if
                    end-evaluate

                    move prg-peso-utf     to tinv-utf
                    move prg-peso-non-utf to tinv-non-utf

                    compute tinv-valore      = tinv-prezzo  * tinv-qta

                    compute tinv-tot-utf     = tinv-utf     * tinv-qta

                    compute tinv-tot-non-utf = tinv-non-utf * tinv-qta

                    compute tinv-tot-peso      =
                            tinv-peso-articolo * tinv-qta

                    write tinv-rec invalid rewrite tinv-rec end-write

                 end-if

              end-perform
           end-if.

      ***---
       CERCA-LISTINO.
           move 0                to save-data.
           move low-value        to rlis-rec.
           move prg-cod-articolo to rlis-articolo.
           start rlistini key >= rlis-k-art
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rlistini next at end exit perform end-read
                    if rlis-articolo not = prg-cod-articolo
                       exit perform 
                    end-if
                    if rlis-ini-val  <= como-data and
                       rlis-fine-val >= como-data   
                       if rlis-ini-val > save-data
                          set cli-tipo-F to true
                          move rlis-fornitore to cli-codice desf-codice
                          move rlis-destino   to desf-prog
                          read clienti  no lock 
                               invalid continue 
                          end-read
                          read destinif no lock 
                               invalid 
                               move cli-nazione to desf-nazione
                          end-read
                          move rlis-codice to tlis-codice
                          read tlistini no lock                
                          move tlis-trasp-f to como-trasporto-f
                          move tlis-trasp-c to como-trasporto-c
                          perform CALCOLA-PRZ-FINITO
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       DESCRIZIONE-IMBALLO.
           move prg-tipo-imballo to imq-codice.
           read timbalqta
               invalid continue
           not invalid
               move imq-tipo to imb-codice
               read timballi no lock invalid continue end-read
           end-read.
           inspect art-descrizione replacing trailing spaces 
                                                by low-value.
           inspect imb-descrizione replacing trailing spaces 
                                                by low-value.
           initialize tinv-art-descrizione.
           move imq-qta-imb       to imballi-ed.
           call "C$JUSTIFY"    using imballi-ed, "L".
           string art-descrizione delimited low-value
                  " - "           delimited size
                  imb-descrizione delimited low-value
                  " da "          delimited size
                  imballi-ed      delimited spaces
                  " x "           delimited size
                  art-udm-imballo delimited size
                  into tinv-art-descrizione
           end-string.

      ***---
       LOOP-TMOVMAG.
           move low-value       to tmo-rec.
           add  1               to inveday-da-data.
           move inveday-da-data to tmo-data-movim.
           start tmovmag key is > k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag next at end exit perform end-read
                    if tmo-data-movim > inveday-a-data
                       exit perform
                    end-if
                    perform LOOP-RMOVMAG
                 end-perform
           end-start.

      ***---
       LOOP-RMOVMAG.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           move low-value  to rmo-riga.
           start rmovmag key is >= rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno  not = tmo-anno or
                       rmo-movim not = tmo-numero
                       exit perform 
                    end-if
                    initialize tinv-chiave
                    if inveday-padre
                       move rmo-articolo       to tinv-articolo
                    else
                       move rmo-chiave-progmag to tinv-chiave
                    end-if
                    read tmp-inveday no lock
                         invalid continue
                     not invalid
                         move rmo-causale to tca-codice
                         read tcaumag no lock
                              invalid
                              display message "** GRAVE ERRORE **"
                    x"0d0a""CAUSALE: ", tca-codice, " NON TROVATA"
                                        title titolo
                                         icon 3
                          not invalid
                              evaluate true
                              when tca-movim-giac-periodo-pos
                                   add      rmo-qta   to tinv-qta
                              when tca-movim-giac-periodo-neg
                                   subtract rmo-qta from tinv-qta
                              end-evaluate
                              compute tinv-valore   =
                                      tinv-prezzo   * tinv-qta

LUBEXX*****                   |Devo utilizzare il peso della riga
      *****                        compute tinv-tot-peso =
      *****                                rmo-peso * tinv-qta
      *****
      *****                        compute tinv-tot-utf  =
      *****                          (rmo-peso-tot-utf / rmo-qta)  * tinv-qta
      *****
      *****                        compute tinv-tot-non-utf =
      *****                          (rmo-peso-tot / rmo-qta)  * tinv-qta

                              compute tinv-tot-peso =
                                      tinv-peso-articolo * tinv-qta

                              compute tinv-tot-utf  =
                                      tinv-utf      * tinv-qta
      
                              compute tinv-tot-non-utf = 
                                      tinv-non-utf  * tinv-qta

                              rewrite tinv-rec
                              move 2 to tipo-elab
                              move tinv-chiave to prg-chiave
                              perform READ-PROGMAG-LOCK-UPDATE
                         end-read
                    end-read
                 end-perform
           end-start.

      ***---
       OPEN-OUTPUT-LINESEQ.
           move path-csv to wstampa.
           open output lineseq.

      ***---
       GENERA-FILE-EXCEL.
           move low-value to tinv-rec.
           start tmp-inveday key is >= k-ord invalid continue end-start.
           perform until 1 = 2
              read tmp-inveday next
                   at end
                   perform TOTALI-MARCA
                   perform TOTALI-MAGAZZINO
                   perform TOTALI-GENERALI
                   exit perform 
              end-read

              if SaveMarca = 0
                 move tinv-marca to SaveMarca
              end-if

              if SaveMagazzino = spaces
                 move tinv-mag to SaveMagazzino
              end-if

              if prima-volta
                 perform SCRIVI-INTESTAZIONE
              end-if

              if tinv-mag not = SaveMagazzino
                 perform TOTALI-MARCA
                 perform TOTALI-MAGAZZINO
              end-if

              if tinv-marca not = SaveMarca
                 perform TOTALI-MARCA
              end-if

              if tinv-qta not = 0

                 initialize line-riga
                 move tinv-qta      to tinv-qta-edit
                 move tinv-prezzo   to tinv-prezzo-edit
                 move tinv-valore   to tinv-valore-edit
                 move tinv-tot-peso to tinv-kg-edit

                 if inveday-padre
                    string tinv-articolo
                           separatore
                           tinv-asterisk
                           separatore
                           tinv-art-descrizione
                           separatore
                           tinv-um
                           separatore
                           tinv-qta-edit
                           separatore
                           tinv-prezzo-edit
                           separatore
                           tinv-valore-edit
                           separatore
                           tinv-kg-edit delimited by size
                           into line-riga
                    end-string
                 else
                    string tinv-articolo
                           separatore
                           tinv-asterisk
                           separatore
                           tinv-mag
                           separatore
                           tinv-art-descrizione
                           separatore
                           tinv-um
                           separatore
                           tinv-qta-edit
                           separatore
                           tinv-prezzo-edit
                           separatore
                           tinv-valore-edit
                           separatore
                           tinv-kg-edit 
                           separatore
                           tinv-imballo delimited by size
                           into line-riga
                    end-string
                 end-if
                 write line-riga
                 add tinv-valore      to TotValore
                                         TotValoreMag
                                         TotValoreGen
                 add tinv-tot-utf     to TotUtf
                                         TotUtfMag
                                         TotUtfGen
                 add tinv-tot-non-utf to TotNonUtf
                                         TotNonUtfMag
                                         TotNonUtfGen
                 add tinv-tot-peso    to TotKg
                                         TotKgMag
              end-if
           end-perform.

           close lineseq.
           perform CALL-EXCEL.


      ***---
       READ-PROGMAG-LOCK-UPDATE.
           set RecLocked to false.
           initialize geslock-linkage.
           move prg-cod-articolo to prg-cod-articolo-edit.
           move prg-peso         to prg-peso-edit.
           string  "Articolo: "  prg-cod-articolo-edit " | "
                   "Magazzino: " prg-cod-magazzino     " | "
            X"0d0a""Imballo: "   prg-tipo-imballo      "   | "
                   "Peso: "      prg-peso-edit
            X"0d0a""risulta in uso su altro terminale."
            X"0d0a""Questo comporta l'impossibilità ad "
                   "aggiornarne la giacenza DAY." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok to true.
           read progmag with lock invalid continue end-read.
           if RecLocked
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "progmag"      to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-PROGMAG-LOCK-UPDATE
              when ignora  set tutto-ok to true
                           read progmag no lock
                                invalid continue 
                           end-read
              when termina display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           else
              if tipo-elab = 1
                 move prg-giacenza-udm to prg-giac-day
              else
                 if tca-movim-giac-periodo-neg
                    subtract rmo-qta from prg-giac-day
                 else
                    add      rmo-qta to   prg-giac-day
                 end-if
              end-if
              rewrite prg-rec invalid continue end-rewrite
              unlock progmag all records
           end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           set prima-volta          to false.
           initialize line-riga.
           if inveday-padre
              string separatore
                     separatore
                     "-- INVENTARIO DAY AL "
                     inveday-a-data(7:2)
                     "/"
                     inveday-a-data(5:2)
                     "/"
                     inveday-a-data(3:2) 
                     " --" delimited by size
                     into line-riga
              end-string
           else
              string separatore
                     separatore
                     separatore
                     "-- INVENTARIO DAY AL "
                     inveday-a-data(7:2)
                     "/"
                     inveday-a-data(5:2)
                     "/"
                     inveday-a-data(3:2) 
                     " --" delimited by size
                     into line-riga
              end-string
           end-if.
           write line-riga.
           initialize line-riga.

           if inveday-padre
              string "Codice"
                     separatore
                     " "
                     separatore
                     "Descrizione"
                     separatore
                     "UM"
                     separatore
                     "Q.tà"
                     separatore
                     "Prezzo"
                     separatore
                     "VALORE"
                     separatore
                     "Kg." delimited by size
                     into line-riga
              end-string
           else
              string "Codice"
                     separatore
                     " "
                     separatore
                     "Mag."
                     separatore
                     "Descrizione"
                     separatore
                     "UM"
                     separatore
                     "Q.tà"
                     separatore
                     "Prezzo"
                     separatore
                     "VALORE"
                     separatore
                     "Kg." 
                     separatore
                     "Imb" delimited by size
                     into line-riga
              end-string
           end-if.
           write line-riga.
           initialize line-riga.
           if inveday-padre
              string separatore
                     separatore
                     separatore
                     separatore
                     separatore
                     tipo-costo delimited by size
                     into line-riga
              end-string
           else
              string separatore
                     separatore
                     separatore
                     separatore
                     separatore
                     separatore
                     tipo-costo delimited by size
                     into line-riga
              end-string
           end-if.
           write line-riga.

      ***---
       TOTALI-MARCA.
           move SaveMarca  to mar-codice.
           if ( TotValore + TotKg + TotUtf + TotNonUtf ) not = 0
              write line-riga from spaces after 1
              read tmarche no lock invalid continue end-read
              inspect SaveMarca replacing leading x"30" by x"20"
              initialize line-riga
              move TotValore to TotValoreEdit
              move TotKg     to TotKgEdit
              if inveday-padre
                 string " "
                        separatore
                        " "
                        separatore
                        "Totale Marca "
                        mar-descrizione
                        separatore
                        SaveMarca
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        TotValoreEdit
                        separatore
                        TotKgEdit delimited by size
                        into line-riga
                 end-string
              else
                 string " "
                        separatore
                        " "
                        separatore
                        separatore
                        "Totale Marca "
                        mar-descrizione
                        separatore
                        SaveMarca
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        TotValoreEdit
                        separatore
                        TotKgEdit delimited by size
                        into line-riga
                 end-string
              end-if
              write line-riga

              initialize line-riga
              move TotUtf to TotUtfEdit
              if inveday-padre
                 string " "
                        separatore
                        " "
                        separatore
                        "Totale Kg. U T F"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore
                        TotUtfEdit delimited by size
                        into line-riga
                 end-string
              else
                 string " "
                        separatore
                        " "
                        separatore
                        separatore
                        "Totale Kg. U T F"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore
                        TotUtfEdit delimited by size
                        into line-riga
                 end-string
              end-if
              write line-riga

              initialize line-riga
              move TotNonUtf to TotNonUtfEdit
              if inveday-padre
                 string " "
                        separatore
                        " "
                        separatore
                        "Totale Kg. Esenti"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore 
                        " " 
                        separatore
                        TotNonUtfEdit delimited by size
                        into line-riga
                 end-string
              else
                 string " "
                        separatore
                        " "
                        separatore
                        separatore
                        "Totale Kg. Esenti"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore 
                        " " 
                        separatore
                        TotNonUtfEdit delimited by size
                        into line-riga
                 end-string
              end-if
              write line-riga

              initialize line-riga
              write line-riga
              move 0 to TotValore TotKg TotUtf TotNonUtf
           end-if.
           move tinv-marca to SaveMarca.

      ***---
       TOTALI-MAGAZZINO.
           if ( TotValoreMag + 
                TotKgMag     + 
                TotUtfMag    + 
                TotNonUtfMag ) not = 0
              initialize line-riga
              move TotValoreMag to TotValoreMagEdit
              move TotKgMag     to TotKgMagEdit
              if inveday-padre
                 string " "
                        separatore
                        " "
                        separatore
                        "Totale Magazzino"
                        separatore
                        SaveMagazzino
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        TotValoreMagEdit
                        separatore
                        TotKgMagEdit delimited by size
                        into line-riga
                 end-string
              else
                 string " "
                        separatore
                        " "
                        separatore
                        separatore
                        "Totale Magazzino"
                        separatore
                        SaveMagazzino
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        TotValoreMagEdit
                        separatore
                        TotKgMagEdit delimited by size
                        into line-riga
                 end-string
              end-if
              write line-riga
   
              initialize line-riga
              move TotUtfMag to TotUtfMagEdit
              if inveday-padre
                 string " "
                        separatore
                        " "
                        separatore
                        "Totale Kg. U T F"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore
                        TotUtfMagEdit delimited by size
                        into line-riga
                 end-string
              else
                 string " "
                        separatore
                        " "
                        separatore
                        separatore
                        "Totale Kg. U T F"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore
                        TotUtfMagEdit delimited by size
                        into line-riga
                 end-string
              end-if
              write line-riga
          
              initialize line-riga
              move TotNonUtfMag to TotNonUtfMagEdit
              if inveday-padre
                 string " "
                        separatore
                        " "
                        separatore
                        "Totale Kg. Esenti"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore 
                        " " 
                        separatore
                        TotNonUtfMagEdit delimited by size
                        into line-riga
                 end-string
              else
                 string " "
                        separatore
                        " "
                        separatore
                        separatore
                        "Totale Kg. Esenti"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore 
                        " " 
                        separatore
                        TotNonUtfMagEdit delimited by size
                        into line-riga
                 end-string
              end-if
              write line-riga

              initialize line-riga
              write line-riga
              move 0 to TotValoreMag 
                        TotKgMag 
                        TotUtfMag 
                        TotNonUtfMag
           end-if.
           move tinv-mag to SaveMagazzino.

      ***---
       TOTALI-GENERALI.
           if ( TotUtfGen + TotNonUtfGen  + TotValoreGen ) not = 0
              initialize line-riga
              move TotUtfGen to TotUtfGenEdit
              if inveday-padre
                 string " "
                        separatore
                        " "
                        separatore
                        "TOTALE Soggetti U T F --->"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore
                        TotUtfGenEdit delimited by size
                        into line-riga
                 end-string
              else         
                 string " "
                        separatore
                        " "
                        separatore
                        separatore
                        "TOTALE Soggetti U T F --->"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore
                        TotUtfGenEdit delimited by size
                        into line-riga
                 end-string
              end-if
              write line-riga
       
              initialize line-riga
              move TotNonUtfGen to TotNonUtfGenEdit
              if inveday-padre
                 string " "
                        separatore
                        " "
                        separatore
                        "TOTALE Esenti U T F --->"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore 
                        " " 
                        separatore
                        TotNonUtfGenEdit delimited by size
                        into line-riga
                 end-string
              else
                 string " "
                        separatore
                        " "
                        separatore
                        separatore
                        "TOTALE Esenti U T F --->"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore 
                        " " 
                        separatore
                        TotNonUtfGenEdit delimited by size
                        into line-riga
                 end-string
              end-if
              write line-riga

              initialize line-riga
              move TotValoreGen to TotValoreGenEdit
              if inveday-padre
                 string " "
                        separatore
                        " "
                        separatore
                        "TOTALE VALORE --->"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore 
                        " " 
                        separatore
                        TotValoreGenEdit delimited by size
                        into line-riga
                 end-string
              else
                 string " "
                        separatore
                        " "
                        separatore
                        separatore
                        "TOTALE VALORE --->"
                        separatore
                        " "
                        separatore
                        " "
                        separatore
                        " " 
                        separatore 
                        " " 
                        separatore
                        TotValoreGenEdit delimited by size
                        into line-riga
                 end-string
              end-if
              write line-riga

              initialize line-riga
              write line-riga
           end-if.

      ***---
       CLOSE-FILES.
           close rmovmag  articoli  progmag tmarche tcaumag
                 timballi timbalqta tmovmag timposte rlistini
                 impforn tscorte distinteb clienti destinif tpiombo
                 param tparamge tlistini.
           close tmp-inveday.
           delete file tmp-inveday.

      ***---
       CALCOLA-TRASPORTO.
           move 0 to costo-trasporto.
           move spaces to tge-chiave.
           read tparamge no lock.
           if como-trasporto-f = 1
              compute costo-trasporto = 
                      prg-peso * tge-trasp-f
           end-if.
           if como-trasporto-c = 1
              compute costo-trasporto = costo-trasporto +
                    ( prg-peso * tge-trasp-c)
           end-if. 
  
      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
       copy "costo-medio.cpy".
       copy "recupero-anagrafica.cpy".
       copy "prz-finito-forn.cpy".
       copy "imposte-fornitore.cpy".
       copy "addizionale-piombo-fornitore.cpy".
       copy "trova-parametro.cpy".
