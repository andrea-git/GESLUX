       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stmarcon-p.
       AUTHOR.                          Andrea.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "clienti.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "articoli.sl".
           copy "progmag.sl".
           copy "tcaumag.sl".
           copy "tmarche.sl".
           copy "timposte.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "clienti.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "tcaumag.fd".
           copy "tmarche.fd".
           copy "timposte.fd".
           copy "ttipocli.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".
       copy "costo-medio.def".
       copy "imposte.def".

      * COSTANTI
       78  titolo                value 
                "Stampa Marginalità prodotti singoli movimenti Clienti".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-clienti        pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-articoli       pic xx.
       77  status-progmag        pic xx.
       77  status-tcaumag        pic xx.
       77  status-tmarche        pic xx.
       77  status-timposte       pic xx.
       77  status-ttipocli       pic xx.
       77  wstampa               pic x(256).

      * VARIABILI
       77  user-codi             pic x(10).
       77  qta-edit              pic ---.---.--9.
       77  como-qta              pic s9(8).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  prezzo                pic s9(12)v99.
       77  margine               pic s9(12)v99.
       77  como-margine          pic s9(12)v99.
LUBEXX 77  diff-valore           pic s9(12)v99.
       77  prezzo-edit           pic ----.---.---.--9,99.
       77  costo-mp-edit         pic ----.---.---.--9,99.
       77  margine-edit          pic ----.---.---.--9,99.
LUBEXX 77  diff-valore-edit      pic ----.---.---.--9,99.

      * FLAGS
       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       01  filler                pic 9.
         88 prima-volta          value 1, false 0.

       01  filler                pic 9.
         88 record-ok            value 1, false 0.

       01  filler                pic 9.
         88 trovato              value 1, false 0.

       01  filler                pic 9.
         88 RecLocked            value 1, false 0.

       LINKAGE SECTION.
       copy "link-stmarcon.def".

      ******************************************************************
       PROCEDURE DIVISION using stmarcon-linkage.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
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

       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File progressivi [PROGMAG] inesistente"
                          title titolo
                           icon 2
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

       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                display message "Impossibile procedere."
                   x"0d0a""File della testata [TMOVMAG] inesistente"
                          title titolo
                           icon 2
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File delle righe [RMOVMAG] inesistente"
                          title titolo
                           icon 2
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                          title titolo
                           icon 2
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "Impossibile procedere."
                x"0d0a""File causali di magazzino [TCAUMAG] inesistente"
                          title titolo
                           icon 2
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
           move link-stm-user to user-codi.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa.
           set tutto-ok          to true.
           set prima-volta       to true.
           set trovato           to false.
           accept  wstampa       from environment "PATH-ST".
           inspect wstampa       replacing trailing spaces by low-value.
           inspect user-codi     replacing trailing spaces by low-value
           string  wstampa       delimited by low-value
                   "stmarcon"    delimited by size
                   "_"           delimited by size
                   user-codi     delimited by low-value
                   ".csv"        delimited by size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           open output lineseq.
           if tutto-ok
              open input tmovmag 
                         rmovmag 
                         articoli
                         progmag 
                         clienti
                         tcaumag
                         tmarche
                         timposte
                         ttipocli
              if errori goback end-if
           end-if.

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move low-value        to tmo-rec.
           move link-stm-data-da to tmo-data-movim.
           start tmovmag key is >= k-data
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tmovmag next at end exit perform end-read
                 if tmo-data-movim > link-stm-data-a exit perform end-if
                 set record-ok to false

                 perform CONTROLLI-MOVIMENTO

                 if record-ok
                    set TrattamentoGDO to false
                    if tmo-cliente
                       move cli-tipo to tcl-codice
                       read ttipocli no lock
                       if ttipocli-gdo set TrattamentoGDO to true
                       else            set TrattamentoGDO to false
                       end-if
                    end-if
                    perform LOOP-RIGHE-ORDINE
                 end-if

              end-perform
           end-if.

           if not trovato 
              display message "Nessun movimento nei limiti richiesti"
                        title titolo
                         icon 2
           else
              perform CALL-EXCEL 
           end-if.

      ***---
       CONTROLLI-MOVIMENTO.
           evaluate true
           when link-stm-E
                move tmo-tipo       to cli-tipo-CF
                move tmo-cod-clifor to cli-codice
                read clienti no lock
                     invalid continue
                 not invalid
                     set record-ok to true
                end-read
   
           when link-stm-F
                if tmo-fornitore
                   set  cli-tipo-F      to true
                   if link-stm-codice not = 0
                      move link-stm-codice to cli-codice
                   else
                      move tmo-cod-clifor  to cli-codice
                   end-if
                   read clienti no lock
                        invalid continue
                    not invalid
                        if link-stm-codice = 0
                           set record-ok to true
                        else
                           if link-stm-codice = tmo-cod-clifor
                              set record-ok to true
                           end-if
                        end-if
                   end-read
                end-if

           when link-stm-C
                if tmo-cliente
                   set  cli-tipo-C    to true
                   if link-stm-codice not = 0
                      move link-stm-codice to cli-codice
                   else
                      move tmo-cod-clifor  to cli-codice
                   end-if
                   read clienti no lock
                        invalid continue
                    not invalid
                        if link-stm-codice not = 0
                           if link-stm-codice  = tmo-cod-clifor
                              set record-ok to true
                           end-if
                        else
                           if link-stm-tipo = spaces
                              set record-ok to true
                           else
                              if link-stm-tipo = cli-tipo
                                 set record-ok to true
                              end-if
                           end-if

                           if record-ok
                              if link-stm-gdo not = spaces and
                                 link-stm-gdo not = cli-gdo
                                 set record-ok to false
                              end-if
                           end-if
                                    
                        end-if
                   end-read
              end-if
           end-evaluate.

      ***---
       LOOP-RIGHE-ORDINE.
           move low-value  to rmo-rec.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           start rmovmag key is >= rmo-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              move tmo-causale to tca-codice
              read tcaumag no lock
                   invalid move spaces to tca-descrizione
              end-read
              perform until 1 = 2
                 read rmovmag next at end exit perform end-read
                 if tmo-anno   not = rmo-anno or
                    tmo-numero not = rmo-movim
                    exit perform
                 end-if
                 set record-ok        to false
                 if link-stm-articolo not = 0
                    if link-stm-articolo = rmo-articolo
                       set record-ok to true
                    end-if
                 else
                    set record-ok to true
                 end-if
                 if record-ok
                    set  record-ok    to false
                    move rmo-articolo to art-codice
                    read articoli
                         invalid continue
                     not invalid
                         if link-stm-marca = 0
                            set record-ok to true
                         else
                            if link-stm-marca = art-marca-prodotto
                               set record-ok to true
                            end-if
                         end-if
                    end-read
                 end-if
                 if record-ok  
                    move 0 to prezzo costo-mp margine diff-valore
                    perform CALCOLA-PREZZO
                    if cli-tipo-C
                       move rmo-chiave-progmag to prg-chiave
                       move spaces  to prg-cod-magazzino
                       move spaces  to prg-tipo-imballo
                       move 0       to prg-peso
                       read progmag no lock
                            invalid set record-ok to false
                        not invalid
                            perform RECUPERA-COSTO-MP
                            perform CALCOLA-MARGINE
                       end-read
                    end-if
                 end-if
                 if record-ok
                    perform VALORIZZA-RIGA
                 end-if
              end-perform
           end-if.
           set tutto-ok to true.

      ***---
       CALCOLA-PREZZO.
           compute prezzo = rmo-netto + rmo-imp-cons + rmo-coubat.

      ***---
       CALCOLA-MARGINE.
           compute margine = prezzo - costo-mp.

LUBEXX***---
       CALCOLA-DIFF-VALORE.
           compute diff-valore = margine * rmo-qta.

      ***---
       VALORIZZA-RIGA.
           if prima-volta
              perform ACCETTA-SEPARATORE
              set prima-volta to false
              initialize line-riga
              string "Causale"              delimited size
                     separatore             delimited size
                     "Descrizione Causale"  delimited size                                     
                     separatore             delimited size
                     "Codice"               delimited size
                     separatore             delimited size
                     "Cliente/Fornitore"    delimited size
                     separatore             delimited size
                     "Nr. Doc."             delimited size
                     separatore             delimited size
                     "Riga"                 delimited size
                     separatore             delimited size
                     "Data Doc."            delimited size
                     separatore             delimited size
                     "Cod. Art."            delimited size
                     separatore             delimited size
                     "Descrizione Articolo" delimited size
                     separatore             delimited size
                     "Q.tà"                 delimited size
                     separatore             delimited size
                     "Prezzo"               delimited size
                     separatore             delimited size
                     "Costo Ul"             delimited size
                     separatore             delimited size
                     "Margine"              delimited size
LUBEXX               separatore             delimited size
LUBEXX               "Diff. Valore"         delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces
              set   trovato   to   true
           end-if.

           move prezzo   to prezzo-edit.
           move costo-mp to costo-mp-edit.

LUBEXX     if rmo-qta = 0 move 1 to rmo-qta end-if.
LUBEXX     perform CALCOLA-DIFF-VALORE.
           move rmo-qta to como-qta.

LUBEXX     if tca-imponibile-neg
LUBEXX        compute margine     = margine     - ( margine     * 2 )
LUBEXX        compute diff-valore = diff-valore - ( diff-valore * 2 )
LUBEXX        compute como-qta    = rmo-qta     - ( rmo-qta     * 2 )
LUBEXX     end-if.

           move como-qta    to qta-edit.

LUBEXX     move margine     to margine-edit.
LUBEXX     move diff-valore to diff-valore-edit.

           initialize line-riga.
           string tmo-causale         delimited size
                  separatore          delimited size
                  tca-descrizione     delimited size
                  separatore          delimited size
                  cli-codice          delimited size
                  separatore          delimited size
                  cli-ragsoc-1        delimited size
                  separatore          delimited size
                  tmo-numero          delimited size
                  separatore          delimited size
                  rmo-riga            delimited size
                  separatore          delimited size
                  tmo-data-movim(7:2) delimited size
                  "/"                 delimited size
                  tmo-data-movim(5:2) delimited size
                  "/"                 delimited size
                  tmo-data-movim(1:4) delimited size
                  separatore          delimited size
                  art-codice          delimited size
                  separatore          delimited size
                  art-descrizione     delimited size
                  separatore          delimited size
                  qta-edit            delimited size
                  separatore          delimited size
                  prezzo-edit         delimited size
                  separatore          delimited size
                  costo-mp-edit       delimited size
                  separatore          delimited size
                  margine-edit        delimited size
LUBEXX            separatore          delimited size
LUBEXX            diff-valore-edit    delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       CLOSE-FILES.
           close clienti 
                 lineseq 
                 articoli 
                 progmag 
                 tmovmag 
                 rmovmag 
                 tcaumag
                 tmarche
                 timposte
                 ttipocli.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
       copy "costo-medio.cpy".
       copy "calcola-costo-mp-when-zero.cpy".
       copy "recupera-costo-mp.cpy".
