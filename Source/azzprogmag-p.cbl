       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzprogamg-p.
       AUTHOR.                          Andrea.
       REMARKS. Questo batch verrà lanciato solo a FINE ANNO dai
                dipendenti Lubex per azzerare i valori consolidati e 
                valorizzare la giacenza iniziale con la giacenza periodo.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".
           copy "articoli.sl".
           copy "tmarche.sl".
           copy "timposte.sl".

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd".  
           copy "articoli.fd".
           copy "tmarche.fd". 
           copy "timposte.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "costo-medio.def".
       copy "imposte.def".

      * COSTANTI
       78  titolo value "Batch Azzeramento".

      * FILE-STATUS
       77  status-progmag        pic xx.
       77  status-articoli       pic xx.
       77  status-tmarche        pic xx.
       77  status-timposte       pic xx.

      * FLAGS
       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

      * VARIABILI
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       77  link-handle           handle of window.

       PROCEDURE DIVISION USING link-handle.

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
                     open i-o progmag allowing readers
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
           perform OPEN-FILE.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILE
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.
           move 0 to counter counter2.

      ***---
       OPEN-FILE.
           open i-o progmag allowing readers.
           open input articoli tmarche timposte.

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move low-value  to prg-rec.
           start progmag  key is >= prg-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read progmag next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 30
                                             line 18
                    move 0 to counter2
                 end-if
                                                                      
                 if prg-cod-magazzino = spaces and
                    prg-tipo-imballo  = spaces and
                    prg-peso          = 0                
                    perform CALCOLA-COSTO-MP-COMPLETO
                    add 0,005 to costo-mp giving costo-mp-2dec
                 end-if
      
                 move costo-mp-2dec    to prg-costo-medio
                 |DA COMMENTARE PER RIPRISTINO
                 move prg-giacenza-udm to prg-ini-udm
                 move prg-giacenza-kg  to prg-ini-kg
                 compute prg-ini-valore   =
                         prg-giacenza-udm * costo-mp-2dec
                 ||
                 move prg-ini-udm to prg-giacenza-udm
                 move prg-ini-kg  to prg-giacenza-kg

                 move 0 to |prg-ini-valore
                           prg-acq-udm
                           prg-acq-kg
                           prg-acq-valore
                           prg-ven-udm
                           prg-ven-kg
                           prg-ven-valore
                           prg-var-inv-udm
                           prg-var-inv-kg
                           prg-var-inv-valore
                           prg-resi-fornitori-udm
                           prg-resi-fornitori-kg
                           prg-resi-fornitori-valore
                           prg-resi-da-cli-udm
                           prg-resi-da-cli-kg
                           prg-resi-da-cli-valore
                           prg-udm-el    
                           prg-kg-el  
                           prg-valore-el         
                           prg-udm-el2
                           prg-kg-el2
                           prg-valore-el2
                           prg-udm-ul
                           prg-kg-ul
                           prg-valore-ul
                           prg-udm-ul2
                           prg-kg-ul2
                           prg-valore-ul2
                 rewrite prg-rec  invalid continue end-rewrite
              end-perform
           end-if.

      ***---
       CLOSE-FILE.
           close progmag articoli tmarche timposte.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "costo-medio.cpy".
           copy "recupero-anagrafica.cpy".
