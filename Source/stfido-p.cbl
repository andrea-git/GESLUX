       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stfido-p.
       AUTHOR.                          Andrea.
       REMARKS. Interrogazione saldi clienti.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "comune.def".
       copy "link-calfido.def".
       copy "common-excel.def".
       copy "acugui.def".
       copy "link-geslock.def".

      * COSTANTI
       78  titolo                value "Interrogazione saldi".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-clienti        pic xx.

       77  wstampa               pic x(256).

      * VARIABILI
       77  totale                pic s9(12)v999.
       77  rimanenza             pic s9(12)v999.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  user-codi             pic x(10).

       77  r-totale              pic ----.---.---.--9,99.
       77  r-rischio             pic ----.---.---.--9,99.
       77  r-rimanenza           pic ----.---.---.--9,99.
       77  r-saldo               pic ----.---.---.--9,99.
       77  r-fido                pic ----.---.---.--9.

      * FLAGS

       01  filler                pic 9.
         88 record-ok            value 1 false 0.

       LINKAGE SECTION.
       copy "link-stfido.def".

      ******************************************************************
       PROCEDURE DIVISION using stfido-linkage.

       DECLARATIVES.

       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
             x"0d0a""Tabella [CLIENTI] inesistente"
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
                  x"0d0a""Chiudere file Excel!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TXT"   to geslock-nome-file
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
           move st-user to user-codi.
           move 0 to counter counter2.
           initialize wstampa.
           set tutto-ok         to true.
           set trovato          to false.
           accept  wstampa      from environment "PATH-ST".
           inspect wstampa      replacing trailing spaces by low-value.
           inspect user-codi    replacing trailing spaces by low-value.
           string  wstampa      delimited by low-value
                   "stfido"     delimited by size
                   "_"          delimited by size
                   user-codi    delimited by low-value
                   ".csv"       delimited by size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           open input  clienti.

      ***---
       ELABORAZIONE.
           move low-value      to cli-rec.
           set cli-tipo-C to true.
           move st-cliente-da to cli-codice.
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 20
                       move counter to counter-edit
                       display counter-edit
                          upon st-handle at column 26,00 line 10,00
                       move 0 to counter2
                    end-if

                    read clienti next at end exit perform end-read
                    if cli-tipo-F 
                       exit perform
                    end-if
                    if cli-codice > st-cliente-a
                       exit perform
                    end-if
                    set record-ok to true
                    if st-tipo not = spaces and
                       st-tipo not = cli-tipo
                       set record-ok to false
                    end-if
                    if record-ok
                       initialize calfido-linkage 
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move cli-codice     to link-cli-codice
                       call "C$JUSTIFY" using link-cli-codice, "R"
                       inspect link-cli-codice 
                               replacing leading x"20" by x"30"
                       call   "calfido" using calfido-linkage
                       cancel "calfido"

                       compute totale = saldo + effetti-rischio
                       compute rimanenza = cli-fido - totale

                       if saldo           not = 0 or
                          effetti-rischio not = 0 or
                          cli-fido        not = 0

                          set record-ok to false

                          if st-fuori-fido
                             if rimanenza < 0
                                set record-ok to true
                             end-if
                          else
                             set record-ok to true
                          end-if

                          if record-ok
                             if not trovato
                                open output lineseq
                                if errori exit perform end-if
                                set trovato to true
                                perform ACCETTA-SEPARATORE
                                initialize line-riga
                                string "Codice"          delimited size
                                       separatore        delimited size
                                       "Ragione Sociale" delimited size
                                       separatore        delimited size
                                       "Fido"            delimited size
                                       separatore        delimited size
                                       "Saldo Scheda"    delimited size
                                       separatore        delimited size
                                       "RB a scadere"    delimited size
                                       separatore        delimited size
                                       "Esposizione"     delimited size
                                       separatore        delimited size
                                       "Fido residuo"    delimited size
                                       into line-riga
                                end-string
                                write line-riga
                             end-if

                             move saldo           to r-saldo
                             move effetti-rischio to r-rischio
                             move totale          to r-totale
                             move rimanenza       to r-rimanenza
                             move cli-fido        to r-fido

                             initialize line-riga
                             string cli-codice    delimited size
                                    separatore    delimited size
                                    cli-ragsoc-1  delimited size
                                    separatore    delimited size
                                    r-fido        delimited size
                                    separatore    delimited size
                                    r-saldo       delimited size
                                    separatore    delimited size
                                    r-rischio     delimited size
                                    separatore    delimited size
                                    r-totale      delimited size
                                    separatore    delimited size
                                    r-rimanenza   delimited size
                                    into line-riga
                             end-string
                             write line-riga
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.
           if not trovato
              display message "Nessun cliente trovato!"
                        title titolo
                         icon 2
           else
              perform CALL-EXCEL
           end-if.

      ***---
       CLOSE-FILES.
           close clienti.

      ***---
       EXIT-PGM.
           display "                                           "
                   upon st-handle at column 26,00 line 10,00.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
