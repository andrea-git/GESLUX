       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      impnazcatage.
       AUTHOR.                          Andrea.
       REMARKS. Batch che scrive sui files SSI TBLNA, TBLCA e TBLAG record
                prelevati dai nostri file GESLUX TNAZIONI TTIPOCLI AGENTI
                (che contengono rispettivamente NAZIONI, CATEGORIE CLIENTI
                e AGENTI)
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tnazioni.sl". 
           copy "agenti.sl".
           copy "ttipocli.sl".
           copy "TBLNA.sl".
           copy "TBLCA.sl".
           copy "TBLAG.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tnazioni.fd". 
           copy "agenti.fd".
           copy "ttipocli.fd".
           copy "TBLNA.fd".
           copy "TBLCA.fd".
           copy "TBLAG.fd".

       WORKING-STORAGE SECTION.
           copy "link-G2Agg.def".
           copy "link-geslock.def".
           copy "comune.def".

       78  titolo                           value "Importazione".

       77  status-tnazioni         pic xx.
       77  status-agenti          pic xx.
       77  status-ttipocli         pic xx.
       77  status-TBLNA            pic xx.
       77  status-TBLCA            pic xx.
       77  status-TBLAG            pic xx.

       77  sw-pulisci       pic x.
           88 pulizia-NAZ   value "N".
           88 pulizia-CAT   value "C".
           88 pulizia-AGE   value "A".
           88 pulizia-ALL   value "T".

       LINKAGE SECTION.
       77  tipo-import                      pic x.
           88 imp-naz                       value "N".
           88 imp-cat                       value "C".
           88 imp-age                       value "A".
           88 imp-all                       value "T".

      ******************************************************************
       PROCEDURE DIVISION USING tipo-import.

       DECLARATIVES.
       TNAZIONI-ERR SECTION.
           use after error procedure on tnazioni.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tnazioni
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle nazioni [TNAZIONI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TNAZIONI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TNAZIONI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
                initialize geslock-messaggio
                string   "File TNAZIONI già in uso."
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "tnazioni"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input tnazioni
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message box "Impossibile procedere."
            x"0d0a""File delle categorie clienti [TTIPOCLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
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
                initialize geslock-messaggio
                string   "File TTIPOCLI già in uso."
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

       AGENTI-ERR SECTION.
           use after error procedure on agenti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-agenti
           when "35"
                display message "Impossibile procedere."
                 x"0d0a""File degli agenti [AGENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [AGENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[AGENTI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
                initialize geslock-messaggio
                string   "File AGENTI già in uso."
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "agenti"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input agenti
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       TBLNA-ERR SECTION.
           use after error procedure on TBLNA.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TBLNA
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle nazioni G2 [TBLNA] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TBLNA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TBLNA] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       TBLCA-ERR SECTION.
           use after error procedure on TBLCA.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TBLCA
           when "35"
                display message "Impossibile procedere."
            x"0d0a""File delle categorie clienti G2 [TBLCA] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TBLCA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TBLCA] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       TBLAG-ERR SECTION.
           use after error procedure on TBLAG.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TBLAG
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File degli agenti G2 [TBLAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TBLAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TBLAG] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
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
           set RecLocked to false.
           set tutto-ok  to true.
           set trovato   to false.

      ***---
       OPEN-FILES.
           open input tnazioni.
           if tutto-ok 
              open input ttipocli
              if tutto-ok
                 open input agenti
                 if errori
                    close tnazioni ttipocli
                 end-if
              else
                 close tnazioni
              end-if
           end-if.

      ***---
       ELABORAZIONE.
      ***--- Questa fase di import fa pulizia degli archivi prima di 
      ***--- iniziare l'elaborazione
      ***--- Massi 25/08/2005
           move tipo-import to sw-pulisci.
           perform PULISCI-ARCHIVI.
      ***--- End Massi

           evaluate true
           when imp-naz perform IMPORTA-NAZIONI
           when imp-age perform IMPORTA-AGENTI
           when imp-cat perform IMPORTA-CATEGORIE
           when imp-all perform IMPORTA-NAZIONI
                        perform IMPORTA-AGENTI
                        perform IMPORTA-CATEGORIE
           end-evaluate.

           if trovato
              display message "Elaborazione effettuata!"
                        title titolo
           end-if.

      ***---
       IMPORTA-NAZIONI.
           set imp-naz to true.
           move low-value to naz-rec.
           start tnazioni key is >= naz-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tnazioni next at end exit perform end-read
                 perform IMPORTA
              end-perform
           end-if.      

      ***---
       IMPORTA-CATEGORIE.
           set imp-cat to true.
           move low-value to tcl-rec.
           start ttipocli key is >= tcl-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read ttipocli next at end exit perform end-read
                 perform IMPORTA
              end-perform
           end-if.            

      ***---
       IMPORTA-AGENTI.
           set imp-age to true.
           move low-value to age-rec.
           start agenti key is >= age-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read agenti next at end exit perform end-read
                 perform IMPORTA
              end-perform
           end-if.

      ***---
       IMPORTA.
           initialize G2Agg-linkage.
           evaluate true
           when imp-naz
                set  G2Agg-naz    to true
                move naz-codice   to G2Agg-nazione
           when imp-cat
                set  G2Agg-cat    to true
                move tcl-codice   to G2Agg-categoria
           when imp-age
                set  G2Agg-age    to true
                move age-codice   to G2Agg-agente
           end-evaluate.
           if G2Agg-status = 0
              set trovato to true
           end-if.
           set G2Agg-insert to true.
           call   "G2Agg" using G2Agg-linkage.
           cancel "G2Agg".

      ***---
       PULISCI-ARCHIVI.
           evaluate true
           when pulizia-naz perform PULISCI-NAZIONI
           when pulizia-cat perform PULISCI-CATEGORIE
           when pulizia-age perform PULISCI-AGENTI
           when pulizia-all perform PULISCI-NAZIONI
                            perform PULISCI-CATEGORIE
                            perform PULISCI-AGENTI
           end-evaluate.

      ***---
       PULISCI-NAZIONI.
           open i-o TBLNA.
           if RecLocked
              string   "Il file delle nazioni G2"
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string
              move 0 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           else
              set tutto-ok      to true
              initialize record-tblna
              move "NA" to tblna-codice1
              start tblna key >= tblna-codice
                    invalid set errori to true
              end-start
              if tutto-ok
                 perform until 1 = 2
                    read tblna next no lock 
                          at end exit perform 
                    end-read
                    if tblna-codice1 not = "NA"
                       exit perform
                    end-if
                    delete tblna record 
                          invalid continue 
                    end-delete
                 end-perform
              end-if
           end-if.
           close TBLNA. 

      ***---
       PULISCI-CATEGORIE.
           open i-o TBLCA.
           if RecLocked
              string   "Il file delle cateogire clienti G2"
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string
              move 0 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           else
              set tutto-ok      to true
              initialize record-tblca
              move "CA" to tblca-codice1
              start tblca key >= tblca-codice
                    invalid set errori to true
              end-start
              if tutto-ok
                 perform until 1 = 2
                    read tblca next no lock 
                          at end exit perform 
                    end-read
                    if tblca-codice1 not = "CA"
                       exit perform
                    end-if
                    delete tblca record 
                          invalid continue 
                    end-delete
                 end-perform
              end-if
           end-if.
           close TBLCA.

      ***---
       PULISCI-AGENTI.
           open i-o TBLAG.
           if RecLocked
              string   "Il file decli agenti G2"
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string
              move 0 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           else
              set tutto-ok      to true
              initialize record-tblag
              move "AG" to tblag-codice1
              start tblag key >= tblag-codice
                    invalid set errori to true
              end-start
              if tutto-ok
                 perform until 1 = 2
                    read tblag next no lock 
                          at end exit perform 
                    end-read
                    if tblag-codice1 not = "AG"
                       exit perform
                    end-if
                    delete tblag record 
                          invalid continue 
                    end-delete
                 end-perform
              end-if
           end-if.
           close TBLAG.

      ***---
       CLOSE-FILES.
           close agenti tnazioni ttipocli.

      ***---
       EXIT-PGM.
           goback. 
