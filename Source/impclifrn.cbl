       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      impclifrn.
       AUTHOR.                          Andrea.
       REMARKS. Batch che scrive sui files SSI CLI e FRN record
                prelevati dal nostro file GESLUX CLIENTI 
                (che contiene sia i clienti che i fornitori)
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl". 
           copy "CLI.sl".
           copy "FRN.sl".
           copy "DES.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "CLI.fd".
           copy "FRN.fd".
           copy "DES.fd".

       WORKING-STORAGE SECTION.
           copy "link-G2Agg.def".
           copy "link-geslock.def".
           copy "comune.def".

       78  titolo                           value "Importazione".

       77  status-clienti                   pic xx.
       77  status-CLI       pic xx.
       77  status-FRN       pic xx.
       77  status-DES       pic xx.

       77  sw-pulisci       pic xx.
           88 pulizia-des-frn   value "DF".
           88 pulizia-des-cli   value "DC".

       LINKAGE SECTION.
       77  tipo-import                      pic x.
           88 imp-cli                       value "C".
           88 imp-frn                       value "F".
           88 imp-entrambi                  value "E".

      ******************************************************************
       PROCEDURE DIVISION USING tipo-import.

       DECLARATIVES.
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File dei Clienti [CLIENTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
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
                initialize geslock-messaggio
                string   "File CLIENTI già in uso."
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

       CLI-ERR SECTION.
           use after error procedure on CLI.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-CLI 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File dei Clienti G2 [CLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [CLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CLI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       FRN-ERR SECTION.
           use after error procedure on FRN.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-FRN
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File dei Fornitori G2 [FRN] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [FRN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[FRN] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       DES-ERR SECTION.
           use after error procedure on DES.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-DES
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File dei Clienti G2 [DES] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [DES] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DES] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
                initialize geslock-linkage
                move "DES" to geslock-nome-file
                string   "Destino ", des-filiale , " risulta"
                  x"0d0a""in uso su G2 con altro terminale."
                  x"0d0a""Questo comporta l'impossibilità"
                  x"0d0a""di sincronizzare il record." delimited size
                      into geslock-messaggio
                end-string

                move 1 to geslock-v-riprova
                move 1 to geslock-v-ignora
                move 1 to geslock-v-termina
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova read DES    lock invalid continue end-read
                when ignora  read DES no lock invalid continue end-read
                when termina set errori to true
                             display message "Operazione interrotta!"
                                       title titolo
                                        icon 2
                             move -1 to G2Agg-status
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
           set RecLocked to false.
           set tutto-ok  to true.
           set trovato   to false.

      ***---
       OPEN-FILES.
           open input clienti.
      
      ***---
       ELABORAZIONE.
      ***--- Questa fase di import fa pulizia degli archivi prima di 
      ***--- iniziare l'elaborazione
      ***--- Massi 25/08/2005
           initialize sw-pulisci.
           if imp-frn or imp-entrambi
              perform PULISCI-FRN
              set pulizia-des-frn  to true
              perform PULISCI-DES
           end-if.
           if imp-cli or imp-entrambi
              perform PULISCI-CLI
              set pulizia-des-cli  to true
              perform PULISCI-DES
           end-if.
      ***--- End Massi

           set tutto-ok to true.
           move low-value to cli-rec.
           if imp-frn
              set cli-tipo-F to true
           end-if.
           start clienti key is >= cli-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read clienti next at end exit perform end-read
                 evaluate true
                 when cli-tipo-C
                      evaluate true
                      when imp-cli
                      when imp-entrambi
                           perform IMPORTA
                      when other
                           exit perform
                      end-evaluate
                 when cli-tipo-F
                      evaluate true
                      when imp-frn
                      when imp-entrambi
                           perform IMPORTA
                      when other
                           exit perform
                      end-evaluate
                 end-evaluate
              end-perform
           end-if.

           if trovato
              display message "Elaborazione effettuata!"
                        title titolo
           end-if.

      ***---
       IMPORTA.
           set tutto-ok to true.
           initialize G2Agg-linkage.
           move cli-codice to G2Agg-codice.
           if cli-tipo-C
              set G2Agg-cli to true
           else
              set G2Agg-for to true
           end-if.
           set G2Agg-insert to true.
           call   "G2Agg" using G2Agg-linkage.
           cancel "G2Agg".
           if G2Agg-status = 0
              set trovato to true
           end-if.

      ***---
       PULISCI-FRN.
           set tutto-ok to true.
           open i-o frn.
           if RecLocked
              string   "Il file dei Fornitori di G2" 
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
              initialize frn-codice
              start frn key not less frn-codice
                 invalid set errori to true
              end-start
              if tutto-ok
                 perform until 1 = 2
                    read frn next no lock 
                          at end exit perform 
                    end-read
                    delete frn record 
                          invalid continue 
                    end-delete
                 end-perform
              end-if
           end-if.
           close frn.

      ***---
       PULISCI-DES.
           set tutto-ok to true.
           open i-o des.
           if RecLocked
              string   "Il file dei Destinatari di G2" 
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
              initialize des-codice-g2
              if pulizia-des-frn
                 set des-tipo-frn   to true
              else
                 set des-tipo-cli   to true
              end-if
              start des key not less des-codice-g2
                 invalid set errori to true
              end-start
              if tutto-ok
                 perform until 1 = 2
                    read des next no lock 
                          at end exit perform 
                    end-read
                    if (pulizia-des-frn and not des-tipo-frn) or
                       (pulizia-des-cli and not des-tipo-cli)
                        exit perform 
                    end-if
                    delete des record 
                               invalid continue 
                    end-delete
                 end-perform
              end-if
           end-if.
           close des.

      ***---
       PULISCI-CLI.
           set tutto-ok to true.
           open i-o cli.
           if RecLocked
              string   "Il file dei Clienti di G2" 
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
              initialize cli-codice-g2
              start cli key not less cli-codice-g2
                 invalid set errori to true
              end-start
              if tutto-ok
                 perform until 1 = 2
                    read cli next no lock 
                          at end exit perform 
                    end-read
                    delete cli record 
                          invalid continue 
                    end-delete
                 end-perform
              end-if
           end-if.
           close cli.


      ***---
       CLOSE-FILES.
           close clienti.

      ***---
       EXIT-PGM.
           goback.
