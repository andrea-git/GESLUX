       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      calinc-p.
       AUTHOR.                          Andrea.
       REMARKS. Aggiornamento della data di liquidazione sul file
                PROVVIG per tutte le fatture già COMPLETAMENTE
                incassate e che non sono ancora state liquidate.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "PAT.sl".
           copy "PAS.sl".
           copy "TBLDO.sl".
           copy "TBLCO.sl".
           copy "TBLTR.sl".
           copy "G2.sl".
           copy "provvig.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "PAT.fd".
           copy "PAS.fd".
           copy "TBLDO.fd".
           copy "TBLCO.fd".
           copy "TBLTR.fd".
           copy "G2.fd".
           copy "provvig.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
       78  titolo   value "LIQUIDAZIONE PROVVIGIONI: CALCOLO INCASSATO".

      * FILE STATUS
       77  status-PAT            pic xx.
       77  status-PAS            pic xx.
       77  status-TBLDO          pic xx.
       77  status-TBLCO          pic xx.
       77  status-TBLTR          pic xx.
       77  status-G2             pic xx.
       77  status-provvig        pic xx.

      * VARIABILI
       77  data-fatt-edit        pic zz.zzz.zz9.
       77  riga-edit             pic zz.zz9.
       77  cli-codice-x          pic x(8).
       77  num-fattura-x         pic x(8).
       77  num-fattura-x6        pic x(6).
       77  como-numero-rif       pic x(12).
       01  save-chiave.
         05 save-anno-fat        pic 9(4).
         05 save-num-fat         pic 9(8).
         05 save-riga-fat        pic 9(5).

       77  como-data             pic 9(8).
       77  como-data-1           pic 9(8).
       77  como-data-2           pic 9(8).

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

      * FILE STATUS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 record-ok          value 1, false 0.

       LINKAGE SECTION.
       77  link-user             pic x(20).
       77  link-data             pic 9(8).
       77  link-tot-provv        pic 9(8).
       77  link-handle           handle of window.

      ******************************************************************
       PROCEDURE DIVISION USING link-user, 
                                link-data, 
                                link-tot-provv,
                                link-handle.

       DECLARATIVES.

      ***---
       PAT-ERR SECTION.
           use after error procedure on PAT.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pat
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""File testata partitari [PAT] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [PAT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PAT] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.

      ***---
       PAS-ERR SECTION.
           use after error procedure on PAS.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pas
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""File righe scadenzario [PAS] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [PAS] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PAS] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.

      ***---
       TBLDO-ERR SECTION.
           use after error procedure on TBLDO.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tbldo
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""Tabella Documenti vendita [TBLDO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TBLDO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TBLDO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.

      ***---
       TBLCO-ERR SECTION.
           use after error procedure on TBLCO.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tblco
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""Tabella causali contabili [TBLCO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TBLCO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TBLCO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.

      ***---
       TBLTR-ERR SECTION.
           use after error procedure on TBLTR.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tbltr
           when "35"
                display message box        "Impossibile procedere."
            x"0d0a""Tabella scadenze effetti e spese[TBLTR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TBLTR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TBLTR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.

      ***---
       G2-ERR SECTION.
           use after error procedure on G2.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-g2
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""File parametri G2 [G2] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [G2] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[G2] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate.

      ***---
       PROVVIG-ERR SECTION.
           use after error procedure on provvig.
           set tutto-ok  to true.
           evaluate status-provvig
           when "35"
                set errori to true
                display message "File [PROVVIG] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [PROVVIG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROVVIG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"                     
                set  RecLocked    to true
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "provvig"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o provvig
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           when "99"
                set  RecLocked    to true
                move pvv-num-fat  to data-fatt-edit
                move pvv-riga-fat to riga-edit
                initialize geslock-messaggio
                string "Il record di provvigione per: "
                x"0d0a""Fattura n. ", data-fatt-edit, 
                       " riga n. ", riga-edit
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad aggiornarne"
                       " la data di liquidazione."
                      delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 1 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "provvig"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     set RecLocked to false
                     read provvig lock
                when ignora
                     |RIPOSIZIONO IL CURSORE
                     perform RIPOSIZIONA-CURSORE
                when termina
                     set errori to true
                end-evaluate
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform LEGGI-PARAMETRI
              if tutto-ok
                 perform ELABORAZIONE
              end-if
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to link-tot-provv.
           move 0 to counter counter2.
           set tutto-ok to true.
           set trovato  to false.

      ***---
       OPEN-FILES.
           perform OPEN-IO-PROVVIG.
           if tutto-ok
              open input pat pas tbldo tblco g2 tbltr
              if errori
                 close provvig
              end-if
           end-if.

      ***---
       OPEN-IO-PROVVIG.
           open i-o provvig.

      ***---
       LEGGI-PARAMETRI.
           set tutto-ok to true.
           move spaces to G2-codice.
           read G2 no lock
                invalid
                set errori to true
                display message 
                        "Record parametri generali G2 NON PRESENTE"
                 x"0d0a""Impossibile procedere"
                          title titolo
                           icon 2
            not invalid
                if G2-cod-fatture = spaces
                   set errori to true
                   display message "Codice Fatture NON PRESENTE"
                            x"0d0a""Impossibile procedere"
                             title titolo
                              icon 2
                end-if
           end-read.

           if tutto-ok
              move "DO"           to tbldo-codice1
              move G2-cod-fatture to tbldo-codice2
              read tbldo no lock
                   invalid
                   set errori to true
                   display message
                           "Record tipologie documenti NON PRESENTE"
                    x"0d0a""Impossibile procedere"
                             title titolo
                              icon 2
               not invalid
                   move "CO"            to tblco-codice1
                   move tbldo-codice-co to tblco-codice2
                   |MI SERVE LA VARIABILE "TBLCO-TIPO-DOCUMENTO"
                   read tblco no lock
                        invalid
                        set errori to true
                        display message
                               "Record causale contabile NON PRESENTE"
                        x"0d0a""Impossibile procedere"
                                  title titolo
                                   icon 2
                   end-read
              end-read
           end-if. 

      ***---
       ELABORAZIONE.
           move low-value to pvv-rec.
           move 0 to pvv-data-liq.
           start provvig key is >= k-data
                 invalid
                 set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read provvig next no lock at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 20
                                             line 03
                    move 0 to counter2
                 end-if

                 |Esco quando trovo le provvigioni già liquidate...
                 if pvv-data-liq not = 0
                    exit perform
                 end-if
                 |...o quelle fatturate DOPO il periodo di riferimento
                 if pvv-data-fat > link-data
                    exit perform
                 end-if
                 perform VERIFICA-PAGAMENTO
                 if record-ok
                    perform READ-PROVVIG-LOCK
                    if errori
                       exit perform
                    else
                       if not RecLocked
                          set trovato to true
                          add 1 to link-tot-provv
                          move link-data to pvv-data-liq
                          rewrite pvv-rec invalid continue end-rewrite
                          perform RIPOSIZIONA-CURSORE
                       end-if
                    end-if
                 end-if      
              end-perform

           end-if.

           if not trovato
              display message "Nessuna provvigione trovata!"
                        title titolo
                         icon 2
           end-if.

      ***---
       VERIFICA-PAGAMENTO.
           set record-ok to false.

           move pvv-cliente to cli-codice-x.
           call "C$JUSTIFY"         using cli-codice-x, "R".
           inspect cli-codice-x replacing leading x"20" by x"30".

           move pvv-num-fat              to num-fattura-x.
           inspect num-fattura-x  replacing leading x"30" by x"20".
           call "C$JUSTIFY"           using num-fattura-x, "L".
           move num-fattura-x            to num-fattura-x6.
           call "C$JUSTIFY"           using num-fattura-x6, "R".
           inspect num-fattura-x6 replacing leading x"20" by x"30".

           initialize como-numero-rif.
           string num-fattura-x6       delimited size
                  tblco-tipo-documento delimited size
                  into como-numero-rif
           end-string.

           move low-value to record-pat.
           set  pat-tipo-cfm-cli to true.
           move cli-codice-x     to pat-codice-cfm.
           move pvv-data-fat     to pat-data-riferimento.
           move como-numero-rif  to pat-numero-riferimento.
           start pat key is >= pat-codice1
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read pat next at end exit perform end-read
                    if pat-numero-riferimento not = como-numero-rif or
                       pat-data-riferimento   not = pvv-data-fat    or
                       pat-codice-cfm         not = cli-codice-x    or
                       not pat-tipo-cfm-cli
                       exit perform
                    end-if
                    move low-value       to pas-riga
                    move pat-progressivo to pas-progressivo
                    start pas key is >= pas-codice
                          invalid continue
                      not invalid
                          perform until 1 = 2
                            read pas next no lock
                                 at end exit perform
                            end-read
                            if pas-progressivo not = pat-progressivo
                               exit perform
                            end-if

                            if pas-situazione = 2
                               move "TR"          to tbltr-codice1
                               move pas-codice-tr to tbltr-codice2
                               read tbltr no lock
                                    invalid set record-ok to true
                                not invalid
                                    if tbltr-rischio = "N" or = "n"
                                       set record-ok to true
                                    else
                                       perform ADD-DAYS
                                       if como-data <= link-data
                                          set record-ok to true
                                       else
                                          set record-ok to false
                                       end-if
                                    end-if
                               end-read
                            else
                               set record-ok to false
                            end-if

                            if not record-ok exit perform end-if

                         end-perform
                    end-start
                 end-perform
           end-start.

      ***---
       ADD-DAYS.
           move pas-data-scadenza to como-data-1.
           compute como-data-2 = 
                   function INTEGER-OF-DATE(como-data-1).
           add tbltr-giorni-rischio to como-data-2.
           compute como-data = 
                   function DATE-OF-INTEGER(como-data-2).

      ***---
       READ-PROVVIG-LOCK.
           set RecLocked to false.
           read provvig lock invalid continue end-read.

      ***---
       RIPOSIZIONA-CURSORE.
           move pvv-chiave to save-chiave.
           start provvig key is >= k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read provvig next no lock
                         at end exit perform
                    end-read
                    if pvv-chiave = save-chiave
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***--
       CLOSE-FILES.
           unlock provvig all records.
           close  provvig.
           close tbldo tblco pas pat g2 tbltr.

      ***---
       EXIT-PGM.
           goback.
