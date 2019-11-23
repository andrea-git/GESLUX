       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      G2Agg.
       AUTHOR.                          Andrea.
       REMARKS. Inserimento/Modifica/Cancellazione di Clienti / Fornitori e 
                Destini nei rispettivi archivi CLI FRN DES utilizzati da G2
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "recapiti.sl".
           copy "clienti.sl". 
           copy "ttipocli.sl".
           copy "tnazioni.sl".
           copy "agenti.sl".
           copy "DOCDI.sl".
           copy "CLI.sl".
           copy "FRN.sl".
           copy "TBLNA.sl". |NAZIONI
           copy "TBLCA.sl". |CATEGORIE
           copy "TBLAG.sl". |AGENTI
           copy "TBLME.sl". |GRUPPI MERCEOLOGICI
           copy "TBLCS.sl". |GRUPPI GDO
           copy "FPGRUPPICS.sl". |GRUPPI GDO COLLEGAMENTO
           copy "G2.sl".
           copy "ABI.sl".
LUBEXX     copy "tgrupgdo.sl".
           copy "ART.sl".
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "recapiti.fd".
           copy "clienti.fd". 
           copy "ttipocli.fd".
           copy "tnazioni.fd".
           copy "agenti.fd".
           copy "DOCDI.fd".
           copy "CLI.fd".
           copy "FRN.fd".
           copy "TBLNA.fd". |NAZIONI
           copy "TBLCA.fd". |CATEGORIE
           copy "TBLAG.fd". |AGENTI  
           copy "TBLME.fd". |GRUPPI MERCEOLOGICI
           copy "TBLCS.fd". |GRUPPI GDO
           copy "FPGRUPPICS.fd". |GRUPPI GDO COLLEGAMENTO
           copy "G2.fd".
           copy "ABI.fd".
LUBEXX     copy "tgrupgdo.fd".   
           copy "ART.fd".   
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "comune.def".

       77  status-recapiti    pic xx.
       77  status-clienti     pic xx.
       77  status-ttipocli    pic xx.
       77  status-agenti      pic xx.
       77  status-tnazioni    pic xx.
       77  status-TBLNA       pic xx.
       77  status-TBLAG       pic xx.
       77  status-TBLCA       pic xx.
       77  status-TBLME       pic xx.
       77  status-DOCDI       pic xx.
       77  status-CLI         pic xx.
       77  status-FRN         pic xx.
       77  status-G2          pic xx.
       77  status-abi         pic xx.
LUBEXX 77  status-tgrupgdo    pic xx.     
       77  status-art         pic xx.
       77  status-articoli    pic xx.
       77  status-fpgruppics  pic xx.
       77  status-tblcs       pic xx.
                                               
       77  TotChar            pic 9(4) value 0.
       77  ComoChar           pic 9(4) value 0.
       77  como-char          pic x.

       01  filler             pic 9 value 0.
         88 trovato-me        value 1, false 0.

       78  titolo    value "Aggiornamento files G2".

       LINKAGE SECTION.
       copy "link-G2Agg.def".

      ******************************************************************
       PROCEDURE DIVISION using G2Agg-linkage.

       DECLARATIVES.
      ***---
       RECAPITI-ERR SECTION.
           use after error procedure on recapiti.
           set tutto-ok  to true.
           evaluate status-recapiti
           when "35"
                display message "File [RECAPITI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [RECAPITI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RECAPITI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       ABI-ERR SECTION.
           use after error procedure on abi.
           set tutto-ok  to true.
           evaluate status-abi
           when "35"
                display message "File [ABI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [ABI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ABI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "File [CLIENTI] not found!"
                          title titolo
                           icon 3
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
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message "File [TTIPOCLI] not found!"
                          title titolo
                           icon 3
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
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       TNAZIONI-ERR SECTION.
           use after error procedure on tnazioni.
           set tutto-ok  to true.
           evaluate status-tnazioni
           when "35"
                display message "File [TNAZIONI] not found!"
                          title titolo
                           icon 3
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
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       AGENTI-ERR SECTION.
           use after error procedure on agenti.
           set tutto-ok  to true.
           evaluate status-agenti
           when "35"
                display message "File [AGENTI] not found!"
                          title titolo
                           icon 3
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
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       DOCDI-ERR SECTION.
           use after error procedure on docdi.
           set tutto-ok  to true.
           evaluate status-docdi
           when "35"
                display message "File [DOCDI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [DOCDI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DOCDI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       G2-ERR SECTION.
           use after error procedure on G2.
           set tutto-ok  to true.
           evaluate status-G2
           when "35"
                display message "File [G2] not found!"
                          title titolo
                           icon 3
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
           when "99"
                set RecLocked to true
           end-evaluate.

      ***---
       CLI-ERR SECTION.
           use after error procedure on CLI.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-CLI 
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File Clienti G2 [CLI] inesistente"
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

      ***---
       FRN-ERR SECTION.
           use after error procedure on FRN.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-FRN
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File Fornitori G2 [FRN] inesistente"
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

      ***---
       TBLNA-ERR SECTION.
           use after error procedure on TBLNA.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TBLNA
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File Nazioni G2 [TBLNA] inesistente"
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

      ***---
       TBLAG-ERR SECTION.
           use after error procedure on TBLAG.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TBLAG
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File Agenti G2 [TBLAG] inesistente"
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

      ***---
       TBLME-ERR SECTION.
           use after error procedure on TBLMe.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TBLME
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File Agenti G2 [TBLME] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TBLME] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TBLME] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TBLCA-ERR SECTION.
           use after error procedure on TBLCA.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-TBLCA
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File Categorie G2 [TBLCA] inesistente"
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

LUBEXX***---
       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File gruppi GDO [TGRUPGDO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TGRUPGDO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TGRUPGDO] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.               

      ***---
       ART-ERR SECTION.
           use after error procedure on ART.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ART
           when "35"
                display message "Impossibile procedere."
                         x"0d0a""File Fornitori G2 [ART] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [ART] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ART] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       END DECLARATIVES.

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
           move 0        to G2Agg-status.
           set RecLocked to false.
           set tutto-ok  to true.

      ***---
       OPEN-FILES.
           evaluate true
           when G2Agg-cli perform OPEN-IO-CLI
           when G2Agg-for perform OPEN-IO-FRN
           when G2Agg-cat perform OPEN-IO-TBLCA
           when G2Agg-age perform OPEN-IO-TBLAG 
           when G2Agg-naz perform OPEN-IO-TBLNA
           when G2Agg-art perform OPEN-IO-ART
           when G2Agg-gdo perform OPEN-IO-FPGRUPPICS
                          perform OPEN-IO-TBLCS
           end-evaluate.
           if tutto-ok
              open input recapiti
                         ABI
                         G2
                         DOCDI
                         clienti
                         ttipocli
                         agenti
                         tnazioni
                         tgrupgdo
                         articoli
                         TBLME
           end-if.

      ***---
       OPEN-IO-CLI.
           set tutto-ok to true.
           move "CLI"   to geslock-nome-file.
           open i-o CLI.
           if RecLocked

              string   "Il file dei Clienti di G2" 
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-CLI
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           end-if.

      ***---
       OPEN-IO-FRN.
           set tutto-ok   to true.
           move "FRN" to geslock-nome-file.
           open i-o FRN.
           if RecLocked

              string   "Il file dei Fornitori di G2" 
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-FRN
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           end-if.

      ***---
       OPEN-IO-TBLNA.
           set tutto-ok to true.
           move "TBLNA"   to geslock-nome-file.
           open i-o TBLNA.
           if RecLocked

              string   "Il file Nazioni di G2" 
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-TBLNA
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           end-if.

      ***---
       OPEN-IO-TBLCA.
           set tutto-ok to true.
           move "TBLCA"   to geslock-nome-file.
           open i-o TBLCA.
           if RecLocked

              string   "Il file Categorie di G2" 
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-TBLCA
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           end-if.

      ***---
       OPEN-IO-TBLAG.
           set tutto-ok to true.
           move "TBLAG"   to geslock-nome-file.
           open i-o TBLAG.
           if RecLocked

              string   "Il file Agenti di G2" 
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-TBLAG
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           end-if.        

      ***---
       OPEN-IO-ART.
           set tutto-ok   to true.
           move "ART" to geslock-nome-file.
           open i-o ART.
           if RecLocked

              string   "Il file degli articoli di G2" 
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-ART
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           end-if.      

      ***---
       OPEN-IO-FPGRUPPICS.
           set tutto-ok   to true.
           move "FPGRUPPICS" to geslock-nome-file.
           open i-o FPGRUPPICS.
           if RecLocked

              string   "Il file dei gruppi di G2" 
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-FPGRUPPICS
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           end-if.

      ***---
       OPEN-IO-TBLCS.
           set tutto-ok   to true.
           move "TBLCS" to geslock-nome-file.
           open i-o TBLCS.
           if RecLocked

              string   "Il file dei gruppi di G2" 
                x"0d0a""risulta in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità"
                x"0d0a""di sincronizzare i record." delimited size
                    into geslock-messaggio
              end-string

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform OPEN-IO-TBLCS
              when termina set errori to true
                           display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
              end-evaluate
           end-if.

      ***---
       ELABORAZIONE.
           if G2Agg-delete
              evaluate true
              when G2Agg-cli 
                   move G2Agg-codice to cli-codice-G2
                   perform READ-CLI-LOCK
                   if tutto-ok
                      delete CLI record invalid continue end-delete
                   end-if
              when G2Agg-for
                   move G2Agg-codice to frn-codice
                   perform READ-FRN-LOCK
                   if tutto-ok
                      delete FRN record invalid continue end-delete
                   end-if
              when G2Agg-naz
                   move "NA"          to tblna-codice1
                   move G2Agg-nazione to tblna-codice2
                   perform READ-TBLNA-LOCK
                   if tutto-ok
                      delete TBLNA record invalid continue end-delete
                   end-if
              when G2Agg-cat
                   move "CA"            to tblca-codice1 
                   move G2Agg-categoria to tblca-codice2
                   perform READ-TBLCA-LOCK
                   if tutto-ok
                      delete TBLCA record invalid continue end-delete
                   end-if
              when G2Agg-age
                   move "AG"         to tblag-codice1 
                   move G2Agg-agente to tblag-codice2
                   perform READ-TBLAG-LOCK
                   if tutto-ok
                      delete TBLAG record invalid continue end-delete
                   end-if  
              when G2Agg-art
                   move G2Agg-articolo to art-codice-G2
                   perform READ-ART-LOCK
                   if tutto-ok
                      delete ART record invalid continue end-delete
                   end-if 
              when G2Agg-gdo
                   move G2Agg-codice-GDO to fpgruppics-codice-geslux
                   perform READ-FPGRUPPICS-LOCK
                   if tutto-ok
                      move "CS"                 to tblcs-codice1
                      move fpgruppics-codice-cs to tblcs-codice2
                      perform READ-TBLCS-LOCK
                      if tutto-ok  
                         delete fpgruppics record
                                invalid continue
                         end-delete
                         delete TBLCS   record
                                invalid continue
                         end-delete
                      end-if
                   end-if
              end-evaluate  
           else
              if G2Agg-insert
                 evaluate true
                 when G2Agg-cli
                 when G2Agg-for
                      move spaces          to G2-codice
                      read G2 no lock invalid continue end-read
                      move "DI"            to docdi-codice1
                      move G2-codice-login to docdi-codice2
                      move 0               to docdi-codice3
                      read DOCDI no lock invalid continue end-read
                 end-evaluate
              end-if
              evaluate true
              when G2Agg-for 
                   set cli-tipo-F    to true
                   move G2Agg-codice to cli-codice
                   read clienti no lock 
                        invalid set errori to true 
                   end-read
                   if tutto-ok
                      perform ELABORAZIONE-FRN
                   end-if

              when G2Agg-cli 
                   set cli-tipo-C to true
                   move G2Agg-codice to cli-codice
                   read clienti no lock 
                        invalid set errori to true 
                   end-read
                   if tutto-ok
                      perform ELABORAZIONE-CLI
                   end-if

              when G2Agg-naz
                   move G2Agg-nazione to naz-codice
                   read tnazioni no lock 
                        invalid set errori to true 
                   end-read
                   if tutto-ok
                      perform ELABORAZIONE-TBLNA
                   end-if

              when G2Agg-cat
                   move G2Agg-categoria to tcl-codice
                   read ttipocli no lock 
                        invalid set errori to true 
                   end-read
                   if tutto-ok
                      perform ELABORAZIONE-TBLCA
                   end-if

              when G2Agg-age
                   move G2Agg-agente to age-codice
                   read agenti no lock 
                        invalid set errori to true 
                   end-read
                   if tutto-ok
                      perform ELABORAZIONE-TBLAG
                   end-if  

              when G2Agg-art
                   move G2Agg-articolo to art-codice
                   read articoli no lock 
                        invalid set errori to true 
                   end-read
                   if tutto-ok
                      perform ELABORAZIONE-ART
                   end-if

              when G2Agg-gdo
                   move G2Agg-codice-gdo to gdo-codice
                   read tgrupgdo no lock 
                        invalid set errori to true 
                   end-read
                   if tutto-ok
                      perform ELABORAZIONE-GDO
                   end-if

              end-evaluate
           end-if.

      ***---
       ELABORAZIONE-CLI.
           set tutto-ok to true.
           if G2Agg-insert
              initialize record-cli replacing numeric data by zeroes
                                         alphanumeric data by spaces
           else
              move G2Agg-codice    to cli-codice-G2
              perform READ-CLI-LOCK
           end-if.
           if tutto-ok
              initialize rec-rec
              move G2Agg-codice  to rec-codice
              read recapiti no lock invalid continue end-read
              if rec-ragsoc-1 = spaces
                 move cli-ragsoc-1  to rec-ragsoc-1
                 move cli-ragsoc-2  to rec-ragsoc-2
                 move cli-indirizzo to rec-indirizzo
                 move cli-cap       of cli-rec
                                    to rec-cap
                 move cli-localita  to rec-localita
                 move cli-prov      to rec-provincia
                 move cli-nazione   to rec-nazione
              end-if
              move cli-abi       to abi-codice-abi
              move cli-cab       to abi-codice-cab
              read ABI  no lock invalid continue end-read
              perform VALORIZZA-REC-CLI
              write record-cli invalid rewrite record-cli end-write
           end-if.

      ****---
       READ-CLI-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
      
           set tutto-ok to true.
           read CLI with lock invalid set errori to true end-read.
           if RecLocked
              string   "Il Cliente ", cli-codice-g2 " risulta"
                x"0d0a""in uso su G2 con altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""essere aggiornato." delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "CLI" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-CLI-LOCK
              when termina display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
                           set errori to true
              end-evaluate

           end-if.

      ***---
       VALORIZZA-REC-CLI.
      *    ** MOVE FROM CLIENTI **
           move cli-codice               of cli-rec 
             to cli-codice-G2            of record-cli.
           move cli-ragsoc-1             of cli-rec 
             to cli-descrizione1         of record-cli.
           move cli-ragsoc-2             of cli-rec 
             to cli-descrizione2         of record-cli.
           move cli-indirizzo            of cli-rec 
             to cli-via                  of record-cli.
           move cli-cap                  of cli-rec 
             to cli-cap                  of record-cli.
           move cli-localita             of cli-rec
             to cli-citta                of record-cli.
           move cli-prov                 of cli-rec
             to cli-provincia            of record-cli.
           move cli-nazione              of cli-rec
             to cli-codice-na            of record-cli.
           move cli-tel-1                of cli-rec
             to cli-telefono             of record-cli.
           move cli-fax                  of cli-rec
             to cli-fax                  of record-cli.
           move cli-pag                  of cli-rec
             to cli-codice-pa            of record-cli.
LUBEXX     move cli-gdo                  of cli-rec
LUBEXX       to gdo-codice.
LUBEXX     read tgrupgdo no lock 
LUBEXX          invalid move spaces to gdo-capogruppo
LUBEXX     end-read.
LUBEXX     move gdo-capogruppo to cli-codice-alternativo of record-cli.
LUBEXX     move gdo-codice-g2  to cli-codice-cs          of record-cli.
           if cli-spost-ric-agosto = "S" or = "s"
              move 10 to cli-giorno1
              move 08 to cli-mese1
              move 01 to cli-escluso-dal-giorno1
           end-if.
           if cli-spost-ric-dicembre = "S" or = "s"
              move 10 to cli-giorno2
              move 12 to cli-mese2
              move 01 to cli-escluso-dal-giorno2
           end-if.
           move cli-abi                  of cli-rec
             to cli-codice-abi           of record-cli.
           move cli-cab                  of cli-rec
             to cli-codice-cab           of record-cli.
           move cli-fido                 of cli-rec
             to cli-fido                 of record-cli.
           move cli-iva-ese              of cli-rec
             to cli-codice-iv            of record-cli.
           move cli-dich-esp             of cli-rec
             to cli-numero-richiesta     of record-cli.
           move cli-data-dich            of cli-rec
             to cli-data-richiesta       of record-cli.
           move cli-num-reg              of cli-rec
             to cli-numero-registrazione of record-cli.
           move cli-data-reg             of cli-rec
             to cli-data-registrazione   of record-cli.
           move cli-ragsoc-1             of cli-rec 
             to cli-nome1-a              of record-cli.
           move cli-ragsoc-2             of cli-rec 
             to cli-nome2-a              of record-cli.
           move cli-indirizzo            of cli-rec 
             to cli-via-a                of record-cli.
           move cli-cap                  of cli-rec 
             to cli-cap-a                of record-cli.
           move cli-localita             of cli-rec
             to cli-citta-a              of record-cli.
           move cli-prov                 of cli-rec
             to cli-provincia-a          of record-cli.
           |Il codice agente G2 è di 3 crt.
           move cli-agente               of cli-rec(3:3)
             to cli-codice-ag            of record-cli.
           move cli-tipo                 of cli-rec
             to cli-codice-ca            of record-cli.
           move cli-note                 of cli-rec
             to cli-commento             of record-cli.
           move cli-data-ultima-modifica of cli-rec
             to cli-data-aggiornamento   of record-cli.
           move cli-web                  of cli-rec
             to cli-internet             of record-cli.
           move cli-nazione              of cli-rec
             to cli-codice-na-a          of record-cli.
           move cli-email                of cli-rec
             to cli-e-mail               of record-cli.
           
      *    ** SETTAGGIO FLAGS **
           move "R" to cli-tipo-fattura  of record-cli.
           move 1   to cli-cp-vendite    of record-cli.

      *    ** MOVE FROM OTHER FILES **
           |RECAPITI
           move rec-ragsoc-1             to cli-nome1-f.
           move rec-ragsoc-2             to cli-nome2-f.
           move rec-indirizzo            to cli-via-f.
           move rec-cap                  to cli-cap-f.
           move rec-localita             to cli-citta-f.
           move rec-provincia            to cli-provincia-f.
           move rec-nazione              to cli-codice-na-f.
           |ABI
           move abi-banca1               to cli-banca1.
           move abi-banca2               to cli-banca2.   

           if cli-prov not = "EE"
              move cli-piva                 of cli-rec
                to cli-partita-iva          of record-cli
              move cli-codfis               of cli-rec
                to cli-codice-fiscale       of record-cli
           else
              move spaces to cli-partita-iva    of record-cli
                             cli-codice-fiscale of record-cli
LUBEXX*****     if cli-prov = "EE" and 
      *****        cli-partita-iva-intra of record-cli = spaces
              inspect cli-piva replacing trailing spaces by low-value
              move 0 to TotChar
              inspect cli-piva tallying TotChar 
                               for characters before low-value
              perform varying ComoChar from 1 by 1
                        until ComoChar > TotChar
                 move cli-piva(ComoChar:1) to como-char
                 if como-char is numeric
                    exit perform
                 end-if
              end-perform              
              inspect cli-piva replacing trailing low-value by spaces
              if ComoChar > 0 and ComoChar < TotChar
                 move cli-piva(ComoChar:) 
                   to cli-partita-iva-intra of record-cli
              end-if
LUBEXX*****     end-if.
LUBEXX*****     if cli-prov = "EE" and 
      *****        cli-codice-iva-estero of record-cli = spaces
              inspect cli-piva replacing trailing spaces by low-value
              move 0 to TotChar
              inspect cli-piva tallying TotChar 
                               for characters before low-value
              perform varying ComoChar from 1 by 1
                        until ComoChar > TotChar
                 move cli-piva(ComoChar:1) to como-char
                 if como-char is numeric
                    exit perform
                 end-if
              end-perform              
              inspect cli-piva replacing trailing low-value by spaces
              if ComoChar > 0 and ComoChar < TotChar
                 move cli-piva(ComoChar:) 
                   to cli-codice-iva-estero of record-cli
              end-if
LUBEXX     end-if.

      *    ** VALORIZZAZIONE DEFAULT SE NUOVO **
           if G2Agg-insert
              |Richiesta in data 28/02/06 di Trivella.
              |I mastri sono già stati decisi!!!
      *****        move docdi-tblpc-cli       to cli-codice-pc
              move "0313"                to cli-codice-pc
              move docdi-tblce-cli       to cli-codice-ce
              move "S"                   to cli-partite  
              move "S"                   to cli-scadenze
              move "S"                   to cli-allegato
           end-if.
           
      ***---
       ELABORAZIONE-FRN.
           set tutto-ok to true.
           if G2Agg-insert
              initialize record-frn replacing numeric data by zeroes
                                         alphanumeric data by spaces
           else
              move G2Agg-codice    to frn-codice
              perform READ-FRN-LOCK
           end-if.
           if tutto-ok
              move cli-abi       to abi-codice-abi
              move cli-cab       to abi-codice-cab
              read ABI  no lock invalid continue end-read
              perform VALORIZZA-REC-FRN
              write record-frn invalid rewrite record-frn end-write
           end-if.
           
      ****---
       READ-FRN-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
      
           set tutto-ok to true.
           read FRN with lock invalid set errori to true end-read.
           if RecLocked
              string   "Il Fornitore ", frn-codice , " risulta"
                x"0d0a""in uso su G2 con altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""essere aggiornato." delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "FRN" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-FRN-LOCK
              when termina display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
                           set errori to true
              end-evaluate
           end-if.

      ***---
       VALORIZZA-REC-FRN.
      *    ** MOVE FROM CLIENTI **
           move cli-codice               of cli-rec 
             to frn-codice               of record-frn.
           move cli-ragsoc-1             of cli-rec 
             to frn-descrizione1         of record-frn.
           move cli-ragsoc-2             of cli-rec 
             to frn-descrizione2         of record-frn.
           move cli-indirizzo            of cli-rec 
             to frn-via                  of record-frn.
           move cli-cap                  of cli-rec 
             to frn-cap                  of record-frn.
           move cli-localita             of cli-rec
             to frn-citta                of record-frn.
           move cli-prov                 of cli-rec
             to frn-provincia            of record-frn.
           move cli-nazione              of cli-rec
             to frn-codice-na            of record-frn.
           move cli-tel-1                of cli-rec
             to frn-telefono             of record-frn.
           move cli-fax                  of cli-rec
             to frn-fax                  of record-frn.   

           if cli-prov not = "EE"
              move cli-codfis               of cli-rec
                to frn-codice-fiscale       of record-frn
              move cli-piva                 of cli-rec
                to frn-partita-iva          of record-frn
           else
              move spaces to frn-codice-fiscale of record-frn
                             frn-partita-iva    of record-frn
LUBEXX*****     if cli-prov = "EE" and 
      *****        frn-partita-iva-intra of record-frn = spaces
              inspect cli-piva replacing trailing spaces by low-value
              move 0 to TotChar
              inspect cli-piva tallying TotChar 
                               for characters before low-value
              perform varying ComoChar from 1 by 1
                        until ComoChar > TotChar
                 move cli-piva(ComoChar:1) to como-char
                 if como-char is numeric
                    exit perform
                 end-if
              end-perform              
              inspect cli-piva replacing trailing low-value by spaces
              if ComoChar > 0 and ComoChar < TotChar
                 move cli-piva(ComoChar:) 
                   to frn-partita-iva-intra of record-frn
              end-if
LUBEXX*****     end-if.
LUBEXX*****     if cli-prov = "EE" and 
      *****        frn-codice-iva-estero of record-frn = spaces
              inspect cli-piva replacing trailing spaces by low-value
              move 0 to TotChar
              inspect cli-piva tallying TotChar 
                               for characters before low-value
              perform varying ComoChar from 1 by 1
                        until ComoChar > TotChar
                 move cli-piva(ComoChar:1) to como-char
                 if como-char is numeric
                    exit perform
                 end-if
              end-perform              
              inspect cli-piva replacing trailing low-value by spaces
              if ComoChar > 0 and ComoChar < TotChar
                 move cli-piva(ComoChar:) 
                   to frn-codice-iva-estero of record-frn
              end-if
LUBEXX     end-if.
                                                       
           move cli-pag                  of cli-rec
             to frn-codice-pa            of record-frn.
           move cli-abi                  of cli-rec
             to frn-codice-abi           of record-frn.
           move cli-cab                  of cli-rec
             to frn-codice-cab           of record-frn.
           move cli-fido                 of cli-rec
             to frn-fido                 of record-frn.
           move cli-iva-ese              of cli-rec
             to frn-codice-iv            of record-frn.
           move cli-dich-esp             of cli-rec
             to frn-numero-richiesta     of record-frn.
           move cli-data-dich            of cli-rec
             to frn-data-richiesta       of record-frn.
           move cli-num-reg              of cli-rec
             to frn-numero-registrazione of record-frn.
           move cli-data-reg             of cli-rec
             to frn-data-registrazione   of record-frn.
           move cli-ragsoc-1             of cli-rec 
             to frn-nome1-a              of record-frn.
           move cli-ragsoc-2             of cli-rec 
             to frn-nome2-a              of record-frn.
           move cli-indirizzo            of cli-rec 
             to frn-via-a                of record-frn.
           move cli-cap                  of cli-rec 
             to frn-cap-a                of record-frn.
           move cli-localita             of cli-rec
             to frn-citta-a              of record-frn.
           move cli-prov                 of cli-rec
             to frn-provincia-a          of record-frn.
           move cli-note                 of cli-rec
             to frn-commento             of record-frn.
           move cli-data-ultima-modifica of cli-rec
             to frn-data-aggiornamento   of record-frn.
           move cli-web                  of cli-rec
             to frn-internet             of record-frn.
           move cli-nazione              of cli-rec
             to frn-codice-na-a          of record-frn.
           move cli-email                of cli-rec
             to frn-e-mail               of record-frn.

      *    ** MOVE FROM OTHER FILES **
           |ABI
           move abi-banca1               to frn-banca1.
           move abi-banca2               to frn-banca2.
           
      *    ** VALORIZZAZIONE DEFAULT SE NUOVO **
           if G2Agg-insert
              |Richiesta in data 28/02/06 di Trivella.
              |I mastri sono già stati decisi!!!
      *****        move docdi-tblpc-frn       to frn-codice-pc
              move "2015"                to frn-codice-pc
              move docdi-tblce-frn       to frn-codice-ce
              move "S"                   to frn-partite  
              move "S"                   to frn-scadenze
              move "S"                   to frn-allegato
           end-if.

      ***---
       ELABORAZIONE-TBLNA.
           set tutto-ok to true.
           if G2Agg-insert
              initialize record-tblna replacing numeric data by zeroes
                                           alphanumeric data by spaces
           else
              move "NA"          to tblna-codice1
              move G2Agg-nazione to tblna-codice2
              perform READ-TBLNA-LOCK
           end-if.

           if tutto-ok
              initialize naz-rec
              move G2Agg-nazione  to naz-codice
              read tnazioni no lock invalid continue end-read
              perform VALORIZZA-REC-TBLNA
              write record-tblna invalid rewrite record-tblna end-write
           end-if.

      ****---
       READ-TBLNA-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
      
           set tutto-ok to true.
           read TBLNA with lock invalid set errori to true end-read.
           if RecLocked
              string   "La Nazione ", tblna-codice2 " risulta"
                x"0d0a""in uso su G2 con altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""essere aggiornato." delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "TBLNA" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-TBLNA-LOCK
              when termina display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
                           set errori to true
              end-evaluate

           end-if.

      ***---
       VALORIZZA-REC-TBLNA.
           move "NA"            to tblna-codice1.
           move naz-codice      to tblna-codice2.
           move naz-descrizione to tblna-descrizione1.
           move naz-codice      to tblna-codice-iso.

      ***---
       ELABORAZIONE-TBLCA.
           set tutto-ok to true.
           if G2Agg-insert
              initialize record-tblca replacing numeric data by zeroes
                                           alphanumeric data by spaces
           else
              move "CA"            to tblca-codice1
              move G2Agg-categoria to tblca-codice2
              perform READ-TBLCA-LOCK
           end-if.

           if tutto-ok
              initialize tcl-rec
              move G2Agg-categoria to tcl-codice
              read ttipocli no lock invalid continue end-read
              perform VALORIZZA-REC-TBLCA
              write record-tblca invalid rewrite record-tblca end-write
           end-if.

      ****---
       READ-TBLCA-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
      
           set tutto-ok to true.
           read TBLCA with lock invalid set errori to true end-read.
           if RecLocked
              string   "La Categoria ", tblca-codice2 " risulta"
                x"0d0a""in uso su G2 con altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""essere aggiornato." delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "TBLCA" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-TBLCA-LOCK
              when termina display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
                           set errori to true
              end-evaluate

           end-if.

      ***---
       VALORIZZA-REC-TBLCA.
           move "CA"            to tblca-codice1.
           move tcl-codice      to tblca-codice2.
           move tcl-descrizione to tblca-descr1.

      ***---
       ELABORAZIONE-TBLAG.
           set tutto-ok to true.
           if G2Agg-insert
              initialize record-tblag replacing numeric data by zeroes
                                           alphanumeric data by spaces
           else
              move "AG"         to tblag-codice1
              move G2Agg-agente to tblag-codice2
              perform READ-TBLAG-LOCK
           end-if.

           if tutto-ok
              initialize age-rec
              move G2Agg-agente to age-codice
              read agenti no lock invalid continue end-read
              perform VALORIZZA-REC-TBLAG
              write record-tblag invalid rewrite record-tblag end-write
           end-if.

      ****---
       READ-TBLAG-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
      
           set tutto-ok to true.
           read TBLAG with lock invalid set errori to true end-read.
           if RecLocked
              string   "L'Agente ", tblag-codice2 " risulta"
                x"0d0a""in uso su G2 con altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""essere aggiornato." delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "TBLAG" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-TBLAG-LOCK
              when termina display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
                           set errori to true
              end-evaluate

           end-if.

      ***---
       VALORIZZA-REC-TBLAG.
           move "AG"            to tblag-codice1.
           |Essendo di 3 crt su CLI dev'essere di 3 anche
           |nel file degli agenti essendo alfa
           move age-codice(3:3) to tblag-codice2.
           move age-ragsoc-1    to tblag-descrizione1.
           move age-ragsoc-2    to tblag-descrizione2.
           
      ***---
       ELABORAZIONE-ART.
           set tutto-ok to true.
           if G2Agg-insert
              initialize record-art replacing numeric data by zeroes
                                         alphanumeric data by spaces
           else
              move G2Agg-articolo    to art-codice-G2
              perform READ-ART-LOCK
           end-if.
           if tutto-ok          
              perform VALORIZZA-REC-ART
              write record-art invalid rewrite record-art end-write
           end-if.
           
      ****---
       READ-ART-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
      
           set tutto-ok to true.
           read ART with lock invalid set errori to true end-read.
           if RecLocked
              string   "L'articolo ", art-codice , " risulta"
                x"0d0a""in uso su G2 con altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""essere aggiornato." delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "ART" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-FRN-LOCK
              when termina display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
                           set errori to true
              end-evaluate
           end-if.

      ***---
       VALORIZZA-REC-ART.
      *    ** MOVE FROM ARTICOLI ** 
           move art-codice to art-codice-G2.            
           move art-descrizione1 to art-descrizione1-G2.
           move art-descrizione2 to art-descrizione2-G2.
           accept art-data-aggiornamento from century-date.
           move art-marca-prodotto(2:) to art-codice-cm.

           move low-value to record-tblme.
           
           set trovato-me to false.
           move "ME" to tblme-codice1.
           start tblme key >= TBLMe-CODICE
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tblme next at end exit perform end-read
                    if tblme-codice1 not = "ME" 
                       exit perform
                    end-if
                    evaluate art-cod-doganale(1:4)
                    when "2710" 
                    when "3403" if tblme-cod-soggetto-oli
                                   set trovato-me to true
                                   exit perform
                                end-if
                    when "8507" if tblme-cod-soggetto-bat 
                                   set trovato-me to true
                                   exit perform
                                end-if
                    when other  if tblme-cod-soggetto-alt 
                                   set trovato-me to true
                                   exit perform
                                end-if
                    end-evaluate
                 end-perform
           end-start.
           if not trovato-me
              display message "ARTICOLO: " art-codice              
           x"0d0a""Categoria merceologica: " art-cod-doganale(1:4) 
           x"0d0a""non trovata sul file TBLME di G2"
           x"0d0a""Verrà inserito il valore vuoto nel file ART di G2."         
           x"0d0a""Inserire il valore merceologico in G2 dopodiché"
           x"0d0a""effettuare il salvataggio dell'articolo in GESLUX"
           x"0d0a""o lanciare il programma G2Agg-art"
           x"0d0a""per l'allineamento di questo valore."
                         title titolo
                          icon 2                    
              move spaces    to art-codice-me
           else
              move tblme-codice2    to art-codice-me
           end-if.
           move art-classe-1(2:) to art-codice-in.
           move "NR" to art-codice-um.
           if art-litri > 0
              move "LT" to art-codice-um1
           end-if.
           
      ***---
       ELABORAZIONE-GDO.
           set tutto-ok to true.
           if G2Agg-insert
              initialize record-fpgruppics
                         record-tblcs
                         replacing numeric data by zeroes
                              alphanumeric data by spaces
           else
              move G2Agg-codice-gdo to fpgruppics-codice-geslux
              perform READ-FPGRUPPICS-LOCK
              if tutto-ok               
                 move "CS"                 to tblcs-codice1
                 move fpgruppics-codice-cs to tblcs-codice2
                 perform READ-TBLCS-LOCK
              end-if
           end-if.
           if tutto-ok
              move G2Agg-codice-gdo to gdo-codice
              read tgrupgdo no lock invalid continue end-read 
              perform VALORIZZA-REC-FPGRUPPICS
              write record-fpgruppics 
                    invalid rewrite record-fpgruppics
              end-write
              perform VALORIZZA-REC-TBLCS
              write record-tblcs 
                    invalid rewrite record-tblcs 
              end-write
           end-if.  
           
      ****---
       READ-FPGRUPPICS-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
      
           set tutto-ok to true.
           read fpgruppics with lock invalid set errori to true end-read
           if RecLocked
              string   "Il codice ", fpgruppics-codice-geslux , 
                       " risulta"
                x"0d0a""in uso su G2 con altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""essere aggiornato." delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "FPGRUPPICS" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-FPGRUPPICS-LOCK
              when termina display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
                           set errori to true
              end-evaluate
           end-if.
           
      ****---
       READ-TBLCS-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
      
           set tutto-ok to true.
           read TBLCS with lock invalid set errori to true end-read
           if RecLocked
              string   "Il codice ", fpgruppics-codice-geslux , 
                       " risulta"
                x"0d0a""in uso su G2 con altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad"
                x"0d0a""essere aggiornato." delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              move "FPGRUPPICS" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova perform READ-TBLCS-LOCK
              when termina display message box "Operazione interrotta!"
                                     title titolo
                                      icon 2
                           move -1 to G2Agg-status
                           set errori to true
              end-evaluate
           end-if.  

      ***---             
       VALORIZZA-REC-FPGRUPPICS.
      *    ** MOVE FROM TGRUPGDO ** 
           move gdo-codice    to fpgruppics-codice-geslux.
           move gdo-codice-G2 to fpgruppics-codice-cs
           inspect gdo-codice replacing trailing spaces by low-value.
           initialize fpgruppics-descrizione1.
           string  gdo-codice delimited low-value
                   " - "      delimited size
                   gdo-intestazione delimited size
              into fpgruppics-descrizione1
           end-string.


      ***---             
       VALORIZZA-REC-TBLCS.
      *    ** MOVE FROM TGRUPGDO **  
           move "CS"                    to tblcs-codice1.
           move gdo-codice-G2           to tblcs-codice2.
           inspect gdo-codice replacing trailing spaces by low-value.
           initialize tblcs-descr1.
           string  gdo-codice delimited low-value
                   " - "      delimited size
                   gdo-intestazione delimited size
              into tblcs-descr1
           end-string.

      ***---
       CLOSE-FILES.
           close abi 
                 G2 
                 DOCDI 
                 clienti 
                 recapiti
                 ttipocli
                 agenti
                 tnazioni
                 tgrupgdo
                 articoli
                 TBLME.

           evaluate true
           when G2Agg-cli close CLI
           when G2Agg-for close FRN
           when G2Agg-cat close TBLCA
           when G2Agg-age close TBLAG
           when G2Agg-naz close TBLNA
           when G2Agg-art close ART
           when G2Agg-gdo close FPGRUPPICS TBLCS
           end-evaluate.

      ***---
       EXIT-PGM.
           goback.           
