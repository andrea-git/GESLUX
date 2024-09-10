       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      primanotaescad.
       AUTHOR.                          Andrea.
       REMARKS. Partendo dal file DVT contenente i documenti di
                vendita creo i files di prima nota PNT(testata)
                PNR(righe) PNI(iva) e degli scadenziari
                PAT(testata) PAR(righe) PAS(scadenze). Inoltre, così
                come da esigenze di Gestionale2 vado ad aggiornare i
                contatori MAZ e CLZ per quell'anno di esercizio.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "CLI.sl".
           copy "G2.sl".
           copy "TBLCO.sl".
           copy "TBLDO.sl".
           copy "DOCCN.sl".
           copy "PAT.sl".
           copy "PAS.sl".
           copy "PAR.sl".
           copy "PNT.sl".
           copy "PNR.sl".
           copy "PNI.sl".
           copy "CLZ.sl".
           copy "MAZ.sl".
           copy "MAS.sl".
           copy "GDVT.sl".
           copy "tmp-iva.sl".
           copy "FRN.sl".
           copy "FRZ.sl".
           copy "contab.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "CLI.fd".
           copy "G2.fd".
           copy "TBLCO.fd".
           copy "TBLDO.fd".
           copy "DOCCN.fd".
           copy "PAT.fd".
           copy "PAS.fd".
           copy "PAR.fd".
           copy "PNT.fd".
           copy "PNR.fd".
           copy "PNI.fd".
           copy "CLZ.fd".
           copy "MAZ.fd".
           copy "MAS.fd".
           copy "GDVT.fd".
           copy "tmp-iva.fd".
           copy "FRN.fd".
           copy "FRZ.fd".
           copy "contab.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo                    value "PRIMA NOTA E SCADENZIARI".

      *FILE-STATUS
       77  status-CLI                pic xx.
       77  status-G2                 pic xx.
       77  status-TBLCO              pic xx.
       77  status-TBLDO              pic xx.
       77  status-DOCCN              pic xx.
       77  status-PAT                pic xx.
       77  status-PAS                pic xx.
       77  status-PAR                pic xx.
       77  status-PNT                pic xx.
       77  status-PNR                pic xx.
       77  status-PNI                pic xx.
       77  status-CLZ                pic xx.
       77  status-MAZ                pic xx.
       77  status-MAS                pic xx.
       77  status-GDVT               pic xx.
       77  status-tmp-iva            pic xx.
       77  status-FRN                pic xx.
       77  status-FRZ                pic xx.
       77  status-contab             pic xx.

       77  SELMAZ                    pic x(256).
       77  SELCLZ                    pic x(256).
       77  SELFRZ                    pic x(256).

       77  path-tmp-iva              pic x(256).

      * VARIABILI
       77  idx                       pic 99.
       77  dec-esercizio             pic x.
       77  contatore-pat             pic 9(18).
       77  contatore-pnt             pic 9(18).
       77  contatore-protocollo      pic 9(18).
       77  importo-pri               pic 9(12)v99.
       77  counter                   pic 9(10).
       77  counter2                  pic 9(10).
       77  counter-edit              pic z(10).
       77  esercizio-G2              pic x(2).

      * FLAGS
       01  filler                    pic 9.
           88 contab-protocollo      value 1.
           88 contab-pnt             value 2.
           88 contab-pat             value 3.
       77  controllo                 pic xx.
           88  tutto-ok              value "OK".
           88  errori                value "ER".
       77  filler                    pic 9.
           88 RecLocked              value 1, false 0.
       77  filler                    pic 9.
           88 trovato                value 1, false 0.
       77  filler                    pic 9.
           88 PrimaVolta             value 1, false 0.

       LINKAGE SECTION.
       copy "link-primanotaescad.def".

      ******************************************************************
       PROCEDURE DIVISION using primanotaescad-linkage.

       DECLARATIVES.
      ***---
       G2-ERR SECTION.
           use after error procedure on G2.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-G2
           when "39"
                display message "File Mismatch size!"
                          title "G2"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "G2"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
              x"0d0a""Tabella parametri G2 [G2] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TBLCO-ERR SECTION.
           use after error procedure on TBLCO.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tblco
           when "39"
                display message "File Mismatch size!"
                          title "TBLCO"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "TBLCO"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
              x"0d0a""Tabella causali contabili [TBLCO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---  
       TBLDO-ERR SECTION.
           use after error procedure on TBLDO.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tbldo
           when "39"
                display message "File Mismatch size!"
                          title "TBLDO"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "TBLDO"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Docum. di vendita [TBLDO] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---  
       PAT-ERR SECTION.
           use after error procedure on PAT.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pat
           when "39"
                display message "File Mismatch size!"
                          title "PAT"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "PAT"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Scadenze [PAT] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PAS-ERR SECTION.
           use after error procedure on PAS.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pas
           when "39"
                display message "File Mismatch size!"
                          title "PAS"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "PAS"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Scadenze [PAS] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PAR-ERR SECTION.
           use after error procedure on PAR.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-par
           when "39"
                display message "File Mismatch size!"
                          title "PAR"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "PAR"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Scadenze [PAR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PNT-ERR SECTION.
           use after error procedure on pnt.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pnt
           when "39"
                display message "File Mismatch size!"
                          title "PNT"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "PNT"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Prima Nota [PNT] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PNR-ERR SECTION.
           use after error procedure on pnr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pnr
           when "39"
                display message "File Mismatch size!"
                          title "PNR"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "PNR"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Prima Nota [PNR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PNI-ERR SECTION.
           use after error procedure on pni.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pni
           when "39"
                display message "File Mismatch size!"
                          title "PNI"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "PNI"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Prima Nota [PNI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       CLI-ERR SECTION.
           use after error procedure on cli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-cli
           when "39"
                display message "File Mismatch size!"
                          title "CLI"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "CLI"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Clienti [CLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       CLZ-ERR SECTION.
           use after error procedure on clz.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clz
           when "39"
                display message "File Mismatch size!"
                          title "CLZ"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "CLZ"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Contatori [CLZ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       FRN-ERR SECTION.
           use after error procedure on frn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-frn
           when "39"
                display message "File Mismatch size!"
                          title "FRN"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "FRN"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Fornitori [FRN] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       FRZ-ERR SECTION.
           use after error procedure on frz.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-frz
           when "39"
                display message "File Mismatch size!"
                          title "FRZ"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "FRZ"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Contatori [FRZ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       MAZ-ERR SECTION.
           use after error procedure on maz.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-maz
           when "39"
                display message "File Mismatch size!"
                          title "MAZ"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "MAZ"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Contatori [MAZ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       MAS-ERR SECTION.
           use after error procedure on mas.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mas
           when "39"
                display message "File Mismatch size!"
                          title "MAS"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "MAS"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Mastri [MAS] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       GDVT-ERR SECTION.
           use after error procedure on gdvt.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-gdvt
           when "39"
                display message "File Mismatch size!"
                          title "GDVT"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "GDVT"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Docum. di vendita [GDVT] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       DOCCN-ERR SECTION.
           use after error procedure on doccn.
           set tutto-ok  to true.
           evaluate status-doccn
           when "39"
                display message "File Mismatch size!"
                          title "DOCCN"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "DOCCN"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Contatori [DOCCN] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
                initialize geslock-messaggio
                string "La tabella dei contatori G2 risulta"
                x"0d0a""in uso su altro terminale."
                x"0d0a""Impossibile procedere!" delimited size
                x"0d0a""Chiudere Gestionale 2!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "doccn"      to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o doccn allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       TMP-IVA-ERR SECTION.
           use after error procedure on tmp-iva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-iva
           when "39"
                display message "File Mismatch size!"
                          title "TMP-IVA"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "TMP-IVA"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""File TMP [TMP-IVA] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       CONTAB-ERR SECTION.
           use after error procedure on contab.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-contab
           when "39"
                display message "File Mismatch size!"
                          title "CONTAB"
                           icon 3
                set errori to true
           when "98"
                display message "Indexed file corrupt!"
                          title "CONTAB"
                           icon 3
                set errori to true
           when "35"
                display message "Impossibile procedere."
               x"0d0a""File [CONTAB] inesistente"
                          title titolo
                           icon 2
                set errori to true
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
           set  PrimaVolta        to true.
           set  tutto-ok          to true.
           move link-selclz       to selclz.
           move link-selmaz       to selmaz.
           move link-path-tmp-iva to path-tmp-iva.
           accept esercizio-G2 from environment "ESERCIZIO_G2".

      ***---
       OPEN-FILES.
           perform OPEN-DOCCN-LOCK.
           if tutto-ok
              open i-o   pnr pnt pni pas pat par clz maz mas contab
              open input gdvt tblco tbldo G2 CLI 
              if path-tmp-iva not = spaces
                 open input tmp-iva
              end-if
           end-if.

      ***---
       OPEN-DOCCN-LOCK.
           open i-o doccn allowing readers.

      ***---
       ELABORAZIONE.
           initialize contab-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           accept contab-data from century-date.
           accept contab-ora  from time.
           move link-tipo-doc to contab-tipo.
           write contab-rec invalid continue end-write.

           move 0 to link-pn-result.
           move spaces to G2-chiave.
           read G2 no lock invalid continue end-read.

           |Recupero l'ultimo numero progressivo per prima nota
           |e scadenziari per evitare sovrapposizione di contatore
           move high-value to  pnt-codice.
           start pnt key is <= pnt-codice
                 invalid move 0 to contatore-PNT
             not invalid read pnt previous no lock
                         move pnt-progressivo to contatore-PNT
           end-start.

           move high-value to  pat-codice.
           start pat key is <= pat-codice
                 invalid move 0 to contatore-PAT
             not invalid read pat previous no lock
                         move pat-progressivo to contatore-PAT
           end-start.

           move low-values to record-dvt.
           start gdvt key is >= dvt-codice
                 invalid set errori to true
           end-start.
           if tutto-ok

              |RIPULISCO LA SCREEN DAL CONTATORE
              display "                          "
                 upon link-handle at column 34
                                       line 25
              display "                          "
                 upon link-handle at column 30
                                       line 26
              ||||||||

              perform until 1 = 2
                 read gdvt next at end exit perform end-read
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 34
                                             line 25
                    move 0 to counter2
                    if counter = 100
                       display "PRIMANOT COUNTER" 
                          upon link-handle at column 30
                                                line 26
                    end-if
                 end-if
                 if PrimaVolta
                    set PrimaVolta to false
                    perform READ-DOCCN-PRIMA-NOTA
                    perform READ-DOCCN-SCAD
                    perform READ-DOCCN-PROTOCOLLO
                 end-if
                 move 1 to link-pn-result
                 add  1 to contatore-pnt
                 add  1 to contatore-protocollo
                 add  1 to contatore-pat
                 perform READ-CAUSALE-CONTABILE
LUBEXX           initialize record-cli replacing numeric data by zeroes
LUBEXX                                      alphanumeric data by spaces
                 move dvt-codice-cfi to cli-codice-G2
                 read CLI no lock invalid continue end-read
                 perform CREA-PNR
                 perform CREA-PNI
                 perform CREA-PNT
                 perform AGGIORNA-CONTATORE-PNT
                 perform AGGIORNA-CONTATORE-PROTOCOLLO

                 |devo tenermi in linea la prima riga
                 |per valorizzare i campi successivi
                 move contatore-pnt to pnr-progressivo
                 move 1             to pnr-riga
                 read pnr no lock invalid continue end-read

                 perform CREA-PAS
                 perform CREA-PAR
                 perform CREA-PAT
                 perform AGGIORNA-CONTATORE-PAT

                 add 1 to contab-tot-documento
                 rewrite contab-rec invalid continue end-rewrite

                 call "w$flush"
              end-perform
           end-if.

      ***---
       CREA-PNR.
           move contatore-pnt          to pnr-progressivo.
           move 1 to pnr-riga.
           |RIGA TOTALE
           initialize resto-record-pnr replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           move tblco-tipo-cfm(1)   to pnr-tipo-cfm.
           move dvt-codice-cfi      to pnr-codice-cfm.
           move esercizio-G2        to pnr-esercizio.
           move dvt-data-fattura    to pnr-data-registrazione.
           move dvt-codice-co       to pnr-codice-co.
           move tblco-descrizione1  to pnr-descrizione1.
           move cli-descrizione1    to pnr-descrizione2.
           move tblco-segno-cfm(1)  to pnr-dare-avere.
           move tblco-tipo-cfm(3)   to pnr-tipo-cp.
           move dvt-codice-mas      to pnr-codice-cp.
           move dvt-importo-totale  to pnr-importo pnr-importo-va.
           write record-pnr invalid continue end-write.
           perform MOVPRI.

           move contatore-pnt          to pnr-progressivo.
           move 2 to pnr-riga.
           |RIGA IVA
           initialize resto-record-pnr replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           move tblco-tipo-cfm(2)   to pnr-tipo-cfm.
           move tblco-codice-cfm(2) to pnr-codice-cfm.
           move esercizio-G2        to pnr-esercizio.
           move dvt-data-fattura    to pnr-data-registrazione.
           move dvt-codice-co       to pnr-codice-co.
           move tblco-descrizione1  to pnr-descrizione1.
           move cli-descrizione1    to pnr-descrizione2.
           move tblco-segno-cfm(2)  to pnr-dare-avere.
           move tblco-tipo-cfm(2)   to pnr-tipo-cp.
           move cli-codice-G2       to pnr-codice-cp.
           move dvt-iva-totale      to pnr-importo pnr-importo-va.
           move "S"                 to pnr-riga-iva.
           write record-pnr invalid continue end-write.
           perform MOVPRI.

           move contatore-pnt          to pnr-progressivo.
           move 3 to pnr-riga.
           |RIGA CONTROPARTITA
           initialize resto-record-pnr replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           move dvt-codice-co         to pnr-codice-co.
           move tblco-descrizione1    to pnr-descrizione1.
           move cli-descrizione1      to pnr-descrizione2.
           move dvt-codice-mas        to pnr-codice-cfm.
           move esercizio-G2          to pnr-esercizio.
           move dvt-data-fattura      to pnr-data-registrazione.
           move tblco-tipo-cfm(3)     to pnr-tipo-cfm.
           move tblco-segno-cfm(3)    to pnr-dare-avere.
           move tblco-tipo-cfm(1)     to pnr-tipo-cp.
           move cli-codice-G2         to pnr-codice-cp.
           move dvt-imponibile-totale to pnr-importo pnr-importo-va.
           write record-pnr invalid   continue end-write.
           perform MOVPRI.

      ***---
       CREA-PNI.
           initialize record-pni 
                      tmp-iv-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move contatore-pnt   to pni-progressivo 
           if path-tmp-iva not = spaces
              move dvt-progressivo to tmp-iv-progressivo
              read tmp-iva no lock invalid continue end-read
              perform varying idx from 1 by 1 until idx > 6
                 if tmp-iv-el-cod-iva(idx) = spaces exit perform end-if
                 move tmp-iv-el-cod-iva(idx)   to pni-tbliv-codice(idx)
                 move tmp-iv-el-iva(idx)       to pni-imposta(idx)
                                                  pni-imposta-va(idx)
                                                  pni-detraibile(idx)
                                                  pni-detraibile-va(idx)
                 move tmp-iv-el-imponibile(idx)to pni-imponibile(idx)
                                                  pni-imponibile-va(idx)
              end-perform
           end-if.
           move tblco-tipo-cfm(1) to pni-tipo-cfm.
           move cli-codice-G2     to pni-codice-cfm.
           write record-pni invalid continue end-write.

      ***---
       CREA-PNT.
           initialize record-pnt replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move contatore-pnt          to pnt-progressivo.
           move dvt-data-fattura       to pnt-data-registrazione.
           move dvt-numero-fattura     to pnt-numero-documento.
           move tblco-tipo-documento   to pnt-tipo-documento.
           move dvt-data-fattura       to pnt-data-documento.
           move contatore-protocollo   to pnt-numero-protocollo.
           move tblco-tipo-protocollo  to pnt-tipo-protocollo.
           move dvt-codice-co          to pnt-codice-co.
           move tblco-descrizione1     to pnt-descrizione1(1).
           move cli-descrizione1       to pnt-descrizione2(1).
           move esercizio-G2           to pnt-esercizio.
           move pnr-riga               to pnt-ultima-riga.
           move tblco-tipo-documento   to pnt-num-documento.
           move aa of dvt-data-fattura to aa of pnt-data-competenza-iva.
           move mm of dvt-data-fattura to mm of pnt-data-competenza-iva.
           write record-pnt invalid continue end-write.

      ***---
       AGGIORNA-CONTATORE-PNT.
           move "CN" to doccn-codice1.
           move G2-codice-login to doccn-ditta.
           move spaces to doccn-esercizio.
           move "00"   to doccn-tipo.
           read doccn invalid continue end-read.
           move contatore-pnt to doccn-contatore.
           accept doccn-data from century-date.
           rewrite record-doccn invalid continue end-rewrite.

           set contab-pnt to true.
           perform AGGIORNA-TRACCIA-CONTATORI.

      ***---
       AGGIORNA-CONTATORE-PROTOCOLLO.
           move "CN"            to doccn-codice1.
           move G2-codice-login to doccn-ditta.
           move dec-esercizio   to doccn-esercizio.
           move spaces to doccn-tipo.
           string "02"               delimited size
                  dvt-num-protocollo delimited size
                  into doccn-tipo
           end-string.
           read doccn invalid continue end-read.
           move contatore-protocollo to doccn-contatore.
           accept doccn-data from century-date.
           rewrite record-doccn invalid continue end-rewrite.

           set contab-protocollo to true.
           perform AGGIORNA-TRACCIA-CONTATORI.

      ***---
       CREA-PAS.
           move 0 to idx.
           initialize record-pas replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move contatore-pat to pas-progressivo.
           perform varying idx from 1 by 1 
                     until idx > dvt-num-rate
              move idx to pas-riga
              initialize resto-record-pas 
                         replacing numeric data by zeroes
                              alphanumeric data by spaces
              move pnr-codice-conto   to pas-codice-conto
              move cli-codice-G2      to pas-codice-cfm
              move dvt-data-fattura   to pas-data-riferimento
              move pnt-documento      to pas-numero-riferimento
              move dvt-scadenza(idx)  to pas-data-scadenza
              move pnr-descrizione1   to pas-descrizione1
              move pnr-descrizione2   to pas-descrizione2
              move dvt-codice-tr(idx) to pas-codice-tr
              move idx                to pas-rata
              if idx = dvt-num-rate
                 move "S"             to pas-saldo-acconto
              else
                 move "A"             to pas-saldo-acconto
              end-if
              if dvt-inizio-conteggio(idx) = 99
                 move "S" to pas-a-vista
              end-if
              if pnr-dare-avere = "D"
                 move pnr-codice-va        to pas-codice-va-dare
                 move dvt-importo(idx)     to pas-importo-dare
                 move dvt-importo(idx)     to pas-importo-dare-va
                 move dvt-data-fattura     to pas-data-registrazione-d
                 move dvt-data-fattura     to pas-data-documento-d
                 move dvt-numero-fattura   to pas-numero-documento-d
                 move contatore-protocollo to pas-numero-protocollo-d
                 move tblco-tipo-documento to pas-num-documento-d
              else
                 move pnr-codice-va        to pas-codice-va-avere
                 move dvt-importo(idx)     to pas-importo-avere
                 move dvt-importo(idx)     to pas-importo-avere-va
                 move dvt-data-fattura     to pas-data-registrazione-a
                 move dvt-data-fattura     to pas-data-documento-a
                 move dvt-numero-fattura   to pas-numero-documento-a
                 move contatore-protocollo to pas-numero-protocollo-a
                 move tblco-tipo-documento to pas-num-documento-a
              end-if
              move cli-codice-ba to pas-codice-ba
              move 0 to pas-situazione
              write record-pas  invalid continue end-write
           end-perform.

      ***---
       CREA-PAR.
           initialize record-par replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move contatore-pat        to par-progressivo.
           move 1                    to par-riga.
           move dvt-data-fattura     to par-data-registrazione.
           move contatore-protocollo to par-numero-protocollo.
           move dvt-data-fattura     to par-data-documento.
           move dvt-numero-fattura   to par-numero-documento.
           move pnr-descrizione1     to par-descrizione1.
           move pnr-descrizione2     to par-descrizione2.
           move pnr-dare-avere       to par-dare-avere.
           move pnr-importo          to par-importo par-importo-va.
           move pnr-codice           to par-codice-pnr.
           move tblco-tipo-documento to par-num-documento.
           write record-par invalid continue end-write.

      ***---
       CREA-PAT.
           initialize record-pat replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move dvt-data-fattura     to pat-data-registrazione.
           move contatore-protocollo to pat-numero-protocollo.
           move contatore-pat        to pat-progressivo.
           move pnr-codice-conto     to pat-codice-conto.
           move dvt-data-fattura     to pat-data-riferimento.
           move pnt-documento        to pat-numero-riferimento.
           if pnr-dare-avere = "D"
              move pnr-importo to pat-importo-dare
           else
              move pnr-importo to pat-importo-avere
           end-if.
           compute pat-importo-saldo = 
                   pat-importo-dare - pat-importo-avere.
           move dvt-codice-ag        to pat-codice-ag.
           move dvt-codice-pa        to pat-codice-pa.
           move pas-riga             to pat-ultima-riga-pas.
           move par-riga             to pat-ultima-riga-par.
           move dvt-data-fattura     to pat-data-documento.
           move dvt-numero-fattura   to pat-numero-documento.
           move pnr-importo          to pat-importo-documento.
           move tblco-tipo-documento to pat-num-documento
           move pnr-dare-avere       to pat-dare-avere.
           write record-pat invalid continue end-write.

      ***---
       AGGIORNA-CONTATORE-PAT.
           move "CN" to doccn-codice1.
           move G2-codice-login to doccn-ditta.
           move spaces to doccn-esercizio.
           move "26"   to doccn-tipo.
           read doccn invalid continue end-read.
           move contatore-pat to doccn-contatore.
           rewrite record-doccn invalid continue end-rewrite.

           set contab-pat to true.
           perform AGGIORNA-TRACCIA-CONTATORI.

      ***---
       READ-CAUSALE-CONTABILE.
           move "DO"                       to tbldo-codice1.
           if LinkNote move G2-cod-nc      to tbldo-codice2
           else        move G2-cod-fatture to tbldo-codice2
           end-if.
           read tbldo  no lock invalid continue end-read.

           move "CO"               to tblco-codice1.
           move tbldo-codice-co    to tblco-codice2.
           read tblco  no lock invalid continue end-read.

      ***---
       READ-DOCCN-PRIMA-NOTA.
           move "CN" to doccn-codice1.
           move G2-codice-login to doccn-ditta.
           move spaces to doccn-esercizio.
           move "00"   to doccn-tipo.
           read doccn  lock
                invalid perform CREA-CONTATORE-PRIMA-NOTA
           end-read.
           if contatore-PNT > doccn-contatore
              move contatore-PNT   to doccn-contatore 
           else
              move doccn-contatore to contatore-PNT
           end-if.

      ***---
       CREA-CONTATORE-PRIMA-NOTA.
           initialize record-doccn replacing numeric data by zeroes
                                        alphanumeric data by spaces.
           move "CN"               to doccn-codice1.
           move G2-codice-login    to doccn-ditta.
           move spaces             to doccn-esercizio.
           move "00"               to doccn-tipo.
           move "Prima Nota"       to doccn-descrizione.
           accept doccn-data  from century-date.
           write record-doccn invalid continue end-write.
           read doccn lock    invalid continue end-read.

           set contab-pnt to true.
           perform AGGIORNA-TRACCIA-CONTATORI.

      ***---
       READ-DOCCN-SCAD.
           move "CN" to doccn-codice1.
           move G2-codice-login to doccn-ditta.
           move spaces to doccn-esercizio.
           move "26"   to doccn-tipo.
           read doccn with lock 
                invalid perform CREA-CONTATORE-SCAD
           end-read.
           if contatore-PAT > doccn-contatore
              move contatore-PAT   to doccn-contatore 
           else
              move doccn-contatore to contatore-PAT
           end-if.

      ***---
       CREA-CONTATORE-SCAD.
           initialize record-doccn replacing numeric data by zeroes
                                        alphanumeric data by spaces.
           move "CN"               to doccn-codice1.
           move G2-codice-login    to doccn-ditta.
           move spaces             to doccn-esercizio.
           move "26"               to doccn-tipo.
           move "Scadenziari"      to doccn-descrizione.
           move dvt-data-fattura   to doccn-data.
           write record-doccn invalid continue end-write.
           read doccn lock    invalid continue end-read.

           set contab-pat to true.
           perform AGGIORNA-TRACCIA-CONTATORI.

      ***---
       READ-DOCCN-PROTOCOLLO.
           move "CN" to doccn-codice1.
           move G2-codice-login to doccn-ditta.
           evaluate aa of dvt-data-fattura(3:2) 
           when 00 move "A" to dec-esercizio
           when 01 move "B" to dec-esercizio
           when 02 move "C" to dec-esercizio
           when 03 move "D" to dec-esercizio
           when 04 move "E" to dec-esercizio
           when 05 move "F" to dec-esercizio
           when 06 move "G" to dec-esercizio
           when 07 move "H" to dec-esercizio
           when 08 move "I" to dec-esercizio
           when 09 move "L" to dec-esercizio
           when 10 move "M" to dec-esercizio
           when 11 move "N" to dec-esercizio
           when 12 move "O" to dec-esercizio
           when 13 move "P" to dec-esercizio
           when 14 move "Q" to dec-esercizio
           when 15 move "R" to dec-esercizio
           when 16 move "S" to dec-esercizio
           when 17 move "T" to dec-esercizio
           when 18 move "U" to dec-esercizio
           when 19 move "V" to dec-esercizio
           when 20 move "W" to dec-esercizio
           when 21 move "X" to dec-esercizio
           when 22 move "Y" to dec-esercizio
           when 23 move "Z" to dec-esercizio
           when 24 move "a" to dec-esercizio
           when 25 move "b" to dec-esercizio
           when 26 move "c" to dec-esercizio
           when 27 move "d" to dec-esercizio
           when 28 move "e" to dec-esercizio
           when 29 move "f" to dec-esercizio
           when 30 move "g" to dec-esercizio
           when 31 move "h" to dec-esercizio
           when 32 move "i" to dec-esercizio
           when 33 move "l" to dec-esercizio
           when 34 move "m" to dec-esercizio
           when 35 move "n" to dec-esercizio
           when 36 move "o" to dec-esercizio
           when 37 move "p" to dec-esercizio
           when 38 move "q" to dec-esercizio
           when 39 move "r" to dec-esercizio
           when 40 move "s" to dec-esercizio
           when 41 move "t" to dec-esercizio
           when 42 move "u" to dec-esercizio
           when 43 move "v" to dec-esercizio
           when 44 move "w" to dec-esercizio
           when 45 move "x" to dec-esercizio
           when 46 move "y" to dec-esercizio
           when 47 move "z" to dec-esercizio
           end-evaluate.
           move dec-esercizio to doccn-esercizio.
           move spaces to doccn-tipo.
           string "02"               delimited size
                  dvt-num-protocollo delimited size
                  into doccn-tipo
           end-string.
           read doccn with lock
                invalid perform CREA-CONTATORE-PROTOCOLLO
           end-read.
           move doccn-contatore to contatore-protocollo.

      ***---
       CREA-CONTATORE-PROTOCOLLO.
           initialize record-doccn replacing numeric data by zeroes
                                        alphanumeric data by spaces.
           move "CN"               to doccn-codice1.
           move G2-codice-login    to doccn-ditta.
           move dec-esercizio      to doccn-esercizio.
           string "02"               delimited size
                  dvt-num-protocollo delimited size
                  into doccn-tipo
           end-string.
           string "Protocollo IVA Clienti " delimited size
                  aa of dvt-data-fattura    delimited size
                  into doccn-descrizione
           end-string.
           move dvt-data-fattura   to doccn-data.
           write record-doccn invalid continue end-write.
           read doccn lock    invalid continue end-read.
           
           set contab-protocollo to true.
           perform AGGIORNA-TRACCIA-CONTATORI.

      ***--
       CLOSE-FILES.
           close pnr pnt pni pas pat par clz maz mas
                 gdvt tblco tbldo doccn G2 CLI contab.

           if path-tmp-iva not = spaces
              close tmp-iva
           end-if.

      ***---
       AGGIORNA-TRACCIA-CONTATORI.
           move DVT-numero-fattura to contab-a-documento.
           if contab-da-documento = 0
              move DVT-numero-fattura to contab-da-documento
           end-if.
           evaluate true
           when contab-protocollo
                if contab-da-protocollo = 0
                   move contatore-protocollo to contab-da-protocollo
                end-if
                move contatore-protocollo to contab-a-protocollo
                move doccn-codice         to contab-k-protocollo
           when contab-pnt
                if contab-da-pnt = 0
                   move contatore-pnt to contab-da-pnt
                end-if
                move contatore-pnt to contab-a-pnt
                move doccn-codice  to contab-k-pnt
           when contab-pat
                if contab-da-pat = 0
                   move contatore-pat to contab-da-pat
                end-if
                move contatore-pat to contab-a-pat
                move doccn-codice  to contab-k-pat
           end-evaluate.
      
      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "movpri.cpy".
