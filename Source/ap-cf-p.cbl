       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ap-cf-p.
       AUTHOR.                          Andrea.
       REMARKS.Effettua l'apertura del conto "Bilancio d'apertura" per 
               tutti i clienti/fornitori presenti dal vecchio sistema 
               GEUR verso G2. Riguarda la contabilità e lo scadenziario 
               per le scadenze ancora aperte da quando è iniziata la
               gestione del cliente/fornitore presso Lubex fino ad oggi.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "ap-cf.sl".
           copy "scad.sl".
           copy "PNT.sl".
           copy "PNR.sl".
           copy "PAT.sl".
           copy "PAS.sl".
           copy "PAR.sl".
           copy "DOCCN.sl".
           copy "CLZ.sl".
           copy "MAZ.sl".
           copy "MAS.sl".
           copy "TBLCO.sl".
           copy "CLI.sl".
           copy "FRN.sl".
           copy "FRZ.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "ap-cf.fd".
           copy "scad.fd".
           copy "PNT.fd".
           copy "PNR.fd".
           copy "PAT.fd".
           copy "PAS.fd".
           copy "PAR.fd".
           copy "DOCCN.fd".
           copy "CLZ.fd".
           copy "MAZ.fd".
           copy "MAS.fd".
           copy "TBLCO.fd".
           copy "CLI.fd".
           copy "FRN.fd".
           copy "FRZ.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".

      *    COSTANTI
       78  titolo value"Generazione bilancio d'apertura e scadenziario".

      *    FILE STATUS
       77  status-ap-cf     pic xx.
       77  status-scad      pic xx.
       77  status-PNT       pic xx.
       77  status-PNR       pic xx.
       77  status-PAT       pic xx.
       77  status-PAS       pic xx.
       77  status-PAR       pic xx.
       77  status-DOCCN     pic xx.
       77  status-CLZ       pic xx.
       77  status-MAZ       pic xx.
       77  status-MAS       pic xx.
       77  status-TBLCO     pic xx.
       77  status-CLI       pic xx.
       77  status-FRN       pic xx.
       77  status-FRZ       pic xx.

       77  selclz           pic x(256).
       77  selmaz           pic x(256).
       77  selfrz           pic x(256).

      *    FLAGS
       01  controlli        pic x(2).
           88 tutto-ok      value "OK".
           88 errori        value "ER".
        
       01  filler           pic 9.
         88 PrimaVolta      value 1, false 0.
        
       01  filler           pic 9.
         88 RecLocked       value 1, false 0.
        
       01  filler           pic 9.
         88 RigaChiusura    value 1, false 0.

      *    VARIABILI
       77  contatore-pat             pic 9(18).
       77  contatore-pnt             pic 9(18).
       77  save-tipo                 pic x.
       77  importo-pri               pic 9(12)v99.
       77  tot-saldo-dare            pic s9(12)v99 value 0.
       77  tot-saldo-avere           pic s9(12)v99 value 0.
       77  idx                       pic 9(3).
       77  righe-par                 pic 9(3).
       77  tot-scadenze              pic 9(3).
       77  PrimoSegno                pic x.
       77  SecondoSegno              pic x.

       LINKAGE SECTION.
       copy "link-ap-cf.def".

      ******************************************************************
       PROCEDURE DIVISION USING ap-cf-linkage.

       DECLARATIVES.
      ***---
       AP-CF-ERR SECTION.
           use after error procedure on ap-cf.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ap-cf
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File d'apertura [AP-CF] inesistente"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [AP-CF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[AP-CF] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           when "23"
           when "02" continue
           when other display message "ERROR ", status-ap-cf
           end-evaluate.  

      ***---
       SCAD-ERR SECTION.
           use after error procedure on scad.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-scad
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle scadenze [SCAD] inesistente"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [SCAD] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[SCAD] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           when "23"
           when "02" continue
           when other display message "ERROR ", status-scad
           end-evaluate.

      ***---
       CLI-ERR SECTION.
           use after error procedure on CLI.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-CLI
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File Clienti SSI [CLI] inesistente"
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
                display message box        "Impossibile procedere."
                  x"0d0a""File Fornitori SSI [FRN] inesistente"
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
       TBLCO-ERR SECTION.
           use after error procedure on TBLCO.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tblco
           when "35"
                display message "Impossibile procedere."
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
           end-evaluate.

      ***---  
       PAT-ERR SECTION.
           use after error procedure on PAT.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pat
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Scadenze [PAT] inesistente"
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
           end-evaluate.

      ***---
       PAS-ERR SECTION.
           use after error procedure on PAS.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pas
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Scadenze [PAS] inesistente"
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
           end-evaluate.

      ***---
       PAR-ERR SECTION.
           use after error procedure on PAR.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-par
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Scadenze [PAR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [PAR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PAR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PNT-ERR SECTION.
           use after error procedure on pnt.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pnt
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Prima Nota [PNT] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [PNT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PNT] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PNR-ERR SECTION.
           use after error procedure on pnr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-pnr
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Prima Nota [PNR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [PNR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PNR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       CLZ-ERR SECTION.
           use after error procedure on clz.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clz
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Contatori [CLZ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [CLZ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CLZ] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       MAZ-ERR SECTION.
           use after error procedure on maz.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-maz
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Contatori [MAZ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [MAZ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[MAZ] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       MAS-ERR SECTION.
           use after error procedure on mas.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mas
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Mastri [MAS] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [MAS] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[MAS] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       DOCCN-ERR SECTION.
           use after error procedure on doccn.
           set tutto-ok  to true.
           evaluate status-doccn
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Contatori [DOCCN] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [DOCCN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DOCCN] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
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
           set tutto-ok      to true.
           set PrimaVolta    to true.
           move ap-cf-selclz to selclz.
           move ap-cf-selmaz to selmaz.
           move ap-cf-selfrz to selfrz.

      ***---
       OPEN-FILES.
           perform OPEN-DOCCN-LOCK.
           if tutto-ok
              open i-o   pnr pnt pas pat par clz maz mas frz
              open input tblco cli ap-cf scad frn
           end-if.

      ***---
       OPEN-DOCCN-LOCK.
           open i-o doccn allowing readers.
      
      ***---
       ELABORAZIONE.
           set tutto-ok to true.

           initialize record-tblco.
           move "CO"          to tblco-codice1.
           move ap-cf-causale to tblco-codice2.
           read tblco no lock invalid continue end-read.

           |1. CICLO CLIENTI
           move "C" to save-tipo
           perform BILANCIO-APERTURA.
           |2. CICLO FORNITORI
           set PrimaVolta to true.
           move "F" to save-tipo.
           perform BILANCIO-APERTURA.
           set PrimaVolta to true.
           perform SCADENZIARIO.

      ***---
       BILANCIO-APERTURA.
           move high-value to  pnt-codice.
           start pnt key is <= pnt-codice
                 invalid move 0 to contatore-PNT
             not invalid read pnt previous no lock
                         move pnt-progressivo to contatore-PNT
           end-start.
           move low-value to ap-chiave.
           move save-tipo to ap-tipo-CF.
           start ap-cf key is >= ap-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read ap-cf next no lock at end exit perform end-read
                 if ap-tipo-cf not = save-tipo  exit perform end-if
                 if PrimaVolta
                    set RigaChiusura to false
                    move 0 to pnr-riga tot-saldo-dare tot-saldo-avere
                    perform READ-DOCCN-PRIMA-NOTA
                    set PrimaVolta to false
                    add 1 to contatore-pnt
                 end-if
                 if ap-cliente
                    move ap-codice-clifor to cli-codice-G2
                    read CLI no lock invalid continue end-read
                 else
                    move ap-codice-clifor to frn-codice
                    read FRN no lock invalid continue end-read
                 end-if
                 perform CREA-PNR
              end-perform
              if pnr-riga > 0
                 set RigaChiusura to true
                 perform CREA-PNR
                 |CREO LA TESTATA ED AGGIORNO IL CONTATORE
                 perform CREA-PNT
                 perform AGGIORNA-CONTATORE-PNT
              end-if
           end-if.

      ***---
       CREA-PNR.
           move contatore-pnt          to pnr-progressivo.
           add 1 to pnr-riga.
           |RIGA TOTALE
           initialize resto-record-pnr replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           if not RigaChiusura
              move ap-tipo-CF          to pnr-tipo-cfm
              move ap-codice-clifor    to pnr-codice-cfm
           else
              move tblco-tipo-cfm(1)   to pnr-tipo-cfm
              move tblco-codice-cfm(1) to pnr-codice-cfm
           end-if.

           move ap-cf-anno             to pnr-esercizio.
           move ap-cf-data             to pnr-data-registrazione.
           move ap-cf-causale          to pnr-codice-co.
           move tblco-descrizione1     to pnr-descrizione1.
           move tblco-descrizione2     to pnr-descrizione2.

           if not RigaChiusura
              move ap-segno            to pnr-dare-avere
              move ap-saldo            to pnr-importo pnr-importo-va
              if ap-dare add ap-saldo  to tot-saldo-dare
              else       add ap-saldo  to tot-saldo-avere
              end-if
           else
              if tot-saldo-dare > tot-saldo-avere
                 compute pnr-importo pnr-importo-va =
                         tot-saldo-dare - tot-saldo-avere
                 set  pnr-avere        to true
              else
                 compute pnr-importo pnr-importo-va =
                         tot-saldo-avere - tot-saldo-dare
                 set  pnr-dare         to true
              end-if
           end-if.

           write record-pnr invalid continue end-write.
           perform MOVPRI.

      ***---
       CREA-PNT.
           initialize record-pnt replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move contatore-pnt          to pnt-progressivo.
           move ap-cf-data             to pnt-data-registrazione.
           move ap-cf-data             to pnt-data-documento.
           move ap-cf-anno             to pnt-esercizio.
           move pnr-riga               to pnt-ultima-riga.
           move tblco-tipo-documento   to pnt-tipo-documento.
           move tblco-tipo-documento   to pnt-num-documento.
           move ap-cf-causale          to pnt-codice-co.
           move tblco-descrizione1     to pnt-descrizione1(1).
           move tblco-descrizione2     to pnt-descrizione2(1).
           write record-pnt invalid continue end-write.

      ***---
       READ-DOCCN-PRIMA-NOTA.
           move "CN" to doccn-codice1.
           move ap-cf-codice-login to doccn-ditta.
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
           move ap-cf-codice-login to doccn-ditta.
           move spaces             to doccn-esercizio.
           move "00"               to doccn-tipo.
           move "Prima Nota"       to doccn-descrizione.
           accept doccn-data  from century-date.
           write record-doccn invalid continue end-write.
           read doccn lock    invalid continue end-read.

      ***---
       AGGIORNA-CONTATORE-PNT.
           move "CN" to doccn-codice1.
           move ap-cf-codice-login to doccn-ditta.
           move spaces to doccn-esercizio.
           move "00"   to doccn-tipo.
           read doccn invalid continue end-read.
           move contatore-pnt to doccn-contatore.
           rewrite record-doccn invalid continue end-rewrite.

      ***---
       CLOSE-FILES.
           close doccn pnr pnt pas pat par clz frz
                 maz mas tblco cli ap-cf scad frn.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "movpri.cpy".
           copy "scadenziario.cpy".
