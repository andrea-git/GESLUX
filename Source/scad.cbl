       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      scad.
       AUTHOR.                          Andrea.
       REMARKS.Effettua la registrazione dei soli movimenti di scadenziario.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "scad.sl".
           copy "PAT.sl".
           copy "PAS.sl".
           copy "PAR.sl".
           copy "DOCCN.sl".
           copy "CLI.sl".
           copy "FRN.sl".
           copy "G2.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "scad.fd".
           copy "PAT.fd".
           copy "PAS.fd".
           copy "PAR.fd".
           copy "DOCCN.fd".
           copy "CLI.fd".
           copy "FRN.fd".
           copy "G2.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".

      *    COSTANTI
       78  titolo value"Generazione bilancio d'apertura e scadenziario".

      *    FILE STATUS 
       77  status-scad      pic xx.
       77  status-PAT       pic xx.
       77  status-PAS       pic xx.
       77  status-PAR       pic xx.
       77  status-DOCCN     pic xx.
       77  status-CLI       pic xx.
       77  status-FRN       pic xx.
       77  status-G2        pic xx.

       77  ap-cf-codice-login pic x(8).

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
       77  save-tipo                 pic x.
       77  importo-pri               pic 9(12)v99.
       77  tot-saldo-dare            pic s9(12)v99 value 0.
       77  tot-saldo-avere           pic s9(12)v99 value 0.
       77  idx                       pic 9(3).
       77  righe-par                 pic 9(3).
       77  tot-scadenze              pic 9(3).
       77  PrimoSegno                pic x.
       77  SecondoSegno              pic x.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
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

      ***---
       OPEN-FILES.
           perform OPEN-DOCCN-LOCK.
           if tutto-ok
              open i-o   pas pat par
              open input scad frn cli G2
           end-if.

      ***---
       OPEN-DOCCN-LOCK.
           open i-o doccn allowing readers.
      
      ***---
       ELABORAZIONE.
           move spaces to G2-chiave.
           read G2 no lock invalid continue end-read.
           move G2-codice-login to ap-cf-codice-login.
           set tutto-ok to true.
           set PrimaVolta to true.
           perform SCADENZIARIO.
           if tutto-ok
              display message "Elaborazione terminata!"
                        title titolo
                         icon 2
           else
              display message "Elaborazione non effettuata!"
                        title titolo
                         icon 2
           end-if.

      ***---
       CLOSE-FILES.
           close pas pat par scad doccn cli frn G2.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "scadenziario.cpy".
