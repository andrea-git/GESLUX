       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      attiva-cli.
       AUTHOR.                          Andrea.
       REMARKS. Attiva la gestione fido su tutti i clienti ed imposta 
                come giorni di  dilazione quelli presenti sulla PRIMA 
                scadenza del codice pagamento relativo
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "tcodpag.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "tcodpag.fd".

       working-storage section.

       78  titolo          value "Attivazione fido clienti".

       77  status-clienti  pic xx.
       77  status-tcodpag  pic xx.

       01  controlli       pic xx.
           88 tutto-ok     value "OK".
           88 errori       value "ER".


       PROCEDURE DIVISION.
       DECLARATIVES.
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [CLIENTI] inesistente"
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
                set errori    to true
           end-evaluate. 

      ***---
       TCODPAG-ERR SECTION.
           use after error procedure on tcodpag.
           set tutto-ok  to true.
           evaluate status-tcodpag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [TCODPAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TCODPAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TCODPAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set errori    to true
           end-evaluate. 

       END DECLARATIVES.     

      ***---
       MAIN.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open input tcodpag.
           open i-o   clienti.

      ***---
       ELABORAZIONE.
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F exit perform end-if
              set cli-gestione-fido-si to true
              move "PA"    to tblpa-codice1
              move cli-pag to tblpa-codice2
              read tcodpag no lock
                   invalid continue
               not invalid
                   if tblpa-tipo-scadenze-88-i(1)
                      if tblpa-scadenza(1) not = 0
                         move tblpa-scadenza(1) to cli-gg-dilazione
                      end-if
                   end-if
              end-read
              rewrite cli-rec
           end-perform.

      ***---
       CLOSE-FILES.
           close clienti tcodpag.

      ***---
       EXIT-PGM.
           display message "FINE ELABORAZIONE"
                     title titolo.
           goback.
