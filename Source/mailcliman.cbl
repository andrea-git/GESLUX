       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mailcliman.
       AUTHOR.                          Andrea.
       REMARKS. Imposta l'invio "Manuale" nel recapito del cliente
                che ha la mail valorizzata.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "recapiti.sl". 
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "recapiti.fd". 
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Impostazione stato manuale".

      * FILE STATUS
       77  status-recapiti       pic xx.
       77  status-clienti        pic xx. 

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
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
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [CLIENTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       RECAPITI-ERR SECTION.
           use after error procedure on recapiti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-recapiti
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
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RECAPITI] inesistente"
                        title = titolo
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
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open input clienti.
           open i-o   recapiti.

      ***---
       ELABORAZIONE.
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave.
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F exit perform end-if
              if cli-email not = spaces
                 add 1 to num-rec
                 move cli-codice to rec-codice
                 read recapiti no lock
                      invalid add 1 to num-rec-ko
                  not invalid
                      set rec-invio-manuale to true
                      rewrite rec-rec
                      add 1 to num-rec-ok
                 end-read
              end-if
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""ELABORATI: ", num-rec,
                    x"0d0a""RECAPITI CORRETTI: ", num-rec-ok
                    x"0d0a""RECAPITI NON PRESENTI: ", num-rec-ko
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close clienti recapiti.

      ***---
       EXIT-PGM.
           goback.
