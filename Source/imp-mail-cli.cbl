       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-mail-cli.
       AUTHOR.                          Andrea.
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
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Importazione indirizzi mail clienti".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-clienti        pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI
       77  r-codice              pic x(5).
       77  r-mail                pic x(100).
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

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "39"
                set errori to true
                display message "File [LINESEQ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [LINESEQ] inesistente"
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
           move "mail-cli.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   clienti.
           open input lineseq.

      ***---
       ELABORAZIONE.
           |PREVIO AZZERAMENTO DI TUTTE LE MAIL PRESENTI
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave.
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F exit perform end-if
              move spaces to cli-email
              rewrite cli-rec
           end-perform.

           initialize line-riga.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into r-codice
                            r-mail
              end-unstring
              move r-codice to cli-codice convert
              set cli-tipo-C to true
              read clienti no lock 
                   invalid add 1 to num-rec-ko
               not invalid
                   move r-mail to cli-email
                   rewrite cli-rec
                   add 1 to num-rec-ok
              end-read
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""ELABORATI: ", num-rec,
                    x"0d0a""CORRETTI: ", num-rec-ok
                    x"0d0a""ERRATI: ", num-rec-ko
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close clienti lineseq.

      ***---
       EXIT-PGM.
           goback.
