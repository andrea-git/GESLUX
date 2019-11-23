       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-cf-piva.
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
       78  titolo value "Importazione C.F./P.IVA".

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
       77  r-cli-codfis          pic x(16).
       77  r-cli-piva            pic x(11).
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-piva          pic 9(6)   value 0.
       77  num-rec-cf            pic 9(6)   value 0.
       77  num-rec-both          pic 9(6)   value 0.

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
           move "cf-piva.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   clienti.
           open input lineseq.

      ***---
       ELABORAZIONE.
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into cli-tipo-CF
                            cli-codice
                            r-cli-codfis
                            r-cli-piva
              end-unstring
              read clienti no lock 
                   invalid add 1 to num-rec-ko
               not invalid
                   if r-cli-codfis not = spaces
                      move r-cli-codfis to cli-codfis
                      add 1 to num-rec-cf
                   end-if
                   if r-cli-piva not = spaces
                      move r-cli-piva to cli-piva
                      add 1 to num-rec-piva
                   end-if
                   if r-cli-codfis not = spaces and
                      r-cli-piva   not = spaces
                      subtract 1 from num-rec-cf
                      subtract 1 from num-rec-piva
                      add 1 to num-rec-both
                   end-if
                   rewrite cli-rec invalid continue end-rewrite
              end-read
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""CLIENTI: ", num-rec,
                    x"0d0a""C.F.: ", num-rec-cf,
                    x"0d0a""P.IVA: ", num-rec-piva
                    x"0d0a""ENTRAMBI: ", num-rec-both
                    x"0d0a""ERRATI: ", num-rec-ko
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close clienti lineseq.

      ***---
       EXIT-PGM.
           goback.
