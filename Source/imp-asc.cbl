       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-asc.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "assorcli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "assorcli.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Importazione codici articolo cliente".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-assorcli       pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI
       77  r-cod-art-cli         pic x(15).
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0.
       77  num-rec-nn            pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       ASSORCLI SECTION.
           use after error procedure on assorcli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-assorcli
           when "39"
                set errori to true
                display message "File [ASSORCLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ASSORCLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [ASSORCLI] inesistente"
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
           move "codart.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   assorcli.
           open input lineseq.

      ***---
       ELABORAZIONE.
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga r-cod-art-cli
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into asc-cod-gruppo-gdo
                            asc-cod-cliente
                            asc-progressivo-destino
                            asc-cod-articolo
                            r-cod-art-cli
              end-unstring
              if r-cod-art-cli not = spaces
                 read assorcli
                      invalid add 1 to num-rec-ko
                  not invalid 
                      add 1 to num-rec-ok
                      move r-cod-art-cli to asc-cod-articolo-per-cliente
                      rewrite asc-rec invalid continue end-rewrite
                 end-read 
              else
                 add 1 to num-rec-nn
              end-if
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""ASSORCLI: ", num-rec,
                    x"0d0a""IMPORTATI: ", num-rec-ok,
                    x"0d0a""NON TROVATI: ", num-rec-ko
                    x"0d0a""NON IMPORTATI: ", num-rec-nn
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close assorcli lineseq.

      ***---
       EXIT-PGM.
           goback.
