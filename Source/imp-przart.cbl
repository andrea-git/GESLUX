       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-przart.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Importazione prezzi articolo".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-articoli       pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI                          
       77  n-min-vend            pic 9(8).
       77  n-banco               pic 9(8). 
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [ARTICOLI] inesistente"
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
           move "przart.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   articoli.
           open input lineseq.

      ***---
       ELABORAZIONE.
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga n-min-vend n-banco
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into art-codice
                            n-min-vend
                            n-banco
              end-unstring
              if n-banco > 0 and n-min-vend > 0
                 read articoli
                      invalid add 1 to num-rec-ko
                  not invalid 
                      add 1 to num-rec-ok
                      if n-min-vend > 0
                         move n-min-vend to art-prz-min-vend
                         compute art-prz-min-vend = 
                                 art-prz-min-vend / 100
                      end-if
                      if n-banco > 0 
                         move n-banco to art-prezzo-banco
                         compute art-prezzo-banco = 
                                 art-prezzo-banco / 100
                      end-if
                      rewrite art-rec invalid continue end-rewrite
                 end-read 
              end-if
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""ARTICOLI: ", num-rec,
                    x"0d0a""IMPORTATI: ", num-rec-ok,
                    x"0d0a""NON TROVATI: ", num-rec-ko
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close articoli lineseq.

      ***---
       EXIT-PGM.
           goback.
