       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-no-estero.
       AUTHOR.                          Andrea.
       REMARKS. 
           Gli articoli presenti NON devono avere il flag ESTERO 
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Articoli no estero".

      * FILE STATUS
       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.

       77  tot-elab              pic 9(5) value 0.
       77  n-elab                pic 9(5) value 0.

       77  como-articolo         pic x(6).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0. 

      ******************************************************************
       LINKAGE SECTION.

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
           open i-o   articoli. 
           move "TAMOIL.csv" to wstampa.
           open input lineseq.

      ***---
       ELABORAZIONE.
           move low-value to line-riga.

           perform until 1 = 2
              read lineseq next at end exit perform end-read 
              move line-riga to como-articolo
              call "C$JUSTIFY" using como-articolo, "R"
              inspect como-articolo replacing leading x"20" by x"30"
              add 1 to tot-elab
              move como-articolo to art-codice
              read articoli no lock
                   invalid continue
               not invalid
                   set art-no-estero to true
                   rewrite art-rec
                   add 1 to n-elab
              end-read
           end-perform.

      ***---
       CLOSE-FILES.
           close articoli lineseq. 

      ***---
       EXIT-PGM. 
           display message "ELABORATI: " tot-elab
                    x"0d0a""MODIFICATI: " n-elab
                     title titolo
                      icon 2.

           goback.
