       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-magstd.
       AUTHOR.                          Andrea.
       REMARKS. Imposta a "LBX" il magazzino standard per tutti gli articoli
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Magazzino standard LBX".

      * FILE STATUS
       77  status-articoli       pic xx.

       77  tot-elab              pic 9(5).
       77  n-elab                pic 9(5).
       77  n-no-elab             pic 9(5).

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

      ***---
       ELABORAZIONE.
           move low-value to art-rec.
           start articoli key >= art-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read articoli next at end exit perform end-read
              if art-mag-std not = "LBX"
                 move "LBX" to art-mag-std
                 rewrite art-rec
                 add 1 to n-elab
              else
                 add 1 to n-no-elab
              end-if
              add 1 to tot-elab
           end-perform.

      ***---
       CLOSE-FILES.
           close articoli. 

      ***---
       EXIT-PGM. 
           display message "ELABORATI: " tot-elab
                    x"0d0a""IMPOSTATI: " n-elab
                    x"0d0a""GIA' LBX: " n-no-elab
                     title titolo
                      icon 2.

           goback.
