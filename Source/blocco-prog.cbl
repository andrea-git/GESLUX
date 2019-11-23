       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      blocco-prog.
       AUTHOR.                          Andrea.
       REMARKS. 
           
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Blocco progressivi".

      * FILE STATUS
       77  status-progmag        pic xx.
       77  status-lineseq        pic xx.

       77  como-peso             pic 9(10).

       77  tot-elab              pic 9(5) value 0.
       77  n-elab                pic 9(5) value 0.
       77  n-no                  pic 9(5) value 0.

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
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag
           when "39"
                set errori to true
                display message "File [progmag] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[progmag] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [progmag] inesistente"
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
           open i-o   progmag. 
           move "blocco-prog.csv" to wstampa.
           open input lineseq.

      ***---
       ELABORAZIONE.
           move low-value to line-riga.
           perform until 1 = 2
              read lineseq next at end exit perform end-read 
              unstring line-riga delimited by ";"
                       into prg-cod-articolo
                            prg-cod-magazzino
                            prg-tipo-imballo
                            como-peso
              divide como-peso by 1000 giving prg-peso
              read progmag no lock
                   invalid add 1 to n-no
               not invalid
                   set prg-bloccato to true
                   rewrite prg-rec
                   add 1 to n-elab
              end-read
              add 1 to tot-elab
           end-perform.

      ***---
       CLOSE-FILES.
           close progmag lineseq. 

      ***---
       EXIT-PGM. 
           display message "ELABORATI: " tot-elab
                    x"0d0a""MODIFICATI: " n-elab 
                    x"0d0a""NON TROVATI: " n-no
                     title titolo
                      icon 2.

           goback.
