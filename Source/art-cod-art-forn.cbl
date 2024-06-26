       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-cod-art-forn.
       AUTHOR.                          Andrea.
       REMARKS. 
           - trova l�articolo con il cod_art_forn = alla colonna A
           - sostituisce il cod_art_forn = colonna B
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
       78  titolo value "Cambio cod.art.forn. su articoli".

      * FILE STATUS
       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.

       77  tot-elab              pic 9(5) value 0.
       77  n-elab                pic 9(5) value 0.

       77  nuovo-cod             pic x(15).
       77  vecchio-cod           pic x(15).

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
           move "chevron.csv" to wstampa.
           open input lineseq.

      ***---
       ELABORAZIONE.
           move low-value to line-riga.
           perform until 1 = 2
              read lineseq next at end exit perform end-read 
              unstring line-riga delimited by ";"
                       into vecchio-cod nuovo-cod 
              if nuovo-cod not = spaces
                 add 1 to tot-elab
                 move low-value to art-rec
                 start articoli key >= art-chiave
                       invalid continue
                 end-start
                 perform until 1 = 2
                    read articoli next at end exit perform end-read
                    if art-cod-art-frn = vecchio-cod
                       move nuovo-cod to art-cod-art-frn
                       rewrite art-rec
                    add 1 to n-elab
                    end-if
                 end-perform
              end-if
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
