       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-pesi.
       AUTHOR.                          Andrea.
       REMARKS. Impostaa il peso fisso all'articolo ed al progressivo 
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "progmag.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "progmag.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo      value "Correzione peso".
       78  78-peso     value 0,0150.


      * FILE STATUS
       77  status-articoli       pic xx.
       77  status-progmag        pic xx.
       77  status-lineseq        pic xx.
       77  wstampa               pic x(256).

       77  como-codice           pic x(6).

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
           move "pesi.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   articoli progmag. 
           open input lineseq.

      ***---
       ELABORAZIONE.
           move low-value to line-riga.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              move line-riga to como-codice
              call "C$JUSTIFY" using como-codice, "R"
              inspect como-codice replacing leading x"20" by x"30"
              move como-codice to art-codice
              read articoli
              move 78-peso to art-peso-non-utf
              rewrite art-rec
              move low-value  to prg-chiave
              move art-codice to prg-cod-articolo
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-articolo not = art-codice
                          exit perform
                       end-if
                       if prg-peso = 0 
                          move 78-peso to prg-peso-non-utf
                          rewrite prg-rec
                       else
                          delete progmag record
                          move 78-peso to prg-peso prg-peso-non-utf
                          write prg-rec
                       end-if
                    end-perform
              end-start
           end-perform.

      ***---
       CLOSE-FILES.
           close articoli progmag lineseq. 

      ***---
       EXIT-PGM. 
           display message "ELABORAZIONE TERMINATA!"
                     title titolo.
           goback.
