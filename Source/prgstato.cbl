       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      prgstato.
       AUTHOR.                          Andrea.
       REMARKS. 
           Imposta a "Attivo" i progressivi senza stato
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd".

       WORKING-STORAGE SECTION.
       77  status-progmag pic xx.

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.

      ***---
       OPEN-FILES.
           open i-o   progmag.

      ***---
       ELABORAZIONE.
           move low-value to prg-chiave.
           start progmag key >= prg-chiave.
           perform until 1 = 2
              read progmag next at end exit perform end-read
              if prg-stato = space
                 move "A" to prg-stato
                 rewrite prg-rec
              end-if
           end-perform.

      ***---
       CLOSE-FILES.
           close progmag.

      ***---
       EXIT-PGM.
           goback.
