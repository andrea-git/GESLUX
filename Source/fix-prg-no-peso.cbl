       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      fix-prg-no-peso.
       AUTHOR.                          Andrea.
       REMARKS.
           Sistema i progressivi che non hanno peso utf e nemmeno 
           peso non utf, dopo l'errore in insprog

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd".
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
      *    COSTANTI
       78  titolo               value "Correzione pesi progressivi".

      *    FILE STATUS
       77  status-progmag       pic xx.
       77  status-articoli      pic xx.

       77  nr                   pic 9(5) value 0.

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           open i-o progmag.
           open input articoli.

           move low-value to prg-chiave.
           start progmag key >= prg-chiave.
           perform until 1 = 2
              read progmag next at end exit perform end-read
              if prg-peso-utf     = 0 and 
                 prg-peso-non-utf = 0
                 move prg-cod-articolo to art-codice
                 read articoli no lock
                      invalid continue
                  not invalid
                      if art-peso-utf is numeric and 
                         art-peso-utf > 0
                         move prg-peso to prg-peso-utf
                         rewrite prg-rec
                         add 1 to nr
                      end-if
                      if art-peso-non-utf is numeric and 
                         art-peso-non-utf > 0
                         move prg-peso to prg-peso-non-utf
                         rewrite prg-rec
                         add 1 to nr
                      end-if
                         
                 end-read
              end-if
           end-perform.

           close progmag articoli.
           display message "Elaborazione terminata"
                    x"0d0a""PRG SISTEMATI: " nr
                     title titolo
                      icon 2
           goback.


