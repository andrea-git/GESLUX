       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-flag-spi.
       AUTHOR.                          Andrea.
       REMARKS. Valorizza il flag "SPI" con TRUE su marche 
                CASTROL E BP (codici 6 e 7).
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

       78  titolo value "Valorizza flag SPI su articoli".

       77  status-articoli         pic x(2).

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           open i-o articoli.
           move low-value to art-rec.
           start articoli key >= art-chiave.
           perform until 1 = 2
              read articoli next at end exit perform end-read
              if art-marca-prodotto = 7 or = 8
                 set art-SPI-si to true
                 rewrite art-rec
              end-if
           end-perform.
           display message "Elaborazione terminata"
                     title titolo.

           goback.
