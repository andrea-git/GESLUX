       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-flag-web.
       AUTHOR.                          Andrea.
       REMARKS. Valorizza il flag "WEB" come "gruppi"
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

       78  titolo value "Valorizza flag WEB su articoli da CSV".

       77  como-art-codice         pic x(6).

       77  status-articoli         pic x(2).
       77  status-lineseq          pic x(2).
       77  wstampa                 pic x(256).

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           open i-o articoli.
           move low-value to art-rec.
           start articoli key >= art-chiave.
           perform until 1 = 2
              read articoli next at end exit perform end-read
              set art-web-no to true
              rewrite art-rec
           end-perform.
           move "articoli-web.txt" to wstampa.
           open input lineseq.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              move line-riga to como-art-codice
              call "C$JUSTiFY" using como-art-codice, "R"
              inspect como-art-codice replacing leading x"20" by x"30"
              move como-art-codice to art-codice
              read articoli no lock
              set art-web-si to true
              rewrite art-rec
           end-perform.

           display message "Elaborazione terminata"
                     title titolo.

           close lineseq.
           close articoli.
           goback.
