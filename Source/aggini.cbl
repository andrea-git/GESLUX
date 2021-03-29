       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      aggini.
       AUTHOR.                          Andrea.
       REMARKS. 
           - prendere in input un csv con codice/magazzino/imballo/peso/valore
           - aggiornare il valore delle "Giacenze Iniziali" del relativo 
             progressivo on quello riportato nell'ultima colonna del csv
           - devo poter aggiornare anche il padre, quindi inserendo 
             solo il codice nel csv.
  
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
       77  status-progmag   pic xx.
       77  status-lineseq   pic xx.

       77  csv-art          pic x(6).
       77  csv-mag          pic x(3).
       77  csv-imb          pic x(3).
       77  csv-peso         pic x(8).
       77  csv-valore       pic x(11).
       77  csv-segno        pic x.

       77  como-peso        pic 9(8).
       77  como-valore      pic 9(11).

       77  rec              pic 9(5) value 0.
       77  rec-ok           pic 9(5) value 0.
       77  rec-ko           pic 9(5) value 0.

       78  titolo           value "Aggiornamento iniziali valore".

       PROCEDURE DIVISION.

      ***---
       MAIN.
           move "aggini.csv" to wstampa.
           open input lineseq.
           open i-o   progmag.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              add 1 to rec
              unstring line-riga delimited by ";"
                  into csv-art
                       csv-mag
                       csv-imb
                       csv-peso
                       csv-valore
                       csv-segno
              move csv-art  to prg-cod-articolo convert
              move csv-mag  to prg-cod-magazzino
              move csv-imb  to prg-tipo-imballo
              move csv-peso to como-peso convert
              divide como-peso by 1000 giving prg-peso
              read progmag no lock
                   invalid add 1 to rec-ko
               not invalid 
                   add 1 to rec-ok
                   move csv-valore to como-valore convert
                   divide como-valore by 100 giving prg-ini-valore
                   if csv-segno = "-"
                      compute prg-ini-valore = prg-ini-valore * -1
                   end-if
                   rewrite prg-rec
              end-read
           end-perform.
           close lineseq progmag.

           display message "Elaborazione terminata"
                     x"0d0a""Record trattati: " rec
                     x"0d0a""Record aggiornati: " rec-ok
                     x"0d0a""Record errati: " rec-ko
                     title titolo

           goback.
