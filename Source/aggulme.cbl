       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      aggulme.
       AUTHOR.                          Andrea.
       REMARKS. 
           - prendere in input un csv con codice articolo
           - aggiornare PRG_COSTO_ULTIMO e  PRG_COSTO_MEDIO con il prezzo indicato nel file
           - sia per il padre che per i figli
  
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

       77  como-articolo    pic 9(8).     
       77  como-valore      pic 9(11).
       77  como-costo       pic s9(9)v99.

       77  rec              pic 9(5) value 0.
       77  rec-ok           pic 9(5) value 0.
       77  rec-ko           pic 9(5) value 0.
       77  prg-ok           pic 9(5) value 0.

       78  titolo           value "Aggiornamento ultimo e medio valore".

       PROCEDURE DIVISION.

      ***---
       MAIN.
           move "aggulme.csv" to wstampa.
           open input lineseq.
           open i-o   progmag.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              add 1 to rec
              unstring line-riga delimited by ";"
                  into csv-art
                       csv-valore
                       csv-segno
              move csv-art    to como-articolo convert
              move csv-valore to como-valore   convert
              divide como-valore by 100 giving como-costo
              if csv-segno = "-"                              
                 compute como-costo = como-costo * -1
              end-if                                          

              move low-value to prg-chiave
              move como-articolo to prg-cod-articolo
              start progmag key >= prg-chiave
                    invalid add 1 to rec-ko
                not invalid
                    add 1 to rec-ok
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-articolo not = como-articolo
                          exit perform
                       end-if
                       add 1 to prg-ok            
                       move como-costo to prg-costo-medio 
                                          prg-costo-ultimo
                       rewrite prg-rec
                    end-perform
              end-start
           end-perform.
           close lineseq progmag.

           display message "Elaborazione terminata"
                     x"0d0a""Record trattati: " rec
                     x"0d0a""Record aggiornati: " rec-ok
                     x"0d0a""Record errati: " rec-ko
                     x"0d0a""Progressivi aggiornati: " prg-ok
                     title titolo

           goback.
