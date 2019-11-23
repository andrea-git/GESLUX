       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ripr-mrordini.
       AUTHOR.                          Andrea.
       REMARKS. Dato un txt in posizione fissa "ripr-mrordini.csv" nella
                cartella Archivi ributta dentro le righe nell'archivio
                effettivo SENZA sovrascrivere
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mrordini.sl". 
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mrordini.fd". 
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.

       78  titolo value "Ripristino righe master".

       77  status-mrordini         pic x(2).
       77  status-lineseq          pic x(2).
       77  wstampa                 pic x(256).

       77  tot-righe               pic 9(5) value 0.
       77  como-peso-utf           pic 9(6).
       77  como-peso-non-utf       pic 9(6).
       77  como-prg-peso           pic 9(6).

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           move "ripr-mrordini.csv" to wstampa.

      ***---
       OPEN-FILES.
           open input lineseq.
           open i-o   mrordini.
      
      ***---
       ELABORAZIONE.
           read lineseq next.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces
                 exit perform
              end-if
              add 1 to tot-righe
              initialize mro-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              unstring line-riga delimited by ";"
                       into mro-anno
                            mro-numero
                            mro-riga
                            mro-cod-articolo
                            mro-qta
                            mro-qta-e
                            mro-qta-b
                            mro-qta-f
                            mro-qta-omaggi
                            mro-prz-unitario
                            mro-imp-consumo
                            mro-imp-cou-cobat
                            mro-add-piombo
                            mro-imponib-merce
                            mro-perce-sconto
                            mro-omaggio     
                            como-peso-utf    
                            como-peso-non-utf
                            mro-num-colli   
                            mro-cod-imballo 
                            mro-des-imballo 
                            mro-qta-imballi 
                            mro-cod-art-cli 
                            mro-cod-iva     
                            mro-prz-commle  
                            mro-prg-cod-articolo 
                            mro-prg-cod-magazzino
                            mro-prg-tipo-imballo 
                            como-prg-peso  
                            mro-bli-codice
                            mro-bli-qta   
                            mro-bli-perce 
                            mro-blister   
                            mro-promo     
                            mro-flag-cancellato
                            mro-prz-promo
                            mro-progr
                            mro-evadi-dal
                            mro-data-creazione
                            mro-ora-creazione
                            mro-utente-creazione      
                            mro-data-ultima-modifica  
                            mro-ora-ultima-modifica   
                            mro-utente-ultima-modifica
                            mro-stato      
                            mro-prz-manuale
                            mro-num-vuoto-1
                            mro-num-vuoto-2
                            mro-num-vuoto-3
                            mro-alfa-vuoto 
              end-unstring
              divide mro-prz-unitario   by 100  giving mro-prz-unitario
              divide mro-imp-consumo    by 100  giving mro-imp-consumo  
              divide mro-imp-cou-cobat  by 100  giving mro-imp-cou-cobat
              divide mro-add-piombo     by 100  giving mro-add-piombo   
              divide mro-imponib-merce  by 100  giving mro-imponib-merce
              divide mro-prz-commle     by 100  giving mro-prz-commle
              divide como-peso-utf      by 1000 giving mro-peso-utf
              divide como-peso-non-utf  by 1000 giving mro-peso-non-utf
              divide como-prg-peso      by 1000 giving mro-prg-peso
              write  mro-rec
                    invalid display message "IMPOSSIBILE " tot-righe
              end-write
           end-perform.

      ***---
       CLOSE-FILES.
           close lineseq mrordini.

      ***---
       EXIT-PGM.
           display message "FINE! Aggiunte righe " tot-righe
           goback.
