       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      prgblocordf.
       AUTHOR.                          Andrea.
       REMARKS. Controlla gli ordini creati e sistema i progressivi bloccati
      ******************************************************************

       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordforn.sl".
           copy "rordforn.sl".
           copy "progmag.sl". 
           copy "tnumordf.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "progmag.fd". 
           copy "tnumordf.fd".

       WORKING-STORAGE SECTION.
       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.
       77  status-progmag        pic xx.
       77  status-tnumordf       pic xx.     

       01  s-prg-chiave.
           05 s-cod-articolo     pic 9(6).
           05 s-cod-magazzino    pic x(3).
           05 s-tipo-imballo     pic x(3).
           05 s-peso             pic 9(5)v9(3).

       LINKAGE SECTION.
           copy "link-batch.def".

       PROCEDURE DIVISION USING batch-linkage.  
       DECLARATIVES.
      ***---
       RORDFORN-ERR SECTION.
           use after error procedure on rordforn.
           continue.
       END DECLARATIVES.

      ***---
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
           open input tnumordf tordforn progmag.
           open i-o   rordforn.

      ***---
       ELABORAZIONE.      
           move high-value to tnf-rec.
           start tnumordf key <= tnf-chiave
                 invalid continue
             not invalid
                 read tnumordf previous
                 move tnf-data(1:4) to tof-anno
                 move tnf-da-num    to tof-numero
                 start tordforn key >= tof-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read tordforn next 
                               at end exit perform 
                          end-read
                          if tof-anno not = tnf-data(1:4) or
                             tof-numero > tnf-a-num
                             exit perform
                          end-if
                          perform LOOP-RIGHE-RORDFORN
                       end-perform
                 end-start
           end-start.

      ***---
       LOOP-RIGHE-RORDFORN.
           move low-value to rof-rec
           move tof-chiave to rof-chiave-testa
           start rordforn key >= rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn next 
                         at end exit perform 
                    end-read
                    if rof-chiave-testa not = tof-chiave
                       exit perform
                    end-if
                    perform until 1 = 2
                       read rordforn lock
                       if status-rordforn = "00"
                          move rof-prg-chiave to prg-chiave
                          read progmag no lock
                               invalid continue
                           not invalid
                               if prg-bloccato or prg-disattivo
                                  perform TROVA-PROGRESSIVO-ATTIVO
                                  move s-prg-chiave to rof-prg-chiave
                                  move rof-tipo-imballo
                                    to rof-imb-ordinato
                                  rewrite rof-rec
                               end-if
                          end-read
                          unlock rordforn all records
                          exit perform
                       end-if
                    end-perform
                 end-perform
           end-start.            

      ***---
       TROVA-PROGRESSIVO-ATTIVO.
           |Me la salvo comunque
           move prg-chiave to s-prg-chiave
           initialize prg-chiave replacing numeric data by zeroes
                                      alphanumeric data by spaces
           move rof-cod-articolo  to prg-cod-articolo
           move rof-cod-magazzino to prg-cod-magazzino
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo  not = rof-cod-articolo or
                       prg-cod-magazzino not = rof-cod-magazzino
                       exit perform
                    end-if
                    if prg-attivo
                       |Al primo progressivo attivo che trovo
                       |memorizzo la chiave
                       move prg-chiave to s-prg-chiave
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close tordforn rordforn progmag tnumordf.

      ***---
       EXIT-PGM.
           goback.                
