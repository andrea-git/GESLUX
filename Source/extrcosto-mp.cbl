       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      entrcosto-mp.
       AUTHOR.                          Andrea.
       REMARKS.
           Esportazione del costo mp da un progamg a txt.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".
           copy "lineseq.sl".

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "progmag.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.

      *    COSTANTI
       78  titolo            value "Estrazione".

      *    FILE STATUS
       77  status-progmag        pic xx.
       77  status-lineseq        pic xx.

       77  wstampa               pic x(256).
                                                  
       77  costo-medio-z         pic z(11).
       77  costo-medio-x         pic x(11).

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
           move "cmp.txt" to wstampa.

      ***---
       OPEN-FILES.    
           open output lineseq.
           open input  progmag.
      
      ***---
       ELABORAZIONE. 
           move low-value to prg-rec.
           start progmag key >= prg-chiave.
           perform until 1 = 2
              read progmag next at end exit perform end-read
              initialize line-riga
              move prg-chiave      to line-riga(1:20)
              if prg-costo-medio < 0
                 move "-" to line-riga(50:1)
              else                             
                 move " " to line-riga(50:1)
              end-if
              multiply prg-costo-medio by 100 giving prg-costo-medio
              move prg-costo-medio to costo-medio-z
              move costo-medio-z   to costo-medio-x
              move costo-medio-x   to line-riga(30:15)
              write line-riga
           end-perform.

      ***---
       CLOSE-FILES.
           close lineseq progmag.

      ***---
       EXIT-PGM.                 
           display message "OK".
           goback.
