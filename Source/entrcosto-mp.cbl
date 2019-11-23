       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      entrcosto-mp.
       AUTHOR.                          Andrea.
       REMARKS.
           Recupero del costo mp da un txt.
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
           open input lineseq.
           open i-o   progmag.

      ***---
       ELABORAZIONE.
           move low-value to line-riga.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              move line-riga(1:20) to prg-chiave
              read progmag no lock
                   invalid continue
               not invalid
                   move line-riga(30:11)  to costo-medio-x
                   move costo-medio-x     to prg-costo-medio
                   divide prg-costo-medio by 100 giving prg-costo-medio
                   if line-riga(50:1) = "-"
                      compute prg-costo-medio = prg-costo-medio * -1
                   end-if
                   compute prg-ini-valore  =
                           prg-costo-medio * prg-ini-udm
                   rewrite prg-rec
              end-read
           end-perform.

      ***---
       CLOSE-FILES.
           close lineseq progmag.

      ***---
       EXIT-PGM.
           display message "OK".
           goback.
