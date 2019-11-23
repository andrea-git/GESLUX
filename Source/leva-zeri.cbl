       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      leva-zeri.
       AUTHOR.                          Andrea.
       REMARKS. Toglie gli zero davanti al numero bolla
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "btnotacr.sl". 

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "btnotacr.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".

       77  status-btnotacr  pic xx.

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open i-o btnotacr.

      ***---
       ELABORAZIONE.
           move low-value to btno-rec.
           start btnotacr key >= btno-chiave 
                 invalid continue
           end-start.
           perform until 1 = 2
              read btnotacr next at end exit perform end-read
              inspect btno-num-bolla replacing leading x"30" by x"20"
              call "C$JUSTIFY" using btno-num-bolla, "L"
              rewrite btno-rec invalid continue end-rewrite
           end-perform.

      ***---
       CLOSE-FILES.
           close btnotacr.

      ***---
       EXIT-PGM.
           goback.
