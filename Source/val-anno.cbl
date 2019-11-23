       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-anno.
       AUTHOR.                          Andrea.
       REMARKS. Valorizza, all'interno delle bozze l'anno bolla/fatt/n.c.
                prelevandoli dalla data del documento
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

       78  titolo    value "Val. anno documenti su bozze".

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
           start btnotacr key >= btno-chiave invalid continue end-start.
           perform until 1 = 2
              read btnotacr next at end exit perform end-read
              if btno-anno-fatt = 0
                 move btno-data-fatt(1:4) to btno-anno-fatt
              end-if
              if btno-anno-bolla = 0
                 move btno-data-bolla(1:4) to btno-anno-bolla
              end-if
              if btno-anno-nc = 0
                 move btno-data-nc(1:4) to btno-anno-nc
              end-if
              if btno-anno-fm = 0
                 move btno-data-fm(1:4) to btno-anno-fm
              end-if
              rewrite btno-rec invalid continue end-rewrite
           end-perform.

      ***---
       CLOSE-FILES.
           close btnotacr.

      ***---
       EXIT-PGM.
           goback.
