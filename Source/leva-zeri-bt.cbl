       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      leva-zeri-bt.
       AUTHOR.                          Andrea.
       REMARKS. Toglie gli zero nei campi anno nc e fm
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
       77  n                pic 9(5) value 0.

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
              if ( btno-anno-nc not > 0 and btno-anno-nc not = 0 ) or
                 ( btno-anno-fm not > 0 and btno-anno-fm not = 0 )
                 add 1 to n
                 if btno-anno-nc not > 0
                    move 0 to btno-anno-nc
                 end-if
                 if btno-anno-fm not > 0  
                    move 0 to btno-anno-fm
                 end-if
                 rewrite btno-rec invalid continue end-rewrite
              end-if       
           end-perform.

      ***---
       CLOSE-FILES.
           close btnotacr.

      ***---
       EXIT-PGM.
           display message "ELABORAZIONE TERMINATA"
                    x"0d0a""Corretti: " n " record.".
           goback.
