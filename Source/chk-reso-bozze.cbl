       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      chk-reso-bozze.
       AUTHOR.                          Andrea.
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

       78  titolo    value "Check bozze reso".

       77  status-btnotacr   pic xx.

       LINKAGE SECTION.
       copy "link-chk-reso-bozze.def".

      ******************************************************************
       PROCEDURE DIVISION using chk-bz-cli-linkage.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0        to chk-bz-status.

      ***---
       OPEN-FILES.
           open input btnotacr.
      
      ***---
       ELABORAZIONE.
           move chk-bz-anno     to btno-anno.
           move chk-bz-num-reso to btno-num-reso.
           start btnotacr key >= k-reso
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read btnotacr next at end exit perform end-read
                    if btno-anno     not = chk-bz-anno or
                       btno-num-reso not = chk-bz-num-reso
                       exit perform
                    end-if
                    if btno-anno   not = chk-bz-anno or
                       btno-numero not = chk-bz-numero
                       if btno-data(5:2) = chk-bz-mese
                          move btno-chiave to chk-bz-chiave
                          move -1 to chk-bz-status
                          exit perform
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close btnotacr.

      ***---
       EXIT-PGM.
           goback.
