       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-eva-cli.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".

       WORKING-STORAGE SECTION.

       78  titolo    value "Check evasioni cliente".

       77  status-tordini   pic xx.

       LINKAGE SECTION.
       copy "link-chk-ord-cli.def".

      ******************************************************************
       PROCEDURE DIVISION using chk-ord-cli-linkage.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0        to chk-ord-status.

      ***---
       OPEN-FILES.
           open input tordini.
      
      ***---
       ELABORAZIONE.
           move chk-cod-cli     to tor-cod-cli.
           move chk-prg-destino to tor-prg-destino.
           move chk-num-ord-cli to tor-num-ord-cli.
           start tordini key >= k-ord-cli
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-cod-cli     not = chk-cod-cli     or
                       tor-prg-destino not = chk-prg-destino or
                       tor-num-ord-cli not = chk-num-ord-cli
                       exit perform
                    end-if
                    if tor-anno   not = chk-ord-anno or
                       tor-numero not = chk-ord-numero
                       move tor-anno   to chk-ord-anno
                       move tor-numero to chk-ord-numero
                       move -1 to chk-ord-status
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close tordini.

      ***---
       EXIT-PGM.
           goback.
