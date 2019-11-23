       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-ord-cli.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                        
           copy "mtordini.sl".
           copy "EDI-mtordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".
           copy "EDI-mtordini.fd".

       WORKING-STORAGE SECTION.

       78  titolo    value "Check ordini cliente".

       77  status-mtordini     pic xx.
       77  status-edi-mtordini pic xx.

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
           open input mtordini.
           open input EDI-mtordini.
      
      ***---
       ELABORAZIONE.
      *     move chk-cod-cli     to mto-cod-cli.
      *     move chk-prg-destino to mto-prg-destino. 
           move chk-ord-anno    to mto-anno
           move chk-num-ord-cli to mto-num-ord-cli.
           start mtordini key >= mto-k-ord-cli
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    if chk-ord-anno   not = mto-anno or
                       chk-num-ord-cli not = mto-num-ord-cli
                       exit perform
                    end-if

                    if chk-cod-cli = mto-cod-cli and
                       chk-prg-destino = mto-prg-destino
                       if mto-anno   not = chk-ord-anno or
                          mto-numero not = chk-ord-numero
                          move mto-anno   to chk-ord-anno
                          move mto-numero to chk-ord-numero
                          move -1 to chk-ord-status
                          exit perform
                       end-if
                    end-if

                 end-perform
           end-start.

           if chk-ord-anno-EDI    not = 0  and
              chk-num-ord-cli-EDI not = spaces
              move chk-ord-anno-EDI    to emto-anno
              move chk-num-ord-cli-EDI to emto-num-ord-cli
              start EDI-mtordini key >= emto-k-ord-cli
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read EDI-mtordini next 
                            at end exit perform 
                       end-read
                       if chk-ord-anno-EDI    not = emto-anno or
                          chk-num-ord-cli-EDI not = emto-num-ord-cli
                          exit perform
                       end-if

                       if chk-cod-cli-EDI     = emto-cod-cli and
                          chk-prg-destino-EDI = emto-prg-destino
                          if emto-anno   not = chk-ord-anno-EDI or
                             emto-numero not = chk-ord-numero-EDI
                             move emto-anno   to chk-ord-anno-EDI
                             move emto-numero to chk-ord-numero-EDI
                             move -1 to chk-ord-status-EDI
                             exit perform
                          end-if
                       end-if
                    end-perform
              end-start
           end-if.

      ***---
       CLOSE-FILES.
           close mtordini EDI-mtordini.

      ***---
       EXIT-PGM.
           goback.
