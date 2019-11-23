       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-tordini.
       REMARKS. Controlla che non ci siano teste senza righe (e le cancella) 
              e controlla che non ci siano righe senza testa per l'anno 2010
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "rordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".

       WORKING-STORAGE SECTION.
           copy "acugui.def".

       77  status-tordini       pic X(2).
       77  status-rordini       pic X(2).

       77  cont                 pic 9(6) value 0.
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.
      ***---
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open i-o   tordini.
           open input rordini.

      ***---
       ELABORAZIONE.
           move low-value to tor-chiave.
           move 2010 to tor-anno.
           start tordini key >= tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    move tor-anno   to ror-anno
                    move tor-numero to ror-num-ordine
                    move low-value  to ror-num-riga
                    start rordini key >= ror-chiave
                          invalid
                          delete tordini record
                          add 1 to cont
      *****                    display message "TESTA " tor-chiave
      *****                             x"0d0a""SENZA RIGHE!!!"
      *****                               icon 2
                      not invalid
                          read rordini next
                          if ror-anno       not = tor-anno or
                             ror-num-ordine not = tor-numero
                             delete tordini record
                             add 1 to cont
      *****                       display message "TESTA " tor-chiave
      *****                                x"0d0a""SENZA RIGHE!!!"
      *****                                  icon 2
                          end-if
                    end-start
                 end-perform
           end-start.
           
           display message "FINE " cont.

           display message "Controllare anche le righe?"
                     type mb-yes-no
                   giving scelta
           
           if scelta = mb-yes
              move 0 to cont
              move low-value to ror-chiave
              move 2010 to ror-anno
              start rordini key >= ror-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rordini next at end exit perform end-read
                       move ror-anno       to tor-anno
                       move ror-num-ordine to tor-numero
                       start tordini key >= tor-chiave
                             invalid
                             add 1 to cont
                             display message "RIGA " ror-chiave
                                      x"0d0a""SENZA TESTA!!!"
                                        icon 2
                         not invalid
                             read tordini next
                             if tor-anno   not = ror-anno or
                                tor-numero not = ror-num-ordine
                                add 1 to cont
                                display message "RIGA " ror-chiave
                                         x"0d0a""SENZA TESTA!!!"
                                           icon 2
                             end-if
                       end-start
                    end-perform
              end-start
              display message "FINE " cont
           end-if.

      ***---
       CLOSE-FILES.
           close tordini rordini.

      ***---
       EXIT-PGM.
           goback.
