       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      del-master-0.
       REMARKS. BATCH cancellazione master (righe e testata) a 0
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".
           copy "mrordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".
           copy "mrordini.fd".

       WORKING-STORAGE SECTION.       
       77  num-r              pic 999 value 0.
       77  num-t              pic 999 value 0.

      * Status Files GESLUX
       77  status-mtordini    pic X(2).
       77  status-mrordini    pic X(2).

       78  titolo            value "Cancellazione master 0".

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           open i-o mtordini mrordini.
           move low-value to mto-chiave.
           start mtordini key >= mto-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    if mto-numero = 0
                       delete mtordini record
                       add 1 to num-t
                    else
                       add  1 to mto-anno
                       move 0 to mto-numero
                       start mtordini key >= mto-chiave
                             invalid exit perform
                       end-start
                    end-if           
                 end-perform
           end-start.                  
           move low-value to mto-chiave.
           move 2017 to mto-anno.
           start mtordini key >= mto-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    move low-value to mro-chiave
                    move mto-chiave to mro-chiave-testa
                    start mrordini key >= mro-chiave
                          invalid
                          delete mtordini record
                          add 1 to num-t
                      not invalid
                          read mrordini next
                          if mro-chiave-testa not = mto-chiave
                             delete mtordini record
                             add 1 to num-t
                          end-if
                    end-start        
                 end-perform
           end-start.

           move low-value to mro-chiave.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-numero = 0
                       delete mrordini record
                       add 1 to num-r
                    else
                       add  1 to mro-anno
                       move 0 to mro-numero
                       start mrordini key >= mro-chiave
                             invalid exit perform
                       end-start
                    end-if           
                 end-perform
           end-start.
           close mtordini mrordini.
           display message "ELABORAZIONE TERMINATA"    
                    x"0d0a""TESTATE CANCELLATE: " num-t
                    x"0d0a""RIGHE CANCELLATE: " num-r
                    title titolo
                     icon 2.

           goback.
