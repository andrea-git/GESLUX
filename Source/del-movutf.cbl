       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      del-movutf.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "movutf.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "movutf.fd".

       WORKING-STORAGE SECTION.

      * Status Files GESLUX
       77  status-movutf    pic X(2).

       78  titolo            value "Cancellazione UTF Febbraio".

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           open i-o movutf.
           move low-value to mov-rec.
           move 2009      to mov-anno.
           start movutf  key >= mov-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read movutf next at end exit perform end-read
                    if mov-anno not = 2009
                       exit perform
                    end-if
                    if mov-data >= 20090201 and
                       mov-data <= 20090228
                       delete movutf record
                    end-if
                 end-perform
           end-start.
           close movutf.

           goback.
