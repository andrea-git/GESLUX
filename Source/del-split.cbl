       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      del-split.
       REMARKS. BATCH cancellazione notturna split per bolla.
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

      * Status Files GESLUX
       77  status-tordini    pic X(2).
       77  status-rordini    pic X(2).

       78  titolo            value "Cancellazione Split".

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           open i-o tordini rordini.
           move 9999 to tor-anno.
           start tordini key >= tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno not = 9999
                       exit perform
                    end-if
                    delete tordini record
                 end-perform
           end-start.
           move 9999 to ror-anno.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno not = 9999
                       exit perform
                    end-if
                    delete rordini record
                 end-perform
           end-start.
           close tordini rordini.

           goback.
