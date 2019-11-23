       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      del-stato.
       AUTHOR. Andrea
       REMARKS. Elimina gli stati di consegna sgli ordini dall'anno 2008
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".

       WORKING-STORAGE SECTION.

      * Status Files GESLUX
       77  status-tordini    pic X(2).

       78  titolo            value "Cancellazione stato consegna".

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           open i-o tordini.
           move low-value to tor-rec.
           move 2008      to tor-anno.
           start tordini key >= tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    move spaces to tor-esito-consegna
                    rewrite tor-rec
                 end-perform
           end-start.
           close tordini.

           goback.
