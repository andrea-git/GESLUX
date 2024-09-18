       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-contrassegno.
       AUTHOR.                          Andrea.
       REMARKS.
           Valorizza con "N" il valore del contrassegno 
           per le evasioni senza emissione fattura

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
      *    COSTANTI
       78  titolo               value "Val contrassegno".

      *    FILE STATUS
       77  status-tordini       pic xx.

      ******************************************************************
       PROCEDURE DIVISION.
           open i-o tordini.
           move low-value to tor-chiave.
           move 2020      to tor-anno.
           start tordini key >= tor-chiave.
           perform until 1 = 2
              read tordini next at end exit perform end-read
              if tor-causale = "FTND" or "FTTR" or "FTMA"
                 if tor-contrassegno = space
                    move "N" to tor-contrassegno
                    rewrite tor-rec
                 end-if
              end-if
           end-perform.

           close tordini.
           display message "FINE".
           goback.
