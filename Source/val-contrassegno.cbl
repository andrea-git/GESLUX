       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-contrassegno.
       AUTHOR.                          Andrea.
       REMARKS.
           Valorizza con "N" il valore del contrassegno delle evasioni presenti

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "mtordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "mtordini.fd".

       WORKING-STORAGE SECTION.
      *    COSTANTI
       78  titolo               value "Val contrassegno".

      *    FILE STATUS
       77  status-tordini       pic xx.
       77  status-mtordini      pic xx.

      ******************************************************************
       PROCEDURE DIVISION.
           open i-o tordini mtordini.
           move low-value to tor-chiave.
           move 2020      to tor-anno.
           start tordini key >= tor-chiave.
           perform until 1 = 2
              read tordini next at end exit perform end-read
              if tor-contrassegno = space
                 move "N" to tor-contrassegno
                 rewrite tor-rec
              end-if
           end-perform.
           move low-value to mto-chiave.
           move 2020      to mto-anno.
           start mtordini key >= mto-chiave.
           perform until 1 = 2
              read mtordini next at end exit perform end-read
              if mto-contrassegno = space
                 move "N" to mto-contrassegno
                 rewrite mto-rec
              end-if
           end-perform.

           close tordini mtordini.
           display message "FINE".
           goback.
