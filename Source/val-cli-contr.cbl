       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-cli-contr.
       AUTHOR.                          Andrea.
       REMARKS.
           Valorizza con "N" il valore del contrassegno nei clienti

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
      *    COSTANTI
       78  titolo               value "Val contrassegno".

      *    FILE STATUS
       77  status-clienti       pic xx.

      ******************************************************************
       PROCEDURE DIVISION.
           open i-o clienti.
           move low-value to cli-chiave.
           start clienti key >= cli-chiave.
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F exit perform end-if
              if cli-contrassegno = space
                 move "N" to cli-contrassegno
                 rewrite cli-rec
              end-if
           end-perform.

           close clienti.
           display message "FINE".
           goback.
