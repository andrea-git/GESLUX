       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-cli-piva.
       AUTHOR.                          Andrea.
       REMARKS. Controllo che un nuovo cliente non abbia una p.iva già presente

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
      * COSTANTI
       78  titolo              value "CONTROLLO CLIENTE PARTITA IVA".

      *FILE-STATUS
       77  status-clienti        pic xx.

       LINKAGE SECTION.                   
       77  lnk-piva              pic x(11).
       77  lnk-cliente           pic z(5).

      ******************************************************************
       PROCEDURE DIVISION USING lnk-piva lnk-cliente.

      ***---
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open input clienti.

      ***---
       ELABORAZIONE.
           move 0 to lnk-cliente.
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key >= cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti next at end exit perform end-read
                    if cli-tipo-F exit perform end-if
                    if cli-piva = lnk-piva
                       move cli-codice to lnk-cliente
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***--
       CLOSE-FILES.
           close  clienti.

      ***---
       EXIT-PGM.
           goback.
