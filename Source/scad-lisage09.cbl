       PROGRAM-ID. scad-lisage09.
       REMARKS.
           Per tutti i listini (agenti e promo) aventi come scadenza
           il 31/12/2008 quest-ultima andrà impostata al 31/12/2009
           sia che riguardi il vecchio o il nuovo periodo.

       SPECIAL-NAMES. decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lisagente.sl".

       DATA DIVISION.
       FILE SECTION.
           copy "lisagente.fd".

       WORKING-STORAGE SECTION.

      * FILE STATUS AND VARIABLES
       77  status-lisagente         pic xx.

      * COSTANTI
       78  titolo  value "Modifica scadenza listini".

       PROCEDURE DIVISION.

      ***---
       MAIN.
           perform OPEN-FILES.
           perform ELABORAZIONE
           perform CLOSE-FILES
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open i-o lisagente.

      ***---
       ELABORAZIONE.
           move low-value to lis-chiave.

           start lisagente key >= lis-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read lisagente next at end exit perform end-read
                    if lis-data-fine-old = 20081231
                       move 20091231 to lis-data-fine-old
                       rewrite lis-rec
                    end-if
                    if lis-data-fine-new = 20081231
                       move 20091231 to lis-data-fine-new
                       rewrite lis-rec
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close lisagente.

      ***---
       EXIT-PGM.
           goback.
