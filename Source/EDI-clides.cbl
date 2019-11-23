       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      edi-clides.
       REMARKS. Aggiorna massivamente dei destini GDO coi dati del cliente
                (ove richiesti), nessuna sovrteascrittura dei dati del destino
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.             
           copy "EDI-clides.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.               
           copy "EDI-clides.fd".

       WORKING-STORAGE SECTION. 

      * Status Files SSI           
       77  status-edi-clides   pic x(2).

       77  s-ecd-cod-dest      pic x(17).

       78  titolo              value "Aggiornamento".

      ******************************************************************
       PROCEDURE DIVISION.
      ***---
       MAIN.
           open i-o edi-clides.
           move low-value to ecd-rec.
           start edi-clides key >= ecd-chiave
           perform until 1 = 2
              read edi-clides next at end exit perform end-read
              if ecd-prg-destino = 0
                 move ecd-cod-dest to s-ecd-cod-dest
              else
                 move s-ecd-cod-dest to ecd-cod-dest
                 rewrite ecd-rec
              end-if           
           end-perform.
           close      edi-clides.

           display message "Elaborazione terminata"
                      title titolo
           goback.
