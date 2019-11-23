       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      cor-dt-via.
      * REMARKS. conversione dalla versione 2.3 verso la 2.4 per 
      *          aggiungere il logo brand
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmovmag.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tmovmag       pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.
       77  como-data            pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tmovmag
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la correzione della data Via"
                          x"0D0A"
                          "sul file TMOVMAG?"
                          type mb-yes-no
                          default mb-no
                          giving scelta
                          icon 2
           if scelta = mb-yes
              perform CORREZIONE
           end-if.

           goback.


      ***---
       CORREZIONE.
           move zero   to cont

           open i-o tmovmag.


           move 2009      to tmo-anno
           move low-value to tmo-numero


           start tmovmag key not less tmo-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tmovmag next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tmovmag.

           move cont   to cont-ed
           display message box "Trattati " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont
           if tmo-dt-via not = zero
              if tmo-dt-via(5:4) = 2009
                 move tmo-dt-via(1:2) to como-data(7:2)
                 move tmo-dt-via(3:2) to como-data(5:2)
                 move tmo-dt-via(5:4) to como-data(1:4)
                 move como-data       to tmo-dt-via
                 rewrite tmo-rec
                    invalid
                       continue
                 end-rewrite
              end-if
           end-if.

