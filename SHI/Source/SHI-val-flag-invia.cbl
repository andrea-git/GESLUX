       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      SHI-val-flag-invia.
       REMARKS. I vuoti del file tordini della versione 2.6 erano sporchi
                programma per ripulire tali vuoti
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
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
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tordini    pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tordini
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
               "Confermi l'inizializzazione dei vuoti del file tordini?"
                          type mb-yes-no
                          default mb-no
                          giving scelta
                          icon 2
           if scelta = mb-yes
              perform CONVERSIONE
           end-if.

           goback.


      ***---
       CONVERSIONE.
           move zero   to cont

           open i-o tordini


           move low-value to tor-chiave.

           start tordini key not less tor-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tordini next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tordini

           move cont   to cont-ed
           display message box "Puliti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.

           move zero   to tor-num-vuoto-3

           set tor-da-inviare-no   to true.


           rewrite tor-rec.                      


