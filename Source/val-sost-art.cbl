       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-sost-art.
       REMARKS. 
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "destini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "destini.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-clienti pic X(2).
       77  status-destini pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     clienti
                     destini
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
            "Confermi la valorizzazione dei nuovi flag sui clienti?"
                          type mb-yes-no
                          default mb-no
                          giving scelta
                          icon 2
           if scelta = mb-yes
              perform VALORIZZAZIONE
           end-if.

           goback.


      ***---
       VALORIZZAZIONE.
           move zero   to cont

           open i-o clienti.
           open i-o destini.

           set cli-tipo-C to true
           move low-value to cli-codice

           start clienti key not less cli-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read clienti next
                       at end
                          exit perform
                    end-read
                    if not cli-tipo-C
                       exit perform
                    end-if

                    perform VAL-FLAG

                 end-perform
           end-start.

           close clienti
                 destini.

           move cont   to cont-ed
           display message box "Valorizzati " cont-ed " record.".


      ***---
       VAL-FLAG.
           add 1 to cont.

           set cli-sost-no   to true

           evaluate cli-tipo
           when "1 "
                set cli-tipo-art-GDA     to true
           when "7 "
                set cli-tipo-art-GDS     to true
           when "4 "
                set cli-tipo-art-ESTERO  to true
           when other
                set cli-tipo-art-gruppi  to true
           end-evaluate

           rewrite cli-rec
              invalid
                 continue
           end-rewrite.


           move cli-codice   to des-codice
           move low-value    to des-prog

           start destini key not < des-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read destini next
                       at end
                          exit perform
                    end-read
                    if cli-codice not = des-codice
                       exit perform
                    end-if

                    move cli-tipo-art to des-tipo-art
                    rewrite des-rec
                       invalid
                          continue
                    end-rewrite
                 end-perform
           end-start.

