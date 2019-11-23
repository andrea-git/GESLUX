       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-note-list.
       REMARKS. 
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tlistini.sl".
           copy "nlistini.sl".
           copy "nforn-dest.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tlistini.fd".
           copy "nlistini.fd".
           copy "nforn-dest.fd".


       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tlistini   pic X(2).
       77  status-nlistini   pic X(2).
       77  status-nforn-dest pic X(2).



       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                       tlistini
                       nlistini
                       nforn-dest
                       .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la valorizzazione delle note"
                          x"0D0A"
                          "delle offerte fornitore?"
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

           open input tlistini.
           open input nforn-dest.
           open i-o   nlistini.
                       
           move low-value to tlis-chiave.

           start tlistini key not less tlis-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tlistini next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tlistini 
                 nforn-dest
                 nlistini.

           move cont   to cont-ed
           display message box "Trattati " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           move tlis-fornitore  to nfod-codice
           move tlis-destino    to nfod-dest
           move low-value       to nfod-prog

           start nforn-dest key not < nfod-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read nforn-dest next
                       at end
                          exit perform
                    end-read
                    if tlis-fornitore not = nfod-codice or
                       tlis-destino   not = nfod-dest
                       exit perform
                    end-if

                    move tlis-codice        to nlis-tlis-codice
                    move nfod-prog          to nlis-num-nota
                    move nfod-nota          to nlis-nota
                    move tlis-dati-comuni   to nlis-dati-comuni


                    write nlis-rec
                       invalid 
                          rewrite nlis-rec
                    end-write
                 end-perform
           end-start.

