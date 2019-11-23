       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      valpfa.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rlistini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "rlistini.fd".
           

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-rlistini      pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     rlistini
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la valorizzazione del flag PFA "
                          "sulle righe dei listini?"
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

           open i-o rlistini.

           move low-value to rlis-chiave.

           start rlistini key not less rlis-chiave
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read rlistini next 
                       at end 
                          exit perform 
                    end-read

                    set rlis-PFA-si   to true
                    rewrite rlis-rec
                       invalid
                          continue
                    end-rewrite
                    add 1 to cont
                 end-perform
           end-start.

           close rlistini.

           move cont   to cont-ed
           display message box "Valorizzati " cont-ed " record.".



