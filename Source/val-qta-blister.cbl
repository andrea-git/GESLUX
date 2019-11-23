       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-qta-blister.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mrordini.sl".
           copy "blister.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "mrordini.fd".
           copy "blister.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-mrordini      pic X(2).
       77  status-blister       pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     mrordini
                     blister
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la valorizzazione della qta "
                          "dei blister sul file mrordini?"
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
           open i-o    mrordini
           open input  blister.


           move low-value to mro-chiave.

           start mrordini key not less mro-chiave
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read mrordini next 
                       at end 
                          exit perform 
                    end-read

                    add 1 to cont
                    if mro-si-blister
                       perform VAL-QTA-BLI
                    end-if

                 end-perform
           end-start.

           close mrordini
                 blister.

           move cont   to cont-ed
           display message box "Esaminati " cont-ed " record.".


       
      ***---
       VAL-QTA-BLI.
      *    è la prima riga di un blister, quindi metto in linea il blister
      *    e azzero il contatore delle righe del blister
           if mro-qta-imballi not = 0
              move 0 to idx
              move mro-bli-codice  to bli-codice

              read blister
                 invalid
                    initialize bli-tab-componenti
              end-read
           end-if
           add 1 to idx

                      
           move bli-el-qta(idx) to mro-bli-qta
      *    per sicurezza controllo di aver valorizzato in modo giusto i 
      *    blister
           if mro-bli-qta = zero
              move 1   to mro-bli-qta
           end-if.

           rewrite MRO-REC
              invalid
                 continue
           end-rewrite.
