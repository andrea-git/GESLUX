       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      find-progr.
       REMARKS.
           Trova il primo progressivo libero per le righe nuove
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mrordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mrordini.fd".

       WORKING-STORAGE SECTION.

      * Status Files GESLUX
       77  status-mrordini   pic X(2).

       LINKAGE SECTION.
       copy "link-find-progr.def".

      ******************************************************************
       PROCEDURE DIVISION USING fp-linkage.

       MAIN-PRG.
           open input mrordini.
           move low-value to mro-rec.
           move fp-chiave to mro-chiave-testa.
           move 0           to fp-riga fp-progr fp-tot-righe
           start mrordini key > mro-chiave
                 invalid 
                 move 0 to mro-progr mro-riga
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = fp-chiave
                       exit perform
                    end-if
                    if mro-riga > fp-riga
                       move mro-riga to fp-riga
                    end-if                           
                    if mro-progr > fp-progr
                       move mro-progr to fp-progr
                    end-if
                    add 1 to fp-tot-righe
                 end-perform
      *****           read mrordini previous
           end-start.
           add 1 to fp-progr.
           add 1 to fp-riga.
      *****     add 1 to mro-progr giving fp-progr.
      *****     add 1 to mro-riga  giving fp-riga.
           close mrordini.

           goback.
