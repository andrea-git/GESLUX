       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lst-azzera-prog.
       REMARKS. BATCH azzeramento progressivo di magazzino quando 
           erronemante diverso dall'articolo
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "listini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "listini.fd".

       WORKING-STORAGE SECTION.

      * Status Files GESLUX
       77  status-listini   pic X(2).

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           open i-o listini.

           move low-value to lst-rec.
           start listini key >= lst-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read listini next at end exit perform end-read
                    if lst-articolo not = lst-prg-cod-articolo and
                       lst-prg-cod-articolo not = 0
                       initialize lst-prg-chiave
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       rewrite lst-rec
                    end-if
                 end-perform
           end-start.
           
           close listini.

           goback.
