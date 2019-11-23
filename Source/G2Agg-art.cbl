       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      g2agg-art.
       REMARKS. Aggiorna massivamente gli articoli di GESLUX in G2
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.     
      *    Files SSI
           copy "articoli.sl".
           copy "art.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.     
      *    Files SSI     
           copy "articoli.fd".
           copy "art.fd".

       WORKING-STORAGE SECTION.
           COPY "link-G2agg.def".

      * Status Files SSI
       77  status-articoli   pic x(2).
       77  status-art        pic x(2).

       77 n pic 9(10) value 0.

       78  titolo            value "Aggiornamento articoli G2".

      ******************************************************************
       PROCEDURE DIVISION.
           open input articoli art.
           move low-value to art-rec.
           start articoli key >= art-chiave
           perform until 1 = 2
              read articoli next at end exit perform end-read
              add 1 to n
              initialize G2Agg-linkage
              set G2Agg-art   to true
              move art-codice to G2Agg-articolo
              read art no lock
                   invalid set G2Agg-insert to true
               not invalid set G2Agg-update to true
              end-read
              call   "G2Agg" using G2Agg-linkage
              cancel "G2Agg"   
           end-perform.                                     
           close articoli art.
           display message "Elaborazione terminata"
                    x"0d0a"n " articoli"
                      title titolo
           goback.
