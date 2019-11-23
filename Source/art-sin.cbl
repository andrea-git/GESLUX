       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-sin.
       AUTHOR.                          Andrea.
       REMARKS.
           Gli articoli con queste marche: 54,51,71,58,13,61,59,77,8
           devono avere il mag std "SIN"
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd". 

       WORKING-STORAGE SECTION.
       77  status-articoli       pic xx.
       77  art                   pic 9(5) value 0.

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
           open i-o articoli.
           move low-value to art-chiave.
           start articoli key >= art-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read articoli next at end exit perform end-read
                    evaluate art-marca-prodotto
                    when 54
                    when 51
                    when 71
                    when 58
                    when 13
                    when 61
                    when 59
                    when 77
                    when  8
                         move "SIN" to art-mag-std
                         rewrite art-rec
                         add 1 to art
                    end-evaluate
                 end-perform
           end-start.
           close articoli.
           display message "Modificati " art " articoli".
           goback.
                         
