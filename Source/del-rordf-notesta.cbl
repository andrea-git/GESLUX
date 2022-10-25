       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      del-rordf-notesta.
       AUTHOR.                          Andrea.
       REMARKS. Cancella le righe di ordini f senza testata
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordforn.sl".
           copy "rordforn.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "tordforn.fd".
           copy "rordforn.fd".

       WORKING-STORAGE SECTION.

       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.

       77  nn                    pic 9(10) value 0.
       77  como-data             pic 9(8).
       
      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.          
           open input tordforn.
           open i-o   rordforn.
           accept como-data from century-date.
           move low-value to rof-rec.
           move como-data(1:4) to rof-anno.
           start rordforn key >= rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn next at end exit perform end-read
                    move rof-anno   to tof-anno
                    move rof-numero to tof-numero
                    read tordforn no lock
                         invalid
                         delete rordforn record
                         add 1 to nn
                    end-read
                 end-perform
           end-start.
           close tordforn rordforn.
           display message "Cancellate righe: " nn.
           goback.
