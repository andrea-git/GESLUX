       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      set-emto-contrass.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "edi-mtordini.sl".

      *********************************************************g********
       DATA DIVISION.
       FILE SECTION.              
           copy "edi-mtordini.fd".

       WORKING-STORAGE SECTION.             
      * FILE STATUS
       77  status-edi-mtordini  pic xx.     

       LINKAGE SECTION.                     
      ******************************************************************
       PROCEDURE DIVISION.
           open i-o edi-mtordini.
           move low-value to emto-rec.
           start edi-mtordini key >= emto-chiave.
           perform until 1 = 2
              read edi-mtordini next at end exit perform end-read
              if emto-contrassegno = space
                 move "N" to emto-contrassegno
                 rewrite emto-rec
              end-if
           end-perform.
           close    edi-mtordini.
           goback.
