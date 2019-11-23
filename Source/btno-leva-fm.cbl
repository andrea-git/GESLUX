       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      btno-anno-fm.
       AUTHOR.                          Andrea.
       REMARKS. Leva dalle bozze le fatture manuali legate che sono 
                state cancellate.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "btnotacr.sl".
           copy "tordini.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "btnotacr.fd".
           copy "tordini.fd".

       working-storage section.

       78  titolo          value "Cancellazione FM su bozze".

       77  status-btnotacr  pic xx.
       77  status-tordini   pic xx.

       01  controlli       pic xx.
           88 tutto-ok     value "OK".
           88 errori       value "ER".


       PROCEDURE DIVISION.   

      ***---
       MAIN.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.

      ***---
       OPEN-FILES.
           open i-o btnotacr.
           open input tordini.

      ***---
       ELABORAZIONE.
           move low-value to btno-chiave.
           start btnotacr key >= btno-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read btnotacr next at end exit perform end-read
              if btno-num-fm  not = 0
                 move btno-num-fm  to tor-numero
                 move btno-anno-fm to tor-anno
                 read tordini no lock
                      invalid
                      move 0 to btno-anno-fm
                      move 0 to btno-data-fm
                      move 0 to btno-num-fm
                      rewrite btno-rec
                 end-read
              end-if
           end-perform.

      ***---
       CLOSE-FILES.
           close btnotacr tordini.

      ***---
       EXIT-PGM.
           display message "FINE ELABORAZIONE"
                     title titolo.
           goback.
