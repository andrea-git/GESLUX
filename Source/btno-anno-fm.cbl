       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      btno-anno-fm.
       AUTHOR.                          Andrea.
       REMARKS. Valorizza l'anno della FM sulle bozze prendendolo dalla 
                data. Questo valore è iniziato a servire dopo una 
                richiesta del 28/06/2012 e prima non veniva valorizzato.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "btnotacr.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "btnotacr.fd".

       working-storage section.

       78  titolo          value "Valorizzazione anno FM su bozze".

       77  status-btnotacr  pic xx.

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

      ***---
       ELABORAZIONE.
           move low-value to btno-chiave.
           start btnotacr key >= btno-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read btnotacr next at end exit perform end-read
              if btno-num-fm  not = 0 and
                 btno-anno-fm     = 0
                 move btno-data-fm(1:4) to btno-anno-fm
                 rewrite btno-rec
              end-if
           end-perform.

      ***---
       CLOSE-FILES.
           close btnotacr.

      ***---
       EXIT-PGM.
           display message "FINE ELABORAZIONE"
                     title titolo.
           goback.
