       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      val-trasp.
       AUTHOR.                          Andrea.
       REMARKS. Valorizza le scorte (trasporto fornitore e cliente) coi
                valori presenti in parametri generali
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tparamge.sl". 
           copy "tscorte.sl". 

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tparamge.fd". 
           copy "tscorte.fd". 

       WORKING-STORAGE SECTION.

       77  status-tparamge  pic xx.
       77  status-tscorte   pic xx.

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open i-o   tscorte.
           open input tparamge.
      
      ***---
       ELABORAZIONE.
           move spaces to tge-codice.
           read tparamge.

           move low-value to sco-rec.
           start tscorte key >= sco-chiave invalid continue end-start.
           perform until 1 = 2
              read tscorte next at end exit perform end-read
              move tge-trasp-c to sco-trasp-c
              move tge-trasp-f to sco-trasp-f
              rewrite sco-rec
           end-perform.

      ***---
       CLOSE-FILES.
           close tscorte tparamge.

      ***---
       EXIT-PGM.
           goback.
