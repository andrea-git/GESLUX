       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzstatprog.
       AUTHOR.                          Andrea.
       REMARKS.  Azzero i dati prog del mese successivo al consolidamento.
                 Utilizzato solamente per le prove test delta!!!
      ******************************************************************

       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "statsett.sl".
           COPY "tparamge.sl".
                                
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "statsett.fd".
           COPY "tparamge.fd".

       WORKING-STORAGE SECTION.

      * FILE STATUS AND VARIABLES
       77  status-statsett            pic xx.
       77  status-tparamge            pic xx.

       77  mese                       pic 99.

      ******************************************************************
       PROCEDURE DIVISION.

      ***--- 
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open input tparamge.
           open i-o   statsett.

      ***---
       ELABORAZIONE.
           move low-value to sts-chiave.
           move spaces    to tge-chiave.
           read tparamge  no lock invalid continue end-read.
           move tge-data-consolid-progmag(5:2) to mese.
           if mese = 12 move 1 to mese end-if.
           add 1 to mese.
           move mese to sts-mese.
           start statsett key >= k-ord
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read statsett next at end exit perform end-read
                    if sts-mese not = mese    exit perform end-if
                    move 0 to sts-kg-prog
                    move 0 to sts-qta-prog
                    move 0 to sts-fat-prog
                    move 0 to sts-csm-prog
                    move 0 to sts-adeguam-prog
                    move 0 to sts-adeguam-corr
                    rewrite sts-rec invalid continue end-rewrite
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close statsett tparamge.
  
      ***---
       EXIT-PGM.
           goback.
