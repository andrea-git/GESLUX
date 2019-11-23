       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      leva-valore.
       AUTHOR.                          Andrea.
       REMARKS. 
           elimina il valore 801971955 da articoli e offerte fornitore
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "rlistini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "rlistini.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo                value "Leva codice".
       78  78-codice             value 801971955.

      * FILE STATUS
       77  status-articoli       pic xx.
       77  status-rlistini       pic xx.

       77  tot-elab              pic 9(5) value 0.
       77  n-elab                pic 9(5) value 0.

      ******************************************************************
       LINKAGE SECTION.

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
           open i-o articoli rlistini. 

      ***---
       ELABORAZIONE. 
           move low-value to art-codice.
           start articoli key >= art-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read articoli next at end exit perform end-read
                    add 1 to tot-elab
                    if art-cod-art-frn = 78-codice
                       add 1 to n-elab
                       move spaces to art-cod-art-frn
                       rewrite art-rec
                    end-if
                 end-perform
           end-start.

           move low-value to rlis-chiave.
           start rlistini key >= rlis-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rlistini next at end exit perform end-read
                    add 1 to tot-elab
                    if rlis-art-forn = 78-codice                   
                       add 1 to n-elab
                       move spaces to rlis-art-forn
                       rewrite rlis-rec
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close articoli rlistini.

      ***---
       EXIT-PGM. 
           display message "ELABORATI: " tot-elab
                    x"0d0a""MODIFICATI: " n-elab
                     title titolo
                      icon 2.

           goback.
