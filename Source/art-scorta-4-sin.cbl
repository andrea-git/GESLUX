       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      art-scorta-4-sin.
       AUTHOR.                          Andrea.
       REMARKS. Passa gli articoli e li mette in scorta 4 e mag standard SIN
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Batch articoli scorta 4 SIN".

      * FILE STATUS
       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.
       77  wstampa               pic x(256).  

      * VARIABILI
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0.
       
      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

      ******************************************************************
       PROCEDURE DIVISION.

      ***---
       MAIN-PRG.
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
           move "art-scorta-4-sin.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   articoli. 
           open input lineseq.   

      ***---
       ELABORAZIONE. 
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into art-codice
              end-unstring
              read articoli
                   invalid add 1 to num-rec-ko
               not invalid 
                   add  1     to num-rec-ok
                   move 4     to art-scorta
                   move "SIN" to art-mag-std
                   rewrite art-rec invalid continue end-rewrite
              end-read 
           end-perform.
      
           display message "Operazione terminata!"
                    x"0d0a""ELABORATI: ", num-rec,
                    x"0d0a""CORRETTI: ", num-rec-ok,
                    x"0d0a""ERRATI: ", num-rec-ko
                     title titolo
                      icon 2.     

      ***---
       CLOSE-FILES.
           close articoli lineseq.

      ***---
       EXIT-PGM. 
           goback.
