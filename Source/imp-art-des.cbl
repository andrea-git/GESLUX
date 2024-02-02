       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-art-des.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Importazione descrizioni in lingua".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-articoli       pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  r-des-spa             pic x(60).
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI
       77  como-des              pic x(50).
       77  num-rec-ing           pic 9(6)   value 0.
       77  num-rec-spa           pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.      

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.                    
           move "imp-art-des.csv" to wstampa.

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
              unstring line-riga delimited by ";"
                       into art-codice
                            r-des-spa
              end-unstring
              read articoli no lock
                   invalid add 1 to num-rec-ko
               not invalid
                   move r-des-spa       to art-des-spa
                   rewrite art-rec
                   add 1 to num-rec-spa
              end-read
           end-perform.

           move low-value to art-chiave.
           start articoli key >= art-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read articoli next at end exit perform end-read
                    add 1 to num-rec-ing
                    if art-des-ing = spaces
                       move art-descrizione1 to como-des
                       inspect como-des 
                               replacing trailing spaces by low-value
                       string como-des         delimited low-value
                              art-descrizione2 delimited size
                         into art-des-ing
                       end-string
                    end-if
                    if art-des-spa = spaces
                       move art-descrizione1 to como-des
                       inspect como-des 
                               replacing trailing spaces by low-value
                       string como-des         delimited low-value
                              art-descrizione2 delimited size
                         into art-des-spa
                       end-string
                       end-if
                    rewrite art-rec
                 end-perform
           end-start.

           display message "Operazione terminata!"              
                    x"0d0a""ARTICOLI (SPAGNOLO): ", num-rec-spa,
                    x"0d0a""ARTICOLI (INGLESE): ", num-rec-ing,
                    x"0d0a""ERRATI: ", num-rec-ko
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close articoli lineseq.

      ***---
       EXIT-PGM.
           goback.
