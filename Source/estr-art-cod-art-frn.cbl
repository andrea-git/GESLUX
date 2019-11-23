       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      estr-art-cod-art-frn.
       AUTHOR.                          Andrea.
       REMARKS. 
           - trova l’articolo con il cod_art_forn = alla colonna A
           - gli affianca il codice articolo Lubex
           - riscrive
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
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Cambio cod.art.forn. su articoli".

      * FILE STATUS
       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.

       77  tot-elab              pic 9(5) value 0.
       77  n-elab                pic 9(5) value 0.

       77  cod-art-forn          pic x(15).

       77  idx                   pic 9(5) value 0.
       01  el-riga               pic x(30) occurs 99999.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 trovato            value 1 false 0. 

      ******************************************************************
       LINKAGE SECTION.

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

      ***---
       OPEN-FILES.
           open input articoli. 
           move "cod_art_forn.txt" to wstampa.
           open input lineseq.

      ***---
       ELABORAZIONE.
           move low-value to line-riga.
           perform until 1 = 2
              read lineseq next at end exit perform end-read 
              set trovato to false
              move line-riga(1:15) to cod-art-forn
              if cod-art-forn not = spaces
                 add 1 to tot-elab
                 move low-value to art-rec
                 start articoli key >= art-chiave
                       invalid continue
                 end-start
                 perform until 1 = 2
                    read articoli next at end exit perform end-read
                    if art-cod-art-frn = cod-art-forn
                       add 1 to idx
                       set trovato to true
                       add 1 to n-elab
                       exit perform
                    end-if
                 end-perform
                 initialize el-riga(idx)
                 if trovato
                    string cod-art-forn delimited size
                           ";"          delimited size
                           art-codice   delimited size
                           ";"          delimited size
                      into el-riga(idx)
                    end-string
                 else
                    move line-riga to el-riga(idx)
                 end-if
              end-if
           end-perform.
           if idx not = 0
              close lineseq
              open output lineseq
              perform varying idx from 1 by 1 
                        until idx > n-elab
                 move el-riga(idx) to line-riga
                 write line-riga
              end-perform
           end-if.
              

      ***---
       CLOSE-FILES.
           close articoli lineseq. 

      ***---
       EXIT-PGM. 
           display message "ELABORATI: " tot-elab
                    x"0d0a""TROVATI: " n-elab
                     title titolo
                      icon 2.

           goback.
