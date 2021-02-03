       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      fix-mag-erot.
       AUTHOR.                          Andrea.
       REMARKS. Partendo da un csv di movimenti di magazzino corregge
                il magazzino del progressivo con quello della causale.
                Imposta lo stesso progressivo e se non c'è lo crea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rmovmag.sl".
           copy "lineseq.sl".
           copy "progmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "rmovmag.fd".
           copy "lineseq.fd".
           copy "progmag.fd".

       WORKING-STORAGE SECTION.
           COPY "link-wprogmag.def".

       78  titolo    value "Correzione progressivi ROT".

       77  status-lineseq   pic xx.  
       77  status-rmovmag   pic xx.
       77  status-progmag   pic xx.
       77  wstampa          pic x(256).

       77  como-data        pic 9(8).
       77  como-ora         pic 9(8).  
                                             
       77  n-rec            pic 9(5) value 0.
       77  n-rec-ok         pic 9(5) value 0.
       77  n-rec-ko         pic 9(5) value 0.
       77  n-rec-no         pic 9(5) value 0.
       77  n-rec-prg        pic 9(5) value 0.

       01  l-chiave.
         05 l-anno          pic 9(4).
         05 l-numero        pic 9(8).
         05 l-riga          pic 9(5).
                       

      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT.
           move "fix-mag-erot.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   rmovmag.
           open input lineseq progmag.

      ***---
       ELABORAZIONE.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              add 1 to n-rec
              unstring line-riga delimited by ";"
                            into l-anno
                                 l-numero
                                 l-riga
              move l-chiave to rmo-chiave
              read rmovmag no lock
                   invalid add 1 to n-rec-ko
               not invalid 
                   if rmo-codmag = "ROT"
                      add 1 to n-rec-no
                   else
                      move rmo-chiave-progmag to prg-chiave
                      read progmag no lock 
                           invalid add 1 to n-rec-ko |Salvo peso utf e non utf
                       not invalid
                           move "ROT" to rmo-codmag
                           rewrite rmo-rec
                           add 1 to n-rec-ok
                           perform VERIFICA-PRG
                      end-read
                   end-if
              end-read
           end-perform.

      ***---
       VERIFICA-PRG.
           move rmo-chiave-progmag to prg-chiave
           read progmag no lock
                invalid 
                perform CREA-PRG
                add 1 to n-rec-prg
           end-read.

      ***---
       CREA-PRG.
           initialize link-wprogmag replacing numeric data by zeroes
                                         alphanumeric data by spaces.

           set link-batch       to true.
           move "FIX-MOV-EROT"  to link-user.

           move prg-chiave to link-key

           move prg-peso-utf       to link-utf.
           move prg-peso-non-utf   to link-non-utf.

           add link-utf to link-non-utf giving link-peso.

           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".


      ***---
       CLOSE-FILES.
           close rmovmag lineseq progmag.

      ***---
       EXIT-PGM.
           display message "ELABORAZIONE TERMINATA" 
                    x"0d0a""RECORD TRATTATI: " n-rec         
                    x"0d0a""RECORD SCARTATI: " n-rec-no
                    x"0d0a""RECORD ERRATI: " n-rec-ko
                    x"0d0a""RECORD VARIATI: " n-rec-ok
                    x"0d0a""PROGRESSIVI AGGIUNTI: " n-rec-prg
                     title titolo
           display message "Procedere con il ricalcolo delle giacenze"
                     title titolo
           goback.
