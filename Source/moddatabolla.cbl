       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      moddatabolla.
       AUTHOR.                          Andrea.
       REMARKS. Dato un csv con numero/anno e data, leggo l'evasione 
                e modifico la data della bolla.
                La data è in formato GG/MM/AAAA
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "tordini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "tordini.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Modifica data bolla".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-tordini        pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI                          
       77  r-anno                pic x(4).
       77  r-numero              pic x(8).
       77  r-data                pic x(10).
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0.
       77  num-rec-nn            pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "39"
                set errori to true
                display message "File [LINESEQ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [LINESEQ] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       END DECLARATIVES.

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
           move "evasioni.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   tordini.
           open input lineseq.

      ***---
       ELABORAZIONE.
           |Salto l'intestazione
           read lineseq next.
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga r-anno r-numero r-data
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into r-anno
                            r-numero
                            r-data
              end-unstring
              move r-anno to tor-anno
              call "C$JUSTIFY" using r-numero, "R"
              inspect r-numero replacing leading x"20" by x"30"
              move r-numero to tor-numero
              read tordini
                   invalid add 1 to num-rec-ko
               not invalid 
                   add 1 to num-rec-ok
                   if tor-data-bolla = 0
                      add 1 to num-rec-nn
                   else                                
                      move r-data(7:4) to tor-data-bolla(1:4)
                      move r-data(4:2) to tor-data-bolla(5:2)
                      move r-data(1:2) to tor-data-bolla(7:2)
                      rewrite tor-rec invalid continue end-rewrite
                   end-if
              end-read 
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""BOLLE: ", num-rec,
                    x"0d0a""IMPORTATI: ", num-rec-ok,
                    x"0d0a""NON TROVATI: ", num-rec-ko
                    x"0d0a""SENZA BOLLA: ", num-rec-nn
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close tordini lineseq.

      ***---
       EXIT-PGM.
           goback.
