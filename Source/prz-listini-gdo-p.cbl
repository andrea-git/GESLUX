       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      prz-listini-gdo-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "listini.sl".
           copy "tgrupgdo.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "listini.fd".
           copy "tgrupgdo.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Cambio prezzo listini GDO".

      * FILE STATUS
       77  status-listini      pic xx.
       77  status-tgrupgdo     pic xx.

      * VARIABILI 
       77  cont-n pic 9(15).

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
           move 0 to cont-n.

      ***---
       OPEN-FILES.
           open i-o   listini 
           open input tgrupgdo.

      ***---
       ELABORAZIONE.                
           move low-value to lst-chiave.
           move 20210101 to lst-data.
           start listini key >= lst-k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read listini next at end exit perform end-read
                    move lst-gdo to gdo-chiave
                    read tgrupgdo no lock
                         invalid continue
                     not invalid
                         if gdo-tipocli = 11
                            move 999999,99 to lst-prezzo
                            rewrite lst-rec
                            add 1 to cont-n
                         end-if
                    end-read
                 end-perform
           end-start.

           display message "Operazione terminata!"
                    x"0d0a""AGGIORNATI: ",  cont-n,
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close listini tgrupgdo.

      ***---
       EXIT-PGM.
           goback.
