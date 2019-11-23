       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      data-offor.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tlistini.sl".
           copy "rlistini.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "tlistini.fd".
           copy "rlistini.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Validità offerte fornitori".

      * FILE STATUS
       77  status-tlistini      pic xx.
       77  status-rlistini      pic xx.

      * VARIABILI 
       77  cont-n pic 9(5).
       77  cont-o pic 9(5).

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
           move 0 to cont-n cont-o.

      ***---
       OPEN-FILES.
           open i-o tlistini rlistini.

      ***---
       ELABORAZIONE.
           move low-value to tlis-rec.
           start tlistini key >= tlis-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tlistini next at end exit perform end-read
                    if tlis-fine-val = 20121231
                       move 20131231 to tlis-fine-val
                       rewrite tlis-rec
                       add 1 to cont-n
                    end-if
                    move low-value   to rlis-rec
                    move tlis-codice to rlis-codice
                    start rlistini key >= rlis-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read rlistini next 
                                  at end exit perform 
                             end-read
                             if tlis-codice not = rlis-codice
                                exit perform
                             end-if                                
                             if rlis-fine-val = 20121231
                                move 20131231 to rlis-fine-val
                                rewrite rlis-rec
                             end-if
                          end-perform
                    end-start
                 end-perform
           end-start.

           display message "Operazione terminata!"
                    x"0d0a""AGGIORNATI: ",  cont-n,
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close tlistini rlistini.

      ***---
       EXIT-PGM.
           goback.
