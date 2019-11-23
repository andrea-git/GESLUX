       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      del-rif-promo.
       AUTHOR.                          Andrea.
       REMARKS.
           Serve per cancellare i riferimenti a promo non più
           esistenti dalle righe dei master

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mrordini.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "mrordini.fd".
           copy "tpromo.fd".
           copy "rpromo.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "GESLUX - Cancellazione riferimento promo".

      * FILE-STATUS
       77  status-mrordini           pic xx.
       77  status-tpromo             pic xx.
       77  status-rpromo             pic xx.

      * VARIABILI
       77  promo-annullate           pic 9(5) value 0.

      *****************************************************************

       PROCEDURE DIVISION.
       

      ***---
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       OPEN-FILES.
           open input tpromo rpromo.
           open i-o   mrordini.

      ***---
       ELABORAZIONE.
           move low-value to mro-rec.
           move 1         to mro-promo.
           start mrordini key >= mro-k-promo
                 invalid continue
           end-start.
           perform until 1 = 2 
              read mrordini next at end exit perform end-read
              move mro-promo to tpr-codice
              read tpromo no lock 
                   invalid
                   move 0 to mro-promo
                   rewrite mro-rec
                   add 1 to promo-annullate
               not invalid
                   if mro-bli-codice not = 0
                      move mro-bli-codice to mro-cod-articolo
                   end-if
                   move mro-promo        to rpr-codice
                   move mro-cod-articolo to rpr-articolo
                   read rpromo no lock
                        invalid
                        move 0 to mro-promo
                        rewrite mro-rec
                        add 1 to promo-annullate
                   end-read
              end-read
           end-perform.
           display message "Operazione terminata!"
                    x"0d0a""Cancellati riferimenti non validi: "
                           promo-annullate
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close tpromo mrordini rpromo.

      ***---
       EXIT-PGM.
           goback.
