       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      del-promo-f.
       AUTHOR.                          Andrea.
       REMARKS.
           Serve per cancellare le promo fittizie che fanno riferimento
           a master inesistenti o CHIUSI.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "mtordini.fd".
           copy "tpromo.fd".
           copy "rpromo.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo 
           value "GESLUX - Cancellazione promo su master non validi".

      * FILE-STATUS
       77  status-mtordini           pic xx.
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
           open i-o   tpromo rpromo.
           open input mtordini.

      ***---
       ELABORAZIONE.
           move 999999  to tpr-codice.
           start tpromo key >= tpr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2 
                   read tpromo next at end exit perform end-read
                   move tpr-descrizione(1:4) to mto-anno
                   move tpr-descrizione(6:8) to mto-numero
                   read mtordini no lock
                        invalid
                        perform CANCELLA-PROMO
                    not invalid
                        if mto-chiuso
                           perform CANCELLA-PROMO
                        end-if
                  end-read
               end-perform
           end-start.

           move low-value  to rpr-rec.
           start rpromo key >= rpr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                 read rpromo next at end exit perform end-read
                 if rpr-codice not = 0
                    exit perform
                 end-if
                 delete rpromo record invalid continue end-delete
                 end-perform
           end-start.

           display message "Operazione terminata!"
                    x"0d0a""Cancellati riferimenti non validi: "
                           promo-annullate
                     title titolo.

      ***---
       CANCELLA-PROMO.
           delete tpromo  record
                  invalid continue
              not invalid
                  add 1 to promo-annullate
                  move low-value  to rpr-rec
                  move tpr-codice to rpr-codice
                  start rpromo key >= rpr-chiave
                        invalid continue
                    not invalid
                        perform until 1 = 2
                           read rpromo next at end exit perform end-read
                           if rpr-codice not = tpr-codice
                              exit perform
                           end-if
                           delete rpromo record invalid continue 
                           end-delete
                        end-perform
                  end-start
           end-delete.

      ***---                   
       CLOSE-FILES.
           close tpromo mtordini rpromo.

      ***---
       EXIT-PGM.
           goback.
