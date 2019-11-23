       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      tpromo-pren-si.
       REMARKS. Valorizzazione a true del check prenotazioni
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rpromo.sl". 
           copy "promoeva.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "rpromo.fd".  
           copy "promoeva.fd".

       WORKING-STORAGE SECTION.  
       77  status-rpromo    pic X(2).
       77  status-promoeva  pic X(2).

      ******************************************************************
       PROCEDURE DIVISION.     
       MAIN-PRG.           
           open i-o rpromo promoeva   

           move low-value to rpr-chiave.

           start rpromo key >= rpr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rpromo next 
                         at end exit perform
                    end-read
                    set rpr-prenotazioni-si to true
                    rewrite rpr-rec
                 end-perform
           end-start.

           move low-value to pev-chiave.

           start promoeva key >= pev-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read promoeva next 
                         at end exit perform
                    end-read
                    set pev-rpr-prenotazioni-si to true
                    rewrite pev-rec
                 end-perform
           end-start.

           close rpromo promoeva.
           display message "ELABORAZIONE TERMINATA".
           goback.
