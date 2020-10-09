       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-pesi-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tpromo.sl".
           copy "rpromo.sl". 

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tpromo.fd".
           copy "rpromo.fd".

       WORKING-STORAGE SECTION.
      * COPY
       77  status-tpromo         pic xx.
       77  status-rpromo         pic xx.
                                              
       77  wk-tpr-rec            pic x(5000).
       77  wk-rpr-rec            pic x(5000) occurs 5000.
       77  idx                   pic 9(5) value 0.
       77  totIdx                pic 9(5) value 0.

      * COSTANTI
       78  titolo                value "GESLUX - Copia promo".

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.

      * VARIABILI

       LINKAGE SECTION.
       77  link-tpr-codice      pic 9(15).
       77  link-user            pic x(20).

      ******************************************************************
       PROCEDURE DIVISION USING link-tpr-codice link-user.

       DECLARATIVES.             
      ***---
       TPROMO-ERR SECTION.
           use after error procedure on tpromo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tpromo
           when "39"
                set errori to true
                display message "File [TPROMO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TPROMO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       RPROMO-ERR SECTION.
           use after error procedure on rpromo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rpromo
           when "39"
                set errori to true
                display message "File [RPROMO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RPROMO] inesistente"
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
           set tutto-ok  to true.
           set trovato   to false.

      ***---
       OPEN-FILES.
           open i-o tpromo 
           open i-o rpromo.

      ***---
       ELABORAZIONE.
           move link-tpr-codice to tpr-codice.
           read tpromo no lock.
           move tpr-rec to wk-tpr-rec.
           move low-value  to rpr-rec.
           move tpr-codice to rpr-codice.
           start rpromo key >= rpr-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read rpromo next at end exit perform end-read
              if rpr-codice not = tpr-codice
                 exit perform
              end-if
              add 1 to idx totIdx
              move rpr-rec to wk-rpr-rec(idx)
           end-perform.
           move 999999 to tpr-codice. |start fittizia delle promo
           start tpromo key < tpr-chiave.
           read tpromo previous.
           move tpr-codice to link-tpr-codice.
           move wk-tpr-rec to tpr-rec
           move link-tpr-codice to tpr-codice.
           perform until 1 = 2      
              initialize tpr-comuni replacing numeric data by zeroes
                                         alphanumeric data by spaces
              move link-user to tpr-utente-creazione
              accept tpr-ora-creazione from time
              accept tpr-data-modifica from century-date
              write tpr-rec 
                    invalid add 1 to tpr-codice
                not invalid exit perform
              end-write
           end-perform.
           move tpr-codice to link-tpr-codice.

           perform varying idx from 1 by 1 
                     until idx > totIdx
              move wk-rpr-rec(idx) to rpr-rec       
              initialize rpr-comuni replacing numeric data by zeroes
                                         alphanumeric data by spaces
              move link-user to rpr-utente-creazione
              accept rpr-ora-creazione from time
              accept rpr-data-modifica from century-date
              move tpr-codice to rpr-codice
              write rpr-rec
           end-perform.

      ***---
       CLOSE-FILES.
           close tpromo rpromo.

      ***---
       EXIT-PGM.
           goback.
