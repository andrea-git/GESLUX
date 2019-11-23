       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ordf-sol.
       AUTHOR.                          Andrea.
       REMARKS. Crea dei solleciti per ogni ordine fornitore con data consegna

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "sordforn.sl".
           copy "tordforn.sl".
           copy "rordforn.sl".
           copy "rlistini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "sordforn.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "rlistini.fd".

       WORKING-STORAGE SECTION.
      * COPY

       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.
       77  status-sordforn       pic xx.
       77  status-rlistini       pic xx.

       77  como-data             pic 9(8).
       77  data-soll             pic 9(8).
       77  lead-time             pic 9(3).

       LINKAGE SECTION.
       01  link-chiave.
         05 link-anno            pic 9(4).
         05 link-numero          pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION USING link-chiave.

      ***---
       MAIN-PRG.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.


      ***---
       OPEN-FILES.
           open input tordforn rordforn rlistini.
           open i-o sordforn.

      ***---
       ELABORAZIONE.
           move 0 to lead-time.
           move link-chiave to tof-chiave.
           read tordforn no lock
                invalid continue
            not invalid  

                initialize sof-rec replacing numeric data by zeroes
                                        alphanumeric data by spaces
                move tof-chiave           to sof-chiave-testa
                move tof-utente-creazione to sof-utente-creazione
                accept sof-data-creazione from century-date
                accept sof-ora-creazione  from time

                move tof-chiave to rof-chiave
                move low-value  to rof-riga
                start rordforn key >= rof-chiave
                      invalid continue
                  not invalid
                      perform until 1 = 2
                         read rordforn next at end exit perform end-read
                         if rof-chiave-testa not = tof-chiave
                            exit perform
                         end-if       
                         move rof-cod-listino  to rlis-codice
                         move rof-cod-articolo to rlis-articolo
                         read rlistini no lock
                              invalid move 0 to rlis-lead-time
                         end-read
      *****                   if rlis-lead-time = 0
                            move tof-data-consegna to sof-data-arr
      *****                   else
      *****                      perform CALCOLA-DATA-SOLLECITI
      *****                      move data-soll to sof-data-arr
      *****                      if rlis-lead-time > lead-time
      *****                         move rlis-lead-time to lead-time
      *****                      end-if
      *****                   end-if
                         add 1 to sof-prog
                         move rof-qta-ord to sof-qta
                         write sof-rec invalid rewrite sof-rec end-write
                      end-perform
                end-start
           end-read.   

           initialize sof-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move tof-chiave           to sof-chiave-testa.
           move tof-utente-creazione to sof-utente-creazione.
           accept sof-data-creazione from century-date.
           accept sof-ora-creazione  from time.
      *****     if lead-time > 0
      *****        move lead-time to rlis-lead-time
      *****        perform CALCOLA-DATA-SOLLECITI
      *****        move data-soll to sof-data-arr
      *****     else
              move tof-data-consegna to sof-data-arr
      *****     end-if.
           write sof-rec invalid rewrite sof-rec end-write.

      ***---
       CALCOLA-DATA-SOLLECITI.
      *****     accept data-soll from century-date.
           move tof-data-consegna to data-soll.
           compute como-data = function integer-of-date (data-soll).
           add rlis-lead-time to como-data.
           compute data-soll = function date-of-integer (como-data).

      ***---
       CLOSE-FILES.
           close tordforn rordforn sordforn rlistini.

      ***---
       EXIT-PGM.
           goback.
