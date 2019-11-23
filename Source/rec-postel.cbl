       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rec-postel.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "recapiti.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "recapiti.fd".

       working-storage section.
       77  status-clienti          pic xx.
       77  status-recapiti         pic xx.

       77  tot-recapiti            pic 9(10).
       77  tot-cambio-invio        pic 9(10).

       procedure division.
      ***---
       MAIN.
           open input clienti 
           open i-o   recapiti.

           move 0 to tot-cambio-invio tot-recapiti.
           move low-value to cli-codice.
           set cli-tipo-C to true.
           start clienti key is >= cli-chiave.
           perform until 1 = 2
              read clienti next at end exit perform end-read
              if cli-tipo-F            exit perform end-if
              move cli-codice to rec-codice
              read recapiti no lock
                   invalid
                   initialize rec-dati replacing numeric data by zeroes
                                            alphanumeric data by spaces
                   set rec-invio-manuale to true
                   accept rec-data-creazione from century-date
                   accept rec-ora-creazione  from time
                   move "BOSS" to rec-utente-creazione
                   write rec-rec
                   add 1 to tot-recapiti
               not invalid
                   if rec-invio-postel-OLD
                      set rec-invio-manuale to true
                      rewrite rec-rec
                      add 1 to tot-cambio-invio
                   end-if
              end-read
           end-perform.

           display message "CREATI ", tot-recapiti, " RECAPITI".
           display message "MODIFCATI ", tot-cambio-invio, " INVII".
                 
           close recapiti clienti.

           goback.
