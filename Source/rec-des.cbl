       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      rec-des.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "destini.sl".

       SELECT old-destini
           ASSIGN       TO  "old-destini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-destini
           RECORD KEY   IS old-des-chiave
           ALTERNATE RECORD KEY IS K1 = old-des-ragsoc-1, 
           old-des-codice, old-des-prog
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-localita = old-des-localita
           WITH DUPLICATES.


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "destini.fd".

       FD  old-destini.
       01 old-des-rec.
           05 old-des-chiave.
               10 old-des-codice       PIC  9(5).
               10 old-des-prog         PIC  9(5).
           05 old-des-dati.
               10 old-des-ragsoc-1     PIC  x(40).
               10 old-des-ragsoc-2     PIC  x(40).
               10 old-des-indirizzo    PIC  x(40).
               10 old-des-cap          PIC  x(5).
               10 old-des-localita     PIC  x(35).
               10 old-des-prov         PIC  x(2).
               10 old-des-nazione      PIC  x(3).
               10 old-des-telef-1      PIC  x(15).
               10 old-des-telef-2      PIC  x(15).
               10 old-des-fax          PIC  X(15).
               10 old-des-mail         PIC  X(30).
               10 old-des-referente    PIC  x(30).
               10 old-des-vettore      PIC  9(5).
               10 old-des-deposito-UTF PIC  x.
               10 old-des-superamento-500          PIC  x.
               10 old-des-stato        PIC  x.
                   88 old-des-attivo VALUE IS "A". 
                   88 old-des-disattivo VALUE IS "D". 
                   88 old-des-bloccato VALUE IS "B". 
               10 old-des-dati-comuni.
                   15 old-des-data-creazione           PIC  9(8).
                   15 old-des-ora-creazione            PIC  9(8).
                   15 old-des-utente-creazione         PIC  X(10).
                   15 old-des-data-ultima-modifica     PIC  9(8).
                   15 old-des-ora-ultima-modifica      PIC  9(8).
                   15 old-des-utente-ultima-modifica   PIC  X(10).
               10 old-des-vuoti.
                   15 old-des-num-vuoto-1  PIC  9(15).
                   15 old-des-num-vuoto-2  PIC  9(15).
                   15 old-des-num-vuoto-3  PIC  9(15).
                   15 old-des-invio-fatt   PIC  x.
                       88 old-des-si-invio VALUE IS "S". 
                       88 old-des-no-invio VALUE IS "N". 
                   15 old-des-alfa-vuoto-1 PIC  X(19).
                   15 old-des-alfa-vuoto-2 PIC  X(20).
                   15 old-des-alfa-vuoto-3 PIC  X(20).

       working-storage section.
       77  status-destini pic xx.
       77  save-codice    pic 9(5).
       77  tot            pic 9(4).

       procedure division.
      ***---
       MAIN.
           open i-o destini old-destini.

           move 245       to save-codice.
           perform LOOP.
           display message "TRASF. " tot " PER IDIR"

           move 374       to save-codice.
           perform LOOP.
           display message "TRASF. " tot " PER SIME"

           move 444       to save-codice.
           perform LOOP.
           display message "TRASF. " tot " PER DEMAUTO"

           close destini old-destini.

           goback.

      ***---
       LOOP.
           move 0 to tot.
           move save-codice to old-des-codice.
           move low-value   to old-des-prog.
           start old-destini key is >= old-des-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read old-destini next at end exit perform end-read
              if old-des-codice not = save-codice
                 exit perform
              end-if
              move old-des-rec to des-rec
              write des-rec 
                    invalid continue 
                not invalid add 1 to tot
              end-write
           end-perform.
