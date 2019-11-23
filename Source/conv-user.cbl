       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-user.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "user.sl".
       SELECT USER-old
           ASSIGN       TO  "users-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS AUTOMATIC
           FILE STATUS  IS STATUS-USER-old
           RECORD KEY   IS user-old-chiave.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "user.fd".
       FD  USER-old.
       01 user-old-rec.
           05 user-old-chiave.
      * PROMPT =USER ID
               10 user-old-cod         PIC  x(10).
           05 user-old-dati.
      * PROMPT =NAME
               10 user-old-name        PIC  x(30).
      * PROMPT =Password
               10 user-old-pass        PIC  x(20).
               10 user-old-pass-data   PIC  9(8).
               10 user-old-path-bullzip            PIC  x(128).
               10 user-old-dati-comuni.
                   15 user-old-data-creazione          PIC  9(8).
                   15 user-old-ora-creazione           PIC  9(8).
                   15 user-old-utente-creazione        PIC  x(10).
                   15 user-old-data-ultima-modifica    PIC  9(8).
                   15 user-old-ora-ultima-modifica     PIC  9(8).
                   15 user-old-utente-ultima-modifica  PIC  x(10).
               10 user-old-vuoti.
                   15 user-old-msg-promo   PIC  9.
                   15 user-old-msg-banner-T            PIC  9.
                   15 user-old-msg-banner-G            PIC  9.
                   15 user-old-ev-man      PIC  9(1).
                   15 user-old-num-vuoto-1 PIC  9(9).
                   15 user-old-num-vuoto-2 PIC  9(15).
                   15 user-old-num-vuoto-3 PIC  9(15).
                   15 user-old-tel-diretto PIC  x(20).
                   15 user-old-SO          PIC  xx.
                   15 user-old-Office      PIC  xx.
                   15 user-old-alfa-vuoto-2           PIC  x(16).
                   15 user-old-alfa-vuoto-3           PIC  x(20).

       working-storage section.

       77  status-user  pic xx.
       77  status-user-old  pic xx.

       77  tot   pic 9(10).

       procedure division.
      ***---
       MAIN.
           open input user-old.
           open i-o   user.
           move low-value to user-old-chiave.
           start user-old key >= user-old-chiave.
           perform until 1 = 2
              read user-old next at end exit perform end-read
              move user-old-cod          to user-cod          
              move user-old-name         to user-name         
              move user-old-pass         to user-pass         
              move user-old-pass-data    to user-pass-data    
              move user-old-path-bullzip to user-path-bullzip 
              move user-old-dati-comuni  to user-dati-comuni
              move user-old-msg-promo    to user-msg-promo    
              move user-old-msg-banner-T to user-msg-banner-T 
              move user-old-msg-banner-G to user-msg-banner-G 
              move user-old-ev-man       to user-ev-man       
              move user-old-num-vuoto-1  to user-num-vuoto-1  
              move user-old-num-vuoto-2  to user-num-vuoto-2  
              move user-old-num-vuoto-3  to user-num-vuoto-3  
              move user-old-tel-diretto  to user-tel-diretto  
              move user-old-SO           to user-SO           
              move user-old-Office       to user-Office       
              write user-rec
           end-perform.
                 
           close user user-old.

           goback.
