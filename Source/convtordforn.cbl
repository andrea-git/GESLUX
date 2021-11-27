       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convtordforn.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordforn.sl".
      ** IN CASO DI MODIFICA ADEGUARE ANCHE TMP-ROF-AUTO
       SELECT tordforn-old
           ASSIGN       TO  "tordforn-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-tordforn-old
           RECORD KEY   IS old-tof-chiave
           ALTERNATE RECORD KEY IS old-tof-k-causale = old-tof-causale, 
           old-tof-chiave
           ALTERNATE RECORD KEY IS old-tof-k-stato = old-tof-stato, 
           old-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-fornitore = old-tof-cod-forn, 
           old-tof-destino, old-tof-stato, old-tof-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-tof-k-data = old-tof-data-ordine
            old-tof-chiave
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "tordforn.fd".

      *
      *
       FD  tordforn-old.
       01 old-tof-rec.
           05 old-tof-chiave.
               10 old-tof-anno         PIC  9(4).
               10 old-tof-numero       PIC  9(8).
           05 old-tof-dati.
               10 old-tof-causale      PIC  x(4).
               10 old-tof-data-ordine  PIC  9(8).
               10 FILLER           PIC  9(8).
               10 old-tof-cod-forn     PIC  9(5).
               10 old-tof-destino      PIC  9(5).
               10 old-tof-dati-forn.
                   15 old-tof-referente    PIC  x(40).
                   15 old-tof-tel-dir      PIC  x(20).
                   15 old-tof-fax          PIC  x(20).
                   15 old-tof-email        PIC  x(100).
               10 FILLER           PIC  9(12).
               10 old-tof-cliente      PIC  9(5).
               10 old-tof-data-listino PIC  9(8).
               10 old-tof-cod-pagamento            PIC  x(3).
               10 old-tof-cod-ese-iva  PIC  x(3).
               10 old-tof-tipo-scarico PIC  x.               
               10 old-tof-data-consegna            PIC  9(8).
               10 old-tof-mese-rif     PIC  99.
               10 old-tof-pz-tot       PIC  9(8).
               10 old-tof-pz-arrivati  PIC  9(8).
               10 old-tof-tipo-creazione           PIC  x.
               10 old-tof-stato        PIC  x.            
               10 old-tof-stato-evasione           PIC  x.
               10 old-tof-da-confermare            PIC  9(1).
               10 old-tof-rivisto      PIC  9(1).
               10 old-tof-programmazione           PIC  9.
               10 old-tof-tipo-invio   PIC  x.
               10 old-tof-dati-invio.
                   15 old-tof-data-invio   PIC  9(8).
                   15 old-tof-ora-invio    PIC  9(8).
                   15 old-tof-utente-invio PIC  X(10).
               10 old-tof-tipo-chiusura            PIC  x.
               10 old-tof-dati-chiusura.
                   15 old-tof-nota-chiusura            PIC  x(50).
                   15 old-tof-data-chiusura            PIC  9(8).
                   15 old-tof-ora-chiusura PIC  9(8).
                   15 old-tof-utente-chiusura          PIC  X(10).
               10 old-tof-dati-comuni.
                   15 old-tof-data-creazione           PIC  9(8).
                   15 old-tof-ora-creazione            PIC  9(8).
                   15 old-tof-utente-creazione         PIC  X(10).
                   15 old-tof-data-ultima-modifica     PIC  9(8).
                   15 old-tof-ora-ultima-modifica      PIC  9(8).
                   15 old-tof-utente-ultima-modifica   PIC  X(10).
               10 old-tof-vuoti.
                   15 old-tof-destino-c    PIC  9(5).
                   15 old-tof-promo        PIC  9(15).
                   15 old-tof-franco-part  PIC  9.
                   15 old-tof-aperto       PIC  9(1).
                   15 old-tof-st-dati-fatt PIC  9.
                   15 old-tof-evasione     PIC  9(6).
                   15 old-tof-num-vuoto-2  PIC  9(7).
                   15 old-tof-num-vuoto-3  PIC  9(18).
                   15 old-tof-alfa-vuoto-1 PIC  X(20).
                   15 old-tof-alfa-vuoto-2 PIC  X(20).
                   15 old-tof-stampante    PIC  X.
                   15 old-tof-alfa-vuoto-3 PIC  X(19).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tordforn      pic X(2).
       77  status-tordforn-old  pic X(2).

       77  cont                 PIC 9(6).
       77  cont-ed              PIC zzz.zz9.
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tordforn
                     tordforn-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file tordforn?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "tordforn"
                          x"22"
                          " in "
                          x"22"
                          "tordforn-old"
                          x"22"
                          "."
                          type mb-yes-no
                          default mb-no
                          giving scelta
                          icon 2
           if scelta = mb-yes
              perform CONVERSIONE
           end-if.

           goback.


      ***---
       CONVERSIONE.
           move 0 to cont

           open input  tordforn-old.
           open output tordforn.


           move low-value to old-tof-chiave.

           start tordforn-old key >= old-tof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordforn-old next at end exit perform end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tordforn
                 tordforn-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize tof-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move old-tof-rec to tof-rec.
       
           write tof-rec.
