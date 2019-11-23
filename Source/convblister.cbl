       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convblister.
       REMARKS. conversione dalla versione 2.4 verso la 2.5 
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "blister.sl".
       SELECT blister-old
           ASSIGN       TO  "blister-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-blister-old
           RECORD KEY   IS old-bli-chiave
           ALTERNATE RECORD KEY IS old-k-magaz = old-bli-magazzino, 
                                   old-bli-codice
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-des = old-bli-descrizione
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-bli-codice-ean-1 OF blister-old
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-bli-codice-ean-2 OF blister-old
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-bli-codice-ean-3 OF blister-old
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-bli-codice-ean-4 OF blister-old
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-bli-codice-ean-5 OF blister-old
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "blister.fd".
       FD  blister-old.      
       01 old-bli-rec.
           05 old-bli-chiave.
               10 old-bli-codice       PIC  9(6).
           05 old-bli-dati.
               10 old-bli-descrizione  PIC  x(50).
               10 old-bli-magazzino    PIC  x(3).
               10 old-bli-prezzo       PIC  9(6)v99.
               10 old-bli-tab-componenti.
                   15 old-bli-elemento
                              OCCURS 50 TIMES.
                       20 old-bli-el-articolo  PIC  9(6).
                       20 old-bli-el-qta       PIC  9(8).
                       20 old-bli-el-perce     PIC  9(3)v99.
               10 old-bli-stato        PIC  x.
                   88 old-bli-attivo VALUE IS "A". 
                   88 old-bli-disattivo VALUE IS "D". 
                   88 old-bli-bloccato VALUE IS "B". 
               10 old-bli-codice-ean-1 PIC  9(13).
               10 old-bli-codice-ean-2 PIC  9(13).
               10 old-bli-codice-ean-3 PIC  9(13).
               10 old-bli-codice-ean-4 PIC  9(13).
               10 old-bli-codice-ean-5 PIC  9(13).
               10 old-bli-foto         PIC  X(128).
               10 old-bli-dati-comuni.
                   15 old-bli-data-creazione           PIC  9(8).
                   15 old-bli-ora-creazione            PIC  9(8).
                   15 old-bli-utente-creazione         PIC  X(10).
                   15 old-bli-data-ultima-modifica     PIC  9(8).
                   15 old-bli-ora-ultima-modifica      PIC  9(8).
                   15 old-bli-utente-ultima-modifica   PIC  X(10).
               10 old-bli-vuoti.
                   15 old-bli-num-vuoto-1  PIC  9(18).
                   15 old-bli-num-vuoto-2  PIC  9(18).
                   15 old-bli-num-vuoto-3  PIC  9(18).
                   15 old-bli-alfa-vuoto   PIC  X(500).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-blister      pic X(2).
       77  status-blister-old  pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     blister
                     blister-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file blister?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "blister"
                          x"22"
                          " in "
                          x"22"
                          "blister-old"
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
           move zero   to cont

           open input blister-old.
           open output blister


           move low-value to old-bli-chiave.

           start blister-old key >= old-bli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read blister-old next at end exit perform end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close blister
                 blister-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           move old-bli-codice       to bli-codice     
           move old-bli-descrizione  to bli-descrizione
           move old-bli-magazzino    to bli-magazzino  
           move old-bli-prezzo       to bli-prezzo     

           perform varying idx from 1 by 1 
                     until idx > 50
              move old-bli-el-articolo(idx) to bli-el-articolo(idx)
              move old-bli-el-qta(idx)      to bli-el-qta(idx)     
              move old-bli-el-perce(idx)    to bli-el-perce(idx)   
           end-perform.

           move old-bli-stato        to bli-stato       
           move old-bli-codice-ean-1 to bli-codice-ean-1
           move old-bli-codice-ean-2 to bli-codice-ean-2
           move old-bli-codice-ean-3 to bli-codice-ean-3
           move old-bli-codice-ean-4 to bli-codice-ean-4
           move old-bli-codice-ean-5 to bli-codice-ean-5
           move old-bli-foto         to bli-foto        
           move old-bli-dati-comuni  to bli-dati-comuni 
           move old-bli-vuoti        to bli-vuoti       

           write bli-rec.                      



