       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convtcla1art.
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tcla1art.sl".
           
       SELECT tcla1art-old
           ASSIGN       TO  "tcla1art-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tcla1art
           RECORD KEY   IS old-cl1-chiave.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tcla1art.fd".              
           
       FD  tcla1art-old.
       01 old-cl1-rec.
           05 old-cl1-chiave.
               10 old-cl1-codice       PIC  9(4).
           05 old-cl1-dati.
               10 old-cl1-descrizione  PIC  x(30).
               10 old-cl1-dati-comuni.
                   15 old-cl1-data-creazione           PIC  9(8).
                   15 old-cl1-ora-creazione            PIC  9(8).
                   15 old-cl1-utente-creazione         PIC  x(10).
                   15 old-cl1-data-ultima-modifica     PIC  9(8).
                   15 old-cl1-ora-ultima-modifica      PIC  9(8).
                   15 old-cl1-utente-ultima-modifica   PIC  x(10).
               10 old-cl1-vuoti.
                   15 old-cl1-livello      PIC  99.
                   15 old-cl1-num-vuoto-1  PIC  9(13).
                   15 old-cl1-num-vuoto-2  PIC  9(15).
                   15 old-cl1-num-vuoto-3  PIC  9(15).
                   15 old-cl1-alfa-vuoto-1 PIC  x(20).
                   15 old-cl1-alfa-vuoto-2 PIC  x(20).
                   15 old-cl1-alfa-vuoto-3 PIC  x(20).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tcla1art      pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tcla1art
                     tcla1art-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file tcla1art?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "tcla1art"
                          x"22"
                          " in "
                          x"22"
                          "tcla1art-old"
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

           open input  tcla1art-old.
           open output tcla1art


           move low-value to old-cl1-chiave.

           start tcla1art-old key >= old-cl1-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tcla1art-old next at end exit perform end-read
                    perform MUOVI-RECORD
                 end-perform
           end-start.

           close tcla1art
                 tcla1art-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.  
           initialize cl1-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move old-cl1-codice      to cl1-codice.
           move old-cl1-descrizione to cl1-descrizione.
           move old-cl1-dati-comuni to cl1-dati-comuni.
           move old-cl1-livello     to cl1-livello.
           write cl1-rec.
