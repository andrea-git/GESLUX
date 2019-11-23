       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convage.
       REMARKS. conversione agenti per allargamento campo email
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "agenti.sl".
       SELECT agenti-old
           ASSIGN       TO  "agenti-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-agenti-old
           RECORD KEY   IS old-age-chiave
           ALTERNATE RECORD KEY IS old-age-ragsoc-1
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "agenti.fd".       
           
       FD  agenti-old.
       01 old-age-rec.
           05 old-age-chiave.
               10 old-age-codice       PIC  9(5).
           05 old-age-dati.
               10 old-age-dati-anagrafici.
                   15 old-age-ragsoc-1     PIC  X(40).
                   15 old-age-ragsoc-2     PIC  X(40).
                   15 old-age-indirizzo    PIC  X(40).
                   15 old-age-cap          PIC  X(5).
                   15 old-age-localita     PIC  X(35).
                   15 old-age-provincia    PIC  X(2).
                   15 old-age-nazione      PIC  X(3).
                   15 old-age-codice-fiscale           PIC  X(16).
                   15 old-age-partita-iva  PIC  X(11).
                   15 old-age-telefono-1   PIC  X(15).
                   15 old-age-fax          PIC  X(15).
                   15 old-age-email        PIC  X(45).
                   15 old-age-iscrizione-cciaa         PIC  X(15).
                   15 old-age-numero-enasarco          PIC  X(10).
                   15 old-age-iscrizione-albo          PIC  X(10).
                   15 old-age-mono-plurimandatario     PIC  X(1).
                       88 old-age-mono VALUE IS "M". 
                       88 old-age-pluri VALUE IS "P". 
               10 old-age-dati-condizioni.
                   15 old-age-tipo         PIC  x.
                       88 old-age-normale VALUE IS "N". 
                       88 old-age-speciale VALUE IS "S". 
                   15 old-age-marg         PIC  9(3)v99.
                   15 old-age-minimo       PIC  9(9).
                   15 old-age-prezzo       PIC  9.
                       88 old-age-prezzo-normale VALUE IS 0. 
                       88 old-age-prezzo-speciale VALUE IS 1. 
                   15 old-age-listino      PIC  9(4).
                   15 old-age-tab-marche.
                       20 old-age-marche
                                  OCCURS 10 TIMES.
                           25 old-age-marca        PIC  9(4).
                           25 old-age-perce-marca  PIC  9(3)v99.
                   15 old-age-omaggi       PIC  9(3).
               10 old-age-dati-comuni.
                   15 old-age-data-creazione           PIC  9(8).
                   15 old-age-ora-creazione            PIC  9(8).
                   15 old-age-utente-creazione         PIC  X(10).
                   15 old-age-data-ultima-modifica     PIC  9(8).
                   15 old-age-ora-ultima-modifica      PIC  9(8).
                   15 old-age-utente-ultima-modifica   PIC  X(10).
               10 old-age-vuoti.
                   15 old-age-add-pb       PIC  9.
                       88 old-age-si-add-pb VALUE IS 1. 
                       88 old-age-no-add-pb VALUE IS 0. 
                   15 old-age-num-vuoto-1  PIC  9(14).
                   15 old-age-num-vuoto-2  PIC  9(15).
                   15 old-age-num-vuoto-3  PIC  9(15).
                   15 old-age-telefono-2   PIC  X(15).
                   15 old-age-alfa-vuoto-1 PIC  X(5).
                   15 old-age-alfa-vuoto-2 PIC  X(20).
                   15 old-age-alfa-vuoto-3 PIC  X(20).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-agenti        pic X(2).
       77  status-agenti-old    pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     agenti
                     agenti-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file agenti?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "agenti"
                          x"22"
                          " in "
                          x"22"
                          "agenti-old"
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

           open input agenti-old.
           open output agenti


           move low-value to old-age-chiave.

           start agenti-old key >= old-age-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read agenti-old next at end exit perform end-read
                    perform MUOVI-RECORD
                 end-perform
           end-start.

           close agenti
                 agenti-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.  
           initialize age-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move old-age-codice               to age-codice              
           move old-age-ragsoc-1             to age-ragsoc-1            
           move old-age-ragsoc-2             to age-ragsoc-2            
           move old-age-indirizzo            to age-indirizzo           
           move old-age-cap                  to age-cap                 
           move old-age-localita             to age-localita            
           move old-age-provincia            to age-provincia           
           move old-age-nazione              to age-nazione             
           move old-age-codice-fiscale       to age-codice-fiscale      
           move old-age-partita-iva          to age-partita-iva         
           move old-age-telefono-1           to age-telefono-1          
           move old-age-fax                  to age-fax                 
           move old-age-email                to age-email               
           move old-age-iscrizione-cciaa     to age-iscrizione-cciaa    
           move old-age-numero-enasarco      to age-numero-enasarco     
           move old-age-iscrizione-albo      to age-iscrizione-albo     
           move old-age-mono-plurimandatario to age-mono-plurimandatario
           move old-age-dati-condizioni      to age-dati-condizioni     
           move old-age-dati-comuni          to age-dati-comuni         
           move old-age-vuoti                to age-vuoti               
               
           write age-rec.



