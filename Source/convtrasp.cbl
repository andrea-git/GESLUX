       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convtrasp.
       REMARKS. conversione dalla versione 2.3 verso la 2.4 per 
                aggiungere le note
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "trasporti.sl".
       SELECT trasporti-old
           ASSIGN       TO  "trasporti-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-trasporti-old
           RECORD KEY   IS old-trs-chiave
           ALTERNATE RECORD KEY IS old-k-data-bolla = old-trs-data-bolla, 
           old-trs-num-bolla, old-trs-prog-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-data-fattura = 
           old-trs-data-fattura, 
           old-trs-data-bolla, old-trs-num-bolla, old-trs-prog-bolla
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "trasporti.fd".
           
       FD  trasporti-old.
       01 old-trs-rec.
           05 old-trs-chiave.
               10 old-trs-anno         PIC  9(4).
               10 old-trs-num-bolla    PIC  9(8).
               10 old-trs-prog-bolla   PIC  9(10).
           05 old-trs-dati.
               10 old-trs-data-bolla   PIC  9(8).
               10 old-trs-data-fattura PIC  9(8).
               10 old-trs-num-fattura  PIC  9(8).
               10 old-trs-vettore      PIC  9(5).
               10 old-trs-cliente      PIC  9(5).
               10 old-trs-destino      PIC  9(5).
               10 old-trs-regione      PIC  9(3).
               10 old-trs-provincia    PIC  x(2).
               10 old-trs-qta-kg-s1    PIC  9(9)v999.
               10 old-trs-qta-arrot-s1 PIC  9(9)v999.
               10 old-trs-tariffa-s1   PIC  9(9)v99.
               10 old-trs-note         PIC  x(500).
               10 old-trs-dati-comuni.
                   15 old-trs-data-creazione           PIC  9(8).
                   15 old-trs-ora-creazione            PIC  9(8).
                   15 old-trs-utente-creazione         PIC  X(10).
                   15 old-trs-data-ultima-modifica     PIC  9(8).
                   15 old-trs-ora-ultima-modifica      PIC  9(8).
                   15 old-trs-utente-ultima-modifica   PIC  X(10).
               10 old-trs-vuoti.
                   15 old-trs-qta-kg-s2    PIC  9(9)v999.
                   15 old-trs-qta-arrot-s2 PIC  9(9)v999.
                   15 old-trs-tariffa-s2   PIC  9(9)v99.
                   15 old-trs-num-vuoto-2  PIC  9(1).
                   15 old-trs-num-vuoto-3  PIC  9(18).
                   15 old-trs-qta-kg-s3    PIC  9(9)v999.
                   15 old-trs-qta-arrot-s3 PIC  9(9)v999.
                   15 old-trs-tariffa-s3   PIC  9(9)v99.
               10 old-trs-tmo-chiave.
                   15 old-trs-tmo-anno     PIC  9(4).
                   15 old-trs-tmo-numero   PIC  9(8).
               10 old-trs-causale      PIC  x(4).
               10 old-trs-alfa-vuoto   PIC  X(449).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-trasporti      pic X(2).
       77  status-trasporti-old  pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     trasporti
                     trasporti-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file trasporti?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "trasporti"
                          x"22"
                          " in "
                          x"22"
                          "trasporti-old"
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

           open input trasporti-old.
           open output trasporti


           move low-value to old-trs-chiave.

           start trasporti-old key not less old-trs-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read trasporti-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close trasporti
                 trasporti-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.

           move old-trs-anno          to trs-anno        
           move old-trs-num-bolla     to trs-num-bolla   
           move old-trs-prog-bolla    to trs-prog-bolla  
           move old-trs-data-bolla    to trs-data-bolla  
           move old-trs-data-fattura  to trs-data-fattura 
           move old-trs-num-fattura   to trs-num-fattura 
           move old-trs-vettore       to trs-vettore     
           move old-trs-cliente       to trs-cliente     
           move old-trs-destino       to trs-destino     
           move old-trs-regione       to trs-regione     
           move old-trs-provincia     to trs-provincia   
           move old-trs-qta-kg-s1     to trs-qta-kg-s1
           move old-trs-qta-arrot-s1  to trs-qta-arrot-s1   
           move old-trs-tariffa-s1    to trs-tariffa-s1
           move old-trs-qta-kg-s2     to trs-qta-kg-s2
           move old-trs-qta-arrot-s2  to trs-qta-arrot-s2
           move old-trs-tariffa-s2    to trs-tariffa-s2
           move old-trs-qta-kg-s3     to trs-qta-kg-s3
           move old-trs-qta-arrot-s3  to trs-qta-arrot-s3
           move old-trs-tariffa-s3    to trs-tariffa-s3
           move old-trs-dati-comuni   to trs-dati-comuni 
           move old-trs-tmo-chiave    to trs-tmo-chiave.
           move old-trs-causale       to trs-causale.
                                      
           initialize trs-note. 
                                      
           write trs-rec.                      



