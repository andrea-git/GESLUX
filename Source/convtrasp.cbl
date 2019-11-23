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
       01  old-trs-rec.
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
               10 old-trs-qta-kg       PIC  9(9)v999.
               10 old-trs-qta-arrot    PIC  9(9)v999.
               10 old-trs-tariffa      PIC  9(9)v99.
               10 old-trs-dati-comuni.
                   15 old-trs-data-creazione           PIC  9(8).
                   15 old-trs-ora-creazione            PIC  9(8).
                   15 old-trs-utente-creazione         PIC  X(10).
                   15 old-trs-data-ultima-modifica     PIC  9(8).
                   15 old-trs-ora-ultima-modifica      PIC  9(8).
                   15 old-trs-utente-ultima-modifica   PIC  X(10).
               10 trs-vuoti.
                   15 old-trs-num-vuoto-1  PIC  9(13).
                   15 old-trs-num-vuoto-2  PIC  9(15).
                   15 old-trs-num-vuoto-3  PIC  9(15).
                   15 old-trs-alfa-vuoto1  PIC  X(20).
                   15 old-trs-alfa-vuoto-2 PIC  X(20).
                   15 old-trs-alfa-vuoto-3 PIC  X(20).

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
           move old-trs-qta-kg        to trs-qta-kg      
           move old-trs-qta-arrot     to trs-qta-arrot   
           move old-trs-tariffa       to trs-tariffa     
           move old-trs-dati-comuni   to trs-dati-comuni 
                                      
           initialize trs-note. 
                                      
           write trs-rec.                      



