       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convrordf.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rordforn.sl".
       SELECT rordforn-old
           ASSIGN       TO  "rordforn-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-rordforn-old
           RECORD KEY   IS old-rof-chiave 
           ALTERNATE RECORD KEY IS old-rof-k-articolo = 
           old-rof-cod-articolo, old-rof-chiave
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "rordforn.fd".
           
       FD  rordforn-old.
       01 old-rof-rec.
           05 old-rof-chiave.
               10 old-rof-chiave-testa.
                   15 old-rof-anno         PIC  9(4).
                   15 old-rof-numero       PIC  9(8).
               10 old-rof-riga         PIC  9(5).
           05 old-rof-dati.
               10 old-rof-prg-chiave.
                   15 old-rof-cod-articolo PIC  9(6).
                   15 old-rof-cod-magazzino            PIC  X(3).
                   15 old-rof-tipo-imballo PIC  X(3).
                   15 old-rof-peso         PIC  9(3)V9(3).
               10 old-rof-imb-ordinato PIC  x(3).
               10 old-rof-qta-ord      PIC  9(8).
               10 old-rof-qta-evasa    PIC  9(8).
               10 old-rof-prz-unitario PIC  9(9)v9(2).
               10 old-rof-sconto-1     PIC  9(3)v9(2).
               10 old-rof-sconto-2     PIC  9(3)v9(2).
               10 old-rof-sconto-3     PIC  9(3)v9(2).
               10 old-rof-sconto-4     PIC  9(3)v9(2).
               10 old-rof-sconto-5     PIC  9(3)v9(2).
               10 old-rof-imponib-merce            PIC  9(9)v9(2).
               10 old-rof-imp-consumo  PIC  9(4)v9(2).
               10 old-rof-imp-cou-cobat            PIC  9(4)v9(2).
               10 old-rof-add-piombo   PIC  9(4)v9(2).
               10 old-rof-costi-aggiuntivi         PIC  9(9)v9(2).
               10 old-rof-cod-iva      PIC  x(3).
               10 old-rof-peso-utf     PIC  9(3)v9(3).
               10 old-rof-peso-non-utf PIC  9(3)v9(3).
               10 old-rof-cod-imballo  PIC  X(3).
               10 old-rof-qta-imballi  PIC  9(4).
               10 old-rof-promo        PIC  9(15).
               10 old-rof-cod-listino  PIC  9(15).
               10 old-rof-ddt.
                   15 old-rof-anno-ddt     PIC  9(4).
                   15 old-rof-numero-ddt   PIC  9(8).
               10 old-rof-dati-carico.
                   15 old-rof-data-carico  PIC  9(8).
                   15 old-rof-ora-carico   PIC  9(8).
                   15 old-rof-utente-carico            PIC  X(10).
               10 old-rof-dati-comuni.
                   15 old-rof-data-creazione           PIC  9(8).
                   15 old-rof-ora-creazione            PIC  9(8).
                   15 old-rof-utente-creazione         PIC  X(10).
                   15 old-rof-data-ultima-modifica     PIC  9(8).
                   15 old-rof-ora-ultima-modifica      PIC  9(8).
                   15 old-rof-utente-ultima-modifica   PIC  X(10).
               10 old-rof-vuoti.
                   15 old-rof-num-vuoto-1  PIC  9(18).
                   15 old-rof-num-vuoto-2  PIC  9(18).
                   15 old-rof-num-vuoto-3  PIC  9(18).
                   15 old-rof-alfa-vuoto-1 PIC  X(20).
                   15 old-rof-alfa-vuoto-2 PIC  X(20).
                   15 old-rof-alfa-vuoto-3 PIC  X(20).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-rordforn      pic X(2).
       77  status-rordforn-old  pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     rordforn
                     rordforn-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file listini?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "rordforn"
                          x"22"
                          " in "
                          x"22"
                          "rordforn-old"
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

           open input  rordforn-old.
           open output rordforn


           move low-value to old-rof-chiave.

           start rordforn-old key not less old-rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn-old next at end exit perform end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close rordforn
                 rordforn-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.

           move old-rof-anno             to rof-anno            
           move old-rof-numero           to rof-numero          
           move old-rof-riga             to rof-riga            
           move old-rof-prg-chiave       to rof-prg-chiave      
           move old-rof-imb-ordinato     to rof-imb-ordinato    
           move old-rof-qta-ord          to rof-qta-ord         
           move old-rof-qta-evasa        to rof-qta-evasa       
           move old-rof-prz-unitario     to rof-prz-unitario    
           move old-rof-sconto-1         to rof-sconto-1        
           move old-rof-sconto-2         to rof-sconto-2        
           move old-rof-sconto-3         to rof-sconto-3        
           move old-rof-sconto-4         to rof-sconto-4        
           move old-rof-sconto-5         to rof-sconto-5        
           move old-rof-imponib-merce    to rof-imponib-merce   
           move old-rof-imp-consumo      to rof-imp-consumo     
           move old-rof-imp-cou-cobat    to rof-imp-cou-cobat   
           move old-rof-add-piombo       to rof-add-piombo      
           move old-rof-costi-aggiuntivi to rof-costi-aggiuntivi
           move old-rof-cod-iva          to rof-cod-iva         
           move old-rof-peso-utf         to rof-peso-utf        
           move old-rof-peso-non-utf     to rof-peso-non-utf    
           move old-rof-cod-imballo      to rof-cod-imballo     
           move old-rof-qta-imballi      to rof-qta-imballi     
           move old-rof-promo            to rof-promo           
           move old-rof-cod-listino      to rof-cod-listino     
           move old-rof-ddt              to rof-ddt             
           move old-rof-dati-carico      to rof-dati-carico     
           move old-rof-dati-comuni      to rof-dati-comuni     
                                               
           write rof-rec.                      

