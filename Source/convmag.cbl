       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convmag.
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmagaz.sl".
       SELECT tmagaz-old
           ASSIGN       TO  "tmagaz-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmagaz-old
           RECORD KEY   IS old-mag-chiave.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmagaz.fd".   

       FD  tmagaz-old.
       01 old-mag-rec.
           05 old-mag-chiave.
               10 old-mag-codice       PIC  x(3).
           05 old-mag-dati.
               10 old-mag-descrizione  PIC  x(50).
               10 old-mag-principale   PIC  X(1).
               10 old-mag-scr-aux      PIC  X(1).
               10 old-mag-utf          PIC  X(1). 
               10 old-mag-dati-comuni.
                   15 old-mag-data-creazione           PIC  9(8).
                   15 old-mag-ora-creazione            PIC  9(8).
                   15 old-mag-utente-creazione         PIC  x(10).
                   15 old-mag-data-ultima-modifica     PIC  9(8).
                   15 old-mag-ora-ultima-modifica      PIC  9(8).
                   15 old-mag-utente-ultima-modifica   PIC  x(10).
               10 old-mag-vuoti.
                   15 old-mag-perce-riordino           PIC  9(3)v999.
                   15 old-mag-causali.
                       20 old-mag-cau-c        PIC  x(4).
                       20 old-mag-cau-s        PIC  x(4).
                   15 old-mag-causali-bozza-nc.
                       20 old-mag-cau-reso     PIC  x(4).
                       20 old-mag-cau-diff     PIC  x(4).
                   15 old-mag-vettore      PIC  9(5).
                   15 old-mag-cau-carico-rot           PIC  x(4).
                   15 old-mag-cau-scarico-rot          PIC  x(4).
                   15 old-mag-num-vuoto-3  PIC  9(10).
                   15 old-mag-per-promo    PIC  x.
                   15 old-mag-causale-eva  PIC  x(4).
                   15 old-mag-causale-omag PIC  x(4).
                   15 old-mag-sostituzione PIC  x.
                   15 old-mag-blocco-24000 PIC  X(1).
                   15 old-mag-blister      PIC  x.   
                   15 old-mag-tab-scorte.
                       20 old-mag-sco-codice   PIC  xx
                                  OCCURS 20 TIMES.
                   15 old-mag-ritira-lbx   PIC  x.   
                   15 old-mag-priorita-mag PIC  99.
                   15 old-mag-da-inviare   PIC  x.   
                   15 old-mag-gen-auto     PIC  x.   
                   15 old-mag-scorta       PIC  x.   
                   15 old-mag-alfa-vuoto-3 PIC  x.

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tmagaz        pic X(2).
       77  status-tmagaz-old    pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tmagaz
                     tmagaz-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file tmagaz?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "tmagaz"
                          x"22"
                          " in "
                          x"22"
                          "tmagaz-old"
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

           open input  tmagaz-old.
           open output tmagaz


           move low-value to old-mag-chiave.

           start tmagaz-old key >= old-mag-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmagaz-old next 
                         at end exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tmagaz
                 tmagaz-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.                         
           move old-mag-codice           to mag-codice          
           move old-mag-descrizione      to mag-descrizione     
           move old-mag-principale       to mag-principale      
           move old-mag-scr-aux          to mag-scr-aux         
           move old-mag-utf              to mag-utf             
           move old-mag-dati-comuni      to mag-dati-comuni     
           move old-mag-perce-riordino   to mag-perce-riordino  
           move old-mag-causali          to mag-causali         
           move old-mag-causali-bozza-nc to mag-causali-bozza-nc
           move old-mag-vettore          to mag-vettore         
           move old-mag-cau-carico-rot   to mag-cau-carico-rot          
           move old-mag-cau-scarico-rot  to mag-cau-scarico-rot         
           move old-mag-num-vuoto-3      to mag-num-vuoto-3     
           move old-mag-per-promo        to mag-per-promo       
           move old-mag-causale-eva      to mag-causale-eva     
           move old-mag-causale-omag     to mag-causale-omag    
           move old-mag-sostituzione     to mag-sostituzione    
           move old-mag-blocco-24000     to mag-blocco-24000    
           move old-mag-blister          to mag-blister         
           move old-mag-tab-scorte       to mag-tab-scorte      
           move old-mag-ritira-lbx       to mag-ritira-lbx      
           move old-mag-priorita-mag     to mag-priorita-mag    
           move old-mag-da-inviare       to mag-da-inviare      
           move old-mag-gen-auto         to mag-gen-auto        
           move old-mag-scorta           to mag-scorta          
           move old-mag-alfa-vuoto-3     to mag-alfa-vuoto-3    
           
           write mag-rec.                      



