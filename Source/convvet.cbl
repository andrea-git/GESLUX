       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convvet.
       REMARKS. conversione dalla versione 2.4 verso la 2.5 
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tvettori.sl".
       SELECT tvettori-old
           ASSIGN       TO  "tvettori-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tvettori-old
           RECORD KEY   IS old-vet-chiave
           ALTERNATE RECORD KEY IS old-k-des = old-vet-descrizione
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tvettori.fd".
       FD tvettori-old.
       01 old-vet-rec.
           05 old-vet-chiave.
               10 old-vet-codice       PIC  9(5).
           05 old-vet-dati.
               10 old-vet-descrizione  PIC  X(40).
               10 old-vet-indirizzo    PIC  X(40).
               10 old-vet-sigla        PIC  X(3).
               10 old-vet-tariffa      PIC  x.
                   88 old-vet-regione VALUE IS "R". 
                   88 old-vet-prov VALUE IS "P". 
                   88 old-vet-cliente VALUE IS "C". 
                   88 old-vet-clides VALUE IS "D". 
               10 old-vet-min-tass     PIC  9(9)v99.
               10 old-vet-scaglione-peso.
                   15 old-vet-el-scaglione-peso
                              OCCURS 20 TIMES.
                       20 old-vet-qli-da-peso  PIC  9(9)v99.
                       20 old-vet-qli-a-peso   PIC  9(9)v99.
               10 old-vet-scaglione-arrot.
                   15 old-vet-el-scaglione-arrot
                              OCCURS 10 TIMES.
                       20 old-vet-qli-da-arrot PIC  9(9)v99.
                       20 old-vet-qli-a-arrot  PIC  9(9)v99.
                       20 old-vet-valore-arrot PIC  9(9)v99.
               10 old-vet-url          PIC  x(250).
               10 old-vet-piva         PIC  x(11).
               10 old-vet-n-albo       PIC  x(50).
               10 old-vet-dati-comuni.
                   15 old-vet-data-creazione           PIC  9(8).
                   15 old-vet-ora-creazione            PIC  9(8).
                   15 old-vet-utente-creazione         PIC  X(10).
                   15 old-vet-data-ultima-modifica     PIC  9(8).
                   15 old-vet-ora-ultima-modifica      PIC  9(8).
                   15 old-vet-utente-ultima-modifica   PIC  X(10).
               10 old-vet-vuoti.
                   15 old-vet-su-autorizz  PIC  9.
                       88 old-vet-su-autorizz-si VALUE IS 1. 
                       88 old-vet-su-autorizz-no VALUE IS 0. 
                   15 old-vet-pod-std      PIC  9.
                       88 old-vet-pod-std-si VALUE IS 1. 
                       88 old-vet-pod-std-no VALUE IS 0. 
                   15 old-vet-num-vuoto-1  PIC  9(16).
                   15 old-vet-num-vuoto-2  PIC  9(18).
                   15 old-vet-num-vuoto-3  PIC  9(18).
                   15 old-vet-pod-std-path PIC  x(200).
                   15 old-vet-regioni.
                       20 old-vet-abruzzo      PIC  9(3).
                       20 old-vet-basilicata   PIC  9(3).
                       20 old-vet-calabria     PIC  9(3).
                       20 old-vet-campania     PIC  9(3).
                       20 old-vet-emilia-romagna           PIC  9(3).
                       20 old-vet-friuli       PIC  9(3).
                       20 old-vet-lazio        PIC  9(3).
                       20 old-vet-liguria      PIC  9(3).
                       20 old-vet-lombardia    PIC  9(3).
                       20 old-vet-marche       PIC  9(3).
                       20 old-vet-molise       PIC  9(3).
                       20 old-vet-piemonte     PIC  9(3).
                       20 old-vet-puglia       PIC  9(3).
                       20 old-vet-sardegna     PIC  9(3).
                       20 old-vet-sicilia      PIC  9(3).
                       20 old-vet-toscana      PIC  9(3).
                       20 old-vet-trentino     PIC  9(3).
                       20 old-vet-umbria       PIC  9(3).
                       20 old-vet-valle        PIC  9(3).
                       20 old-vet-veneto       PIC  9(3).
                   15 old-vet-path-pod     PIC  x(100).
                   15 old-vet-mail-solleciti           PIC  x(100).
                   15 old-vet-sigla-pod    PIC  x(3).
                   15 FILLER           PIC  x(37).
       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tvettori      pic X(2).
       77  status-tvettori-old  pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tvettori
                     tvettori-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file tvettori?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "tvettori"
                          x"22"
                          " in "
                          x"22"
                          "tvettori-old"
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

           open input tvettori-old.
           open output tvettori


           move low-value to old-vet-chiave.

           start tvettori-old key >= old-vet-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tvettori-old next 
                         at end exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tvettori
                 tvettori-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.                         
           move old-vet-codice                 to vet-codice                
           move old-vet-descrizione            to vet-descrizione           
           move old-vet-indirizzo              to vet-indirizzo             
           move old-vet-sigla                  to vet-sigla                 
           move old-vet-tariffa                to vet-tariffa               
           move old-vet-min-tass               to vet-min-tass              
           move old-vet-scaglione-peso         to vet-scaglione-peso        
           move old-vet-scaglione-arrot        to vet-scaglione-arrot       
           move old-vet-url                    to vet-url       .           
           move old-vet-piva                   to vet-piva                  
           move old-vet-n-albo                 to vet-n-albo                
           move old-vet-data-creazione         to vet-data-creazione        
           move old-vet-ora-creazione          to vet-ora-creazione         
           move old-vet-utente-creazione       to vet-utente-creazione      
           move old-vet-data-ultima-modifica   
             to vet-data-ultima-modifica  
           move old-vet-ora-ultima-modifica    
             to vet-ora-ultima-modifica   
           move old-vet-utente-ultima-modifica 
             to vet-utente-ultima-modifica
           move old-vet-su-autorizz            to vet-su-autorizz           
           move old-vet-pod-std                to vet-pod-std               
           move old-vet-num-vuoto-1            to vet-num-vuoto-1           
           move old-vet-num-vuoto-2            to vet-num-vuoto-2           
           move old-vet-num-vuoto-3            to vet-num-vuoto-3           
           move old-vet-pod-std-path           to vet-pod-std-path          
           move old-vet-abruzzo                to vet-abruzzo               
           move old-vet-basilicata             to vet-basilicata            
           move old-vet-calabria               to vet-calabria              
           move old-vet-campania               to vet-campania              
           move old-vet-emilia-romagna         to vet-emilia-romagna        
           move old-vet-friuli                 to vet-friuli                
           move old-vet-lazio                  to vet-lazio                 
           move old-vet-liguria                to vet-liguria               
           move old-vet-lombardia              to vet-lombardia             
           move old-vet-marche                 to vet-marche                
           move old-vet-molise                 to vet-molise                
           move old-vet-piemonte               to vet-piemonte              
           move old-vet-puglia                 to vet-puglia                
           move old-vet-sardegna               to vet-sardegna              
           move old-vet-sicilia                to vet-sicilia               
           move old-vet-toscana                to vet-toscana               
           move old-vet-trentino               to vet-trentino              
           move old-vet-umbria                 to vet-umbria                
           move old-vet-valle                  to vet-valle                 
           move old-vet-veneto                 to vet-veneto                
           move old-vet-path-pod               to vet-path-pod              
           move old-vet-mail-solleciti         to vet-mail-solleciti        
           move old-vet-sigla-pod              to vet-sigla-pod             
           
           write vet-rec.                      



