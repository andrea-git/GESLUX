       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convcli.
       REMARKS. Aumento dimensione email.
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".

       SELECT clienti-old
           ASSIGN       TO  "clienti-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-clienti-old
           RECORD KEY   IS old-cli-chiave 
           ALTERNATE RECORD KEY IS old-cli-K1 = old-cli-tipo-CF,
           old-cli-ragsoc-1, old-cli-codice
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-cli-K3 = old-cli-gdo, 
           old-cli-chiave 
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-cli-K4 = old-cli-utf, 
           old-cli-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-cli-ragsoc-1 
           WITH DUPLICATES . 

           copy "destini.sl".

       SELECT destini-old
           ASSIGN       TO  "destini-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-destini-old
           RECORD KEY   IS old-des-chiave
           ALTERNATE RECORD KEY IS old-K1 = old-des-ragsoc-1, 
           old-des-codice, old-des-prog
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-localita = old-des-localita
           WITH DUPLICATES .   

           copy "destinif.sl".

       SELECT destinif-old
           ASSIGN       TO  "destinif-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-destinif-old
           RECORD KEY   IS old-desf-chiave
           ALTERNATE RECORD KEY IS old-K1 = old-desf-ragsoc-1, 
           old-desf-codice, old-desf-prog
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-desf-k2 = old-desf-codice, 
           old-desf-ragsoc-1, old-desf-prog
           WITH DUPLICATES .           

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".

       FD  clienti-old.
       01 old-cli-rec.
           05 old-cli-chiave.
               10 old-cli-tipo-CF      PIC  X(1).
                   88 old-cli-tipo-C VALUE IS "C". 
                   88 old-cli-tipo-F VALUE IS "F". 
               10 old-cli-codice       PIC  9(5).
           05 old-cli-dati.
               10 old-cli-ragsoc-1     PIC  x(40).
               10 old-cli-ragsoc-2     PIC  x(40).
               10 old-cli-indirizzo    PIC  x(40).
               10 old-cli-cap          PIC  x(5).
               10 old-cli-localita     PIC  x(35).
               10 old-cli-prov         PIC  x(2).
               10 old-cli-nazione      PIC  x(3).
               10 old-cli-codfis       PIC  x(16).
               10 old-cli-piva         PIC  x(11).
               10 old-cli-tel-1        PIC  x(15).
               10 old-cli-tel-2        PIC  x(15).
               10 old-cli-fax          PIC  x(15).
               10 old-cli-email        PIC  x(100).
               10 old-cli-web          PIC  x(100).
               10 old-cli-tipo         PIC  x(2).
               10 old-cli-gdo          PIC  x(5).
               10 old-cli-utf          PIC  x.
               10 old-cli-referente    PIC  x(30).
               10 old-cli-note         PIC  x(2000).
               10 old-cli-note-agg     PIC  x(256).
               10 old-cli-vettore      PIC  9(5).
               10 old-cli-inoltro      PIC  x.
               10 old-cli-agente       PIC  9(5).
               10 old-cli-iva-ese      PIC  x(3).
               10 old-cli-pag          PIC  x(3).
               10 old-cli-spost-ric-agosto         PIC  x.
               10 old-cli-spost-ric-dicembre       PIC  x.
               10 old-cli-fido         PIC  9(11)v9(2).
               10 old-cli-fido-data    PIC  9(8).
               10 old-cli-abi          PIC  x(5).
               10 old-cli-cab          PIC  x(5).
               10 old-cli-superamento-500          PIC  x.
               10 old-cli-stato        PIC  x.
                   88 old-cli-attivo VALUE IS "A". 
                   88 old-cli-disattivo VALUE IS "D". 
                   88 old-cli-bloccato VALUE IS "B". 
               10 old-cli-dich-esp     PIC  x(8).
               10 old-cli-data-dich    PIC  9(8).
               10 old-cli-data-reg     PIC  9(8).
               10 old-cli-num-reg      PIC  x(8).
               10 old-cli-fido-extra   PIC  9(8)v99.
               10 old-cli-tipo-persona PIC  x.
                   88 old-cli-fisica VALUE IS "F". 
                   88 old-cli-giuridica VALUE IS space. 
               10 old-cli-referente-ord            PIC  x(30).
               10 old-cli-tel-dir-ref-ord          PIC  x(20).
               10 old-cli-mail-ref-ord PIC  x(100).
      *(( XFD NAME = old-cli-blocco-24000_1 ))
               10 old-cli-blocco-24000 PIC  9.
                   88 old-cli-si-blocco VALUE IS 1. 
                   88 old-cli-no-blocco VALUE IS 0. 
               10 old-cli-cod-ditta    PIC  x(15).
               10 old-cli-gg-dilazione PIC  9(3).
               10 old-cli-gestione-fido            PIC  9.
                   88 old-cli-gestione-fido-si VALUE IS 1. 
                   88 old-cli-gestione-fido-no VALUE IS 0. 
               10 old-cli-saldi-banco  PIC  9(1).
                   88 old-cli-saldi-banco-si VALUE IS 1. 
                   88 old-cli-saldi-banco-no VALUE IS 0. 
               10 old-cli-cau-blocco   PIC  x.
                   88 old-cli-no-angraf VALUE IS "N". 
                   88 old-cli-prob-pag VALUE IS "P". 
                   88 old-cli-nuovo-ragsoc VALUE IS "R". 
                   88 old-cli-fuori-fido VALUE IS "F". 
                   88 old-cli-blocco-amm VALUE IS "A". 
               10 old-cli-dati-comuni.
                   15 old-cli-data-creazione           PIC  9(8).
                   15 old-cli-ora-creazione            PIC  9(8).
                   15 old-cli-utente-creazione         PIC  X(10).
                   15 old-cli-data-ultima-modifica     PIC  9(8).
                   15 old-cli-ora-ultima-modifica      PIC  9(8).
                   15 old-cli-utente-ultima-modifica   PIC  X(10).
               10 old-cli-vuoti.
                   15 old-cli-sost         PIC  x.
                       88 old-cli-sost-auto VALUE IS "A". 
                       88 old-cli-sost-richiesta VALUE IS "R". 
                       88 old-cli-sost-no VALUE IS " " "N". 
                   15 old-cli-tipo-art     PIC  9.
                       88 old-cli-tipo-art-diretti VALUE IS 1. 
                       88 old-cli-tipo-art-gruppi VALUE IS 2. 
                       88 old-cli-tipo-art-specialist VALUE IS 3. 
                       88 old-cli-tipo-art-DO VALUE IS 4. 
                       88 old-cli-tipo-art-GDA VALUE IS 5. 
                       88 old-cli-tipo-art-GDS VALUE IS 6. 
                       88 old-cli-tipo-art-ESTERO VALUE IS 7. 
                   15 old-cli-iva          PIC  X(3).
                   15 old-cli-invio-bolle-EDI          PIC  x.
                       88 old-cli-invio-bolle-EDI-si VALUE IS "S". 
                       88 old-cli-invio-bolle-EDI-no VALUE IS "N" " ". 
                   15 old-cli-destino-auto-EDI         PIC  x.
                       88 old-cli-destino-auto-EDI-si VALUE IS "S". 
                       88 old-cli-destino-auto-EDI-no VALUE IS "N" " ". 
      *(( XFD NAME = old-cli-agente-2 ))
                   15 old-cli-agente2      PIC  9(5).
                   15 old-cli-codice-SDI   PIC  X(10).
                   15 old-cli-pec          PIC  X(100).
                   15 old-cli-esigibilita-iva          PIC  X.
                       88 old-cli-esigibilita-iva-immediata VALUE "I". 
                       88 old-cli-esigibilita-iva-differita VALUE "D". 
                       88 old-cli-esigibilita-iva-scissione VALUE "S". 
      *(( XFD NAME = old-cli-alfa-vuoto-1_8 ))
                   15 old-cli-contrassegno PIC  x.
                       88 old-cli-contrassegno-si VALUE IS "S". 
                       88 old-cli-contrassegno-no VALUE IS "N", " ". 
      *(( XFD NAME = old-cli-alfa-vuoto-1_8 ))
                   15 FILLER           PIC  X(376).
                   15 old-cli-fidejussione PIC  s9(8)v9(4).
                   15 old-cli-pfa          PIC  s9(8)v9(4).
                   15 old-cli-pfa-perce    PIC  9(3)v999.
                   15 old-cli-data-fido-extra          PIC  9(8).
                   15 old-cli-grade        PIC  9(2).
                   15 old-cli-escludi-fido PIC  9.
                       88 old-cli-escludi-fido-si VALUE IS 1. 
                       88 old-cli-escludi-fido-no VALUE IS 0. 
                   15 old-cli-num-vuoto-3  PIC  9(13).

       copy "destini.fd".
       FD  destini-old.
       01 old-des-rec.
           05 old-des-chiave.
               10 old-des-codice       PIC  9(5).
               10 old-des-prog         PIC  9(5).
           05 old-des-dati.
               10 old-des-ragsoc-1     PIC  x(100).
               10 old-des-ragsoc-2     PIC  x(100).
               10 old-des-indirizzo    PIC  x(40).
               10 old-des-cap          PIC  x(5).
               10 old-des-localita     PIC  x(35).
               10 old-des-prov         PIC  x(2).
               10 old-des-nazione      PIC  x(3).
               10 old-des-telef-1      PIC  x(15).
               10 old-des-telef-2      PIC  x(15).
               10 old-des-fax          PIC  X(15).
               10 old-des-mail         PIC  X(200).
               10 old-des-referente    PIC  x(30).
               10 old-des-vettore      PIC  9(5).
               10 old-des-deposito-UTF PIC  x.
               10 old-des-superamento-500          PIC  x.
               10 old-des-stato        PIC  x.
               10 old-des-dati-comuni.
                   15 old-des-data-creazione           PIC  9(8).
                   15 old-des-ora-creazione            PIC  9(8).
                   15 old-des-utente-creazione         PIC  X(10).
                   15 old-des-data-ultima-modifica     PIC  9(8).
                   15 old-des-ora-ultima-modifica      PIC  9(8).
                   15 old-des-utente-ultima-modifica   PIC  X(10).
               10 old-des-vuoti.
                   15 old-des-piva         PIC  9(11).
                   15 old-des-piva-dupl    PIC  9(1).
                   15 old-des-cod-ditta    PIC  x(15).
                   15 old-des-tipo-art     PIC  9.   
                   15 old-des-num-vuoto-2  PIC  9(1).
                   15 old-des-saldi-banco  PIC  9(1).
                   15 old-des-saldi-promo  PIC  9(1).
                   15 old-des-escludi-evadi-tutto      PIC  9(1).
                   15 old-des-accorpa-master           PIC  9(1).
                   15 old-des-num-vuoto-3  PIC  9(12).
                   15 old-des-invio-fatt   PIC  x.               
                   15 old-des-note-bolla-1 PIC  X(500).
                   15 old-des-note-bolla-2 PIC  X(500).
                   15 old-des-CIG          PIC  X(15). 
                   15 FILLER           PIC  X(1985).

       copy "destinif.fd".

       FD  destinif-old.
       01 old-desf-rec.
           05 old-desf-chiave.
               10 old-desf-codice      PIC  9(5).
               10 old-desf-prog        PIC  9(5).
           05 old-desf-dati.
               10 old-desf-ragsoc-1    PIC  x(40).
               10 old-desf-ragsoc-2    PIC  x(40).
               10 old-desf-indirizzo   PIC  x(40).
               10 old-desf-cap         PIC  x(5).
               10 old-desf-localita    PIC  x(35).
               10 old-desf-prov        PIC  x(2).
               10 old-desf-nazione     PIC  x(3).
               10 old-desf-telef-1     PIC  x(15).
               10 old-desf-telef-2     PIC  x(15).
               10 old-desf-fax         PIC  X(15).
               10 old-desf-mail        PIC  X(100).
               10 old-desf-referente   PIC  x(30).
               10 old-desf-vettore     PIC  9(5).
               10 old-desf-depostio-UTF            PIC  x.
               10 old-desf-superamento-500         PIC  x.
               10 old-desf-stato       PIC  x.
                   88 old-desf-attivo VALUE IS space. 
                   88 old-desf-disattivo VALUE IS "D". 
                   88 old-desf-bloccato VALUE IS "B". 
               10 old-desf-dati-ord.
                   15 old-desf-referente-ord           PIC  x(30).
                   15 old-desf-tel-dir-ref-ord         PIC  x(20).
                   15 old-desf-mail-ref-ord            PIC  x(100).
                   15 old-desf-mail-ref-ord-cc         PIC  x(199).
                   15 old-desf-premio-netto            PIC  x.
                       88 old-desf-premio-netto-no VALUE IS " " "N". 
                       88 old-desf-premio-netto-si VALUE IS "S". 
                   15 old-desf-perce-premi-fine-anno   PIC  9(3)v99.
               10 old-desf-gg-consegna PIC  9(3).
               10 old-desf-dati-comuni.
                   15 old-desf-data-creazione          PIC  9(8).
                   15 old-desf-ora-creazione           PIC  9(8).
                   15 old-desf-utente-creazione        PIC  X(10).
                   15 old-desf-data-ultima-modifica    PIC  9(8).
                   15 old-desf-ora-ultima-modifica     PIC  9(8).
                   15 old-desf-utente-ultima-modifica  PIC  X(10).
               10 old-desf-vuoti.
                   15 old-desf-saldi       PIC  9.
                       88 old-desf-saldi-si VALUE IS 1. 
                       88 old-desf-saldi-no VALUE IS 0. 
                   15 old-desf-num-vuoto-1 PIC  9(17).
                   15 old-desf-num-vuoto-2 PIC  9(18).
                   15 old-desf-num-vuoto-3 PIC  9(18).
                   15 old-desf-ufficio     PIC  X.
                       88 old-desf-ufficio-non-gestito VALUE IS " ". 
                       88 old-desf-ufficio-luca VALUE IS "L". 
                       88 old-desf-ufficio-massimo VALUE IS "M". 
                   15 old-desf-pag         PIC  x(3).
                   15 old-desf-ev-immediata            PIC  x.
                       88 old-desf-ev-immediata-no VALUE IS " ", "N". 
                       88 old-desf-ev-immediata-si VALUE IS "S". 
                   15 old-desf-invio-sol   PIC  x.
                       88 old-desf-invio-sol-no VALUE IS " ", "N". 
                       88 old-desf-invio-sol-si VALUE IS "S". 
                   15 old-desf-alfa-vuoto  PIC  X(494).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-clienti      pic X(2).
       77  status-clienti-old  pic X(2).
       77  status-destini      pic X(2).
       77  status-destini-old  pic X(2).
       77  status-destinif     pic X(2).
       77  status-destinif-old pic X(2).

       77  cont-c               PIC 9(6).
       77  cont-d               PIC 9(6).
       77  cont-f               PIC 9(6).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     clienti
                     clienti-old
                     destini
                     destini-old
                     destinif
                     destinif-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message
           "Confermi la conversione dei files clienti/destini/destinif?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "clienti"
                          x"22"
                          " in "
                          x"22"
                          "clienti-old"
                          x"22"
                          "."
                          x"22"
                          "destini"
                          x"22"
                          " in "
                          x"22"
                          "destini-old"
                          x"22"    
                          "destinif"
                          x"22"
                          " in "
                          x"22"
                          "destinif-old"
                          type mb-yes-no
                          default mb-no
                          giving scelta
                          icon 2
           if scelta = mb-yes
              perform CONVERSIONE-CLIENTI
              perform CONVERSIONE-DESTINI
              perform CONVERSIONE-DESTINIF
           end-if.           

           display message "Convertiti " cont-c " clienti."
                    x"0d0a""Convertiti " cont-d " destini."
                    x"0d0a""Convertiti " cont-f " destinif.".

           goback.    


      ***---
       CONVERSIONE-CLIENTI.
           move 0 to cont-c

           open input  clienti-old.
           open output clienti


           move low-value to old-cli-chiave.

           start clienti-old key >= old-cli-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read clienti-old next at end exit perform end-read
                    perform MUOVI-RECORD-CLIENTI
                 end-perform
           end-start.

           close clienti
                 clienti-old.

      ***---
       MUOVI-RECORD-CLIENTI.
           add 1 to cont-c.
           move old-cli-chiave    to cli-chiave    
           move old-cli-ragsoc-1  to cli-ragsoc-1    
           move old-cli-ragsoc-2  to cli-ragsoc-2    
           move old-cli-indirizzo to cli-indirizzo   
           move old-cli-cap       to cli-cap         
           move old-cli-localita  to cli-localita    
           move old-cli-prov      to cli-prov        
           move old-cli-nazione   to cli-nazione     
           move old-cli-codfis    to cli-codfis      
           move old-cli-piva      to cli-piva        
           move old-cli-tel-1     to cli-tel-1       
           move old-cli-tel-2     to cli-tel-2       
           move old-cli-fax       to cli-fax         
           move old-cli-email     to cli-email       
           move old-cli-web       to cli-web         
           move old-cli-tipo      to cli-tipo        
           move old-cli-gdo       to cli-gdo         
           move old-cli-utf       to cli-utf         
           move old-cli-referente to cli-referente   
           move old-cli-note      to cli-note        
           move old-cli-note-agg  to cli-note-agg    
           move old-cli-vettore   to cli-vettore     
           move old-cli-inoltro   to cli-inoltro     
           move old-cli-agente    to cli-agente      
           move old-cli-iva-ese   to cli-iva-ese     
           move old-cli-pag       to cli-pag         
           move old-cli-spost-ric-agosto  
             to cli-spost-ric-agosto  
           move old-cli-spost-ric-dicembre
             to cli-spost-ric-dicembre
           move old-cli-fido             to cli-fido            
           move old-cli-fido-data        to cli-fido-data       
           move old-cli-abi              to cli-abi             
           move old-cli-cab              to cli-cab             
           move old-cli-superamento-500  to cli-superamento-500 
           move old-cli-stato            to cli-stato           
           move old-cli-dich-esp         to cli-dich-esp        
           move old-cli-data-dich        to cli-data-dich       
           move old-cli-data-reg         to cli-data-reg        
           move old-cli-num-reg          to cli-num-reg         
           move old-cli-fido-extra       to cli-fido-extra      
           move old-cli-tipo-persona     to cli-tipo-persona    
           move old-cli-referente-ord    to cli-referente-ord   
           move old-cli-tel-dir-ref-ord  to cli-tel-dir-ref-ord 
           move old-cli-mail-ref-ord     to cli-mail-ref-ord    
           move old-cli-blocco-24000     to cli-blocco-24000    
           move old-cli-cod-ditta        to cli-cod-ditta       
           move old-cli-gg-dilazione     to cli-gg-dilazione    
           move old-cli-gestione-fido    to cli-gestione-fido   
           move old-cli-saldi-banco      to cli-saldi-banco     
           move old-cli-cau-blocco       to cli-cau-blocco      
           move old-cli-dati-comuni      to cli-dati-comuni     
           move old-cli-sost             to cli-sost            
           move old-cli-tipo-art         to cli-tipo-art        
           move old-cli-iva              to cli-iva             
           move old-cli-invio-bolle-EDI  to cli-invio-bolle-EDI 
           move old-cli-destino-auto-EDI to cli-destino-auto-EDI
           move old-cli-agente2          to cli-agente2         
           move old-cli-codice-SDI       to cli-codice-SDI      
           move old-cli-pec              to cli-pec             
           move old-cli-esigibilita-iva  to cli-esigibilita-iva 
           move old-cli-contrassegno     to cli-contrassegno    
           move old-cli-fidejussione     to cli-fidejussione    
           move old-cli-pfa              to cli-pfa             
           move old-cli-pfa-perce        to cli-pfa-perce       
           move old-cli-data-fido-extra  to cli-data-fido-extra 
           move old-cli-grade            to cli-grade           
           move old-cli-escludi-fido     to cli-escludi-fido    

           write cli-rec.
                          
      ***---
       CONVERSIONE-DESTINI.
           move 0 to cont-d

           open input  destini-old.
           open output destini


           move low-value to old-des-chiave.

           start destini-old key >= old-des-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read destini-old next at end exit perform end-read
                    perform MUOVI-RECORD-DESTINI
                 end-perform
           end-start.

           close destini
                 destini-old.

      ***---
       MUOVI-RECORD-DESTINI.
           add 1 to cont-d.
           move old-des-chiave          to des-chiave         
           move old-des-ragsoc-1        to des-ragsoc-1       
           move old-des-ragsoc-2        to des-ragsoc-2       
           move old-des-indirizzo       to des-indirizzo      
           move old-des-cap             to des-cap            
           move old-des-localita        to des-localita       
           move old-des-prov            to des-prov           
           move old-des-nazione         to des-nazione        
           move old-des-telef-1         to des-telef-1        
           move old-des-telef-2         to des-telef-2        
           move old-des-fax             to des-fax            
           move old-des-mail            to des-mail           
           move old-des-referente       to des-referente      
           move old-des-vettore         to des-vettore        
           move old-des-deposito-UTF    to des-deposito-UTF   
           move old-des-superamento-500 to des-superamento-500
           move old-des-stato           to des-stato          
           move old-des-dati-comuni     to des-dati-comuni    
           move old-des-vuoti           to des-vuoti          
           write des-rec.

      ***---
       CONVERSIONE-DESTINIF.
           move 0 to cont-f

           open input  destinif-old.
           open output destinif


           move low-value to old-desf-chiave.

           start destinif-old key >= old-desf-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read destinif-old next at end exit perform end-read
                    perform MUOVI-RECORD-DESTINIF
                 end-perform
           end-start.

           close destinif
                 destinif-old.

      ***---
       MUOVI-RECORD-DESTINIF.
           add 1 to cont-f.

           move old-desf-chiave                to desf-chiave               
           move old-desf-ragsoc-1              to desf-ragsoc-1             
           move old-desf-ragsoc-2              to desf-ragsoc-2             
           move old-desf-indirizzo             to desf-indirizzo            
           move old-desf-cap                   to desf-cap                  
           move old-desf-localita              to desf-localita             
           move old-desf-prov                  to desf-prov                 
           move old-desf-nazione               to desf-nazione              
           move old-desf-telef-1               to desf-telef-1              
           move old-desf-telef-2               to desf-telef-2              
           move old-desf-fax                   to desf-fax                  
           move old-desf-mail                  to desf-mail                 
           move old-desf-referente             to desf-referente            
           move old-desf-vettore               to desf-vettore              
           move old-desf-depostio-UTF          to desf-depostio-UTF         
           move old-desf-superamento-500       to desf-superamento-500      
           move old-desf-stato                 to desf-stato                
           move old-desf-dati-ord              to desf-dati-ord.            
           move old-desf-referente-ord         to desf-referente-ord        
           move old-desf-tel-dir-ref-ord       to desf-tel-dir-ref-ord      
           move old-desf-mail-ref-ord          to desf-mail-ref-ord         
           move old-desf-mail-ref-ord-cc       to desf-mail-ref-ord-cc      
           move old-desf-premio-netto          to desf-premio-netto         
           move old-desf-perce-premi-fine-anno 
             to desf-perce-premi-fine-anno
           move old-desf-gg-consegna           to desf-gg-consegna          
           move old-desf-dati-comuni           to desf-dati-comuni          
           move old-desf-vuoti                 to desf-vuoti                
           
           write desf-rec.
