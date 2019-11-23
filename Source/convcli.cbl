       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convblister.
       REMARKS. Aumento dimensione campo fido.
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
           RECORD KEY   IS old-cli-chiave OF clienti-old
           ALTERNATE RECORD KEY IS old-cli-K1 of clienti-old = 
           old-cli-tipo-CF  OF clienti-old, 
           old-cli-ragsoc-1 OF clienti-old, 
           old-cli-codice   OF clienti-old
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-cli-K3 of clienti-old = 
           old-cli-gdo      OF clienti-old, 
           old-cli-chiave   OF clienti-old
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-cli-K4 of clienti-old = 
           old-cli-utf OF clienti-old, old-cli-chiave OF clienti-old
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-cli-ragsoc-1 OF clienti-old
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
               10 old-cli-note         PIC  x(300).
               10 old-cli-note-agg     PIC  x(256).
               10 old-cli-vettore      PIC  9(5).
               10 old-cli-inoltro      PIC  x.
               10 old-cli-agente       PIC  9(5).
               10 old-cli-iva-ese      PIC  x(3).
               10 old-cli-pag          PIC  x(3).
               10 old-cli-spost-ric-agosto         PIC  x.
               10 old-cli-spost-ric-dicembre       PIC  x.
               10 old-cli-fido         PIC  9(8)v9(2).
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
                   15 old-cli-alfa-vuoto-1 PIC  X(498).
                   15 old-cli-fidejussione PIC  s9(8)v9(4).
                   15 old-cli-pfa          PIC  s9(8)v9(4).
                   15 old-cli-pfa-perce    PIC  9(3)v999.
                   15 FILLER           PIC  9(4).
                   15 old-cli-num-vuoto-2  PIC  9(2).
                   15 old-cli-num-vuoto-3  PIC  9(18).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-clienti      pic X(2).
       77  status-clienti-old  pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     clienti
                     clienti-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file clienti?"
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

           open input  clienti-old.
           open output clienti


           move low-value to old-cli-chiave.

           start clienti-old key >= old-cli-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read clienti-old next at end exit perform end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close clienti
                 clienti-old.

           move cont to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           move old-cli-chiave             to cli-chiave.
           move old-cli-ragsoc-1           to cli-ragsoc-1          
           move old-cli-ragsoc-2           to cli-ragsoc-2          
           move old-cli-indirizzo          to cli-indirizzo         
           move old-cli-cap                to cli-cap               
           move old-cli-localita           to cli-localita          
           move old-cli-prov               to cli-prov              
           move old-cli-nazione            to cli-nazione           
           move old-cli-codfis             to cli-codfis            
           move old-cli-piva               to cli-piva              
           move old-cli-tel-1              to cli-tel-1             
           move old-cli-tel-2              to cli-tel-2             
           move old-cli-fax                to cli-fax               
           move old-cli-email              to cli-email             
           move old-cli-web                to cli-web               
           move old-cli-tipo               to cli-tipo              
           move old-cli-gdo                to cli-gdo               
           move old-cli-utf                to cli-utf               
           move old-cli-referente          to cli-referente         
           move old-cli-note               to cli-note              
           move old-cli-note-agg           to cli-note-agg          
           move old-cli-vettore            to cli-vettore           
           move old-cli-inoltro            to cli-inoltro           
           move old-cli-agente             to cli-agente            
           move old-cli-iva-ese            to cli-iva-ese
           move old-cli-pag                to cli-pag               
           move old-cli-spost-ric-agosto   to cli-spost-ric-agosto         
           move old-cli-spost-ric-dicembre to cli-spost-ric-dicembre      
           move old-cli-fido               to cli-fido              
           move old-cli-fido-data          to cli-fido-data         
           move old-cli-abi                to cli-abi               
           move old-cli-cab                to cli-cab               
           move old-cli-superamento-500    to cli-superamento-500   
           move old-cli-stato              to cli-stato             
           move old-cli-dich-esp           to cli-dich-esp          
           move old-cli-data-dich          to cli-data-dich         
           move old-cli-data-reg           to cli-data-reg          
           move old-cli-num-reg            to cli-num-reg           
           move old-cli-fido-extra         to cli-fido-extra        
           move old-cli-tipo-persona       to cli-tipo-persona      
           move old-cli-referente-ord      to cli-referente-ord     
           move old-cli-tel-dir-ref-ord    to cli-tel-dir-ref-ord   
           move old-cli-mail-ref-ord       to cli-mail-ref-ord      
           move old-cli-blocco-24000       to cli-blocco-24000      
           move old-cli-cod-ditta          to cli-cod-ditta         
           move old-cli-gg-dilazione       to cli-gg-dilazione      
           move old-cli-gestione-fido      to cli-gestione-fido     
           move old-cli-saldi-banco        to cli-saldi-banco       
           move old-cli-cau-blocco         to cli-cau-blocco        
           move old-cli-dati-comuni        to cli-dati-comuni       
           move old-cli-vuoti              to cli-vuoti             

           write cli-rec.
