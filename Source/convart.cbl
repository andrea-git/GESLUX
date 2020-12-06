       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convart.
       REMARKS. conversione dalla versione 2.6 verso la 2.6.1 per 
                allungare i path dei file
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
       SELECT articoli-old
           ASSIGN       TO  "articoli-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-articoli-old
           RECORD KEY   IS old-art-chiave
           ALTERNATE RECORD KEY IS old-art-k1 = old-art-descrizione 
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-art-codice-ean-1
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-art-codice-ean-2
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-art-codice-ean-3
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-art-codice-ean-4
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-art-codice-ean-5
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-art-collegato 
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
       FD  articoli-old.
       01 old-art-rec.
           05 old-art-chiave.
               10 old-art-codice       PIC  9(6).
           05 old-art-dati.
               10 old-art-descrizione  PIC  X(50).
               10 old-art-settore-merceologico     PIC  9(4).
               10 old-art-marca-prodotto           PIC  9(4).
               10 old-art-classe-1     PIC  9(4).
               10 old-art-classe-2     PIC  9(3).
               10 old-art-classe-3     PIC  9(3).
               10 old-art-classe-4     PIC  9(3).
               10 old-art-unita-di-misura          PIC  X(2).
               10 old-art-cod-fornitore            PIC  9(5).
               10 old-art-gestione-utf PIC  X(1).
                   88 old-art-si-utf VALUE IS "S". 
                   88 old-art-misto VALUE IS "M". 
                   88 old-art-no-utf VALUE IS "N". 
               10 old-art-peso-utf     PIC  9(3)v9(3).
               10 old-art-peso-non-utf PIC  9(3)v9(3).
               10 old-art-peso-standard            PIC  9(4)v9(3).
               10 old-art-imballo-standard         PIC  X(3).
               10 old-art-udm-imballo  PIC  x(5).
               10 old-art-codice-iva   PIC  x(3).
               10 old-art-prezzo-vendita           PIC  9(9)v9(2).
               10 old-art-perce-sconto-agente      PIC  9(2)v9(2).
               10 old-art-prezzo-acquisto          PIC  9(9)v9(2).
               10 old-art-perce-sconto-acquisto    PIC  9(2)v9(2).
               10 old-art-cod-doganale PIC  9(8).
               10 old-art-soggetto-imposte         PIC  X(1).
                   88 old-art-si-imposte VALUE IS "S". 
                   88 old-art-no-imposte VALUE IS "N". 
               10 old-art-perce-imposte            PIC  9(3)v9(3).
               10 old-art-perce-cou    PIC  9(3)v9(3).
               10 old-art-soggetto-cobat           PIC  X(1).
                   88 old-art-si-cobat VALUE IS "S". 
                   88 old-art-no-cobat VALUE IS "N". 
               10 old-art-amperaggio   PIC  9(3).
               10 old-art-auto-moto-per-cobat      PIC  X(1).
                   88 old-art-auto-cobat VALUE IS "A". 
                   88 old-art-moto-cobat VALUE IS "M". 
               10 old-art-note         PIC  X(30).
               10 old-art-codice-ean-1 PIC  9(13).
               10 old-art-codice-ean-2 PIC  9(13).
               10 old-art-codice-ean-3 PIC  9(13).
               10 old-art-codice-ean-4 PIC  9(13).
               10 old-art-codice-ean-5 PIC  9(13).
               10 old-art-foto         PIC  X(128).
               10 old-art-note-agg     PIC  x(128).
               10 old-art-scheda-tecnica           PIC  X(90).
               10 old-art-tossicologica            PIC  x(76).
               10 old-art-descrizione-2            PIC  x(50).
               10 old-art-qta-epal     PIC  9(8).
               10 old-art-qta-std      PIC  9(8).
               10 old-art-misure.
                   15 old-art-altezza      PIC  9(6)v99.
                   15 old-art-larghezza    PIC  9(6)v99.
                   15 old-art-profondita   PIC  9(6)v99.
               10 old-art-stato        PIC  X(1).
                   88 old-art-attivo VALUE IS "A". 
                   88 old-art-disattivo VALUE IS "D". 
                   88 old-art-bloccato VALUE IS "B". 
               10 old-art-dati-comuni.
                   15 old-art-data-creazione           PIC  9(8).
                   15 old-art-ora-creazione            PIC  9(8).
                   15 old-art-utente-creazione         PIC  X(10).
                   15 old-art-data-ultima-modifica     PIC  9(8).
                   15 old-art-ora-ultima-modifica      PIC  9(8).
                   15 old-art-utente-ultima-modifica   PIC  X(10).
               10 old-art-diretti      PIC  9.
                   88 old-art-si-diretti VALUE IS 1. 
                   88 old-art-no-diretti VALUE IS 0. 
               10 old-art-gruppi       PIC  9.
                   88 old-art-si-gruppi VALUE IS 1. 
                   88 old-art-no-gruppi VALUE IS 0. 
               10 old-art-gda          PIC  9.
                   88 old-art-si-gda VALUE IS 1. 
                   88 old-art-no-gda VALUE IS 0. 
               10 old-art-agenti       PIC  9.
                   88 old-art-si-agenti VALUE IS 1. 
                   88 old-art-no-agenti VALUE IS 0. 
               10 old-art-specialist   PIC  9.
                   88 old-art-si-specialist VALUE IS 1. 
                   88 old-art-no-specialist VALUE IS 0. 
               10 old-art-estero       PIC  9.
                   88 old-art-si-estero VALUE IS 1. 
                   88 old-art-no-estero VALUE IS 0. 
               10 old-art-gds          PIC  9.
                   88 old-art-si-gds VALUE IS 1. 
                   88 old-art-no-gds VALUE IS 0. 
               10 old-art-scorta       PIC  s9(8).
               10 old-art-prezzo-banco PIC  9(6)v9(2).
               10 old-art-prz-min-vend PIC  9(5)v9(2).
               10 old-art-moq            PIC  9(8).
               10 old-art-peso-reale   PIC  9(3)v999.
               10 old-art-do           PIC  9.
                   88 old-art-si-do VALUE IS 1. 
                   88 old-art-no-do VALUE IS 0. 
               10 old-art-cod-art-frn  PIC  x(15).
               10 old-art-mag-std      PIC  x(3).
               10 old-art-collegato    PIC  9(6).
               10 old-art-cod-desf-forn            PIC  9(5).
               10 old-art-cod-prodener PIC  x(10).
               10 old-art-tipo-stoc    PIC  x.
                   88 old-art-confezionato VALUE IS "C" " ". 
                   88 old-art-sfuso VALUE IS "S". 
               10 old-art-logo-brand   PIC  X(128).
               10 old-art-vuoti.
                   15 old-art-conf-cartone PIC  9(6).
                   15 old-art-cartone-UDC  PIC  9(6).
                   15 old-art-misure-pz.
                       20 old-art-altezza-pz   PIC  9(6)v99.
                       20 old-art-larghezza-pz PIC  9(6)v99.
                       20 old-art-profondita-pz            PIC  9(6)v99.
                   15 old-art-web          PIC  9.
                       88 old-art-web-si VALUE IS 1. 
                       88 old-art-web-no VALUE IS 0. 
                   15 old-art-num-vuoto-3  PIC  9(17).
                   15 old-art-adr          PIC  x(15).
                   15 old-art-alfa-vuoto   PIC  X(485).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-articoli      pic X(2).
       77  status-articoli-old  pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     articoli
                     articoli-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file articoli?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "articoli"
                          x"22"
                          " in "
                          x"22"
                          "articoli-old"
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

           open input articoli-old.
           open output articoli


           move low-value to old-art-chiave.

           start articoli-old key not less old-art-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read articoli-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close articoli
                 articoli-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.

                                                                         



           move old-art-codice               to art-codice               
           move old-art-descrizione          to art-descrizione          
           move old-art-settore-merceologico to art-settore-merceologico   
           move old-art-marca-prodotto       to art-marca-prodotto          
           move old-art-classe-1             to art-classe-1             
           move old-art-classe-2             to art-classe-2             
           move old-art-classe-3             to art-classe-3             
           move old-art-classe-4             to art-classe-4             
           move old-art-unita-di-misura      to art-unita-di-misura         
           move old-art-cod-fornitore        to art-cod-fornitore           
           move old-art-gestione-utf         to art-gestione-utf         
           move old-art-peso-utf             to art-peso-utf             
           move old-art-peso-non-utf         to art-peso-non-utf         
           move old-art-peso-standard        to art-peso-standard
           move old-art-imballo-standard     to art-imballo-standard        
           move old-art-udm-imballo          to art-udm-imballo          
           move old-art-codice-iva           to art-codice-iva           
           move old-art-prezzo-vendita       to art-prezzo-vendita          
           move old-art-perce-sconto-agente  to art-perce-sconto-agente     
           move old-art-prezzo-acquisto      to art-prezzo-acquisto         
           move old-art-perce-sconto-acquisto 
                                         to art-perce-sconto-acquisto     
           move old-art-cod-doganale         to art-cod-doganale         
           move old-art-soggetto-imposte     to art-soggetto-imposte        
           move old-art-perce-imposte        to art-perce-imposte           
           move old-art-perce-cou            to art-perce-cou            
           move old-art-soggetto-cobat       to art-soggetto-cobat          
           move old-art-amperaggio           to art-amperaggio           
           move old-art-auto-moto-per-cobat  to art-auto-moto-per-cobat     
           move old-art-note                 to art-note                 
           move old-art-codice-ean-1         to art-codice-ean-1         
           move old-art-codice-ean-2         to art-codice-ean-2         
           move old-art-codice-ean-3         to art-codice-ean-3         
           move old-art-codice-ean-4         to art-codice-ean-4         
           move old-art-codice-ean-5         to art-codice-ean-5         
           move old-art-foto                 to art-foto                 
           move old-art-note-agg             to art-note-agg             
           move old-art-scheda-tecnica       to art-scheda-tecnica       
           move old-art-tossicologica        to art-tossicologica        
           move old-art-descrizione-2        to art-descrizione-2        
           move old-art-qta-epal             to art-qta-epal             
           move old-art-qta-std              to art-qta-std              
           move old-art-altezza              to art-altezza              
           move old-art-larghezza            to art-larghezza            
           move old-art-profondita           to art-profondita           
           move old-art-stato                to art-stato                
           move old-art-gruppi               to art-gruppi               
           move old-art-gda                  to art-gda                  
           move old-art-agenti               to art-agenti               
           move old-art-specialist           to art-specialist           
           move old-art-estero               to art-estero               
           move old-art-gds                  to art-gds                  
           move old-art-scorta               to art-scorta               
           move old-art-prezzo-banco         to art-prezzo-banco         
           move old-art-prz-min-vend         to art-prz-min-vend         
           move old-art-moq                  to art-moq
           move old-art-peso-reale           to art-peso-reale           
           move old-art-do                   to art-do                   
           move old-art-cod-art-frn          to art-cod-art-frn      
           move old-art-mag-std              to art-mag-std              
           move old-art-collegato            to art-collegato            
           move old-art-cod-desf-forn        to art-cod-desf-forn        
           move old-art-cod-prodener         to art-cod-prodener         
           move old-art-tipo-stoc            to art-tipo-stoc            
           move old-art-logo-brand           to art-logo-brand           
           move old-art-conf-cartone         to art-conf-cartone         
           move old-art-cartone-UDC          to art-cartone-UDC           
           move old-art-altezza-pz           to art-altezza-pz            
           move old-art-larghezza-pz         to art-larghezza-pz          
           move old-art-profondita-pz        to art-profondita-pz         
           move old-art-web                  to art-web                   
           move old-art-num-vuoto-3          to art-num-vuoto-3           
           move old-art-adr                  to art-adr                   
           move old-art-alfa-vuoto           to art-alfa-vuoto            
           move old-art-dati-comuni          to art-dati-comuni           





           write art-rec.                      



