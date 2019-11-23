       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-sto-mtordini.
       remarks. Per allineamento storico
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       copy "STO-mtordini.sl".

       SELECT STO-mtordini-old
           ASSIGN       TO "sto-mtordini-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-STO-mtordini-old
           RECORD KEY   IS OLD-STO-mto-chiave
           ALTERNATE RECORD KEY IS OLD-STO-mto-k-ord-cli = 
           OLD-STO-mto-anno, 
           OLD-STO-mto-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-mto-k-data = 
           OLD-STO-mto-data-ordine, OLD-STO-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-mto-k-clides = 
           OLD-STO-mto-cod-cli, 
           OLD-STO-mto-prg-destino, OLD-STO-mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-mto-k-age = 
           OLD-STO-mto-cod-agente, 
           OLD-STO-mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-OLD-STO-mto-stato-sel = 
           OLD-STO-mto-stato-ordine, OLD-STO-mto-cod-cli, 
           OLD-STO-mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-OLD-STO-mto-stato = 
           OLD-STO-mto-stato-ordine, OLD-STO-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-mto-k-gdo = OLD-STO-mto-gdo, 
           OLD-STO-mto-data-ordine, OLD-STO-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-STO-mto-k-bloc = 
           OLD-STO-mto-stato-attivazione, OLD-STO-mto-data-ordine, 
           OLD-STO-mto-cod-cli, OLD-STO-mto-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-giang = OLD-STO-mto-data-note1, 
           OLD-STO-mto-chiave
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "STO-mtordini.fd".
      *(( XFD FILE = OLD-STO-mtordini ))
       FD  STO-mtordini-old.
       01 OLD-STO-mto-rec.
           05 OLD-STO-mto-chiave.
               10 OLD-STO-mto-anno     PIC  9(4).
               10 OLD-STO-mto-numero   PIC  9(8).
           05 OLD-STO-mto-dati.
               10 OLD-STO-mto-causale  PIC  x(4).
               10 OLD-STO-mto-tipo-CF  PIC  x.
               10 OLD-STO-mto-cod-cli  PIC  9(5).
               10 OLD-STO-mto-prg-destino          PIC  9(5).
               10 OLD-STO-mto-gdo      PIC  x(5).
               10 OLD-STO-mto-num-ord-cli          PIC  X(10).
               10 OLD-STO-mto-data-ordine          PIC  9(8).
               10 OLD-STO-mto-data-passaggio-ordine           PIC  9(8).
               10 OLD-STO-mto-cod-agente           PIC  9(5).
               10 OLD-STO-mto-cod-pagamento        PIC  x(3).
               10 OLD-STO-mto-cod-ese-iva          PIC  x(3).
               10 OLD-STO-mto-gest-plus            PIC  9(5).
               10 OLD-STO-mto-vettore  PIC  9(5).
               10 OLD-STO-mto-note1    PIC  X(19).
               10 OLD-STO-mto-data-note1           PIC  9(8).
               10 OLD-STO-mto-note2    PIC  X(30).
               10 OLD-STO-mto-note3    PIC  X(30).
               10 OLD-STO-mto-note4    PIC  X(30).
               10 OLD-STO-mto-note     PIC  X(500).
               10 OLD-STO-mto-pz-tot   PIC  9(8).
               10 OLD-STO-mto-pz-eva   PIC  9(8).
               10 OLD-STO-mto-ritira-in-lubex      PIC  9.
                   88 OLD-STO-mto-ritira-si VALUE IS 1. 
                   88 OLD-STO-mto-ritira-no VALUE IS 0. 
               10 OLD-STO-mto-promo    PIC  9.
                   88 OLD-STO-mto-si-promo VALUE IS 1. 
                   88 OLD-STO-mto-no-promo VALUE IS 0. 
               10 OLD-STO-mto-stato-attivazione    PIC  X(1).
                   88 OLD-STO-mto-attivo VALUE IS "A". 
                   88 OLD-STO-mto-bloccato VALUE IS "B". 
                   88 OLD-STO-mto-chiuso-man VALUE IS "C". 
               10 OLD-STO-mto-stato-ordine         PIC  9.
                   88 OLD-STO-mto-registrato VALUE IS 1. 
                   88 OLD-STO-mto-in-lavorazione VALUE IS 2. 
                   88 OLD-STO-mto-sped-parz VALUE IS 3. 
                   88 OLD-STO-mto-sped-tot VALUE IS 4. 
      *****             88 OLD-STO-mto-fatt-parz VALUE IS 5. 
      *****             88 OLD-STO-mto-fatt-tot VALUE IS 6. 
      *
      *
                   88 OLD-STO-mto-chiuso VALUE IS 7. 
               10 OLD-STO-mto-dati-comuni.
                   15 OLD-STO-mto-data-creazione       PIC  9(8).
                   15 OLD-STO-mto-ora-creazione        PIC  9(8).
                   15 OLD-STO-mto-utente-creazione     PIC  X(10).
                   15 OLD-STO-mto-data-ultima-modifica PIC  9(8).
                   15 OLD-STO-mto-ora-ultima-modifica  PIC  9(8).
                   15 OLD-STO-mto-utente-ultima-modifica           PIC  
           X(10).
               10 OLD-STO-mto-prenotazione-qta     PIC  9(1).
                   88 OLD-STO-mto-prenotazione-qta-si VALUE IS 1. 
                   88 OLD-STO-mto-prenotazione-qta-no VALUE IS 0. 
               10 OLD-STO-mto-causale-blocco       PIC  xx.
                   88 OLD-STO-mto-causale-blocco-prezzo VALUE IS "PR". 
                   88 OLD-STO-mto-causale-blocco-fido VALUE IS "FI". 
                   88 OLD-STO-mto-causale-blocco-manuale VALUE IS "MA". 
               10 OLD-STO-mto-saldi-banco          PIC  9(1).
                   88 OLD-STO-mto-saldi-banco-si VALUE IS 1. 
                   88 OLD-STO-mto-saldi-banco-no VALUE IS 0. 
               10 OLD-STO-mto-forn-reso            PIC  9(5).
               10 OLD-STO-mto-saldi-promo          PIC  9(1).
                   88 OLD-STO-mto-saldi-promo-si VALUE IS 1. 
                   88 OLD-STO-mto-saldi-promo-no VALUE IS 0. 
               10 OLD-STO-mto-immediato            PIC  9(1).
                   88 OLD-STO-mto-immediato-si VALUE IS 1. 
                   88 OLD-STO-mto-immediato-no VALUE IS 0. 
               10 OLD-STO-mto-vuoti.
      *(( XFD NAME = OLD-STO-mto-num-vuoto- ))
                   15 OLD-STO-mto-promo-fittizia       PIC  9(15).
                   15 OLD-STO-mto-alfa-vuoto-1         PIC  X(17).
                   15 OLD-STO-mto-alfa-vuoto-2         PIC  X(20).
                   15 OLD-STO-mto-alfa-vuoto-3         PIC  X(20).
               10 OLD-STO-mto-ultima-evasione.
                   15 OLD-STO-mto-data-evasione        PIC  9(8).
                   15 OLD-STO-mto-ora-evasione         PIC  9(8).
                   15 OLD-STO-mto-utente-evasione      PIC  x(15).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-sto-mtordini       pic X(2).
       77  status-sto-mtordini-old   pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.
       77  path-archivi         pic x(1200).
       77  PATH-STO-MTORDINI     pic x(1200).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     STO-mtordini
                     sto-mtordini-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.         
           accept path-archivi from environment "PATH_ARCHIVI_STO".
           set environment "FILE_PREFIX" to path-archivi.
           inspect path-archivi replacing trailing spaces by low-value.
           string path-archivi delimited low-value
                  "mtordini"   delimited size
                  into path-sto-mtordini
           end-string.

           display message box
                        "Confermi la conversione del file STO-mtordini?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "sto-mtordini"
                          x"22"
                          " in "
                          x"22"
                          "sto-mtordini-old"
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
           move zero   to cont.

           open input  sto-mtordini-old.
           open output sto-mtordini.

           move low-value to OLD-STO-mto-chiave.

           start STO-mtordini-old key >= old-STO-mto-chiave
                 invalid 
                 continue
             not invalid
                 perform until 1 = 2
                    read STO-mtordini-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close STO-mtordini
                 sto-mtordini-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize STO-mto-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.      
                                       
           move OLD-STO-mto-anno           to STO-mto-anno                     
           move OLD-STO-mto-numero         to STO-mto-numero                 
           move OLD-STO-mto-causale        to STO-mto-causale                
           move OLD-STO-mto-tipo-CF        to STO-mto-tipo-CF                
           move OLD-STO-mto-cod-cli        to STO-mto-cod-cli                
           move OLD-STO-mto-prg-destino    to STO-mto-prg-destino            
           move OLD-STO-mto-gdo            to STO-mto-gdo                    
           move OLD-STO-mto-num-ord-cli    to STO-mto-num-ord-cli            
           move OLD-STO-mto-data-ordine    to STO-mto-data-ordine            
           move OLD-STO-mto-data-passaggio-ordine  to 
           STO-mto-data-passaggio-ordine  
           move OLD-STO-mto-cod-agente             to STO-mto-cod-agente             
           move OLD-STO-mto-cod-pagamento          to 
           STO-mto-cod-pagamento          
           move OLD-STO-mto-cod-ese-iva            to 
           STO-mto-cod-ese-iva            
           move OLD-STO-mto-gest-plus              to STO-mto-gest-plus              
           move OLD-STO-mto-vettore                to STO-mto-vettore                
           move OLD-STO-mto-note1                  to STO-mto-note1                  
           move OLD-STO-mto-data-note1             to STO-mto-data-note1             
           move OLD-STO-mto-note2                  to STO-mto-note2                  
           move OLD-STO-mto-note3                  to STO-mto-note3                  
           move OLD-STO-mto-note4                  to STO-mto-note4                  
           move OLD-STO-mto-note                   to STO-mto-note                   
           move OLD-STO-mto-pz-tot                 to STO-mto-pz-tot                 
           move OLD-STO-mto-pz-eva                 to STO-mto-pz-eva                 
           move OLD-STO-mto-ritira-in-lubex        to 
           STO-mto-ritira-in-lubex        
           move OLD-STO-mto-promo                  to STO-mto-promo                  
           move OLD-STO-mto-stato-attivazione      to 
           STO-mto-stato-attivazione      
           move OLD-STO-mto-stato-ordine      to STO-mto-stato-ordine           
           move OLD-STO-mto-data-creazione    to STO-mto-data-creazione         
           move OLD-STO-mto-ora-creazione     to STO-mto-ora-creazione          
           move OLD-STO-mto-utente-creazione       to 
           STO-mto-utente-creazione       
           move OLD-STO-mto-data-ultima-modifica   to 
           STO-mto-data-ultima-modifica   
           move OLD-STO-mto-ora-ultima-modifica    to 
           STO-mto-ora-ultima-modifica    
           move OLD-STO-mto-utente-ultima-modifica to 
           STO-mto-utente-ultima-modifica 
           move OLD-STO-mto-prenotazione-qta to STO-mto-prenotazione-qta               
           move OLD-STO-mto-causale-blocco   to STO-mto-causale-blocco                 
           move OLD-STO-mto-saldi-banco      to STO-mto-saldi-banco                    
           move OLD-STO-mto-forn-reso        to STO-mto-forn-reso              
           move OLD-STO-mto-saldi-promo      to STO-mto-saldi-promo                    
           move OLD-STO-mto-immediato        to STO-mto-immediato                      
           move OLD-STO-mto-promo-fittizia   to STO-mto-promo-fittizia         
           move OLD-STO-mto-alfa-vuoto-1     to STO-mto-alfa-vuoto-1           
           move OLD-STO-mto-alfa-vuoto-2     to STO-mto-alfa-vuoto-2           
           move OLD-STO-mto-alfa-vuoto-3     to STO-mto-alfa-vuoto-3               
           move OLD-STO-mto-data-evasione    to STO-mto-data-evasione          
           move OLD-STO-mto-ora-evasione     to STO-mto-ora-evasione           
           move OLD-STO-mto-utente-evasione  to STO-mto-utente-evasione        

           write STO-mto-rec. 
