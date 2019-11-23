       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convmtordini.
       remarks. Per aggiunta note bolla
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mtordini.sl".

       SELECT mtordini-old
           ASSIGN       TO  "mtordini-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-mtordini-old
           RECORD KEY   IS OLD-mto-chiave
           ALTERNATE RECORD KEY IS OLD-mto-k-ord-cli = OLD-mto-anno, 
           OLD-mto-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-mto-k-data = OLD-mto-data-ordine, 
           OLD-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-mto-k-clides = OLD-mto-cod-cli, 
           OLD-mto-prg-destino, OLD-mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-mto-k-age = OLD-mto-cod-agente, 
           OLD-mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-mto-stato-sel = 
           OLD-mto-stato-ordine, 
           OLD-mto-cod-cli, OLD-mto-data-ordine
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-mto-stato = 
           OLD-mto-stato-ordine, 
           OLD-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-mto-k-gdo = OLD-mto-gdo, 
           OLD-mto-data-ordine, OLD-mto-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS OLD-mto-k-bloc = 
           OLD-mto-stato-attivazione, 
           OLD-mto-data-ordine, OLD-mto-cod-cli, OLD-mto-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-giang = 
           OLD-mto-data-note1, OLD-mto-chiave
           WITH DUPLICATES .

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "mtordini.fd".

       FD  mtordini-old.
       01 OLD-mto-rec.
           05 OLD-mto-chiave.
               10 OLD-mto-anno         PIC  9(4).
               10 OLD-mto-numero       PIC  9(8).
           05 OLD-mto-dati.
               10 OLD-mto-causale      PIC  x(4).
               10 OLD-mto-tipo-CF      PIC  x.
               10 OLD-mto-cod-cli      PIC  9(5).
               10 OLD-mto-prg-destino  PIC  9(5).
               10 OLD-mto-gdo          PIC  x(5).
               10 OLD-mto-num-ord-cli  PIC  X(50).
               10 OLD-mto-data-ordine  PIC  9(8).
               10 OLD-mto-data-passaggio-ordine    PIC  9(8).
               10 OLD-mto-cod-agente   PIC  9(5).
               10 OLD-mto-cod-pagamento            PIC  x(3).
               10 OLD-mto-cod-ese-iva  PIC  x(3).
               10 OLD-mto-gest-plus    PIC  9(5).
               10 OLD-mto-vettore      PIC  9(5).
               10 OLD-mto-note1        PIC  X(19).
               10 OLD-mto-data-note1   PIC  9(8).
               10 OLD-mto-note2        PIC  X(30).
               10 OLD-mto-note3        PIC  X(30).
               10 OLD-mto-note4        PIC  X(30).
               10 OLD-mto-note         PIC  X(500).
               10 OLD-mto-pz-tot       PIC  9(8).
               10 OLD-mto-pz-eva       PIC  9(8).
               10 OLD-mto-ritira-in-lubex          PIC  9.
                   88 OLD-mto-ritira-si VALUE IS 1. 
                   88 OLD-mto-ritira-no VALUE IS 0. 
               10 OLD-mto-promo        PIC  9.
                   88 OLD-mto-si-promo VALUE IS 1. 
                   88 OLD-mto-no-promo VALUE IS 0. 
               10 OLD-mto-stato-attivazione        PIC  X(1).
                   88 OLD-mto-attivo VALUE IS "A". 
                   88 OLD-mto-bloccato VALUE IS "B". 
                   88 OLD-mto-chiuso-man VALUE IS "C". 
               10 OLD-mto-stato-ordine PIC  9.
                   88 OLD-mto-registrato VALUE IS 1. 
                   88 OLD-mto-in-lavorazione VALUE IS 2. 
                   88 OLD-mto-sped-parz VALUE IS 3. 
                   88 OLD-mto-sped-tot VALUE IS 4. 
      *****             88 OLD-mto-fatt-parz VALUE IS 5. 
      *****             88 OLD-mto-fatt-tot VALUE IS 6. 
      *
      *
                   88 OLD-mto-chiuso VALUE IS 7. 
               10 OLD-mto-dati-comuni.
                   15 OLD-mto-data-creazione           PIC  9(8).
                   15 OLD-mto-ora-creazione            PIC  9(8).
                   15 OLD-mto-utente-creazione         PIC  X(10).
                   15 OLD-mto-data-ultima-modifica     PIC  9(8).
                   15 OLD-mto-ora-ultima-modifica      PIC  9(8).
                   15 OLD-mto-utente-ultima-modifica   PIC  X(10).
               10 OLD-mto-prenotazione-qta         PIC  9(1).
                   88 OLD-mto-prenotazione-qta-si VALUE IS 1. 
                   88 OLD-mto-prenotazione-qta-no VALUE IS 0. 
               10 OLD-mto-causale-blocco           PIC  xx.
                   88 OLD-mto-causale-blocco-prezzo VALUE IS "PR". 
                   88 OLD-mto-causale-blocco-fido VALUE IS "FI". 
                   88 OLD-mto-causale-blocco-manuale VALUE IS "MA". 
               10 OLD-mto-saldi-banco  PIC  9(1).
                   88 OLD-mto-saldi-banco-si VALUE IS 1. 
                   88 OLD-mto-saldi-banco-no VALUE IS 0. 
               10 OLD-mto-forn-reso    PIC  9(5).
               10 OLD-mto-saldi-promo  PIC  9(1).
                   88 OLD-mto-saldi-promo-si VALUE IS 1. 
                   88 OLD-mto-saldi-promo-no VALUE IS 0. 
               10 OLD-mto-immediato    PIC  9(1).
                   88 OLD-mto-immediato-si VALUE IS 1. 
                   88 OLD-mto-immediato-no VALUE IS 0. 
               10 OLD-mto-vuoti.
      *(( XFD NAME = OLD-mto-num-vuoto-3_1 ))
                   15 OLD-mto-promo-fittizia           PIC  9(15).
                   15 OLD-mto-ordine-EDI.
                       20 OLD-mto-ordine-EDI-anno          PIC  9(4).
                       20 OLD-mto-ordine-EDI-numero        PIC  9(8).
                   15 OLD-mto-alfa-vuoto-1 PIC  X(5).
                   15 OLD-mto-alfa-vuoto-2 PIC  X(20).
                   15 OLD-mto-alfa-vuoto-3 PIC  X(20).
               10 OLD-mto-ultima-evasione.
                   15 OLD-mto-data-evasione            PIC  9(8).
                   15 OLD-mto-ora-evasione PIC  9(8).
                   15 OLD-mto-utente-evasione          PIC  x(15).
               10 FILLER           PIC  x(200). 


       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-mtordini      pic X(2).
       77  status-mtordini-old  pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     mtordini
                     mtordini-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file mtordini?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "mtordini"
                          x"22"
                          " in "
                          x"22"
                          "mtordini-old"
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

           open input  mtordini-old.
           open output mtordini


           move low-value to old-mto-chiave.

           start mtordini-old key not less old-mto-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close mtordini
                 mtordini-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize mto-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.      

           move OLD-mto-chiave                to mto-chiave               
           move OLD-mto-causale               to mto-causale              
           move OLD-mto-tipo-CF               to mto-tipo-CF              
           move OLD-mto-cod-cli               to mto-cod-cli              
           move OLD-mto-prg-destino           to mto-prg-destino          
           move OLD-mto-gdo                   to mto-gdo                  
           move OLD-mto-num-ord-cli           to mto-num-ord-cli          
           move OLD-mto-data-ordine           to mto-data-ordine          
           move OLD-mto-data-passaggio-ordine 
             to mto-data-passaggio-ordine   
           move OLD-mto-cod-agente            to mto-cod-agente           
           move OLD-mto-cod-pagamento         to mto-cod-pagamento        
           move OLD-mto-cod-ese-iva           to mto-cod-ese-iva          
           move OLD-mto-gest-plus             to mto-gest-plus            
           move OLD-mto-vettore               to mto-vettore              
           move OLD-mto-note1                 to mto-note1                
           move OLD-mto-data-note1            to mto-data-note1           
           move OLD-mto-note2                 to mto-note2                
           move OLD-mto-note3                 to mto-note3                
           move OLD-mto-note4                 to mto-note4                
           move OLD-mto-note                  to mto-note                 
           move OLD-mto-pz-tot                to mto-pz-tot               
           move OLD-mto-pz-eva                to mto-pz-eva               
           move OLD-mto-ritira-in-lubex       to mto-ritira-in-lubex      
           move OLD-mto-promo                 to mto-promo                
           move OLD-mto-stato-attivazione     to mto-stato-attivazione    
           move OLD-mto-stato-ordine          to mto-stato-ordine         
           move OLD-mto-dati-comuni           to mto-dati-comuni          
           move OLD-mto-prenotazione-qta      to mto-prenotazione-qta     
           move OLD-mto-causale-blocco        to mto-causale-blocco       
           move OLD-mto-saldi-banco           to mto-saldi-banco          
           move OLD-mto-forn-reso             to mto-forn-reso            
           move OLD-mto-saldi-promo           to mto-saldi-promo          
           move OLD-mto-immediato             to mto-immediato            
           move OLD-mto-vuoti                 to mto-vuoti                
           move OLD-mto-ultima-evasione       to mto-ultima-evasione      

           write mto-rec.     
