       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-sto-tordini.
       remarks. Per allineamento storico
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       copy "STO-tordini.sl".

       SELECT sto-tordini-old
           ASSIGN       TO "sto-tordini-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-sto-tordini-old
           RECORD KEY   IS OLD-STO-tor-chiave
           ALTERNATE RECORD KEY IS k-causale = OLD-STO-tor-causale, 
           OLD-STO-tor-anno, OLD-STO-tor-numero
           ALTERNATE RECORD KEY IS k1 = OLD-STO-tor-cod-cli, 
           OLD-STO-tor-prg-destino, OLD-STO-tor-anno, 
           OLD-STO-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k2 =
           OLD-STO-tor-data-passaggio-ordine, 
           OLD-STO-tor-anno, OLD-STO-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-bolla = OLD-STO-tor-anno-bolla, 
           OLD-STO-tor-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k3 = OLD-STO-tor-anno-bolla, 
           OLD-STO-tor-data-bolla, OLD-STO-tor-num-bolla, 
           OLD-STO-tor-bolla-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-fattura = OLD-STO-tor-anno-fattura, 
           OLD-STO-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k4 = OLD-STO-tor-anno-fattura, 
           OLD-STO-tor-data-fattura, OLD-STO-tor-num-fattura, 
           OLD-STO-tor-num-prenot, OLD-STO-tor-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-contab = OLD-STO-tor-agg-contab, 
           OLD-STO-tor-anno-fattura, OLD-STO-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tipo = OLD-STO-tor-tipo, 
           OLD-STO-tor-chiave
           ALTERNATE RECORD KEY IS k-data = OLD-STO-tor-data-creazione, 
           OLD-STO-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-agfatt = OLD-STO-tor-anno-fattura, 
           OLD-STO-tor-data-fattura, OLD-STO-tor-num-fattura, 
           OLD-STO-tor-num-prenot, OLD-STO-tor-fatt-prenotata, 
           OLD-STO-tor-chiave
           ALTERNATE RECORD KEY IS k-stbolle = OLD-STO-tor-anno-bolla, 
           OLD-STO-tor-data-bolla, OLD-STO-tor-num-bolla, 
           OLD-STO-tor-bolla-prenotata, OLD-STO-tor-chiave
           ALTERNATE RECORD KEY IS k-andamento-data = 
           OLD-STO-tor-agg-contab, OLD-STO-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-cliente = 
           OLD-STO-tor-cod-cli, OLD-STO-tor-agg-contab,
           OLD-STO-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-andamento-clides = 
           OLD-STO-tor-cod-cli, OLD-STO-tor-prg-destino, 
           OLD-STO-tor-agg-contab, 
           OLD-STO-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-promo = OLD-STO-tor-stato, 
           OLD-STO-tor-promo, OLD-STO-tor-data-ordine, 
           OLD-STO-tor-numero, 
           OLD-STO-tor-cod-cli, OLD-STO-tor-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-or = OLD-STO-tor-cod-cli, 
           OLD-STO-tor-prg-destino, OLD-STO-tor-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-tor-inviare = 
           OLD-STO-tor-da-inviare , OLD-STO-tor-chiave
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "STO-tordini.fd".
      *(( XFD FILE = OLD-STO-tordini ))
       FD  STO-tordini-old.
       01 OLD-STO-tor-rec.
           05 OLD-STO-tor-chiave.
               10 OLD-STO-tor-anno     PIC  9(4).
               10 OLD-STO-tor-numero   PIC  9(8).
           05 OLD-STO-tor-dati.
               10 OLD-STO-tor-causale  PIC  x(4).
               10 OLD-STO-tor-cod-cli  PIC  9(5).
               10 OLD-STO-tor-prg-destino          PIC  9(5).
               10 OLD-STO-tor-num-ord-cli          PIC  X(10).
               10 OLD-STO-tor-data-ordine          PIC  9(8).
               10 OLD-STO-tor-data-passaggio-ordine           PIC  9(8).
               10 OLD-STO-tor-cod-agente           PIC  9(5).
               10 OLD-STO-tor-cod-pagamento        PIC  x(3).
               10 OLD-STO-tor-cod-ese-iva          PIC  x(3).
               10 OLD-STO-tor-spostam-ric-ago      PIC  X(1).
                   88 OLD-STO-tor-si-ric-ago VALUE IS "S". 
                   88 OLD-STO-tor-no-ric-ago VALUE IS "N". 
               10 OLD-STO-tor-spostam-ric-dic      PIC  X(1).
                   88 OLD-STO-tor-si-ric-dic VALUE IS "S". 
                   88 OLD-STO-tor-no-ric-dic VALUE IS "N". 
               10 OLD-STO-tor-vettore  PIC  9(5).
               10 OLD-STO-tor-note1    PIC  X(19).
               10 OLD-STO-tor-data-note1           PIC  9(8).
               10 OLD-STO-tor-note2    PIC  X(30).
               10 OLD-STO-tor-note3    PIC  X(30).
               10 OLD-STO-tor-note4    PIC  X(30).
               10 OLD-STO-tor-invio    PIC  x.
                   88 OLD-STO-tor-invio-manuale VALUE IS "M". 
                   88 OLD-STO-tor-invio-postel VALUE IS "P". 
                   88 OLD-STO-tor-invio-edi VALUE IS "E". 
               10 OLD-STO-tor-bolla.
                   15 OLD-STO-tor-anno-bolla           PIC  9(4).
                   15 OLD-STO-tor-num-bolla            PIC  9(8).
                   15 OLD-STO-tor-data-bolla           PIC  9(8).
                   15 OLD-STO-tor-bolla-prenotata      PIC  x.
                       88 OLD-STO-tor-bolla-si-prenotata VALUE IS "S". 
                       88 OLD-STO-tor-bolla-no-prenotata VALUE IS "N". 
               10 OLD-STO-tor-fattura.
                   15 OLD-STO-tor-anno-fattura         PIC  9(4).
                   15 OLD-STO-tor-num-fattura          PIC  9(8).
                   15 OLD-STO-tor-data-fattura         PIC  9(8).
                   15 OLD-STO-tor-num-prenot           PIC  9(8).
                   15 OLD-STO-tor-fatt-prenotata       PIC  x.
                       88 OLD-STO-tor-fatt-si-prenotata VALUE IS "S". 
                       88 OLD-STO-tor-fatt-no-prenotata VALUE IS "N". 
               10 OLD-STO-tor-mod-caricamento      PIC  X(1).
                   88 OLD-STO-tor-manuale VALUE IS "M". 
                   88 OLD-STO-tor-guidata VALUE IS "G". 
               10 OLD-STO-tor-agg-contab           PIC  x.
                   88 OLD-STO-tor-si-agg-contab VALUE IS "S". 
                   88 OLD-STO-tor-no-agg-contab VALUE IS "N". 
               10 OLD-STO-tor-tipo     PIC  x.
                   88 OLD-STO-tor-fattura-manuale VALUE IS "M". 
                   88 OLD-STO-tor-ordine VALUE IS "O". 
               10 OLD-STO-tor-note     PIC  X(500).
               10 OLD-STO-tor-contropartita        PIC  X(8).
               10 OLD-STO-tor-stato    PIC  X(1).
                   88 OLD-STO-tor-attivo VALUE IS "A". 
                   88 OLD-STO-tor-disattivo VALUE IS "D". 
                   88 OLD-STO-tor-bloccato VALUE IS "B". 
               10 OLD-STO-tor-dati-comuni.
                   15 OLD-STO-tor-data-creazione       PIC  9(8).
                   15 OLD-STO-tor-ora-creazione        PIC  9(8).
                   15 OLD-STO-tor-utente-creazione     PIC  X(10).
                   15 OLD-STO-tor-data-ultima-modifica PIC  9(8).
                   15 OLD-STO-tor-ora-ultima-modifica  PIC  9(8).
                   15 OLD-STO-tor-utente-ultima-modifica           PIC  
           X(10).
               10 OLD-STO-tor-vuoti.
                   15 OLD-STO-tor-data-contab          PIC  9(8).
                   15 OLD-STO-tor-promo    PIC  9.
                       88 OLD-STO-tor-si-promo VALUE IS 1. 
                       88 OLD-STO-tor-no-promo VALUE IS 0. 
                   15 OLD-STO-tor-gest-plus            PIC  9(5).
                   15 OLD-STO-tor-ritira-in-lubex      PIC  9.
                       88 OLD-STO-tor-ritira-si VALUE IS 1. 
                       88 OLD-STO-tor-ritira-no VALUE IS 0. 
                   15 OLD-STO-tor-taglio   PIC  9(6).
                   15 OLD-STO-tor-flag-rec-prezzi      PIC  9.
                       88 OLD-STO-tor-rec-prezzi VALUE IS 1  WHEN SET TO 
           FALSE  0. 
                   15 OLD-STO-tor-ordine-testa.
                       20 OLD-STO-tor-anno-testa           PIC  9(4).
                       20 OLD-STO-tor-num-testa            PIC  9(8).
                   15 OLD-STO-tor-da-ordine            PIC  9.
                       88 OLD-STO-tor-da-ordine-no VALUE IS 0. 
                       88 OLD-STO-tor-da-ordine-si VALUE IS 1. 
                   15 OLD-STO-tor-forn-reso            PIC  9(5).
                   15 OLD-STO-tor-num-vuoto-3          PIC  9(5).
                   15 OLD-STO-tor-esito-consegna       PIC  X(10).
                   15 OLD-STO-tor-data-bolla-effettiva PIC  9(8).
                   15 OLD-STO-tor-tipo-evasione        PIC  X(1).
                       88 OLD-STO-tor-ev-singola VALUE IS "S". 
                       88 OLD-STO-tor-ev-manuale VALUE IS "M". 
                       88 OLD-STO-tor-ev-immediata VALUE IS "I". 
                       88 OLD-STO-tor-ev-normale VALUE IS "N". 
                       88 OLD-STO-tor-ev-auto-trad VALUE IS "T". 
                       88 OLD-STO-tor-ev-auto-gdo VALUE IS "G". 
                   15 OLD-STO-tor-da-inviare           PIC  X(1).
                       88 OLD-STO-tor-da-inviare-si VALUE IS "S". 
                       88 OLD-STO-tor-da-inviare-no VALUE IS "N" " "    
           WHEN SET TO FALSE  " ". 
                   15 OLD-STO-tor-ora-contab           PIC  9(8).
                   15 OLD-STO-tor-fattura-from.
                       20 OLD-STO-tor-fattura-from-data    PIC  9(8).
                       20 OLD-STO-tor-fattura-from-numero  PIC  x(8).
                   15 OLD-STO-tor-alfa-vuoto-3         PIC  X(16).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-sto-tordini       pic X(2).
       77  status-sto-tordini-old   pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.
       77  path-archivi         pic x(1200).
       77  PATH-STO-TORDINI     pic x(1200).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     STO-tordini
                     sto-tordini-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.         
           accept path-archivi from environment "PATH_ARCHIVI_STO".
           set environment "FILE_PREFIX" to path-archivi.
           inspect path-archivi replacing trailing spaces by low-value.
           string path-archivi delimited low-value
                  "tordini"    delimited size
                  into path-sto-tordini
           end-string.

           display message box
                         "Confermi la conversione del file STO-tordini?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "sto-tordini"
                          x"22"
                          " in "
                          x"22"
                          "sto-tordini-old"
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

           open input  sto-tordini-old.
           open output sto-tordini.

           move low-value to OLD-STO-tor-chiave.

           start STO-tordini-old key >= old-STO-tor-chiave
                 invalid 
                 continue
             not invalid
                 perform until 1 = 2
                    read STO-tordini-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close STO-tordini
                 sto-tordini-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize STO-tor-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.      
           move OLD-STO-tor-anno                 to STO-tor-anno                 
           move OLD-STO-tor-numero               to STO-tor-numero               
           move OLD-STO-tor-causale              to STO-tor-causale              
           move OLD-STO-tor-cod-cli              to STO-tor-cod-cli              
           move OLD-STO-tor-prg-destino          to STO-tor-prg-destino          
           move OLD-STO-tor-num-ord-cli          to STO-tor-num-ord-cli          
           move OLD-STO-tor-data-ordine          to STO-tor-data-ordine          
           move OLD-STO-tor-data-passaggio-ordine to 
           STO-tor-data-passaggio-ordine
           move OLD-STO-tor-cod-agente           to STO-tor-cod-agente           
           move OLD-STO-tor-cod-pagamento        to 
           STO-tor-cod-pagamento        
           move OLD-STO-tor-cod-ese-iva          to STO-tor-cod-ese-iva          
           move OLD-STO-tor-spostam-ric-ago      to  
           STO-tor-spostam-ric-ago      
           move OLD-STO-tor-spostam-ric-dic      to  
           STO-tor-spostam-ric-dic      
           move OLD-STO-tor-vettore              to STO-tor-vettore              
           move OLD-STO-tor-note1                to STO-tor-note1                
           move OLD-STO-tor-data-note1           to STO-tor-data-note1           
           move OLD-STO-tor-note2                to STO-tor-note2                
           move OLD-STO-tor-note3                to STO-tor-note3                
           move OLD-STO-tor-note4                to STO-tor-note4                
           move OLD-STO-tor-invio                to STO-tor-invio                
           move OLD-STO-tor-anno-bolla           to STO-tor-anno-bolla           
           move OLD-STO-tor-num-bolla            to STO-tor-num-bolla            
           move OLD-STO-tor-data-bolla           to STO-tor-data-bolla           
           move OLD-STO-tor-bolla-prenotata      to  
           STO-tor-bolla-prenotata      
           move OLD-STO-tor-anno-fattura         to STO-tor-anno-fattura         
           move OLD-STO-tor-num-fattura          to STO-tor-num-fattura          
           move OLD-STO-tor-data-fattura         to STO-tor-data-fattura         
           move OLD-STO-tor-num-prenot           to STO-tor-num-prenot           
           move OLD-STO-tor-fatt-prenotata       to  
           STO-tor-fatt-prenotata       
           move OLD-STO-tor-mod-caricamento      to  
           STO-tor-mod-caricamento      
           move OLD-STO-tor-agg-contab           to STO-tor-agg-contab           
           move OLD-STO-tor-tipo                 to STO-tor-tipo                 
           move OLD-STO-tor-note                 to STO-tor-note                 
           move OLD-STO-tor-contropartita        to
           STO-tor-contropartita        
           move OLD-STO-tor-stato                to STO-tor-stato                
           move OLD-STO-tor-data-creazione       to  
           STO-tor-data-creazione       
           move OLD-STO-tor-ora-creazione        to  
           STO-tor-ora-creazione        
           move OLD-STO-tor-utente-creazione     to  
           STO-tor-utente-creazione     
           move OLD-STO-tor-data-ultima-modifica to  
           STO-tor-data-ultima-modifica 
           move OLD-STO-tor-ora-ultima-modifica  to  
           STO-tor-ora-ultima-modifica  
           move OLD-STO-tor-utente-ultima-modifica to
           STO-tor-utente-ultima-modifica
           move OLD-STO-tor-data-contab          to STO-tor-data-contab          
           move OLD-STO-tor-promo                to STO-tor-promo                
           move OLD-STO-tor-gest-plus            to STO-tor-gest-plus            
           move OLD-STO-tor-ritira-in-lubex      to  
           STO-tor-ritira-in-lubex      
           move OLD-STO-tor-taglio               to STO-tor-taglio               
           move OLD-STO-tor-flag-rec-prezzi      to  
           STO-tor-flag-rec-prezzi      
           move OLD-STO-tor-anno-testa           to STO-tor-anno-testa           
           move OLD-STO-tor-num-testa            to STO-tor-num-testa            
           move OLD-STO-tor-da-ordine            to STO-tor-da-ordine            
           move OLD-STO-tor-forn-reso            to STO-tor-forn-reso            
           move OLD-STO-tor-num-vuoto-3          to STO-tor-num-vuoto-3          
           move OLD-STO-tor-esito-consegna       to  
           STO-tor-esito-consegna       
           move OLD-STO-tor-data-bolla-effettiva to  
           STO-tor-data-bolla-effettiva 
           move OLD-STO-tor-tipo-evasione        to  
           STO-tor-tipo-evasione        
           move OLD-STO-tor-da-inviare           to STO-tor-da-inviare           
           move OLD-STO-tor-ora-contab           to STO-tor-ora-contab           
           move OLD-STO-tor-fattura-from-data    to  
           STO-tor-fattura-from-data    
           move OLD-STO-tor-fattura-from-numero  to  
           STO-tor-fattura-from-numero  
           write STO-tor-rec. 
