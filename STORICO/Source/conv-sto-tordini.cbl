       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-sto-tordini.
       remarks. Per ampliamento numero ordine cliente
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       copy "STO-tordini.sl".

       SELECT STO-tordini-old
           ASSIGN       TO  "STO-tordini-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS status-sto-tordini-old
           RECORD KEY   IS OLD-sto-tor-chiave
           ALTERNATE RECORD KEY IS old-k-causale = OLD-sto-tor-causale, 
           OLD-sto-tor-anno, OLD-sto-tor-numero
           ALTERNATE RECORD KEY IS old-k1 = OLD-sto-tor-cod-cli, 
           OLD-sto-tor-prg-destino, OLD-sto-tor-anno, OLD-sto-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k2 = 
           OLD-sto-tor-data-passaggio-ordine, 
           OLD-sto-tor-anno, OLD-sto-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-bolla = OLD-sto-tor-anno-bolla, 
           OLD-sto-tor-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k3 = OLD-sto-tor-anno-bolla, 
           OLD-sto-tor-data-bolla, OLD-sto-tor-num-bolla, 
           OLD-sto-tor-bolla-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-fattura = 
           OLD-sto-tor-anno-fattura, 
           OLD-sto-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k4 = OLD-sto-tor-anno-fattura, 
           OLD-sto-tor-data-fattura, OLD-sto-tor-num-fattura, 
           OLD-sto-tor-num-prenot, OLD-sto-tor-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-contab = 
           OLD-sto-tor-agg-contab, 
           OLD-sto-tor-anno-fattura, OLD-sto-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-tipo = OLD-sto-tor-tipo, 
           OLD-sto-tor-chiave
           ALTERNATE RECORD KEY IS old-k-data = 
           OLD-sto-tor-data-creazione, 
           OLD-sto-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-agfatt = 
           OLD-sto-tor-anno-fattura, 
           OLD-sto-tor-data-fattura, OLD-sto-tor-num-fattura, 
           OLD-sto-tor-num-prenot, OLD-sto-tor-fatt-prenotata, 
           OLD-sto-tor-chiave
           ALTERNATE RECORD KEY IS old-k-stbolle = 
           OLD-sto-tor-anno-bolla, 
           OLD-sto-tor-data-bolla, OLD-sto-tor-num-bolla, 
           OLD-sto-tor-bolla-prenotata, OLD-sto-tor-chiave
           ALTERNATE RECORD KEY IS old-k-andamento-data = 
           OLD-sto-tor-agg-contab, OLD-sto-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-andamento-cliente = 
           OLD-sto-tor-cod-cli, OLD-sto-tor-agg-contab, 
           OLD-sto-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-andamento-clides = 
           OLD-sto-tor-cod-cli, OLD-sto-tor-prg-destino, 
           OLD-sto-tor-agg-contab, 
           OLD-sto-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-promo = OLD-sto-tor-stato, 
           OLD-sto-tor-promo, OLD-sto-tor-data-ordine, 
           OLD-sto-tor-numero, 
           OLD-sto-tor-cod-cli, OLD-sto-tor-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-or = OLD-sto-tor-cod-cli, 
           OLD-sto-tor-prg-destino, OLD-sto-tor-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-tor-inviare = 
           OLD-sto-tor-da-inviare 
           , OLD-sto-tor-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-tor-tipocli = 
           OLD-sto-tor-tipocli, OLD-sto-tor-cod-cli, 
           OLD-sto-tor-prg-destino, OLD-sto-tor-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-tor-gdo = OLD-sto-tor-gdo
           , OLD-sto-tor-cod-cli, 
           OLD-sto-tor-prg-destino, OLD-sto-tor-chiave
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "STO-tordini.fd".   

      *(( XFD FILE = STO-tordini ))
       FD  STO-tordini-old.
       01 OLD-sto-tor-rec.
           05 OLD-sto-tor-chiave.
               10 OLD-sto-tor-anno     PIC  9(4).
               10 OLD-sto-tor-numero   PIC  9(8).
           05 OLD-sto-tor-dati.
               10 OLD-sto-tor-causale  PIC  x(4).
               10 OLD-sto-tor-cod-cli  PIC  9(5).
               10 OLD-sto-tor-prg-destino          PIC  9(5).
               10 OLD-sto-tor-num-ord-cli          PIC  X(10).
               10 OLD-sto-tor-data-ordine          PIC  9(8).
               10 OLD-sto-tor-data-passaggio-ordine           PIC  9(8).
               10 OLD-sto-tor-cod-agente           PIC  9(5).
               10 OLD-sto-tor-cod-pagamento        PIC  x(3).
               10 OLD-sto-tor-cod-ese-iva          PIC  x(3).
               10 OLD-sto-tor-spostam-ric-ago      PIC  X(1).
                   88 OLD-sto-tor-si-ric-ago VALUE IS "S". 
                   88 OLD-sto-tor-no-ric-ago VALUE IS "N". 
               10 OLD-sto-tor-spostam-ric-dic      PIC  X(1).
                   88 OLD-sto-tor-si-ric-dic VALUE IS "S". 
                   88 OLD-sto-tor-no-ric-dic VALUE IS "N". 
               10 OLD-sto-tor-vettore  PIC  9(5).
               10 OLD-sto-tor-note1    PIC  X(19).
               10 OLD-sto-tor-data-note1           PIC  9(8).
               10 OLD-sto-tor-note2    PIC  X(30).
               10 OLD-sto-tor-note3    PIC  X(30).
               10 OLD-sto-tor-note4    PIC  X(30).
               10 OLD-sto-tor-invio    PIC  x.
                   88 OLD-sto-tor-invio-manuale VALUE IS "M". 
                   88 OLD-sto-tor-invio-postel VALUE IS "P". 
                   88 OLD-sto-tor-invio-edi VALUE IS "E". 
               10 OLD-sto-tor-bolla.
                   15 OLD-sto-tor-anno-bolla           PIC  9(4).
                   15 OLD-sto-tor-num-bolla            PIC  9(8).
                   15 OLD-sto-tor-data-bolla           PIC  9(8).
                   15 OLD-sto-tor-bolla-prenotata      PIC  x.
                       88 OLD-sto-tor-bolla-si-prenotata VALUE IS "S". 
                       88 OLD-sto-tor-bolla-no-prenotata VALUE IS "N". 
               10 OLD-sto-tor-fattura.
                   15 OLD-sto-tor-anno-fattura         PIC  9(4).
                   15 OLD-sto-tor-num-fattura          PIC  9(8).
                   15 OLD-sto-tor-data-fattura         PIC  9(8).
                   15 OLD-sto-tor-num-prenot           PIC  9(8).
                   15 OLD-sto-tor-fatt-prenotata       PIC  x.
                       88 OLD-sto-tor-fatt-si-prenotata VALUE IS "S". 
                       88 OLD-sto-tor-fatt-no-prenotata VALUE IS "N". 
               10 OLD-sto-tor-mod-caricamento      PIC  X(1).
                   88 OLD-sto-tor-manuale VALUE IS "M". 
                   88 OLD-sto-tor-guidata VALUE IS "G". 
               10 OLD-sto-tor-agg-contab           PIC  x.
                   88 OLD-sto-tor-si-agg-contab VALUE IS "S". 
                   88 OLD-sto-tor-no-agg-contab VALUE IS "N". 
               10 OLD-sto-tor-tipo     PIC  x.
                   88 OLD-sto-tor-fattura-manuale VALUE IS "M". 
                   88 OLD-sto-tor-ordine VALUE IS "O". 
               10 OLD-sto-tor-note     PIC  X(500).
      *(( XFD NAME = tor-contropartita_ ))
               10 OLD-sto-tor-contropartita        PIC  X(8).
               10 OLD-sto-tor-stato    PIC  X(1).
                   88 OLD-sto-tor-attivo VALUE IS "A". 
                   88 OLD-sto-tor-disattivo VALUE IS "D". 
                   88 OLD-sto-tor-bloccato VALUE IS "B". 
               10 OLD-sto-tor-dati-comuni.
                   15 OLD-sto-tor-data-creazione       PIC  9(8).
                   15 OLD-sto-tor-ora-creazione        PIC  9(8).
                   15 OLD-sto-tor-utente-creazione     PIC  X(10).
                   15 OLD-sto-tor-data-ultima-modifica PIC  9(8).
                   15 OLD-sto-tor-ora-ultima-modifica  PIC  9(8).
                   15 OLD-sto-tor-utente-ultima-modifica           PIC  
           X(10).
               10 OLD-sto-tor-vuoti.
                   15 OLD-sto-tor-data-contab          PIC  9(8).
                   15 OLD-sto-tor-promo    PIC  9.
                       88 OLD-sto-tor-si-promo VALUE IS 1. 
                       88 OLD-sto-tor-no-promo VALUE IS 0. 
                   15 OLD-sto-tor-gest-plus            PIC  9(5).
                   15 OLD-sto-tor-ritira-in-lubex      PIC  9.
                       88 OLD-sto-tor-ritira-si VALUE IS 1. 
                       88 OLD-sto-tor-ritira-no VALUE IS 0. 
                   15 OLD-sto-tor-taglio   PIC  9(6).
                   15 OLD-sto-tor-flag-rec-prezzi      PIC  9.
                       88 OLD-sto-tor-rec-prezzi VALUE IS 1 WHEN SET TO 
           FALSE  0. 
                   15 OLD-sto-tor-ordine-testa.
                       20 OLD-sto-tor-anno-testa           PIC  9(4).
                       20 OLD-sto-tor-num-testa            PIC  9(8).
                   15 OLD-sto-tor-da-ordine            PIC  9.
                       88 OLD-sto-tor-da-ordine-no VALUE IS 0. 
                       88 OLD-sto-tor-da-ordine-si VALUE IS 1. 
                   15 OLD-sto-tor-forn-reso            PIC  9(5).
                   15 OLD-sto-tor-num-vuoto-3          PIC  9(5).
                   15 OLD-sto-tor-esito-consegna       PIC  X(10).
                   15 OLD-sto-tor-data-bolla-effettiva PIC  9(8).
                   15 OLD-sto-tor-tipo-evasione        PIC  X(1).
                       88 OLD-sto-tor-ev-singola VALUE IS "S". 
                       88 OLD-sto-tor-ev-manuale VALUE IS "M". 
                       88 OLD-sto-tor-ev-immediata VALUE IS "I". 
                       88 OLD-sto-tor-ev-normale VALUE IS "N". 
                       88 OLD-sto-tor-ev-auto-trad VALUE IS "T". 
                       88 OLD-sto-tor-ev-auto-gdo VALUE IS "G". 
                   15 OLD-sto-tor-da-inviare           PIC  X(1).
                       88 OLD-sto-tor-da-inviare-si VALUE IS "S". 
                       88 OLD-sto-tor-da-inviare-no VALUE IS "N" " "    
           WHEN SET TO FALSE  " ". 
                   15 OLD-sto-tor-ora-contab           PIC  9(8).
                   15 OLD-sto-tor-fattura-from.
                       20 OLD-sto-tor-fattura-from-data    PIC  9(8).
                       20 OLD-sto-tor-fattura-from-numero  PIC  x(8).
                   15 OLD-sto-tor-gdo      PIC  X(5).
      *(( XFD NAME = tor-gdo_1 ))
                   15 OLD-sto-tor-tipocli  PIC  X(2).
      *(( XFD NAME = tor-gdo_1_1 ))
                   15 OLD-sto-tor-note-bolla-1         PIC  X(500).
      *(( XFD NAME = tor-gdo_1_1_1 ))
                   15 OLD-sto-tor-note-bolla-2         PIC  X(500).
      *(( XFD NAME = tor-gdo_1_1_1_1 ))
                   15 OLD-sto-tor-causale-orig         PIC  9(4).
      *(( XFD NAME = tor-gdo_1_1_2 ))
                   15 FILLER           PIC  X(1996).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".

       77  status-sto-tordini       pic X(2).
       77  status-sto-tordini-old   pic X(2).

       77  CONT                 PIC 9(6).
       77  CONT-ED              PIC Z(6).
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
                 invalid continue
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
                                       
           move OLD-sto-tor-anno                    
           to sto-tor-anno                  
           move OLD-sto-tor-numero                  
           to sto-tor-numero                      
           move OLD-sto-tor-causale                 
           to sto-tor-causale               
           move OLD-sto-tor-cod-cli                 
           to sto-tor-cod-cli               
           move OLD-sto-tor-prg-destino             
           to sto-tor-prg-destino           
           move OLD-sto-tor-num-ord-cli             
           to sto-tor-num-ord-cli           
           move OLD-sto-tor-data-ordine             
           to sto-tor-data-ordine           
           move OLD-sto-tor-data-passaggio-ordine   
           to sto-tor-data-passaggio-ordine         
           move OLD-sto-tor-cod-agente              
           to sto-tor-cod-agente            
           move OLD-sto-tor-cod-pagamento           
           to sto-tor-cod-pagamento         
           move OLD-sto-tor-cod-ese-iva             
           to sto-tor-cod-ese-iva           
           move OLD-sto-tor-spostam-ric-ago         
           to sto-tor-spostam-ric-ago       
           move OLD-sto-tor-spostam-ric-dic         
           to sto-tor-spostam-ric-dic       
           move OLD-sto-tor-vettore                 
           to sto-tor-vettore               
           move OLD-sto-tor-note1                   
           to sto-tor-note1                 
           move OLD-sto-tor-data-note1              
           to sto-tor-data-note1            
           move OLD-sto-tor-note2                   
           to sto-tor-note2                 
           move OLD-sto-tor-note3                   
           to sto-tor-note3                 
           move OLD-sto-tor-note4                   
           to sto-tor-note4                 
           move OLD-sto-tor-invio                   
           to sto-tor-invio                 
           move OLD-sto-tor-anno-bolla              
           to sto-tor-anno-bolla            
           move OLD-sto-tor-num-bolla               
           to sto-tor-num-bolla             
           move OLD-sto-tor-data-bolla              
           to sto-tor-data-bolla            
           move OLD-sto-tor-bolla-prenotata         
           to sto-tor-bolla-prenotata       
           move OLD-sto-tor-anno-fattura            
           to sto-tor-anno-fattura          
           move OLD-sto-tor-num-fattura             
           to sto-tor-num-fattura           
           move OLD-sto-tor-data-fattura            
           to sto-tor-data-fattura          
           move OLD-sto-tor-num-prenot              
           to sto-tor-num-prenot            
           move OLD-sto-tor-fatt-prenotata          
           to sto-tor-fatt-prenotata        
           move OLD-sto-tor-mod-caricamento         
           to sto-tor-mod-caricamento       
           move OLD-sto-tor-agg-contab              
           to sto-tor-agg-contab            
           move OLD-sto-tor-tipo                    
           to sto-tor-tipo                  
           move OLD-sto-tor-note                    
           to sto-tor-note                  
           move OLD-sto-tor-contropartita           
           to sto-tor-contropartita         
           move OLD-sto-tor-stato                   
           to sto-tor-stato                 
           move OLD-sto-tor-data-creazione          
           to sto-tor-data-creazione        
           move OLD-sto-tor-ora-creazione           
           to sto-tor-ora-creazione         
           move OLD-sto-tor-utente-creazione        
           to sto-tor-utente-creazione      
           move OLD-sto-tor-data-ultima-modifica    
           to sto-tor-data-ultima-modifica  
           move OLD-sto-tor-ora-ultima-modifica     
           to sto-tor-ora-ultima-modifica   
           move OLD-sto-tor-utente-ultima-modifica  
           to sto-tor-utente-ultima-modifica         
           move OLD-sto-tor-data-contab             
           to sto-tor-data-contab           
           move OLD-sto-tor-promo                   
           to sto-tor-promo                 
           move OLD-sto-tor-gest-plus               
           to sto-tor-gest-plus             
           move OLD-sto-tor-ritira-in-lubex         
           to sto-tor-ritira-in-lubex       
           move OLD-sto-tor-taglio                  
           to sto-tor-taglio                
           move OLD-sto-tor-flag-rec-prezzi         
           to sto-tor-flag-rec-prezzi                
           move OLD-sto-tor-anno-testa              
           to sto-tor-anno-testa            
           move OLD-sto-tor-num-testa               
           to sto-tor-num-testa             
           move OLD-sto-tor-da-ordine               
           to sto-tor-da-ordine             
           move OLD-sto-tor-forn-reso               
           to sto-tor-forn-reso             
           move OLD-sto-tor-num-vuoto-3             
           to sto-tor-num-vuoto-3           
           move OLD-sto-tor-esito-consegna          
           to sto-tor-esito-consegna        
           move OLD-sto-tor-data-bolla-effettiva    
           to sto-tor-data-bolla-effettiva  
           move OLD-sto-tor-tipo-evasione           
           to sto-tor-tipo-evasione         
           move OLD-sto-tor-da-inviare              
           to sto-tor-da-inviare            
           move OLD-sto-tor-ora-contab              
           to sto-tor-ora-contab            
           move OLD-sto-tor-fattura-from-data       
           to sto-tor-fattura-from-data     
           move OLD-sto-tor-fattura-from-numero     
           to sto-tor-fattura-from-numero   
           move OLD-sto-tor-gdo                     
           to sto-tor-gdo                   
           move OLD-sto-tor-tipocli                 
           to sto-tor-tipocli               
           move OLD-sto-tor-note-bolla-1            
           to sto-tor-note-bolla-1          
           move OLD-sto-tor-note-bolla-2            
           to sto-tor-note-bolla-2          
           move OLD-sto-tor-causale-orig            
           to sto-tor-causale-orig          

           write STO-tor-rec. 
