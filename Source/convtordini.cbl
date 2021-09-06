       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convtordini.
       remarks. Per ampliamento numero ordine cliente
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".

       SELECT tordini-old
           ASSIGN       TO  "tordini-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS STATUS-tordini-old
           RECORD KEY   IS old-tor-chiave
           ALTERNATE RECORD KEY IS old-k-causale = 
           old-tor-causale, old-tor-anno, 
           old-tor-numero
           ALTERNATE RECORD KEY IS old-k1 = old-tor-cod-cli, 
           old-tor-prg-destino, 
           old-tor-anno, old-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k2 = 
           old-tor-data-passaggio-ordine, 
           old-tor-anno, old-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-bolla = old-tor-anno-bolla, 
           old-tor-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k3 = old-tor-anno-bolla, 
           old-tor-data-bolla, 
           old-tor-num-bolla, old-tor-bolla-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-fattura = old-tor-anno-fattura, 
           old-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k4 = old-tor-anno-fattura, 
           old-tor-data-fattura, old-tor-num-fattura, 
           old-tor-num-prenot, 
           old-tor-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-contab = old-tor-agg-contab, 
           old-tor-anno-fattura, old-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-tipo = old-tor-tipo, 
           old-tor-chiave
           ALTERNATE RECORD KEY IS old-k-data = old-tor-data-creazione, 
           old-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-agfatt = old-tor-anno-fattura, 
           old-tor-data-fattura, old-tor-num-fattura, 
           old-tor-num-prenot, 
           old-tor-fatt-prenotata, old-tor-chiave
           ALTERNATE RECORD KEY IS old-k-stbolle = old-tor-anno-bolla, 
           old-tor-data-bolla, old-tor-num-bolla, 
           old-tor-bolla-prenotata, 
           old-tor-chiave
           ALTERNATE RECORD KEY IS old-k-andamento-data = 
           old-tor-agg-contab, 
           old-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-andamento-cliente = 
           old-tor-cod-cli, 
           old-tor-agg-contab, old-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-andamento-clides = 
           old-tor-cod-cli, 
           old-tor-prg-destino, old-tor-agg-contab, old-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-promo = old-tor-stato, 
           old-tor-promo, 
           old-tor-data-ordine, old-tor-numero, old-tor-cod-cli, 
           old-tor-prg-destino
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-or = old-tor-cod-cli, 
           old-tor-prg-destino, 
           old-tor-num-ord-cli
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-old-tor-inviare = 
           old-tor-da-inviare, old-tor-chiave 
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-old-tor-tipocli = 
           old-tor-tipocli , old-tor-cod-cli, old-tor-prg-destino, 
           old-tor-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-k-old-tor-gdo = 
           old-tor-gdo, 
           old-tor-cod-cli, old-tor-prg-destino, 
           old-tor-chiave
           WITH DUPLICATES.


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "tordini.fd".

       FD  tordini-old.
       01 old-tor-rec.
           05 old-tor-chiave.
               10 old-tor-anno         PIC  9(4).
               10 old-tor-numero       PIC  9(8).
           05 old-tor-dati.
               10 old-tor-causale      PIC  x(4).
               10 old-tor-cod-cli      PIC  9(5).
               10 old-tor-prg-destino  PIC  9(5).
               10 old-tor-num-ord-cli  PIC  X(10).
               10 old-tor-data-ordine  PIC  9(8).
               10 old-tor-data-passaggio-ordine    PIC  9(8).
               10 old-tor-cod-agente   PIC  9(5).
               10 old-tor-cod-pagamento            PIC  x(3).
               10 old-tor-cod-ese-iva  PIC  x(3).
               10 old-tor-spostam-ric-ago          PIC  X(1).
                   88 old-tor-si-ric-ago VALUE IS "S". 
                   88 old-tor-no-ric-ago VALUE IS "N". 
               10 old-tor-spostam-ric-dic          PIC  X(1).
                   88 old-tor-si-ric-dic VALUE IS "S". 
                   88 old-tor-no-ric-dic VALUE IS "N". 
               10 old-tor-vettore      PIC  9(5).
               10 old-tor-note1        PIC  X(19).
               10 old-tor-data-note1   PIC  9(8).
               10 old-tor-note2        PIC  X(30).
               10 old-tor-note3        PIC  X(30).
               10 old-tor-note4        PIC  X(30).
               10 old-tor-invio        PIC  x.
                   88 old-tor-invio-manuale VALUE IS "M". 
                   88 old-tor-invio-postel VALUE IS "P". 
                   88 old-tor-invio-edi VALUE IS "E". 
               10 old-tor-bolla.
                   15 old-tor-anno-bolla   PIC  9(4).
                   15 old-tor-num-bolla    PIC  9(8).
                   15 old-tor-data-bolla   PIC  9(8).
                   15 old-tor-bolla-prenotata          PIC  x.
                       88 old-tor-bolla-si-prenotata VALUE IS "S". 
                       88 old-tor-bolla-no-prenotata VALUE IS "N". 
               10 old-tor-fattura.
                   15 old-tor-anno-fattura PIC  9(4).
                   15 old-tor-num-fattura  PIC  9(8).
                   15 old-tor-data-fattura PIC  9(8).
                   15 old-tor-num-prenot   PIC  9(8).
                   15 old-tor-fatt-prenotata           PIC  x.
                       88 old-tor-fatt-si-prenotata VALUE IS "S". 
                       88 old-tor-fatt-no-prenotata VALUE IS "N". 
               10 old-tor-mod-caricamento          PIC  X(1).
                   88 old-tor-manuale VALUE IS "M". 
                   88 old-tor-guidata VALUE IS "G". 
               10 old-tor-agg-contab   PIC  x.
                   88 old-tor-si-agg-contab VALUE IS "S". 
                   88 old-tor-no-agg-contab VALUE IS "N". 
               10 old-tor-tipo         PIC  x.
                   88 old-tor-fattura-manuale VALUE IS "M". 
                   88 old-tor-ordine VALUE IS "O". 
               10 old-tor-note         PIC  X(500).
      *(( XFD NAME = old-tor-contropartita_ ))
               10 old-tor-contropartita            PIC  X(8).
               10 old-tor-stato        PIC  X(1).
                   88 old-tor-attivo VALUE IS "A". 
                   88 old-tor-disattivo VALUE IS "D". 
                   88 old-tor-bloccato VALUE IS "B". 
               10 old-tor-dati-comuni.
                   15 old-tor-data-creazione           PIC  9(8).
                   15 old-tor-ora-creazione            PIC  9(8).
                   15 old-tor-utente-creazione         PIC  X(10).
                   15 old-tor-data-ultima-modifica     PIC  9(8).
                   15 old-tor-ora-ultima-modifica      PIC  9(8).
                   15 old-tor-utente-ultima-modifica   PIC  X(10).
               10 old-tor-vuoti.
                   15 old-tor-data-contab  PIC  9(8).
                   15 old-tor-promo        PIC  9.
                       88 old-tor-si-promo VALUE IS 1. 
                       88 old-tor-no-promo VALUE IS 0. 
                   15 old-tor-gest-plus    PIC  9(5).
                   15 old-tor-ritira-in-lubex          PIC  9.
                       88 old-tor-ritira-si VALUE IS 1. 
                       88 old-tor-ritira-no VALUE IS 0. 
                   15 old-tor-taglio       PIC  9(6).
                   15 old-tor-flag-rec-prezzi          PIC  9.
                       88 old-tor-rec-prezzi VALUE IS 1    WHEN SET TO 
           FALSE  0. 
                   15 old-tor-ordine-testa.
                       20 old-tor-anno-testa   PIC  9(4).
                       20 old-tor-num-testa    PIC  9(8).
                   15 old-tor-da-ordine    PIC  9.
                       88 old-tor-da-ordine-no VALUE IS 0. 
                       88 old-tor-da-ordine-si VALUE IS 1. 
                   15 old-tor-forn-reso    PIC  9(5).
                   15 old-tor-num-vuoto-3  PIC  9(5).
                   15 old-tor-esito-consegna           PIC  X(10).
                   15 old-tor-data-bolla-effettiva     PIC  9(8).
                   15 old-tor-tipo-evasione            PIC  X(1).
                       88 old-tor-ev-singola VALUE IS "S". 
                       88 old-tor-ev-manuale VALUE IS "M". 
                       88 old-tor-ev-immediata VALUE IS "I". 
                       88 old-tor-ev-normale VALUE IS "N". 
                       88 old-tor-ev-auto-trad VALUE IS "T". 
                       88 old-tor-ev-auto-gdo VALUE IS "G". 
                   15 old-tor-da-inviare   PIC  X(1).
                       88 old-tor-da-inviare-si VALUE IS "S". 
                       88 old-tor-da-inviare-no VALUE IS "N" " "    WHEN 
           SET TO FALSE  " ". 
                   15 old-tor-ora-contab   PIC  9(8).
                   15 old-tor-fattura-from.
                       20 old-tor-fattura-from-data        PIC  9(8).
                       20 old-tor-fattura-from-numero      PIC  x(8).
                   15 old-tor-gdo          PIC  X(5).
                   15 old-tor-tipocli      PIC  X(2).
                   15 old-tor-note-bolla-1 PIC  X(500).
                   15 old-tor-note-bolla-2 PIC  X(500).
                   15 old-tor-causale-orig PIC  X(4).
                   15 FILLER           PIC  X(1996).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tordini       pic X(2).
       77  status-tordini-old   pic X(2).

       77  CONT                 PIC 9(10).
       77  CONT-ED              PIC Z(10).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tordini
                     tordini-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file tordini?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "tordini"
                          x"22"
                          " in "
                          x"22"
                          "tordini-old"
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

           open input  tordini-old.
           open output tordini


           move low-value to old-tor-chiave.

           start tordini-old key not less old-tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tordini
                 tordini-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize tor-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move old-tor-anno                   to tor-anno                    
           move old-tor-numero                 to tor-numero                  
           move old-tor-causale                to tor-causale                 
           move old-tor-cod-cli                to tor-cod-cli                 
           move old-tor-prg-destino            to tor-prg-destino             
           move old-tor-num-ord-cli            to tor-num-ord-cli             
           move old-tor-data-ordine            to tor-data-ordine             
           move old-tor-data-passaggio-ordine  
           to tor-data-passaggio-ordine   
           move old-tor-cod-agente             to tor-cod-agente              
           move old-tor-cod-pagamento          to tor-cod-pagamento           
           move old-tor-cod-ese-iva            to tor-cod-ese-iva             
           move old-tor-spostam-ric-ago        to tor-spostam-ric-ago         
           move old-tor-spostam-ric-dic        to tor-spostam-ric-dic         
           move old-tor-vettore                to tor-vettore                 
           move old-tor-note1                  to tor-note1                   
           move old-tor-data-note1             to tor-data-note1              
           move old-tor-note2                  to tor-note2                   
           move old-tor-note3                  to tor-note3                   
           move old-tor-note4                  to tor-note4                   
           move old-tor-invio                  to tor-invio                   
           move old-tor-anno-bolla             to tor-anno-bolla              
           move old-tor-num-bolla              to tor-num-bolla               
           move old-tor-data-bolla             to tor-data-bolla              
           move old-tor-bolla-prenotata        to tor-bolla-prenotata         
           move old-tor-anno-fattura           to tor-anno-fattura            
           move old-tor-num-fattura            to tor-num-fattura             
           move old-tor-data-fattura           to tor-data-fattura            
           move old-tor-num-prenot             to tor-num-prenot              
           move old-tor-fatt-prenotata         to tor-fatt-prenotata          
           move old-tor-mod-caricamento        to tor-mod-caricamento            
           move old-tor-agg-contab             to tor-agg-contab              
           move old-tor-tipo                   to tor-tipo                    
           move old-tor-note                   to tor-note                    
           move old-tor-contropartita          to tor-contropartita           
           move old-tor-stato                  to tor-stato                   
           move old-tor-data-creazione         to tor-data-creazione          
           move old-tor-ora-creazione          to tor-ora-creazione           
           move old-tor-utente-creazione       to tor-utente-creazione        
           move old-tor-data-ultima-modifica   
           to tor-data-ultima-modifica    
           move old-tor-ora-ultima-modifica    
           to tor-ora-ultima-modifica     
           move old-tor-utente-ultima-modifica 
           to tor-utente-ultima-modifica  
           move old-tor-data-contab            to tor-data-contab             
           move old-tor-promo                  to tor-promo                   
           move old-tor-gest-plus              to tor-gest-plus               
           move old-tor-ritira-in-lubex        to tor-ritira-in-lubex         
           move old-tor-taglio                 to tor-taglio                  
           move old-tor-flag-rec-prezzi        to tor-flag-rec-prezzi         
           move old-tor-ordine-testa           to tor-ordine-testa.                     
           move old-tor-da-ordine              to tor-da-ordine               
           move old-tor-forn-reso              to tor-forn-reso               
           move old-tor-num-vuoto-3            to tor-num-vuoto-3             
           move old-tor-esito-consegna         to tor-esito-consegna          
           move old-tor-data-bolla-effettiva   
           to tor-data-bolla-effettiva    
           move old-tor-tipo-evasione          to tor-tipo-evasione           
           move old-tor-da-inviare             to tor-da-inviare              
           move old-tor-ora-contab             to tor-ora-contab              
           move old-tor-fattura-from-data      to tor-fattura-from-data       
           move old-tor-fattura-from-numero    
           to tor-fattura-from-numero     
           move old-tor-gdo                    to tor-gdo                     
           move old-tor-tipocli                to tor-tipocli                 
           move old-tor-note-bolla-1           to tor-note-bolla-1            
           move old-tor-note-bolla-2           to tor-note-bolla-2            
           move old-tor-causale-orig           to tor-causale-orig            

           write tor-rec.
