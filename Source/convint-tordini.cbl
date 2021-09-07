       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convint-tordini.
       remarks. Per ampliamento numero ordine cliente
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "int-tordini.sl".

       SELECT int-tordini-old
           ASSIGN       TO  "int-tordini-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL
           FILE STATUS  IS status-int-tordini-old
           RECORD KEY   IS old-int-tor-chiave
           ALTERNATE RECORD KEY IS old-int-k-causale = 
           old-int-tor-causale, 
           old-int-tor-anno, old-int-tor-numero
           ALTERNATE RECORD KEY IS old-int-k1 = old-int-tor-cod-cli, 
           old-int-tor-prg-destino, old-int-tor-anno, old-int-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k2 = 
           old-int-tor-data-passaggio-ordine, 
           old-int-tor-anno, old-int-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k-bolla = 
           old-int-tor-anno-bolla, 
           old-int-tor-num-bolla
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k3 = old-int-tor-anno-bolla, 
           old-int-tor-data-bolla, old-int-tor-num-bolla, 
           old-int-tor-bolla-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k-fattura = 
           old-int-tor-anno-fattura, old-int-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k4 = 
           old-int-tor-anno-fattura, 
           old-int-tor-data-fattura, old-int-tor-num-fattura, 
           old-int-tor-num-prenot, old-int-tor-fatt-prenotata
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k-contab = 
           old-int-tor-agg-contab, 
           old-int-tor-anno-fattura, old-int-tor-num-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k-tipo = old-int-tor-tipo, 
           old-int-tor-chiave
           ALTERNATE RECORD KEY IS old-int-k-data = 
           old-int-tor-data-creazione, 
           old-int-tor-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k-agfatt = 
           old-int-tor-anno-fattura, 
           old-int-tor-data-fattura, old-int-tor-num-fattura, 
           old-int-tor-num-prenot, old-int-tor-fatt-prenotata, 
           old-int-tor-chiave
           ALTERNATE RECORD KEY IS old-int-k-stbolle = 
           old-int-tor-anno-bolla, 
           old-int-tor-data-bolla, old-int-tor-num-bolla, 
           old-int-tor-bolla-prenotata, old-int-tor-chiave
           ALTERNATE RECORD KEY IS old-int-k-andamento-data = 
           old-int-tor-agg-contab, old-int-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k-andamento-cliente = 
           old-int-tor-cod-cli, old-int-tor-agg-contab, 
           old-int-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k-andamento-clides = 
           old-int-tor-cod-cli, old-int-tor-prg-destino, 
           old-int-tor-agg-contab, 
           old-int-tor-data-fattura
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-int-k-promo = old-int-tor-stato, 
           old-int-tor-promo, old-int-tor-data-ordine, 
           old-int-tor-numero, 
           old-int-tor-cod-cli, old-int-tor-prg-destino
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "int-tordini.fd".

       FD  int-tordini-old.
       01 old-int-tor-rec.
           05 old-int-tor-chiave.
               10 old-int-tor-anno     PIC  9(4).
               10 old-int-tor-numero   PIC  9(8).
           05 old-int-tor-dati.
               10 old-int-tor-causale  PIC  x(4).
               10 old-int-tor-cod-cli  PIC  9(5).
               10 old-int-tor-prg-destino          PIC  9(5).
               10 old-int-tor-num-ord-cli          PIC  X(10).
               10 old-int-tor-data-ordine          PIC  9(8).
               10 old-int-tor-data-passaggio-ordine          PIC  9(8).
               10 old-int-tor-cod-agente           PIC  9(5).
               10 old-int-tor-cod-pagamento        PIC  x(3).
               10 old-int-tor-cod-ese-iva          PIC  x(3).
               10 old-int-tor-spostam-ric-ago      PIC  X(1).
                   88 old-int-tor-si-ric-ago VALUE IS "S". 
                   88 old-int-tor-no-ric-ago VALUE IS "N". 
               10 old-int-tor-spostam-ric-dic      PIC  X(1).
                   88 old-int-tor-si-ric-dic VALUE IS "S". 
                   88 old-int-tor-no-ric-dic VALUE IS "N". 
               10 old-int-tor-vettore  PIC  9(5).
               10 old-int-tor-note1    PIC  X(19).
               10 old-int-tor-data-note1           PIC  9(8).
               10 old-int-tor-note2    PIC  X(30).
               10 old-int-tor-note3    PIC  X(30).
               10 old-int-tor-note4    PIC  X(30).
               10 old-int-tor-invio    PIC  x.
                   88 old-int-tor-invio-manuale VALUE IS "M". 
                   88 old-int-tor-invio-postel VALUE IS "P". 
               10 old-int-tor-bolla.
                   15 old-int-tor-anno-bolla           PIC  9(4).
                   15 old-int-tor-num-bolla            PIC  9(8).
                   15 old-int-tor-data-bolla           PIC  9(8).
                   15 old-int-tor-bolla-prenotata      PIC  x.
                       88 old-int-tor-bolla-si-prenotata VALUE IS "S". 
                       88 old-int-tor-bolla-no-prenotata VALUE IS "N". 
               10 old-int-tor-fattura.
                   15 old-int-tor-anno-fattura         PIC  9(4).
                   15 old-int-tor-num-fattura          PIC  9(8).
                   15 old-int-tor-data-fattura         PIC  9(8).
                   15 old-int-tor-num-prenot           PIC  9(8).
                   15 old-int-tor-fatt-prenotata       PIC  x.
                       88 old-int-tor-fatt-si-prenotata VALUE IS "S". 
                       88 old-int-tor-fatt-no-prenotata VALUE IS "N". 
               10 old-int-tor-mod-caricamento      PIC  X(1).
                   88 old-int-tor-manuale VALUE IS "M". 
                   88 old-int-tor-guidata VALUE IS "G". 
               10 old-int-tor-agg-contab           PIC  x.
                   88 old-int-tor-si-agg-contab VALUE IS "S". 
                   88 old-int-tor-no-agg-contab VALUE IS "N". 
               10 old-int-tor-tipo     PIC  x.
                   88 old-int-tor-fattura-manuale VALUE IS "M". 
                   88 old-int-tor-ordine VALUE IS "O". 
               10 old-int-tor-note     PIC  X(500).
      *(( XFD NAME = tor-contropartita_ ))
               10 old-int-tor-contropartita        PIC  X(8).
               10 old-int-tor-stato    PIC  X(1).
                   88 old-int-tor-attivo VALUE IS "A". 
                   88 old-int-tor-disattivo VALUE IS "D". 
                   88 old-int-tor-bloccato VALUE IS "B". 
               10 old-int-tor-dati-comuni.
                   15 old-int-tor-data-creazione       PIC  9(8).
                   15 old-int-tor-ora-creazione        PIC  9(8).
                   15 old-int-tor-utente-creazione     PIC  X(10).
                   15 old-int-tor-data-ultima-modifica PIC  9(8).
                   15 old-int-tor-ora-ultima-modifica  PIC  9(8).
                   15 old-int-tor-utente-ultima-modifica           PIC  
           X(10).
               10 old-int-tor-vuoti.
                   15 old-int-tor-data-contab          PIC  9(8).
                   15 old-int-tor-promo    PIC  9.
                       88 old-int-tor-si-promo VALUE IS 1. 
                       88 old-int-tor-no-promo VALUE IS 0. 
                   15 old-int-tor-num-vuoto-1          PIC  9(6).
                   15 old-int-tor-num-vuoto-2          PIC  9(15).
                   15 old-int-tor-num-vuoto-3          PIC  9(15).
                   15 old-int-tor-alfa-vuoto-1         PIC  X(20).
                   15 old-int-tor-alfa-vuoto-2         PIC  X(20).
                   15 old-int-tor-alfa-vuoto-3         PIC  X(20).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-int-tordini       pic X(2).
       77  status-int-tordini-old   pic X(2).

       77  CONT                 PIC 9(10).
       77  CONT-ED              PIC Z(10).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     int-tordini
                     int-tordini-old
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
                          "int-tordini"
                          x"22"
                          " in "
                          x"22"
                          "int-tordini-old"
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

           open input  int-tordini-old.
           open output int-tordini


           move low-value to old-int-tor-chiave.

           start int-tordini-old key not less old-int-tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read int-tordini-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close int-tordini
                 int-tordini-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize int-tor-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.           
                                                                              
           move old-int-tor-anno                  to int-tor-anno                   
           move old-int-tor-numero                to int-tor-numero                 
           move old-int-tor-causale               to int-tor-causale                
           move old-int-tor-cod-cli               to int-tor-cod-cli                
           move old-int-tor-prg-destino           to int-tor-prg-destino            
           move old-int-tor-num-ord-cli           to int-tor-num-ord-cli            
           move old-int-tor-data-ordine           to int-tor-data-ordine            
           move old-int-tor-data-passaggio-ordine 
             to int-tor-data-passaggio-ordine  
           move old-int-tor-cod-agente       to int-tor-cod-agente             
           move old-int-tor-cod-pagamento    to int-tor-cod-pagamento          
           move old-int-tor-cod-ese-iva      to int-tor-cod-ese-iva            
           move old-int-tor-spostam-ric-ago  to int-tor-spostam-ric-ago        
           move old-int-tor-spostam-ric-dic  to int-tor-spostam-ric-dic        
           move old-int-tor-vettore          to int-tor-vettore                
           move old-int-tor-note1            to int-tor-note1                  
           move old-int-tor-data-note1       to int-tor-data-note1             
           move old-int-tor-note2            to int-tor-note2                  
           move old-int-tor-note3            to int-tor-note3                  
           move old-int-tor-note4            to int-tor-note4                  
           move old-int-tor-invio            to int-tor-invio                  
           move old-int-tor-anno-bolla       to int-tor-anno-bolla             
           move old-int-tor-num-bolla        to int-tor-num-bolla              
           move old-int-tor-data-bolla       to int-tor-data-bolla             
           move old-int-tor-bolla-prenotata  to int-tor-bolla-prenotata        
           move old-int-tor-anno-fattura     to int-tor-anno-fattura           
           move old-int-tor-num-fattura      to int-tor-num-fattura            
           move old-int-tor-data-fattura     to int-tor-data-fattura           
           move old-int-tor-num-prenot       to int-tor-num-prenot             
           move old-int-tor-fatt-prenotata   to int-tor-fatt-prenotata         
           move old-int-tor-mod-caricamento  to int-tor-mod-caricamento        
           move old-int-tor-agg-contab       to int-tor-agg-contab             
           move old-int-tor-tipo             to int-tor-tipo                   
           move old-int-tor-note             to int-tor-note                   
           move old-int-tor-contropartita    to int-tor-contropartita          
           move old-int-tor-stato            to int-tor-stato                  
           move old-int-tor-data-creazione   to int-tor-data-creazione         
           move old-int-tor-ora-creazione    to int-tor-ora-creazione          
           move old-int-tor-utente-creazione to int-tor-utente-creazione       
           move old-int-tor-data-ultima-modifica  
             to int-tor-data-ultima-modifica   
           move old-int-tor-ora-ultima-modifica   
             to int-tor-ora-ultima-modifica    
           move old-int-tor-utente-ultima-modifica
             to int-tor-utente-ultima-modifica   
           move old-int-tor-data-contab     to int-tor-data-contab            
           move old-int-tor-promo           to int-tor-promo                  
           move old-int-tor-num-vuoto-1     to int-tor-num-vuoto-1            
           move old-int-tor-num-vuoto-2     to int-tor-num-vuoto-2            
           move old-int-tor-num-vuoto-3     to int-tor-num-vuoto-3            
           move old-int-tor-alfa-vuoto-1    to int-tor-alfa-vuoto-1           
           move old-int-tor-alfa-vuoto-2    to int-tor-alfa-vuoto-2           
           move old-int-tor-alfa-vuoto-3    to int-tor-alfa-vuoto-3           

           write int-tor-rec.
