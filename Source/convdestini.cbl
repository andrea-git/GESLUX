       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convdestini.
       remarks. Per aggiunta note bolla
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "destini.fd". 

       FD  destini-old.
       01 old-des-rec.
           05 old-des-chiave.
               10 old-des-codice       PIC  9(5).
               10 old-des-prog         PIC  9(5).
           05 old-des-dati.
               10 old-des-ragsoc-1     PIC  x(40).
               10 old-des-ragsoc-2     PIC  x(40).
               10 old-des-indirizzo    PIC  x(40).
               10 old-des-cap          PIC  x(5).
               10 old-des-localita     PIC  x(35).
               10 old-des-prov         PIC  x(2).
               10 old-des-nazione      PIC  x(3).
               10 old-des-telef-1      PIC  x(15).
               10 old-des-telef-2      PIC  x(15).
               10 old-des-fax          PIC  X(15).
               10 old-des-mail         PIC  X(30).
               10 old-des-referente    PIC  x(30).
               10 old-des-vettore      PIC  9(5).
               10 old-des-deposito-UTF PIC  x.
               10 old-des-superamento-500          PIC  x.
               10 old-des-stato        PIC  x.
                   88 old-des-attivo VALUE IS "A". 
                   88 old-des-disattivo VALUE IS "D". 
                   88 old-des-bloccato VALUE IS "B". 
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
                       88 old-des-si-piva-dupl VALUE IS 1. 
                       88 old-des-no-piva-dupl VALUE IS 0. 
                   15 old-des-cod-ditta    PIC  x(15).
                   15 old-des-tipo-art     PIC  9.
                       88 old-des-tipo-art-diretti VALUE IS 1. 
                       88 old-des-tipo-art-gruppi VALUE IS 2. 
                       88 old-des-tipo-art-specialist VALUE IS 3. 
                       88 old-des-tipo-art-DO VALUE IS 4. 
                       88 old-des-tipo-art-GDA VALUE IS 5. 
                       88 old-des-tipo-art-GDS VALUE IS 6. 
                       88 old-des-tipo-art-ESTERO VALUE IS 7. 
                   15 old-des-num-vuoto-2  PIC  9(1).
                   15 old-des-saldi-banco  PIC  9(1).
                       88 old-des-saldi-banco-si VALUE IS 1. 
                       88 old-des-saldi-banco-no VALUE IS 0. 
                   15 old-des-saldi-promo  PIC  9(1).
                       88 old-des-saldi-promo-si VALUE IS 1. 
                       88 old-des-saldi-promo-no VALUE IS 0. 
                   15 old-des-escludi-evadi-tutto      PIC  9(1).
                       88 old-des-escludi-evadi-tutto-si VALUE IS 1. 
                       88 old-des-escludi-evadi-tutto-no VALUE IS 0. 
                   15 old-des-accorpa-master           PIC  9(1).
                       88 old-des-accorpa-master-si VALUE IS 1. 
                       88 old-des-accorpa-master-no VALUE IS 0. 
                   15 old-des-num-vuoto-3  PIC  9(12).
                   15 old-des-invio-fatt   PIC  x.
                       88 old-des-si-invio VALUE IS "S". 
                       88 old-des-no-invio VALUE IS "N". 
                   15 old-des-note-bolla-1 PIC  X(500).
                   15 old-des-note-bolla-2 PIC  X(500).
                   15 old-des-CIG          PIC  X(15).
      *(( XFD NAME = old-des-mail2_1_2 ))
                   15 FILLER           PIC  X(1985).


       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-destini       pic X(2).
       77  status-destini-old   pic X(2).

       77  CONT                 PIC 9(6).
       77  CONT-ED              PIC Z(6).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     destini
                     destini-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file destini?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "destini"
                          x"22"
                          " in "
                          x"22"
                          "destini-old"
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

           open input  destini-old.
           open output destini


           move low-value to old-des-chiave.

           start destini-old key not less old-des-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read destini-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close destini
                 destini-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize des-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.         

           move old-des-codice       
             to des-codice                  
           move old-des-prog                                               
             to des-prog                    
           move old-des-ragsoc-1                                           
             to des-ragsoc-1                
           move old-des-ragsoc-2                                           
             to des-ragsoc-2                
           move old-des-indirizzo                                          
             to des-indirizzo               
           move old-des-cap                                                
             to des-cap                     
           move old-des-localita                                           
             to des-localita                
           move old-des-prov                                               
             to des-prov                    
           move old-des-nazione                                            
             to des-nazione                 
           move old-des-telef-1                                            
             to des-telef-1                 
           move old-des-telef-2                                            
             to des-telef-2                 
           move old-des-fax                                                
             to des-fax                     
           move old-des-mail                                               
             to des-mail                    
           move old-des-referente                                          
             to des-referente               
           move old-des-vettore                                            
             to des-vettore                 
           move old-des-deposito-UTF                                       
             to des-deposito-UTF            
           move old-des-superamento-500                                    
             to des-superamento-500         
           move old-des-stato                                              
             to des-stato                   
           move old-des-data-creazione                                     
             to des-data-creazione          
           move old-des-ora-creazione                                      
             to des-ora-creazione           
           move old-des-utente-creazione                                   
             to des-utente-creazione        
           move old-des-data-ultima-modifica                                 
             to des-data-ultima-modifica    
           move old-des-ora-ultima-modifica                                  
             to des-ora-ultima-modifica     
           move old-des-utente-ultima-modifica                             
             to des-utente-ultima-modifica  
           move old-des-piva                  
             to des-piva                    
           move old-des-piva-dupl                                          
             to des-piva-dupl               
           move old-des-cod-ditta                                          
             to des-cod-ditta               
           move old-des-tipo-art                                           
             to des-tipo-art                
           move old-des-num-vuoto-2                                        
             to des-num-vuoto-2             
           move old-des-saldi-banco                                        
             to des-saldi-banco             
           move old-des-saldi-promo                                        
             to des-saldi-promo             
           move old-des-escludi-evadi-tutto                                  
             to des-escludi-evadi-tutto     
           move old-des-accorpa-master                                       
             to des-accorpa-master          
           move old-des-num-vuoto-3                                          
             to des-num-vuoto-3             
           move old-des-invio-fatt                                           
             to des-invio-fatt              
           move old-des-note-bolla-1                                         
             to des-note-bolla-1            
           move old-des-note-bolla-2                                         
             to des-note-bolla-2            
           move old-des-CIG                                                  
             to des-CIG                     

           write des-rec.      
