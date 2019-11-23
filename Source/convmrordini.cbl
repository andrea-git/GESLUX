       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convmrordini.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mrordini.sl".
       SELECT mrordini-old
           ASSIGN       TO  "mrordini-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS MANUAL WITH ROLLBACK 
           FILE STATUS  IS STATUS-mrordini-old
           RECORD KEY   IS old-mro-chiave
           ALTERNATE RECORD KEY IS 
              old-mro-k-promo = old-mro-promo, old-mro-chiave
           ALTERNATE RECORD KEY IS 
              old-mro-k-articolo = old-mro-cod-articolo, old-mro-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS 
              old-mro-k-progr = old-mro-chiave-testa, old-mro-progr
           WITH DUPLICATES .
           copy "blister.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "mrordini.fd".
           
       FD  mrordini-old.
       01 old-mro-rec.
           05 old-mro-chiave.
               10 old-mro-chiave-testa.
                   15 old-mro-anno         PIC  9(4).
                   15 old-mro-numero       PIC  9(8).
               10 old-mro-riga         PIC  9(5).
           05 old-mro-dati.
               10 old-mro-cod-articolo PIC  9(6).
               10 old-mro-qta          PIC  9(8).
               10 old-mro-qta-e        PIC  9(8).
               10 old-mro-qta-b        PIC  9(8).
               10 old-mro-qta-f        PIC  9(8).
               10 old-mro-qta-omaggi   PIC  9(8).
               10 old-mro-prz-unitario PIC  9(9)v9(2).
               10 old-mro-imp-consumo  PIC  9(4)v9(2).
               10 old-mro-imp-cou-cobat            PIC  9(4)v9(2).
               10 old-mro-add-piombo   PIC  9(4)v9(2).
               10 old-mro-imponib-merce            PIC  9(9)v9(2).
               10 old-mro-perce-sconto PIC  9(2)v9(2).
               10 old-mro-omaggio      PIC  X(1).
                   88 old-mro-si-omaggio VALUE IS "S". 
                   88 old-mro-no-omaggio VALUE IS "N". 
               10 old-mro-peso-utf     PIC  9(3)v9(3).
               10 old-mro-peso-non-utf PIC  9(3)v9(3).
               10 old-mro-num-colli    PIC  9(5).
               10 old-mro-cod-imballo  PIC  X(3).
               10 old-mro-des-imballo  PIC  X(50).
               10 old-mro-qta-imballi  PIC  9(4).
               10 old-mro-cod-art-cli  PIC  X(15).
               10 old-mro-cod-iva      PIC  x(3).
               10 old-mro-prz-commle   PIC  9(9)v9(2).
               10 old-mro-prg-chiave.
                   15 old-mro-prg-cod-articolo         PIC  9(6).
                   15 old-mro-prg-cod-magazzino        PIC  X(3).
                   15 old-mro-prg-tipo-imballo         PIC  X(3).
                   15 old-mro-prg-peso     PIC  9(3)V9(3).
               10 old-mro-dati-blister.
                   15 old-mro-bli-codice   PIC  9(6).
                   15 old-mro-bli-perce    PIC  9(3)v99.
                   15 old-mro-blister      PIC  9.
                       88 old-mro-si-blister VALUE IS 1    WHEN SET TO 
           FALSE  0. 
               10 old-mro-promo        PIC  9(15).
               10 old-mro-flag-cancellato          PIC  9.
                   88 old-mro-cancellato VALUE IS 1    WHEN SET TO FALSE  
           0. 
               10 old-mro-prz-promo    PIC  9.
                   88 old-mro-si-prz-promo VALUE IS 1. 
                   88 old-mro-no-prz-promo VALUE IS 0. 
               10 old-mro-progr        PIC  9(5).
               10 old-mro-dati-comuni.
                   15 old-mro-data-creazione           PIC  9(8).
                   15 old-mro-ora-creazione            PIC  9(8).
                   15 old-mro-utente-creazione         PIC  X(10).
                   15 old-mro-data-ultima-modifica     PIC  9(8).
                   15 old-mro-ora-ultima-modifica      PIC  9(8).
                   15 old-mro-utente-ultima-modifica   PIC  X(10).
               10 old-mro-vuoti.
                   15 old-mro-evadi-dal    PIC  9(8).
                   15 old-mro-num-vuoto-1  PIC  9(10).
                   15 old-mro-num-vuoto-2  PIC  9(18).
                   15 old-mro-num-vuoto-3  PIC  9(18).
                   15 old-mro-alfa-vuoto-1 PIC  X(20).
                   15 old-mro-alfa-vuoto-2 PIC  X(20).
                   15 old-mro-alfa-vuoto-3 PIC  X(20).
           copy "blister.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-mrordini      pic X(2).
       77  status-mrordini-old  pic X(2).
       77  status-blister       pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     mrordini
                     mrordini-old
                     blister
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file mrordini?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "mrordini"
                          x"22"
                          " in "
                          x"22"
                          "mrordini-old"
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

           open input  mrordini-old.
           open output mrordini
           open input  blister.


           move low-value to old-mro-chiave.

           start mrordini-old key not less old-mro-chiave
              invalid 
                 continue
              not invalid
                 perform until 1 = 2
                    read mrordini-old next 
                       at end 
                          exit perform 
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close mrordini
                 mrordini-old
                 blister.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.

           move old-mro-anno                to mro-anno             
           move old-mro-numero              to mro-numero           
           move old-mro-riga                to mro-riga             
           move old-mro-cod-articolo        to mro-cod-articolo     
           move old-mro-qta                 to mro-qta              
           move old-mro-qta-e               to mro-qta-e            
           move old-mro-qta-b               to mro-qta-b            
           move old-mro-qta-f               to mro-qta-f            
           move old-mro-qta-omaggi          to mro-qta-omaggi       
           move old-mro-prz-unitario        to mro-prz-unitario     
           move old-mro-imp-consumo         to mro-imp-consumo      
           move old-mro-imp-cou-cobat       to mro-imp-cou-cobat    
           move old-mro-add-piombo          to mro-add-piombo       
           move old-mro-imponib-merce       to mro-imponib-merce    
           move old-mro-perce-sconto        to mro-perce-sconto     
           move old-mro-omaggio             to mro-omaggio          
           move old-mro-peso-utf            to mro-peso-utf         
           move old-mro-peso-non-utf        to mro-peso-non-utf     
           move old-mro-num-colli           to mro-num-colli        
           move old-mro-cod-imballo         to mro-cod-imballo      
           move old-mro-des-imballo         to mro-des-imballo      
           move old-mro-qta-imballi         to mro-qta-imballi      
           move old-mro-cod-art-cli         to mro-cod-art-cli      
           move old-mro-cod-iva             to mro-cod-iva          
           move old-mro-prz-commle          to mro-prz-commle       
           move old-mro-prg-cod-articolo    to mro-prg-cod-articolo    
           move old-mro-prg-cod-magazzino   to mro-prg-cod-magazzino      
           move old-mro-prg-tipo-imballo    to mro-prg-tipo-imballo    
           move old-mro-prg-peso            to mro-prg-peso         
           move old-mro-bli-codice          to mro-bli-codice       
           move old-mro-bli-perce           to mro-bli-perce        
           move old-mro-blister             to mro-blister          
           move old-mro-promo               to mro-promo            
           move old-mro-flag-cancellato     to mro-flag-cancellato  
           move old-mro-prz-promo           to mro-prz-promo        
           move old-mro-progr               to mro-progr            
           move old-mro-dati-comuni         to mro-dati-comuni      
           move old-mro-evadi-dal           to mro-evadi-dal        
                                            
                                            
           if mro-si-blister
              perform VAL-QTA-BLI
           else
              move zero   to mro-bli-qta
           end-if
                                            
           move zero   to mro-num-vuoto-1
                          mro-num-vuoto-2
                          mro-num-vuoto-3
           move space  to mro-alfa-vuoto
                   
           write mro-rec.                      
       
      ***---
       VAL-QTA-BLI.
      *    è la prima riga di un blister, quindi metto in linea il blister
      *    e azzero il contatore delle righe del blister
           if mro-qta-imballi not = zero
              move zero   to idx
              move mro-bli-codice  to bli-codice
              read blister
                 invalid
                    initialize bli-tab-componenti
              end-read
           end-if
           add 1 to idx

                      
           move bli-el-qta(idx) to mro-bli-qta
      *    per sicurezza controllo di aver valorizzato in modo giusto i 
      *    blister
           if mro-bli-qta = zero
              move 1   to mro-bli-qta
           end-if.
              
