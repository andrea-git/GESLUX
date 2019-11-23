       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convtparamge.
       REMARKS. conversione dalla versione 2.6 verso la 2.6.1
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tparamge.sl".
       SELECT tparamge-old
           ASSIGN       TO  "tparamge-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tparamge-old
           RECORD KEY   IS old-tge-chiave.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tparamge.fd".
       FD  tparamge-old.
       01 old-tge-rec.
           05 old-tge-chiave.
               10 old-tge-codice       PIC  X(2).
           05 old-tge-dati.
               10 old-tge-anno         PIC  9(4).
               10 old-tge-data-consolid-progmag    PIC  9(8).
               10 old-tge-cod-iva-std  PIC  x(3).
               10 old-tge-cod-iva-omag PIC  x(3).
               10 old-tge-cod-pag      PIC  x(3).
               10 old-tge-cliente-corrisp          PIC  9(6).
               10 old-tge-causale-corrisp          PIC  x(4).
               10 old-tge-causale-ordini-std       PIC  x(4).
               10 old-tge-listino-promo            PIC  9(4).
               10 old-tge-altre-date.
      *Ultimo ricalcolo programma ordini
                   15 old-tge-data-promo   PIC  9(8).
                   15 old-tge-data-2       PIC  9(8).
                   15 old-tge-data-3       PIC  9(8).
                   15 old-tge-data-4       PIC  9(8).
               10 old-tge-num-vuoto-1  PIC  9(2).
               10 old-tge-forn-corrisp PIC  9(6).
               10 old-tge-dati-comuni.
                   15 old-tge-data-creazione           PIC  9(8).
                   15 old-tge-ora-creazione            PIC  9(8).
                   15 old-tge-utente-creazione         PIC  X(10).
                   15 old-tge-data-ultima-modifica     PIC  9(8).
                   15 old-tge-ora-ultima-modifica      PIC  9(8).
                   15 old-tge-utente-ultima-modifica   PIC  X(10).
               10 old-tge-vuoti.
                   15 old-tge-perce-arrot-bancale      PIC  9(3)v99.
                   15 old-tge-perce-fido   PIC  s9(3)v99.
                   15 old-tge-trasp-italy  PIC  9(6)v999.
                   15 old-tge-trasp-estero PIC  9(6)v999.
                   15 old-tge-limite-kg    PIC  9(3)v999.
                   15 old-tge-vettore-std  PIC  9(5).
      *(( XFD NAME = old-tge-num-vuoto-3_1_ ))
                   15 old-tge-gg-plus-consegna         PIC  9(3).
      *(( XFD NAME = old-tge-num-vuoto-3_1_ ))
                   15 old-tge-num-vuoto-3  PIC  9(1).
                   15 old-tge-ttipocli-privato         PIC  x(2).
                   15 old-tge-causale-ord-forn         PIC  x(4).
                   15 old-tge-causale-omag PIC  x(4).
                   15 old-tge-reg-PILE     PIC  x(20).
                   15 old-tge-data-consolid-effet      PIC  9(8).
                   15 old-tge-gg-scadenza-vol          PIC  s9(3).
                   15 old-tge-gg-evadi-parziale        PIC  9(3).
                   15 old-tge-gg-cons-max  PIC  9(3).
                   15 old-tge-causale-rotta-s          PIC  X(4).
                   15 old-tge-causale-rotta-c          PIC  X(4).
                   15 old-tge-alfa-vuoto-3 PIC  X(5).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tparamge      pic X(2).
       77  status-tparamge-old  pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tparamge
                     tparamge-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file tparamge?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "tparamge"
                          x"22"
                          " in "
                          x"22"
                          "tparamge-old"
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

           open input tparamge-old.
           open output tparamge


           move low-value to old-tge-chiave.

           start tparamge-old key not less old-tge-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tparamge-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tparamge
                 tparamge-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.

           move old-tge-codice                to tge-codice              
           move old-tge-anno                  to tge-anno                
           move old-tge-data-consolid-progmag 
                                            to tge-data-consolid-progmag 
           move old-tge-cod-iva-std           to tge-cod-iva-std         
           move old-tge-cod-iva-omag          to tge-cod-iva-omag        
           move old-tge-cod-pag               to tge-cod-pag             
           move old-tge-cliente-corrisp       to tge-cliente-corrisp      
           move old-tge-causale-corrisp       to tge-causale-corrisp      
           move old-tge-causale-ordini-std    to tge-causale-ordini-std   
           move old-tge-listino-promo         to tge-listino-promo        
           move old-tge-data-promo            to tge-data-promo          
           move old-tge-data-2                to tge-data-2              
           move old-tge-data-3                to tge-data-3              
           move old-tge-data-4                to tge-data-4              
           move old-tge-num-vuoto-1           to tge-num-vuoto-1         
           move old-tge-forn-corrisp          to tge-forn-corrisp        
           move old-tge-dati-comuni           to tge-dati-comuni         
           move old-tge-perce-arrot-bancale   to tge-perce-arrot-bancale  
           move old-tge-perce-fido            to tge-perce-fido          
           move old-tge-trasp-italy           to tge-trasp-italy         
           move old-tge-trasp-estero          to tge-trasp-estero        
           move old-tge-limite-kg             to tge-limite-kg           
           move old-tge-vettore-std           to tge-vettore-std         
           move old-tge-gg-plus-consegna      to tge-gg-plus-consegna     
           move old-tge-ttipocli-privato      to tge-ttipocli-privato     
           move old-tge-causale-ord-forn      to tge-causale-ord-forn     
           move old-tge-causale-omag          to tge-causale-omag        
           move old-tge-reg-PILE              to tge-reg-PILE            
           move old-tge-data-consolid-effet   to tge-data-consolid-effet  
           move old-tge-gg-scadenza-vol       to tge-gg-scadenza-vol      
           move old-tge-gg-evadi-parziale     to tge-gg-evadi-parziale    
           move old-tge-gg-cons-max           to tge-gg-cons-max         
           move old-tge-causale-rotta-s       to tge-causale-rotta-s      
           move old-tge-causale-rotta-c       to tge-causale-rotta-c      


           move zero   to tge-ora-contab
                          tge-data-contab
                          tge-ora-consolid-effet


           write tge-rec. 
                          
                          
                          
                          
                          
