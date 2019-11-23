       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convdestinif.
       remarks. versione del 19/05/2009 per versione 2.2.1
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "destinif.sl".
       SELECT destinif-old
           ASSIGN       TO  "destinif-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-destinif-old
           RECORD KEY   IS old-desf-chiave
           ALTERNATE RECORD KEY IS old-K1 = old-desf-ragsoc-1, 
           old-desf-codice, 
           old-desf-prog
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS old-desf-k2 = old-desf-codice, 
           old-desf-ragsoc-1, old-desf-prog
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "destinif.fd".
       FD  destinif-old.
       01 old-desf-rec.
           05 old-desf-chiave.
               10 old-desf-codice      PIC  9(5).
               10 old-desf-prog        PIC  9(5).
           05 old-desf-dati.
               10 old-desf-ragsoc-1    PIC  x(40).
               10 old-desf-ragsoc-2    PIC  x(40).
               10 old-desf-indirizzo   PIC  x(40).
               10 old-desf-cap         PIC  x(5).
               10 old-desf-localita    PIC  x(35).
               10 old-desf-prov        PIC  x(2).
               10 old-desf-nazione     PIC  x(3).
               10 old-desf-telef-1     PIC  x(15).
               10 old-desf-telef-2     PIC  x(15).
               10 old-desf-fax         PIC  X(15).
               10 old-desf-mail        PIC  X(100).
               10 old-desf-referente   PIC  x(30).
               10 old-desf-vettore     PIC  9(5).
               10 old-desf-depostio-UTF            PIC  x.
               10 old-desf-superamento-500         PIC  x.
               10 old-desf-stato       PIC  x.
                   88 old-desf-attivo VALUE IS space. 
                   88 old-desf-disattivo VALUE IS "D". 
                   88 old-desf-bloccato VALUE IS "B". 
               10 old-desf-dati-ord.
                   15 old-desf-referente-ord           PIC  x(30).
                   15 old-desf-tel-dir-ref-ord         PIC  x(20).
                   15 old-desf-mail-ref-ord            PIC  x(100).
                   15 old-desf-perce-premi-fine-anno   PIC  9(3)v99.
                   15 FILLER           PIC  9(12).
               10 old-desf-dati-comuni.
                   15 old-desf-data-creazione          PIC  9(8).
                   15 old-desf-ora-creazione           PIC  9(8).
                   15 old-desf-utente-creazione        PIC  X(10).
                   15 old-desf-data-ultima-modifica    PIC  9(8).
                   15 old-desf-ora-ultima-modifica     PIC  9(8).
                   15 old-desf-utente-ultima-modifica  PIC  X(10).
               10 old-desf-vuoti.
                   15 old-desf-gg-consegna PIC  9(3).
                   15 old-desf-num-vuoto-1 PIC  9(12).
                   15 old-desf-num-vuoto-2 PIC  9(15).
                   15 old-desf-num-vuoto-3 PIC  9(15).
                   15 old-desf-alfa-vuoto-1            PIC  X(20).
                   15 old-desf-alfa-vuoto-2            PIC  X(20).
                   15 old-desf-alfa-vuoto-3            PIC  X(20).


       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-destinif      pic X(2).
       77  status-destinif-old  pic X(2).

       77  CONT                 PIC 9(3).
       77  CONT-ED              PIC Z(3).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     destinif
                     destinif-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file destinif?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "destinif"
                          x"22"
                          " in "
                          x"22"
                          "destinif-old"
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

           open input destinif-old.
           open output destinif


           move low-value to old-desf-chiave.

           start destinif-old key not less old-desf-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read destinif-old next
                       at end
                          exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close destinif
                 destinif-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.

           move old-desf-codice          to desf-codice               
           move old-desf-prog            to desf-prog                 
           move old-desf-ragsoc-1        to desf-ragsoc-1             
           move old-desf-ragsoc-2        to desf-ragsoc-2             
           move old-desf-indirizzo       to desf-indirizzo            
           move old-desf-cap             to desf-cap                  
           move old-desf-localita        to desf-localita             
           move old-desf-prov            to desf-prov                 
           move old-desf-nazione         to desf-nazione              
           move old-desf-telef-1         to desf-telef-1              
           move old-desf-telef-2         to desf-telef-2              
           move old-desf-fax             to desf-fax                  
           move old-desf-mail            to desf-mail                 
           move old-desf-referente       to desf-referente            
           move old-desf-vettore         to desf-vettore              
           move old-desf-depostio-UTF    to desf-depostio-UTF          
           move old-desf-superamento-500 to desf-superamento-500       
           move old-desf-stato           to desf-stato                
           move old-desf-referente-ord   to desf-referente-ord         
           move old-desf-tel-dir-ref-ord to desf-tel-dir-ref-ord       
           move old-desf-mail-ref-ord    to desf-mail-ref-ord         
           move old-desf-perce-premi-fine-anno 
                                         to desf-perce-premi-fine-anno  
           move old-desf-dati-comuni     to desf-dati-comuni          
           move old-desf-gg-consegna     to desf-gg-consegna          

           initialize desf-mail-ref-ord-cc
           write desf-rec.                   

