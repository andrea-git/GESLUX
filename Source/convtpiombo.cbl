       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convtpiopmbo.
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tpiombo.sl".

       SELECT tpiombo-old
           ASSIGN       TO  "tpiombo-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-old-tpiombo
           RECORD KEY   IS old-tpb-chiave.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tpiombo.fd".

      *
      *
       FD  tpiombo-OLD.
       01 OLD-tpb-rec.
           05 OLD-tpb-chiave.
               10 OLD-tpb-marca        PIC  9(4).
               10 OLD-tpb-data         PIC  9(8).
           05 OLD-tpb-dati.
               10 OLD-tpb-perce-auto   PIC  9(3)v9(3).
               10 OLD-tpb-perce-moto   PIC  9(3)v9(3).
               10 OLD-tpb-dati-comuni.
                   15 OLD-tpb-data-creazione           PIC  9(8).
                   15 OLD-tpb-ora-creazione            PIC  9(8).
                   15 OLD-tpb-utente-creazione         PIC  X(10).
                   15 OLD-tpb-data-modifica            PIC  9(8).
                   15 OLD-tpb-ora-modifica PIC  9(8).
                   15 OLD-tpb-utente-modifica          PIC  X(10).
               10 OLD-tpb-vuoti.
                   15 OLD-tpb-euro-ampere  PIC  9(2)v9999.
                   15 OLD-tpb-num-vuoto-1  PIC  9(7).
                   15 OLD-tpb-num-vuoto-2  PIC  9(15).
                   15 OLD-tpb-num-vuoto-3  PIC  9(15).
               10 OLD-tpb-scaglioni-pb.
                   15 OLD-tpb-pb-sca-1-da  PIC  9(3).
                   15 OLD-tpb-pb-sca-1-a   PIC  9(3).
                   15 OLD-tpb-pb-sca-1-euro            PIC  9(3)v9(3).
                   15 OLD-tpb-pb-sca-2-da  PIC  9(3).
                   15 OLD-tpb-pb-sca-2-a   PIC  9(3).
                   15 OLD-tpb-pb-sca-2-euro            PIC  9(3)v9(3).
                   15 OLD-tpb-pb-sca-3-da  PIC  9(3).
                   15 OLD-tpb-pb-sca-3-a   PIC  9(3).
                   15 OLD-tpb-pb-sca-3-euro            PIC  9(3)v9(3).
                   15 OLD-tpb-pb-sca-4-da  PIC  9(3).
                   15 OLD-tpb-pb-sca-4-a   PIC  9(3).
                   15 OLD-tpb-pb-sca-4-euro            PIC  9(3)v9(3).
                   15 OLD-tpb-pb-sca-5-da  PIC  9(3).
                   15 OLD-tpb-pb-sca-5-a   PIC  9(3).
                   15 OLD-tpb-pb-sca-5-euro            PIC  9(3)v9(3).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tpiombo       pic x(2).
       77  status-old-tpiombo   pic x(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tpiombo
                     tpiombo-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file trasporti?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "tpiombo"
                          x"22"
                          " in "
                          x"22"
                          "tpiombo-old"
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

           open input  tpiombo-old.
           open output tpiombo


           move low-value to old-tpb-chiave.

           start tpiombo-old key >= old-tpb-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tpiombo-old next at end exit perform end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tpiombo
                 tpiombo-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
      *
           initialize tpb-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move OLD-tpb-marca            to tpb-marca            
           move OLD-tpb-data             to tpb-data             
           move OLD-tpb-perce-auto       to tpb-perce-auto       
           move OLD-tpb-perce-moto       to tpb-perce-moto       
           move OLD-tpb-data-creazione   to tpb-data-creazione   
           move OLD-tpb-ora-creazione    to tpb-ora-creazione    
           move OLD-tpb-utente-creazione to tpb-utente-creazione 
           move OLD-tpb-data-modifica    to tpb-data-modifica    
           move OLD-tpb-ora-modifica     to tpb-ora-modifica     
           move OLD-tpb-utente-modifica  to tpb-utente-modifica  
           move OLD-tpb-euro-ampere      to tpb-euro-ampere      
           move OLD-tpb-num-vuoto-1      to tpb-num-vuoto-1      
           move OLD-tpb-num-vuoto-2      to tpb-num-vuoto-2      
           move OLD-tpb-num-vuoto-3      to tpb-num-vuoto-3      
           move OLD-tpb-pb-sca-1-da      to tpb-pb-sca-1-da      
           move OLD-tpb-pb-sca-1-a       to tpb-pb-sca-1-a       
           move OLD-tpb-pb-sca-1-euro    to tpb-pb-sca-1-euro    
           move OLD-tpb-pb-sca-2-da      to tpb-pb-sca-2-da      
           move OLD-tpb-pb-sca-2-a       to tpb-pb-sca-2-a       
           move OLD-tpb-pb-sca-2-euro    to tpb-pb-sca-2-euro    
           move OLD-tpb-pb-sca-3-da      to tpb-pb-sca-3-da      
           move OLD-tpb-pb-sca-3-a       to tpb-pb-sca-3-a       
           move OLD-tpb-pb-sca-3-euro    to tpb-pb-sca-3-euro    
           move OLD-tpb-pb-sca-4-da      to tpb-pb-sca-4-da      
           move OLD-tpb-pb-sca-4-a       to tpb-pb-sca-4-a       
           move OLD-tpb-pb-sca-4-euro    to tpb-pb-sca-4-euro    
           move OLD-tpb-pb-sca-5-da      to tpb-pb-sca-5-da      
           move OLD-tpb-pb-sca-5-a       to tpb-pb-sca-5-a       
           move OLD-tpb-pb-sca-5-euro    to tpb-pb-sca-5-euro    
                                      
           write tpb-rec.                      
