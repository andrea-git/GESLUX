       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      contcontat.
       REMARKS. conversione dalla versione 2.4 verso la 2.5 
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tcontat.sl".

       SELECT tcontat-old
           ASSIGN       TO  "tcontat-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tcontat-old
           RECORD KEY   IS old-con-chiave.        

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tcontat.fd".
           
       FD  tcontat-old.
       01 old-con-rec.
           05 old-con-chiave.
               10 old-con-anno         PIC  9(4).
           05 old-con-dati.
               10 old-con-num-ordine   PIC  9(8).
               10 old-con-ult-num-movim            PIC  9(8).
               10 old-con-num-bolle-gdo            PIC  9(8).
               10 old-con-ult-stampa-bolle-gdo     PIC  9(8).
               10 old-con-num-fatt     PIC  9(8).
               10 old-con-ult-stampa-fatt          PIC  9(8).
               10 old-con-ult-num-pren PIC  9(8).
               10 old-con-ult-num-nc   PIC  9(8).     
               10 old-con-ult-num-nc-fisc          PIC  9(8).
               10 old-con-ult-stampa-nc            PIC  9(8).
               10 old-con-ult-num-postel           PIC  9(8).
               10 old-con-ult-num-nc-postel        PIC  9(8).
               10 old-con-dati-comuni.
                   15 old-con-data-creazione           PIC  9(8).
                   15 old-con-ora-creazione            PIC  9(8).
                   15 old-con-utente-creazione         PIC  x(10).
                   15 old-con-data-ultima-modifica     PIC  9(8).
                   15 old-con-ora-ultima-modifica      PIC  9(8).
                   15 old-con-utente-ultima-modifica   PIC  x(10).
               10 old-con-vuoti.
                   15 old-con-num-bozza    PIC  9(8).
                   15 old-con-num-cont     PIC  9(7).
                   15 old-con-num-ord-forn PIC  9(8).
                   15 old-con-num-evasione PIC  9(7).
                   15 old-con-num-bolle-mv PIC  9(8).
                   15 old-con-ult-stampa-bolle-mv      PIC  9(8).
      *(( XFD NAME = old-con-num-ordine_1 ))
                   15 old-con-num-ordine-m PIC  9(8).
                   15 old-con-num-bolle-at PIC  9(8).
                   15 old-con-ult-stampa-bolle-at      PIC  9(8).
                   15 old-con-ult-num-EDI  PIC  9(8).
                   15 old-con-ult-num-nc-EDI           PIC  9(8).
                   15 old-con-ult-num-bolle-EDI-1      PIC  9(8).
                   15 old-con-ult-num-bolle-EDI-2      PIC  9(8).
                   15 FILLER           PIC  x(3).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-tcontat      pic X(2).
       77  status-tcontat-old  pic X(2).

       77  CONT                 PIC 9(4).
       77  CONT-ED              PIC Z(4).
       77  scelta               pic 9.
       77  idx                  pic 9(3).

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     tcontat
                     tcontat-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file tcontat?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "tcontat"
                          x"22"
                          " in "
                          x"22"
                          "tcontat-old"
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

           open input  tcontat-old.
           open output tcontat.


           move low-value to old-con-chiave.

           start tcontat-old key >= old-con-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tcontat-old next 
                         at end exit perform
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close tcontat
                 tcontat-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.                         
           
           move old-con-anno                 to con-anno               
           move old-con-num-ordine           to con-num-ordine         
           move old-con-ult-num-movim        to con-ult-num-movim      
           move old-con-num-bolle-gdo        to con-num-bolle-gdo      
           move old-con-ult-stampa-bolle-gdo to con-ult-stampa-bolle-gdo
           move old-con-num-fatt             to con-num-fatt           
           move old-con-ult-stampa-fatt      to con-ult-stampa-fatt    
           move old-con-ult-num-pren         to con-ult-num-pren       
           move old-con-ult-num-nc           to con-ult-num-nc         
           move old-con-ult-num-nc-fisc      to con-ult-num-nc-fisc     
           move old-con-ult-stampa-nc        to con-ult-stampa-nc       
           move old-con-ult-num-postel       to con-ult-num-postel      
           move old-con-ult-num-nc-postel    to con-ult-num-nc-postel   
           move old-con-data-creazione       to con-data-creazione      
           move old-con-ora-creazione        to con-ora-creazione       
           move old-con-utente-creazione     to con-utente-creazione    
           move old-con-data-ultima-modifica to con-data-ultima-modifica 
           move old-con-ora-ultima-modifica  to con-ora-ultima-modifica 
           move old-con-utente-ultima-modifica to 
                    con-utente-ultima-modifica 
           move old-con-num-bozza            to con-num-bozza          
           move old-con-num-cont             to con-num-cont           
           move old-con-num-ord-forn         to con-num-ord-forn       
           move old-con-num-evasione         to con-num-evasione       
           move old-con-num-bolle-mv         to con-num-bolle-mv       
           move old-con-ult-stampa-bolle-mv  to con-ult-stampa-bolle-mv 
           move old-con-num-ordine-m         to con-num-ordine-m       
           move old-con-num-bolle-at         to con-num-bolle-at       
           move old-con-ult-stampa-bolle-at  to con-ult-stampa-bolle-at 
           move old-con-ult-num-EDI          to con-ult-num-EDI        
           move old-con-ult-num-nc-EDI       to con-ult-num-nc-EDI      
           move old-con-ult-num-bolle-EDI-1  to con-ult-num-bolle-EDI-1 
           move old-con-ult-num-bolle-EDI-2  to con-ult-num-bolle-EDI-2 

           write con-rec.                      
