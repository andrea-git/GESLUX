       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convprgric.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmagric.sl".
       SELECT progmagric-old
           ASSIGN       TO  "progmagric-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-progmagric-old
           RECORD KEY   IS OLD-prr-chiave OF progmagric-old.


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "progmagric.fd".
       FD  progmagric-old.
       01 OLD-prr-rec.
           05 OLD-prr-chiave.
               10 OLD-prr-cod-articolo PIC  9(6).
               10 OLD-prr-cod-magazzino            PIC  X(3).
               10 OLD-prr-tipo-imballo PIC  X(3).
               10 OLD-prr-peso         PIC  9(3)V9(3).
           05 OLD-prr-dati.
               10 OLD-prr-peso-utf     PIC  9(3)v9(3).
               10 OLD-prr-peso-non-utf PIC  9(3)v9(3).
               10 OLD-prr-dinamici.
                   15 OLD-prr-costo-ultimo PIC  S9(9)V9(2).
                   15 OLD-prr-costo-inizio PIC  S9(9)V9(2).
                   15 OLD-prr-costo-medio  PIC  s9(8)V9(3).
               10 OLD-prr-consolidati.
                   15 OLD-prr-iniziali.
                       20 OLD-prr-ini-udm      PIC  S9(8).
                       20 OLD-prr-ini-kg       PIC  S9(9)V9(3).
                       20 OLD-prr-ini-valore   PIC  S9(9)V9(2).
                   15 OLD-prr-acquisti.
                       20 OLD-prr-acq-udm      PIC  S9(8).
                       20 OLD-prr-acq-kg       PIC  S9(9)V9(3).
                       20 OLD-prr-acq-valore   PIC  S9(9)V9(2).
                   15 OLD-prr-vendite.
                       20 OLD-prr-ven-udm      PIC  S9(8).
                       20 OLD-prr-ven-kg       PIC  S9(9)V9(3).
                       20 OLD-prr-ven-valore   PIC  S9(9)V9(2).
                   15 OLD-prr-variazioni.
                       20 OLD-prr-var-inv-udm  PIC  S9(8).
                       20 OLD-prr-var-inv-kg   PIC  S9(9)V9(3).
                       20 OLD-prr-var-inv-valore        PIC  S9(9)V9(2).
                   15 OLD-prr-resi-a-fornitori.
                       20 OLD-prr-resi-fornitori-udm       PIC  S9(8).
                       20 OLD-prr-resi-fornitori-kg     PIC  S9(9)V9(3).
                       20 OLD-prr-resi-fornitori-valore PIC  S9(9)V9(2).
                   15 OLD-prr-resi-da-cliente.
                       20 OLD-prr-resi-da-cli-udm          PIC  S9(8).
                       20 OLD-prr-resi-da-cli-kg        PIC  S9(9)V9(3).
                       20 OLD-prr-resi-da-cli-valore    PIC  S9(9)V9(2).
                   15 OLD-prr-giacenza-periodo.
                       20 OLD-prr-giacenza-udm PIC  s9(8).
                       20 OLD-prr-giacenza-kg  PIC  S9(9)V9(3).
                   15 OLD-prr-entrate.
                       20 OLD-prr-udm-el       PIC  s9(8).
                       20 OLD-prr-kg-el        PIC  S9(9)V9(3).
                       20 OLD-prr-valore-el    PIC  S9(9)V9(2).
                   15 OLD-prr-uscite.
                       20 OLD-prr-udm-ul       PIC  s9(8).
                       20 OLD-prr-kg-ul        PIC  S9(9)V9(3).
                       20 OLD-prr-valore-ul    PIC  S9(9)V9(2).
               10 OLD-prr-dati-comuni.
                   15 OLD-prr-data-creazione           PIC  9(8).
                   15 OLD-prr-ora-creazione            PIC  9(8).
                   15 OLD-prr-utente-creazione         PIC  X(10).
                   15 OLD-prr-data-ultima-modifica     PIC  9(8).
                   15 OLD-prr-ora-ultima-modifica      PIC  9(8).
                   15 OLD-prr-utente-ultima-modifica   PIC  X(10).
               10 OLD-prr-vuoti.
                   15 OLD-prr-prz-anagrafica           PIC  9(6)v99.
                   15 OLD-prr-num-vuoto-1  PIC  9(7).
                   15 OLD-prr-num-vuoto-2  PIC  9(15).
                   15 OLD-prr-num-vuoto-3  PIC  9(15).
                   15 OLD-prr-alfa-vuoto-1 PIC  X(20).
                   15 OLD-prr-alfa-vuoto-2 PIC  X(20).
                   15 OLD-prr-alfa-vuoto-3 PIC  X(20).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-progmagric      pic X(2).
       77  status-progmagric-old  pic X(2).

       77  cont                 PIC 9(6).
       77  cont-ed              PIC zzz.zz9.
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     progmagric
                     progmagric-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file progmagric?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "progmagric"
                          x"22"
                          " in "
                          x"22"
                          "progmagric-old"
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
           move 0 to cont

           open input  progmagric-old.
           open output progmagric.


           move low-value to old-prr-chiave.

           start progmagric-old key >= old-prr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmagric-old next 
                         at end exit perform 
                    end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close progmagric
                 progmagric-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize prr-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move OLD-prr-chiave            to prr-chiave          
           move OLD-prr-peso-utf          to prr-peso-utf        
           move OLD-prr-peso-non-utf      to prr-peso-non-utf    
           move OLD-prr-dinamici          to prr-dinamici        
           move OLD-prr-iniziali          to prr-iniziali        
           move OLD-prr-acquisti          to prr-acquisti        
           move OLD-prr-vendite           to prr-vendite         
           move OLD-prr-variazioni        to prr-variazioni      
           move OLD-prr-resi-a-fornitori  to prr-resi-a-fornitori
           move OLD-prr-resi-da-cliente   to prr-resi-da-cliente 
           move OLD-prr-giacenza-periodo  to prr-giacenza-periodo
           move OLD-prr-entrate           to prr-entrate         
           move OLD-prr-uscite            to prr-uscite          
           move OLD-prr-dati-comuni       to prr-dati-comuni     
           move OLD-prr-prz-anagrafica    to prr-prz-anagrafica  

           write prr-rec.
