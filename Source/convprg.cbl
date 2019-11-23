       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      convprg.

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".
       SELECT progmag-old
           ASSIGN       TO  "progmag-old"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-progmag-old
           RECORD KEY   IS old-prg-chiave
           ALTERNATE RECORD KEY IS key01 = old-prg-cod-magazzino,
           old-prg-cod-articolo, old-prg-tipo-imballo, old-prg-peso
           WITH DUPLICATES .


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "progmag.fd".
       FD  progmag-old.
       01 OLD-prg-rec.
           05 OLD-prg-chiave.
               10 OLD-prg-cod-articolo PIC  9(6).
               10 OLD-prg-cod-magazzino            PIC  X(3).
               10 OLD-prg-tipo-imballo PIC  X(3).
               10 OLD-prg-peso         PIC  9(3)V9(3).
           05 OLD-prg-dati.
               10 OLD-prg-peso-utf     PIC  9(3)v9(3).
               10 OLD-prg-peso-non-utf PIC  9(3)v9(3).
               10 OLD-prg-sezione-dinamici.
                   15 OLD-prg-costo-ultimo PIC  S9(9)V9(2).
                   15 OLD-prg-costo-medio  PIC  S9(9)V9(2).
                   15 OLD-prg-scorta       PIC  S9(8).
                   15 OLD-prg-giacenza     PIC  S9(8).
                   15 OLD-prg-impegnato    PIC  S9(8).
                   15 OLD-prg-ordinato-1   PIC  S9(8).
                   15 OLD-prg-ordinato-2   PIC  S9(8).
                   15 OLD-prg-ordinato-3   PIC  S9(8).
                   15 OLD-prg-ordinato-4   PIC  S9(8).
                   15 OLD-prg-ordinato-5   PIC  S9(8).
                   15 OLD-prg-ordinato-6   PIC  S9(8).
               10 OLD-prg-sezione-consolidati.
                   15 OLD-prg-iniziali.
                       20 OLD-prg-ini-udm      PIC  S9(8).
                       20 OLD-prg-ini-kg       PIC  S9(9)V9(3).
                       20 OLD-prg-ini-valore   PIC  S9(9)V9(2).
                   15 OLD-prg-acquisti.
                       20 OLD-prg-acq-udm      PIC  S9(8).
                       20 OLD-prg-acq-kg       PIC  S9(9)V9(3).
                       20 OLD-prg-acq-valore   PIC  S9(9)V9(2).
                   15 OLD-prg-vendite.
                       20 OLD-prg-ven-udm      PIC  S9(8).
                       20 OLD-prg-ven-kg       PIC  S9(9)V9(3).
                       20 OLD-prg-ven-valore   PIC  S9(9)V9(2).
                   15 OLD-prg--variazioni.
                       20 OLD-prg-var-inv-udm  PIC  S9(8).
                       20 OLD-prg-var-inv-kg   PIC  S9(9)V9(3).
                       20 OLD-prg-var-inv-valore        PIC  S9(9)V9(2).
                   15 OLD-prg-resi-a-fornitori.
                       20 OLD-prg-resi-fornitori-udm      PIC  S9(8).
                       20 OLD-prg-resi-fornitori-kg      PIC S9(9)V9(3).
                       20 OLD-prg-resi-fornitori-valore PIC  S9(9)V9(2).
                   15 OLD-prg-resi-da-cliente.
                       20 OLD-prg-resi-da-cli-udm        PIC  S9(8).
                       20 OLD-prg-resi-da-cli-kg        PIC  S9(9)V9(3).
                       20 OLD-prg-resi-da-cli-valore    PIC  S9(9)V9(2).
                   15 OLD-prg-giacenza-periodo.
                       20 OLD-prg-giacenza-udm PIC  s9(8).
                       20 OLD-prg-giacenza-kg  PIC  S9(9)V9(3).
                   15 OLD-prg-entrate.
                       20 OLD-prg-udm-el       PIC  s9(8).
                       20 OLD-prg-kg-el        PIC  S9(9)V9(3).
                       20 OLD-prg-valore-el    PIC  S9(9)V9(2).
                   15 OLD-prg-uscite.
                       20 OLD-prg-udm-ul       PIC  s9(8).
                       20 OLD-prg-kg-ul        PIC  S9(9)V9(3).
                       20 OLD-prg-valore-ul    PIC  S9(9)V9(2).
               10 OLD-prg-giac-day     PIC  s9(8).
               10 OLD-prg-stato        PIC  X(1).
                   88 OLD-prg-attivo VALUE IS "A". 
                   88 OLD-prg-disattivo VALUE IS "D". 
                   88 OLD-prg-bloccato VALUE IS "B". 
               10 OLD-prg-dati-comuni.
                   15 OLD-prg-data-creazione           PIC  9(8).
                   15 OLD-prg-ora-creazione            PIC  9(8).
                   15 OLD-prg-utente-creazione         PIC  X(10).
                   15 OLD-prg-data-ultima-modifica     PIC  9(8).
                   15 OLD-prg-ora-ultima-modifica      PIC  9(8).
                   15 OLD-prg-utente-ultima-modifica   PIC  X(10).
               10 OLD-prg-vuoti.
                   15 OLD-prg-imp-GDO      PIC  s9(8).
                   15 OLD-prg-imp-TRAD     PIC  s9(7).
                   15 OLD-prg-imp-master   PIC  s9(8).
                   15 OLD-prg-num-vuoto-2  PIC  9(7).
                   15 OLD-prg-num-vuoto-3  PIC  9(15).
                   15 OLD-prg-alfa-vuoto-1 PIC  X(20).
                   15 OLD-prg-alfa-vuoto-2 PIC  X(20).
                   15 OLD-prg-alfa-vuoto-3 PIC  X(20).

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "acugui.def".
       77  status-progmag      pic X(2).
       77  status-progmag-old  pic X(2).

       77  cont                 PIC 9(6).
       77  cont-ed              PIC zzz.zz9.
       77  scelta               pic 9.

      ******************************************************************
       PROCEDURE DIVISION.     
           COPY "DECLXER1".
                     progmag
                     progmag-old
                     .
           COPY "DECLXER2".

       MAIN-PRG.           
           display message box 
                          "Confermi la conversione del file progmag?"
                          x"0D0A"
                          "Prima di procedere con la conversione"
                          x"0D0A"
                          "rinominare il file "
                          x"22"
                          "progmag"
                          x"22"
                          " in "
                          x"22"
                          "progmag-old"
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

           open input  progmag-old.
           open output progmag.


           move low-value to old-prg-chiave.

           start progmag-old key >= old-prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag-old next at end exit perform end-read

                    perform MUOVI-RECORD

                 end-perform
           end-start.

           close progmag
                 progmag-old.

           move cont   to cont-ed
           display message box "Convertiti " cont-ed " record.".


      ***---
       MUOVI-RECORD.
           add 1 to cont.
           initialize prg-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move OLD-prg-chiave           to prg-chiave.
           move OLD-prg-peso-utf         to prg-peso-utf.
           move OLD-prg-peso-non-utf     to prg-peso-non-utf.
           move OLD-prg-costo-ultimo     to prg-costo-ultimo.
           move OLD-prg-costo-medio      to prg-costo-medio.
           move OLD-prg-scorta           to prg-scorta.
           move OLD-prg-giacenza         to prg-giacenza.
           move OLD-prg-impegnato        to prg-impegnato.
           move OLD-prg-ordinato-1       to prg-ordinato-1.
           move OLD-prg-ordinato-2       to prg-ordinato-2.
           move OLD-prg-ordinato-3       to prg-ordinato-3.
           move OLD-prg-ordinato-4       to prg-ordinato-4.
           move OLD-prg-ordinato-5       to prg-ordinato-5.
           move OLD-prg-ordinato-6       to prg-ordinato-6.
           move OLD-prg-iniziali         to prg-iniziali.
           move OLD-prg-acquisti         to prg-acquisti.
           move OLD-prg-vendite          to prg-vendite.
           move OLD-prg--variazioni      to prg--variazioni.
           move OLD-prg-resi-a-fornitori to prg-resi-a-fornitori.
           move OLD-prg-resi-da-cliente  to prg-resi-da-cliente.
           move OLD-prg-giacenza-periodo to prg-giacenza-periodo.
           move OLD-prg-entrate          to prg-entrate.
           move OLD-prg-uscite           to prg-uscite.
           move OLD-prg-giac-day         to prg-giac-day.
           move OLD-prg-stato            to prg-stato.
           move OLD-prg-dati-comuni      to prg-dati-comuni.
           move OLD-prg-imp-GDO          to prg-imp-GDO.
           move OLD-prg-imp-TRAD         to prg-imp-TRAD.
           move OLD-prg-imp-master       to prg-imp-master.

           write prg-rec.
