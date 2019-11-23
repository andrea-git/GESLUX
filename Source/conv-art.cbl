       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      conv-art.
       AUTHOR.                          Andrea.
  
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "progmag.sl".
           copy "rordini.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "progmag.fd".
           copy "rordini.fd".

       working-storage section.
       01 save-prg-rec.
           05 save-prg-chiave.
               10 save-prg-cod-articolo PIC  9(6).
               10 save-prg-cod-magazzino            PIC  X(3).
               10 save-prg-tipo-imballo PIC  X(3).
               10 save-prg-peso         PIC  9(3)V9(3).
           05 save-prg-dati.
               10 save-prg-peso-utf     PIC  9(3)v9(3).
               10 save-prg-peso-non-utf PIC  9(3)v9(3).
               10 save-prg-sezione-dinamici.
                   15 save-prg-costo-ultimo PIC  S9(9)V9(2).
                   15 save-prg-costo-medio  PIC  S9(9)V9(2).
                   15 save-prg-scorta       PIC  S9(8).
                   15 save-prg-giacenza     PIC  S9(8).
                   15 save-prg-impegnato    PIC  S9(8).
                   15 save-prg-ordinato     PIC  S9(8).
               10 save-prg-sezione-consolidati.
                   15 save-prg-iniziali.
                       20 save-prg-ini-udm      PIC  S9(8).
                       20 save-prg-ini-kg       PIC  S9(9)V9(3).
                       20 save-prg-ini-valore   PIC  S9(9)V9(2).
                   15 save-prg-acquisti.
                       20 save-prg-acq-udm      PIC  S9(8).
                       20 save-prg-acq-kg       PIC  S9(9)V9(3).
                       20 save-prg-acq-valore   PIC  S9(9)V9(2).
                   15 save-prg-vendite.
                       20 save-prg-ven-udm      PIC  S9(8).
                       20 save-prg-ven-kg       PIC  S9(9)V9(3).
                       20 save-prg-ven-valore   PIC  S9(9)V9(2).
                   15 save-prg--variazioni.
                       20 save-prg-var-inv-udm  PIC  S9(8).
                       20 save-prg-var-inv-kg   PIC  S9(9)V9(3).
                       20 save-prg-var-inv-valore        PIC S9(9)V9(2).
                   15 save-prg-resi-a-fornitori.
                       20 save-prg-resi-fornitori-udm    PIC S9(8).
                       20 save-prg-resi-fornitori-kg     PIC S9(9)V9(3).
                       20 save-prg-resi-fornitori-valore PIC S9(9)V9(2).
                   15 save-prg-resi-da-cliente.
                       20 save-prg-resi-da-cli-udm       PIC S9(8).
                       20 save-prg-resi-da-cli-kg        PIC S9(9)V9(3).
                       20 save-prg-resi-da-cli-valore    PIC S9(9)V9(2).
                   15 save-prg-giacenza-periodo.
                       20 save-prg-giacenza-udm PIC  s9(8).
                       20 save-prg-giacenza-kg  PIC  S9(9)V9(3).
                   15 save-prg-entrate.
                       20 save-prg-udm-el       PIC  s9(8).
                       20 save-prg-kg-el        PIC  S9(9)V9(3).
                       20 save-prg-valore-el    PIC  S9(9)V9(2).
                   15 save-prg-uscite.
                       20 save-prg-udm-ul       PIC  s9(8).
                       20 save-prg-kg-ul        PIC  S9(9)V9(3).
                       20 save-prg-valore-ul    PIC  S9(9)V9(2).
               10 save-prg-giac-day     PIC  s9(8).
               10 save-prg-stato        PIC  X(1).
                   88 save-prg-attivo VALUE IS "A". 
                   88 save-prg-disattivo VALUE IS "D". 
                   88 save-prg-bloccato VALUE IS "B". 
               10 save-prg-dati-comuni.
                   15 save-prg-data-creazione           PIC  9(8).
                   15 save-prg-ora-creazione            PIC  9(8).
                   15 save-prg-utente-creazione         PIC  X(10).
                   15 save-prg-data-ultima-modifica     PIC  9(8).
                   15 save-prg-ora-ultima-modifica      PIC  9(8).
                   15 save-prg-utente-ultima-modifica   PIC  X(10).
               10 save-prg-vuoti.
                   15 save-prg-num-vuoto-1  PIC  9(15).
                   15 save-prg-num-vuoto-2  PIC  9(15).
                   15 save-prg-num-vuoto-3  PIC  9(15).
                   15 save-prg-alfa-vuoto-1 PIC  X(20).
                   15 save-prg-alfa-vuoto-2 PIC  X(20).
                   15 save-prg-alfa-vuoto-3 PIC  X(20).

       77  status-rordini  pic xx.
       77  status-progmag  pic xx.
       77  status-articoli pic xx.

       77  peso  pic 9(10)v999.
       77  tot   pic 9(10).

       procedure division.
      ***---
       MAIN.
           open i-o articoli progmag rordini.

           move 0 to tot.
           display message "ELABORAZIONE ARTICOLI".
           move low-value to art-chiave.
           start articoli key is >= art-chiave.
           perform until 1 = 2
              read articoli next at end exit perform end-read
              compute peso = art-peso-utf + art-peso-non-utf
              if art-imballo-standard = "FNO" and peso > 100
                 move "FTO" to art-imballo-standard
                 rewrite art-rec
                 add 1 to tot
              end-if
           end-perform.
           display message "ELABORATI " tot " ARTICOLI".

           move 0 to tot.
           display message "ELABORAZIONE RORDINI".
           move low-value to ror-chiave.
           start rordini key is >= ror-chiave.
           perform until 1 = 2
              read rordini next at end exit perform end-read
              compute peso = ror-peso-utf + ror-peso-non-utf
              if ror-cod-imballo      = "FNO" or
                 ror-prg-tipo-imballo = "FNO"
                 if peso > 100
                    move "FTO" to ror-cod-imballo ror-prg-tipo-imballo
                    rewrite ror-rec
                    add 1 to tot
                 end-if
              end-if
           end-perform. 
           display message "ELABORATI " tot " RORDINI".
                                             
           move 0 to tot.
           display message "ELABORAZIONE PROGMAG".
           move low-value to prg-chiave.
           start progmag key is >= prg-chiave.
           perform until 1 = 2
              read progmag next at end exit perform end-read
              compute peso = prg-peso-utf + prg-peso-non-utf
              if prg-tipo-imballo = "FNO" and peso > 100
                 move prg-rec to save-prg-rec
                 delete progmag record
                 move save-prg-rec to prg-rec
                 move "FTO" to prg-tipo-imballo
                 write prg-rec invalid continue end-write  
                 add 1 to tot
              end-if
           end-perform.
           display message "ELABORATI " tot " PROGMAG".
                 
           close    articoli progmag rordini.

           goback.
