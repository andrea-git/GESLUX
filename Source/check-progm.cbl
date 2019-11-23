       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-progm.
       AUTHOR.                          Andrea.
       REMARKS. Controllo della corrispondenza dei valori ( per il 
                calcolo del costo mp ) 
                tra il padre e la somma di tutti i suoi figli.
                Controllo che sul padre peso UTF + NON UTF sia = standard
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "progmag.sl".
           copy "progmag.sl"
                replacing ==progmag== by ==progmag2==,
                   ==STATUS-progmag== by ==STATUS-progmag2==.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "progmag.fd".
           copy "progmag.fd"
                replacing ==progmag== by ==progmag2==,
                   ==STATUS-progmag== by ==STATUS-progmag2==.

       WORKING-STORAGE SECTION.
       77  costo-mp              pic s9(7)v9(11).
       77  status-articoli       pic xx.
       77  status-progmag        pic xx.
       77  status-progmag2       pic xx.

       77  SaveArticolo          pic 9(6).
       77  articolo-edit         pic z(6).
       77  scelta                pic 9.

       01  totali.
         05 tot-ini-valore               pic s9(15)v999.
         05 tot-acq-valore               pic s9(15)v999.
         05 tot-valore-el                pic s9(15)v999.
         05 tot-var-inv-valore           pic s9(15)v999.
         05 tot-resi-fornitori-valore    pic s9(15)v999.
         05 tot-ini-udm                  pic s9(15)v999.
         05 tot-acq-udm                  pic s9(15)v999.
         05 tot-udm-el                   pic s9(15)v999.
         05 tot-var-inv-udm              pic s9(15)v999.
         05 tot-resi-fornitori-udm       pic s9(15)v999.
         05 tot-giacenza-udm             pic s9(15)v999.
         05 tot-costo-mp                 pic s9(7)v9(11).

       01 como-rec.
           05 chiave.
               10 cod-articolo PIC  9(6).
               10 cod-magazzino            PIC  X(3).
               10 tipo-imballo PIC  X(3).
               10 peso         PIC  9(3)V9(3).
           05 dati.
               10 peso-utf     PIC  9(3)v9(3).
               10 peso-non-utf PIC  9(3)v9(3).
               10 sezione-dinamici.
                   15 costo-ultimo PIC  S9(9)V9(2).
                   15 costo-medio  PIC  S9(9)V9(2).
                   15 scorta       PIC  S9(8).
                   15 giacenza     PIC  S9(8).
                   15 impegnato    PIC  S9(8).
                   15 ordinato     PIC  S9(8).
               10 sezione-consolidati.
                   15 iniziali.
                       20 ini-udm      PIC  S9(8).
                       20 ini-kg       PIC  S9(9)V9(3).
                       20 ini-valore   PIC  S9(9)V9(2).
                   15 acquisti.
                       20 acq-udm      PIC  S9(8).
                       20 acq-kg       PIC  S9(9)V9(3).
                       20 acq-valore   PIC  S9(9)V9(2).
                   15 vendite.
                       20 ven-udm      PIC  S9(8).
                       20 ven-kg       PIC  S9(9)V9(3).
                       20 ven-valore   PIC  S9(9)V9(2).
                   15 variazioni.
                       20 var-inv-udm  PIC  S9(8).
                       20 var-inv-kg   PIC  S9(9)V9(3).
                       20 var-inv-valore           PIC  S9(9)V9(2).
                   15 resi-a-fornitori.
                       20 resi-fornitori-udm       PIC  S9(8).
                       20 resi-fornitori-kg        PIC  S9(9)V9(3).
                       20 resi-fornitori-valore    PIC  S9(9)V9(2).
                   15 resi-da-cliente.
                       20 resi-da-cli-udm          PIC  S9(8).
                       20 resi-da-cli-kg           PIC  S9(9)V9(3).
                       20 resi-da-cli-valore       PIC  S9(9)V9(2).
                   15 giacenza-periodo.
                       20 giacenza-udm PIC  s9(8).
                       20 giacenza-kg  PIC  S9(9)V9(3).
                   15 entrate.
                       20 udm-el       PIC  s9(8).
                       20 kg-el        PIC  S9(9)V9(3).
                       20 valore-el    PIC  S9(9)V9(2).
                   15 uscite.
                       20 udm-ul       PIC  s9(8).
                       20 kg-ul        PIC  S9(9)V9(3).
                       20 valore-ul    PIC  S9(9)V9(2).
               10 giac-day     PIC  s9(8).
               10 stato        PIC  X(1).
                   88 attivo VALUE IS "A". 
                   88 disattivo VALUE IS "D". 
                   88 bloccato VALUE IS "B". 
               10 dati-comuni.
                   15 data-creazione           PIC  9(8).
                   15 ora-creazione            PIC  9(8).
                   15 utente-creazione         PIC  X(10).
                   15 data-ultima-modifica     PIC  9(8).
                   15 ora-ultima-modifica      PIC  9(8).
                   15 utente-ultima-modifica   PIC  X(10).
               10 vuoti.
                   15 num-vuoto-1  PIC  9(15).
                   15 num-vuoto-2  PIC  9(15).
                   15 num-vuoto-3  PIC  9(15).
                   15 alfa-vuoto-1 PIC  X(20).
                   15 alfa-vuoto-2 PIC  X(20).
                   15 alfa-vuoto-3 PIC  X(20).
           
       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       PROCEDURE DIVISION.
              
       DECLARATIVES.
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                display message box        "Impossibile procedere."
            x"0d0a""File progressivi di magazzino [PROGMAG] inesistente"
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [PROGMAG] mismatch size!"
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAG] Indexed file corrupt!"
                           icon 3
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message box        "Impossibile procedere."
            x"0d0a""File [ARTICOLI] inesistente"
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                           icon 3
           end-evaluate.
  
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok to true.
           move 0 to SaveArticolo.

      ***---
       OPEN-FILES.
           open input progmag progmag2 articoli.

      ***---
       ELABORAZIONE.
           move low-value to prg-rec of progmag.
           start progmag key is >= prg-chiave of progmag 
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read progmag next 
                      at end
                      perform CONFRONTA-SOMMA-CON-PADRE
                      exit perform 
                 end-read                 
                 if prg-cod-magazzino of progmag = spaces and
                    prg-tipo-imballo  of progmag = spaces and
                    prg-peso          of progmag = 0
                    if SaveArticolo not = 0
                       perform CONFRONTA-SOMMA-CON-PADRE
                       if errori exit perform end-if
                    end-if
                    move prg-cod-articolo of progmag to SaveArticolo
                    initialize totali replacing numeric data by zeroes
                 else
                    perform SOMMA-VALORI
                    move prg-rec of progmag to como-rec
                    perform CALCOLA-COSTO-MP
                    compute tot-costo-mp =
                            tot-costo-mp + costo-mp
                 end-if
              end-perform
           end-if.
           display message "SISTEMARE IL PESO?"
                      type 2
                    giving scelta
                    
           if scelta = 1
              perform SISTEMAZIONE-PESO-PADRE
           end-if.


      ***---
       SISTEMAZIONE-PESO-PADRE.
           close progmag.
           open i-o progmag.
           set tutto-ok to true.
           move low-value to prg-rec of progmag.
           start progmag key is >= prg-chiave of progmag 
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read progmag next at end exit perform end-read                 
                 if prg-cod-magazzino of progmag = spaces and
                    prg-tipo-imballo  of progmag = spaces and
                    prg-peso          of progmag = 0
                    move prg-cod-articolo of progmag to art-codice
                    read articoli no lock
                    if ( prg-peso-utf     of progmag +
                         prg-peso-non-utf of progmag ) not = 
                         art-peso-standard
                       if art-si-utf
                          move art-peso-standard
                            to prg-peso-utf of progmag
                       else
                          move art-peso-standard
                            to prg-peso-non-utf of progmag
                       end-if
                       rewrite prg-rec of progmag
                    end-if
                 end-if
              end-perform
           end-if.
           close progmag.

           open input progmag.
           set tutto-ok to true.
           move low-value to prg-rec of progmag.
           start progmag key is >= prg-chiave of progmag 
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read progmag next at end exit perform end-read                 
                 if prg-cod-magazzino of progmag = spaces and
                    prg-tipo-imballo  of progmag = spaces and
                    prg-peso          of progmag = 0
                    move prg-cod-articolo of progmag to art-codice
                    read articoli no lock
                    if ( prg-peso-utf     of progmag +
                         prg-peso-non-utf of progmag ) not = 
                         art-peso-standard
                       display message "ARTICOLO " art-codice
                    end-if
                 end-if
              end-perform
           end-if.

      ***---
       CONFRONTA-SOMMA-CON-PADRE.
           move SaveArticolo to articolo-edit.
           perform LEGGI-PADRE.
           perform CALCOLA-COSTO-MP.
           if tot-costo-mp not = costo-mp
              display message "INCONGRUENZA SU COSTO MP: "
                              articolo-edit
                title "ERRORE - ELABORAZIONE TERMINATA!"
              set errori to true
           end-if.

      ***---
       LEGGI-PADRE.
           initialize prg-chiave of progmag2 
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.
           move SaveArticolo to prg-cod-articolo of progmag2.
           read progmag2 no lock invalid continue end-read.
           if prg-ini-valore            of progmag2 not =
              tot-ini-valore            or
              prg-acq-valore            of progmag2 not =
              tot-acq-valore            or
              prg-valore-el             of progmag2 not =
              tot-valore-el             or
              prg-var-inv-valore        of progmag2 not =
              tot-var-inv-valore        or
              prg-resi-fornitori-valore of progmag2 not =
              tot-resi-fornitori-valore or
              prg-ini-udm               of progmag2 not =
              tot-ini-udm               or
              prg-acq-udm               of progmag2 not =
              tot-acq-udm               or
              prg-udm-el                of progmag2 not =
              tot-udm-el                or
              prg-var-inv-udm           of progmag2 not =
              tot-var-inv-udm           or
              prg-resi-fornitori-udm    of progmag2 not =
              tot-resi-fornitori-udm    or
              prg-giacenza-udm          of progmag2 not =
              tot-giacenza-udm
              move SaveArticolo         to articolo-edit
              display message "INCONGRUENZA SU ARTICOLO: "
                              articolo-edit
                        title "ERRORE - ELABORAZIONE TERMINATA!"
              set errori to true
           end-if.
           move prg-rec of progmag2 to como-rec.

      ***---
       SOMMA-VALORI.
           add prg-ini-valore            of progmag 
            to tot-ini-valore.
           add prg-acq-valore            of progmag 
            to tot-acq-valore.
           add prg-valore-el             of progmag 
            to tot-valore-el.
           add prg-var-inv-valore        of progmag 
            to tot-var-inv-valore.
           add prg-resi-fornitori-valore of progmag 
            to tot-resi-fornitori-valore.
           add prg-ini-udm               of progmag 
            to tot-ini-udm.
           add prg-acq-udm               of progmag 
            to tot-acq-udm.
           add prg-udm-el                of progmag 
            to tot-udm-el.
           add prg-var-inv-udm           of progmag 
            to tot-var-inv-udm.
           add prg-resi-fornitori-udm    of progmag 
            to tot-resi-fornitori-udm.
           add prg-giacenza-udm          of progmag 
            to tot-giacenza-udm.      
           move prg-rec of progmag       to como-rec.
            
      ***---
       CALCOLA-COSTO-MP.
           if ( ini-valore     +
                acq-valore     +
                valore-el      +
                var-inv-valore +
                resi-fornitori-valore) = 0
              move 0 to costo-mp
           else
              if ( ini-udm     + 
                   acq-udm     +
                   udm-el      +
                   var-inv-udm +
                   resi-fornitori-udm ) = 0
                 move 0 to costo-mp
              else
                 compute costo-mp =
                       ( ini-valore + 
                         acq-valore +
                         valore-el  +
                         var-inv-valore -
                         resi-fornitori-valore) /
                       ( ini-udm     + 
                         acq-udm     +
                         udm-el      +
                         var-inv-udm - 
                         resi-fornitori-udm )
              end-if
           end-if.
           compute costo-mp = costo-mp * giacenza-udm.

      ***---
       CLOSE-FILES.
           close progmag progmag2 articoli.

      ***---
       EXIT-PGM.
           goback.
