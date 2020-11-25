       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-prog-cons.
       AUTHOR.                          Andrea.
       REMARKS. Controllo della corrispondenza dei valori ( dei valori consolidati)
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
       77  status-articoli       pic xx.
       77  status-progmag        pic xx.
       77  status-progmag2       pic xx.

       77  SaveArticolo          pic 9(6).
       77  articolo-edit         pic z(6).
       77  scelta                pic 9.

       01  totali.      
         05 tot-prg-ini-udm                 pic s9(15).
         05 tot-prg-ini-kg                  pic s9(13)v9(3).
         05 tot-prg-ini-valore              pic s9(13)v9(2).
         05 tot-prg-acq-udm                 pic s9(15).
         05 tot-prg-acq-kg                  pic s9(13)v9(3).
         05 tot-prg-acq-valore              pic s9(13)v9(2).
         05 tot-prg-ven-udm                 pic s9(15).
         05 tot-prg-ven-kg                  pic s9(13)v9(3).
         05 tot-prg-ven-valore              pic s9(13)v9(2).            
         05 tot-prg-var-inv-udm             pic s9(15).
         05 tot-prg-var-inv-kg              pic s9(13)v9(3).            
         05 tot-prg-var-inv-valore          pic s9(13)v9(2).
         05 tot-prg-resi-fornitori-udm      pic s9(15).     
         05 tot-prg-resi-fornitori-kg       pic s9(13)v9(3).
         05 tot-prg-resi-fornitori-valore   pic s9(13)v9(2).
         05 tot-prg-resi-da-cli-udm         pic s9(15).
         05 tot-prg-resi-da-cli-kg          pic s9(13)v9(3).
         05 tot-prg-resi-da-cli-valore      pic s9(13)v9(2).
         05 tot-prg-giacenza-udm            pic s9(15).
         05 tot-prg-giacenza-kg             pic s9(13)v9(3).
         05 tot-prg-udm-el                  pic s9(15).
         05 tot-prg-kg-el                   pic s9(13)v9(3).
         05 tot-prg-valore-el               pic s9(13)v9(2).
         05 tot-prg-udm-el2                 pic s9(15).
         05 tot-prg-kg-el2                  pic s9(13)v9(3).
         05 tot-prg-valore-el2              pic s9(13)v9(2).
         05 tot-prg-udm-ul                  pic s9(15).
         05 tot-prg-kg-ul                   pic s9(13)v9(3).
         05 tot-prg-valore-ul               pic s9(13)v9(2).
         05 tot-prg-udm-ul2                 pic s9(15).
         05 tot-prg-kg-ul2                  pic s9(13)v9(3).
         05 tot-prg-valore-ul2              pic s9(13)v9(2).
         05 tot-giacenza     pic s9(15).
         05 tot-impegnato    pic s9(15).
         05 tot-imp-master   pic s9(15).
         05 tot-imp-gdo      pic s9(15).
         05 tot-imp-trad     pic s9(15).
         05 tot-ordinato-1   pic s9(15).
         05 tot-ordinato-2   pic s9(15).
         05 tot-ordinato-3   pic s9(15).
         05 tot-ordinato-4   pic s9(15).
         05 tot-ordinato-5   pic s9(15).
         05 tot-ordinato-6   pic s9(15).

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
           open input progmag2 articoli.
           open i-o progmag.

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
                    end-if
                    move prg-cod-articolo of progmag to SaveArticolo
                    initialize totali replacing numeric data by zeroes
                 else
                    perform SOMMA-VALORI
                    move prg-rec of progmag to como-rec
                 end-if
              end-perform
           end-if.
      *****     display message "SISTEMARE IL PESO?"
      *****                type 2
      *****              giving scelta
      *****              
      *****     if scelta = 1
      *****        perform SISTEMAZIONE-PESO-PADRE
      *****     end-if.


      ********---
      ***** SISTEMAZIONE-PESO-PADRE.
      *****     close progmag.
      *****     open i-o progmag.
      *****     set tutto-ok to true.
      *****     move low-value to prg-rec of progmag.
      *****     start progmag key is >= prg-chiave of progmag 
      *****           invalid set errori to true
      *****     end-start.
      *****     if tutto-ok
      *****        perform until 1 = 2
      *****           read progmag next at end exit perform end-read                 
      *****           if prg-cod-magazzino of progmag = spaces and
      *****              prg-tipo-imballo  of progmag = spaces and
      *****              prg-peso          of progmag = 0
      *****              move prg-cod-articolo of progmag to art-codice
      *****              read articoli no lock
      *****              if ( prg-peso-utf     of progmag +
      *****                   prg-peso-non-utf of progmag ) not = 
      *****                   art-peso-standard
      *****                 if art-si-utf
      *****                    move art-peso-standard
      *****                      to prg-peso-utf of progmag
      *****                 else
      *****                    move art-peso-standard
      *****                      to prg-peso-non-utf of progmag
      *****                 end-if
      *****                 rewrite prg-rec of progmag
      *****              end-if
      *****           end-if
      *****        end-perform
      *****     end-if.
      *****     close progmag.
      *****
      *****     open input progmag.
      *****     set tutto-ok to true.
      *****     move low-value to prg-rec of progmag.
      *****     start progmag key is >= prg-chiave of progmag 
      *****           invalid set errori to true
      *****     end-start.
      *****     if tutto-ok
      *****        perform until 1 = 2
      *****           read progmag next at end exit perform end-read                 
      *****           if prg-cod-magazzino of progmag = spaces and
      *****              prg-tipo-imballo  of progmag = spaces and
      *****              prg-peso          of progmag = 0
      *****              move prg-cod-articolo of progmag to art-codice
      *****              read articoli no lock
      *****              if ( prg-peso-utf     of progmag +
      *****                   prg-peso-non-utf of progmag ) not = 
      *****                   art-peso-standard
      *****                 display message "ARTICOLO " art-codice
      *****              end-if
      *****           end-if
      *****        end-perform
      *****     end-if.

      ***---
       CONFRONTA-SOMMA-CON-PADRE.
           move SaveArticolo to articolo-edit.
           perform LEGGI-PADRE. 

      ***---
       LEGGI-PADRE.
           initialize prg-chiave of progmag2 
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.
           move SaveArticolo to prg-cod-articolo of progmag2.
           read progmag2 no lock invalid continue end-read.
           if prg-ini-udm                 of progmag2 not = 
              tot-prg-ini-udm             or
              prg-ini-kg                  of progmag2 not = 
              tot-prg-ini-kg              or 
              prg-ini-valore              of progmag2 not = 
              tot-prg-ini-valore          or 
              prg-acq-udm                 of progmag2 not = 
              tot-prg-acq-udm             or 
              prg-acq-kg                  of progmag2 not = 
              tot-prg-acq-kg              or 
              prg-acq-valore              of progmag2 not = 
              tot-prg-acq-valore          or 
              prg-ven-udm                 of progmag2 not = 
              tot-prg-ven-udm             or 
              prg-ven-kg                  of progmag2 not = 
              tot-prg-ven-kg              or 
              prg-ven-valore              of progmag2 not = 
              tot-prg-ven-valore          or 
              prg-var-inv-udm             of progmag2 not = 
              tot-prg-var-inv-udm         or 
              prg-var-inv-kg              of progmag2 not = 
              tot-prg-var-inv-kg          or 
              prg-var-inv-valore          of progmag2 not = 
              tot-prg-var-inv-valore      or 
              prg-resi-fornitori-udm      of progmag2 not = 
              tot-prg-resi-fornitori-udm  or 
              prg-resi-fornitori-kg       of progmag2 not = 
              tot-prg-resi-fornitori-kg   or 
              prg-resi-fornitori-valore   of progmag2 not = 
              tot-prg-resi-fornitori-valore or
              prg-resi-da-cli-udm         of progmag2 not = 
              tot-prg-resi-da-cli-udm     or 
              prg-resi-da-cli-kg          of progmag2 not = 
              tot-prg-resi-da-cli-kg      or 
              prg-resi-da-cli-valore      of progmag2 not = 
              tot-prg-resi-da-cli-valore  or 
              prg-giacenza-udm            of progmag2 not = 
              tot-prg-giacenza-udm        or 
              prg-giacenza-kg             of progmag2 not = 
              tot-prg-giacenza-kg         or 
              prg-udm-el                  of progmag2 not = 
              tot-prg-udm-el              or 
              prg-kg-el                   of progmag2 not = 
              tot-prg-kg-el               or 
              prg-valore-el               of progmag2 not = 
              tot-prg-valore-el           or 
              prg-udm-el2                 of progmag2 not = 
              tot-prg-udm-el2             or 
              prg-kg-el2                  of progmag2 not = 
              tot-prg-kg-el2              or 
              prg-valore-el2              of progmag2 not = 
              tot-prg-valore-el2          or 
              prg-udm-ul                  of progmag2 not = 
              tot-prg-udm-ul              or 
              prg-kg-ul                   of progmag2 not = 
              tot-prg-kg-ul               or 
              prg-valore-ul               of progmag2 not = 
              tot-prg-valore-ul           or 
              prg-udm-ul2                 of progmag2 not = 
              tot-prg-udm-ul2             or 
              prg-kg-ul2                  of progmag2 not = 
              tot-prg-kg-ul2              or 
              prg-valore-ul2              of progmag2 not = 
              tot-prg-valore-ul2          or
              prg-giacenza              of progmag2 not =
              tot-giacenza           or
              prg-impegnato           of progmag2 not =
              tot-impegnato            or
              prg-imp-master             of progmag2 not =
              tot-imp-master             or
              prg-imp-gdo        of progmag2 not =
              tot-imp-gdo        or
              prg-imp-trad of progmag2 not =
              tot-imp-trad or
              prg-ordinato-1               of progmag2 not =
              tot-ordinato-1               or     
              prg-ordinato-2               of progmag2 not =
              tot-ordinato-2               or
              prg-ordinato-3               of progmag2 not =
              tot-ordinato-3               or
              prg-ordinato-4               of progmag2 not =
              tot-ordinato-4               or
              prg-ordinato-5               of progmag2 not =
              tot-ordinato-5               or
              prg-ordinato-6               of progmag2 not =
              tot-ordinato-6
              move SaveArticolo         to articolo-edit
              display message "INCONGRUENZA SU ARTICOLO: "
                              articolo-edit                 
      *            x"0d0a""Giacenza padre: " prg-giacenza of progmag2
      *            x"0d0a""Giacenza figli: " tot-giacenza
      *            x"0d0a""---------------------------------"
      *            x"0d0a""Impegnato padre: " prg-impegnato of progmag2
      *            x"0d0a""Impegnato figli: " tot-impegnato
      *            x"0d0a""---------------------------------"
      *            x"0d0a""I.master padre: " prg-imp-master of progmag2
      *            x"0d0a""I.master figli: " tot-imp-master
      *            x"0d0a""---------------------------------"
      *            x"0d0a""I.GDO padre: " prg-imp-gdo of progmag2
      *            x"0d0a""I.GDO figli: " tot-imp-gdo
      *            x"0d0a""---------------------------------"
      *            x"0d0a""I.trad padre: " prg-imp-trad of progmag2
      *            x"0d0a""I.trad figli: " tot-imp-trad
      *            x"0d0a""---------------------------------"
      *            x"0d0a""I.ordinato1 padre: "prg-ordinato-1 of progmag2
      *            x"0d0a""I.ordinato1 figli: "tot-ordinato-1
      *            x"0d0a""---------------------------------"
      *            x"0d0a""I.ordinato2 padre: "prg-ordinato-2 of progmag2
      *            x"0d0a""I.ordinato2 figli: "tot-ordinato-2
      *            x"0d0a""---------------------------------"
      *            x"0d0a""I.ordinato3 padre: "prg-ordinato-3 of progmag2
      *            x"0d0a""I.ordinato3 figli: "tot-ordinato-3
      *            x"0d0a""---------------------------------"
      *            x"0d0a""I.ordinato4 padre: "prg-ordinato-4 of progmag2
      *            x"0d0a""I.ordinato4 figli: "tot-ordinato-4
      *            x"0d0a""---------------------------------"
      *            x"0d0a""I.ordinato5 padre: "prg-ordinato-5 of progmag2
      *            x"0d0a""I.ordinato5 figli: "tot-ordinato-5
      *            x"0d0a""---------------------------------"
      *            x"0d0a""I.ordinato6 padre: "prg-ordinato-6 of progmag2
      *            x"0d0a""I.ordinato6 figli: "tot-ordinato-6
      *            x"0d0a""---------------------------------"
                        title "ERRORE - ELABORAZIONE TERMINATA!"
              set errori to true
           end-if.
           move prg-rec of progmag2 to como-rec.

      ***---
       SOMMA-VALORI.        
           add prg-giacenza            of progmag 
            to tot-giacenza.
           add prg-impegnato            of progmag 
            to tot-impegnato.
           add prg-imp-master             of progmag 
            to tot-imp-master.
           add prg-imp-gdo        of progmag 
            to tot-imp-gdo.
           add prg-imp-trad of progmag 
            to tot-imp-trad.
           add prg-ordinato-1               of progmag 
            to tot-ordinato-1.
           add prg-ordinato-2               of progmag 
            to tot-ordinato-2.
           add prg-ordinato-3            of progmag 
            to tot-ordinato-3.
           add prg-ordinato-4          of progmag 
            to tot-ordinato-4.
           add prg-ordinato-5    of progmag 
            to tot-ordinato-5.
           add prg-ordinato-6          of progmag 
            to tot-ordinato-6.         
           add prg-ini-udm            of progmag 
            to tot-prg-ini-udm.
           add prg-ini-kg            of progmag 
            to tot-prg-ini-kg.
           add prg-ini-valore             of progmag 
            to tot-prg-ini-valore.
           add prg-acq-udm        of progmag 
            to tot-prg-acq-udm.
           add prg-acq-kg of progmag 
            to tot-prg-acq-kg.
           add prg-acq-valore               of progmag 
            to tot-prg-acq-valore.
           add prg-ven-udm               of progmag 
            to tot-prg-ven-udm.
           add prg-ven-kg            of progmag 
            to tot-prg-ven-kg.
           add prg-ven-valore          of progmag 
            to tot-prg-ven-valore.
           add prg-var-inv-udm    of progmag 
            to tot-prg-var-inv-udm.
           add prg-var-inv-kg          of progmag 
            to tot-prg-var-inv-kg.      
           add prg-var-inv-valore          of progmag 
            to tot-prg-var-inv-valore.      
           add prg-resi-fornitori-udm          of progmag 
            to tot-prg-resi-fornitori-udm.      
           add prg-resi-fornitori-kg          of progmag 
            to tot-prg-resi-fornitori-kg.      
           add prg-resi-fornitori-valore          of progmag 
            to tot-prg-resi-fornitori-valore.      
           add prg-resi-da-cli-udm          of progmag 
            to tot-prg-resi-da-cli-udm.      
           add prg-resi-da-cli-kg          of progmag 
            to tot-prg-resi-da-cli-kg.      
           add prg-resi-da-cli-valore          of progmag 
            to tot-prg-resi-da-cli-valore.      
           add prg-udm-el          of progmag 
            to tot-prg-udm-el.           
           add prg-giacenza-kg          of progmag 
            to tot-prg-giacenza-kg.      
           add prg-giacenza-udm          of progmag 
            to tot-prg-giacenza-udm.      
           add prg-kg-el          of progmag 
            to tot-prg-kg-el.      
           add prg-valore-el          of progmag 
            to tot-prg-valore-el.      
           add prg-udm-el2          of progmag 
            to tot-prg-udm-el2.      
           add prg-kg-el2          of progmag 
            to tot-prg-kg-el2.      
           add prg-valore-el2          of progmag 
            to tot-prg-valore-el2.      
           add prg-udm-ul          of progmag 
            to tot-prg-udm-ul.      
           add prg-kg-ul          of progmag 
            to tot-prg-kg-ul.      
           add prg-valore-ul          of progmag 
            to tot-prg-valore-ul.      
           add prg-udm-ul2          of progmag 
            to tot-prg-udm-ul2.      
           add prg-var-inv-kg          of progmag 
            to tot-prg-var-inv-kg.      
           add prg-kg-ul2          of progmag 
            to tot-prg-kg-ul2.      
           add prg-valore-ul2          of progmag 
            to tot-prg-valore-ul2.      
           move prg-rec of progmag       to como-rec.

      ***---
       CLOSE-FILES.
           close progmag progmag2 articoli.

      ***---
       EXIT-PGM.
           goback.
