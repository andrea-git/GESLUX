       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-extra.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "rpromo.sl".
           copy "articoli.sl".
           copy "timposte.sl".
           copy "listini.sl".
           copy "tmarche.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "rpromo.fd".
           copy "articoli.fd".
           copy "timposte.fd".
           copy "listini.fd".
           copy "tmarche.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "imposte.def".

       77  status-rpromo        pic xx.
       77  status-articoli      pic xx.
       77  status-timposte      pic xx.
       77  status-listini       pic xx.
       77  status-tmarche       pic xx.

      * COSTANTI
       78  titolo                value "GESLUX - Inserimento Extra".

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.

      * VARIABILI
       01  progmag.
           03  prg-peso-utf      pic 9(5)v999.
           03  prg-peso-non-utf  pic 9(5)v999.
       77  tot-imposte           pic 9(9)v99. 
       77  sconto                pic 9(9)v99.
       77  prezzo-netto          pic 9(9)v99.
       77  NumChars              pic 9(3).
       77  CountChar             pic 9(3).
       77  articolo-ed           pic z(6).
       77  num-articoli          pic 9(6) value 0.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
           copy "link-lab-extra.def".

      ******************************************************************
       PROCEDURE DIVISION using lab-extra-linkage.

       DECLARATIVES.

      ***---
       RPROMO-ERR SECTION.
           use after error procedure on rpromo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rpromo
           when "39"
                set errori to true
                display message "File [RPROMO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "Impossibile procedere."
                   x"0d0a""File [RPROMO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate. 

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-timposte
           when "39"
                set errori to true
                display message "File [TIMPOSTE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMPOSTE] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TIMPOSTE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       LISTINI-ERR SECTION.
           use after error procedure on listini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-listini
           when "39"
                set errori to true
                display message "File [LISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [LISTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
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
           move 0    to counter counter2.
           set tutto-ok    to true.
           set RecLocked   to false.
           move 0 to NumChars.
           inspect extra-testo replacing trailing spaces by low-value.
           inspect extra-testo tallying NumChars for 
                               characters before low-value.

      ***---
       OPEN-FILES.
           open input articoli timposte listini tmarche.
           open i-o rpromo.
           if errori
              close articoli timposte listini
           end-if.

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move low-value to art-rec.
           start articoli key >= art-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read articoli next at end exit perform end-read
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon extra-handle at column 42,00 line 11,00
                    move 0 to counter2
                 end-if
                 move 0 to CountChar
                 inspect  art-descrizione replacing trailing 
                                          spaces by low-value
                 inspect  art-descrizione 
                 tallying CountChar for all extra-testo(1:NumChars)
                 before low-value
                 if CountChar not = 0
                    perform CHECK-ESISTENZA-VALIDITA-LISTINO
                    if trovato
                       perform CHECK-ESISTENZA-ARTICOLO-IN-PROMO
                       if not trovato
                          add 1 to num-articoli
                          |Da Walter: fisso NO
                          set  TrattamentoPiombo    to false
                          |Per cui non leggo nemmeno la marca
                          set  TrattamentoGDO       to true
                          move art-peso-utf         to prg-peso-utf
                          move art-peso-non-utf     to prg-peso-non-utf
                          perform CALCOLA-IMPOSTE
                          compute tot-imposte = imposta-consumo +
                                                imposta-cou     +
                                                imposta-cobat
                          compute prezzo-netto   =
                              lst-prezzo - tot-imposte
                          compute sconto =
                               (( prezzo-netto ) *
                                  extra-sconto ) / 100
                          compute rpr-prz-acq     =
                                  prezzo-netto - sconto + tot-imposte
                          if rpr-utente-creazione = spaces
                             move extra-user to rpr-utente-creazione
                          else
                             move extra-user to rpr-utente-modifica
                          end-if
                          if rpr-ora-creazione = spaces
                             accept rpr-ora-creazione from time
                          else
                             accept rpr-ora-modifica from time
                          end-if
                          if rpr-data-creazione = spaces
                             accept rpr-data-creazione from century-date
                          else
                             accept rpr-data-modifica from century-date
                          end-if
                          write rpr-rec invalid continue end-write
                       end-if
                    end-if
                 end-if
              end-perform
           end-if.
           if num-articoli = 0
              set inserito-extra to false
              display message "Nessun Articolo elaborato"
                        title titolo
                         icon 2
           else
              set inserito-extra to true
              display message "Elaborati ", num-articoli, " articoli"
                        title titolo
                         icon 2
           end-if.

      ***---
       CHECK-ESISTENZA-VALIDITA-LISTINO.
           set trovato to false.
           move extra-gdo     to lst-gdo.
           move art-codice    to lst-articolo.
           move extra-ini-dpo to lst-data.
           start listini key <= lst-k-articolo
                 invalid continue
             not invalid
                 read listini previous
                 if lst-articolo = art-codice    and
                    lst-gdo      = extra-gdo     and
                    lst-data    <= extra-ini-dpo and
                    lst-prezzo  >  0 
                    set trovato to true
                 end-if
           end-start.

      ***---
       CHECK-ESISTENZA-ARTICOLO-IN-PROMO.
           set trovato to false.
           initialize rpr-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move extra-codice to rpr-codice.
           move art-codice   to rpr-articolo.
           read rpromo no lock 
                invalid continue 
            not invalid
                set trovato to true
                display message "Articolo " rpr-articolo, 
                                " già presente sul volantino!"
                          title titolo
                           icon 2
           end-read.

      ***---
       CLOSE-FILES.
           close rpromo articoli timposte listini tmarche.

      ***---
       EXIT-PGM.
           display "                                                "
              upon extra-handle at column 42,00 line 11,00.

           goback.

      ***---
       PARAGRAFO-COPY.
           copy "imposte.cpy".
