       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-progmag.
       AUTHOR.                          Andrea.
       REMARKS. Controlla la presenza dei progressivi per gli articoli 
                presenti in anagrafica. Se non esiste lo crea come in 
                fase di creazione articolo
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "progmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "progmag.fd".

       WORKING-STORAGE SECTION.
       copy "link-wprogmag.def".
       77  status-articoli       pic xx.
       77  status-progmag        pic xx. 
       77  n                     pic 999 value 0.
           
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

      ***---
       OPEN-FILES.
           open input articoli.
           open i-o   progmag.

      ***---
       ELABORAZIONE.                                         
           move low-value to art-rec.
           start articoli key >= art-chiave
           perform until 1 = 2
              read articoli next at end exit perform end-read
              initialize prg-chiave replacing numeric data by zeroes
                                         alphanumeric data by spaces
              move art-codice to prg-cod-articolo
              read progmag no lock
                   invalid
                   add 1 to n
                   set link-batch to true
                   perform SCRIVI-PROGMAG
              end-read
           end-perform.
           display message "Inseriti " n " progressivi".

      ***---
       SCRIVI-PROGMAG.
           move "CHECK-PROGMAG"      to link-user.
           move art-codice           to link-articolo.
           move art-descrizione      to link-des-articolo. 
           move art-imballo-standard to link-imballo
           move art-mag-std          to link-magazzino.
           move art-peso-utf         to link-utf.
           move art-peso-non-utf     to link-non-utf.
           add link-utf to link-non-utf giving link-peso.
           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".

      ***---
       CLOSE-FILES.
           close progmag articoli.

      ***---
       EXIT-PGM.
           goback.
