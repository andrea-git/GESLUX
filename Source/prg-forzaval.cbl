       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      prg-forzaval.
       AUTHOR.                          Andrea.
       REMARKS. Dato un elenco di codici articoli forza un valore.
040314          dimezza il VALORE delle iniziali.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "progmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "progmag.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Forzatura valore progressivi".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-progmag        pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI         
       77  como-valore           pic 9(9)v99.
       77  r-codice              pic 9(6).
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0.
       77  num-rec-art           pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag
           when "39"
                set errori to true
                display message "File [PROGMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [PROGMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "39"
                set errori to true
                display message "File [LINESEQ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""[LINESEQ] inesistente"
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
           set tutto-ok to true.
           move "cod-art.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   progmag.
           open input lineseq.

      ***---
       ELABORAZIONE.
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add  1 to num-rec
              move 0 to como-valore
              move line-riga to r-codice convert
              initialize prg-chiave replacing numeric data by zeros 
                                         alphanumeric data by spaces
              move r-codice to prg-cod-articolo
              start progmag key >= prg-chiave
                    invalid add 1 to num-rec-ko
                not invalid
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-articolo not = r-codice
                          exit perform
                       end-if
                       if prg-peso not = 0
                          add 1 to num-rec-art
                          compute prg-ini-valore = prg-ini-valore / 2
                          add prg-ini-valore to como-valore
                          rewrite prg-rec
                                  invalid add 1 to num-rec-ko
                              not invalid add 1 to num-rec-ok
                          end-rewrite
                       end-if
                    end-perform               
                    add 1 to num-rec-art
                    initialize prg-chiave 
                               replacing numeric data by zeros 
                                    alphanumeric data by spaces
                    move r-codice to prg-cod-articolo
                    read progmag no lock
                    move como-valore to prg-ini-valore
                    rewrite prg-rec
                            invalid add 1 to num-rec-ko
                        not invalid add 1 to num-rec-ok
                    end-rewrite
              end-start
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""CODICI: ", num-rec,
                    x"0d0a""ELABORATI: ", num-rec-art,
                    x"0d0a""CORRETTI: ", num-rec-ok
                    x"0d0a""ERRATI: ", num-rec-ko
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close progmag lineseq.

      ***---
       EXIT-PGM.
           goback.
