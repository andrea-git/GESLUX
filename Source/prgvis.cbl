       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      prgvis.
       AUTHOR.                          Andrea.
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
       78  titolo value "Creazione progressivi VIS".

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
       77  filler                pic 9.
           88 peso-utf           value 1 false 0.

       77  r-cod-art             pic x(8).
       77  r-peso                pic x(6).
       77  como-peso             pic 9(6).

      * VARIABILI
       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0.
       77  num-rec-nn            pic 9(6)   value 0.

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
                  x"0d0a""File delle testate [LINESEQ] inesistente"
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
           move "prgvis.csv" to wstampa.

      ***---
       OPEN-FILES.
           open i-o   progmag.
           open input lineseq.

      ***---
       ELABORAZIONE.
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga r-cod-art r-peso
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              add 1 to num-rec
              unstring line-riga delimited by ";"
                       into r-cod-art
                            r-peso
              end-unstring
              call "C$JUSTIFY" using r-cod-art, "R"
              inspect r-cod-art replacing leading x"20" by x"30"
              call "C$JUSTIFY" using r-peso, "R"
              inspect r-peso replacing leading x"20" by x"30"
              move r-peso to como-peso
              if r-cod-art not = spaces
                 initialize prg-chiave replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 move r-cod-art to prg-cod-articolo
                 read progmag no lock
                      invalid add 1 to num-rec-ko
                  not invalid              
                      if prg-peso-utf not = 0
                         set peso-utf to true
                      end-if 
                      if prg-peso-non-utf not = 0
                         set peso-utf to false
                      end-if               
                      initialize prg-rec 
                                 replacing numeric data by zeroes
                                      alphanumeric data by spaces
                      move r-cod-art to prg-cod-articolo
                      divide como-peso by 1000 giving prg-peso
                      move "LBX" to prg-cod-magazzino
                      move "VIS" to prg-tipo-imballo
                      read progmag no lock
                           invalid
                           if peso-utf
                              move prg-peso to prg-peso-utf
                           else
                              move prg-peso to prg-peso-non-utf
                           end-if
                           set prg-attivo to true
                           accept prg-data-creazione from century-date
                           accept prg-ora-creazione  from time
                           move "PRGVIS" to prg-utente-creazione
                           write prg-rec
                           add 1 to num-rec-ok
                       not invalid
                           add 1 to num-rec-nn
                      end-read
                 end-read 
              end-if
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""AGGIUNTI: ", num-rec-ok,
                    x"0d0a""NON TROVATI: ", num-rec-ko
                    x"0d0a""GIA' PRESENTI: ", num-rec-nn
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close progmag lineseq.

      ***---
       EXIT-PGM.
           goback.
