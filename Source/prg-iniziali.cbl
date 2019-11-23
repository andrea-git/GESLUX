       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      prg-iniziali.
       AUTHOR.                          Andrea.
       REMARKS. Dato un elenco di progressivi imposta il valore delle iniziali
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
       78  titolo value "Forzatura valore inizialie".

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
           88 negativo           value 1 false 0.

      * VARIABILI
       77  r-articolo            pic x(6).
       77  r-magazzino           pic x(3).
       77  r-imballo             pic x(3).
       77  r-peso                pic x(10).
       77  r-iniziali            pic x(15).
       
       77  articolo              pic 9(6).
       77  peso                  pic 9(10)v9(5).
       77  iniziali              pic s9(12)v99.

       77  num-rec               pic 9(6)   value 0.
       77  num-rec-ko            pic 9(6)   value 0.
       77  num-rec-ok            pic 9(6)   value 0.

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
           move "prog-ini.csv" to wstampa.

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
              unstring line-riga  delimited by ";"
                       into r-articolo
                            r-magazzino
                            r-imballo
                            r-peso
                            r-iniziali
              end-unstring 
              perform FORMAT-CAMPI

              initialize prg-chiave replacing numeric data by zeros 
                                         alphanumeric data by spaces
              move articolo    to prg-cod-articolo
              move r-magazzino to prg-cod-magazzino
              move r-imballo   to prg-tipo-imballo
              move peso        to prg-peso
              read progmag 
                   invalid add 1 to num-rec-ko
               not invalid
                   move iniziali    to prg-ini-valore
                   rewrite prg-rec                    
                       not invalid add 1 to num-rec-ok
                   end-rewrite
              end-read
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""CODICI: ", num-rec,
                    x"0d0a""ELABORATI: ", num-rec-ok,
                    x"0d0a""NON TROVATI: ", num-rec-ko
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close progmag lineseq.

      ***---
       FORMAT-CAMPI.                    
           call "C$JUSTIFY" using r-articolo, "R".
           inspect r-articolo, replacing leading x"20" by x"30".
           move r-articolo  to articolo.

           call "C$JUSTIFY" using r-peso, "R".
           inspect r-peso, replacing leading x"20" by x"30".
           move r-peso  to peso.
           divide peso by 1000 giving peso.
                                                   
           call "C$JUSTIFY" using r-iniziali, "L".
           if r-iniziali(1:1) = "-"
              set negativo to true                
              move spaces to r-iniziali(1:1)
           else
              set negativo to false
           end-if.
           call "C$JUSTIFY" using r-iniziali, "R".
           inspect r-iniziali, replacing leading x"20" by x"30".
           move r-iniziali  to iniziali.   
           divide iniziali by 100 giving iniziali.
           if negativo
              compute iniziali = iniziali * -1
           end-if.
           

      ***---
       EXIT-PGM.
           goback.
