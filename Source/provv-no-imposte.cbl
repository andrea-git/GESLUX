       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      provv-no-imposte.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "tordini.sl".
           copy "rordini.sl".
           copy "provvig.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "tordini.fd".
           copy "rordini.fd".
           copy "provvig.fd".

       WORKING-STORAGE SECTION.

      * COSTANTI
       78  titolo value "Listini agente".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-provvig        pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI
       01 r-riga.
           05 r-articolo         pic 9(6).
           05 r-prezzo           pic 9(10).

       77  como-data             pic 9(8).
       77  prezzo-z              pic z(10).
       01  progmag.
           05  prg-peso-utf          pic 9(6)v999.
           05  prg-peso-non-utf      pic 9(6)v999.
       77  prezzo                pic 9(8)v99.
       77  como-articolo         pic 9(6).
       77  num-rec-upd           pic 9(6)   value 0.
       77  num-rec-new           pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
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
                  x"0d0a""File  [LINESEQ] inesistente"
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
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa         delimited low-value
                   "lisagente.csv" delimited size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           open i-o   provvig.
           open input tordini rordini lineseq.

      ***---
       ELABORAZIONE.
           initialize line-riga.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              unstring line-riga delimited by ";"
                       into r-articolo           
                            r-prezzo
              end-unstring

              move r-articolo to como-articolo
              call "C$JUSTIFY" using r-prezzo, "R"
              inspect r-prezzo replacing leading x"20" by x"30"
              move r-prezzo to prezzo-z
              move prezzo-z to prezzo

              divide prezzo by 100 
              giving prezzo

              move low-value to pvv-rec
              move 20101001  to pvv-data-fat
              start provvig key >= k-data-fat
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read provvig next at end exit perform end-read
                       if pvv-articolo = como-articolo
                          move pvv-anno-fat to tor-anno-fattura
                          move pvv-num-fat  to tor-num-fattura
                          read tordini no lock key k-fattura
                               invalid continue
                           not invalid
                               move tor-anno     to ror-anno
                               move tor-numero   to ror-num-ordine
                               move pvv-riga-fat to ror-num-riga
                               read rordini no lock
                                    invalid continue
                                not invalid
                                    if ror-cod-articolo = como-articolo
                                       compute prezzo = 
                                               prezzo -
                                               ror-imp-cou-cobat -
                                               ror-imp-consumo
                                       move prezzo 
                                         to pvv-prezzo-netto-agente
                                       rewrite pvv-rec
                                       add 1 to num-rec-upd
                                    end-if
                               end-read
                          end-read
                       end-if
                    end-perform
              end-start
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""AGGIORNATI: ",  num-rec-upd,
                     title titolo
                      icon 2.

      ***---
       CLOSE-FILES.
           close lineseq provvig tordini rordini.

      ***---
       EXIT-PGM.
           goback.
