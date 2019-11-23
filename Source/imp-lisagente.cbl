       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-lisagente.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "articoli.sl".
           copy "timposte.sl".
           copy "tmarche.sl".
           copy "lisagente.sl".


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "articoli.fd".
           copy "timposte.fd".
           copy "tmarche.fd".
           copy "lisagente.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "imposte.def".

      * COSTANTI
       78  titolo value "Importazione listini agente".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-articoli       pic xx.
       77  status-timposte       pic xx.
       77  status-tmarche        pic xx.
       77  status-lisagente      pic xx.
       77  wstampa               pic x(256).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

      * VARIABILI
       01 r-riga.
           05 r-codice           pic 9(4).
           05 r-articolo         pic 9(6).
           05 r-descrizione      pic x(50).
           05 r-data-inizio      pic 9(8).
           05 r-data-fine        pic 9(8).
           05 r-prezzo           pic 9(10).
           05 r-calcolo-imposte  pic 9.
           05 r-scorporo-imposte pic 9.

       77  como-data             pic 9(8).
       77  prezzo-z              pic z(10).
       01  progmag.
           05  prg-peso-utf          pic 9(6)v999.
           05  prg-peso-non-utf      pic 9(6)v999.
       77  prezzo                pic 9(8)v99.
       77  num-rec-upd           pic 9(6)   value 0.
       77  num-rec-new           pic 9(6)   value 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       ARTICOLI SECTION.
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
                  x"0d0a""File delle testate [ARTICOLI] inesistente"
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
           open i-o   lisagente.
           open input lineseq timposte tmarche articoli.

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           initialize line-riga.
           perform until 1 = 2
              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga  = spaces   exit perform end-if
              unstring line-riga delimited by ";"
                       into r-codice             
                            r-articolo           
                            r-descrizione
                            r-data-inizio
                            r-data-fine
                            r-prezzo
                            r-calcolo-imposte
                            r-scorporo-imposte
              end-unstring

              if r-scorporo-imposte = 1
                 move r-articolo to art-codice 
                 read articoli no lock invalid continue end-read
                 move art-marca-prodotto to mar-codice
                 read tmarche no lock invalid continue end-read
                 set TrattamentoGDO to false
                 move art-peso-utf     to prg-peso-utf
                 move art-peso-non-utf to prg-peso-non-utf
                 perform CALCOLA-IMPOSTE
              else
                 move 0 to imposta-cou imposta-consumo imposta-cobat
              end-if

              call "C$JUSTIFY" using r-prezzo, "R"
              inspect r-prezzo replacing leading x"20" by x"30"
              move r-prezzo to prezzo-z
              move prezzo-z to prezzo

              if prezzo not = 0
                 divide prezzo by 100 giving prezzo
                 compute prezzo = prezzo      -
                                  imposta-cou     -
                                  imposta-consumo -
                                  imposta-cobat
              end-if
              
              initialize lis-rec replacing numeric data by zeroes
                                      alphanumeric data by zeroes
              move r-codice               to lis-codice
              move r-articolo             to lis-articolo
              read lisagente no lock
                   invalid
                   move r-data-inizio     to lis-data-inizio-old
                   move r-data-fine       to lis-data-fine-old
                   move prezzo            to lis-prezzo-old
                   move r-calcolo-imposte to lis-calcolo-imposte-old
                   move "IMPORT" to lis-utente-creazione
                   accept lis-data-creazione from century-date
                   accept lis-ora-creazione  from time
               not invalid
                   perform SPOSTA-LISTINO-VECCHIO
                   move r-data-inizio     to lis-data-inizio-new
                   move r-data-fine       to lis-data-fine-new
                   move prezzo            to lis-prezzo-new
                   move r-calcolo-imposte to lis-calcolo-imposte-new
                   move "IMPORT" to lis-utente-ultima-modifica
                   accept lis-data-ultima-modifica from century-date
                   accept lis-ora-ultima-modifica  from time
              end-read
              move r-descrizione     to lis-descrizione

              write lis-rec
                    invalid add 1 to num-rec-upd
                            rewrite lis-rec end-rewrite
                not invalid add 1 to num-rec-new
              end-write

           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""AGGIORNATI: ",  num-rec-upd,
                    x"0d0a""INSERITI: ",    num-rec-new
                     title titolo
                      icon 2.

      ***---
       SPOSTA-LISTINO-VECCHIO.
           move lis-data-inizio-new     to lis-data-inizio-old.
           move lis-data-fine-new       to lis-data-fine-old.
           move lis-prezzo-new          to lis-prezzo-old.
           move lis-calcolo-imposte-new to lis-calcolo-imposte-old.

           if lis-data-fine-old >= r-data-inizio
              compute como-data = 
                      function integer-of-date (r-data-inizio)
              subtract 1 from como-data
              compute lis-data-fine-old = 
                      function date-of-integer (como-data)
              if lis-data-inizio-old > lis-data-fine-old
                 move lis-data-fine-old to lis-data-inizio-old
              end-if
           end-if.

      ***---
       CLOSE-FILES.
           close articoli lineseq timposte tmarche lisagente.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "imposte.cpy".
