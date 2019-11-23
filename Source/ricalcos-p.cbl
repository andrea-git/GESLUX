       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ricalcos-p.
       AUTHOR.                          Andrea.
       REMARKS. Ricalcolo del costo medio (calcolato ad inizio anno)
                su TUTTI i progressivi di magazzino (padri e figli) e 
                valorizzazione del costo ultimo con lo stesso valore 
                (se richiesto).
                Valorizzazione anche del valore delle iniziali 
                utilizzando questo valore.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".
           copy "articoli.sl".
           copy "tmarche.sl".
           copy "timposte.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd". 
           copy "articoli.fd".
           copy "tmarche.fd".
           copy "timposte.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "costo-medio.def".
           copy "imposte.def".

       78  titolo                value "Ricalcolo costo ultimo".
       78  78-clear              value 
           "                                                          ".

       77  status-progmag        pic xx.
       77  status-articoli       pic xx.
       77  status-tmarche        pic xx.
       77  status-timposte       pic xx.

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
      
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.

       LINKAGE SECTION.
       77  link-handle           handle of window.
       77  link-valorizza-costo  pic 9.
         88 link-si-valorizza    value 1.
         88 link-no-valorizza    value 0.

      ******************************************************************
       PROCEDURE DIVISION using link-handle 
                                link-valorizza-costo.

       DECLARATIVES.
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File dei progressivi [PROGMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [PROGMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
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
           move 0 to counter counter2.
           set tutto-ok    to true.

      ***---
       OPEN-FILES.
           perform OPEN-I-O-PROGMAG-LOCK.
           open input articoli tmarche timposte.

      ***---
       OPEN-I-O-PROGMAG-LOCK.
           |Lo apro in lock per verificare che 
           |attualmente non ci sia dentro nessuno. 
           move "progmag" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file dei progressivi di magazzino" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il ricalcolo." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
           open i-o progmag allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"

              evaluate true
              when riprova perform OPEN-I-O-PROGMAG-LOCK
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           end-if.

      ***---
       ELABORAZIONE.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display 78-clear
              upon link-handle at column 01
                                    line 03.
           ||||||||
           move low-value to prg-rec.
           start progmag key is >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 05,00
                                                line 03,00
                       move 0 to counter2
                    end-if

                    read progmag next at end exit perform end-read
                                      
                    if prg-peso = 0
                       move prg-cod-articolo to art-codice
                       perform CALCOLA-COSTO-MP-COMPLETO
                        add 0,005 to costo-mp giving costo-mp-2dec
                    end-if

                    move costo-mp-2dec to prg-costo-medio
                    if link-si-valorizza
                       move costo-mp to prg-costo-ultimo
                    end-if                             

                    compute prg-ini-valore   =
                            prg-ini-udm * costo-mp-2dec
      
                    rewrite prg-rec invalid continue end-rewrite

                 end-perform
           end-start.

      ***--
       CLOSE-FILES.
           close progmag articoli tmarche timposte.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "costo-medio.cpy".
           copy "recupero-anagrafica.cpy".
