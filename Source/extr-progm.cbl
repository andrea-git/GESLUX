       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      extr-progm.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "tmarche.sl".
           copy "progmag.sl".
           copy "lineseq.sl".
           copy "timposte.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "tmarche.fd".
           copy "progmag.fd".
           copy "lineseq.fd".
           copy "timposte.fd".

       WORKING-STORAGE SECTION.
       copy "costo-medio.def".
       copy "imposte.def".

       78  titolo value "EXTRACT PROGMAG".

       77  prg-costo-medio-ed        pic ---.---.--9,99.
       77  prg-ini-valore-ed         pic ---.---.--9,99.
       77  prg-acq-valore-ed         pic ---.---.--9,99.
       77  prg-ini-udm-ed            pic ---.---.--9.
       77  prg-acq-udm-ed            pic ---.---.--9.
       77  costo-mp-ed               pic ---.---.--9,99.

       77  status-articoli           pic xx.
       77  status-tmarche            pic xx.
       77  status-progmag            pic xx.
       77  status-lineseq            pic xx.
       77  status-timposte           pic xx.
       77  wstampa                   pic x(256).

       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

       01  filler                    pic xx.
         88 PrimaVolta               value 1, false 0.

       01  filler                    pic xx.
         88 RecLocked                value 1, false 0.

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
                display message "File [PROGMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

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
                display message "File [TIMPOSTE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.
                        
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "39"
                set errori to true
                display message "File [TMARCHE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMARCHE] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TMARCHE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
           
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
                display message "File [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       END DECLARATIVES.

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
           initialize wstampa.
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa      delimited low-value
                   "EXTR_PROGM" delimited size
                   ".csv"       delimited size
                   into wstampa
           end-string.
           
           set tutto-ok   to true.
           set PrimaVolta to true.

      ***---
       OPEN-FILES.
           open input  progmag articoli tmarche timposte.
           open output lineseq.
      
      ***---
       ELABORAZIONE.
           accept imp-data from century-date.

           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move low-value to prg-chiave.
           start progmag key is >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next no lock 
                         at end exit perform 
                    end-read
                    if prg-cod-magazzino = spaces and
                       prg-tipo-imballo  = spaces and
                       prg-peso          = 0
                       if PrimaVolta
                          set PrimaVolta to false
                          string "ARTICOLO"      delimited size
                                 ";"              delimited size
                                 "MARCA"          delimited size
                                 ";"              delimited size
                                 "COSTO MP (PRG)" delimited size
                                 ";"              delimited size
                                 "VAL. INIZIALI"  delimited size
                                 ";"              delimited size
                                 "VAL. ACQUISTI"  delimited size
                                 ";"              delimited size
                                 "UDM INI"        delimited size
                                 ";"              delimited size
                                 "UDM ACQ"        delimited size
                                 ";"              delimited size
                                 "COSTO MP CALC." delimited size
                                 ";"              delimited size
                                 "COSTO MP"       delimited size
                                 into line-riga
                          end-string
                          write line-riga
                       end-if
                       initialize art-rec mar-rec line-riga
                       move prg-cod-articolo to art-codice
                       read articoli no lock invalid continue end-read
                       move art-marca-prodotto to mar-codice
                       read tmarche  no lock invalid continue end-read
                       perform CALCOLA-COSTO-MP
                       perform CALCOLA-COSTO-MP-WHEN-ZERO
                       move prg-costo-medio to prg-costo-medio-ed
                       move prg-ini-valore  to prg-ini-valore-ed
                       move prg-acq-valore  to prg-acq-valore-ed
                       move prg-ini-udm     to prg-ini-udm-ed
                       move prg-acq-udm     to prg-acq-udm-ed
                       move costo-mp        to costo-mp-ed

                       string prg-cod-articolo   delimited size
                              ";"                delimited size
                              mar-descrizione    delimited size
                              ";"                delimited size
                              prg-costo-medio-ed delimited size
                              ";"                delimited size
                              prg-ini-valore-ed  delimited size
                              ";"                delimited size
                              prg-acq-valore-ed  delimited size
                              ";"                delimited size
                              prg-ini-udm-ed     delimited size
                              ";"                delimited size
                              prg-acq-udm-ed     delimited size
                              ";"                delimited size
                              costo-mp-ed        delimited size
                              into line-riga
                       end-string
                       write line-riga
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close progmag lineseq articoli tmarche timposte.

      ***---
       EXIT-PGM.
           display message "FINE".
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "costo-medio.cpy".
       copy "calcola-costo-mp-when-zero.cpy".
