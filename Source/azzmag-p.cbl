       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      azzmag-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "progmag.sl".

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "progmag.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "link-wprogmag.def".

       77  scelta                pic 9(8).
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-progmag        pic xx.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       78  titolo                value "Riaggiornamento magazzino".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       copy "link-agmag.def".

      ******************************************************************
       PROCEDURE DIVISION using agmag-linkage.

       DECLARATIVES.
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TMOVMAG] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
           when "39"
                set errori to true
                display message "File [TMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [RMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

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
                set errori to true
                display message "File [PROGMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
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
           move 0 to agmag-tot-mov.
           move 0 to agmag-mov-da.
           move 0 to agmag-mov-a.
           set tutto-ok    to true.
           set prima-volta to true.

      ***---
       OPEN-FILES.
           |Quando si consolida il magazzino NESSUNO deve essere
           |dentro al file dei movimenti (tranne che in visua) e mi
           |sembra il minimo dato che devo fare operazioni in massa
           perform OPEN-IO-TMOVMAG-LOCK
           if tutto-ok
              perform OPEN-IO-RMOVMAG-LOCK
              if tutto-ok
                 perform OPEN-IO-PROGMAG-LOCK
                 if errori
                    close tmovmag rmovmag
                 end-if
              else
                 close tmovmag
              end-if
           end-if.

           if errori goback end-if.

      ***---
       OPEN-IO-TMOVMAG-LOCK.
           move "tmovmag" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file delle testate dei movimenti" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il consolidamento." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
           open i-o tmovmag allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-IO-TMOVMAG-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-IO-RMOVMAG-LOCK.
           move "rmovmag" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file delle righe dei movimenti" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il consolidamento." delimited size
                 into geslock-messaggio
           end-string.

           set tutto-ok  to true.
           set RecLocked to false.
           open i-o rmovmag allowing readers.
           if RecLocked
              set errori to true
              move 1     to geslock-v-termina
              move 1     to geslock-v-riprova
              move 0     to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-IO-RMOVMAG-LOCK
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-IO-PROGMAG-LOCK.
           |Lo apro in lock per verificare che attualmente non ci 
           |sia dentro nessuno. Il controllo dell'apertura con
           |lock passa poi direttamente al pgm. "wprogmag".
           move "progmag" to geslock-nome-file.
           initialize geslock-messaggio.
           string   "Il file dei progressivi di magazzino" 
             x"0d0a""risulta in uso su altro terminale."
             x"0d0a""Questo comporta l'impossibilità a"
             x"0d0a""procedere con il consolidamento." delimited size
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
              when riprova perform OPEN-IO-PROGMAG-LOCK
              when other   display message "Operazione interrotta!"
                                     title titolo
                                      icon 2
              end-evaluate
           end-if.

      ***---
       ELABORAZIONE.
           perform AZZERA-PROGMAG.
           move 0 to tmo-numero.
           move 0 to counter counter2.
           string agmag-data(1:4) delimited size
                  "01"            delimited size
                  "01"            delimited size
                  into tmo-data-movim
           end-string.

           start tmovmag key is >= k-data
                 invalid set errori to true
           end-start.

           if tutto-ok
              |RIPULISCO LA SCREEN DAL CONTATORE
              display "                      "
                 upon agmag-handle at column 25
                                        line 03
              ||||||||
              perform until 1 = 2
                 read tmovmag next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    if counter = 100
                       display "AZZ TMOVMAG" 
                          upon agmag-handle at column 25
                                                 line 03
                   end-if
                    move counter to counter-edit
                    display counter-edit
                       upon agmag-handle at column 35
                                              line 03
                    move 0 to counter2
                 end-if

                 if tmo-data-movim > agmag-data
                    exit perform
                 end-if

                 perform LOOP-RIGHE-RMOVMAG
                 if trovato
                    add 1 to agmag-tot-mov
                    if tmo-numero < agmag-mov-da
                       move tmo-numero to agmag-mov-da
                    end-if
                    if tmo-numero > agmag-mov-a
                       move tmo-numero to agmag-mov-a
                    end-if
                 end-if
                 if errori exit perform end-if
              end-perform
           end-if.

           if agmag-tot-mov = 0
              display message "Nessun movimento da consolidare!"
                        title titolo
                         icon 2
           end-if.

      ***---
       LOOP-RIGHE-RMOVMAG.
           set trovato     to false.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           move low-values to rmo-riga.
           start rmovmag key is >= rmo-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rmovmag next at end exit perform end-read
                 if rmo-anno  not = tmo-anno  or
                    rmo-movim not = tmo-numero
                    exit perform
                 end-if

                 move agmag-user         to link-user
                 move rmo-chiave-progmag to link-key
                 move "0001111111111100" to link-array
                 move rmo-peso           to link-valore-peso

                 if rmo-qta not = 0
                    compute link-valore-monetario = 
                          ( rmo-netto    +
                            rmo-imp-cons + 
                            rmo-coubat ) * rmo-qta
                    compute link-valore-peso      = 
                            rmo-peso * rmo-qta
                 else
                    move 0 to link-valore-peso
                    compute link-valore-monetario = 
                            rmo-netto + rmo-imp-cons + rmo-coubat
                 end-if

                 move rmo-qta            to link-valore
                 move rmo-causale        to link-causale
                 set link-update         to true
                 set link-open-with-lock to true
                 set link-update-um      to true
                 set link-update-peso    to true
                 set link-update-valore  to true
                 if prima-volta
                    move tmo-numero to agmag-mov-da agmag-mov-a
                    set prima-volta to false
                    close progmag |wprogmag aprirà con lock il file!!!
                 end-if
                 call "wprogmag" using link-wprogmag
                 set trovato     to true
                 if link-wprogmag-status = -1
                    set errori to true
                    exit perform
                 end-if
              end-perform
           end-if.

      ***---
       AZZERA-PROGMAG.
           move 0 to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                      "
              upon agmag-handle at column 25
                                     line 03.
           ||||||||
           move low-value to prg-rec.
           start progmag key is >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       if counter = 100
                          display "AZZ PROGMAG" 
                             upon agmag-handle at column 25
                                                    line 03
                      end-if
                       move counter to counter-edit
                       display counter-edit
                          upon agmag-handle at column 35
                                                 line 03
                       move 0 to counter2
                    end-if

                    |Azzeramento dei valori consolidati
                    |ad eccezione della giacenza
                    move 0 to prg-acq-udm
                    move 0 to prg-acq-kg
                    move 0 to prg-acq-valore
                    move 0 to prg-ven-udm
                    move 0 to prg-ven-kg
                    move 0 to prg-ven-valore
                    move 0 to prg-var-inv-udm
                    move 0 to prg-var-inv-kg
                    move 0 to prg-var-inv-valore
                    move 0 to prg-resi-fornitori-udm
                    move 0 to prg-resi-fornitori-kg
                    move 0 to prg-resi-fornitori-valore
                    move 0 to prg-resi-da-cli-udm
                    move 0 to prg-resi-da-cli-kg
                    move 0 to prg-resi-da-cli-valore
                    move 0 to prg-giacenza-udm
                    move 0 to prg-giacenza-kg
                    move 0 to prg-udm-el
                    move 0 to prg-kg-el
                    move 0 to prg-valore-el
                    move 0 to prg-udm-el2
                    move 0 to prg-kg-el2
                    move 0 to prg-valore-el2
                    move 0 to prg-udm-ul
                    move 0 to prg-kg-ul
                    move 0 to prg-valore-ul
                    move 0 to prg-udm-ul2
                    move 0 to prg-kg-ul2
                    move 0 to prg-valore-ul2
                    |Valorizzazione della giacenza periodo
                    |con la giacenza iniziale
                    move prg-ini-udm to prg-giacenza-udm
                    move prg-ini-kg  to prg-giacenza-kg
                    |Aggiornamento del record
                    rewrite prg-rec invalid continue end-rewrite
                 end-perform
           end-start.

      ***--
       CLOSE-FILES.
           close tmovmag rmovmag.

      ***---
       EXIT-PGM.
           goback.
