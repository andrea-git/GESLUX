       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      agmag-p.
       AUTHOR.                          Andrea.
       REMARKS. Consolidamento movimenti aggiornando progmag coi valori 
                indicati nella causale. 
                Aggiornamento della data di consolidamento all'
                interno di tparamge.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tparamge.sl".
           copy "progmag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmovmag.fd".
           copy "rmovmag.fd". 
           copy "tparamge.fd".
           copy "progmag.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "link-wprogmag.def".
       77  scelta                pic 9(8).
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-tparamge       pic xx.
       77  status-progmag        pic xx.

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       78  titolo              value "MAGAZZINO: Aggiornamento Mensile".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.

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

       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File parametri [TPARAMGE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TPARAMGE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPARAMGE] Indexed file corrupt!"
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
              perform LEGGI-PARAMETRI
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to agmag-tot-mov.
           move 0 to agmag-mov-da.
           move 0 to agmag-mov-a.
           move 0 to counter counter2.
           set tutto-ok    to true.
           set prima-volta to true.

      ***---
       OPEN-FILES.
           |Quando si consolida il magazzino NESSUNO deve essere
           |dentro al file dei movimenti (tranne che in visua) e mi
           |sembra il minimo dato che devo fare operazioni in massa
           perform OPEN-IO-TPARAMGE-LOCK
           if tutto-ok  
              perform OPEN-IO-TMOVMAG-LOCK
              if tutto-ok
                 perform OPEN-IO-RMOVMAG-LOCK
                 if tutto-ok
                    perform OPEN-IO-PROGMAG-LOCK
                    if errori
                       close tparamge tmovmag rmovmag
                    end-if
                 else
                    close tparamge tmovmag
                 end-if
              else
                 close tparamge
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

           perform until 1 = 2
              set tutto-ok  to true
              set RecLocked to false
              open i-o tmovmag allowing readers
              if RecLocked
                 set errori to true
                 move 0     to geslock-v-termina
                 move 1     to geslock-v-riprova
                 move 0     to geslock-v-ignora
                 call   "geslock" using geslock-linkage
                 cancel "geslock"
              
                 evaluate true
                 when riprova continue
                 when other   display message "Operazione interrotta!"
                                        title titolo
                                         icon 2
                              exit perform
                 end-evaluate
              else
                 exit perform
              end-if
           end-perform.

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

           perform until 1 = 2
              set tutto-ok  to true
              set RecLocked to false
              open i-o rmovmag allowing readers
              if RecLocked
                 set errori to true
                 move 0     to geslock-v-termina
                 move 1     to geslock-v-riprova
                 move 0     to geslock-v-ignora
                 call   "geslock" using geslock-linkage
                 cancel "geslock"

                 evaluate true
                 when riprova continue
                 when other   display message "Operazione interrotta!"
                                        title titolo
                                         icon 2
                              exit perform
                 end-evaluate
              else
                 exit perform
              end-if
           end-perform.
        
      ***---
       OPEN-IO-TPARAMGE-LOCK.
           move "tparamge" to geslock-nome-file.
           initialize geslock-messaggio.
           string     "La tabella dei parametri generali"
             x"0D0A", "risulta in uso su altro terminale."
             x"0D0A", "Questo comporta l'impossibilità a"
             x"0d0a", "procedere con il consolidamento." delimited size
                 into geslock-messaggio
           end-string.

           perform until 1 = 2
              set tutto-ok  to true
              set RecLocked to false
              open i-o tparamge allowing readers
              if RecLocked
                 set errori to true
                 move 0 to geslock-v-termina
                 move 1 to geslock-v-riprova
                 move 0 to geslock-v-ignora
                 call   "geslock" using geslock-linkage
                 cancel "geslock"

                 evaluate true
                 when riprova continue
                 when other   display message "Operazione interrotta!"
                                        title titolo
                                         icon 2
                              exit perform
                 end-evaluate
              else
                 exit perform
              end-if
           end-perform.

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

           
           perform until 1 = 2
              set tutto-ok  to true
              set RecLocked to false
              open i-o progmag allowing readers
              if RecLocked
                 set errori to true
                 move 0     to geslock-v-termina
                 move 1     to geslock-v-riprova
                 move 0     to geslock-v-ignora
                 call   "geslock" using geslock-linkage
                 cancel "geslock"
              
                 evaluate true
                 when riprova continue
                 when other   display message "Operazione interrotta!"
                                        title titolo
                                         icon 2
                              exit perform
                 end-evaluate
              else
                 exit perform
              end-if
           end-perform.

      ***---
       LEGGI-PARAMETRI.
           move space to tge-chiave.
           read tparamge no lock invalid continue end-read.

      ***---
       ELABORAZIONE.
           move 0 to tmo-numero.
           add  1 to tge-data-consolid-progmag giving tmo-data-movim.
           start tmovmag key is > k-data
                 invalid
                 display message "Nessun movimento da consolidare!"
                           title titolo
                            icon 2
                 set errori to true
           end-start.

           if tutto-ok

              |RIPULISCO LA SCREEN DAL CONTATORE
              display "                          "
                 upon agmag-handle at column 34
                                        line 03
              ||||||||

              perform until 1 = 2
                 read tmovmag next at end exit perform end-read
                 if tmo-data-movim > agmag-data
                    exit perform
                 end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 50
                    move counter to counter-edit
                    display counter-edit
                       upon agmag-handle at column 34,00
                                              line  3,00
                    move 0 to counter2
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

           if tutto-ok
              if agmag-tot-mov > 0
                 move agmag-data to tge-data-consolid-progmag
                 accept tge-data-consolid-effet from century-date
                 accept tge-ora-consolid-effet from time
                 rewrite tge-rec
                         invalid
                         display message
                     "Impossibile aggiornare la data di consolidamento"
              x"0d0a""all'interno della tabella dei parametri generali."
              x"0d0a""Prendere nota e contattare assistenza"
              x"0d0a""o agire manualmente!"
                                   title "GRAVE ERRORE!!"
                                    icon 3
                 end-rewrite
                 set link-close to true
                 call   "wprogmag" using link-wprogmag
                 cancel "wprogmag"
              else
                 display message "Nessun movimento da consolidare!"
                           title titolo
                            icon 2
              end-if
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
                 read rmovmag  next at end exit perform end-read

                 if tmo-anno   not = rmo-anno   or
                    tmo-numero not = rmo-movim
                    exit perform
                 end-if
                 move agmag-user         to link-user
                 move rmo-chiave-progmag to link-key
                 move "0001111111111101" to link-array

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

      ***--
       CLOSE-FILES.
           close tmovmag rmovmag tparamge.

      ***---
       EXIT-PGM.
           goback.
