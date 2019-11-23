       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      chvet-p.
       AUTHOR.                          Andrea.
       REMARKS. Sostituisce il vettore indicato in chvet-old-vet con 
                chvet-new-vet nei clienti / destini avente la 
                regione/provincia indicata .
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tprov.sl".
           copy "clienti.sl". 
           copy "destini.sl". 

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tprov.fd".
           copy "clienti.fd". 
           copy "destini.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo                value "Cambio veloce vettori".

      * FILE STATUS
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tprov          pic xx.

      * VARIABILI
       77  codice-edit           pic z(5).
       77  prog-edit             pic z(5).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

      * FLAGS
       77  filler                pic 9.
           88  record-ok         value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.

       LINKAGE SECTION.
       copy "link-chvet.def".

      ******************************************************************
       PROCEDURE DIVISION using chvet-linkage.

       DECLARATIVES.
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "File [CLIENTI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [CLIENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.
       
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                display message "File [DESTINI] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [DESTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DESTINI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       TPROV-ERR SECTION.
           use after error procedure on tprov.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tprov
           when "35"
                display message "File [TPROV] not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File [TPROV] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPROV] Indexed file corrupt!"
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
           set tutto-ok  to true.
           set trovato   to false.
           set RecLocked to false.
           move 0        to chvet-c-counter.
           move 0        to chvet-d-counter.

      ***---
       OPEN-FILES.
           open input tprov.
           if tutto-ok
              perform OPEN-IO-CLIENTI
              if tutto-ok
                 perform OPEN-IO-DESTINI
                 if errori
                    close clienti tprov
                 end-if
              else
                 close tprov
              end-if
           end-if.

           if errori goback end-if.

      ***---
       OPEN-IO-CLIENTI.
           set tutto-ok  to true.
           open i-o clienti.
           if RecLocked
              initialize geslock-linkage
              move "clienti" to geslock-nome-file
              move "File Clienti in uso su altro terminale."
                to geslock-messaggio
              move 1 to geslock-v-termina
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-IO-CLIENTI
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       OPEN-IO-DESTINI.
           move "destini" to geslock-nome-file.

           set tutto-ok  to true.
           open i-o destini.
           if RecLocked
              initialize geslock-linkage
              move "File Destini in uso su altro terminale."
                to geslock-messaggio
              move 1 to geslock-v-termina
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform OPEN-IO-DESTINI
              when other   display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           end-if.

      ***---
       ELABORAZIONE.
           perform ELABORA-CLIENTI.
           perform ELABORA-DESTINI.

      ***---
       ELABORA-CLIENTI.
           move 0        to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                          "
              upon chvet-handle at column 09
                                     line 03.
           ||||||||
           set tutto-ok   to true.
           move low-value to cli-rec.
           set cli-tipo-C to true.
           start clienti key is >= cli-chiave
                 invalid set errori to true
           end-start.
           perform until 1 = 2
              set record-ok to false
              read clienti next no lock at end exit perform end-read

              add 1 to counter
              add 1 to counter2
              if counter2 = 100
                 move counter to counter-edit
                 display counter-edit
                    upon chvet-handle at column 12
                                           line 03
                 if counter = 100
                    display "CLI"
                       upon chvet-handle at column 09
                                              line 03
                 end-if
                 move 0 to counter2
              end-if

              if cli-tipo-F exit perform end-if
              if chvet-prov = spaces
                 move cli-prov to prv-codice
                 read tprov no lock invalid continue
                  not invalid
                      if prv-regione = chvet-reg
                         set record-ok to true
                      end-if   
                 end-read
              else
                 if cli-prov = chvet-prov
                    set record-ok to true
                 end-if
              end-if
LUBEXX        if record-ok
LUBEXX           if chvet-tipo not = spaces and
LUBEXX              chvet-tipo not = cli-tipo
LUBEXX              set record-ok to false
LUBEXX           end-if
LUBEXX        end-if
              if record-ok
                 if chvet-old-vet = 0
                    set record-ok to true
                 else
                    if cli-vettore not = chvet-old-vet
                       set record-ok to false
                    end-if
                 end-if
              end-if
              if record-ok
                 perform AGGIORNA-CLIENTE
                 if errori exit perform end-if
              end-if
           end-perform.

      ***---
       AGGIORNA-CLIENTE.
           set RecLocked to false.
           set tutto-ok  to true.
           read clienti lock invalid continue end-read.
           if RecLocked
              initialize geslock-linkage
              move cli-codice to codice-edit
              move "clienti"  to geslock-nome-file
              string "Cliente "     delimited size
                     codice-edit    delimited size
                     " già in uso." delimited size
                     into geslock-messaggio
              end-string
              move 1 to geslock-v-termina
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform AGGIORNA-CLIENTE
              when ignora  read clienti no lock
              when termina set errori   to true
                           display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           else
              move chvet-new-vet to cli-vettore
              rewrite cli-rec invalid continue end-rewrite
              unlock clienti all record
              add 1 to chvet-c-counter
           end-if.                 

      ***---
       ELABORA-DESTINI.
           move 0        to counter counter2.
           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                          "
              upon chvet-handle at column 09
                                     line 03.
           ||||||||
           set tutto-ok   to true.
           move low-value to des-rec.
           start destini key is >= des-chiave
                 invalid set errori to true
           end-start.
           perform until 1 = 2
              set record-ok to false
              read destini next no lock at end exit perform end-read

              add 1 to counter
              add 1 to counter2
              if counter2 = 100
                 move counter to counter-edit
                 display counter-edit
                    upon chvet-handle at column 12
                                           line 03
                 if counter = 100
                    display "DES"
                       upon chvet-handle at column 09
                                              line 03
                 end-if
                 move 0 to counter2
              end-if

              if chvet-prov = spaces
                 move des-prov to prv-codice
                 read tprov no lock invalid continue
                  not invalid
                      if prv-regione = chvet-reg
                         set record-ok to true
                      end-if   
                 end-read
              else
                 if des-prov = chvet-prov
                    set record-ok to true
                 end-if
              end-if
              if record-ok
                 move des-codice to cli-codice
                 set  cli-tipo-C to true
                 read clienti no lock 
                      invalid set record-ok to false
                 end-read
LUBEXX           if record-ok
LUBEXX              if chvet-tipo not = spaces and
LUBEXX                 chvet-tipo not = cli-tipo
LUBEXX                 set record-ok to false
LUBEXX              end-if
LUBEXX           end-if
              end-if
              if record-ok
                 if chvet-old-vet = 0
                    set record-ok to true
                 else
                    if des-vettore not = chvet-old-vet
                       set record-ok to false
                    end-if
                 end-if
              end-if
              if record-ok
                 perform AGGIORNA-DESTINO
                 if errori exit perform end-if
              end-if
           end-perform.

      ***---
       AGGIORNA-DESTINO.
           set RecLocked to false.
           set tutto-ok  to true.
           read destini lock invalid continue end-read.
           if RecLocked
              initialize geslock-linkage
              move des-codice to codice-edit
              move des-prog   to prog-edit
              move "destini"  to geslock-nome-file
              string  "Cliente " codice-edit,
               x"0d0a""Destino " prog-edit
               x"0d0a""Già in uso su altro terminale" delimited size
                    into geslock-messaggio
              end-string
              move 1 to geslock-v-termina
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              call   "geslock" using geslock-linkage
              cancel "geslock"
              
              evaluate true
              when riprova perform AGGIORNA-DESTINO
              when ignora  read clienti no lock
              when termina set errori   to true
                           display message box "Operazione interrotta!"
                                   title titolo
                                   icon 2
              end-evaluate
           else
              move chvet-new-vet to des-vettore
              rewrite des-rec invalid continue end-rewrite
              unlock destini all record
              add 1 to chvet-d-counter
           end-if.

      ***---
       CLOSE-FILES.
           close clienti destini tprov.

      ***---
       EXIT-PGM.
           goback.
