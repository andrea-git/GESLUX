       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ripristino-f-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "tnotacr.sl".
           copy "tcontat.sl".

           copy "rordini.sl".

           copy "clienti.sl". 
           copy "destini.sl".
           copy "tpromo.sl".
           copy "tcaumag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd". 
           copy "tnotacr.fd".
           copy "tcontat.fd".

           copy "rordini.fd".

           copy "clienti.fd". 
           copy "destini.fd".
           copy "tpromo.fd". 
           copy "tcaumag.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".
            
      * COSTANTI
       78  titolo    value "Ripristino Documenti".

      * FILE STATUS & VARIABLES
       77  status-tordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-tcontat        pic xx.
       77  status-rordini        pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tpromo         pic xx.
       77  status-tcaumag        pic xx.

      * FLAGS
       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       01  filler                pic 9.
         88 RecLocked            value 1, false 0.

       01  filler                pic 9.
         88 record-ok            value 1, false 0.

       01  filler                pic 9.
         88 UpdateTContatLotto   value 1, false 0.

      * VARIABILI
       01  SaveK4.
         05 save-anno-fattura    pic 9(4).
         05 save-data-fattura    pic 9(8).
         05 save-num-fattura     pic 9(8).
         05 save-num-prenot      pic 9(8).
         05 save-fatt-prenotata  pic x.
         05 save-chiave          pic 9(12).

       77  tor-numero-edit       pic z(8).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       copy "link-ripristino.def".

      ******************************************************************
       PROCEDURE DIVISION USING ripristino-linkage.

       DECLARATIVES.
      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini 
           when "39"
                set errori to true
                display message "File [TORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true

           when "93"
                string   "Il file delle testate degli ordini" 
                  x"0d0a""risulta in uso su altro terminale."
                  x"0d0a""Questo comporta l'impossibilità ad"
                  x"0d0a""effettuarne il ripristino." delimited size
                      into geslock-messaggio
                end-string

                set tutto-ok   to true
                move "tordini" to geslock-nome-file

                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova open i-o tordini
                when termina set errori to true
                             display message "Operazione interrotta!"
                                       title titolo
                                        icon 2
                end-evaluate

           when "99"
                set RecLocked to true
                if tor-num-fattura = 0
                   move tor-numero to tor-numero-edit
                   string "Il documento " tor-numero-edit
                   x"0d0a""risulta in uso su altro terminale."
                   x"0d0a""Questo comporta l'impossibilità ad"
                   x"0d0a""essere ripristinato." delimited size
                     into geslock-messaggio
                   end-string
                else
                   move tor-num-fattura to tor-numero-edit
                   string "La fattura " tor-numero-edit
                   x"0d0a""risulta in uso su altro terminale."
                   x"0d0a""Questo comporta l'impossibilità ad"
                   x"0d0a""essere ripristinato." delimited size
                     into geslock-messaggio
                   end-string
                end-if

                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move "tordini"      to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova 
                     set RecLocked to false
                     read tordini lock key is k-agfatt 
                          invalid continue 
                     end-read
                when termina  
                     set errori to true
                     set UpdateTContatLotto to false
                end-evaluate
           end-evaluate.  
                          
      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "39"
                set errori to true
                display message "File [TNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TNOTACR] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
                string   "Il file delle testate delle N.C." 
                  x"0d0a""risulta in uso su altro terminale."
                  x"0d0a""Questo comporta l'impossibilità ad"
                  x"0d0a""effettuarne il ripristino." delimited size
                      into geslock-messaggio
                end-string

                set tutto-ok   to true
                move "tnotacr" to geslock-nome-file

                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova open i-o tnotacr
                when termina set errori to true
                             display message "Operazione interrotta!"
                                       title titolo
                                        icon 2
                end-evaluate

           when "99"
                set RecLocked to true
                if tno-num-fattura = 0
                   move tno-numero to tor-numero-edit
                   string "Il documento " tor-numero-edit
                   x"0d0a""risulta in uso su altro terminale."
                   x"0d0a""Questo comporta l'impossibilità ad"
                   x"0d0a""essere ripristinato." delimited size
                     into geslock-messaggio
                   end-string
                else
                   move tno-num-fattura to tor-numero-edit
                   string "La N.C. " tor-numero-edit
                   x"0d0a""risulta in uso su altro terminale."
                   x"0d0a""Questo comporta l'impossibilità ad"
                   x"0d0a""essere ripristinato." delimited size
                     into geslock-messaggio
                   end-string
                end-if

                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move "tnotacr"      to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova 
                     set RecLocked to false
                     read tnotacr lock key is k-agfatt
                          invalid continue 
                     end-read
                when termina
                     set errori to true
                     set UpdateTContatLotto to false
                end-evaluate
           end-evaluate.  

      ***---
       TCONTAT-ERR SECTION.
           use after error procedure on tcontat.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcontat
           when "39"
                set errori to true
                display message "File [TCONTAT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCONTAT] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File dei contatori [TCONTAT] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
                string   "Il file dei contatori" 
                  x"0d0a""risulta in uso su altro terminale."
                  x"0d0a""Questo comporta l'impossibilità ad"
                  x"0d0a""effettuare il ripristino." delimited size
                      into geslock-messaggio
                end-string

                set tutto-ok   to true
                move "tcontat" to geslock-nome-file

                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova open i-o tcontat
                when termina set errori to true
                             display message "Operazione interrotta!"
                                       title titolo
                                        icon 2
                end-evaluate

           when "99" 
                string   "Il record dei contatori dell'anno"
                  x"0d0a""risulta in uso su altro terminale."
                  x"0d0a""Questo comporta l'impossibilità ad"
                  x"0d0a""effettuare il ripristino." delimited size
                      into geslock-messaggio
                end-string

                set tutto-ok   to true
                move "tcontat" to geslock-nome-file

                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina

                call   "geslock" using geslock-linkage
                cancel "geslock"

                evaluate true
                when riprova read tcontat lock invalid continue end-read
                when termina set errori to true
                             display message "Operazione interrotta!"
                                       title titolo
                                        icon 2
                end-evaluate
           end-evaluate.  

      ***---

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
           set RecLocked to false.
           set tutto-ok  to true.

      ***---
       OPEN-FILES.
           open input clienti destini tpromo tcaumag.
           if ripr-note open i-o tnotacr
           else         open i-o tordini
           end-if.
           if tutto-ok
              open i-o tcontat
              if errori
                 if ripr-note close tnotacr
                 else         close tordini
                 end-if
              end-if
           end-if.
      
      ***---
       ELABORAZIONE.
           move ripr-anno to con-anno.
           read tcontat lock invalid continue end-read.
           move con-ult-num-pren to ripr-lotto.
           if tutto-ok
              if ripr-note perform RIPRISTINO-NOTE
              else         perform RIPRISTINO-FATTURE
              end-if
              if ripr-tot-fat > 0
                 if ripr-FirstNumber not = 0
                    if ripr-note
                       subtract 1 from ripr-FirstNumber
                                giving con-ult-num-nc-fisc
                    else
                       subtract 1 from ripr-FirstNumber
                                giving con-num-fatt
                    end-if
                 end-if
                 if UpdateTContatLotto
                    subtract 1 from con-ult-num-pren
                 else
                    display message "Effettuare di nuovo il ripristino"
                         x"0d0a""al fine di mantenere la progressività"
                         x"0d0a""fiscale dei documenti"
                              title "IMPORTANTE!!!"
                               icon 3
                 end-if
                 accept con-data-ultima-modifica from century-date
                 accept con-ora-ultima-modifica  from time
                 move ripr-user   to con-utente-ultima-modifica
                 rewrite con-rec invalid continue end-rewrite
                 unlock tcontat all records
              else
                 if ripr-tot-doc > 0
                    if UpdateTContatLotto
                       subtract 1 from con-ult-num-pren
                       accept con-data-ultima-modifica from century-date
                       accept con-ora-ultima-modifica  from time
                       move ripr-user   to con-utente-ultima-modifica
                       rewrite con-rec invalid continue end-rewrite
                    else
                       display message 
                               "Effettuare di nuovo il ripristino"
                        x"0d0a""al fine di mantenere la progressività"
                        x"0d0a""fiscale dei documenti"
                                 title "IMPORTANTE!!!"
                                  icon 3
                    end-if
                    unlock tcontat all records
                 end-if
              end-if
           end-if.

      ***---
       RIPRISTINO-NOTE.
           set  UpdateTContatLotto to true.
           move ripr-anno          to tno-anno-fattura.
           move ripr-lotto         to tno-num-prenot.
           move high-value         to tno-data-fattura tno-num-fattura.
           move high-value         to tno-chiave.
           set  tno-fatt-si-prenotata to true.
      
           |Siccome il lotto da ripristinare è quasi sempre
           |l'ultimo numero parto dall'ultima fattura a vado a ritroso
           start tnotacr key is <= k-agfatt
              invalid set errori to true 
           end-start.

           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                          "
              upon ripr-handle at column 08
                                    line 03
           ||||||||

           if tutto-ok
              perform until 1 = 2
              
                 read tnotacr previous no lock 
                      at end exit perform 
                 end-read
                 
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 20
                    move 0 to counter2          
                    move counter to counter-edit
                    display counter-edit
                       upon ripr-handle at column 12
                                             line 03
                 end-if
      
                 if tno-anno-fattura not = ripr-anno
                    exit perform 
                 end-if

                 |I lotti sono tutti vicini e, trovatone uno 
                 |(valorizzo la data come se fosse un flag), 
                 |se trovo un lotto diverso, esco dal ciclo
                 if ripr-data not = 0
                    if tno-num-prenot not = ripr-lotto
                       exit perform
                    end-if
                 end-if

                 if tno-no-agg-contab         and
                    tno-data-fattura  not = 0 and
                    tno-num-prenot        = ripr-lotto

                    perform READ-TNOTACR-LOCK
                    if not RecLocked
                       perform NC-IN-DOCUMENTO
                       perform RIPRISTINA-CURSORE-TNOTACR
                    else
                       exit perform
                    end-if
                 end-if
      
              end-perform
           end-if.

      ****---
       READ-TNOTACR-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
           read tnotacr lock key is k-agfatt invalid continue end-read.

      ***---
       NC-IN-DOCUMENTO.
LUBEXX     add 1 to ripr-tot-doc.
           move tno-anno-fattura   to save-anno-fattura.
           move tno-data-fattura   to save-data-fattura.
           move tno-num-fattura    to save-num-fattura.
           move tno-num-prenot     to save-num-prenot.
           move tno-fatt-prenotata to save-fatt-prenotata.
           move tno-chiave         to save-chiave.

           set ripr-si-eseguita to true.
           if ripr-data = 0
              move tno-data-fattura to ripr-data
           end-if.
           if tno-num-fattura not = 0
              add 1 to ripr-tot-fat
              if tno-invio-postel
                 add 1 to ripr-tot-postel
              end-if
              if tno-invio-edi
                 move tno-causale to tca-codice
                 read tcaumag no lock
                 if tca-causale-edi not = spaces
                    add 1 to ripr-tot-edi
                 end-if
              end-if
              if ripr-LastNumber = 0
                 move tno-num-fattura to ripr-LastNumber
              end-if
              move tno-num-fattura to ripr-FirstNumber
           end-if.
           move 0 to tno-anno-fattura.
           move 0 to tno-data-fattura.
           move 0 to tno-num-fattura.
           move 0 to tno-num-prenot.
           accept tno-data-ultima-modifica from century-date.
           accept tno-ora-ultima-modifica  from time.
           move ripr-user to tno-utente-ultima-modifica.
           rewrite tno-rec invalid continue end-rewrite.
           unlock tnotacr all records.

      ***---
       RIPRISTINA-CURSORE-TNOTACR.
           move save-anno-fattura   to tno-anno-fattura.
           move save-data-fattura   to tno-data-fattura.
           move save-num-fattura    to tno-num-fattura.
           move save-num-prenot     to tno-num-prenot.
           move save-fatt-prenotata to tno-fatt-prenotata.
           move save-chiave         to tno-chiave.

           start tnotacr key is <= k-agfatt
                 invalid continue
           end-start.

      ***---
       RIPRISTINO-FATTURE.
           set  UpdateTContatLotto to true.
           move ripr-anno          to tor-anno-fattura.
           move ripr-lotto         to tor-num-prenot.
           move high-value         to tor-data-fattura tor-num-fattura.
           move high-value         to tor-chiave.
           set tor-fatt-si-prenotata to true.
      
           |Siccome il lotto da ripristinare è quasi sempre
           |l'ultimo numero parto dall'ultima fattura a vado a ritroso
           start tordini key is <= k-agfatt
              invalid set errori to true 
           end-start.

           |RIPULISCO LA SCREEN DAL CONTATORE
           display "                          "
              upon ripr-handle at column 08
                                    line 03
           ||||||||

           if tutto-ok
              perform until 1 = 2

                 read tordini previous no lock
                      at end exit perform
                 end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 20
                    move 0 to counter2
                    move counter to counter-edit
                    display counter-edit
                       upon ripr-handle at column 12
                                             line 03
                 end-if

                 if tor-anno-fattura not = ripr-anno
                    exit perform
                 end-if

                 |Dato che ogni documento del lotto ha la stessa data
                 |fattura, come questa cambia, esco dal ciclo
                 if ripr-data not = 0
                    if ripr-data not = tor-data-fattura
                       exit perform
                    end-if
                 end-if

                 if tor-no-agg-contab         and
                    tor-data-fattura  not = 0 and
                    tor-num-prenot        = ripr-lotto

                    if ripr-merce
                       if tor-ordine set record-ok to true
                       else          set record-ok to false
                       end-if
                    else
                       if tor-ordine set record-ok to false
                       else          set record-ok to true
                       end-if
                    end-if

                    if record-ok
                       perform READ-TORDINI-LOCK
                       if not RecLocked
                          perform FATTURA-IN-BOLLA
                          perform RIPRISTINA-CURSORE-TORDINI
                       else
                          exit perform
                       end-if
                    end-if

                 end-if
      
              end-perform
           end-if.

      ****---
       READ-TORDINI-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
           read tordini lock key is k-agfatt invalid continue end-read.

      ***---
       FATTURA-IN-BOLLA.
LUBEXX     add 1 to ripr-tot-doc.
           move tor-anno-fattura   to save-anno-fattura.
           move tor-data-fattura   to save-data-fattura.
           move tor-num-fattura    to save-num-fattura.
           move tor-num-prenot     to save-num-prenot.
           move tor-fatt-prenotata to save-fatt-prenotata.
           move tor-chiave         to save-chiave.

           set ripr-si-eseguita to true.
           if ripr-data = 0
              move tor-data-fattura to ripr-data
           end-if.
           if tor-num-fattura not = 0
              add 1 to ripr-tot-fat
              if tor-invio-postel
                 add 1 to ripr-tot-postel
              end-if
              if tor-invio-edi        
                 move tor-causale to tca-codice
                 read tcaumag no lock
                 if tca-causale-edi not = spaces
                    add 1 to ripr-tot-edi
                 end-if
              end-if
              if ripr-LastNumber = 0
                 move tor-num-fattura to ripr-LastNumber
              end-if
              move tor-num-fattura to ripr-FirstNumber
           end-if.
           move 0 to tor-anno-fattura.
           move 0 to tor-data-fattura.
           move 0 to tor-num-fattura.
           move 0 to tor-num-prenot.
           accept tor-data-ultima-modifica from century-date.
           accept tor-ora-ultima-modifica  from time.
           move ripr-user to tor-utente-ultima-modifica.
           rewrite tor-rec invalid continue end-rewrite.
           unlock tordini all records.

      ***---
       RIPRISTINA-CURSORE-TORDINI.
           move save-anno-fattura   to tor-anno-fattura.
           move save-data-fattura   to tor-data-fattura.
           move save-num-fattura    to tor-num-fattura.
           move save-num-prenot     to tor-num-prenot.
           move save-fatt-prenotata to tor-fatt-prenotata.
           move save-chiave         to tor-chiave.

           start tordini key is <= k-agfatt
                 invalid continue
           end-start.

      ***---
       CLOSE-FILES.
           if ripr-note close tnotacr
           else         close tordini
           end-if.
           close tcontat clienti destini tpromo tcaumag.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
