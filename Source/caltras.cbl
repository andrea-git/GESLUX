       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      caltras.
       AUTHOR.                          Andrea.
       REMARKS. LE MODIFICHE IMPORTANTI VANNO RIPORTATE ANCHE SU RICALTRAS!!!!
                Calcolo trasporti su movimenti di magazzino aventi
                causali con flag "TRASPORTO" attivo. Riempio il file 
                trasporti senza andare MAI in rewrite. Nel caso avessi
                due volte la stessa combinazione Num - Anno Bolla devo
                gestirla con un progressivo.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovtrat.sl".
           copy "trasporti.sl".
           copy "tvettori.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tarifvet.sl".
           copy "tcaumag.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tprov.sl".
           copy "articoli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmovtrat.fd".
           copy "trasporti.fd".
           copy "tvettori.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "tarifvet.fd".
           copy "tcaumag.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tprov.fd".
           copy "articoli.fd".

       WORKING-STORAGE SECTION.
      * COPY
           copy "link-geslock.def".

      * COSTANTI
       78  titolo              value "CALCOLO TRASPORTI".

      *FILE-STATUS
       77  status-tmovtrat       pic xx.
       77  status-trasporti      pic xx.
       77  status-tvettori       pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tarifvet       pic xx.
       77  status-tcaumag        pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-tprov          pic xx.
       77  status-articoli       pic xx.

      * VARIABILI
       77  comodo1               pic 9(3).
       77  como-arrot            pic 9(9)v99.
       77  como-idx              pic 9(5).
       77  idx                   pic 9(5).
       77  tot-peso-kg           pic 9(9)v999.
       77  tot-peso-kg-SHI       pic 9(9)v999.
       77  tot-peso-kg-GET       pic 9(9)v999. 

       77  tot-peso-qli-arrot    pic 9(9)v99.
       77  s-tot-peso-qli        pic 9(9)v999999.
       01  s-tot-peso-qli-red    redefines s-tot-peso-qli.
           05 cifra              pic 9 occurs 15.
       77  tot-peso-qli          pic 9(9)v999999.
       77  tot-peso-qli-SHI      pic 9(9)v999999.
       77  tot-peso-qli-GET      pic 9(9)v999999. 
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  esercizio             pic 9(4).
       77  esercizio-x           pic x(4).

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
LUBEXX 77  filler                pic 9.
LUBEXX     88 trovata-tariffa    value 1, false 0.
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 record-ok          value 1, false 0.
       77  filler                pic 9.
           88 EsisteDestino      value 1, false 0.
       77  filler                pic 9.
           88 esiste-scaglione   value 1, false 0.
       77  filler                pic 9.
           88 trovato-scaglione  value 1, false 0.
       77  tipo-arrot            pic 9.
           88 nessuno            value 1.
           88   10-kg            value 2.
           88   20-kg            value 3.
           88   50-kg            value 4.
           88  100-kg            value 5.         

       LINKAGE SECTION.
       77  tot-mov-from-tras      pic 9(5).
       77  ult-num-mov            pic 9(8).
       77  link-user              pic x(20).
       77  link-result            pic 9.
       77  scr-oper-handle        handle of window.
       77  caltras-data-from      pic 9(8).
       77  caltras-data-to        pic 9(8).
       77  link-vettore           pic 9(5).

      ******************************************************************
       PROCEDURE DIVISION using tot-mov-from-tras
                                ult-num-mov
                                link-user
                                link-result
                                scr-oper-handle
                                caltras-data-from
                                caltras-data-to
                                link-vettore.
       DECLARATIVES.

      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Vettori [TVETTORI] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
           when "39"
                set errori to true
                display message "File [TVETTORI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TVETTORI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---  
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Causali [TCAUMAG] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
           when "39"
                set errori to true
                display message "File [TCAUMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---  
       TPROV-ERR SECTION.
           use after error procedure on tprov.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tprov
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella Province [TPROV] inesistente"
                          title titolo
                           icon 2                              
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

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File dei Clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File dei destini [DESTINI] inesistente"
                          title titolo
                           icon 2
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

      ***---
       TARIFVET-ERR SECTION.
           use after error procedure on tarifvet.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tarifvet
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella Tariffari vettori [TARIFVET] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TARIFVET] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TARIFVET] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TRASPORTI-ERR SECTION.
           use after error procedure on trasporti.
           set tutto-ok  to true.
           evaluate status-trasporti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File trasporti [TRASPORTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TRASPORTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TRASPORTI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "trasporti"  to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o trasporti allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       TMOVTRAT-ERR SECTION.
           use after error procedure on tmovtrat.
           set tutto-ok  to true.
           evaluate status-tmovtrat 
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [TMOVTRAT] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TMOVTRAT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVTRAT] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "tmovtrat"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o tmovtrat
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           when "99"
                initialize geslock-messaggio
                string "Il record per l'anno in corso risulta"
                x"0d0a""in uso su altro terminale."
                x"0d0a""Questo comporta l'impossibilità ad aggiornarlo"
                x"0d0a""con il numero di ultima fattura trattata."
                      delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 1 to geslock-v-ignora
                move 0 to geslock-v-termina
                move   "tmovtrat"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     read tmovtrat lock
                when ignora
                     set errori to true
                     read tmovtrat no lock
                end-evaluate
           end-evaluate.

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella [TMOVMAG] inesistente"
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

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message "Impossibile procedere."
               x"0d0a""Tabella [RMOVMAG] inesistente"
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

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform LEGGI-PARAMETRI
              if tutto-ok
                 perform ELABORAZIONE
              end-if
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to counter counter2.
           move 0 to tot-mov-from-tras
                     ult-num-mov.
           set tutto-ok to true.
           accept esercizio-x from environment "ESERCIZIO".
           move   esercizio-x to esercizio.

      ***---
       OPEN-FILES.
           |Quando calcolo i trasporti nessuno deve lvaorare
           |dentro al file TRASPORTI stesso, mentre il lock per tmovtrat
           |(per la scritura dell'ultimo numero di fattura) sarà
           |gestito attraverso lock per record
           perform OPEN-IO-TRASPORTI-LOCK.
           if tutto-ok  
              perform OPEN-IO-TMOVTRAT
              if tutto-ok
                 open input clienti
                            destini
                            tvettori
                            tarifvet
                            tcaumag
                            tmovmag
                            rmovmag
                            tprov
                            articoli
                 if errori
                    close trasporti tmovtrat
                 end-if
              else
                 close trasporti
              end-if
           end-if.

      ***---
       OPEN-IO-TRASPORTI-LOCK.
           open i-o trasporti allowing readers.
           
      ***---
       OPEN-IO-TMOVTRAT.
           open i-o tmovtrat.
      
      ***---
       LEGGI-PARAMETRI.
           move esercizio to tra-anno.
           read tmovtrat no lock 
                invalid 
                display message 
                  "Record su TMOVTRAT inesistente per anno in corso."
           x"0d0a""Impossibile procedere!!"
                          title titolo
                           icon 2
                set errori to true
            not invalid
                add 1 to tra-ult-mov-tras
           end-read.

      ***---
       ELABORAZIONE.
           move tra-anno         to tmo-anno
           move tra-ult-mov-tras to tmo-numero
           start tmovmag key is >= tmo-chiave
                 invalid
                 set errori to true
           end-start.

           if tutto-ok

              |RIPULISCO LA SCREEN DAL CONTATORE
              display "                          "
                 upon scr-oper-handle at column 34
                                           line 25
              display "                          "
                 upon scr-oper-handle at column 30
                                           line 26
              ||||||||

              perform until 1 = 2
                 set record-ok to false
                 read tmovmag next no lock at end exit perform end-read

                 if tmo-anno not = esercizio 
                    exit perform 
                 end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon scr-oper-handle at column 34
                                              line 25
                    if counter = 100
                       display " CALTRAS COUNTER" 
                          upon scr-oper-handle at column 30
                                                    line 26
                    end-if
                    move 0 to counter2
                    
                 end-if
                 if tmo-cliente
                    |CONSIDERO SOLAMENTE I MOVIMENTI GIA' BOLLETTATI
                    if tmo-numdoc-clifor not = 0 and
                       tmo-data-doc      not = 0 and
                       tmo-data-fattura  not = 0
                       set  cli-tipo-C     to true
                       move tmo-cod-clifor to cli-codice
                       read clienti no lock
                            invalid continue
                        not invalid
                            set EsisteDestino to false
                            if tmo-destino = 0
                               set record-ok       to true
                            else
                               set EsisteDestino   to true
                               move tmo-cod-clifor to des-codice
                               move tmo-destino    to des-prog
                               read destini no lock
                                    invalid continue
                                not invalid set record-ok to true
                               end-read
                            end-if
                       end-read
                       if record-ok
                          move tmo-causale to tca-codice
                          read tcaumag no lock invalid continue end-read
                          if tca-si-tras
                             if EsisteDestino
                                move des-prov  to prv-codice
                             else
                                move cli-prov  to prv-codice
                             end-if
                             read tprov no lock
                                  invalid move spaces to prv-regione
                             end-read
                             perform LOOP-RIGHE-MOVIMENTO
                          end-if
                       end-if
                       call "w$flush"
                    end-if
                 end-if
              end-perform

              if not trovato
                 move 0 to link-result
              else
                 move 1 to link-result      
                 set  tutto-ok to true
                 |SCRITTURA FILE TMOVTRAT!!!
                 read tmovtrat lock invalid continue end-read
                 if not RecLocked
                    move ult-num-mov to tra-ult-mov-tras
                    rewrite tra-rec invalid continue end-rewrite
                    unlock tmovtrat all record
                 end-if
              end-if
           
           end-if.

      ***---
       LOOP-RIGHE-MOVIMENTO.
           move low-values to rmo-rec.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           start rmovmag  key is >= rmo-chiave
                 invalid  set errori to true
           end-start.

           if tutto-ok
              move tmo-causale to tca-codice
              read tcaumag  no lock
              set record-ok to false
              move 0 to tot-peso-kg
              perform until 1 = 2
                 read rmovmag next at end exit perform end-read
                 if rmo-anno  not = tmo-anno   or
                    rmo-movim not = tmo-numero
                    exit perform
                 end-if
                 set record-ok to true
                 compute tot-peso-kg =
                         tot-peso-kg  +
                         rmo-peso-tot + 
                         rmo-peso-tot-utf
                 if tca-cod-magaz = "SHI"
                    move rmo-articolo to art-codice
                    read articoli no lock
                    compute tot-peso-kg-SHI =
                            tot-peso-kg-SHI +
                          ( rmo-qta * art-peso-SHI )
                 end-if
                 if tca-cod-magaz = "GET"
                    move rmo-articolo to art-codice
                    read articoli no lock
                    compute tot-peso-kg-GET =
                            tot-peso-kg-GET +
                          ( rmo-qta * art-peso-GET )
                 end-if
              end-perform
           end-if.

           if record-ok
              initialize trs-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces

              move tmo-anno           to trs-anno
              move tmo-numdoc-clifor  to trs-num-bolla

              move tmo-data-doc       to trs-data-bolla
              move tmo-data-fattura   to trs-data-fattura
              move tmo-num-fattura    to trs-num-fattura
              move tmo-vettore        to trs-vettore
              move tmo-cod-clifor     to trs-cliente
              move tmo-destino        to trs-destino
              move prv-codice         to trs-provincia
              move prv-regione        to trs-regione
              move tot-peso-kg        to trs-qta-kg
              move tot-peso-kg-SHI    to trs-qta-kg-SHI
              move tot-peso-kg-GET    to trs-qta-kg-GET

              perform CALCOLA-QTA-ARROTONDATA-TARIFFA
              perform TROVA-TARIFFA-E-VALORIZZA-CAMPO
              perform CALCOLA-QTA-ARROTONDATA-TARIFFA-SHI
              perform TROVA-TARIFFA-E-VALORIZZA-CAMPO-SHI
              perform CALCOLA-QTA-ARROTONDATA-TARIFFA-GET
              perform TROVA-TARIFFA-E-VALORIZZA-CAMPO-GET
              perform VALORIZZA-DATI-COMUNI

              perform varying trs-prog-bolla from 1 by 1
                        until trs-prog-bolla > 999999999

                 write trs-rec
                       invalid add 1 to trs-prog-bolla
                   not invalid
                       if tmo-numero not = ult-num-mov
                          move tmo-numero to ult-num-mov
                          add  1          to tot-mov-from-tras
                       end-if
                       set trovato to true
                       exit perform
                 end-write
                 call "w$flush"

              end-perform
           end-if.

      ***---
       VALORIZZA-DATI-COMUNI.
           if trs-data-creazione = 0
              accept trs-data-creazione from century-date
           end-if.
           if trs-ora-creazione = 0
              accept trs-ora-creazione from time
           end-if.
           if trs-utente-creazione = space
              move link-user to trs-utente-creazione
           end-if.

           accept trs-data-ultima-modifica from century-date.
           accept trs-ora-ultima-modifica  from time.
           move link-user to trs-utente-ultima-modifica.

      ***---
       TROVA-TARIFFA-E-VALORIZZA-CAMPO.
           move 0          to trs-tariffa.
           move low-value  to tfv-rec.
           move vet-codice to tfv-codice.

           start tarifvet key is >= tfv-chiave
                 invalid continue
             not invalid
LUBEXX           set trovata-tariffa to false
                 perform until 1 = 2
                    read tarifvet next no lock
                         at end exit perform
                    end-read
                    if tfv-codice not = vet-codice
                       exit perform
                    end-if

LUBEXX              evaluate true
LUBEXX              when vet-regione
LUBEXX                   if prv-regione = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-prov
LUBEXX                   if prv-codice = tfv-prov
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-cliente
LUBEXX                   if cli-codice = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-clides
LUBEXX                   if des-codice = tfv-campo1 and
LUBEXX                      des-prog   = tfv-campo2
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              end-evaluate

LUBEXX              if trovata-tariffa
                       if trs-qta-arrot >= tfv-qli-da and
                          trs-qta-arrot <= tfv-qli-a
                          move tfv-euro to trs-tariffa
                          exit perform
                       end-if
LUBEXX              end-if

                 end-perform
           end-start.

      ***---
       TROVA-TARIFFA-E-VALORIZZA-CAMPO-SHI.
           move 0          to trs-tariffa-SHI.
           move low-value  to tfv-rec.
           move vet-codice to tfv-codice.

           start tarifvet key is >= tfv-chiave
                 invalid continue
             not invalid
LUBEXX           set trovata-tariffa to false
                 perform until 1 = 2
                    read tarifvet next no lock
                         at end exit perform
                    end-read
                    if tfv-codice not = vet-codice
                       exit perform
                    end-if

LUBEXX              evaluate true
LUBEXX              when vet-regione
LUBEXX                   if prv-regione = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-prov
LUBEXX                   if prv-codice = tfv-prov
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-cliente
LUBEXX                   if cli-codice = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-clides
LUBEXX                   if des-codice = tfv-campo1 and
LUBEXX                      des-prog   = tfv-campo2
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              end-evaluate

LUBEXX              if trovata-tariffa
                       if trs-qta-arrot-SHI >= tfv-qli-da and
                          trs-qta-arrot-SHI <= tfv-qli-a
                          move tfv-euro to trs-tariffa-SHI
                          exit perform
                       end-if
LUBEXX              end-if

                 end-perform
           end-start.

      ***---
       TROVA-TARIFFA-E-VALORIZZA-CAMPO-GET.
           move 0          to trs-tariffa-GET.
           move low-value  to tfv-rec.
           move vet-codice to tfv-codice.

           start tarifvet key is >= tfv-chiave
                 invalid continue
             not invalid
LUBEXX           set trovata-tariffa to false
                 perform until 1 = 2
                    read tarifvet next no lock
                         at end exit perform
                    end-read
                    if tfv-codice not = vet-codice
                       exit perform
                    end-if

LUBEXX              evaluate true
LUBEXX              when vet-regione
LUBEXX                   if prv-regione = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-prov
LUBEXX                   if prv-codice = tfv-prov
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-cliente
LUBEXX                   if cli-codice = tfv-campo1
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              when vet-clides
LUBEXX                   if des-codice = tfv-campo1 and
LUBEXX                      des-prog   = tfv-campo2
LUBEXX                      set trovata-tariffa to true
LUBEXX                   end-if
LUBEXX              end-evaluate

LUBEXX              if trovata-tariffa
                       if trs-qta-arrot-GET >= tfv-qli-da and
                          trs-qta-arrot-GET <= tfv-qli-a
                          move tfv-euro to trs-tariffa-GET
                          exit perform
                       end-if
LUBEXX              end-if

                 end-perform
           end-start.

      ***--
       CLOSE-FILES.
           unlock trasporti all records.
           close  tmovtrat
                  trasporti
                  clienti
                  destini
                  tarifvet
                  tcaumag
                  tmovmag
                  rmovmag
                  tprov
                  articoli.
      
      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "caltras-arrot.cpy".
