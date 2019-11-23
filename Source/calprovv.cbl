       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      calprovv.
       AUTHOR.                          Andrea.
       REMARKS. Calcolo provvigioni su ordini con numero 
                fattura già assegnato e con agente.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmovtrat.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "articoli.sl".
           copy "tmarche.sl".
           copy "agenti.sl".
           copy "lisagente.sl".
           copy "provvig.sl".
           copy "tparamge.sl".
           copy "tpiombo.sl".
           copy "param.sl".
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmovtrat.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "articoli.fd".
           copy "tmarche.fd".
           copy "agenti.fd".
           copy "lisagente.fd".
           copy "provvig.fd".
           copy "tparamge.fd".
           copy "tpiombo.fd".
           copy "param.fd".
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "imposte.def".
           copy "trova-parametro.def".
       78  titolo              value "CALCOLO PROVVIGIONI".

       77  status-tmovtrat       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-articoli       pic xx.
       77  status-tmarche        pic xx.
       77  status-agenti         pic xx.
       77  status-lisagente      pic xx.
       77  status-provvig        pic xx.
       77  status-tparamge       pic xx.
       77  status-tpiombo        pic xx.
       77  status-param          pic xx.
       77  status-clienti        pic xx.

       77  margine               pic s9(12)v99  value 0.
       77  provvigione           pic s9(12)v999 value 0.
       77  como-valore           pic s9(12)v99  value 0.
       77  como-perce            pic s9(12)v99  value 0.
       77  idx                   pic 9(3).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  esercizio             pic 9(4). 
       77  esercizio-x           pic x(4). 
       77  prezzo-listino        pic 9(6)v99.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 record-ok          value 1, false 0.
       77  filler                pic 9.
           88 valido             value 1, false 0.
       77  filler                pic 9.
           88 trovata-marca      value 1, false 0.

       LINKAGE SECTION.
       77  tot-fatt-from-provv    pic 9(5).
       77  ult-num-fatt           pic 9(8).
       77  link-user              pic x(20).
       77  link-result            pic 9.
       77  scr-oper-Handle        handle of window.

      ******************************************************************
       PROCEDURE DIVISION using tot-fatt-from-provv
                                ult-num-fatt
                                link-user
                                link-result
                                scr-oper-Handle.

       DECLARATIVES.
      ***---  
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle marche [TMARCHE] inesistente"
                          title titolo
                           icon 2                              
                set errori to true
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
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File degli articoli [ARTICOLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
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
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       AGENTI-ERR SECTION.
           use after error procedure on agenti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-agenti
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File degli agenti [AGENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [AGENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[AGENTI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       LISAGENTE-ERR SECTION.
           use after error procedure on lisagente.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-lisagente
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""File dei listini agente [LISAGENTE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [LISAGENTE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LISAGENTE] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""File delle testate [TORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
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
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""File delle righe [RORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [RORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TPIOMBO-ERR SECTION.
           use after error procedure on tpiombo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tpiombo
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""File delle testate [TPIOMBO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TPIOMBO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPIOMBO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                display message box        "Impossibile procedere."
                x"0d0a""Tabella par. generali [TPARAMGE] inesistente"
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

      ***---
       PROVVIG-ERR SECTION.
           use after error procedure on provvig.
           set tutto-ok  to true.
           evaluate status-provvig
           when "35"
                set errori to true
                display message "File [PROVVIG] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [PROVVIG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROVVIG] Indexed file corrupt!"
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
                move   "provvig"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o provvig allowing readers
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
                set errori to true
                display message "File [TMOVTRAT] not found!"
                          title titolo
                           icon 3
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
           move 0 to tot-fatt-from-provv
                     ult-num-fatt.
           set tutto-ok to true.
           accept esercizio-x from environment "ESERCIZIO".
           move   esercizio-x to esercizio.

      ***---
       OPEN-FILES.
           |Quando calcolo le provvigioni nessuno deve lvaorare
           |dentro al file PROVVIG stesso, mentre il lock per tmovtrat
           |(per la scritura dell'ultimo numero di fattura) sarà
           |gestito attraverso lock per record
           perform OPEN-IO-PROVVIG-LOCK.
           if tutto-ok  
              perform OPEN-IO-TMOVTRAT
              if tutto-ok
                 open input tordini
                            rordini
                            articoli
                            tmarche
                            agenti
                            lisagente
                            tparamge
                            tpiombo
                            param
                            clienti
                 if errori
                    close provvig tmovtrat
                 end-if
              else
                 close provvig
              end-if
           end-if.

      ***---
       OPEN-IO-PROVVIG-LOCK.
           open i-o provvig allowing readers.
           
      ***---
       OPEN-IO-TMOVTRAT.
           open i-o tmovtrat.
      
      ***---
       LEGGI-PARAMETRI.
           move space to tge-chiave.
           read tparamge  no lock invalid continue end-read.
           move esercizio to tra-anno.
           read tmovtrat  no lock 
                invalid 
                display message 
                  "Record su TMOVTRAT inesistente per anno in corso."
           x"0d0a""Impossibile procedere!!"
                          title titolo
                           icon 2
                set errori to true
            not invalid
                add 1 to tra-ult-mov-provv
           end-read.

      ***---
       ELABORAZIONE.
           move tra-anno          to tor-anno-fattura.
           move tra-ult-mov-provv to tor-num-fattura.
           start tordini key is >= k-fattura
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
                 read tordini next no lock at end exit perform end-read
                 if tor-anno-fattura not = esercizio 
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
                       display "CALPROVV COUNTER" 
                          upon scr-oper-handle at column 30
                                                    line 26
                    end-if
                    move 0 to counter2
                 end-if

                 |SCARTO LE FATTURE MANUALI COME DA CONFERMA
                 |SIG. TRIVELLA E LE FATTURE SENZA AGENTE
                 if tor-ordine     and
                    tor-cod-agente not = 0
                    |CONSIDERO SOLAMENTE GLI ORDINI GIA' FATTURATI
                    if tor-num-fattura  not = 0 and
                       tor-anno-fattura not = 0
                       move tor-cod-agente to age-codice
                       read agenti no lock
                            invalid continue
                        not invalid
                            perform LOOP-RIGHE-RORDINI
                       end-read
                    end-if
                 end-if
              end-perform

              if not trovato
                 move 0 to link-result
              else
                 move 1 to link-result
                 |SCRITTURA FILE TMOVTRAT!!!
                 set  tutto-ok to true
                 read tmovtrat lock invalid continue end-read
                 if not RecLocked
                    move ult-num-fatt to tra-ult-mov-provv
                    rewrite tra-rec invalid continue end-rewrite
                    unlock tmovtrat all record
                 end-if
              end-if

           end-if.

      ***---
       LOOP-RIGHE-RORDINI.
           move low-values   to ror-rec.
           move tor-anno     to ror-anno.
           move tor-numero   to ror-num-ordine.
           start rordini key is >= ror-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rordini  next at end exit perform end-read
                 if ror-anno       not = tor-anno  or
                    ror-num-ordine not = tor-numero
                    exit perform
                 end-if
                 set  record-ok        to false
                 move ror-cod-articolo to art-codice
                 read articoli no lock
                      invalid continue
                  not invalid set record-ok to true
                 end-read

                 if record-ok
                    initialize pvv-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move tor-anno-fattura    to pvv-anno-fat
                    move tor-num-fattura     to pvv-num-fat
                    move tor-data-fattura    to pvv-data-fat
                    move tor-cod-agente      to pvv-agente
                    move tor-cod-cli         to pvv-cliente

                    move ror-num-riga        to pvv-riga-fat

                    move ror-cod-articolo    to pvv-articolo
                    move art-unita-di-misura to pvv-um

OMAGGI              subtract ror-qta-omaggi from ror-qta

                    move ror-qta             to pvv-qta-vend
                    move art-peso-standard   to pvv-peso-um
                    move art-marca-prodotto  to pvv-marca

                    perform VALORIZZA-PREZZO-VENDITA
                    perform VALORIZZA-PREZZO-AGENTE-LISTINO
                    perform VALORIZZA-TIPO-VENDITA

                    if art-prezzo-vendita > pvv-prezzo-unit-vend
                       compute como-valore = 
                               art-prezzo-vendita -
                               pvv-prezzo-unit-vend
                       compute como-perce  =
                             ( como-valore * 100 ) / art-prezzo-vendita
                    else
                       move 0 to como-perce
                    end-if

                    move como-perce to pvv-sconto-listino
                    perform CALCOLA-PROVVIGIONE
                    perform VALORIZZA-DATI-COMUNI
                    write pvv-rec invalid rewrite pvv-rec end-write

OMAGGI              if ror-qta-omaggi not = 0
                       move 0 to ror-imponib-merce
                                 ror-imp-consumo
                                 ror-imp-cou-cobat
                       set  ror-si-omaggio to true
                       move ror-qta-omaggi to pvv-qta-vend

                       perform VALORIZZA-PREZZO-VENDITA
                       perform VALORIZZA-PREZZO-AGENTE-LISTINO
                       perform VALORIZZA-TIPO-VENDITA

                       if art-prezzo-vendita > pvv-prezzo-unit-vend
                          compute como-valore = 
                                  art-prezzo-vendita -
                                  pvv-prezzo-unit-vend
                          compute como-perce  =
                             ( como-valore * 100 ) / art-prezzo-vendita
                       else
                          move 0 to como-perce
                       end-if

                       move como-perce to pvv-sconto-listino
                       perform CALCOLA-PROVVIGIONE
                       add 90000 to pvv-riga-fat
                       write pvv-rec invalid rewrite pvv-rec end-write
OMAGGI              end-if      

                    if tor-num-fattura not = ult-num-fatt
                       move tor-num-fattura  to ult-num-fatt
                       add 1                 to tot-fatt-from-provv
                    end-if
                    set trovato to true
                 end-if
              end-perform
           end-if.

      ***---
       VALORIZZA-PREZZO-VENDITA.
           if age-prezzo-normale
              move ror-imponib-merce to pvv-prezzo-unit-vend
              add  ror-add-piombo    to pvv-prezzo-unit-vend
           else
              compute pvv-prezzo-unit-vend =
                      ror-imponib-merce    +
                      ror-imp-consumo      +
                      ror-imp-cou-cobat    +
                      ror-add-piombo
           end-if.

      ***---
       VALORE-DA-ARTICOLI.
           compute como-valore =
                   art-prezzo-vendita - 
                (( art-prezzo-vendita * art-perce-sconto-agente ) / 100)

           if ror-si-omaggio
              compute como-valore =
                      como-valore - 
                   (( como-valore * age-omaggi ) / 100)
           end-if.

      ***---
       VERIFICA-VALIDITA.
           set valido to false.
           if tor-data-ordine >= lis-data-inizio-old and
              tor-data-ordine <= lis-data-fine-old
              move lis-prezzo-old to prezzo-listino
              set valido to true
           end-if.

           if tor-data-ordine >= lis-data-inizio-new and
              tor-data-ordine <= lis-data-fine-new
              move lis-prezzo-new to prezzo-listino
              set valido to true
           end-if.

      ***---
       VALORIZZA-PREZZO-AGENTE-LISTINO.
           if age-listino = 0
              move tge-listino-promo to lis-codice
              move art-codice        to lis-articolo
              perform LEGGI-LISTINO
           else
              move age-listino to lis-codice
              move art-codice  to lis-articolo
              perform LEGGI-LISTINO
           end-if.

      ***---
       LEGGI-LISTINO.
           read lisagente no lock
                invalid
                perform VALORE-DA-ARTICOLI
                move como-valore to pvv-prezzo-netto-agente
            not invalid
                perform VERIFICA-VALIDITA
                if not valido
                   perform VALORE-DA-ARTICOLI
                   move como-valore to pvv-prezzo-netto-agente
                else
                   if age-si-add-pb
                      move tor-data-ordine to como-data-ordine tpb-data
                      move art-marca-prodotto to tpb-marca
                      move prezzo-listino     to como-prz-unitario
                      move tor-cod-cli        to como-prm-cliente
                      move tor-prg-destino    to como-prm-destino
                      perform ADDIZIONALE-PIOMBO-LISTINO
                      add add-piombo to como-prz-unitario
                          giving prezzo-listino
                   end-if
                   |VALORIZZO IL NUMERO DI LISTINO
                   move lis-codice     to pvv-num-listino
                   move prezzo-listino to pvv-prezzo-netto-agente
                end-if
           end-read.

      ***---
       ADDIZIONALE-PIOMBO-LISTINO.
           move 0 to add-piombo.
                                   
           start tpiombo key <= tpb-chiave
                 invalid continue
             not invalid
                 read tpiombo previous
                 if tpb-marca = art-marca-prodotto and
                    tpb-data <= como-data-ordine

                    accept calcolo-piombo 
                           from environment "CALCOLO_PIOMBO"
                    if nuovo-calcolo-piombo
                       perform TROVA-PARAMETRO
                    else
                       set prm-add-piombo-perce-si to true
                    end-if
                    if prm-add-piombo-perce-si                 
                       compute risultato-imposte  =
                               como-prz-unitario
                       if art-auto-cobat
                          compute add-piombo-3dec =
                          ( risultato-imposte * tpb-perce-auto ) / 100
                       else
                          compute add-piombo-3dec =
                          ( risultato-imposte * tpb-perce-moto ) / 100
                       end-if
                    else                      
                       compute add-piombo-3dec =
                               art-amperaggio * tpb-euro-ampere
                    end-if   
                 end-if
                 add 0,005 to add-piombo-3dec giving add-piombo
           end-start.  

      ***---
       VALORIZZA-TIPO-VENDITA.
           set pvv-normale to true.
           if ror-si-omaggio
              set pvv-omaggio to true
           else
              if pvv-num-listino = tge-listino-promo
                 set pvv-promo to true
              end-if
           end-if.

      ***---
       CALCOLA-PROVVIGIONE.
           if ror-si-omaggio
              compute provvigione =
                      pvv-prezzo-netto-agente * pvv-qta-vend
           else
              perform TROVA-MARCA
              if trovata-marca
                 compute provvigione =
                     ( ( pvv-prezzo-unit-vend * pvv-qta-vend ) *
                                        age-perce-marca(idx) ) / 100
              else
                 perform CALCOLO-PRINCIPALE
              end-if
           end-if.
           add 0,005 to provvigione giving pvv-val-provvig.

      ***---
       TROVA-MARCA.
           set trovata-marca to false.
           perform varying idx from 1 by 1
                     until idx > 10
              if age-marca(idx) = art-marca-prodotto
                 set trovata-marca to true
                 exit perform
              end-if
           end-perform.

      ***---
       CALCOLO-PRINCIPALE.
           compute margine = 
                 ( pvv-prezzo-unit-vend - pvv-prezzo-netto-agente )
                                        * pvv-qta-vend.
           if margine <= 0 
              if age-minimo <= 0
                 move 0 to provvigione
              else
                 compute provvigione =
                       ( pvv-qta-vend *
                         pvv-peso-um  *
                         age-minimo ) / 1936,27
              end-if
           else |OSSIA, CON MARGINE POSITIVO
              if age-marg = 0
                 move 0 to margine
              else
                 compute margine = ( margine * age-marg ) / 100
              end-if
              if age-minimo = 0
                 move margine to provvigione
              else
                 compute provvigione = margine +
                    ( pvv-qta-vend * pvv-peso-um * age-minimo )
                    / 1936,27
              end-if
           end-if.

      ***---
       VALORIZZA-DATI-COMUNI.
           if pvv-data-creazione = 0
              accept pvv-data-creazione from century-date
           end-if.
           if pvv-ora-creazione = 0
              accept pvv-ora-creazione from time
           end-if.
           if pvv-utente-creazione = space
              move link-user to pvv-utente-creazione
           end-if.

           accept pvv-data-ultima-modifica from century-date.
           accept pvv-ora-ultima-modifica  from time.
           move link-user to pvv-utente-ultima-modifica.

      ***--
       CLOSE-FILES.
           unlock provvig  all records.
           close  tmovtrat
                  tordini
                  rordini
                  lisagente
                  tparamge
                  tmarche
                  agenti
                  articoli
                  provvig
                  tpiombo
                  param
                  clienti.

      ***---
       EXIT-PGM.
           goback.                       

      ***---
       PARAGRAFO-COPY.
           copy "trova-parametro.cpy".
