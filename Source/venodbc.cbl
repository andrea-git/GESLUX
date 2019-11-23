       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      venodbc.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tcaumag.sl".
           copy "venodbc.sl".
           copy "tgrupgdo.sl".
           copy "articoli.sl".
           copy "tmarche.sl".
           copy "clienti.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tcaumag.fd".
           copy "venodbc.fd".
           copy "tgrupgdo.fd".
           copy "articoli.fd".
           copy "tmarche.fd".
           copy "clienti.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "ttipocli.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "comune.def".
       copy "link-geslock.def".
       copy "common-excel.def".
       copy "acugui.def".
       copy "recupero-addizionale.def".

      * COSTANTI
       78  titolo       value "Venduto singolo prodotto per gruppo GDO".

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-tcaumag        pic xx.
       77  status-venodbc        pic xx.
       77  status-tgrupgdo       pic xx.
       77  status-articoli       pic xx.
       77  status-tmarche        pic xx.
       77  status-clienti        pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-ttipocli       pic xx.

       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).

      * VARIABILI
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  prezzo-medio          pic s9(12)v99.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  user-codi             pic x(20).

      * FLAGS
       01  filler                pic 9.
         88 prima-volta          value 1 false 0.

       01  filler                pic 9.
         88 record-ok            value 1 false 0.

      * RIGHE PER LA STAMPA
       01  rec-edit.
         05 r-gdo-codice         pic x(5).
         05 r-gdo-intestazione   pic x(50).
         05 r-art-codice         pic z(6).
         05 r-art-descrizione    pic x(50).
         05 r-qta                pic ---.---.--9.
         05 r-prezzo-medio       pic ----.---.---.--9,99.
         05 r-imp                pic ----.---.---.--9,99.
         05 r-add-pb             pic ----.---.---.--9,99.
         05 r-tot-imp            pic ----.---.---.--9,99.
         05 r-mar-descrizione    pic x(50).
         05 r-data-from          pic x(10).
         05 r-data-to            pic x(10).

       LINKAGE SECTION.
       copy "link-premigdo.def".
      ******************************************************************
       PROCEDURE DIVISION using premigdo-linkage.
       DECLARATIVES.

       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [ARTICOLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "Impossibile procedere."
             x"0d0a""Tabella causali di magazzino [TCAUMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TCAUMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File gruppi GDO [TGRUPGDO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TGRUPGDO] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TGRUPGDO] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File delle testate [TMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMOVMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File delle righe [RMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RMOVMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File testate magazzino [TORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RORDINI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RORDINI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File testate magazzino [TNOTACR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TNOTACR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RNOTACR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [RNOTACR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[RNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
                initialize geslock-messaggio
                string   "Chiudere file Excel!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TXT"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output lineseq
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       VENODBC-ERR SECTION.
           use after error procedure on venodbc.
           set tutto-ok  to true.
           evaluate status-venodbc
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File venodbc [VENODBC] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [VENODBC] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[VENODBC] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output venodbc
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate

      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File marche [TMARCHE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMARCHE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMARCHE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
             x"0d0a""Tabella [CLIENTI] inesistente"
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
           when "99"
                set RecLocked to true
                set errori    to true
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
      *-   
           move pg-user to user-codi.
           move 0 to counter counter2.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa path-tmp.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  wstampa      from environment "PATH-ST".
           inspect wstampa      replacing trailing spaces by low-value.
           inspect pg-user    replacing trailing spaces by low-value.
           string  wstampa      delimited by low-value
                   "venodbc"    delimited by size
                   "_"          delimited by size
                   pg-user    delimited by low-value
                   ".csv"       delimited by size
                   into wstampa
           end-string.                  
           accept  path-tmp     from environment "PATH-ST".
           inspect path-tmp     replacing trailing spaces by low-value.
           string  path-tmp     delimited by low-value
                   "venodbc"    delimited by size
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   ".tmp"       delimited by size
                   into path-tmp
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-VENODBC
           if tutto-ok
              close      venodbc
              open i-o   venodbc
              open input tcaumag
                         tmovmag
                         rmovmag
                         tgrupgdo
                         articoli
                         tmarche
                         clienti
                         tordini
                         rordini
                         tnotacr
                         rnotacr
                         ttipocli
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.    

      ***---
       OPEN-OUTPUT-VENODBC.
           open output venodbc.

      ***---
       ELABORAZIONE.
           move low-value      to tmo-rec.
           move pg-data-from to tmo-data-movim.
           start tmovmag key   is >= k-data
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read tmovmag next at end         exit perform end-read
                 if tmo-data-movim > pg-data-to exit perform end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200
                    move counter to counter-edit
                    display counter-edit
                       upon pg-handle at column 15,00
                                             line  3,00
                    move 0 to counter2
                 end-if

                 if record-ok
                    if tmo-fornitore
                       set record-ok to false
                    else
                       set  cli-tipo-C     to true
                       move tmo-cod-clifor to cli-codice
                       read clienti no lock invalid continue end-read
                       move cli-tipo to tcl-codice
                       read ttipocli no lock invalid continue end-read
      *****                 if cli-gdo = spaces
                       if tcl-gdo-no
                          set record-ok to false
                       end-if    
                       if pg-gdo-codice not = spaces
                          move spaces  to gdo-capogruppo
                          move cli-gdo to gdo-codice
                          read tgrupgdo no lock
                               invalid continue
                          end-read
                          if pg-gdo-codice not = gdo-capogruppo
                             set record-ok to false
                          end-if
                       end-if
                       if pg-tcl-codice not = spaces and
                          pg-tcl-codice not = cli-tipo
                          set record-ok to false
                       end-if
                    end-if
                 end-if

                 if record-ok
                    move tmo-causale         to tca-codice
                    read tcaumag no lock
                         invalid continue
                     not invalid perform LOOP-RIGHE
                    end-read
                 end-if

              end-perform
           end-if.

           if not trovato
              display message "Nessun movimento trovato"
                        title titolo
                         icon 2
           else
              perform GENERA-FILE-EXCEL
              if tutto-ok
                 perform CALL-EXCEL
              end-if
           end-if.

      ***---
       GENERA-FILE-EXCEL.
           move low-value to ven-rec.
           start venodbc key is >= ven-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read venodbc next at end exit perform end-read
              if prima-volta
                 perform OPEN-OUTPUT-LINESEQ
                 if errori exit perform end-if
                 perform ACCETTA-SEPARATORE
                 initialize line-riga
                 string "Gruppo"         delimited size
                        separatore       delimited size
                        "Supermercato"   delimited size
                        separatore       delimited size
                        "Articolo"       delimited size
                        separatore       delimited size
                        "Descrizione"    delimited size
                        separatore       delimited size
                        "Q.tà"           delimited size
                        separatore       delimited size
                        "Prezzo Medio"   delimited size
                        separatore       delimited size
                        "Imponibile"     delimited size
                        separatore       delimited size
                        "Add.le Pb"      delimited size
                        separatore       delimited size
                        "Imp. Totale"    delimited size
                        separatore       delimited size
                        "Marca"          delimited size
                        separatore       delimited size
                        "Da data"        delimited size
                        separatore       delimited size
                        "A data"         delimited size
                        into line-riga
                 end-string
                 write line-riga
                 move pg-data-to(1:4)   to r-data-to(7:4)
                 move "/"                 to r-data-to(6:1)
                 move pg-data-to(5:2)   to r-data-to(4:2)
                 move "/"                 to r-data-to(3:1)
                 move pg-data-to(7:2)   to r-data-to(1:2)

                 move pg-data-from(1:4) to r-data-from(7:4)
                 move "/"                 to r-data-from(6:1)
                 move pg-data-from(5:2) to r-data-from(4:2)
                 move "/"                 to r-data-from(3:1)
                 move pg-data-from(7:2) to r-data-from(1:2)
                 set prima-volta to false
              end-if

              compute ven-prezzo-medio = 
                    ( ven-imp-merce    + 
                      ven-consumo      + 
                      ven-cou-cobat  ) / ven-qta

              move ven-gdo-codice       to r-gdo-codice
              move ven-gdo-intestazione to r-gdo-intestazione
              move ven-art-codice       to r-art-codice
              move ven-art-descrizione  to r-art-descrizione
              move ven-qta              to r-qta
              move ven-prezzo-medio     to r-prezzo-medio
              move ven-imp-merce        to r-imp
              move ven-add-pb           to r-add-pb

              add  ven-add-pb           to ven-imp-merce
              move ven-imp-merce        to r-tot-imp

              move ven-mar-descrizione  to r-mar-descrizione

              initialize line-riga
              string r-gdo-codice       delimited size
                     separatore         delimited size
                     r-gdo-intestazione delimited size
                     separatore         delimited size
                     r-art-codice       delimited size
                     separatore         delimited size
                     r-art-descrizione  delimited size
                     separatore         delimited size
                     r-qta              delimited size
                     separatore         delimited size
                     r-prezzo-medio     delimited size
                     separatore         delimited size
                     r-imp              delimited size
                     separatore         delimited size
                     r-add-pb           delimited size
                     separatore         delimited size
                     r-tot-imp          delimited size
                     separatore         delimited size
                     r-mar-descrizione  delimited size
                     separatore         delimited size
                     r-data-from        delimited size
                     separatore         delimited size
                     r-data-to          delimited size
                     into line-riga
              end-string
              write line-riga
           end-perform.

      ***---
       LOOP-RIGHE.
           perform TROVA-ORDINE-NOTA.
           set  tutto-ok   to true.
           move low-value  to rmo-rec.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           start rmovmag key is >= rmo-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rmovmag next at end exit perform end-read

                 if tmo-anno   not = rmo-anno  or
                    tmo-numero not = rmo-movim
                    exit perform
                 end-if
                 perform VALORIZZA-RIGA
              end-perform
           end-if.

      ***---
       VALORIZZA-RIGA.
           initialize ven-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move cli-gdo to gdo-codice.
           read tgrupgdo
                invalid move spaces to gdo-intestazione
           end-read.
           move gdo-capogruppo   to ven-gdo-codice.
           move 0 to add-pb.
           if trovato-ordine
              perform TROVA-ADDIZIONALE-ORDINE
           end-if.

           if trovato-nota
              perform TROVA-ADDIZIONALE-NOTA
           end-if.
           compute rmo-netto = rmo-netto - add-pb.

           move rmo-articolo     to art-codice ven-art-codice.
           read articoli no lock
                invalid move spaces to art-descrizione
           end-read.
           move art-marca-prodotto to mar-codice.
           read tmarche no lock
                invalid move spaces to mar-descrizione
           end-read.
           read venodbc invalid  continue end-read.
           move gdo-intestazione to ven-gdo-intestazione.
           move art-descrizione  to ven-art-descrizione.
           move mar-descrizione  to ven-mar-descrizione.
           if rmo-qta = 0 move 1 to rmo-qta end-if.
           if tca-imponibile-pos
              compute ven-imp-merce    = ven-imp-merce    + 
                                       ( rmo-netto    * rmo-qta)
              compute ven-add-pb       = ven-add-pb       + 
                                       ( add-pb       * rmo-qta)
              compute ven-consumo      = ven-consumo      + 
                                       ( rmo-imp-cons * rmo-qta)
              compute ven-cou-cobat    = ven-cou-cobat    +
                                       ( rmo-coubat   * rmo-qta)
              compute ven-qta          = ven-qta          + rmo-qta
      *****        compute ven-prezzo-medio = ven-prezzo-medio + prezzo-medio
           else
              compute ven-imp-merce    = ven-imp-merce    - 
                                       ( rmo-netto * rmo-qta)
              compute ven-add-pb       = ven-add-pb       -
                                       ( add-pb       * rmo-qta)
              compute ven-consumo      = ven-consumo      -
                                       ( rmo-imp-cons * rmo-qta)
              compute ven-cou-cobat    = ven-cou-cobat    -
                                       ( rmo-coubat   * rmo-qta)
              compute ven-qta          = ven-qta          - rmo-qta
      *****        compute ven-prezzo-medio = ven-prezzo-medio - prezzo-medio
           end-if.
           write ven-rec invalid rewrite ven-rec end-write.
           set trovato        to true.

      ***---
       CLOSE-FILES.
           close tcaumag
                 tmovmag
                 rmovmag
                 venodbc
                 tgrupgdo
                 articoli
                 tmarche
                 clienti
                 tordini
                 rordini
                 tnotacr
                 rnotacr
                 ttipocli.
           delete file venodbc.
           if not prima-volta
              close lineseq
           end-if.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
           copy "recupero-addizionale.cpy".
