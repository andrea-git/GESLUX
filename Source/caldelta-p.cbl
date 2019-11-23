       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      caldelta-p.
       AUTHOR.                          Andrea.
       REMARKS.  Serve per avere (prima di aver consolidato) una proiezione
           di quelli che saranno i guadagni per quella marca del mese.
           La stampa verrà richiamata solamente in caso venga richiesto 
           il mese successivo a quello di consolidamento.
           Più sotto i commenti alle formule
      ******************************************************************

       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "statraff.sl".
           copy "tmarche.sl".
           copy "progmagric.sl".
           copy "tmp-delta-marca.sl".
           copy "tmp-tendenza.sl".
           copy "articoli.sl".
           copy "ttipocli.sl".
           copy "statsett.sl".
           COPY "statsett.sl"
                REPLACING ==statsett== BY ==statsett2==,
                          ==STATUS-statsett== BY ==STATUS-statsett2==.
           copy "rmovmag.sl".
                                
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "statraff.fd".
           copy "tmarche.fd".
           copy "progmagric.fd".
           copy "tmp-delta-marca.fd".
           copy "tmp-tendenza.fd".
           copy "articoli.fd".
           copy "ttipocli.fd".
           copy "statsett.fd".
           COPY "statsett.fd"
                REPLACING ==statsett== BY ==statsett2==,
                          ==STATUS-statsett== BY ==STATUS-statsett2==.
           copy "rmovmag.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * FILE STATUS AND VARIABLES
       77  status-statsett            pic xx.
       77  status-statsett2           pic xx.
       77  status-statraff            pic xx.
       77  status-tmarche             pic xx.
       77  status-progmagric          pic xx.
       77  status-tmp-delta-marca     pic xx.
       77  status-tmp-tendenza        pic xx.
       77  status-articoli            pic xx.
       77  status-ttipocli            pic xx.
       77  status-rmovmag             pic xx.

       77  path-tmp-delta-marca       pic x(256).
       77  path-tmp-tendenza          pic x(256).

      * VARIABILI
       77  como-data                  pic 9(8).
       77  como-ora                   pic 9(8).
       77  como-vendite               pic s9(12)v999.
       77  como-acquisti              pic s9(12)v999.
       77  finali                     pic s9(12)v99.
       77  margine                    pic s9(12)v99.
       77  delta                      pic s9(12)v99.
       77  scostamento                pic s9(12)v99.
       77  incidenza                  pic s9(3)v99.
       77  margine-pos                pic 9(12)v99.

      * FLAGS
       01  controlli                  pic xx.
         88 tutto-ok                  value "OK".
         88 errori                    value "ER".

      * COSTANTI
       78  titolo                     value "Delta margine".
       78  max-righe                  value 62.

       LINKAGE SECTION.
       77  link-path                  pic x(256).
       77  link-mese                  pic 99.
       77  link-data                  pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION USING link-path, link-mese, link-data.

       DECLARATIVES.

      ***---
       STATSETT-ERR SECTION.
           use after error procedure on statsett.
           set tutto-ok  to true.
           evaluate status-statsett
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File statistiche [STATSETT] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [STATSETT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STATSETT] Indexed file corrupt!"
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
                move   "statsett"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open input statsett
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.
      
      ***---
       STATRAFF-ERR SECTION.
           use after error procedure on statraff.
           set tutto-ok  to true.
           evaluate status-statraff
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File marche [STATRAFF] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [STATRAFF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[STATRAFF] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.
      
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
                set errori to true
                display message "File [TMARCHE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMARCHE] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.
      
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File  [ARTICOLI] inesistente"
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
           end-evaluate.
      
      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File  [TTIPOCLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TTIPOCLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TTIPOCLI] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.
      
      ***---
       TMP-DELTA-MARCA-ERR SECTION.
           use after error procedure on tmp-delta-marca.
           set tutto-ok  to true.
           evaluate status-tmp-delta-marca
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [TMP-DELTA-MARCA] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TMP-DELTA-MARCA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-DELTA-MARCA]Indexed file corrupt!"
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
                move   "TMP-DELTA-MARCA"  to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output tmp-delta-marca
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.
      
      ***---
       TMP-TENDENZA-ERR SECTION.
           use after error procedure on tmp-tendenza.
           set tutto-ok  to true.
           evaluate status-tmp-tendenza
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [tmp-tendenza] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [tmp-tendenza] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[tmp-tendenza]Indexed file corrupt!"
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
                move   "tmp-tendenza"  to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output tmp-tendenza
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.
      
      ***---
       PROGMAGRIC-ERR SECTION.
           use after error procedure on progmagric.
           set tutto-ok  to true.
           evaluate status-progmagric
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tipocli [PROGMAGRIC] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [PROGMAGRIC] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAGRIC] Indexed file corrupt!"
                          title titolo
                           icon 3
           end-evaluate.
      
      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File [RMOVMAG] inesistente"
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
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-tmp-delta-marca.
           set tutto-ok       to true.
           accept  path-tmp-delta-marca from environment "PATH-ST".
           inspect path-tmp-delta-marca 
                   replacing trailing spaces by low-value
           string  path-tmp-delta-marca delimited by low-value
                   "tmp-delta"    delimited by size
                   "_"            delimited by size
                   como-data      delimited by size
                   "_"            delimited by size
                   como-ora       delimited by size
                   into path-tmp-delta-marca
           end-string.

           initialize path-tmp-tendenza.
           set tutto-ok       to true.
           accept  path-tmp-tendenza from environment "PATH-ST".
           inspect path-tmp-tendenza 
                   replacing trailing spaces by low-value
           string  path-tmp-tendenza delimited by low-value
                   "tmp-tendenza"    delimited by size
                   "_"               delimited by size
                   como-data         delimited by size
                   "_"               delimited by size
                   como-ora          delimited by size
                   into path-tmp-tendenza
           end-string.

      ***---
       OPEN-FILES.
           if tutto-ok
              perform OPEN-TMP-DELTA-MARCA-LOCK
              perform OPEN-TMP-TENDENZA-LOCK
              if tutto-ok
                 open input statsett statsett2 statraff
                 if tutto-ok
                    open input articoli ttipocli
                               tmarche  progmagric rmovmag
                    if errori
                       close statsett statraff
                       close tmp-delta-marca
                       delete file tmp-delta-marca
                    end-if
                 else
                    close tmp-delta-marca
                    delete file tmp-delta-marca
                 end-if
              end-if
           end-if.

      ***---
       OPEN-TMP-DELTA-MARCA-LOCK.
           open output tmp-delta-marca.
           if tutto-ok
              close    tmp-delta-marca
              open i-o tmp-delta-marca allowing readers
           end-if.

      ***---
       OPEN-TMP-TENDENZA-LOCK.
           open output tmp-tendenza.
           if tutto-ok
              close    tmp-tendenza
              open i-o tmp-tendenza allowing readers
           end-if.

      ***---
       ELABORAZIONE.
           perform MARGINE-RICALCOLATO.
           perform MARGINE-CONSOLIDATO.

           |CALCOLO COLONNE
           move low-value to sts-chiave of statsett.
           move link-mese to sts-mese   of statsett.
           start statsett key >= k-ord
                 invalid continue
             not invalid

                 perform until 1 = 2
                    read statsett next at end exit perform end-read

                    if sts-mese of statsett not = link-mese
                       exit perform
                    end-if

                    perform CALCOLO-DELTA

                    move mar-codice              to tmdt-marca
                    move sts-tipocli of statsett to tmdt-tipocli
                    move delta                   to tmdt-tendenza
                    move incidenza               to tmdt-incidenza
                    write tmdt-rec 
                          invalid display message "ERRORE" 
                    end-write

                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close statsett
                 statsett2
                 statraff
                 tmarche
                 articoli
                 ttipocli
                 progmagric
                 tmp-delta-marca
                 tmp-tendenza
                 rmovmag.
           delete file tmp-delta-marca.
            
  
      ***---
       EXIT-PGM.
           move path-tmp-tendenza to link-path.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "delta.cpy".
