       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      utf-daylist-p.
       AUTHOR.                          Filippo.
      ******************************************************************
       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "movutf.sl".
           copy "clienti.sl".
           copy "lineseq.sl".
           copy "tmp-utf-day.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "movutf.fd".
           copy "clienti.fd".
           copy "lineseq.fd".
           copy "tmp-utf-day.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "link-geslock.def".
           copy "comune.def".

       77  status-movutf          pic x(2).
       77  status-clienti         pic x(2).
       77  status-lineseq         pic x(2).
       77  status-tmp-utf-day     pic x(2).

       77  path-tmp-utf-day       pic x(256).
       77  wstampa                pic x(256).
       77  sw-uscite              pic 9 value 0.
       77  sw-entrate             pic 9 value 0.
       77  tot-kg-uscite          pic s9(9)v999.
       77  tot-kg-entrate         pic s9(9)v999.
       77  com-data6              pic 9(6).   
       01  save-tipo              pic 9.
         88 save-uscita         value 0.
         88 save-entrata        value 1.
       77  sav-riga         pic x(900).

       01  filler                 pic 9.
         88 prima-volta           value 1, false 0.

       77  como-data              pic 9(8).
       77  como-ora               pic 9(8).

       78  titolo value "Stampa della lista del giorno".

       01  st-titolo.
           05 filler        pic x(10) value "*------>  ".
           05 st-u-e        pic x(7).
           05 filler        pic x(9)  value spaces.
           05 filler        pic x(25) value "PRODOTTI SOGGETTI U.T.F.".
           05 filler        pic x(11) value spaces.
           05 filler        pic x(13) value "registro nr. ".
           05 st-nreg       pic z(10).

       01  st-testa.
           05 filler        pic x(3)  value "Col".
           05 filler        pic x(3).
           05 filler        pic x(4)  value "Data".
           05 filler        pic x(4).
           05 filler        pic x(8)  value "N. Bolla".
           05 filler        pic x(4).
           05 filler        pic x(15) value "Ragione Sociale".
           05 filler        pic x(19).
           05 filler        pic x(8)  value "Nr. mov.".
           05 filler        pic x(15).
           05 filler        pic x(3) value "Kg.".

       01  st-line-riga     pic x(86) value all "-".

       01  std-dati.
           05 std-col       pic bx.
           05 filler        pic x(2).
           05 std-data      pic 99/99/99 blank zero.
           05 filler        pic x(2).
           05 std-ndoc      pic z(10).
           05 filler        pic x(2).
           05 std-ragsoc    pic x(30).
           05 filler        pic x(2).
           05 std-nmov      pic z(10).
           05 filler        pic x(2).
           05 std-kg        pic zzz.zzz.zz9,999-.

       01  st-totali.
           05 filler        pic x(52) value spaces.
           05 filler        pic x(18) value "TOTALE Kg. GIORNO".
           05 st-tot-kg     pic zzz.zzz.zz9,999-.
            
       77  num-righe         pic 99 value 0.
       77  max-righe                  pic 9(3).
       77  num-max-righe-x            pic x(3).

       LINKAGE SECTION.
       copy "link-utf-daylist.def".

      ******************************************************************
       PROCEDURE DIVISION using utf-daylist-linkage.

       DECLARATIVES.
       MOVUTF-ERR SECTION.
           use after error procedure on movutf.
           set tutto-ok  to true.
           evaluate status-movutf
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File movimenti utf [MOVUTF] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [MOVUTF] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[MOVUTF] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate. 

       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
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
                string   "File già in uso!"
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

       TMP-ERR SECTION.
           use after error procedure on tmp-utf-day.
           set tutto-ok  to true.
           evaluate status-tmp-utf-day
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tmp [TMP-UTF-DAY] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMP-UTF-DAY] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMP-UTF-DAY] Indexed file corrupt!"
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
                     open output tmp-utf-day
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
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
           set tutto-ok to true.
           set trovato to false.
           accept num-max-righe-x from environment "NUM-MAX-RIGHE".
           move   num-max-righe-x to max-righe with convert.

           accept  wstampa      from environment "PATH-ST".
           accept  como-data         from century-date.
           accept  como-ora          from time.
           inspect wstampa      replacing trailing 
                                     spaces by low-value.
           string wstampa       delimited by low-value
                  "utf-daylist"      delimited by size
                  "_"                delimited by size
                  como-data          delimited by size
                  "_"                delimited by size
                  como-ora           delimited by size
                  ".txt"             delimited by size
                  into wstampa
           end-string.
                                                           
           accept  path-tmp-utf-day from environment "PATH-ST".
           inspect path-tmp-utf-day replacing trailing 
                                       spaces by low-value.
           string path-tmp-utf-day  delimited by low-value
                  "utf-daylist"     delimited by size
                  "_"               delimited by size
                  como-data         delimited by size
                  "_"               delimited by size
                  como-ora          delimited by size
                  ".tmp"            delimited by size
                  into path-tmp-utf-day
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-TMP.
           if tutto-ok
              open input movutf clienti
              if errori
                 close       lineseq
                 delete file lineseq
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-TMP.
           open output tmp-utf-day.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.
      
      ***---
       ELABORAZIONE.
LUBEXX     evaluate true
LUBEXX     when utf-daylist-uscite  perform ELENCO-USCITE
LUBEXX                              perform STAMPA-ELENCO
LUBEXX     when utf-daylist-entrate perform ELENCO-ENTRATE
LUBEXX                              perform STAMPA-ELENCO
LUBEXX     when utf-daylist-tutte   perform ELENCO-USCITE
LUBEXX                              perform ELENCO-ENTRATE
LUBEXX                              perform STAMPA-ELENCO
LUBEXX     end-evaluate.

      ***---
       STAMPA-ELENCO.
           if trovato 
              perform CREA-TXT 
           else
              display message box "Nessun documento presente avente"
                                  " il criterio selezionato"
                      title = titolo
                      icon 2
           end-if.

      ***---
LUBEXX ELENCO-USCITE.
           |ELENCO USCITE
           move 0 to num-righe.
           initialize mov-rec.
           move utf-daylist-data(1:4) to mov-anno.
           move utf-daylist-num-reg   to mov-num-reg.
           move utf-daylist-data      to mov-data.
           move "-"                   to mov-tipo.

           start movutf  key is >= k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read movutf next at end exit perform end-read

                    if mov-anno    not = utf-daylist-data(1:4) or
                       mov-num-reg not = utf-daylist-num-reg   or
                       mov-data    not = utf-daylist-data      or
                       mov-tipo    not = "-"
                       exit perform
                    end-if

                    perform MOVE-DATI
                    move 1 to sw-uscite

                 end-perform
           end-start.

      ***---
LUBEXX ELENCO-ENTRATE.
           |ELENCO ENTRATE
           move 0 to num-righe.
           initialize mov-rec.
           move utf-daylist-data(1:4) to mov-anno.
           move utf-daylist-num-reg   to mov-num-reg.
           move utf-daylist-data      to mov-data.
           move "+"                   to mov-tipo.

           start movutf  key is >= k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read movutf next at end exit perform end-read
                    if mov-anno    not = utf-daylist-data(1:4) or
                       mov-num-reg not = utf-daylist-num-reg   or
                       mov-data    not = utf-daylist-data      or
                       mov-tipo    not = "+"
                       exit perform
                    end-if

                    perform MOVE-DATI
                    move 1 to sw-entrate
                 end-perform
           end-start.
                        
      ***---
       MOVE-DATI.            
           set trovato to true.
           initialize tutf-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           
           if mov-entrata set tutf-entrata to true
           else           set tutf-uscita  to true
           end-if.

           initialize std-dati.
           move mov-col-reg-utf  to tutf-col.
           initialize com-data6.
           move mov-data(3:2)    to com-data6(5:2).
           move mov-data(5:2)    to com-data6(3:2).
           move mov-data(7:2)    to com-data6(1:2).
           move com-data6        to tutf-data.
           move mov-num-doc      to tutf-ndoc.

           initialize cli-rec.
           move mov-tipo-cf      to cli-tipo-cf.
           move mov-cod-clifor   to cli-codice.
           read clienti no lock
                invalid move "** NON TROVATO **" to cli-ragsoc-1
           end-read.
           move cli-ragsoc-1 to tutf-ragsoc.

           move mov-prog-reg to tutf-nmov.
           move mov-kg       to tutf-kg.
           if mov-tipo = "-"
              compute tot-kg-uscite  = tot-kg-uscite + mov-kg
           else
              compute tot-kg-entrate = tot-kg-entrate + mov-kg
           end-if.

           perform until 1 = 2
              add 1 to tutf-prog
              write tutf-rec
                    invalid add 1 to tutf-prog
                not invalid exit perform
              end-write
           end-perform.

      ***---
       CREA-TXT.
           close      tmp-utf-day.
           open input tmp-utf-day.
           move spaces to save-tipo.
           set trovato to false.
           set prima-volta to true.

           move low-value to tutf-chiave.
           start tmp-utf-day key is >= tutf-chiave
                 invalid continue
             not invalid set trovato to true
           end-start.

           if trovato
              perform until 1 = 2

                 read tmp-utf-day next at end  exit perform end-read

                 if prima-volta
                    move tutf-tipo to save-tipo
                    perform OPEN-OUTPUT-LINESEQ
                    if errori exit perform end-if

                    write line-riga from spaces after 1
                    add 1 to num-righe

                    set prima-volta to false
                    perform INTESTA1
                 end-if

                 |PRIMA SEMPRE LE USCITE
                 if tutf-tipo not = save-tipo
                    move  tot-kg-uscite to st-tot-kg
                    perform STAMPA-TOTALI
                    perform SALTO-PAGINA
                    move tutf-tipo to save-tipo
                    perform INTESTA1
                 end-if

                 initialize std-dati
                 move tutf-col    to std-col
                 move tutf-ndoc   to std-ndoc
                 move tutf-data   to std-data
                 move tutf-ragsoc to std-ragsoc
                 move tutf-nmov   to std-nmov
                 move tutf-kg     to std-kg
                 move std-dati    to line-riga

                 perform STAMPA-RIGA
              end-perform
           
              if save-entrata move tot-kg-entrate to st-tot-kg
              else            move tot-kg-uscite  to st-tot-kg
              end-if
              perform STAMPA-TOTALI
           end-if.

      ***---
       STAMPA-RIGA.
           initialize sav-riga.
           move line-riga to sav-riga.
           if num-righe > max-righe - 3
              perform SALTO-PAGINA
              perform INTESTA
           end-if.
           move sav-riga to line-riga.
           write line-riga.
           add 1 to num-righe.

      ***---
       INTESTA1.
           write line-riga from st-line-riga.

           if save-uscita move "USCITE " to st-u-e
           else           move "ENTRATE" to st-u-e
           end-if.

           move mov-num-reg to st-nreg.
           
           write line-riga from st-titolo.
           write line-riga from st-line-riga.
           write line-riga from st-testa.
           write line-riga from st-line-riga.

           move 5 to num-righe.

      ***---
       INTESTA.
           write line-riga from st-line-riga.
           write line-riga from st-testa.
           write line-riga from st-line-riga.
           move 3 to num-righe.
           
      ***---
       STAMPA-TOTALI.
           move st-line-riga to line-riga.
           perform STAMPA-RIGA.

           move st-totali to line-riga.
           perform STAMPA-RIGA.

           move st-line-riga to line-riga.
           perform STAMPA-RIGA.

      ***---
       SALTO-PAGINA.
           write line-riga from spaces after page.
           write line-riga from x"09"  after 1.

      ***---
       CLOSE-FILES.
           close movutf, clienti, tmp-utf-day.
           delete file tmp-utf-day.
           if not trovato
              move spaces to wstampa
           end-if.

      ***---
       EXIT-PGM.
           move wstampa to utf-daylist-path.
           goback.
