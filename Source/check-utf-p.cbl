       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-utf-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Congruenza tra peso UTF e NON UTF tra ordini-progmag

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "progmag.sl".
           copy "articoli.sl".
           copy "movutf.sl".
           copy "tcaumag.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "progmag.fd".
           copy "articoli.fd".
           copy "movutf.fd".
           copy "tcaumag.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           copy "common-excel.def".
           copy "link-geslock.def".
           copy "acugui.def".

      *    COSTANTI
       78  titolo               value "Verifica congruenza UTF".

      *    FILE STATUS
       77  status-tnotacr       pic xx.
       77  status-rnotacr       pic xx.
       77  status-tordini       pic xx.
       77  status-rordini       pic xx.
       77  status-progmag       pic xx.
       77  status-articoli      pic xx.
       77  status-movutf        pic xx.
       77  status-tcaumag       pic xx.
       77  status-lineseq       pic xx.

       77  wstampa              pic x(256).

      * VARIABILI
       77  como-anno            pic 9(4).
       77  counter              pic 9(10).
       77  counter2             pic 9(10).
       77  counter-edit         pic z(10).
       77  user-codi            pic x(10).
       77  ErrorMsg             pic x(55).
       77  tot-utf-bolla        pic 9(9)v999.
       77  tot-utf-movutf       pic 9(9)v999.
       77  tot-utf-corretto     pic 9(9)v999.

      * FLAGS
       01  sw-errore            pic 9.

       01  filler               pic 9.
           88 trovato-registro  value 1, false 0.

       01  controlli            pic xx.
           88 errori            value "ER".
           88 tutto-ok          value "OK".

      * RIGHE PER LA STAMPA
       01  r-riga.
         05 r-anno             pic 9(4).
         05 r-numero           pic z(8).
         05 r-data-bolla       pic x(10).
         05 r-num-bolla        pic z(8).
         05 r-data-fatt        pic x(10).
         05 r-num-fatt         pic z(8).
         05 r-causale          pic x(4).
         05 r-articolo         pic z(6).
         05 r-descrizione      pic x(40).
         05 r-qta              pic zzz.zz9       blank zero.
         05 r-utf              pic z.zz9,999     blank zero.
         05 r-utf-ok           pic z.zz9,999     blank zero.
         05 r-non-utf          pic z.zz9,999     blank zero.
         05 r-non-utf-ok       pic z.zz9,999     blank zero.
         05 r-tot-utf-bolla    pic z.zzz.zz9,999 blank zero.
         05 r-tot-utf-movutf   pic z.zzz.zz9,999 blank zero.
         05 r-tot-utf-corretto pic z.zzz.zz9,999 blank zero.


       LINKAGE SECTION.
       77  link-handle          handle of window.
       77  link-result          signed-short.
       77  link-anno            pic 9(4).
       77  link-from            pic 9(8).
       77  link-to              pic 9(8).
       77  link-user            pic x(10).
       77  link-tipo            pic x.
           88 link-ordini       value "O".
           88 link-note         value "N".

      ******************************************************************
       PROCEDURE DIVISION USING link-handle, 
                                link-result,
                                link-anno,
                                link-from,
                                link-to
                                link-user
                                link-tipo.

       DECLARATIVES.
 
      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set tutto-ok to true.
           evaluate status-tnotacr
           when "35"
                display message "File [TNOTACR] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.
 
      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           set tutto-ok to true.
           evaluate status-rnotacr
           when "35"
                display message "File [RNOTACR] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.
 
      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok to true.
           evaluate status-tordini
           when "35"
                display message "File [TORDINI] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.
 
      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                display message "File [RORDINI] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.
 
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok to true.
           evaluate status-progmag
           when "35"
                display message "File [PROGMAG] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [PROGMAG] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[PROGMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok to true.
           evaluate status-articoli
           when "35"
                display message "File [ARTICOLI] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.
 
      ***---
       MOVUTF-ERR SECTION.
           use after error procedure on movutf.
           set tutto-ok to true.
           evaluate status-movutf
           when "35"
                display message "File [MOVUTF] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.
 
      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok to true.
           evaluate status-tcaumag
           when "35"
                display message "File [TCAUMAG] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.
 
      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "File [CSV] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [CSV] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[CSV] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Chiudere file Excel!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File CSV"   to geslock-nome-file
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
 
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-OUTPUT-CSV.
           if tutto-ok
              perform OPEN-FILES
              evaluate true
              when link-ordini perform ELABORAZIONE-TORDINI
              when link-note   perform ELABORAZIONE-TNOTACR
              end-evaluate
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0       to counter counter2.
           move spaces  to wstampa.
           set tutto-ok to true.

      ***---
       OPEN-OUTPUT-CSV.
           move link-user to user-codi.
           inspect user-codi replacing trailing spaces by low-value.
           initialize wstampa.
           accept  wstampa     from environment "PATH_ST".
           inspect wstampa     replacing trailing spaces by low-value.
           string  wstampa     delimited low-value
                   "CHECK-UTF" delimited size
                   "_"         delimited size
                   link-tipo   delimited size
                   "_"         delimited size
                   user-codi   delimited low-value
                   ".csv"      delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       OPEN-FILES.
           evaluate true
           when link-ordini open input tordini rordini
           when link-note   open input tnotacr rnotacr
           end-evaluate.

           open input progmag articoli movutf tcaumag.

      ***---
       ELABORAZIONE-TORDINI.
           move 0         to link-result.
           move link-anno to tor-anno.
           move link-from to tor-numero.
           start tordini key >= tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2

                    move 0 to tot-utf-bolla
                              tot-utf-movutf
                              tot-utf-corretto

                    read tordini next at end     exit perform end-read
                    if tor-anno  not = link-anno exit perform end-if
                    if tor-numero > link-to      exit perform end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle  at column 28 line 09
                       move 0 to counter2
                    end-if
                    set trovato-registro to false

                    if tor-causale not = "FTMA"
                       perform LOOP-RIGHE-RORDINI

                       move tor-causale to tca-codice
                       read tcaumag no lock invalid continue end-read
                       if tca-si-utf
                          perform REGISTRO-UTF
                       end-if
                    end-if

                    if tot-utf-bolla  not = tot-utf-movutf   or
                       tot-utf-bolla  not = tot-utf-corretto or
                       tot-utf-movutf not = tot-utf-corretto
                       |E' uscita la bolla o il registro, altrimenti
                       |avremmo una differenza dal reale a confonti
                       |inesistenti (uguali a 0)
                       if tot-utf-bolla  > 0 or
                          tot-utf-movutf > 0
                          move "TOTALI NON COINCIDENTI" to ErrorMsg
                          move 0                to ror-cod-articolo
                          move spaces           to art-descrizione
                          move 0                to ror-qta
                          move 0                to ror-peso-utf
                          move 0                to prg-peso-utf
                          move 0                to ror-peso-non-utf
                          move 0                to prg-peso-non-utf
                          move tot-utf-bolla    to r-tot-utf-bolla
                          move tot-utf-movutf   to r-tot-utf-movutf
                          move tot-utf-corretto to r-tot-utf-corretto
                          perform ERRORE
                       end-if
                    end-if

                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-RORDINI.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           move low-value  to ror-num-riga.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    initialize prg-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move ror-prg-chiave to prg-chiave
                    read progmag  no lock invalid continue end-read
                    perform CONTROLLO-UTF
                    if tor-num-bolla  not = 0 and
                       tor-data-bolla not = 0
                       |Controlla solamente l'UTF uscito sulla bolla
                       compute tot-utf-bolla = 
                               tot-utf-bolla + 
                             ( ror-peso-utf  * ror-qta)
                    end-if          
                    compute tot-utf-corretto =
                            tot-utf-corretto + 
                          ( prg-peso-utf     * ror-qta )

                 end-perform
           end-start.                  
      
      ***---
       ELABORAZIONE-TNOTACR.
           move 0         to link-result.
           move link-anno to tno-anno.
           move link-from to tno-numero.
           start tnotacr key >= tno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2

                    move 0 to tot-utf-movutf  
                              tot-utf-corretto

                    read tnotacr next at end     exit perform end-read
                    if tno-anno  not = link-anno exit perform end-if
                    if tno-numero > link-to      exit perform end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle  at column 28 line 09
                       move 0 to counter2
                    end-if

                    set trovato-registro to false

                    evaluate tno-causale 
                    when "NCNC"
                    when "NNEX"
                       perform LOOP-RIGHE-RNOTACR

                       move tor-causale to tca-codice
                       read tcaumag no lock invalid continue end-read
                       if tca-si-utf
                          perform REGISTRO-UTF
                       end-if
                    end-evaluate

                    if tot-utf-movutf not = tot-utf-corretto
                       |E' uscito il registro, altrimenti
                       |avremmo una differenza dal reale a confonto
                       |inesistente (uguale a 0)
                       if tot-utf-movutf > 0
                          move "TOTALI NON COINCIDENTI"to ErrorMsg
                          move 0                to ror-cod-articolo
                          move spaces           to art-descrizione
                          move 0                to ror-qta
                          move 0                to ror-peso-utf
                          move 0                to prg-peso-utf
                          move 0                to ror-peso-non-utf
                          move 0                to prg-peso-non-utf
                          move tot-utf-bolla    to r-tot-utf-bolla
                          move tot-utf-movutf   to r-tot-utf-movutf
                          move tot-utf-corretto to r-tot-utf-corretto
                          perform ERRORE
                       end-if
                    end-if

                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-RNOTACR.
           move tno-anno   to rno-anno.
           move tno-numero to rno-numero.
           move low-value  to rno-num-riga.
           start rnotacr key >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rnotacr next at end exit perform end-read
                    if rno-anno   not = tno-anno or
                       rno-numero not = tno-numero
                       exit perform
                    end-if
                    initialize prg-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move rno-prg-chiave to prg-chiave ror-prg-chiave
                    read progmag  no lock invalid continue end-read
                    move rno-peso-utf     to ror-peso-utf    
                    move rno-peso-non-utf to ror-peso-non-utf
                    move rno-cod-articolo to ror-cod-articolo
                    move tno-anno         to tor-anno
                    move tno-numero       to tor-numero
                    move 0                to tor-data-bolla 
                    move 0                to tor-num-bolla
                    move tno-causale      to tor-causale
                    move tno-num-fattura  to tor-num-fattura
                    move tno-data-fattura to tor-data-fattura
                    move rno-qta          to ror-qta
                    perform CONTROLLO-UTF
                    compute tot-utf-corretto =
                            tot-utf-corretto + 
                          ( prg-peso-utf     * ror-qta )
                 end-perform
           end-start.

      ***---
       CONTROLLO-UTF.
           move 0 to sw-errore.
           if ror-peso-utf     not = prg-peso-utf  or
              ror-peso-non-utf not = prg-peso-non-utf
              add 1 to sw-errore
              move ror-cod-articolo to art-codice
              read articoli no lock invalid continue end-read
              move "Progmag trovato, pesi venduti diversi" to ErrorMsg
              move 0 to r-tot-utf-bolla 
              move 0 to r-tot-utf-movutf 
              move 0 to r-tot-utf-corretto
              perform ERRORE
           end-if.

           if ror-cod-articolo not = ror-prg-cod-articolo
              |Lascio il resto della chiave già valorizzato
              move ror-cod-articolo to prg-cod-articolo
              read progmag no lock 
                   invalid
                   move ror-cod-articolo to art-codice
                   read articoli no lock invalid continue end-read
                   move art-peso-utf     to prg-peso-utf
                   move art-peso-non-utf to prg-peso-non-utf
                   move "Errore su articolo VENDUTO (Recuperato da artic
      -                 "oli)" to ErrorMsg
                   add  1 to sw-errore
                   move 0 to r-tot-utf-bolla 
                   move 0 to r-tot-utf-movutf 
                   move 0 to r-tot-utf-corretto
                   perform ERRORE
               not invalid
                   if ror-peso-utf     not = prg-peso-utf  or
                      ror-peso-non-utf not = prg-peso-non-utf
                      move ror-cod-articolo to art-codice
                      read articoli no lock invalid continue end-read
                      move "Errore su articolo VENDUTO" to ErrorMsg
                      add 1 to sw-errore
                      move 0 to r-tot-utf-bolla 
                      move 0 to r-tot-utf-movutf 
                      move 0 to r-tot-utf-corretto
                      perform ERRORE
                   end-if
              end-read
           end-if.

      ***---
       REGISTRO-UTF.
           |Provo con anno esercizio usato = anno movimento
           move tor-anno         to como-anno.
           perform CERCA-IN-REGISTRO.
           if not trovato-registro
              |Provo con anno esercizio usato < anno movimento
              subtract 1 from tor-anno giving como-anno
              move como-anno to mov-anno
              perform CERCA-IN-REGISTRO
           end-if.

           if not trovato-registro
              |Provo con anno esercizio usato > anno movimento
              add 1 to tor-anno giving como-anno
              move como-anno to mov-anno
              perform CERCA-IN-REGISTRO
           end-if.

      ***---
       CERCA-IN-REGISTRO.
           move low-value        to  mov-rec.
           move como-anno        to  mov-anno.
           move tor-data-fattura to  mov-data.
           move tor-num-bolla    to  mov-num-doc.
           move 1                to  mov-num-reg.
           if tca-movim-giac-pos set mov-entrata to true
           else                  set mov-uscita  to true
           end-if.
           start movutf key >= k-data-bolla
                 invalid continue
             not invalid
                 read movutf next
                 if mov-anno = como-anno        and
                    mov-data = tor-data-fattura and
                    mov-num-doc = tor-num-bolla and
                    mov-tipo    = tca-movim-giacenza
                    move mov-kg to tot-utf-movutf
                    set trovato-registro to true
                 end-if
           end-start.

      ***--
       ERRORE.
           if link-result = 0
              move -1 to link-result
              write line-riga from spaces
              perform ACCETTA-SEPARATORE
              initialize line-riga
              string separatore    delimited size
                     separatore    delimited size
                     separatore    delimited size
                     separatore    delimited size
                     separatore    delimited size
                     separatore    delimited size
                     separatore    delimited size
                     titolo        delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces
              initialize line-riga
              string "Anno"             delimited size
                     separatore         delimited size
                     "Evasione"         delimited size
                     separatore         delimited size
                     "Dt. Bolla"        delimited size
                     separatore         delimited size
                     "N. Bolla"         delimited size
                     separatore         delimited size
                     "DT. Fattura"      delimited size
                     separatore         delimited size
                     "N. Fattura"       delimited size
                     separatore         delimited size
                     "Causale"          delimited size
                     separatore         delimited size
                     "Articolo"         delimited size
                     separatore         delimited size
                     "Descrizione"      delimited size
                     separatore         delimited size
                     "Q.tà"             delimited size
                     separatore         delimited size
                     "UTF"              delimited size
                     separatore         delimited size
                     "Corretto"         delimited size
                     separatore         delimited size
                     "NON UTF"          delimited size
                     separatore         delimited size
                     "Corretto"         delimited size
                     separatore         delimited size
                     "TIPOLOGIA"        delimited size
                     separatore         delimited size
                     "Totale UTF Bolla" delimited size
                     separatore         delimited size
                     "Totale Registro"  delimited size
                     separatore         delimited size
                     "Totale UTF reale" delimited size
                     into line-riga
              end-string
              write line-riga
           end-if.

           move tor-anno       to r-anno.
           move tor-numero     to r-numero.
           initialize r-data-bolla.
           string tor-data-bolla(7:2) delimited size
                  "/"                 delimited size
                  tor-data-bolla(5:2) delimited size
                  "/"                 delimited size
                  tor-data-bolla(1:4) delimited size
                  into r-data-bolla
           end-string.
           move tor-num-bolla   to r-num-bolla.
           move tor-num-fattura to r-num-fatt.
           move tor-causale     to r-causale.
           initialize r-data-fatt.
           string tor-data-fattura(7:2) delimited size
                  "/"                   delimited size
                  tor-data-fattura(5:2) delimited size
                  "/"                   delimited size
                  tor-data-fattura(1:4) delimited size
                  into r-data-fatt
           end-string.
           move ror-cod-articolo to r-articolo.
           move art-descrizione  to r-descrizione.
           move ror-qta          to r-qta.
           move ror-peso-utf     to r-utf.
           move prg-peso-utf     to r-utf-ok.
           move ror-peso-non-utf to r-non-utf.
           move prg-peso-non-utf to r-non-utf-ok.

           initialize line-riga.
           string r-anno             delimited size
                  separatore         delimited size
                  r-numero           delimited size
                  separatore         delimited size
                  r-data-bolla       delimited size
                  separatore         delimited size
                  r-num-bolla        delimited size
                  separatore         delimited size
                  r-data-fatt        delimited size
                  separatore         delimited size
                  r-num-fatt         delimited size
                  separatore         delimited size
                  r-causale          delimited size
                  separatore         delimited size
                  r-articolo         delimited size
                  separatore         delimited size
                  r-descrizione      delimited size
                  separatore         delimited size
                  r-qta              delimited size
                  separatore         delimited size
                  r-utf              delimited size
                  separatore         delimited size
                  r-utf-ok           delimited size
                  separatore         delimited size
                  r-non-utf          delimited size
                  separatore         delimited size
                  r-non-utf-ok       delimited size
                  separatore         delimited size
                  ErrorMsg           delimited size
                  separatore         delimited size
                  r-tot-utf-bolla    delimited size
                  separatore         delimited size
                  r-tot-utf-movutf   delimited size
                  separatore         delimited size
                  r-tot-utf-corretto delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       CLOSE-FILES.
           evaluate true
           when link-ordini close tordini rordini
           when link-note   close tnotacr rnotacr
           end-evaluate.
           
           close progmag articoli lineseq tcaumag movutf.

           if link-result = -1
              perform CALL-EXCEL
           else
              delete file lineseq
           end-if.

      ***---
       EXIT-PGM.
           display "                               "
              upon link-handle  at column 28 line 09.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
