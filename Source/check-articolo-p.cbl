       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-articolo-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Congruenza tra peso articolo venduto e articolo scaricato.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           copy "common-excel.def".
           copy "link-geslock.def".
           copy "acugui.def".

      *    COSTANTI
       78  titolo               value "Verifica congruenza articolo".

      *    FILE STATUS
       77  status-tnotacr       pic xx.
       77  status-rnotacr       pic xx.
       77  status-tordini       pic xx.
       77  status-rordini       pic xx.
       77  status-lineseq       pic xx.

       77  wstampa              pic x(256).

      * VARIABILI
       77  counter              pic 9(10).
       77  counter2             pic 9(10).
       77  counter-edit         pic z(10).
       77  user-codi            pic x(10).

      * FLAGS
       01  controlli            pic xx.
           88 errori            value "ER".
           88 tutto-ok          value "OK".

       01  r-riga.
         05 r-anno              pic 9(4).
         05 r-numero            pic z(8).
         05 r-data-bolla        pic x(10).
         05 r-num-bolla         pic z(8).
         05 r-data-fatt         pic x(10).
         05 r-num-fatt          pic z(8).
         05 r-articolo-v        pic z(6).
         05 r-articolo-s        pic z(6).


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
           set tutto-ok  to true.
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
           set tutto-ok  to true.
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
           set tutto-ok  to true.
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
                   "CHECK-ART" delimited size
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
      
      ***---
       ELABORAZIONE-TORDINI.
           move 0         to link-result.
           move link-anno to tor-anno.
           move link-from to tor-numero.
           start tordini key >= tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-anno  not = link-anno exit perform end-if
                    if tor-numero > link-to  exit perform end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle  at column 28 line 09
                       move 0 to counter2
                    end-if

                    if tor-causale not = "FTMA"
                       perform LOOP-RIGHE-RORDINI
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
                    perform CONTROLLO-ARTICOLO
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
                    read tnotacr next at end exit perform end-read
                    if tno-anno  not = link-anno exit perform end-if
                    if tno-numero > link-to  exit perform end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle  at column 28 line 09
                       move 0 to counter2
                    end-if

                    evaluate tno-causale 
                    when "NCNC"
                    when "NNEX"
                         perform LOOP-RIGHE-RNOTACR
                    end-evaluate

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
                    read rnotacr  next at end exit perform end-read
                    if rno-anno   not = tno-anno or
                       rno-numero not = tno-numero
                       exit perform
                    end-if
                    move rno-prg-cod-articolo to ror-prg-cod-articolo
                    move rno-cod-articolo     to ror-cod-articolo
                    move tno-anno             to tor-anno
                    move tno-numero           to tor-numero
                    move 0                    to tor-data-bolla
                    move 0                    to tor-num-bolla
                    move tno-data-fattura     to tor-data-fattura
                    move tno-num-fattura      to tor-num-fattura
                    perform CONTROLLO-ARTICOLO
                 end-perform
           end-start.

      ***---
       CONTROLLO-ARTICOLO.
           if ror-cod-articolo not = ror-prg-cod-articolo
              perform ERRORE
           end-if.

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
                     titolo        delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces
              initialize line-riga
              string "Anno"               delimited size
                     separatore           delimited size
                     "Evasione"           delimited size
                     separatore           delimited size
                     "Dt. Bolla"          delimited size
                     separatore           delimited size
                     "N. Bolla"           delimited size
                     separatore           delimited size
                     "DT. Fattura"        delimited size
                     separatore           delimited size
                     "N. Fattura"         delimited size
                     separatore           delimited size
                     "Articolo Venduto"   delimited size
                     separatore           delimited size
                     "Articolo Scaricato" delimited size
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
           initialize r-data-fatt.
           string tor-data-fattura(7:2) delimited size
                  "/"                   delimited size
                  tor-data-fattura(5:2) delimited size
                  "/"                   delimited size
                  tor-data-fattura(1:4) delimited size
                  into r-data-fatt
           end-string.
           move ror-cod-articolo     to r-articolo-v.
           move ror-prg-cod-articolo to r-articolo-s.

           initialize line-riga.
           string r-anno        delimited size
                  separatore    delimited size
                  r-numero      delimited size
                  separatore    delimited size
                  r-data-bolla  delimited size
                  separatore    delimited size
                  r-num-bolla   delimited size
                  separatore    delimited size
                  r-data-fatt   delimited size
                  separatore    delimited size
                  r-num-fatt    delimited size
                  separatore    delimited size
                  r-articolo-v  delimited size
                  separatore    delimited size
                  r-articolo-s  delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       CLOSE-FILES.
           evaluate true
           when link-ordini close tordini rordini 
           when link-note   close tnotacr rnotacr
           end-evaluate.
           close lineseq.

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
