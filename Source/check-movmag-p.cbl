       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-movmag-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Congruenza di valori tra movimento di magazzino e ordine in
           base allo stesso numero di fattura.
           Viene anche controllata la correttezza delle imposte.
           Viene controllato che i pesi tot siano stati importati 
           correttamente e che non siano presenti entrambi UTF/NON UTF.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl".
           copy "tmarche.sl".
           copy "timposte.sl".
           copy "articoli.sl".
           copy "clienti.sl".
           copy "ttipocli.sl".
           copy "tcaumag.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "lineseq.sl".
           copy "tcontat.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd".
           copy "tmarche.fd".
           copy "timposte.fd".
           copy "articoli.fd".
           copy "clienti.fd".
           copy "ttipocli.fd".
           copy "tcaumag.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "lineseq.fd".
           copy "tcontat.fd".

       WORKING-STORAGE SECTION.
           copy "common-excel.def".
           copy "link-geslock.def".
           copy "acugui.def".
           copy "imposte.def".

      *    COSTANTI
       78  titolo            value "Verifica congruenza mov. magazzino".

      *    FILE STATUS
       77  status-progmag        pic xx.
       77  status-articoli       pic xx.
       77  status-tmarche        pic xx.
       77  status-timposte       pic xx.
       77  status-clienti        pic xx.
       77  status-ttipocli       pic xx.
       77  status-tcaumag        pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-lineseq        pic xx.
       77  status-tcontat        pic xx.

       77  wstampa               pic x(256).

      * VARIABILI
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  user-codi             pic x(10).

       77  tot-utf-o             pic 9(6)v999.
       77  tot-utf-m             pic 9(6)v999.
       77  tot-non-utf-o         pic 9(6)v999.
       77  tot-non-utf-m         pic 9(6)v999.
       77  tot-imponibile-o      pic 9(6)v99.
       77  tot-imponibile-m      pic 9(6)v99.
       77  tot-cons-o            pic 9(6)v99.
       77  tot-coubat-o          pic 9(6)v99.
       77  tot-cons-m            pic 9(6)v99.
       77  tot-coubat-m          pic 9(6)v99.
       77  tot-cons-m-ok         pic 9(6)v99.
       77  tot-coubat-m-ok       pic 9(6)v99.
       77  tot-cons-o-ok         pic 9(6)v99.
       77  tot-coubat-o-ok       pic 9(6)v99.

       77  ris-controllo-articoli pic x(20).

       77  errore-causale        pic x.
       77  errore-cliente        pic x.
       77  errore-destino        pic x.
       77  errore-articoli       pic x.
       77  errore-utf            pic x.
       77  errore-non-utf        pic x.
       77  errore-imponibile     pic x.
       77  errore-peso-tot       pic x.
       77  errore-pesi-entrambi  pic x.
       77  errore-imposte-m      pic x.
       77  errore-imposte-o      pic x.
       77  errore-cons           pic x.
       77  errore-coubat         pic x.

       77  utf-singolo           pic 9(3)v999.
       77  non-utf-singolo       pic 9(3)v999.
       77  como-qta              pic 9(8).

       77  idx-art-o             pic 9(3).
       77  idx-art-m             pic 9(3).

       01  occurs-art-o.
           05 el-prg-chiave-o    occurs 999.
              10 el-prg-cod-articolo-o   pic 9(6).
              10 el-prg-cod-magazzino-o  pic x(3).
              10 el-prg-tipo-imballo-o   pic x(3).
              10 el-prg-peso-o           pic 9(3)v999.

       01  occurs-art-m.
           05 el-prg-chiave-m    occurs 999.
              10 el-prg-cod-articolo-m   pic 9(6).
              10 el-prg-cod-magazzino-m  pic x(3).
              10 el-prg-tipo-imballo-m   pic x(3).
              10 el-prg-peso-m           pic 9(3)v999.

      * FLAGS
       01  controlli             pic xx.
           88 errori             value "ER".
           88 tutto-ok           value "OK".

       01  filler                pic 9.
           88 ordine-errato      value 1, false 0.

       01  filler                pic 9.
           88 LoopEseguito       value 1, false 0.

       01  r-riga.
         05 r-anno-m             pic 9(4).
         05 r-numero-m           pic z(8).
         05 r-anno-o             pic 9(4).
         05 r-numero-o           pic z(8).
         05 r-data-bolla         pic x(10).
         05 r-num-bolla          pic z(8).
         05 r-data-fatt          pic x(10).
         05 r-num-fatt           pic z(8).
         05 r-causale-m          pic x(4).
         05 r-causale-o          pic x(4).
         05 r-cliente-m          pic z(5).
         05 r-cliente-o          pic z(5).
         05 r-destino-m          pic z(5).
         05 r-destino-o          pic z(5).
         05 r-controllo-articoli pic x(20).
         05 r-tot-utf-m          pic zzz.zz9,999.
         05 r-tot-utf-o          pic zzz.zz9,999.
         05 r-tot-non-utf-m      pic zzz.zz9,999.
         05 r-tot-non-utf-o      pic zzz.zz9,999.
         05 r-tot-imponibile-m   pic zzz.zzz.zz9,99.
         05 r-tot-imponibile-o   pic zzz.zzz.zz9,99.
         05 r-tot-cons-m         pic zzz.zzz.zz9,99.
         05 r-tot-cons-m-ok      pic zzz.zzz.zz9,99.
         05 r-tot-cons-o         pic zzz.zzz.zz9,99.
         05 r-tot-cons-o-ok      pic zzz.zzz.zz9,99.
         05 r-tot-coubat-m       pic zzz.zzz.zz9,99.
         05 r-tot-coubat-m-ok    pic zzz.zzz.zz9,99.
         05 r-tot-coubat-o       pic zzz.zzz.zz9,99.
         05 r-tot-coubat-o-ok    pic zzz.zzz.zz9,99.

       LINKAGE SECTION.
       77  link-handle          handle of window.
       77  link-result          signed-short.
       77  link-anno            pic 9(4).
       77  link-from            pic 9(8).
       77  link-to              pic 9(8).
       77  link-user            pic x(10).

      ******************************************************************
       PROCEDURE DIVISION USING link-handle, 
                                link-result,
                                link-anno,
                                link-from,
                                link-to
                                link-user.

       DECLARATIVES.
 
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
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
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message "File [TTIPOCLI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TTIPOCLI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TTIPOCLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message "File [TMARCHE] not found!"
                           title titolo
                            icon 3
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
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
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
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "File [CLIENTI] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.
 
      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           set tutto-ok  to true.
           evaluate status-timposte
           when "35"
                display message "File [TIMPOSTE] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TIMPOSTE] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TIMPOSTE] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
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
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "35"
                display message "File [TMOVMAG] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.
 
      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set tutto-ok  to true.
           evaluate status-rmovmag
           when "35"
                display message "File [RMOVMAG] not found!"
                           title titolo
                            icon 3
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
              perform ELABORAZIONE
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
           accept  wstampa        from environment "PATH_ST".
           inspect wstampa        replacing trailing spaces by low-value
           string  wstampa        delimited low-value
                   "CHECK-MOVMAG" delimited size
                   "_"            delimited size
                   user-codi      delimited low-value
                   ".csv"         delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       OPEN-FILES.
           open input tordini rordini 
                      tnotacr rnotacr  tcontat
                      tmovmag rmovmag  tcaumag
                      clienti ttipocli timposte
                      articoli tmarche progmag.
      
      ***---
       ELABORAZIONE.
           |E' l'aliquota del 2006!!!
           if link-anno = 2006
              move 0,05 to imp-cou
           end-if.
           move 0         to link-result.
           move link-anno to tmo-anno.
           move link-from to tmo-numero.
           start tmovmag key >= tmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    initialize occurs-art-o
                               occurs-art-m
                               replacing numeric data by zeroes
                                    alphanumeric data by spaces
                    set ordine-errato to false
                    move 0 to idx-art-o idx-art-m
                    move 0 to tot-imponibile-m 
                              tot-imponibile-o
                              tot-utf-m
                              tot-utf-o
                              tot-non-utf-m
                              tot-non-utf-o
                              tot-cons-o
                              tot-coubat-o 
                              tot-cons-m
                              tot-coubat-m 
                              tot-cons-o-ok
                              tot-cons-m-ok
                              tot-coubat-m-ok
                              tot-coubat-o-ok
                    read tmovmag next at end exit perform end-read
                    if tmo-anno  not = link-anno exit perform end-if
                    if tmo-numero > link-to  exit perform end-if
                    set LoopEseguito to false

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 28 line 09
                       move 0 to counter2
                    end-if

                    if tmo-num-fattura  not = 0 and
                       tmo-data-fattura not = 0 and
                       tmo-cliente
                       move tmo-causale to tca-codice
                       read tcaumag no lock invalid continue end-read
                       evaluate tmo-causale 
                       when "NCRE"
                       when "NCPZ" 
                       when "NCNC"
                       when "NREX"
                       when "NPEX" 
                       when "NNEX"

                          move tmo-num-fattura  to tno-num-fattura
                          move tmo-anno-fattura to tno-anno-fattura
                          read tnotacr key k-fattura
                               invalid continue
                           not invalid      

                               move tno-anno to con-anno
                               read tcontat no lock
                  
                               evaluate tcl-serie-bolle
                               when 1 move con-ult-stampa-bolle-gdo 
                                        to imp-data
                               when 2 move con-ult-stampa-bolle-mv  
                                        to imp-data
                               when 3 move con-ult-stampa-bolle-at  
                                        to imp-data
                               end-evaluate

                               start timposte key <= imp-chiave
                                     invalid continue
                                 not invalid
                                     read timposte previous
                               end-start

                               move tno-causale     to tor-causale
                               move tno-cod-cli     to tor-cod-cli
                               move tno-prg-destino to tor-prg-destino
                               perform CONTROLLO-CAUSALE
                               perform CONTROLLO-CLIENTE
                               perform CONTROLLO-DESTINO
                               perform RIEMPI-OCCURS-NOTACR
                               perform LOOP-RIGHE
                               evaluate tno-causale 
                               when "NCNC"
                               when "NNEX"
                                   perform CONTROLLO-ARTICOLI
                                   perform CONTROLLO-PESO-UTF
                                   perform CONTROLLO-PESO-NON-UTF
                                   perform CONTROLLO-CONSUMO
                                   perform CONTROLLO-COUBAT
                                   perform CONTROLLO-IMPOSTE-M-GENERALE
                                   perform CONTROLLO-IMPOSTE-O-GENERALE
                               end-evaluate
                               perform CONTROLLO-IMPONIBILE
                               if ordine-errato
                                  move 0 to tor-num-bolla tor-data-bolla
                                  move tno-anno   to tor-anno
                                  move tno-numero to tor-numero
                                  move tno-data-fattura 
                                    to tor-data-fattura
                                  move tno-num-fattura 
                                    to tor-num-fattura
                                  perform ERRORE
                               end-if
                          end-read
                       when other
                          move tmo-anno-fattura to tor-anno-fattura
                          move tmo-num-fattura  to tor-num-fattura
                          read tordini key k-fattura
                               invalid continue
                           not invalid     

                               move tor-anno to con-anno
                               read tcontat no lock
                  
                               evaluate tcl-serie-bolle
                               when 1 move con-ult-stampa-bolle-gdo 
                                        to imp-data
                               when 2 move con-ult-stampa-bolle-mv  
                                        to imp-data
                               when 3 move con-ult-stampa-bolle-at  
                                        to imp-data
                               end-evaluate

                               start timposte key <= imp-chiave
                                     invalid continue
                                 not invalid
                                     read timposte previous
                               end-start

                               perform CONTROLLO-CAUSALE
                               perform CONTROLLO-CLIENTE
                               perform CONTROLLO-DESTINO
                               perform RIEMPI-OCCURS-ORDINI
                               perform LOOP-RIGHE
                               if tor-causale not = "FTMA" 
                                  perform CONTROLLO-ARTICOLI
                                  perform CONTROLLO-PESO-UTF
                                  perform CONTROLLO-PESO-NON-UTF
                                  perform CONTROLLO-CONSUMO
                                  perform CONTROLLO-COUBAT
                                  perform CONTROLLO-IMPOSTE-M-GENERALE
                                  perform CONTROLLO-IMPOSTE-O-GENERALE
                               end-if
                               perform CONTROLLO-IMPONIBILE
                               if ordine-errato
                                  perform ERRORE
                               end-if
                          end-read
                       end-evaluate
                    end-if
                    |Controllo cmq che peso utf e non utf tot
                    |non siano valorizzati entrambi
                    if not LoopEseguito
                       move tmo-anno     to rmo-anno
                       move tmo-numero   to rmo-movim
                       move low-value    to rmo-riga
                       start rmovmag key >= rmo-chiave
                             invalid continue
                         not invalid
                             move spaces to errore-pesi-entrambi 
                             perform until 1 = 2
                                read rmovmag next 
                                     at end exit perform 
                                end-read
                                if rmo-anno  not = tmo-anno or
                                   rmo-movim not = tmo-numero
                                   exit perform
                                end-if
                                if rmo-peso-tot     not = 0 and
                                   rmo-peso-tot-utf not = 0
                                   set ordine-errato        to true
                                   move "X" to errore-pesi-entrambi
                                   perform ERRORE
                                end-if
                             end-perform
                       end-start
                    end-if
                 end-perform
           end-start.

      ***---
       CONTROLLO-CAUSALE.
           if tor-causale not = tmo-causale
              set ordine-errato to true
              move "X" to errore-causale
           else
              move spaces to errore-causale
           end-if.

      ***---
       CONTROLLO-CLIENTE.
           if tor-cod-cli not = tmo-cod-clifor
              set ordine-errato to true
              move "X" to errore-cliente
           else
              move spaces to errore-cliente
           end-if.

      ***---
       CONTROLLO-DESTINO.
           if tor-prg-destino not = tmo-destino
              set ordine-errato to true
              move "X" to errore-destino
           else
              move spaces to errore-destino
           end-if.

      ***---
       RIEMPI-OCCURS-NOTACR.
           move low-value  to rno-rec.
           move tno-anno   to rno-anno.
           move tno-numero to rno-numero.
           start rnotacr key >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    add 1 to idx-art-o
                    read rnotacr  next at end exit perform end-read
                    if rno-anno   not = tno-anno or
                       rno-numero not = tno-numero
                       exit perform
                    end-if
                    move rno-prg-chiave to el-prg-chiave-o(idx-art-o)
                    compute tot-utf-o =
                            tot-utf-o +
                          ( rno-peso-utf * rno-qta )
                    compute tot-non-utf-o =
                            tot-non-utf-o +
                          ( rno-peso-non-utf * rno-qta )
                    compute tot-imponibile-o =
                            tot-imponibile-o +
                         (( rno-prz-unitario +  
                            rno-imp-consumo  +
                            rno-imp-cou-cobat ) * rno-qta )

                    compute tot-cons-o   =
                            tot-cons-o   +
                          ( rno-imp-consumo * rno-qta )

                    compute tot-coubat-o =
                            tot-coubat-o +
                          ( rno-imp-cou-cobat * rno-qta )

                    move rno-cod-articolo      to prg-cod-articolo
                    move rno-prg-cod-magazzino to prg-cod-magazzino
                    move rno-prg-tipo-imballo  to prg-tipo-imballo
                    move rno-prg-peso          to prg-peso

                    move 0 to imposta-cou imposta-consumo imposta-cobat
                    move rno-prg-chiave to prg-chiave
                    read progmag no lock 
                         invalid continue
                     not invalid
                         move rno-cod-articolo to art-codice
                         read articoli 
                              no lock invalid continue 
                         end-read
                         move art-marca-prodotto to mar-codice
                         read tmarche no lock 
                              invalid continue 
                         end-read
                         set  cli-tipo-C  to true
                         move tno-cod-cli to cli-codice
                         read clienti no lock 
                              invalid continue 
                         end-read
                         move cli-tipo to tcl-codice
                         read ttipocli 
                              no lock invalid continue 
                         end-read
                         |Da Mori: per ora tralasciamo in attesa
                         set TrattamentoPiombo to false
                         evaluate true
                         when ttipocli-gdo
                              set TrattamentoGDO to true
                              perform CALCOLA-IMPOSTE
                         when ttipocli-standard
                              set TrattamentoGDO to false
                              perform CALCOLA-IMPOSTE
                         end-evaluate
                    end-read
                    compute tot-cons-o-ok =
                            tot-cons-o-ok +
                            imposta-consumo * rno-qta

                    compute tot-coubat-o-ok =
                            tot-coubat-o-ok +
                          ( imposta-cou + imposta-cobat ) * rno-qta

                 end-perform
           end-start.

      ***---
       RIEMPI-OCCURS-ORDINI.
           move low-value  to ror-rec.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    add 1 to idx-art-o
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    move ror-prg-chiave to el-prg-chiave-o(idx-art-o)
                    if ror-qta-omaggi not = 0
                       add 1 to idx-art-o
                       move ror-prg-chiave to el-prg-chiave-o(idx-art-o)
                    end-if
                    compute tot-utf-o =
                            tot-utf-o +
                          ( ror-peso-utf * ror-qta )
                    compute tot-non-utf-o =
                            tot-non-utf-o +
                          ( ror-peso-non-utf * ror-qta )
                    compute tot-imponibile-o =
                            tot-imponibile-o +
                          ( ror-prz-unitario * 
                          ( ror-qta - ror-qta-omaggi))

                    compute tot-cons-o   =
                            tot-cons-o   +
                          ( ror-imp-consumo * ror-qta )

                    compute tot-coubat-o =
                            tot-coubat-o +
                          ( ror-imp-cou-cobat * ror-qta )

                    move ror-cod-articolo      to prg-cod-articolo
                    move ror-prg-cod-magazzino to prg-cod-magazzino
                    move ror-prg-tipo-imballo  to prg-tipo-imballo
                    move ror-prg-peso          to prg-peso

                    move 0 to imposta-cou imposta-consumo imposta-cobat
                    move ror-prg-chiave to prg-chiave
                    read progmag no lock 
                         invalid continue
                     not invalid
                         move ror-cod-articolo to art-codice
                         read articoli no lock invalid continue 
                         end-read
                         move art-marca-prodotto to mar-codice
                         read tmarche no lock invalid continue 
                         end-read
                         set  cli-tipo-C  to true
                         move tor-cod-cli to cli-codice
                         read clienti  no lock invalid continue 
                         end-read
                         move cli-tipo to tcl-codice
                         read ttipocli no lock invalid continue 
                         end-read
                         |Da Mori: per ora tralasciamo in attesa
                         set TrattamentoPiombo to false
                         evaluate true
                         when ttipocli-gdo
                              set TrattamentoGDO to true
                              perform CALCOLA-IMPOSTE
                         when ttipocli-standard
                              set TrattamentoGDO to false
                              perform CALCOLA-IMPOSTE
                         end-evaluate
                    end-read
                    compute tot-cons-o-ok =
                            tot-cons-o-ok +
                            imposta-consumo * ror-qta

                    compute tot-coubat-o-ok =
                            tot-coubat-o-ok +
                          ( imposta-cou + imposta-cobat ) * ror-qta

                 end-perform
           end-start.

      ***---
       LOOP-RIGHE.
           set  LoopEseguito to true.
           move tmo-anno     to rmo-anno.
           move tmo-numero   to rmo-movim.
           move low-value    to rmo-riga.
           start rmovmag key >= rmo-chiave
                 invalid continue
             not invalid
                 move spaces to errore-peso-tot errore-pesi-entrambi 
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno  not = tmo-anno or
                       rmo-movim not = tmo-numero
                       exit perform
                    end-if

                    if rmo-qta = 0
                       move 1 to como-qta
                    else
                       move rmo-qta to como-qta
                    end-if

                    add 1 to idx-art-m
                    move rmo-chiave-progmag 
                      to el-prg-chiave-m(idx-art-m)
                    compute tot-utf-m =
                            tot-utf-m + rmo-peso-tot-utf
                    compute tot-non-utf-m =
                            tot-non-utf-m + rmo-peso-tot

                    if tmo-causale(1:2) = "NC"
                       compute tot-imponibile-m =
                               tot-imponibile-m +
                            (( rmo-netto  +
                               rmo-imp-cons +
                               rmo-coubat ) * como-qta )
                    else
                       compute tot-imponibile-m =
                               tot-imponibile-m +
                             ( rmo-listino * como-qta )
                    end-if

                    compute tot-cons-m   =
                            tot-cons-m   +
                          ( rmo-imp-cons * como-qta )

                    compute tot-coubat-m =
                            tot-coubat-m +
                          ( rmo-coubat   * como-qta )

                    if rmo-peso-tot-utf not = 0 and rmo-qta not = 0
                       compute utf-singolo = 
                               rmo-peso-tot-utf / rmo-qta
                       if utf-singolo not = rmo-peso
                          set ordine-errato to true
                          move "X" to errore-peso-tot
                       end-if
                    end-if

                    if rmo-peso-tot not = 0 and rmo-qta not = 0
                       compute non-utf-singolo = 
                               rmo-peso-tot / rmo-qta
                       if non-utf-singolo not = rmo-peso
                          set ordine-errato to true
                          move "X" to errore-peso-tot
                       end-if
                    end-if

                    if rmo-peso-tot not = 0 and rmo-peso-tot-utf not = 0
                       set ordine-errato to true
                       move "X" to errore-pesi-entrambi 
                    end-if

                    move 0 to imposta-cou imposta-consumo imposta-cobat
                    move rmo-chiave-progmag to prg-chiave
                    read progmag no lock 
                         invalid continue
                     not invalid
                         move rmo-articolo to art-codice
                         read articoli no lock invalid continue 
                         end-read
                         set  cli-tipo-C  to true
                         move tmo-cod-clifor to cli-codice
                         read clienti no lock invalid continue 
                         end-read
                         move cli-tipo to tcl-codice
                         read ttipocli no lock invalid continue 
                         end-read
                         |Da Mori: per ora tralasciamo in attesa
                         set TrattamentoPiombo to false
                         evaluate true
                         when ttipocli-gdo
                              set TrattamentoGDO to true
                              perform CALCOLA-IMPOSTE
                         when ttipocli-standard
                              move art-marca-prodotto to mar-codice
                              read tmarche no lock invalid continue 
                              end-read
                              set TrattamentoGDO to false
                              perform CALCOLA-IMPOSTE
                         end-evaluate
                    end-read

                    compute tot-cons-m-ok =
                            tot-cons-m-ok +
                            imposta-consumo * como-qta

                    compute tot-coubat-m-ok =
                            tot-coubat-m-ok +
                          ( imposta-cou + imposta-cobat ) * como-qta

                 end-perform
           end-start.

      ***---
       CONTROLLO-ARTICOLI.
           if occurs-art-o not = occurs-art-m
              move "Articoli differenti" to ris-controllo-articoli
              move "X" to errore-articoli
              set ordine-errato to true
           else
              move "Articoli uguali" to ris-controllo-articoli
              move spaces to errore-articoli
           end-if.

      ***---
       CONTROLLO-PESO-UTF.
           if tca-no-movim-giac-periodo
              move spaces to errore-utf
           else
              if tot-utf-o     not = tot-utf-m
                 set ordine-errato to true
                 move "X" to errore-utf
              else
                 move spaces to errore-utf
              end-if
           end-if.

      ***---
       CONTROLLO-PESO-NON-UTF.
           if tca-no-movim-giac-periodo
              move spaces to errore-utf
           else
              if tot-non-utf-o not = tot-non-utf-m
                 set ordine-errato to true
                 move "X" to errore-non-utf
              else
                 move spaces to errore-non-utf
              end-if
           end-if.

      ***---
       CONTROLLO-IMPONIBILE.
           if tot-imponibile-o not = tot-imponibile-m
              set ordine-errato to true
              move "X" to errore-imponibile
           else
              move spaces to errore-imponibile
           end-if.

      ***---
       CONTROLLO-CONSUMO.
           if tot-cons-o not = tot-cons-m
              set ordine-errato to true
              move "X" to errore-cons
           else
              move spaces to errore-cons
           end-if.

      ***---
       CONTROLLO-COUBAT.
           if tot-coubat-o not = tot-coubat-m
              set ordine-errato to true
              move "X" to errore-coubat
           else
              move spaces to errore-coubat
           end-if.

      ***---
       CONTROLLO-IMPOSTE-M-GENERALE.
           if tot-cons-m   not = tot-cons-m-ok or
              tot-coubat-m not = tot-coubat-m-ok
              set ordine-errato to true
              move "X" to errore-imposte-m
           else
              move spaces to errore-imposte-m
           end-if.

      ***---
       CONTROLLO-IMPOSTE-O-GENERALE.
           if tot-cons-o   not = tot-cons-o-ok or
              tot-coubat-o not = tot-coubat-o-ok
              set ordine-errato to true
              move "X" to errore-imposte-o
           else
              move spaces to errore-imposte-o
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
                     separatore    delimited size
                     separatore    delimited size
                     separatore    delimited size
                     titolo        delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces
              initialize line-riga
              string "Anno M"             delimited size
                     separatore           delimited size
                     "Movimento"          delimited size
                     separatore           delimited size
                     "Anno"               delimited size
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
                     "Causale M"          delimited size
                     separatore           delimited size
                     "Causale O"          delimited size
                     separatore           delimited size
                     "Cliente M"          delimited size
                     separatore           delimited size
                     "Cliente O"          delimited size
                     separatore           delimited size
                     "Destino M"          delimited size
                     separatore           delimited size
                     "Destino O"          delimited size
                     separatore           delimited size
                     "Articoli importati" delimited size
                     separatore           delimited size
                     "Tot. UTF M"         delimited size
                     separatore           delimited size
                     "Tot. UTF O"         delimited size
                     separatore           delimited size
                     "Tot. non UTF M"     delimited size
                     separatore           delimited size
                     "Tot. non UTF O"     delimited size
                     separatore           delimited size
                     "Tot. Imponibile M"  delimited size
                     separatore           delimited size
                     "Tot. Imponibile O"  delimited size
                     separatore           delimited size
                     "Tot. Consumo M"     delimited size
                     separatore           delimited size
                     "Corretto"           delimited size
                     separatore           delimited size
                     "Tot. Consumo O"     delimited size
                     separatore           delimited size
                     "Corretto"           delimited size
                     separatore           delimited size
                     "Tot. Coubat M"      delimited size
                     separatore           delimited size
                     "Corretto"           delimited size
                     separatore           delimited size
                     "Tot. Coubat O"      delimited size
                     separatore           delimited size
                     "Corretto"           delimited size
                     separatore           delimited size
                     "ERRORE CAUSALE"     delimited size
                     separatore           delimited size
                     "ERRORE CLIENTE"     delimited size
                     separatore           delimited size
                     "ERRORE DESTINO"     delimited size
                     separatore           delimited size
                     "ERRORE ARTICOLI"    delimited size
                     separatore           delimited size
                     "ERRORE PESO-TOT"    delimited size
                     separatore           delimited size
                     "ERRORE ENTRAMBI PESI"delimited size
                     separatore           delimited size
                     "ERRORE UTF"         delimited size
                     separatore           delimited size
                     "ERRORE NON UTF"     delimited size
                     separatore           delimited size
                     "ERRORE IMPONIBILE"  delimited size
                     separatore           delimited size
                     "ERRORE I.CONS."     delimited size
                     separatore           delimited size
                     "ERRORE I.COU/COBAT" delimited size
                     separatore           delimited size
                     "ERRORE IMPOSTE M"   delimited size
                     separatore           delimited size
                     "ERRORE IMPOSTE O"   delimited size
                     into line-riga
              end-string
              write line-riga
           end-if.
                                             
           move tmo-anno       to r-anno-m.
           move tmo-numero     to r-numero-m.

           move tor-anno       to r-anno-o.
           move tor-numero     to r-numero-o.
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

           move tmo-causale            to r-causale-m.
           move tor-causale            to r-causale-o.
           move tmo-cod-clifor         to r-cliente-m.
           move tor-cod-cli            to r-cliente-o.
           move tmo-destino            to r-destino-m.
           move tor-prg-destino        to r-destino-o.
           move ris-controllo-articoli to r-controllo-articoli.
           move tot-utf-m              to r-tot-utf-m.
           move tot-utf-o              to r-tot-utf-o.
           move tot-non-utf-m          to r-tot-non-utf-m.
           move tot-non-utf-o          to r-tot-non-utf-o.
           move tot-imponibile-m       to r-tot-imponibile-m.
           move tot-imponibile-o       to r-tot-imponibile-o.
           move tot-cons-m             to r-tot-cons-m.
           move tot-cons-m-ok          to r-tot-cons-m-ok.
           move tot-cons-o             to r-tot-cons-o.
           move tot-cons-o-ok          to r-tot-cons-o-ok.
           move tot-coubat-m           to r-tot-coubat-m. 
           move tot-coubat-m-ok        to r-tot-coubat-m-ok.
           move tot-coubat-o           to r-tot-coubat-o.
           move tot-coubat-o-ok        to r-tot-coubat-o-ok.

           initialize line-riga.
           string r-anno-m             delimited size
                  separatore           delimited size
                  r-numero-m           delimited size
                  separatore           delimited size
                  r-anno-o             delimited size
                  separatore           delimited size
                  r-numero-o           delimited size
                  separatore           delimited size
                  r-data-bolla         delimited size
                  separatore           delimited size
                  r-num-bolla          delimited size
                  separatore           delimited size
                  r-data-fatt          delimited size
                  separatore           delimited size
                  r-num-fatt           delimited size
                  separatore           delimited size
                  r-causale-m          delimited size
                  separatore           delimited size      
                  r-causale-o          delimited size
                  separatore           delimited size            
                  r-cliente-m          delimited size
                  separatore           delimited size            
                  r-cliente-o          delimited size
                  separatore           delimited size            
                  r-destino-m          delimited size
                  separatore           delimited size            
                  r-destino-o          delimited size
                  separatore           delimited size            
                  r-controllo-articoli delimited size
                  separatore           delimited size      
                  r-tot-utf-m          delimited size
                  separatore           delimited size      
                  r-tot-utf-o          delimited size
                  separatore           delimited size      
                  r-tot-non-utf-m      delimited size
                  separatore           delimited size      
                  r-tot-non-utf-o      delimited size
                  separatore           delimited size      
                  r-tot-imponibile-m   delimited size
                  separatore           delimited size      
                  r-tot-imponibile-o   delimited size
                  separatore           delimited size      
                  r-tot-cons-m         delimited size
                  separatore           delimited size
                  r-tot-cons-m-ok      delimited size
                  separatore           delimited size
                  r-tot-cons-o         delimited size
                  separatore           delimited size
                  r-tot-cons-o-ok      delimited size
                  separatore           delimited size      
                  r-tot-coubat-m       delimited size
                  separatore           delimited size
                  r-tot-coubat-m-ok    delimited size
                  separatore           delimited size
                  r-tot-coubat-o       delimited size
                  separatore           delimited size
                  r-tot-coubat-o-ok    delimited size
                  separatore           delimited size
                  errore-causale       delimited size
                  separatore           delimited size
                  errore-cliente       delimited size
                  separatore           delimited size
                  errore-destino       delimited size
                  separatore           delimited size
                  errore-articoli      delimited size
                  separatore           delimited size
                  errore-peso-tot      delimited size
                  separatore           delimited size
                  errore-pesi-entrambi delimited size
                  separatore           delimited size
                  errore-utf           delimited size
                  separatore           delimited size
                  errore-non-utf       delimited size
                  separatore           delimited size
                  errore-imponibile    delimited size
                  separatore           delimited size
                  errore-cons          delimited size
                  separatore           delimited size
                  errore-coubat        delimited size
                  separatore           delimited size
                  errore-imposte-m     delimited size
                  separatore           delimited size
                  errore-imposte-o     delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       CLOSE-FILES.
           close tordini  rordini lineseq tmovmag rmovmag  ttipocli
                 tnotacr  rnotacr tcaumag clienti articoli tmarche
                 timposte progmag tcontat.

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
           copy "imposte.cpy".
