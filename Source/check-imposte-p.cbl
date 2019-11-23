       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      check-imposte-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Congruenza di valori tra imposte presenti sul file (ordini,
           note cr, movim. magazzino e ordini f in base alla scelta) e 
           quelle calcolate sul momento dal programma.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tmarche.sl".
           copy "articoli.sl".
           copy "progmag.sl".
           copy "clienti.sl".
           copy "ttipocli.sl".
           copy "timposte.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "lineseq.sl". 
           copy "tordforn.sl".
           copy "rordforn.sl".
           copy "impforn.sl".
           copy "rlistini.sl".
           copy "tcontat.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tmarche.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "clienti.fd".
           copy "ttipocli.fd".
           copy "timposte.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "lineseq.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "impforn.fd". 
           copy "rlistini.fd".
           copy "tcontat.fd".

       WORKING-STORAGE SECTION.
           copy "common-excel.def".
           copy "link-geslock.def".
           copy "acugui.def".
           copy "imposte.def".

      *    COSTANTI
       78  titolo            value "VERIFICA IMPOSTE".

      *    FILE STATUS
       77  status-tmarche        pic xx.
       77  status-articoli       pic xx.
       77  status-progmag        pic xx.
       77  status-clienti        pic xx.
       77  status-ttipocli       pic xx.
       77  status-timposte       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-lineseq        pic xx.
       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.
       77  status-impforn        pic xx.
       77  status-tcontat        pic xx.
       77  status-rlistini       pic xx.

       77  wstampa               pic x(256).

      * VARIABILI
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  user-codi             pic x(10).
       77  como-cons             pic 9(4)v99.
       77  como-coubat           pic 9(4)v99.
       77  como-add-piombo       pic 9(4)v99.
       77  como-peso             pic 9(4)v999.
       77  como-cons-4dec        pic 9(4)v9999.
       77  como-cou-4dec         pic 9(4)v9999.
       77  imposta-consumo-4dec  pic 9(4)v9999.
       77  imposta-cou-4dec      pic 9(4)v9999.
       77  como-imposta-4dec     pic 9(6)v99999.

      * FLAGS
       01  controlli             pic xx.
           88 errori             value "ER".
           88 tutto-ok           value "OK".

       01  r-riga.
         05 r-anno               pic 9(4).
         05 r-numero             pic z(8).
         05 r-data-ordine        pic x(10).
         05 r-causale            pic x(4).
         05 r-data-bolla         pic x(10).
         05 r-num-bolla          pic z(8).
         05 r-data-fatt          pic x(10).
         05 r-num-fatt           pic z(8).
         05 r-cliente            pic z(5).
         05 r-ragsoc             pic x(40).
         05 r-trattam            pic x(10).
         05 r-articolo           pic z(6).
         05 r-descrizione        pic x(30).
         05 r-utf                pic zz9,999.
         05 r-non-utf            pic zz9,999.
         05 r-cons               pic zz.zz9,9999.
         05 r-cons-ok            pic zz.zz9,9999.
         05 r-cou                pic zz.zz9,9999.
         05 r-cou-ok             pic zz.zz9,9999.
         05 r-cobat-ok           pic zz.zz9,99.

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
           88 link-movmag       value "M".
           88 link-ordini-f     value "F".

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
 
      ***---
       TORDFORN-ERR SECTION.
           use after error procedure on tordforn.
           set tutto-ok  to true.
           evaluate status-tordforn
           when "35"
                display message "File [TORDFORN] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TORDFORN] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TORDFORN] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       RORDFORN-ERR SECTION.
           use after error procedure on rordforn.
           set tutto-ok  to true.
           evaluate status-rordforn
           when "35"
                display message "File [RORDFORN] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [RORDFORN] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[RORDFORN] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-OUTPUT-CSV.
           if tutto-ok
              perform OPEN-FILES
              |E' l'aliquota del 2006!!!
              if link-anno = 2006
                 move 0,05 to imp-cou
              end-if
              evaluate true
              when link-ordini   perform ELABORAZIONE-TORDINI
              when link-note     perform ELABORAZIONE-TNOTACR
              when link-movmag   perform ELABORAZIONE-TMOVMAG
              when link-ordini-f perform ELABORAZIONE-TORDFORN
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
           accept  wstampa        from environment "PATH_ST".
           inspect wstampa        replacing trailing spaces by low-value
           string  wstampa        delimited low-value
                   "CHECK-IMPOSTE"delimited size
                   "_"            delimited size
                   link-tipo      delimited size
                   "_"            delimited size
                   user-codi      delimited low-value
                   ".csv"         delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       OPEN-FILES.
           evaluate true
           when link-ordini   open input tordini  rordini 
           when link-note     open input tnotacr  rnotacr
           when link-movmag   open input tmovmag  rmovmag
           when link-ordini-f open input tordforn rordforn 
                                         impforn  rlistini
           end-evaluate.
                      
           open input timposte articoli clienti tcontat 
                      ttipocli tmarche  progmag.
      
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
                    if tor-numero > link-to      exit perform end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 28 line 09
                       move 0 to counter2
                    end-if

                    if tor-causale not = "FTMA"

                       set  cli-tipo-C  to true
                       move tor-cod-cli to cli-codice
                       read clienti no lock invalid continue end-read
                       move cli-tipo to tcl-codice
                       read ttipocli no lock invalid continue end-read
                       |Da Mori: per ora tralasciamo in attesa
                       set TrattamentoPiombo to false
                       evaluate true
                       when ttipocli-gdo 
                            set TrattamentoGdo to true
                       when ttipocli-standard
                            set TrattamentoGdo to false
                       end-evaluate     

                       move tor-anno to con-anno
                       read tcontat no lock
                  
                       evaluate tcl-serie-bolle
                       when 1 move con-ult-stampa-bolle-gdo to imp-data
                       when 2 move con-ult-stampa-bolle-mv  to imp-data
                       when 3 move con-ult-stampa-bolle-at  to imp-data
                       end-evaluate

                       start timposte key <= imp-chiave
                             invalid continue
                         not invalid
                             read timposte previous
                       end-start

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
                    move ror-imp-consumo       to como-cons
                    move ror-imp-cou-cobat     to como-coubat
                    move ror-add-piombo        to como-add-piombo
                    move ror-cod-articolo      to art-codice 
                                                  prg-cod-articolo
                    move ror-prg-cod-magazzino to prg-cod-magazzino
                    move ror-prg-tipo-imballo  to prg-tipo-imballo
                    move ror-prg-peso          to prg-peso

                    if ror-prz-unitario not = 0
                       perform READ-PROGMAG-CALCOLA-IMPOSTE
                    end-if

                    if ror-cod-iva = "E15"
                       if ror-imp-consumo    not = 0 or
                          ror-imp-cou-cobat  not = 0 or
                          ror-add-piombo     not = 0 or
                          ror-prz-unitario   not = 0 or
                          ror-imponib-merce  not = 0
                          display message ror-chiave
                       end-if
                    end-if

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
                          upon link-handle at column 28 line 09
                       move 0 to counter2
                    end-if

                    evaluate tno-causale 
                    when "NCNC"
                    when "NNEX"
                       set  cli-tipo-C  to true
                       move tno-cod-cli to cli-codice
                       read clienti no lock invalid continue end-read
                       move cli-tipo to tcl-codice
                       read ttipocli no lock invalid continue end-read
                       |Da Mori: per ora tralasciamo in attesa
                       set TrattamentoPiombo to false 

                       move tno-anno to con-anno
                       read tcontat no lock
                  
                       evaluate tcl-serie-bolle
                       when 1 move con-ult-stampa-bolle-gdo to imp-data
                       when 2 move con-ult-stampa-bolle-mv  to imp-data
                       when 3 move con-ult-stampa-bolle-at  to imp-data
                       end-evaluate

                       start timposte key <= imp-chiave
                             invalid continue
                         not invalid
                             read timposte previous
                       end-start

                       evaluate true
                       when ttipocli-gdo 
                            set TrattamentoGdo to true
                            perform LOOP-RIGHE-RNOTACR
                       when ttipocli-standard
                            set TrattamentoGdo to false
                            perform LOOP-RIGHE-RNOTACR
                       end-evaluate
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
                    move rno-imp-consumo       to como-cons
                    move rno-imp-cou-cobat     to como-coubat
                    move rno-imp-cou-cobat     to como-coubat
                    move rno-cod-articolo      to art-codice 
                                                  prg-cod-articolo
                    move rno-prg-cod-magazzino to prg-cod-magazzino
                    move rno-prg-tipo-imballo  to prg-tipo-imballo
                    move rno-prg-peso          to prg-peso
                    move tno-causale           to tor-causale

                    if rno-prz-unitario not = 0
                       perform READ-PROGMAG-CALCOLA-IMPOSTE
                    end-if

                 end-perform
           end-start.

      ***---
       ELABORAZIONE-TMOVMAG.
           move 0         to link-result.
           move link-anno to tmo-anno.
           move link-from to tmo-numero.
           start tmovmag key >= tmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag next at end exit perform end-read
                    if tmo-anno  not = link-anno exit perform end-if
                    if tmo-numero > link-to  exit perform end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 28 line 09
                       move 0 to counter2
                    end-if

                    if tmo-cliente
                       set  cli-tipo-C     to true
                       move tmo-cod-clifor to cli-codice
                       read clienti no lock invalid continue end-read
                       move cli-tipo to tcl-codice
                       read ttipocli no lock invalid continue end-read  

                       move tmo-anno to con-anno
                       read tcontat no lock
                  
                       evaluate tcl-serie-bolle
                       when 1 move con-ult-stampa-bolle-gdo to imp-data
                       when 2 move con-ult-stampa-bolle-mv  to imp-data
                       when 3 move con-ult-stampa-bolle-at  to imp-data
                       end-evaluate

                       start timposte key <= imp-chiave
                             invalid continue
                         not invalid
                             read timposte previous
                       end-start
                       |Da Mori: per ora tralasciamo in attesa
                       set TrattamentoPiombo to false
                       evaluate true
                       when ttipocli-gdo 
                            set TrattamentoGdo to true
                            perform LOOP-RIGHE-RMOVMAG
                       when ttipocli-standard
                            set TrattamentoGdo to false
                            perform LOOP-RIGHE-RMOVMAG
                       end-evaluate
                    end-if
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-RMOVMAG.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           move low-value  to rmo-riga.
           start rmovmag key >= rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno  not = tmo-anno or
                       rmo-movim not = tmo-numero
                       exit perform
                    end-if
                    move rmo-imp-cons to como-cons
                    move rmo-coubat   to como-coubat
                    move rmo-articolo to art-codice
                    move rmo-chiave-progmag to prg-chiave
                    move tmo-causale        to tor-causale

                    if rmo-listino not = 0
                       perform READ-PROGMAG-CALCOLA-IMPOSTE
                    end-if

                 end-perform
           end-start.

      ***---
       ELABORAZIONE-TORDFORN.
           move 0         to link-result.
           move link-anno to tof-anno.
           move link-from to tof-numero.
           start tordforn key >= tof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordforn next at end exit perform end-read
                    if tof-anno  not = link-anno exit perform end-if
                    if tof-numero > link-to  exit perform end-if

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 100
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 28 line 09
                       move 0 to counter2
                    end-if
                    if tof-chiuso
                       continue
                    else           

                       accept imp-data from century-date

                       start timposte key <= imp-chiave
                             invalid continue
                         not invalid
                             read timposte previous
                       end-start
                       perform LOOP-RIGHE-RORDFORN
                    end-if
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-RORDFORN.
           move tof-anno   to rof-anno.
           move tof-numero to rof-numero.
           move low-value  to rof-riga.
           start rordforn key >= rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn next at end exit perform end-read
                    if rof-anno   not = tof-anno or
                       rof-numero not = tof-numero
                       exit perform
                    end-if
                    move rof-imp-consumo    to como-cons-4dec
                    move rof-imp-cou-cobat  to como-cou-4dec
                    move rof-cod-articolo   to art-codice
                    move rof-prg-chiave     to prg-chiave

                    perform READ-PROGMAG-CALCOLA-IMPOSTE-F

                 end-perform
           end-start.

      ***---
       READ-PROGMAG-CALCOLA-IMPOSTE.
           move 0 to imposta-cou     imposta-cobat 
                     imposta-consumo add-piombo.
           read articoli no lock invalid continue end-read.

           initialize prg-dati replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           read progmag no lock 
                invalid continue
            not invalid
                move art-marca-prodotto to mar-codice
                read tmarche no lock invalid continue end-read
                perform CALCOLA-IMPOSTE
           end-read.

           if imposta-consumo               not = como-cons   or
            ( imposta-cou + imposta-cobat ) not = como-coubat or
              add-piombo                    not = como-add-piombo
              evaluate true
              when link-ordini continue
              when link-note
                   move tno-anno          to tor-anno
                   move tno-numero        to tor-numero
                   move tno-cod-cli       to tor-cod-cli
                   move tno-num-fattura   to tor-num-fattura
                   move tno-data-fattura  to tor-data-fattura
                   move tno-causale       to tor-causale
              when link-movmag
                   move tmo-anno          to tor-anno
                   move tmo-numero        to tor-numero
                   move tmo-causale       to tor-causale
                   move tmo-cod-clifor    to tor-cod-cli
                   move tmo-numdoc-clifor to tor-num-bolla
                   move tmo-data-doc      to tor-data-bolla
                   move tmo-num-fattura   to tor-num-fattura
                   move tmo-data-fattura  to tor-data-fattura
              end-evaluate

              add prg-peso-utf to prg-peso-non-utf giving como-peso
              perform ERRORE
           end-if.

      ***---
       READ-PROGMAG-CALCOLA-IMPOSTE-F.
           read articoli no lock.
           read progmag  no lock.
           if art-si-imposte of articoli
              if rof-cod-listino not = 0
                 move rof-cod-listino  to rlis-codice
                 move rof-cod-articolo to rlis-articolo
                 read rlistini no lock invalid continue end-read
                 move rlis-tipo-tratt-imposte to imf-codice
                 read impforn no lock invalid continue end-read
              else
                 initialize imf-prz-reale-utf imf-prz-reale-cou
              end-if

              move 0 to imposta-consumo-4dec
              evaluate true
              when art-misto  of articoli
              when art-si-utf of articoli
                   compute como-imposta-4dec =
               (( prg-peso-utf * imp-imposta-consumo ) 
                               * art-perce-imposte of articoli   ) / 100
               when art-no-utf of articoli
                   compute como-imposta-4dec =
              (( prg-peso-non-utf * imp-imposta-consumo ) 
                                  * art-perce-imposte of articoli) / 100
              end-evaluate
              add 0,00005            to como-imposta-4dec
              move como-imposta-4dec to imposta-consumo-4dec

              if imf-prz-reale-utf-zero
                 move 0 to imposta-consumo-4dec
              end-if
              
              move 0 to imposta-cou-4dec
              evaluate true
              when art-misto  of articoli
              when art-si-utf of articoli
                   compute como-imposta-4dec = 
                     (( prg-peso-utf * imp-cou ) 
                                     * art-perce-cou of articoli ) / 100
              when art-no-utf of articoli
                   compute como-imposta-4dec =
                 (( prg-peso-non-utf * imp-cou )
                                     * art-perce-cou of articoli ) / 100
              end-evaluate
              add 0,00005            to como-imposta-4dec
              move como-imposta-4dec to imposta-cou-4dec

              if imf-prz-reale-cou-zero
                 move 0 to imposta-cou-4dec
              end-if
           else
              |Contiene anche il cobat
              move 0 to como-cou-4dec 
                        imposta-consumo-4dec 
                        imposta-cou-4dec
           end-if.

           if imposta-consumo-4dec  not = como-cons-4dec or
              imposta-cou-4dec      not = como-cou-4dec
              move tof-anno         to tor-anno
              move tof-numero       to tor-numero
              move tof-causale      to tor-causale
              move tof-cod-forn     to tor-cod-cli
              set  cli-tipo-F       to true
              move tor-cod-cli      to cli-codice
              read clienti no lock

              add prg-peso-utf to prg-peso-non-utf giving como-peso
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
              string "Anno"               delimited size
                     separatore           delimited size
                     "Evasione"           delimited size
                     separatore           delimited size
                     "Dt. Ordine"         delimited size
                     separatore           delimited size
                     "Causale"            delimited size
                     separatore           delimited size
                     "Dt. Bolla"          delimited size
                     separatore           delimited size
                     "N. Bolla"           delimited size
                     separatore           delimited size
                     "DT. Fattura"        delimited size
                     separatore           delimited size
                     "N. Fattura"         delimited size
                     separatore           delimited size
                     "Cliente"            delimited size
                     separatore           delimited size
                     "Ragione Sociale"    delimited size
                     separatore           delimited size
                     "Imposte"            delimited size
                     separatore           delimited size
                     "Articolo"           delimited size
                     separatore           delimited size
                     "Descrizione"        delimited size
                     separatore           delimited size
                     "Peso UTF"           delimited size
                     separatore           delimited size
                     "Peso NON UTF"       delimited size
                     separatore           delimited size
                     "I. Consumo"         delimited size
                     separatore           delimited size
                     "Corretta"           delimited size
                     separatore           delimited size
                     "I. Cou/Cobat"       delimited size
                     separatore           delimited size
                     "Cou Corretta"       delimited size
                     separatore           delimited size
                     "Cobat Corretta"     delimited size
                     into line-riga
              end-string
              write line-riga
           end-if.
                                             
           move tor-anno       to r-anno.
           move tor-numero     to r-numero.

           initialize r-data-ordine.
           string tor-data-ordine(7:2) delimited size
                  "/"                  delimited size
                  tor-data-ordine(5:2) delimited size
                  "/"                  delimited size
                  tor-data-ordine(1:4) delimited size
                  into r-data-ordine
           end-string.

           move tor-causale    to r-causale.

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
      
           move cli-codice             to r-cliente.
           move cli-ragsoc-1           to r-ragsoc.
           if TrattamentoGDO
              move "GDO"               to r-trattam
           else
              move "Standard"          to r-trattam
           end-if.
           move art-codice             to r-articolo.
           move art-descrizione        to r-descrizione.
           move prg-peso-utf           to r-utf.
           move prg-peso-non-utf       to r-non-utf.
           if link-ordini-f
              evaluate true
              when tof-inserito           move "INSERITO" to r-trattam
              when tof-inviato            move "INVIATO"  to r-trattam
              when tof-in-lavorazione     move "IN LAV."  to r-trattam
              end-evaluate
              move como-cons-4dec         to r-cons
              move imposta-consumo-4dec   to r-cons-ok
              move como-cou-4dec          to r-cou
              move imposta-cou-4dec       to r-cou-ok
           else
              move como-cons              to r-cons
              move imposta-consumo        to r-cons-ok
              move como-coubat            to r-cou
              move imposta-cou            to r-cou-ok
              move imposta-cobat          to r-cobat-ok
           end-if.
      
           initialize line-riga.
           string r-anno           delimited size
                  separatore       delimited size
                  r-numero         delimited size
                  separatore       delimited size
                  r-data-ordine    delimited size
                  separatore       delimited size
                  r-causale        delimited size
                  separatore       delimited size
                  r-data-bolla     delimited size
                  separatore       delimited size
                  r-num-bolla      delimited size
                  separatore       delimited size
                  r-data-fatt      delimited size
                  separatore       delimited size
                  r-num-fatt       delimited size
                  separatore       delimited size            
                  r-cliente        delimited size
                  separatore       delimited size
                  r-ragsoc         delimited size
                  separatore       delimited size
                  r-trattam        delimited size
                  separatore       delimited size
                  r-articolo       delimited size
                  separatore       delimited size
                  r-descrizione    delimited size
                  separatore       delimited size
                  r-utf            delimited size
                  separatore       delimited size
                  r-non-utf        delimited size
                  separatore       delimited size
                  r-cons           delimited size
                  separatore       delimited size
                  r-cons-ok        delimited size
                  separatore       delimited size
                  r-cou            delimited size
                  separatore       delimited size
                  r-cou-ok         delimited size
                  separatore       delimited size
                  r-cobat-ok       delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       CLOSE-FILES.
           evaluate true
           when link-ordini   close tordini rordini 
           when link-note     close tnotacr rnotacr
           when link-movmag   close tmovmag rmovmag
           when link-ordini-f close tmovmag rmovmag impforn rlistini
           end-evaluate.

           close articoli clienti ttipocli timposte tmarche progmag 
                 tcontat.

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
