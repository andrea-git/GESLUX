       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      intfat-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "tmarche.sl".
           copy "articoli.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tcaumag.sl".
           copy "tmp-marca.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tgrupgdo.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd". 
           copy "tmarche.fd".
           copy "articoli.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tcaumag.fd".
           copy "tmp-marca.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "tgrupgdo.fd".
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
       78  titolo                value "Interrogazioni per fatturati".
       78  MaxRows               value 44.

      * FILE STATUS
       77  status-lineseq        pic xx.
       77  status-tmarche        pic xx.
       77  status-articoli       pic xx.
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.
       77  status-tcaumag        pic xx.
       77  status-tmp-marca      pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tgrupgdo       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-ttipocli       pic xx.

       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).

      * VARIABILI
       77  como-valore           pic s9(9)v99  value 0. 
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  RowCounter            pic 9(3)      value 0.
       77  tot-imp               pic s9(9)v99  value 0.
       77  tot-add-pb            pic s9(9)v99  value 0.
       77  tot-tot-imp           pic s9(9)v99  value 0.
       77  tot-cons              pic s9(9)v99  value 0.
       77  tot-cou               pic s9(9)v99  value 0.
       77  tot-valore            pic s9(9)v99  value 0.
       77  tot-valore2           pic s9(9)v99  value 0.
       77  tot-kg                pic s9(9)v999 value 0.
       77  tot-qta               pic s9(9)v999 value 0.
       77  qta-batt              pic s9(9)v999 value 0.

       77  como-kg               pic  9(9)v999 value 0.
       77  user-codi             pic x(10).

       77  articolo-edit         pic z(5)9.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

      * FLAGS
       01  filler                pic 9.
         88 prima-volta          value 1 false 0.

       01  filler                pic 9.
         88 record-ok            value 1 false 0.

       01  filler                pic 9.
         88 TuttiClienti         value 1 false 0.

      * RIGHE PER LA STAMPA
       01  testata.
         05 te-filler            pic x(26) 
                                 value "** FATTURATO del periodo: ".
         05 t-data-from          pic x(10).
         05 filler               pic x(5) value "  -  ".
         05 t-data-to            pic x(10).
         05 filler               pic x(3) value " **".
         05 filler               pic x(76).

       01  t-gdo.
         05 filler               pic x(10) value "GRUPPO GDO".
         05 filler               pic x(3).
         05 t-cod-gdo            pic x(5).
         05 filler               pic x(3).
         05 t-des-gdo            pic x(50).

       01  t-cliente-fornitore.
         05 t-clifor             pic x(9).
         05 filler               pic x(4).
         05 t-cod-clifor         pic z(5).
         05 filler               pic x(3).
         05 t-ragsoc-clifor      pic x(50).

       01  t-destino.
         05 filler               pic x(7)  value "DESTINO".
         05 filler               pic x(6).
         05 t-prg-destino        pic z(5).
         05 filler               pic x(3).
         05 t-des-destino        pic x(50).

       01  t-articolo.
         05 filler               pic x(8)  value "ARTICOLO".
         05 filler               pic x(4).
         05 t-cod-art            pic z(6) blank zero.
         05 filler               pic x(3).
         05 t-des-art            pic x(50).

       01  riga-div              pic x(168) value all "-".

       01  r-riga.
         05 r-des                pic x(28).
         05 filler               pic x(2).
         05 r-imp-merce          pic ---.---.--9,99.
         05 filler               pic x(2).
         05 r-add-pb             pic ---.---.--9,99.
         05 filler               pic x(2).
         05 r-tot-imp            pic ---.---.--9,99.
         05 filler               pic x(1).
         05 r-cons               pic ---.---.--9,99.
         05 filler               pic x(1).
         05 r-cou                pic ---.---.--9,99.
         05 filler               pic x(1).
         05 r-tot                pic ---.---.--9,99.
         05 filler               pic x(3).
         05 r-tot2               pic ---.---.--9,99.
         05 filler               pic x(3).
         05 r-kg                 pic --.---.--9,999.
         05 filler               pic x(3).
         05 r-qta                pic --.---.--9.
                                                             
       01  tit-riga. 
         05 t-1                  pic x(28)  value "MARCA".
         05 filler               pic x(3).
         05 t-2                  pic x(13)  value "I. MERCE(A)".
         05 filler               pic x(3).
         05 t-3                  pic x(13)  value "ADD.LE PB(D)".
         05 filler               pic x(3).
         05 t-4                  pic x(13)  value "TOTALE IMP".
         05 filler               pic x(2).
         05 t-5                  pic x(13)  value "I CONSUMO(B)".
         05 filler               pic x(2).
         05 t-6                  pic x(13)  value "COU/COBAT(C)".
         05 filler               pic x(2).
         05 t-7                  pic x(13)  value "TOT(A+B+C)".
         05 filler               pic x(4).
         05 t-8                  pic x(13)  value "TOT(A+B+C+D)".
         05 filler               pic x(4).
         05 t-9                  pic x(13)  value "KG.".
         05 filler               pic x(4).
         05 t-10                 pic x(9)  value "QTA BATT".

       01  r-totali.
         05 t-filler             pic x(28)  value "T O T A L I".
         05 filler               pic x(2).
         05 t-imp-merce          pic ---.---.--9,99.
         05 filler               pic x(2).
         05 t-add-pb             pic ---.---.--9,99.
         05 filler               pic x(2).
         05 t-tot-imp            pic ---.---.--9,99.
         05 filler               pic x(1).
         05 t-cons               pic ---.---.--9,99.
         05 filler               pic x(1).
         05 t-cou                pic ---.---.--9,99.
         05 filler               pic x(1).
         05 t-totale             pic ---.---.--9,99.
         05 filler               pic x(3).
         05 t-totale2            pic ---.---.--9,99.
         05 filler               pic x(3).
         05 t-kg                 pic --.---.--9,999.
         05 filler               pic x(3).
         05 t-qta                pic --.---.--9.

       LINKAGE SECTION.
       copy "link-intfat.def".

      ******************************************************************
       PROCEDURE DIVISION using intfat-linkage.

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
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                display message "File [DESTINI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [DESTINI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[DESTINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "35"
                display message "File [TGRUPGDO] not found!"
                           title titolo
                            icon 3
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
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                set errori to true
                display message "File not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
                initialize geslock-messaggio
                if intfat-excel = 0
                   string   "File già in uso!"
                     x"0d0a""Impossibile procedere!" delimited size
                         into geslock-messaggio
                   end-string
                   move   "File TXT"   to geslock-nome-file
                else
                   string   "Chiudere file Excel!"
                     x"0d0a""Impossibile procedere!" delimited size
                         into geslock-messaggio
                   end-string
                   move   "File CSV"   to geslock-nome-file
                end-if
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
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

       TMP-MARCA-ERR SECTION.
           use after error procedure on tmp-marca.
           set tutto-ok  to true.
           evaluate status-tmp-marca
           when "35"
                set errori to true
                display message "File [TMP-MARCA] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [TMP-MARCA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-MARCA] Indexed file corrupt!"
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
                move   "File TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output tmp-marca
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate
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
           move intfat-user to user-codi.
           move 0 to counter counter2.
           move 0 to RowCounter.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa path-tmp.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  wstampa      from environment "PATH-ST".
           inspect wstampa      replacing trailing spaces by low-value.
           if intfat-excel = 0
              string  wstampa   delimited low-value
                      "intfat"  delimited size
                      "_"       delimited size
                      como-data delimited size
                      "_"       delimited size
                      como-ora  delimited size
                      ".txt"    delimited size
                      into wstampa
              end-string
           else
              inspect user-codi   replacing trailing spaces by low-value
              string  wstampa     delimited low-value
                      "intfat"    delimited size
                      "_"         delimited size
                      user-codi   delimited low-value
                      ".csv"      delimited size
                      into wstampa
              end-string
           end-if.
              
           accept  path-tmp     from environment "PATH-ST".
           inspect path-tmp     replacing trailing spaces by low-value.
           string  path-tmp     delimited low-value
                   "intfat"     delimited size
                   "_"          delimited size
                   como-data    delimited size
                   "_"          delimited size
                   como-ora     delimited size
                   ".tmp"       delimited size
                   into path-tmp
           end-string.

           if intfat-codice = 0 set TuttiClienti to true
           else                 set TuttiClienti to false
           end-if.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              perform OPEN-OUTPUT-TMP-MARCA
              if tutto-ok
                 close      tmp-marca
                 open i-o   tmp-marca
                 open input articoli
                            tcaumag
                            tmarche
                            tmovmag
                            rmovmag
                            clienti
                            destini
                            tgrupgdo
                            tordini
                            rordini
                            tnotacr
                            rnotacr
                            ttipocli
                 if errori
                    close lineseq
                    delete file lineseq
                    close tmp-marca
                    delete file tmp-marca
                 end-if
              else
                 close lineseq
                 delete file lineseq
              end-if
           end-if.

           if errori move spaces to wstampa end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.    

      ***---
       OPEN-OUTPUT-TMP-MARCA.
           open output tmp-marca.

      ***---
       ELABORAZIONE.
           move  low-value        to tmo-rec.
           move  intfat-data-from to tmo-data-movim.

           if TuttiClienti
              start tmovmag key is >= k-data
                    invalid set errori to true
              end-start
           else
              move  intfat-tipo-CF  to tmo-tipo
              move  intfat-codice   to tmo-cod-clifor
LUBEXX        move  intfat-des-prog to tmo-destino
              start tmovmag key is >= k2
                    invalid set errori to true
              end-start
           end-if.

           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read tmovmag next at end    exit perform end-read 

                 if not TuttiClienti
                    if intfat-tipo-CF not = tmo-tipo
                       exit perform
                    end-if

                    if intfat-codice not = tmo-cod-clifor
                       exit perform
                    end-if 

LUBEXX              if tmo-data-movim > intfat-data-to or
LUBEXX                 tmo-data-movim < intfat-data-from
LUBEXX                 set record-ok to false
LUBEXX              end-if

                 else

                    if tmo-data-movim > intfat-data-to
                       exit perform
                    end-if

                    if tmo-tipo not = intfat-tipo-CF
                       set record-ok to false
                    end-if

                 end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    if counter = 100
                       display "TMOVMAG" 
                          upon intfat-handle at column 15
                                                  line 03
                   end-if
                    move counter to counter-edit
                    display counter-edit
                       upon intfat-handle at column 21
                                               line 03
                    move 0 to counter2
                 end-if

                 if record-ok
                    move tmo-tipo to cli-tipo-CF
                    move tmo-cod-clifor to cli-codice
                    read clienti no lock
                         invalid set record-ok to false
                     not invalid
                         move cli-tipo to tcl-codice
                         read ttipocli no lock
                              invalid set record-ok to false
                         end-read
                    end-read
                 end-if

                 if record-ok
                    if intfat-gdo not = spaces
      *****                 if cli-gdo = spaces
      *****                 if tcl-gdo-no
      *****                    set record-ok to false
      *****                 else
                          |Capogruppo
                          if intfat-tipo-gdo = 1
                             move cli-gdo to gdo-codice
                             read tgrupgdo no lock 
                                  invalid set record-ok to false 
                              not invalid
                                  if intfat-gdo not = gdo-capogruppo
                                     set record-ok to false
                                  end-if
                             end-read
                          else
                             if intfat-gdo not = cli-gdo
                                set record-ok to false
                             end-if
                          end-if
      *****                 end-if
                    end-if
                 end-if

                 if record-ok
                    if intfat-des-prog not = tmo-destino and
                       intfat-des-prog not = 0
                       set record-ok to false
                    end-if
                 end-if

                 if record-ok
                    perform LOOP-RIGHE
                 end-if

              end-perform
           end-if.

           if not trovato
              display message "Nessun movimento trovato"
                        title titolo
                         icon 2
           else
              |RIPULISCO LA SCREEN DAL CONTATORE
              display "                      "
                 upon intfat-handle at column 15
                                         line 03
              ||||||||
              move 0 to counter counter2
              move low-value to tma-rec
              start tmp-marca key is >= tmp-marca-alfa
                    invalid continue
              end-start
              perform until 1 = 2

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    if counter = 100
                       display "TMP-MAR"
                          upon intfat-handle at column 15
                                                  line 03
                   end-if
                    move counter to counter-edit
                    display counter-edit
                       upon intfat-handle at column 21
                                               line 03
                    move 0 to counter2
                 end-if

                 read tmp-marca next 
                      at end
                      perform SCRIVI-TOTALI
                      exit perform
                 end-read

                 if prima-volta
                    perform ACCETTA-SEPARATORE
                    perform SCRIVI-INTESTAZIONE
                    set prima-volta to false
                 end-if
                 compute como-valore = tma-imp-merce + 
                                       tma-cons + tma-cou
                 move tma-des        to r-des
                 move tma-imp-merce  to r-imp-merce
                 move tma-add-pb     to r-add-pb
                 move tma-cons       to r-cons
                 move tma-cou        to r-cou
                 move como-valore    to r-tot

                 compute como-valore = tma-imp-merce + 
                                       tma-cons + tma-cou +
                                       tma-add-pb
                                             
                 move como-valore    to r-tot2
                 move tma-kg         to r-kg 
                 move tma-qta        to r-qta

                 add tma-add-pb      to tma-imp-merce
                 move tma-imp-merce  to r-tot-imp

                 call "C$JUSTIFY" using r-imp-merce, "R"
                 call "C$JUSTIFY" using r-add-pb,    "R"
                 call "C$JUSTIFY" using r-tot-imp,   "R"
                 call "C$JUSTIFY" using r-cons,      "R"
                 call "C$JUSTIFY" using r-cou,       "R"
                 call "C$JUSTIFY" using r-tot,       "R"
                 call "C$JUSTIFY" using r-tot2,      "R"
                 call "C$JUSTIFY" using r-kg,        "R"
                 call "C$JUSTIFY" using r-qta,       "R"
                 if intfat-excel = 0
                    move r-riga         to line-riga
                 else
                    initialize line-riga
                    string r-des       delimited size
                           separatore  delimited size
                           r-imp-merce delimited size
                           separatore  delimited size
                           r-add-pb    delimited size
                           separatore  delimited size
                           r-tot-imp   delimited size
                           separatore  delimited size
                           r-cons      delimited size
                           separatore  delimited size
                           r-cou       delimited size
                           separatore  delimited size
                           r-tot       delimited size
                           separatore  delimited size
                           r-tot2      delimited size
                           separatore  delimited size
                           r-kg        delimited size
                           separatore  delimited size
                           r-qta       delimited size
                           into line-riga
                    end-string
                 end-if
                 if RowCounter = MaxRows
                    perform SALTO-PAGINA
                 end-if
                 write line-riga
                 add 1 to RowCounter
              end-perform
           end-if.

      ***---
       LOOP-RIGHE.
           if intfat-si-add-pb
              perform TROVA-ORDINE-NOTA
           end-if.
           set  tutto-ok   to true.
           move low-value  to rmo-rec.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           start rmovmag key is >= rmo-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read rmovmag next at end exit perform end-read

                 if tmo-anno   not = rmo-anno  or
                    tmo-numero not = rmo-movim
                    exit perform
                 end-if

                 if record-ok
                    if intfat-articolo not = rmo-articolo and
                       intfat-articolo not = 0
                       set record-ok to false
                    end-if
                 end-if

                 if record-ok
                    move rmo-articolo to art-codice
                    read articoli no lock 
                         invalid
081105                   |Modifica come richiesta da Trivella
                         move art-codice to articolo-edit
                         display message 
                         "ATTENZIONE!!!"
                  x"0d0a""ARTICOLO: " articolo-edit " NON TROVATO!"
                                   title titolo
                                    icon 2
081105                   |FINE MODIFICA
                         set record-ok to false
                    end-read
                 end-if

                 if record-ok
                    if intfat-marca not = 0 and
                       intfat-marca not = art-marca-prodotto 
                       set record-ok to false
                    end-if
                 end-if

                 if record-ok
                    if intfat-promo not = 0
                       if trovato-nota
                          set record-ok to false
                       else
                          set record-ok to false
                          move low-value  to ror-chiave
                          move tor-chiave to ror-chiave
                          start rordini key >= ror-chiave
                                invalid continue
                            not invalid
                                perform until 1 = 2
                                   read rordini next 
                                        at end exit perform 
                                   end-read
                                   if ror-anno       not = tor-anno or
                                      ror-num-ordine not = tor-numero
                                      exit perform
                                   end-if
                                   if ror-cod-articolo = rmo-articolo
                                      if ror-promo = intfat-promo
                                         set record-ok to true
                                      end-if
                                   end-if
                                end-perform
                          end-start   
                       end-if
                    end-if   
                 end-if

                 if record-ok
                    perform VALORIZZA-RIGA
                 end-if

              end-perform
           end-if.

      ***---
       VALORIZZA-RIGA.
           call "w$flush".
           initialize tma-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move tmo-causale        to tca-codice.
           read tcaumag no lock 
                invalid move spaces to tca-descrizione 
           end-read.
           move art-marca-prodotto to tma-marca mar-codice.
           read tmarche no lock 
                invalid move spaces to mar-descrizione
           end-read.
           read tmp-marca invalid continue end-read.
                      
           move 0 to add-pb.
           if intfat-si-add-pb
              if trovato-ordine
                 perform TROVA-ADDIZIONALE-ORDINE
              end-if

              if trovato-nota
                 perform TROVA-ADDIZIONALE-NOTA
              end-if
              compute rmo-netto = rmo-netto - add-pb
           end-if.

           move mar-descrizione  to tma-des.
           if rmo-qta = 0 move 1 to rmo-qta end-if.
           move 0 to qta-batt.
           if art-si-cobat
              move rmo-qta to qta-batt
           end-if.

           if tca-imponibile-pos
              compute tma-imp-merce = 
                      tma-imp-merce + ( rmo-netto    * rmo-qta )
              compute tma-add-pb    = 
                      tma-add-pb    + ( add-pb       * rmo-qta )

              compute tma-cons      = 
                      tma-cons      + ( rmo-imp-cons * rmo-qta )
              compute tma-cou       = 
                      tma-cou       + ( rmo-coubat   * rmo-qta )

              |CALCOLO DEI TOTALIZZATORI FINALI
              compute tot-imp       = 
                      tot-imp       + ( rmo-netto    * rmo-qta )
              compute tot-add-pb    = 
                      tot-add-pb    + ( add-pb       * rmo-qta )

              compute tot-tot-imp   = tot-imp + tot-add-pb

              compute tot-cons      = 
                      tot-cons      + ( rmo-imp-cons * rmo-qta )
              compute tot-cou       = 
                      tot-cou       + ( rmo-coubat   * rmo-qta )

              compute tma-kg         =
                      tma-kg         +
                      ( rmo-peso-tot + rmo-peso-tot-utf )

              compute tot-kg         =
                      tot-kg         +
                      ( rmo-peso-tot + rmo-peso-tot-utf)

              compute tot-qta        =
                      tot-qta        + qta-batt

              compute tma-qta        =
                      tma-qta        + qta-batt
           else
              compute tma-imp-merce = 
                      tma-imp-merce - ( rmo-netto     * rmo-qta )
              compute tma-add-pb    = 
                      tma-add-pb    - ( add-pb       * rmo-qta )

              compute tma-cons      = 
                      tma-cons      - ( rmo-imp-cons * rmo-qta )
              compute tma-cou       = 
                      tma-cou       - ( rmo-coubat   * rmo-qta )

              |CALCOLO DEI TOTALIZZATORI FINALI
              compute tot-imp       = 
                      tot-imp       - ( rmo-netto    * rmo-qta )
              compute tot-add-pb    = 
                      tot-add-pb    - ( add-pb       * rmo-qta )
              compute tot-tot-imp   = tot-imp + tot-add-pb

              compute tot-cons      = 
                      tot-cons      - ( rmo-imp-cons * rmo-qta )
              compute tot-cou       = 
                      tot-cou       - ( rmo-coubat   * rmo-qta )

              compute tma-kg         =
                      tma-kg         -
                      ( rmo-peso-tot + rmo-peso-tot-utf )

              compute tot-kg         =
                      tot-kg         -
                      ( rmo-peso-tot + rmo-peso-tot-utf)

              compute tot-qta        =
                      tot-qta        - qta-batt

              compute tma-qta        =
                      tma-qta        - qta-batt

           end-if.
           write tma-rec invalid rewrite tma-rec end-write.
           set trovato        to true.

      ***---
       SCRIVI-INTESTAZIONE.
           move intfat-data-to(1:4)   to t-data-to(7:4).
           move "/"                   to t-data-to(6:1).
           move intfat-data-to(5:2)   to t-data-to(4:2).
           move "/"                   to t-data-to(3:1).
           move intfat-data-to(7:2)   to t-data-to(1:2).

           move intfat-data-from(1:4) to t-data-from(7:4).
           move "/"                   to t-data-from(6:1).
           move intfat-data-from(5:2) to t-data-from(4:2).
           move "/"                   to t-data-from(3:1).
           move intfat-data-from(7:2) to t-data-from(1:2).

           if intfat-excel = 0
              call "C$JUSTIFY" using testata, "C"
              move testata to line-riga
              add 1 to RowCounter
           else
              initialize line-riga
              string te-filler    delimited size
                     " **"        delimited size
                     into line-riga
              end-string
              write line-riga
              initialize line-riga
              string "** "       delimited size
                     t-data-from delimited size
                     " - "       delimited size
                     t-data-to   delimited size
                     " **"       delimited size
                     into line-riga
              end-string
           end-if.

           write line-riga.

           move spaces to line-riga.
           write line-riga.
           add 1 to RowCounter.

           if intfat-cliente
              move intfat-gdo to t-cod-gdo gdo-codice
              if gdo-codice not = spaces
                 read tgrupgdo no lock
                      invalid move spaces           to t-des-gdo
                  not invalid move gdo-intestazione to t-des-gdo
                 end-read
              end-if
              move t-gdo to line-riga
           else
              move spaces to line-riga
           end-if.
           write line-riga.
           add 1 to RowCounter.

           if intfat-cliente move "Cliente"   to t-clifor
           else              move "Fornitore" to t-clifor
           end-if.
           move intfat-codice  to t-cod-clifor cli-codice.
           move intfat-tipo-CF to cli-tipo-CF.
           read clienti no lock
                invalid continue
            not invalid
                move cli-ragsoc-1 to t-ragsoc-clifor
           end-read.
           move t-cliente-fornitore to line-riga.
           write line-riga.
           add 1 to RowCounter.

           if intfat-cliente
              move intfat-codice   to des-codice
              move intfat-des-prog to des-prog
              read destini no lock
                   invalid move cli-localita to t-des-destino
                           move spaces       to t-prg-destino
               not invalid move des-localita to t-des-destino
                           move des-prog     to t-prg-destino
              end-read
              move t-destino to line-riga
           else
              move spaces    to line-riga
           end-if.
           write line-riga.
           add 1 to RowCounter.

           move intfat-articolo to t-cod-art art-codice.
           if art-codice = 0
              move "Tutti gli articoli" to t-des-art
           else
              read articoli no lock
                   invalid  move spaces          to t-des-art
               not invalid  move art-descrizione to t-des-art
              end-read
           end-if.
           move t-articolo to line-riga.
           write line-riga.
           add 1 to RowCounter.

           move spaces to line-riga.
           write line-riga.

           if intfat-excel = 0
              call "C$JUSTIFY" using t-prg-destino, "R"
              call "C$JUSTIFY" using t-cod-clifor,  "R"
              call "C$JUSTIFY" using t-cod-art,     "R"
              call "C$JUSTIFY" using t-2,           "R"
              call "C$JUSTIFY" using t-3,           "R"
              call "C$JUSTIFY" using t-4,           "R"
              call "C$JUSTIFY" using t-5,           "R"
              call "C$JUSTIFY" using t-6,           "R"
              call "C$JUSTIFY" using t-7,           "R"
              call "C$JUSTIFY" using t-8,           "R"
              call "C$JUSTIFY" using t-9,           "R"
              call "C$JUSTIFY" using t-10,          "R"
              add 1 to RowCounter
              move  riga-div to line-riga
              write line-riga
              add 1 to RowCounter        
              move  tit-riga to line-riga
           else
              initialize line-riga
              string t-1        delimited size
                     separatore delimited size
                     t-2        delimited size
                     separatore delimited size
                     t-3        delimited size
                     separatore delimited size
                     t-4        delimited size
                     separatore delimited size
                     t-5        delimited size
                     separatore delimited size
                     t-6        delimited size
                     separatore delimited size
                     t-7        delimited size
                     separatore delimited size
                     t-8        delimited size
                     separatore delimited size
                     t-9        delimited size
                     separatore delimited size
                     t-10       delimited size
                     into line-riga
              end-string
           end-if.
           write line-riga.

           if intfat-excel = 0
              add 1 to RowCounter
              move  riga-div to line-riga
              write line-riga
              add 1 to RowCounter
           end-if.

      ***---
       SCRIVI-TOTALI.
           if RowCounter > MaxRows - 3
              perform SALTO-PAGINA
           end-if.
           if intfat-excel = 0
              move riga-div to line-riga
              add 1 to RowCounter
           else
              move spaces to line-riga
           end-if.
           write line-riga.
           compute tot-valore  = tot-imp  + tot-cons + tot-cou.
           compute tot-valore2 = tot-imp  + tot-cons + tot-cou + 
                                 tot-add-pb.
           move tot-imp     to t-imp-merce.
           move tot-add-pb  to t-add-pb.
           move tot-tot-imp to t-tot-imp.
           move tot-cons    to t-cons.
           move tot-cou     to t-cou.
           move tot-valore  to t-totale.
           move tot-valore2 to t-totale2.
           move tot-kg      to t-kg.
           move tot-qta     to t-qta.
           if intfat-excel = 0
              move r-totali to line-riga
           else
              initialize line-riga
              string t-filler    delimited size
                     separatore  delimited size
                     t-imp-merce delimited size
                     separatore  delimited size
                     t-add-pb    delimited size
                     separatore  delimited size
                     t-tot-imp   delimited size
                     separatore  delimited size
                     t-cons      delimited size
                     separatore  delimited size
                     t-cou       delimited size
                     separatore  delimited size
                     t-totale    delimited size
                     separatore  delimited size
                     t-totale2   delimited size
                     separatore  delimited size
                     t-kg        delimited size
                     separatore  delimited size
                     t-qta       delimited size
                     into line-riga
              end-string
           end-if.
           write line-riga.
           if intfat-excel = 0
              add 1 to RowCounter
              move riga-div to line-riga
           else
              move spaces to line-riga
           end-if.
           write line-riga.

      ***---
       SALTO-PAGINA.
           if intfat-excel = 0
              write line-riga from spaces after page
              move 0 to RowCounter
           end-if.
           continue.

      ***---
       CLOSE-FILES.
           close articoli
                 tmarche
                 tcaumag
                 tmovmag
                 rmovmag
                 tmp-marca
                 lineseq
                 clienti
                 destini
                 tgrupgdo
                 tordini
                 rordini
                 tnotacr
                 rnotacr
                 ttipocli.
           delete file tmp-marca.
           if not trovato
              delete file lineseq
              move spaces to wstampa
           end-if.
  
      ***---
       EXIT-PGM.
           if intfat-excel = 1 and trovato
              perform CALL-EXCEL
           end-if.
           move wstampa to intfat-path.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
           copy "recupero-addizionale.cpy".
