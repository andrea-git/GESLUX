       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-cumulato-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
           copy "tagli.sl".
           copy "tmarche.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "clienti.sl".
           copy "ttipocli.sl".
           copy "articoli.sl".
           copy "tcaumag.sl".
           copy "tmp-cumulato.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tagli.fd".
           copy "tmarche.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "clienti.fd".
           copy "ttipocli.fd".
           copy "articoli.fd".
           copy "tcaumag.fd".
           copy "tmp-cumulato.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "pie-di-pagina.def".

       77  status-tagli          pic xx.
       77  status-tmarche        pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-articoli       pic xx.
       77  status-tcaumag        pic xx.
       77  status-clienti        pic xx.
       77  status-ttipocli       pic xx.
       77  status-tmp-cumulato   pic xx.
       77  path-tmp-cumulato     pic x(256).
       
      * COSTANTI
       78  titolo                value "GESLUX - Inevaso cumulato".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 33.

      * RIGHE PER LA STAMPA
       01  r-data-from           pic x(10).
       01  r-data-to             pic x(10).

       01  r-titolo              pic x(40).

       01  r-intesta.
           05 filler             pic x(5)  value "Marca".
           05 filler             pic x(7)  value "Inevaso".
           05 filler             pic x(9)  value "Fatturato".
           05 filler             pic x(1)  value "%".

       01  r-riga-marca-perce.
           05 r-marca            pic x(30).
           05 r-perce            pic x(9). 

       01  r-riga-inev-fatt.
           05 r-inev             pic x(19).
           05 r-fatt             pic x(16).

       01  r-tot-fissa.
           05 filler             pic x(12) value "TOT INEVASO:".
           05 filler             pic x(14) value "TOT FATTURATO:".
           05 filler             pic x     value "%".

       01  r-totale.
           05 r-tot-inev         pic x(15).
           05 r-tot-fatt         pic x(15).
           05 r-tot-perce        pic x(9).

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.

      * VARIABILI
       77  perce                 pic s9(4)v99.
       77  tot-inev              pic s9(12)v99.
       77  tot-fatt              pic s9(12)v99.
       77  tot-perce             pic s9(4)v99.
       77  valore-z              pic ----.---.--9,99.
       77  valore-x              pic x(15).
       77  como-valore           pic 9(9)v99.

       77  num-righe             pic 9(3).

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana20BI           handle of font.
       77  Verdana14B            handle of font.
       77  Verdana12BI           handle of font.
       77  Verdana12B            handle of font.
       77  Verdana10BI           handle of font.
       77  Verdana8B             handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       77  link-data-from        pic 9(8).
       77  link-data-to          pic 9(8).
       77  link-handle           handle of window.

      ******************************************************************
       PROCEDURE DIVISION using link-data-from, 
                                link-data-to, 
                                link-handle.

       DECLARATIVES.
      ***---
       TAGLI-ERR SECTION.
           use after error procedure on tagli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tagli
           when "39"
                set errori to true
                display message "File [TAGLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TAGLI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TAGLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
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
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [ARTICOLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcaumag
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
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TCAUMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "39"
                set errori to true
                display message "File [CLIENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CLENTI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [CLIENTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ttipocli
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
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TTIPOCLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini
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
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini
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
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "39"
                set errori to true
                display message "File [TNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TNOTACR] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rnotacr
           when "39"
                set errori to true
                display message "File [RNOTACR] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RNOTACR] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RNOTACR] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmarche
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
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TMARCHE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMP-CUMLATO-ERR SECTION.
           use after error procedure on tmp-cumulato.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-cumulato
           when "39"
                set errori to true
                display message "File [TMP-CUMULATO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-CUMULATO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "Impossibile procedere."
                   x"0d0a""File [TMP-CUMULATO] inesistente"
                          title titolo
                           icon 2
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
                     open output tmp-cumulato
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           when "99" set RecLocked to true
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
           string link-data-from(7:2) delimited size
                  "/"                 delimited size
                  link-data-from(5:2) delimited size
                  "/"                 delimited size
                  link-data-from(1:4) delimited size
                  into r-data-from
           end-string.

           string link-data-to(7:2) delimited size
                  "/"               delimited size
                  link-data-to(5:2) delimited size
                  "/"               delimited size
                  link-data-to(1:4) delimited size
                  into r-data-to
           end-string.
                        
           move 0    to counter counter2.
           move 0    to num-righe tot-inev tot-fatt.
           move 0,7  to passo.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.
           initialize path-tmp-cumulato.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-tmp-cumulato  from environment "PATH_ST".
           inspect path-tmp-cumulato  replacing trailing 
                                      spaces by low-value.
           string  path-tmp-cumulato  delimited low-value
                   "TMP-CUMULATO"     delimited size
                   "_"                delimited size
                   como-data          delimited size
                   "_"                delimited size
                   como-ora           delimited size
                   ".tmp"             delimited size
                   into path-tmp-cumulato
           end-string.

      ***---
       OPEN-FILES.
           open output tmp-cumulato.
           if tutto-ok
              close tmp-cumulato
              open i-o tmp-cumulato allowing readers
              open input tagli tordini rordini tnotacr rnotacr 
                         tmarche articoli clienti ttipocli tcaumag
           end-if.

      ***---
       ELABORAZIONE.
           move low-value      to tag-rec.
           move link-data-from to tag-data.
      
           start tagli key >= tag-chiave
                 invalid set errori to true
           end-start.
      
           if tutto-ok
              perform until 1 = 2
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 50
                    move counter to counter-edit
                    display "TAGLI"
                       upon link-handle at column 08,00 line 07,00
                    display counter-edit
                       upon link-handle at column 18,00 line 07,00
                    move 0 to counter2
                 end-if                                  

                 read tagli next at end exit perform end-read
      
                 if tag-data > link-data-to
                    exit perform
                 end-if
      
                 set trovato to true
                 initialize tmp-cum-rec
                            mar-rec 
                            art-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move tag-articolo to art-codice
                 read articoli no lock invalid continue end-read

                 if art-scorta not = 2
                    move art-marca-prodotto to tmp-cum-marca
                    read tmp-cumulato no lock 
                         invalid 
                         move tmp-cum-marca to mar-codice
                         read tmarche    no lock 
                              invalid continue 
                         end-read
                         move mar-descrizione to tmp-cum-mar-descrizione
                    end-read

                    compute tmp-cum-inev = 
                            tmp-cum-inev + ( tag-qta * tag-prz )
         
                    write tmp-cum-rec 
                          invalid rewrite tmp-cum-rec 
                    end-write
                 end-if

              end-perform
      
           end-if.

           if trovato
              move 0 to counter counter2
              display "                                                "
                 upon link-handle at column 05,00 line 07,00
              display "TORDINI"
                 upon link-handle at column 08,00 line 07,00

              move low-value           to tor-rec
              move link-data-from      to tor-data-fattura
              move link-data-from(1:4) to tor-anno-fattura
      
              start tordini key >= k4
                    invalid set errori to true
              end-start
      
              if tutto-ok
                 perform until 1 = 2

                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 50
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 18,00 line 07,00
                       move 0 to counter2
                    end-if

                    read tordini next at end exit perform end-read
      
                    if tor-data-fattura > link-data-to
                       exit perform
                    end-if

LUBEXX              |Come da richiesta 15/10/07: considerare solo GDO
                    set  cli-tipo-C  to true
                    move tor-cod-cli to cli-codice
                    read clienti no lock 
                         invalid continue 
                     not invalid
                         move cli-tipo to tcl-codice
                         read ttipocli no lock
                              invalid continue
                          not invalid
                              move tor-causale to tca-codice
                              read tcaumag no lock 
                                   invalid continue 
                              end-read
                              if tcl-gdo-si and tca-si-stat
                                 perform LOOP-RIGHE-RORDINI
                              end-if
                         end-read
                    end-read

                 end-perform
      
              end-if
                                                                       
LUBEXX        |Come da richiesta 15/10/07: sottrarre le note credito
              set tutto-ok to true
              move 0 to counter counter2
              display "                                                "
                 upon link-handle at column 05,00 line 07,00
              display "TNOTACR"
                 upon link-handle at column 08,00 line 07,00

              move low-value           to tno-rec
              move link-data-from      to tno-data-fattura
              move link-data-from(1:4) to tno-anno-fattura

              start tnotacr key >= k4
                    invalid set errori to true
              end-start

              if tutto-ok
                 perform until 1 = 2
      
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 50
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 18,00 line 07,00
                       move 0 to counter2
                    end-if
      
                    read tnotacr next at end exit perform end-read
      
                    if tno-data-fattura > link-data-to
                       exit perform
                    end-if
      
LUBEXX              |Come da richiesta 15/10/07: considerare solo GDO
                    set  cli-tipo-C  to true
                    move tno-cod-cli to cli-codice
                    read clienti no lock 
                         invalid continue 
                     not invalid
                         move cli-tipo to tcl-codice
                         read ttipocli no lock
                              invalid continue
                          not invalid
                              move tno-causale to tca-codice
                              read tcaumag no lock 
                                   invalid continue 
                              end-read
                              if tcl-gdo-si and tca-si-stat
                                 perform LOOP-RIGHE-RNOTACR
                              end-if
                         end-read
                    end-read
      
                 end-perform
      
              end-if
              set tutto-ok to true
           end-if.

           if not trovato
              display message "Nessun taglio trovato nel periodo"
                        title titolo
                         icon 2
           else
              perform STAMPA
           end-if.

      ***---
       LOOP-RIGHE-RORDINI.
           move low-value  to ror-chiave.
           move tor-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if

                    initialize tmp-cum-rec
                               art-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move ror-cod-articolo to art-codice
                    read articoli no lock invalid continue end-read

                    move art-marca-prodotto to tmp-cum-marca
                    read tmp-cumulato no lock 
                         invalid 
                         |IN AUTOMATICO FTMA DATO CHE NON HA
                         |ARTICOLO E QUINDI NEMMENO MARCA
                         continue
                     not invalid

                         if ror-qta = 0
                            compute como-valore = ror-prz-unitario
                         else
                            compute como-valore      =
                                    ror-prz-unitario *
                                  ( ror-qta - ror-qta-omaggi )
                         end-if

                         compute tmp-cum-fatt =
                                 tmp-cum-fatt +
                                 como-valore

                         rewrite tmp-cum-rec
                                 invalid continue
                         end-rewrite
                    end-read
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-RNOTACR.
           move low-value  to rno-chiave.
           move tno-chiave to rno-chiave.
           start rnotacr key >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rnotacr next at end exit perform end-read
                    if rno-anno   not = tno-anno or
                       rno-numero not = tno-numero
                       exit perform
                    end-if

                    initialize tmp-cum-rec
                               art-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move rno-cod-articolo to art-codice
                    read articoli no lock invalid continue end-read
      
                    move art-marca-prodotto to tmp-cum-marca
                    read tmp-cumulato no lock
                         invalid  
                         |IN AUTOMATICO NCNC DATO CHE NON HA
                         |ARTICOLO E QUINDI NEMMENO MARCA
                         continue
                     not invalid

                         if rno-qta = 0
                            compute como-valore =
                                    rno-prz-unitario +
                                    rno-imp-consumo  +
                                    rno-imp-cou-cobat
                         else
                            compute como-valore =
                                  ( rno-prz-unitario  +
                                    rno-imp-consumo   +
                                    rno-imp-cou-cobat ) * rno-qta
                         end-if
      
                         compute tmp-cum-fatt =
                                 tmp-cum-fatt -
                                 como-valore

                         rewrite tmp-cum-rec
                                 invalid continue
                         end-rewrite
                    end-read
                 end-perform
           end-start.

      ***---
       STAMPA.
           initialize spooler-link.
           call   "selprint" using selprint-linkage.
           cancel "selprint".

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              move titolo to spl-nome-job
              set spl-apertura to true
              set spl-vertical to true
              set WFDEVICE-WIN-PRINTER    to true
              call "spooler" using spooler-link
              if spl-sta-annu
                 set errori to true
              else
                 move spl-altezza to save-altezza-pagina
                 perform CARICA-FONT
              end-if
           else
              set spl-sta-annu to true
              set errori to true
           end-if.

           if tutto-ok
              perform CONTA-RIGHE
              move 1 to pagina
              evaluate true
              when num-righe < 78-MaxRows
                   move 1 to tot-pag
              when num-righe = 78-MaxRows
                   move 2 to tot-pag
              when other
                   divide num-righe by 78-MaxRows 
                   giving tot-pag   remainder resto
                   if resto > 0 add 1 to tot-pag end-if
              end-evaluate

              move high-value to tmp-cum-rec
              start tmp-cumulato key <= k-ord
                    invalid continue
                not invalid
                    perform SCRIVI-INTESTAZIONE
                    perform until 1 = 2
                       read tmp-cumulato previous
                            at end exit perform 
                       end-read
      
                       if num-righe >= 78-MaxRows
                          perform SALTO-PAGINA
                       end-if

                       perform STAMPA-FRAMES-RIGA
      
                       move Verdana8B               to spl-hfont
      
                       move tmp-cum-mar-descrizione to r-marca

                       if tmp-cum-fatt not = 0
                          compute perce rounded =
                          ( tmp-cum-inev * 100 ) / tmp-cum-fatt
                          perform FORMAT-PERCE
                       else
                          move spaces to r-perce
                       end-if
      
                       perform FORMAT-INEV
                       perform FORMAT-FATT
      
                       add tmp-cum-inev to tot-inev
                       add tmp-cum-fatt to tot-fatt
      
                       move r-riga-marca-perce to spl-riga-stampa
                       move 73                 to spl-tipo-colonna
                       set  spl-nero           to true
                       perform SCRIVI

                       subtract passo from save-riga
      
                       move r-riga-inev-fatt to spl-riga-stampa
                       move 73,5             to spl-tipo-colonna
                       set  spl-rosso        to true
                       perform SCRIVI

                       perform STAMPA-LINEA-RIGA
      
                       add 1 to num-righe
      
                 end-perform
      
                 if num-righe >= 78-MaxRows - 2
                    perform SALTO-PAGINA
                 end-if

                 perform SCRIVI-TOTALE
      
                 perform PIE-DI-PAGINA

                 set spl-chiusura to true
                 call   "spooler" using spooler-link

           end-start.

      ***---
       FORMAT-INEV.
           move tmp-cum-inev   to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-inev.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-inev
           end-string.

      ***---
       FORMAT-FATT.
           move tmp-cum-fatt   to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-fatt.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-fatt
           end-string.

      ***---
       FORMAT-PERCE.
           move perce          to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"

           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-perce.
           string valore-x  delimited low-value
                  "%"       delimited size
                  into r-perce
           end-string.

      ***---
       FORMAT-TOT-INEV.
           move tot-inev       to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-tot-inev.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-tot-inev
           end-string.

      ***---
       FORMAT-TOT-FATT.
           move tot-fatt       to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-tot-fatt
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-tot-fatt
           end-string.

      ***---
       FORMAT-TOT-PERCE.
           move tot-perce      to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-tot-perce.
           string valore-x  delimited low-value
                  "%"       delimited size
                  into r-tot-perce
           end-string.

      ***---
       SCRIVI-TOTALE.
           perform STAMPA-FRAMES-TOTALE.
           subtract 0,7 from save-riga.
           move Verdana12BI to spl-hfont.
           move r-tot-fissa to spl-riga-stampa.
           move 74          to spl-tipo-colonna.
           set  spl-blu     to true.
           perform SCRIVI.
                                   
           perform FORMAT-TOT-INEV.
           perform FORMAT-TOT-FATT.

           if tot-fatt not = 0
              compute tot-perce rounded = ( tot-inev * 100 ) / tot-fatt
              perform FORMAT-TOT-PERCE
           end-if.

           subtract 0,11 from save-riga.
           move Verdana12B to spl-hfont.
           move r-totale   to spl-riga-stampa.
           move 74,5       to spl-tipo-colonna.
           set  spl-rosso  to true.
           perform SCRIVI.

      ***---
       STAMPA-FRAMES-TOTALE.
           move 8     to spl-pen-with.

           add  1,0        to save-riga.
           move save-riga  to spl-riga.

           add 1,15    to spl-riga giving spl-riga-fine.

           move 4,0   to spl-colonna.
           move 15,9  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,05 to spl-riga.
           add 0,48 to spl-riga giving spl-riga-fine.

           move 4     to spl-pen-with.
           move 4,05  to spl-colonna.
           move 8,4   to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.

           move 8,6   to spl-colonna.
           move 12,9  to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.

           move 13,1  to spl-colonna.
           move 15,85 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.

      ***---
       SCRIVI-INTESTAZIONE.
           move 0        to num-righe.

           move 0,4              to save-riga.
           move "** INEVASO MARCA CUMULATO **" to spl-riga-stampa.
           move Verdana20BI      to spl-hfont.
           move 58               to spl-tipo-colonna.
           set spl-blu           to true.
           perform SCRIVI.

           perform STAMPA-FRAMES-DATE.

           initialize r-titolo.
           string "DAL:  "    delimited size
                  r-data-from delimited size
                  "       "   delimited size
                  "AL:  "     delimited size
                  r-data-to   delimited size
                  into r-titolo
           end-string.

           move 1,50               to save-riga.
           move r-titolo           to spl-riga-stampa.
           move Verdana14B         to spl-hfont.
           move 58                 to spl-tipo-colonna.
           set  spl-rosso          to true.
           perform SCRIVI.
      
           move 2,5                to save-riga.
           move r-intesta          to spl-riga-stampa.
           move Verdana10BI        to spl-hfont.
           move 72                 to spl-tipo-colonna.
           set spl-blu to true.
           perform SCRIVI.
      
           move 3,5 to save-riga.
           perform STAMPA-LINEA.
           move 3,3                to save-riga.
            
      ***---
       STAMPA-LINEA.
           move 10                 to spl-pen-with.
           move 2,0                to spl-colonna.
           move 18,0               to spl-colonna-fine.
           add 0,24 to save-riga    giving spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-rosso          to true.
           call "spooler"       using spooler-link.
            
      ***---
       STAMPA-LINEA-RIGA.
           move 2                  to spl-pen-with.
           move 4,0                to spl-colonna.
           move 15,9               to spl-colonna-fine.
           add 0,52 to  save-riga giving spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.

      ***---
       SALTO-PAGINA.
           perform PIE-DI-PAGINA.
           add 1 to pagina.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           perform SCRIVI-INTESTAZIONE.
       
      ***---
       STAMPA-FRAMES-DATE.
           move 4     to spl-pen-with.
           move 2,22  to spl-riga.
           move 2,85  to spl-riga-fine.
           move 6,0   to spl-colonna.
           move 9,8   to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.

           move 4     to spl-pen-with.
           move 11,95 to spl-colonna.
           move 15,75 to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.
       
      ***---
       STAMPA-FRAMES-RIGA.
           move 4      to spl-pen-with.
           add passo   to save-riga giving spl-riga.
           subtract 0,04 from spl-riga.
           add 0,44    to spl-riga  giving spl-riga-fine.
           move 9,0    to spl-colonna.
           move 12,40  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.

           move 4     to spl-pen-with.
           move 12,60 to spl-colonna.
           move 15,90 to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-solid   to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.

      ***---
       CONTA-RIGHE.
           |Il totale finale
           move 2 to num-righe.
           move low-value to tmp-cum-rec.
           start tmp-cumulato key >= tmp-cum-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-cumulato next at end exit perform end-read
                    add 1 to num-righe
                 end-perform
           end-start.

      ***---
       CARICA-FONT.
      * Verdana 20BI
           initialize wfont-data Verdana20BI.
           move 20 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana20BI, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 14B
           initialize wfont-data Verdana14B.
           move 14 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana14B, wfont-data
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

      * Verdana 12BI
           initialize wfont-data Verdana12BI.
           move 12 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana12BI, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 12B
           initialize wfont-data Verdana12B.
           move 12 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana12B, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 10BI
           initialize wfont-data Verdana10BI.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10BI, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 8B
           initialize wfont-data Verdana8B.
           move 8 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana8B, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close tagli tordini rordini tmarche ttipocli
                 articoli tnotacr rnotacr clienti tcaumag.
           delete file tmp-cumulato.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana20BI.
           destroy Verdana14B.
           destroy Verdana12BI.
           destroy Verdana12B.
           destroy Verdana10BI.
           destroy Verdana8B.
           destroy font-pie-pagina.

           cancel "spooler".
           initialize spooler-link.
           display "                                                "
              upon link-handle at column 05,00 line 07,00.

           goback.

      ***---
       PARAGRAFO-COPY.
           copy "pie-di-pagina.cpy".
