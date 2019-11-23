       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-ord-da-fat-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "rordini.sl". 
           copy "clienti.sl".
           copy "tgrupgdo.sl".
           copy "tcaumag.sl".
           copy "tmp-ord-da-fat.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd". 
           copy "rordini.fd". 
           copy "clienti.fd".
           copy "tgrupgdo.fd".
           copy "tcaumag.fd".
           copy "tmp-ord-da-fat.fd".
           copy "ttipocli.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "pie-di-pagina.def".

       77  status-tordini                 pic xx.
       77  status-rordini                 pic xx.
       77  status-clienti                 pic xx.
       77  status-tcaumag                 pic xx.
       77  status-tgrupgdo                pic xx.
       77  status-ttipocli                pic xx.
       77  status-tmp-ord-da-fat          pic xx.
       77  path-tmp-ord-da-fat            pic x(256).

      * COSTANTI
       78  titolo                value "Ordini da fatturare".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 33.

      * RIGHE PER LA STAMPA
       01  riga-titolo           pic x(48).

       01  riga-intestazione.
         05 filler               pic x(7) value "Cliente".
         05 filler               pic x(6) value "Totale".

       01  r-riga.
         05 r-gdo                pic x(35).
         05 r-valore             pic zzz.zzz.zzz.zz9,99. 

       01  t-riga.
         05 t-gdo                pic x(35).
         05 t-valore             pic x(20).

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.

      * VARIABILI
       77  num-righe             pic 9(3).
       77  valore-z              pic zzz.zzz.zzz.zz9,99.
       77  valore-x              pic x(18).
       77  totale                pic 9(12)v99.

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana16BI           handle of font.
       77  Verdana12BI           handle of font.
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
       TORDINI SECTION.
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

       RORDINI SECTION.
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

       CLIENTI SECTION.
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
                display message "[CLIENTI] Indexed file corrupt!"
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

       TCAUMAG SECTION.
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

       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "39"
                set errori to true
                display message "File [TGRUPGDO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TGRUPGDO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TGRUPGDO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       TMP-ORD-DA-FAT-ERR SECTION.
           use after error procedure on tmp-ord-da-fat.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-ord-da-fat
           when "39"
                set errori to true
                display message "File [TMP-ORD-DA-FAT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-ORD-DA-FAT] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "Impossibile procedere."
                   x"0d0a""File [TMP-ORD-DA-FAT] inesistente"
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
                     open output tmp-ord-da-fat
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
           move 0 to num-righe.
           set environment "PRINTER" to "-P SPOOLER".
           move 0,3      to passo.
           move 0        to counter counter2.
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.
           initialize path-tmp-ord-da-fat.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-tmp-ord-da-fat from environment "PATH_ST".
           inspect path-tmp-ord-da-fat replacing trailing 
                                       spaces by low-value.
           string  path-tmp-ord-da-fat delimited low-value
                   "TMP-ORD-DA-FAT"    delimited size
                   "_"                 delimited size
                   como-data           delimited size
                   "_"                 delimited size
                   como-ora            delimited size
                   ".tmp"              delimited size
                   into path-tmp-ord-da-fat
           end-string.

      ***---
       OPEN-FILES.
           open output tmp-ord-da-fat.
           if tutto-ok
              close tmp-ord-da-fat
              open i-o tmp-ord-da-fat allowing readers
              open input clienti  tordini rordini 
                         tgrupgdo tcaumag ttipocli
           end-if.

      ***---
       ELABORAZIONE.
           move low-value      to tor-rec.
           move link-data-from to tor-data-creazione.
           start tordini key is >= k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    add 1 to counter
                    add 1 to counter2
                    if counter2 = 20
                       move counter to counter-edit
                       display counter-edit
                          upon link-handle at column 18,00 line 07,00
                       move 0 to counter2
                    end-if
                    read tordini next at end exit perform end-read
                    if tor-data-creazione > link-data-to
                       exit perform
                    end-if
                    if tor-anno-fattura = 0 and
                       tor-num-fattura  = 0 and
                       tor-data-fattura = 0
                       move tor-causale to tca-codice
                       read tcaumag no lock 
                            invalid continue 
                        not invalid
                            if tca-si-emissione
                               perform LOOP-RIGHE
                            end-if
                       end-read
                    end-if
                 end-perform
           end-start.

           if not trovato
              display message "Nessun ordine da fatturare nel periodo"
                        title titolo
                         icon 2
           else
              perform STAMPA
           end-if.

      ***---
       LOOP-RIGHE.
           move tor-chiave to ror-chiave.
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
                    initialize cli-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    set  cli-tipo-C  to true
                    move tor-cod-cli to cli-codice
                    read clienti no lock invalid continue end-read
                    move cli-tipo to tcl-codice
                    read ttipocli no lock invalid continue end-read
      *****              if cli-gdo not = spaces
                    if tcl-gdo-si or tcl-gdo-opz
                       subtract ror-qta-omaggi from ror-qta
                       set trovato  to true
                       initialize gdo-rec tord-rec
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move cli-gdo to tord-gdo
                       read tmp-ord-da-fat no lock
                            invalid
                            add 1 to num-righe
                            move cli-gdo to gdo-codice
                            read tgrupgdo invalid continue end-read
                            move gdo-intestazione to tord-intestazione
                       end-read
                       compute tord-valore = 
                               tord-valore +
                             ( ror-qta * ror-prz-unitario )
                       write tord-rec invalid rewrite tord-rec end-write
                    end-if
                 end-perform
           end-start.

      ***---
       STAMPA.
           initialize spooler-link.
           call   "selprint" using selprint-linkage
           cancel "selprint".

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              move "GESLUX - Ordini da fatt." to spl-nome-job
              set spl-apertura to true
              set spl-vertical to true
              set WFDEVICE-WIN-PRINTER    to true
              call "spooler" using spooler-link
              if spl-sta-annu
                 set errori to true
              else                                    
                 move spl-altezza to save-altezza-pagina
                 perform CARICA-FONT
                 perform SCRIVI-INTESTAZIONE
              end-if
           else
              set spl-sta-annu to true
              set errori to true
           end-if.
           if tutto-ok
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
              move low-value to tord-gdo
              start tmp-ord-da-fat key >= tord-chiave
                    invalid continue
                not invalid
                    move 0 to totale
                    perform until 1 = 2
                       read tmp-ord-da-fat next 
                            at end
                            if save-riga > save-altezza-pagina - 1,5
                               perform SALTO-PAGINA
                            end-if
                            set spl-blu to true
                            move 10 to spl-pen-with
                            perform STAMPA-LINEA
                            set spl-rosso    to true
                            move Verdana12BI to spl-hfont
                            move "Totale giornaliero:" to t-gdo
                            perform FORMAT-TOTALE
                            move 56                to spl-tipo-colonna
                            move t-riga            to spl-riga-stampa
                                                 
                            perform STAMPA-FRAME

                            set spl-rosso to true
                            perform SCRIVI

                            perform PIE-DI-PAGINA

                            exit perform 
                       end-read

                       if save-riga > save-altezza-pagina - 1
                          perform SALTO-PAGINA
                       end-if

                       move tord-intestazione to r-gdo
                       move tord-valore       to r-valore 
                       move r-riga            to spl-riga-stampa
                       set  spl-nero          to true
                       move 55                to spl-tipo-colonna
                       move Verdana8B         to spl-hfont

                       perform SCRIVI

                       add 0,2 to save-riga
                       set  spl-rosso         to true
                       move 2                 to spl-pen-with
                       perform STAMPA-LINEA
                       subtract 0,1 from save-riga

                       add tord-valore to totale
                    end-perform
              end-start

              set spl-chiusura to true
              call   "spooler" using spooler-link

           end-if.

      ***---
       FORMAT-TOTALE.
           move totale         to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize t-valore.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into t-valore
           end-string.

      ***---
       SCRIVI-INTESTAZIONE.
           initialize riga-titolo.
           move titolo            to riga-titolo
           move 0,5               to save-riga.
           move 53                to spl-tipo-colonna.
           move riga-titolo       to spl-riga-stampa.
           move Verdana16BI       to spl-hfont
           set spl-rosso          to true.
           perform SCRIVI.
           
           initialize riga-titolo.
           string "dal "                     delimited size
                  link-data-from(7:2)        delimited size
                  "/"                        delimited size
                  link-data-from(5:2)        delimited size
                  "/"                        delimited size
                  link-data-from(1:4)        delimited size
                  " al "                     delimited size
                  link-data-to(7:2)          delimited size
                  "/"                        delimited size
                  link-data-to(5:2)          delimited size
                  "/"                        delimited size
                  link-data-to(1:4)          delimited size
                  into riga-titolo
           end-string.

           add 0,6                to save-riga.
           move 53                to spl-tipo-colonna.
           move riga-titolo       to spl-riga-stampa.
           move Verdana16BI       to spl-hfont
           set spl-rosso          to true.
           perform SCRIVI.

           add 0,8                to save-riga.

           move riga-intestazione to spl-riga-stampa.
           set  spl-blu           to true.
           move 54                to spl-tipo-colonna.
           move Verdana12BI       to spl-hfont.
           perform SCRIVI.

           add 0,3                to save-riga.
           
           move 10                 to spl-pen-with.
           perform STAMPA-LINEA.
      *****
      *****     move 1,4               to save-riga.
      *****     move 21                to spl-tipo-colonna.
      *****     move riga-intestazione to line-riga.
      *****     perform SCRIVI.
      *****
      *****     perform STAMPA-LINEA.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.
            
      ***---
       STAMPA-LINEA.
           move 3,2                to spl-colonna.
           move 17,2               to spl-colonna-fine.
           add  passo              to save-riga.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

            
      ***---
       SALTO-PAGINA.
           perform PIE-DI-PAGINA.
           add 1 to pagina.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           perform SCRIVI-INTESTAZIONE.
       
      ***---
       STAMPA-FRAME.
           move 2     to spl-pen-with.
           move 11,80 to spl-colonna.
           move 17,22 to spl-colonna-fine.

           add  0,27  to save-riga giving spl-riga.
           add  0,92  to save-riga giving spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-rosso         to true.
           call "spooler"         using spooler-link.

           move 2     to spl-pen-with.
           move 11,83 to spl-colonna.
           move 17,2  to spl-colonna-fine.

           add  0,3   to save-riga giving spl-riga.
           add  0,9   to save-riga giving spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-giallo        to true.
           call "spooler"         using spooler-link.
          
      ***---
       CARICA-FONT.
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
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 16BI
           initialize wfont-data Verdana16BI.
           move 16 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana16BI, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
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
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect WFONT-NAME replacing trailing SPACE by LOW-VALUE.
           move WFONT-SIZE    to FONT-SIZE-DPLY.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited LOW-VALUE
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio

           inspect messaggio replacing trailing SPACE by LOW-VALUE.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close tordini rordini clienti tgrupgdo 
                 tmp-ord-da-fat tcaumag ttipocli
           delete file tmp-ord-da-fat.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana16BI.
           destroy Verdana12BI.
           destroy Verdana8B.
           destroy font-pie-pagina.

           cancel "spooler".
           initialize spooler-link.

           display "                                           "
                   upon link-handle at column 18,00 line 07,00.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "pie-di-pagina.cpy".
