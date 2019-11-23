       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-ritiri-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "rordini.sl". 
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "articoli.sl".
           copy "tmp-riordino.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd". 
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "articoli.fd".
           copy "tmp-riordino.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "pie-di-pagina.def".

       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-articoli       pic xx.
       77  status-tmp-riordino   pic xx.
       77  path-tmp-riordino     pic x(256).

      * COSTANTI
       78  titolo                value "Riordino".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 48.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(40).

       01  r-intesta.
           05 filler             pic x(13) value "Cod. Articolo".
           05 filler             pic x(11) value "Descrizione".

       01  r-riga-int.
           05 r-articolo         pic x(6).
           05 r-descrizione      pic x(40).

       01  r-riga-int-fissa-1.
           05 filler             pic x(12) value "Data Fattura".
           05 filler             pic x(4)  value "Tipo".
           05 filler             pic x(6)  value "Prezzo".
                                           
       01  r-riga-int-fissa-2.
           05 filler             pic x(12) value "Num. Fattura".
           05 filler             pic x(8)  value "Quantità".

       01  r-num-qta.
           05 r-num              pic z(8).
           05 r-qta              pic zzz.zz9.

       01  r-data-tipo-prz.
           05 r-data             pic x(10).
           05 r-tipo             pic x.
           05 r-prz              pic x(12).

       01  riga-tot-qta.
           05 filler           pic x(22) value "TOTALE QUANTITA' SALDO".
           05 r-tot-qta        pic ---.--9.

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
       77  filler                pic  9.
           88 save-tmp-riord-articolo-zero value 1, false 0.

      * VARIABILI
       77  tot-qta               pic S9(8) value 0.
       77  valore-z              pic zzz.zz9,99.
       77  valore-x              pic x(10).

       77  SaveArticolo          pic 9(5).
       77  data-oggi             pic 9(8).
       77  num-righe             pic 9(3).

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana20BI           handle of font.
       77  Verdana10BI           handle of font.
       77  Verdana10B            handle of font.
       77  Verdana8B             handle of font.
       77  Verdana8BI            handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       copy "link-riordino.def".

      ******************************************************************
       PROCEDURE DIVISION using riordino-linkage.

       DECLARATIVES.
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
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
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
       TMP-RIORDINO-ERR SECTION.
           use after error procedure on tmp-riordino.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-riordino
           when "39"
                set errori to true
                display message "File [TMP-RIORDINO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-RIORDINO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "Impossibile procedere."
                   x"0d0a""File [TMP-RIORDINO] inesistente"
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
                     open output tmp-riordino
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
           accept data-oggi from century-date.
           move 0 to num-righe.
           set environment "PRINTER" to "-P SPOOLER".
           move 0        to counter counter2.
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.
           initialize path-tmp-riordino.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-tmp-riordino   from environment "PATH_ST".
           inspect path-tmp-riordino   replacing trailing 
                                       spaces by low-value.
           string  path-tmp-riordino   delimited low-value
                   "TMP-RIORDINO"      delimited size
                   "_"                 delimited size
                   como-data           delimited size
                   "_"                 delimited size
                   como-ora            delimited size
                   ".tmp"              delimited size
                   into path-tmp-riordino
           end-string.

      ***---
       OPEN-FILES.
           open output tmp-riordino.
           if tutto-ok
              close tmp-riordino
              open i-o tmp-riordino allowing readers
              open input tordini rordini articoli tnotacr rnotacr
           end-if.

      ***---
       ELABORAZIONE.
           move low-value     to tor-rec.
           move riord-cliente to tor-cod-cli.
           move riord-destino to tor-prg-destino.
           move "N"           to tor-agg-contab.
           move 1             to tor-data-fattura.

           start tordini key >= k-andamento-clides
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 200
                    move counter to counter-edit
                    display counter-edit
                       upon riord-handle at column 21 line 26,50
                    move 0 to counter2
                 end-if
                 read tordini next at end exit perform end-read

                 if tor-cod-cli      not = riord-cliente or
                    tor-prg-destino  not = riord-destino or
                    tor-data-fattura     = 0
                    exit perform
                 end-if

                 perform LOOP-RIGHE-RORDINI

           end-if.  

           move low-value     to tno-rec.
           move riord-cliente to tno-cod-cli.
           move riord-destino to tno-prg-destino.
           move "N"           to tno-agg-contab.
           move 1             to tno-data-fattura.

           start tnotacr key >= k-andamento-clides
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon riord-handle at column 21 line 26,50
                    move 0 to counter2
                 end-if
                 read tnotacr next at end exit perform end-read

                 if tno-cod-cli     not = riord-cliente or
                    tno-prg-destino not = riord-destino or
                    tno-data-fattura    = 0
                    exit perform
                 end-if

                 perform LOOP-RIGHE-RNOTACR

           end-if.

           |Riempio il tmp con gli articoli scelti
           |nella lista come da richiesta di Walter
030107     move 0 to riord-idx.
030107     perform until 1 = 2
030107        if riord-idx = 99
030107           exit perform
030107        end-if
030107        add 1 to riord-idx
030107        if riord-el-articolo(riord-idx) = 0
030107           exit perform
030107        end-if
030107        move riord-el-articolo(riord-idx) 
030107          to ror-cod-articolo tmp-riord-articolo
030107        read tmp-riordino key k-articolo
030107             invalid 
030107             move 0 to ror-qta
030107             move 0 to tor-data-creazione
030107             move 0 to tor-numero
030107             set tmp-riord-articolo-zero to true
030107             perform VALORIZZA-RIGA
030107        end-read
030107     end-perform.

           if not trovato
              display message "Nessun ordine trovato"
                        title titolo
                         icon 2
           else
              perform STAMPA
           end-if.

      ***---
       LOOP-RIGHE-RORDINI.
           move tor-chiave to ror-chiave.
           move low-value  to ror-num-riga.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini  next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if

                    perform SEARCH-ELEMENT

                    if record-ok
                       perform VALORIZZA-RIGA
                    end-if

                 end-perform
           end-start.

      ***---
       VALORIZZA-RIGA.
           set trovato to true.

           initialize art-rec tmp-riord-rec
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.
           move "F"                to tmp-riord-tipo.
           move ror-cod-articolo   to tmp-riord-articolo.
           move tor-data-fattura   to tmp-riord-data.
           move tor-num-fattura    to tmp-riord-numero.
           read tmp-riordino
                invalid
                move ror-cod-articolo to art-codice
                read articoli no lock 
                     invalid continue 
                end-read
                move art-descrizione to tmp-riord-art-desc
           end-read.

           |Prendo il prezzo + alto
           if tmp-riord-prz < ror-prz-unitario
              move ror-prz-unitario to tmp-riord-prz
           end-if.

           add  ror-qta   to tmp-riord-qta.
           move riord-idx to tmp-riord-idx

           write tmp-riord-rec
                 invalid rewrite tmp-riord-rec
           end-write.

      ***---
       LOOP-RIGHE-RNOTACR.
           move tno-chiave to rno-chiave.
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

                    move rno-cod-articolo to ror-cod-articolo
                    perform SEARCH-ELEMENT

                    if record-ok
                       set trovato to true

                       initialize art-rec tmp-riord-rec
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move "N"                to tmp-riord-tipo
                       move rno-cod-articolo   to tmp-riord-articolo
                       move tno-data-fattura   to tmp-riord-data
                       move tno-num-fattura    to tmp-riord-numero
                       read tmp-riordino
                            invalid
                            move rno-cod-articolo to art-codice
                            read articoli no lock 
                                 invalid continue 
                            end-read
                            move art-descrizione to tmp-riord-art-desc
                       end-read

                       |Prendo il prezzo + alto
                       if tmp-riord-prz < rno-prz-unitario
                          move rno-prz-unitario to tmp-riord-prz
                       end-if

                       add  rno-qta   to tmp-riord-qta
                       move riord-idx to tmp-riord-idx

                       write tmp-riord-rec
                             invalid rewrite tmp-riord-rec
                       end-write
                    end-if

                 end-perform
           end-start.

      ***---
       SEARCH-ELEMENT.
           set record-ok to false.
           set riord-idx to 1.
           search riord-articoli
           when riord-el-articolo(riord-idx) = ror-cod-articolo
                set record-ok to true
           end-search.

      ***---
       STAMPA.
           initialize spooler-link.
           call   "selprint" using selprint-linkage
           cancel "selprint".

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              move "GESLUX - Ritiri" to spl-nome-job
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
                   if resto > 1 add 1 to tot-pag end-if
              end-evaluate
              move 0 to SaveArticolo
              move low-value to tmp-riord-rec
              start tmp-riordino key >= k-ritiri
                    invalid continue
                not invalid
                    perform SCRIVI-INTESTAZIONE
                    perform until 1 = 2
                       read tmp-riordino next 
                            at end exit perform 
                       end-read
                       if SaveArticolo not = tmp-riord-articolo

                          if SaveArticolo not = 0
                             if not save-tmp-riord-articolo-zero
                                perform SCRIVI-TOT-ARTICOLO
                                add 0,1 to save-riga
                             end-if
                          end-if

                          perform SCRIVI-TITOLO-ARTICOLO

                          move tmp-riord-articolo to SaveArticolo

                       end-if
                       if tmp-riord-articolo-zero 
                          set save-tmp-riord-articolo-zero to true
                       else
                          set save-tmp-riord-articolo-zero to false
                       end-if
                       
                       if num-righe >= 78-MaxRows
                          perform SALTO-PAGINA
                       end-if

                       move 0,5 to passo

                       if not tmp-riord-articolo-zero
                          move Verdana8B        to spl-hfont

                          string tmp-riord-data(7:2) delimited size
                                 "/"                 delimited size
                                 tmp-riord-data(5:2) delimited size
                                 "/"                 delimited size
                                 tmp-riord-data(1:4) delimited size
                                 into r-data
                          end-string
                          move tmp-riord-tipo   to r-tipo
                          perform FORMAT-PREZZO

                          move r-data-tipo-prz  to spl-riga-stampa
                          set  spl-rosso        to true
                          move 66 to spl-tipo-colonna
                          perform SCRIVI
                          subtract passo from save-riga 

                          move tmp-riord-numero to r-num
                          move tmp-riord-qta    to r-qta

                          move r-num-qta        to spl-riga-stampa
                          set  spl-blu          to true
                          move 67 to spl-tipo-colonna
                          perform SCRIVI

                          if tmp-riord-tipo = "N"
                             subtract tmp-riord-qta from tot-qta
                          else
                             add      tmp-riord-qta to tot-qta
                          end-if

                          read tmp-riordino next 
                               at end continue
                           not at end
                               if tmp-riord-articolo = SaveArticolo
                                  perform STAMPA-LINEA-ARTICOLO
                               end-if
                          end-read

                          read tmp-riordino previous
                       
                          add 1 to num-righe
                       else
                          subtract 0,26 from save-riga
                       end-if

                 end-perform
                            
                 if not tmp-riord-articolo-zero
                    perform SCRIVI-TOT-ARTICOLO
                 end-if

                 perform PIE-DI-PAGINA

                 set spl-chiusura to true
                 call   "spooler" using spooler-link

           end-start.

      ***---
       SCRIVI-TOT-ARTICOLO.
           if num-righe > 78-MaxRows - 2
              perform SALTO-PAGINA
           end-if.

           move 0,5 to passo.
           perform STAMPA-LINEA-ARTICOLO-INT.

           add 0,45   to save-riga.

           move 4     to spl-pen-with.
           move 14,0  to spl-colonna.
           move 16,75 to spl-colonna-fine.

           add 0,03   to save-riga giving spl-riga.
           add 0,42   to spl-riga  giving spl-riga-fine.

           set  spl-oggetto     to true.
           set  spl-rettangolo  to true.
           set  spl-giallo      to true.
           set  spl-brush-solid to true.
           call "spooler"       using spooler-link.

           subtract 0,5 from save-riga.

           move Verdana10BI  to spl-hfont.
           move tot-qta      to r-tot-qta.
           move riga-tot-qta to spl-riga-stampa.
           set  spl-rosso    to true.
           move 68           to spl-tipo-colonna.
           perform SCRIVI.
           add 0,05 to save-riga.
           perform STAMPA-LINEA-ARTICOLO-INT.
           move 0 to tot-qta. 
           add  2 to num-righe.

      ***---
       FORMAT-PREZZO.
           move tmp-riord-prz  to valore-z.
           move valore-z       to valore-x.
           call "C$JUSTIFY" using valore-x, "R"
           inspect valore-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using valore-x, "L".
           inspect valore-x replacing trailing spaces by low-value.
           initialize r-prz.
           string "€ "      delimited size
                  valore-x  delimited low-value
                  into r-prz
           end-string.

      ***---
       SCRIVI-INTESTAZIONE.
           move 0        to num-righe.
           move 0,3      to passo.
           perform STAMPA-FRAMES-INIZIALI.

           move 0,4              to save-riga.
           move "** Ritiri x nota credito **" to r-titolo.
           move r-titolo         to spl-riga-stampa.
           move Verdana20BI      to spl-hfont.
           move 58               to spl-tipo-colonna.
           set spl-nero          to true.
           perform SCRIVI.

           move 1,75               to save-riga.
           move "RAGIONE SOCIALE"  to r-titolo.
           move r-titolo           to spl-riga-stampa.
           move Verdana10BI        to spl-hfont.
           move 59                 to spl-tipo-colonna.
           set spl-nero            to true.
           perform SCRIVI.

           move 1,75                to save-riga.
           move "DESTINAZIONE"     to r-titolo.
           move r-titolo           to spl-riga-stampa.
           move Verdana10BI        to spl-hfont.
           move 60                 to spl-tipo-colonna.
           set spl-nero            to true.
           perform SCRIVI.

           move 2,35               to save-riga.
           move riord-ragsoc       to r-titolo.
           move r-titolo           to spl-riga-stampa.
           move Verdana10BI        to spl-hfont.
           move 59                 to spl-tipo-colonna.
           set spl-rosso           to true.
           perform SCRIVI.

           move 2,35               to save-riga.
           move riord-localita     to r-titolo.
           move r-titolo           to spl-riga-stampa.
           move Verdana10BI        to spl-hfont.
           move 60                 to spl-tipo-colonna.
           set spl-rosso           to true.
           perform SCRIVI.

           move 2,95               to save-riga.
           move riord-cliente      to r-titolo.
           inspect r-titolo replacing leading x"30" by x"20".
           move r-titolo           to spl-riga-stampa.
           move Verdana10BI        to spl-hfont.
           move 59                 to spl-tipo-colonna.
           set spl-nero            to true.
           perform SCRIVI.

           move 2,95               to save-riga.
           move riord-destino      to r-titolo.
           inspect r-titolo replacing leading x"30" by x"20".
           move r-titolo           to spl-riga-stampa.
           move Verdana10BI        to spl-hfont.
           move 60                 to spl-tipo-colonna.
           set spl-nero            to true.
           perform SCRIVI.

           move 3,6                to save-riga.
           move r-intesta          to spl-riga-stampa.
           move Verdana8BI         to spl-hfont.
           move 61                 to spl-tipo-colonna.
           set spl-blu             to true.
           perform SCRIVI.

           perform STAMPA-LINEA.
           move 4,1                to save-riga.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.

      ***---
       SCRIVI-TITOLO-ARTICOLO.
           if save-tmp-riord-articolo-zero
              if num-righe > 78-MaxRows - 3
                 perform SALTO-PAGINA
              end-if
           else
              if num-righe > 78-MaxRows - 2
                 perform SALTO-PAGINA
              end-if
           end-if.

           add  0,1                  to save-riga.
           move 0,4                  to passo.
           move tmp-riord-articolo   to r-articolo.
           inspect r-articolo replacing leading x"30" by x"20".
           call "C$JUSTIFY"       using r-articolo, "L".
           move tmp-riord-art-desc   to r-descrizione.
           move r-riga-int           to spl-riga-stampa.
           move Verdana10B           to spl-hfont.
           move 62                   to spl-tipo-colonna.
           set spl-nero              to true.
           perform SCRIVI.

           add 0,15 to save-riga.

           if not tmp-riord-articolo-zero
                                        
              perform STAMPA-LINEA-ARTICOLO-INT

              move r-riga-int-fissa-1   to spl-riga-stampa
              move Verdana8B            to spl-hfont
              move 65                   to spl-tipo-colonna
              set spl-nero              to true
              perform SCRIVI

              subtract passo from save-riga
              move r-riga-int-fissa-2   to spl-riga-stampa
              move Verdana8B            to spl-hfont
              move 65,5                 to spl-tipo-colonna
              set spl-nero              to true
              perform SCRIVI

              add 0,2 to save-riga

              perform STAMPA-LINEA-ARTICOLO

              add 3 to num-righe
           else                         
              perform STAMPA-LINEA-ARTICOLO-INT
              add 0,4 to save-riga
              add 2   to num-righe
           end-if.
            
      ***---
       STAMPA-LINEA.
           move 4,3                to save-riga.
           move 11                 to spl-pen-with.
           move 1                  to spl-colonna.
           move 19                 to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-blu            to true.
           call "spooler"       using spooler-link.
            
      ***---
       STAMPA-LINEA-ARTICOLO-INT.
           add passo 0,05          to save-riga.
           move 11                 to spl-pen-with.
           move 8,3                to spl-colonna.
           move 19                 to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-arancione      to true.
           call "spooler"       using spooler-link.
           subtract 0,3 from save-riga.            
            
      ***---
       STAMPA-LINEA-ARTICOLO.
           subtract 0,08 from save-riga.
           add passo               to save-riga.
           move 4                  to spl-pen-with.
           move 8,3                to spl-colonna.
           move 19                 to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.
           subtract 0,45 from save-riga.

      ***---
       SALTO-PAGINA.
           perform PIE-DI-PAGINA.
           add 1 to pagina.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           perform SCRIVI-INTESTAZIONE.
       
      ***---
       STAMPA-FRAMES-INIZIALI.
           |"** RITIRI X NOTA CREDITO **"
           move 4     to spl-pen-with.
           move 4,2   to spl-colonna.
           move 16,1  to spl-colonna-fine.

           move 0,7   to spl-riga.
           move 1,6   to spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-rosso         to true.
           set  spl-brush-null    to true.
           call "spooler"         using spooler-link.

           |"RAGIONE SOCIALE"
           move 2     to spl-pen-with.
           move 1,0   to spl-colonna.
           move 9,8   to spl-colonna-fine.

           move 2,0   to spl-riga.
           move 2,5   to spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-nero          to true.
           set  spl-brush-solid   to true.
           call "spooler"         using spooler-link.

           move 2     to spl-pen-with.
           move 1,02  to spl-colonna.
           move 9,78  to spl-colonna-fine.

           move 2,02  to spl-riga.
           move 2,48  to spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-giallo        to true.
           set  spl-brush-solid   to true.
           call "spooler"         using spooler-link.

           |RAGIONE SOCIALE
           move 2     to spl-pen-with.
           move 1,0   to spl-colonna.
           move 9,8   to spl-colonna-fine.

           move 2,55  to spl-riga.
           move 3,1   to spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-nero          to true.
           set  spl-brush-null    to true.
           call "spooler"         using spooler-link.

           |"DESTINAZIONE"
           move 2     to spl-pen-with.
           move 10,2  to spl-colonna.
           move 19,0  to spl-colonna-fine.

           move 2,0   to spl-riga.
           move 2,5   to spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-nero          to true.
           set  spl-brush-solid   to true.
           call "spooler"         using spooler-link.

           move 2      to spl-pen-with.
           move 10,22  to spl-colonna.
           move 18,98  to spl-colonna-fine.

           move 2,02  to spl-riga.
           move 2,48  to spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-giallo        to true.
           set  spl-brush-solid   to true.
           call "spooler"         using spooler-link.

           |DESTINO
           move 2     to spl-pen-with.
           move 10,2  to spl-colonna.
           move 19,0  to spl-colonna-fine.

           move 2,55  to spl-riga.
           move 3,1   to spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-nero          to true.
           set  spl-brush-null    to true.
           call "spooler"         using spooler-link.

      ***---
       CONTA-RIGHE.
           |Il titolo finale
           move 2 to num-righe.
           move low-value to tmp-riord-rec.
           start tmp-riordino key >= tmp-riord-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-riordino next at end exit perform end-read
                    if SaveArticolo not = tmp-riord-articolo
                       if SaveArticolo = 0
                          if not tmp-riord-articolo-zero
                             |la riga d'intestazione articolo,
                             |la linea di separazione e l'intestazione
                             add 3 to num-righe
                          else
                             add 2 to num-righe
                          end-if
                       else
                          if not tmp-riord-articolo-zero
                             |Il totale, le due righe di separazione, il 
                             |titolo successivo e l'intestazione successiva
                             add 5 to num-righe
                          else
                             add 2 to num-righe
                          end-if
                       end-if
                       move tmp-riord-articolo to SaveArticolo
                    end-if

                 end-perform
           end-start.
          
      ***---
       CARICA-FONT.
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
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 10B
           initialize wfont-data Verdana10B.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10B, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

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
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 8BI
           initialize wfont-data Verdana8BI.
           move 8 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana8BI, wfont-data
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
           close tordini rordini articoli tmp-riordino tnotacr rnotacr.
           delete file tmp-riordino.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana20BI.
           destroy Verdana10BI.
           destroy Verdana10B.
           destroy Verdana8BI.
           destroy Verdana8B.
           destroy font-pie-pagina.

           cancel "spooler".
           initialize spooler-link. 

           display "                                           "
                   upon riord-handle at column 21 line 26,50.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "pie-di-pagina.cpy".
