       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-fatcon-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl". 
           copy "rordini.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".
           copy "listini.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "articoli.sl".
      *****     copy "blister.sl".
           copy "locali.sl".
           copy "tmp-contest.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd". 
           copy "rordini.fd".
           copy "tpromo.fd".
           copy "rpromo.fd".
           copy "listini.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "articoli.fd".
      *****     copy "blister.fd".
           copy "locali.fd".
           copy "tmp-contest.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "pie-di-pagina.def".

       77  status-tordini        pic xx. 
       77  status-rordini        pic xx. 
       77  status-tpromo         pic xx. 
       77  status-rpromo         pic xx. 
       77  status-listini        pic xx. 
       77  status-clienti        pic xx. 
       77  status-destini        pic xx. 
       77  status-articoli       pic xx.
      ***** 77  status-blister        pic xx.
       77  status-locali         pic xx.
       77  status-tmp-contest    pic xx.
       77  path-tmp-contest      pic x(256).

      * COSTANTI
       78  titolo                value "Fattura Contestazioni".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 7.
       78  IdxPrima              value 1.
       78  IdxAttuale            value 2.
       78  IdxDopo               value 3.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(70).

       01  r-intesta.
           05 filler             pic x(8)  value "Articolo".
           05 filler             pic x(11) value "Descrizione".
           05 filler             pic x(4)  value "Q.tà".
           05 filler             pic x(6)  value "Prezzo".

       01  comodo10              pic x(10).

       01  r-riga.
           05 r-articolo         pic z(6).
           05 r-descr            pic x(30).
           05 r-qta              pic zz.zz9 blank zero.
           05 r-prz              pic x(10).

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
      ***** 77  filler                pic 9.
      *****     88 ok-fattura         value 1 false 0.
       77  filler                pic 9.
           88 da-listino         value 1 false 0.

      * VARIABILI
       01  occurs-promo.
           05 el-riga            occurs 3.
              10 el-volantino    pic 9(15).
              10 el-prz          pic x(10).
              10 el-descr        pic x(20).
              10 el-ini-dpo      pic x(8).
              10 el-fine-dpo     pic x(8).
              10 el-ini-vol      pic x(8).
              10 el-fine-vol     pic x(8).

       77  SaveArticolo          pic 9(5).
       77  data-oggi             pic 9(8).
       77  num-righe             pic 9(3).
       77  idx                   pic 9.

       77  prz-z                 pic z.zz9,99.
       77  prz-x                 pic x(8).   

       77  r-prz-z               pic z.zz9,99.
       77  r-prz-x               pic x(8).

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana20BI           handle of font.
       77  Verdana14BI           handle of font.
       77  Verdana10B            handle of font.
       77  Verdana8BI            handle of font.
       77  Verdana8B             handle of font.
       77  Verdana8              handle of font.
       77  Verdana6              handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       copy "link-contest.def".

      ******************************************************************
       PROCEDURE DIVISION USING contest-linkage.

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
       TPROMO-ERR SECTION.
           use after error procedure on tpromo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tpromo
           when "39"
                set errori to true
                display message "File [TPROMO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TPROMO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RPROMO-ERR SECTION.
           use after error procedure on rpromo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rpromo
           when "39"
                set errori to true
                display message "File [RPROMO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RPROMO] inesistente"
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

      *********---
      ****** BLISTER-ERR SECTION.
      ******     use after error procedure on blister.
      ******     set tutto-ok  to true.
      ******     evaluate status-blister
      ******     when "39"
      ******          set errori to true
      ******          display message "File [BLISTER] mismatch size!"
      ******                    title titolo
      ******                     icon 3
      ******     when "98"
      ******          set errori to true
      ******          display message "[BLISTER] Indexed file corrupt!"
      ******                    title titolo
      ******                     icon 3
      ******     when "35"
      ******          display message box        "Impossibile procedere."
      ******            x"0d0a""File [BLISTER] inesistente"
      ******                  title = titolo
      ******                  icon 2
      ******          set errori to true
      ******     when "93"
      ******     when "99" set RecLocked to true
      ******     end-evaluate.

      ***---
       LISTINI-ERR SECTION.
           use after error procedure on listini.
           set tutto-ok  to true.
           evaluate status-listini
           when "39"
                set errori to true
                display message "File [LISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [LISTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
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

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "39"
                set errori to true
                display message "File [DESTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DESTINI] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [DESTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       LOCALI-ERR SECTION.
           use after error procedure on locali.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-locali
           when "39"
                set errori to true
                display message "File [LOCALI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LOCALI] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [LOCALI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMP-CONTEST-ERR SECTION.
           use after error procedure on tmp-contest.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-contest
           when "39"
                set errori to true
                display message "File [TMP-CONTEST] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-CONTEST] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "Impossibile procedere."
                   x"0d0a""File [TMP-CONTEST] inesistente"
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
                     open output tmp-contest
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
           move 0   to num-righe.
           set environment "PRINTER" to "-P SPOOLER".
           move 0        to counter counter2.
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.
           initialize path-tmp-contest.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-tmp-contest from environment "PATH_ST".
           inspect path-tmp-contest replacing trailing 
                                    spaces by low-value.
           string  path-tmp-contest delimited low-value
                   "TMP-CONTEST"    delimited size
                   "_"              delimited size
                   como-data        delimited size
                   "_"              delimited size
                   como-ora         delimited size
                   ".tmp"           delimited size
                   into path-tmp-contest
           end-string.

      ***---
       OPEN-FILES.
           open output tmp-contest.
           if tutto-ok
              close tmp-contest
              open i-o tmp-contest allowing readers
              open input tordini tpromo listini articoli |blister
                         rordini rpromo destini clienti locali
           end-if.

      ***---               
       ELABORAZIONE.
           move link-anno   to tor-anno-fattura.
           move link-numero to tor-num-fattura.
           read tordini no lock key k-fattura invalid continue end-read.
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

                    if not ror-si-blister

                       initialize art-rec tpr-rec rpr-rec tmc-rec
                                  lst-rec cli-rec des-rec
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
              
                       move ror-cod-articolo to art-codice 
                            tmc-art-codice
                       read tmp-contest
                            invalid
                            read articoli no lock 
                                 invalid continue 
                            end-read
                            move art-descrizione to tmc-art-descrizione
                            add 1 to num-righe
                       end-read
                       if tmc-prezzo < ror-prz-unitario
                          move ror-prz-unitario to tmc-prezzo
                       end-if
                       add ror-qta to tmc-qta
      *****                 if ror-si-blister set tmc-blister to true
      *****                 else              set tmc-blister to false
      *****                 end-if
                       set tmc-blister to false
                       write tmc-rec invalid rewrite tmc-rec end-write
                       set trovato to true
                    end-if

                 end-perform
           end-start.

           if trovato
              perform STAMPA
           else
              display message "Nessun articolo trovato"
                        title titolo
                         icon 2
           end-if.

      ***---
       STAMPA.
           initialize spooler-link.
           call   "selprint" using selprint-linkage
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

              move 1 to pagina
              evaluate true
              when num-righe <= 78-MaxRows
                   move 1 to tot-pag
              when other
                   divide num-righe by 78-MaxRows
                   giving tot-pag   remainder resto
                   if resto > 0 add 1 to tot-pag end-if
              end-evaluate

              move low-value to tmc-rec
              start tmp-contest key >= k-ord
                    invalid continue
              end-start
              set  cli-tipo-C  to true
              move tor-cod-cli to cli-codice
              read clienti no lock invalid continue end-read
              if tor-prg-destino not = 0
                 move cli-localita to des-localita
                 move tor-cod-cli     to des-codice
                 move tor-prg-destino to des-prog
                 read destini no lock invalid continue end-read
              end-if
              perform SCRIVI-INTESTAZIONE
              move 0,5 to passo
      *****        perform CERCA-PROMO
              move 0 to num-righe
              perform until 1 = 2
                 read tmp-contest next at end exit perform end-read
                 if num-righe >= 78-MaxRows
                    perform SALTO-PAGINA
                    move 0,5 to passo
                 end-if

                 perform CERCA-PROMO

                 if not tmc-blister
                    move IdxPrima to idx
                    perform VALORIZZA-PREZZI
                 end-if

                 move 0             to spl-tipo-colonna
                 move Verdana8BI    to spl-hfont
                 set spl-viola      to true
                 move "Promo Prima" to spl-riga-stampa
                 move 9,95          to spl-colonna
                 perform SCRIVI

                 perform SCRIVI-DATE

                 perform SCRIVI-DESCR    

                 perform STAMPA-LINEA-PERIODO

                 move tmc-art-codice      to r-articolo
                 move tmc-art-descrizione to r-descr
                 move tmc-qta             to r-qta
                 perform FORMAT-PRZ
                 move r-riga              to spl-riga-stampa
                 move Verdana8            to spl-hfont
                 set  spl-nero            to true
                 move 87                  to spl-tipo-colonna
                 perform SCRIVI

                 move cli-gdo         to lst-gdo
                 move tmc-art-codice  to lst-articolo
                 move tor-data-ordine to lst-data
                 start listini key <= lst-k-articolo
                       invalid continue
                   not invalid
                       read listini next end-read
                       if lst-gdo      = cli-gdo         and
                          lst-articolo = tmc-art-codice  and
                          lst-data    <= tor-data-ordine
                          move spaces     to r-articolo
                          move lst-prezzo to tmc-prezzo
                          initialize r-descr
                          string "Listino in vigore dal "
                                 lst-data(7:2) delimited size
                                 "/"           delimited size
                                 lst-data(5:2) delimited size
                                 "/"           delimited size
                                 lst-data(3:2) delimited size
                                 into r-descr
                          end-string
                          move 0               to r-qta
                          perform FORMAT-PRZ
                          set spl-blu to true
                          move r-riga to spl-riga-stampa
                          perform SCRIVI
                          subtract passo from save-riga
                       end-if
                 end-start

                 subtract passo from save-riga

                 if not tmc-blister
                    move IdxAttuale to idx
                    perform VALORIZZA-PREZZI
                 end-if

                 move 0             to spl-tipo-colonna
                 move Verdana8BI    to spl-hfont
                 set spl-arancione  to true
                 move "In Promo"    to spl-riga-stampa
                 move 9,95          to spl-colonna
                 perform SCRIVI

      *****           if ok-fattura
      *****              add 0,15 to save-riga
      *****              set spl-nero to true
      *****              move Verdana10B to spl-hfont
      *****              perform SCRIVI-DESCR
      *****              add 0,10 to save-riga
      *****              perform STAMPA-LINEA-PERIODO
      *****              set ok-fattura to false
      *****           else
                 if not da-listino
                    perform SCRIVI-DATE
                    perform SCRIVI-DESCR
                 else
                    move Verdana6 to spl-hfont
                    set spl-nero  to true
                    add 0,25 to save-riga
                    perform SCRIVI-DESCR
                 end-if

                 perform STAMPA-LINEA-PERIODO
      *****           end-if

                 if not tmc-blister
                    move IdxDopo to idx
                    perform VALORIZZA-PREZZI
                 end-if

                 move 0             to spl-tipo-colonna
                 move Verdana8BI    to spl-hfont
                 set spl-marrone    to true
                 move "Promo Dopo"  to spl-riga-stampa
                 move 9,95          to spl-colonna
                 perform SCRIVI

                 perform SCRIVI-DATE

                 perform SCRIVI-DESCR

                 perform STAMPA-LINEA-SEPARAZIONE
                 add 1 to num-righe

              end-perform

              perform PIE-DI-PAGINA
           end-if.

      ***---
       SCRIVI-DATE.
           subtract 0,65 from save-riga.
           move Verdana6 to spl-hfont.
           set spl-blu to true.
           if el-volantino(idx) = 0 or not trovato or tmc-blister
              move spaces to spl-riga-stampa
           else
              move "Dpo"  to spl-riga-stampa
           end-if.
           move 15,9   to spl-colonna.
           perform SCRIVI.

           subtract passo from save-riga.
           set spl-nero to true.
           initialize spl-riga-stampa.
           if el-volantino(idx) not = 0 and trovato and not tmc-blister
              string el-ini-dpo(idx)  delimited size
                     " - "            delimited size
                     el-fine-dpo(idx) delimited size
                     into spl-riga-stampa
              end-string
           end-if.
           move 16,5 to spl-colonna.
           perform SCRIVI.

           subtract 0,1 from save-riga.
           set spl-blu to true.
           if el-volantino(idx) = 0 or not trovato or tmc-blister
              move spaces to spl-riga-stampa
           else
              move "Vol"  to spl-riga-stampa
           end-if.
           move 15,9   to spl-colonna.
           perform SCRIVI,
                               
           set spl-nero to true.
           subtract passo from save-riga.
           initialize spl-riga-stampa.
           if el-volantino(idx) not = 0 and trovato and not tmc-blister
              string el-ini-vol(idx)  delimited size
                     " - "            delimited size
                     el-fine-vol(idx) delimited size
                     into spl-riga-stampa
              end-string
           end-if.
           move 16,5 to spl-colonna.
           perform SCRIVI.

      ***---
       SCRIVI-DESCR.   
           subtract 0,7 from save-riga.
           move 12,2 to spl-colonna.
           if tmc-blister
              set spl-nero to true
              move "BLISTER" to spl-riga-stampa
              perform SCRIVI
           else
      *****        if ok-fattura
      *****           perform FRAME-OK-FATTURA
      *****           set spl-nero to true
      *****           move "OK FATTURA!!" to spl-riga-stampa
      *****           perform SCRIVI
      *****        else
              initialize spl-riga-stampa
              if trovato
                 string el-prz(idx)     delimited size
                        " "             delimited size
                        el-descr(idx)   delimited size
                        into spl-riga-stampa
                 end-string
              end-if
              perform SCRIVI
      *****        end-if
           end-if.

           if idx not = IdxAttuale
              |RIGHE BARRATE
              if el-volantino(idx) not = 0 and trovato or tmc-blister
                 move 6                  to spl-pen-with
                 move 12,2               to spl-colonna
                 move 15,8               to spl-colonna-fine
                 subtract 0,2 from save-riga giving spl-riga
                 add 0,45 to save-riga giving spl-riga-fine
                 set  spl-oggetto        to true
                 set  spl-linea          to true
                 set  spl-pen-solid      to true
                 set  spl-blu            to true
                 call "spooler"       using spooler-link
       
                 move 6                  to spl-pen-with
                 move 12,2               to spl-colonna
                 move 15,8               to spl-colonna-fine
                 subtract 0,2 from save-riga giving spl-riga-fine
                 add 0,45 to save-riga giving spl-riga
                 set  spl-oggetto        to true
                 set  spl-linea          to true
                 set  spl-pen-solid      to true
                 set  spl-blu            to true
                 call "spooler"       using spooler-link
              end-if
           end-if.
               
           add 0,4 to save-riga.

      ***---
       CERCA-PROMO.
           initialize occurs-promo.

           |PROMO ATTUALE
           set trovato to false.

           |Provo con la promo locale
           if tor-prg-destino not = 0
              move high-value       to loc-rec
              move cli-gdo          to loc-gdo
              move cli-codice       to loc-cliente
              move tor-prg-destino  to loc-destino
              move tor-data-ordine  to loc-ini-dpo
              start locali key <= loc-chiave-ricerca
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read locali previous at end exit perform end-read
                       if loc-gdo     not = cli-gdo    or
                          loc-cliente not = cli-codice or
                          loc-destino not = tor-prg-destino
                          exit perform
                       end-if
                       if loc-ini-dpo  <= tor-data-ordine  and
                          loc-fine-dpo >= tor-data-ordine 
                          move loc-codice to tpr-codice
                          read tpromo no lock
                               invalid continue
                           not invalid
                               if tpr-gdo      = loc-gdo     and
                                  tpr-ini-dpo  = loc-ini-dpo and
                                  tpr-fine-dpo = loc-fine-dpo
                                  move tpr-codice     to rpr-codice
                                  move tmc-art-codice to rpr-articolo
                                  read rpromo no lock
                                       invalid continue
                                   not invalid
                                       move IdxAttuale to idx
                                       perform VALORIZZA-EL
                                       set trovato to true
                                       exit perform
                                  end-read
                               end-if
                          end-read
                          exit perform
                       end-if
                    end-perform
              end-start  
           end-if.

           if not trovato
              move high-value       to tpr-rec
              move cli-gdo          to tpr-gdo
              move tor-data-ordine  to tpr-ini-dpo
              start tpromo key <= tpr-chiave-ricerca
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tpromo previous at end exit perform end-read
                       if tpr-gdo not = cli-gdo
                          exit perform
                       end-if
                       if tpr-ini-dpo > tor-data-ordine 
                          exit perform 
                       end-if
                       if tpr-ini-dpo  <= tor-data-ordine  and
                          tpr-fine-dpo >= tor-data-ordine 
                          move tpr-codice     to rpr-codice
                          move tmc-art-codice to rpr-articolo
                          read rpromo no lock
                               invalid continue
                           not invalid
                               move IdxAttuale to idx
                               perform VALORIZZA-EL
                               set trovato to true
                               exit perform
                          end-read      
                       end-if
                    end-perform
              end-start
           end-if.

           |PROMO PRIMA
           |Cerco promo GDO
           set trovato to false.

           |Provo con la promo locale
           if tor-prg-destino not = 0
              move high-value       to loc-rec
              move cli-gdo          to loc-gdo
              move cli-codice       to loc-cliente
              move tor-prg-destino  to loc-destino
              move tor-data-ordine  to loc-ini-dpo
              start locali key <= loc-chiave-ricerca
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read locali previous at end exit perform end-read
                       if loc-gdo     not = cli-gdo    or
                          loc-cliente not = cli-codice or
                          loc-destino not = tor-prg-destino
                          exit perform
                       end-if
                       if loc-ini-dpo  < tor-data-ordine  and
                          loc-fine-dpo < tor-data-ordine 
                          move loc-codice to tpr-codice
                          read tpromo no lock
                               invalid continue
                           not invalid
                               if tpr-gdo      = loc-gdo     and
                                  tpr-ini-dpo  = loc-ini-dpo and
                                  tpr-fine-dpo = loc-fine-dpo
                                  move tpr-codice     to rpr-codice
                                  move tmc-art-codice to rpr-articolo
                                  read rpromo no lock
                                       invalid continue
                                   not invalid
                                       move IdxPrima to idx
                                       perform VALORIZZA-EL
                                       set trovato to true
                                       exit perform
                                  end-read
                               end-if
                          end-read
                          exit perform
                       end-if
                    end-perform
              end-start  
           end-if.

           if not trovato
              move high-value       to tpr-rec
              move cli-gdo          to tpr-gdo
              move tor-data-ordine  to tpr-ini-dpo
              start tpromo key <= tpr-chiave-ricerca
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tpromo previous at end exit perform end-read
                       if tpr-gdo not = cli-gdo
                          exit perform
                       end-if
                       if tpr-ini-dpo  < tor-data-ordine  and
                          tpr-fine-dpo < tor-data-ordine 
                          move tpr-codice     to rpr-codice
                          move tmc-art-codice to rpr-articolo
                          read rpromo no lock
                               invalid continue
                           not invalid
                               move IdxPrima to idx
                               perform VALORIZZA-EL
                               set trovato to true
                               exit perform
                          end-read
                       end-if
                    end-perform
              end-start
           end-if.

           |PROMO DOPO
           set trovato to false.

           |Provo con la promo locale
           if tor-prg-destino not = 0
              move high-value       to loc-rec
              move cli-gdo          to loc-gdo
              move cli-codice       to loc-cliente
              move tor-prg-destino  to loc-destino
              move tor-data-ordine  to loc-ini-dpo
              start locali key >= loc-chiave-ricerca
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read locali next at end exit perform end-read
                       if loc-gdo     not = cli-gdo    or
                          loc-cliente not = cli-codice or
                          loc-destino not = tor-prg-destino
                          exit perform
                       end-if
                       if loc-ini-dpo  > tor-data-ordine  and
                          loc-fine-dpo > tor-data-ordine 
                          move loc-codice to tpr-codice
                          read tpromo no lock
                               invalid continue
                           not invalid
                               if tpr-gdo      = loc-gdo     and
                                  tpr-ini-dpo  = loc-ini-dpo and
                                  tpr-fine-dpo = loc-fine-dpo
                                  move tpr-codice     to rpr-codice
                                  move tmc-art-codice to rpr-articolo
                                  read rpromo no lock
                                       invalid continue
                                   not invalid
                                       move IdxDopo to idx
                                       perform VALORIZZA-EL
                                       set trovato to true
                                       exit perform
                                  end-read
                               end-if
                          end-read
                          exit perform
                       end-if
                    end-perform
              end-start  
           end-if.

           if not trovato
              move high-value       to tpr-rec
              move cli-gdo          to tpr-gdo
              move tor-data-ordine  to tpr-ini-dpo
              start tpromo key >= tpr-chiave-ricerca
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tpromo next at end exit perform end-read
                       if tpr-gdo not = cli-gdo
                          exit perform
                       end-if
                       if tpr-ini-dpo  > tor-data-ordine  and
                          tpr-fine-dpo > tor-data-ordine 
                          move tpr-codice     to rpr-codice
                          move tmc-art-codice to rpr-articolo
                          read rpromo no lock
                               invalid continue
                           not invalid
                               move IdxDopo to idx
                               perform VALORIZZA-EL
                               set trovato to true
                               exit perform
                          end-read
                       end-if
                    end-perform
              end-start
           end-if.

      ***---
       VALORIZZA-EL.
           move tpr-codice      to el-volantino(idx).
           move tpr-descrizione to el-descr(idx).
           string tpr-ini-dpo(7:2) delimited size
                  "/"              delimited size
                  tpr-ini-dpo(5:2) delimited size
                  "/"              delimited size
                  tpr-ini-dpo(3:2) delimited size
                  into el-ini-dpo(idx)
           end-string.
           string tpr-fine-dpo(7:2) delimited size
                  "/"               delimited size
                  tpr-fine-dpo(5:2) delimited size
                  "/"               delimited size
                  tpr-fine-dpo(3:2) delimited size
                  into el-fine-dpo(idx)
           end-string.
           string tpr-ini-volantino(7:2) delimited size
                  "/"                    delimited size
                  tpr-ini-volantino(5:2) delimited size
                  "/"                    delimited size
                  tpr-ini-volantino(3:2) delimited size
                  into el-ini-vol(idx)
           end-string.
           string tpr-fine-volantino(7:2) delimited size
                  "/"                     delimited size
                  tpr-fine-volantino(5:2) delimited size
                  "/"                     delimited size
                  tpr-fine-volantino(3:2) delimited size
                  into el-fine-vol(idx)
           end-string.

      ***---
       VALORIZZA-PREZZI.
           set trovato    to false.
      *****     set ok-fattura to false.
           set da-listino to false.
           move el-volantino(idx) to rpr-codice.
           move tmc-art-codice    to rpr-articolo.
           read rpromo no lock 
                invalid

      *****          if idx = IdxAttuale
      *****             |Allora guardo il listino
      *****             if not trovato
      *****                move cli-gdo          to lst-gdo
      *****                move tor-data-ordine  to lst-data
      *****                move tmc-art-codice   to lst-articolo
      *****                start listini key <= lst-k-articolo
      *****                      invalid continue
      *****                  not invalid
      *****                      read listini previous
      *****                      if lst-gdo      = cli-gdo          and
      *****                         lst-data    <= tor-data-ordine  and
      *****                         lst-articolo = tmc-art-codice
      *****                         set trovato to true
      *****                         if lst-prezzo = tmc-prezzo
      *****                            set ok-fattura to true
      *****                         else
      *****                            set da-listino to true
      *****                            string "LISTINO STD " delimited size
      *****                                   lst-data(7:2)  delimited size
      *****                                   "/"            delimited size
      *****                                   lst-data(5:2)  delimited size
      *****                                   "/"            delimited size
      *****                                   lst-data(3:2)  delimited size
      *****                                   into el-descr(idx)
      *****                            end-string
      *****                            move lst-prezzo to rpr-prz-acq
      *****                            perform FORMAT-PREZZO
      *****                         end-if
      *****                      end-if
      *****                end-start
      *****             end-if
      *****          end-if

            not invalid
                set trovato to true
                if idx = IdxAttuale
      *****             if rpr-prz-acq = tmc-prezzo
      *****                set ok-fattura to true
      *****             else
                   perform FORMAT-PREZZO
      *****             end-if
                else
                   perform FORMAT-PREZZO
                end-if
           end-read.

      ********---
      ***** FRAME-OK-FATTURA.
      *****     move 4     to spl-pen-with.
      *****     move 12,2  to spl-colonna.
      *****     move 15,2  to spl-colonna-fine.
      *****
      *****     add 0,45 to save-riga giving spl-riga.
      *****     add 1,0  to save-riga giving spl-riga-fine.
      *****
      *****     set  spl-oggetto       to true.
      *****     set  spl-rettangolo    to true.
      *****     set  spl-giallo        to true.
      *****     set  spl-brush-solid   to true.
      *****     call "spooler"         using spooler-link.

      ***---
       FORMAT-PRZ.
           move tmc-prezzo  to r-prz-z.
           move r-prz-z     to r-prz-x.
           call "C$JUSTIFY" using r-prz-x, "R"
           inspect r-prz-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using r-prz-x, "L".
           inspect r-prz-x replacing trailing spaces by low-value.
           initialize r-prz
           string "€ "    delimited size
                  r-prz-x delimited low-value
                  into r-prz
           end-string.
            
      ***--- 
       FORMAT-PREZZO.
           move rpr-prz-acq to prz-z.
           move prz-z       to prz-x.
           call "C$JUSTIFY" using prz-x, "R"
           inspect prz-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using prz-x, "L".
           inspect prz-x replacing trailing spaces by low-value.
           initialize el-prz(idx)
           string "€ "   delimited size
                  prz-x  delimited low-value
                  into el-prz(idx)
           end-string.

      ***---
       SCRIVI-INTESTAZIONE.
           move 0        to num-righe.
           move 0,8      to save-riga.
           move "** FATTURA CONTESTAZIONI **" to spl-riga-stampa.
           move Verdana20BI      to spl-hfont.
           move 58               to spl-tipo-colonna.
           set  spl-blu          to true.
           perform SCRIVI.

           move 2,0 to save-riga.
           perform STAMPA-LINEA.

           perform STAMPA-FRAME.

           move 0 to spl-tipo-colonna.
           move 1,05  to spl-colonna.
           move 2,4   to save-riga.
           set spl-rosso to true.
           move Verdana14BI to spl-hfont.
           move "Fattura Nr." to spl-riga-stampa.
           perform SCRIVI.

           move 3,2                to save-riga.
           move 1,05               to spl-colonna.
           set spl-nero            to true.
           move Verdana14BI        to spl-hfont.
           move tor-num-fattura    to spl-riga-stampa.
           inspect spl-riga-stampa replacing leading x"30" by x"20".
           perform SCRIVI.

           move Verdana8BI to spl-hfont.
           set spl-rosso to true.
                                        
           move 2,2      to save-riga.
           move 4,95     to spl-colonna.
           move "Data F" to spl-riga-stampa.
           perform SCRIVI.       
                                        
           move 6,5               to spl-colonna.
           move "Ragione Sociale" to spl-riga-stampa.
           perform SCRIVI.
                                        
           move 12,7      to spl-colonna.
           move "Destino" to spl-riga-stampa.
           perform SCRIVI.
                                        
           move 17,6          to spl-colonna.
           move "Cliente"     to spl-riga-stampa.
           perform SCRIVI.

           set spl-nero to true.

           initialize spl-riga-stampa.
           string tor-data-fattura(7:2) delimited size
                  "/"                   delimited size
                  tor-data-fattura(5:2) delimited size
                  "/"                   delimited size
                  tor-data-fattura(3:2) delimited size
                  into spl-riga-stampa
           end-string.

           move 2,65 to save-riga.
           move 4,6 to spl-colonna.
           perform SCRIVI.

           move 6,5 to spl-colonna.
           move cli-ragsoc-1(1:30) to spl-riga-stampa.
           perform SCRIVI.

           move 12,7 to spl-colonna.
           move des-localita(1:23) to spl-riga-stampa.
           perform SCRIVI.

           move 17,6 to spl-colonna.
           move tor-cod-cli to spl-riga-stampa.
           inspect spl-riga-stampa replacing leading x"30" by x"20".
           call "C$JUSTIFY" using spl-riga-stampa, "L".
           perform SCRIVI.

           move Verdana8BI to spl-hfont.
           set spl-rosso to true.       
           move 3,2      to save-riga.

           move 4,7      to spl-colonna.
           move "Bolla"  to spl-riga-stampa.
           perform SCRIVI.       
                                        
           move 6,95          to spl-colonna.
           move "Data B"      to spl-riga-stampa.
           perform SCRIVI.

           move 8,9       to spl-colonna.
           move "Ordine"  to spl-riga-stampa.
           perform SCRIVI.
                                        
           move 12,15         to spl-colonna.
           move "Data O"      to spl-riga-stampa.
           perform SCRIVI.
                                        
           move 14,4          to spl-colonna.
           move "Evasione"    to spl-riga-stampa.
           perform SCRIVI.
                                        
           move 17,0          to spl-colonna.
           move "Destino"     to spl-riga-stampa.
           perform SCRIVI.

           move Verdana8B  to spl-hfont.
           set spl-nero    to true.       
           move 3,65       to save-riga.

           move 4,7            to spl-colonna.
           move tor-num-bolla  to comodo10.
           inspect comodo10 replacing leading x"30" by x"20".
           call "C$JUSTIFY" using comodo10, "L".
           move   comodo10  to spl-riga-stampa.
           perform SCRIVI.       
                                        
           move 6,7           to spl-colonna.
           initialize spl-riga-stampa.
           string tor-data-bolla(7:2) delimited size
                  "/"                   delimited size
                  tor-data-bolla(5:2) delimited size
                  "/"                   delimited size
                  tor-data-bolla(3:2) delimited size
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.

           move 8,9             to spl-colonna.
           move tor-num-ord-cli to spl-riga-stampa.
           perform SCRIVI.
                                        
           move 11,9          to spl-colonna.
           initialize spl-riga-stampa.
           string tor-data-ordine(7:2) delimited size
                  "/"                  delimited size
                  tor-data-ordine(5:2) delimited size
                  "/"                  delimited size
                  tor-data-ordine(3:2) delimited size
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.
                                        
           move 14,4          to spl-colonna.
           move tor-numero    to comodo10.
           inspect comodo10 replacing leading x"30" by x"20".
           call "C$JUSTIFY" using comodo10, "L".
           move   comodo10  to spl-riga-stampa.
           perform SCRIVI.
                                        
           move 17,0             to spl-colonna.
           move tor-prg-destino  to comodo10.
           inspect comodo10 replacing leading x"30" by x"20".
           call "C$JUSTIFY" using comodo10, "L".
           move    comodo10  to spl-riga-stampa.
           perform SCRIVI.

           move 4,2  to save-riga.
           perform STAMPA-LINEA.

           move 4,5        to save-riga.
           set  spl-rosso  to true.
           move r-intesta  to spl-riga-stampa.
           move Verdana8BI to spl-hfont.
           move 86         to spl-tipo-colonna.
           perform SCRIVI.

      ***---
       STAMPA-FRAME.
           move 4     to spl-pen-with.
           move 0,8   to spl-colonna.
           move 4,4   to spl-colonna-fine.

           move 2,2   to spl-riga.
           move 4,0   to spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-nero          to true.
           set  spl-brush-null    to true.
           call "spooler"         using spooler-link.
            
      ***---
       STAMPA-LINEA.
           move 11                 to spl-pen-with.
           move 0,8                to spl-colonna.
           move 18,7               to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-blu            to true.
           call "spooler"       using spooler-link.  
            
      ***---
       STAMPA-LINEA-PERIODO.
           add 0,2 to save-riga.
           move 6                  to spl-pen-with.
           move 12,2               to spl-colonna.
           move 18,7               to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-blu            to true.
           call "spooler"       using spooler-link.  
           subtract 0,2 from save-riga.
            
      ***---
       STAMPA-LINEA-SEPARAZIONE.
           add 0,2 to save-riga.
           move 8                  to spl-pen-with.
           move 1,8                to spl-colonna.
           move 18,7               to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-rosso          to true.
           call "spooler"       using spooler-link.  
           subtract 0,2 from save-riga.

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
           move 0 to num-righe.
           move 0 to spl-tipo-colonna.
           move 0 to passo.
           perform SCRIVI-INTESTAZIONE.
          
      ***---
       CARICA-FONT.
      * Verdana 6
           initialize wfont-data Verdana6.
           move 6 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana6, wfont-data
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
                        giving wfont-status.

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
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 14BI
           initialize wfont-data Verdana14BI.
           move 14 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana14BI, wfont-data
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
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
                        giving wfont-status.

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
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 8
           initialize wfont-data Verdana8.
           move 8 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana8, wfont-data
                        giving wfont-status.

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

           inspect wfont-name replacing trailing space by low-value.
           move    wfont-size to font-size-dply.

           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close tordini tpromo listini articoli locali |blister
                 rordini rpromo destini clienti tmp-contest.
           delete file tmp-contest.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana20BI.
           destroy Verdana14BI.
           destroy Verdana10B.
           destroy Verdana8BI.
           destroy Verdana8B.
           destroy Verdana8.
           destroy Verdana6.
           destroy font-pie-pagina.

           cancel "spooler".
           initialize spooler-link. 
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "pie-di-pagina.cpy".
