       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-sellout-p.
       AUTHOR.                          Andrea.
       REMARKS. Vengono considerate le promo con data INIZIO DPO O 
                FINE DPO comprese tra la data di oggi e quella passat 
                in linkage digitata dall'utente.
                Vengono considerati inoltre gli articoli venduti in una 
                di quelle promo anche se esso non è più presente nel 
                volantino. Es. Se l'articolo 100 fu venduto in un una 
                promo che rispetti le date indicate, ma esso non è più
                presente (magari è stato forzato oppure cancellato) viene
                comunque riportato in stampa.

               ****************************************************
               * ATTENZIONE!!! ALLINEARE SEMPRE CON SOS-ORDINI!!! *
               ****************************************************

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mrordini.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".
           copy "articoli.sl".
           copy "blister.sl".
           copy "tmp-sellout.sl".
070509     copy "tmp-sellout-qta.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mrordini.fd". 
           copy "tpromo.fd".
           copy "rpromo.fd".
           copy "articoli.fd".
           copy "blister.fd".
           copy "tmp-sellout.fd".
070509     copy "tmp-sellout-qta.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "pie-di-pagina.def".

       77  status-mrordini                pic xx.
       77  status-tpromo                  pic xx.
       77  status-rpromo                  pic xx.
       77  status-articoli                pic xx.
       77  status-blister                 pic xx.
       77  status-tmp-sellout             pic xx.
070509 77  status-tmp-sellout-qta         pic xx.
       77  path-tmp-sellout               pic x(256).
070509 77  path-tmp-sellout-qta           pic x(256).

      * COSTANTI
       78  titolo                value "Sell Out Sicuro".
       78  78-PiePagina          value 1.
       78  78-MaxRows            value 47.

      * RIGHE PER LA STAMPA
       01  riga-titolo           pic x(48).

       01  r-intesta1.
         05 filler               pic x(8)  value "Articolo".
         05 filler               pic x(11) value "Descrizione".
         05 filler               pic x(3)  value "Imb".
         05 filler               pic x(10) value "Qtà Totali".
         05 filler               pic x(14) value "Qtà Bollettate".
         05 filler               pic x(13) value "Qtà Rimanenti".

       01  r-intesta2.
         05 filler               pic x(8).
         05 filler               pic x(11).
         05 filler               pic x(3).
         05 filler               pic x(10) value "Pezzi".
         05 filler               pic x(14) value "Pezzi".
         05 filler               pic x(13) value "Pezzi".

       01  r-riga.
         05 r-articolo           pic z(6).
         05 r-descr              pic x(50).
         05 r-imb                pic x(3).
         05 r-qta-t              pic zzz.zz9.
         05 r-qta-b              pic zzz.zz9.

       77  r-qta-r               pic ----.--9.

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 bisestile          value 1 false 0.
       77  filler                pic 9.
           88 CurrMonthOrNext    value 1 false 0.

      * VARIABILI
       77  idx                   pic 9(3).
       77  bli-idx               pic 9(3).
       77  tot-idx               pic 9(3).
       77  como-giorno           pic 9(2).
       77  como-anno             pic 9(4).
       77  inizio-mese           pic 9(8).
       77  fine-mese             pic 9(8).
       77  ini                   pic x(10).
       77  fine                  pic x(10).
       77  mese-esteso           pic x(9).
070509 77  como-qta              pic 9(8).

       01  tab-volantini.
        05 elemento              occurs 999 indexed by idx-promo.
           10 el-volantino       pic 9(15).

       77  num-righe             pic 9(3).

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Verdana20BI           handle of font.
       77  Verdana12BI           handle of font.
       77  Verdana8BI            handle of font.
       77  Verdana8B             handle of font.
       77  Verdana8              handle of font.
       77  Verdana6BI            handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  data-oggi             pic 9(8).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-mese             pic 99.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       LINKAGE SECTION.
       77  link-data             pic 9(8).
       77  link-handle           handle of window.

      ******************************************************************
       PROCEDURE DIVISION using link-data,
                                link-handle.

       DECLARATIVES.
      ***---
       MRORDINI SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "39"
                set errori to true
                display message "File [MRORDINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[MRORDINI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [MRORDINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TPROMO SECTION.
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
       RPROMO SECTION.
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
       BLISTER-ERR SECTION.
           use after error procedure on blister.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-blister
           when "39"
                set errori to true
                display message "File [BLISTER] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[BLISTER] Indexed file corrupt!"
                          title titolo
                           icon 3  
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [BLISTER] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMP-SELLOUT-ERR SECTION.
           use after error procedure on tmp-sellout.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmp-sellout
           when "39"
                set errori to true
                display message "File [TMP-SELLOUT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMP-SELLOUT] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "Impossibile procedere."
                   x"0d0a""File [TMP-SELLOUT] inesistente"
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
                     open output tmp-sellout
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
           move 0    to idx.
           move 0,45 to passo.
           move 0    to num-righe.
           set environment "PRINTER" to "-P SPOOLER".
           move 0        to counter counter2.
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.
           initialize path-tmp-sellout.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-tmp-sellout from environment "PATH_ST".
           inspect path-tmp-sellout replacing trailing 
                                       spaces by low-value.
           string  path-tmp-sellout delimited low-value
                   "TMP-SELLOUT"    delimited size
                   "_"              delimited size
                   como-data        delimited size
                   "_"              delimited size
                   como-ora         delimited size
                   ".tmp"           delimited size
                   into path-tmp-sellout
           end-string.

070509     initialize path-tmp-sellout-qta.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-tmp-sellout-qta from environment "PATH_ST".
           inspect path-tmp-sellout-qta replacing trailing 
                                       spaces by low-value.
           string  path-tmp-sellout-qta delimited low-value
                   "TMP-SELLOUT-QTA"    delimited size
                   "_"                  delimited size
                   como-data            delimited size
                   "_"                  delimited size
                   como-ora             delimited size
                   ".tmp"               delimited size
                   into path-tmp-sellout-qta
070509     end-string.

           |Il mese attuale o quello immediatamente successivo
           move link-data(5:2) to como-mese.
      *****     if link-mese = como-mese or
      *****        link-mese = como-mese + 1
      *****        set CurrMonthOrNext to true
      *****     else                          
      *****        set CurrMonthOrNext to false
      *****     end-if.

           initialize ini fine.
           accept como-data from century-date.
      *****
           string como-data(7:2) delimited size 
                  "/"            delimited size
                  como-data(5:2) delimited size 
                  "/"            delimited size
                  como-data(1:4) delimited size
                  into ini
           end-string.

      *****     move link-anno      to como-anno.
      *****     multiply como-anno  by 4 giving como-anno.
      *****     if como-anno = link-anno
      *****        set bisestile to true
      *****     else
      *****        set bisestile to false
      *****     end-if.

      *****     evaluate link-mese
      *****     when 01 move "GENNAIO"   to mese-esteso
      *****             move 31          to como-giorno
      *****     when 02 move "FEBBRAIO"  to mese-esteso
      *****             if bisestile            
      *****                move 29       to como-giorno
      *****             else                    
      *****                move 28       to como-giorno
      *****             end-if
      *****     when 03 move "MARZO"     to mese-esteso
      *****             move 31          to como-giorno
      *****     when 04 move "APRILE"    to mese-esteso
      *****             move 30          to como-giorno
      *****     when 05 move "MAGGIO"    to mese-esteso
      *****             move 31          to como-giorno
      *****     when 06 move "GIUGNO"    to mese-esteso
      *****             move 30          to como-giorno
      *****     when 07 move "LUGLIO"    to mese-esteso
      *****             move 31          to como-giorno
      *****     when 08 move "AGOSTO"    to mese-esteso
      *****             move 31          to como-giorno
      *****     when 09 move "SETTEMBRE" to mese-esteso
      *****             move 30          to como-giorno
      *****     when 10 move "OTTOBRE"   to mese-esteso
      *****             move 31          to como-giorno
      *****     when 11 move "NOVEMBRE"  to mese-esteso
      *****             move 30          to como-giorno
      *****     when 12 move "DICEMBRE"  to mese-esteso
      *****             move 31          to como-giorno
      *****     end-evaluate.
      *****
           string link-data(7:2) delimited size 
                  "/"            delimited size
                  link-data(5:2) delimited size 
                  "/"            delimited size
                  link-data(1:4) delimited size
                  into fine
           end-string.

      ***---
       OPEN-FILES.
           open output tmp-sellout tmp-sellout-qta.
           if tutto-ok
              close tmp-sellout tmp-sellout-qta
              open i-o tmp-sellout allowing readers
              open i-o tmp-sellout-qta allowing readers
              open input articoli mrordini rpromo tpromo blister
           end-if.

      ***---
       ELABORAZIONE.
      *****     string link-anno      delimited size
      *****            link-mese      delimited size
      *****            "01"           delimited size
      *****            into inizio-mese
      *****     end-string.
      *****
      *****     |Va bene comunque 31 tanto 20060301 è > di 20060229
      *****     string link-anno      delimited size
      *****            link-mese      delimited size
      *****            "31"           delimited size
      *****            into fine-mese
      *****     end-string.
           accept inizio-mese from century-date.
           move link-data to fine-mese.

           initialize tab-volantini.
           move low-value   to tpr-rec.
           move inizio-mese to tpr-ini-dpo.
           start tpromo key >= tpr-chiave-ini
                 invalid continue
             not invalid
                 perform until 1 = 2
                    set record-ok to false
                    read tpromo next at end     exit perform end-read
                    if tpr-ini-dpo  > fine-mese exit perform end-if

                    if tpr-ini-dpo >= inizio-mese and
                       tpr-ini-dpo <=   fine-mese
                       set record-ok to true
                    end-if

                    if tpr-fine-dpo >= inizio-mese and
                       tpr-fine-dpo <=   fine-mese
                       set record-ok to true
                    end-if

                    if record-ok
                       add 1 to idx
                       move tpr-codice to el-volantino(idx)
                    end-if

                 end-perform
           end-start.

           move low-value to tpr-rec.
           move inizio-mese to tpr-fine-dpo.
           start tpromo key >= tpr-chiave-fine
                 invalid continue
             not invalid
                 perform until 1 = 2
                    set record-ok to false
                    read tpromo next at end  exit perform end-read
                    if tpr-fine-dpo  > fine-mese exit perform end-if

                    if tpr-ini-dpo >= inizio-mese and
                       tpr-ini-dpo <=   fine-mese
                       set record-ok to true
                    end-if

                    if tpr-fine-dpo >= inizio-mese and
                       tpr-fine-dpo <=   fine-mese
                       set record-ok to true
                    end-if

                    if record-ok
                       set idx-promo to 1
                       search elemento
                       when el-volantino(idx-promo) = tpr-codice
                            move 0 to idx-promo
                       end-search
      
                       if idx-promo not = 0
                          add 1 to idx
                          move tpr-codice to el-volantino(idx)
                       end-if
                    end-if

                 end-perform
           end-start.

      *****     if CurrMonthOrNext
      *****        |DPO IN VIGORE NEL MESE RICHIESTO MA CON FINE DPO > OGGI
      *****        move low-value to tpr-rec
      *****        move como-data to tpr-fine-dpo
      *****        start tpromo key > tpr-chiave-fine
      *****              invalid continue
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read tpromo next at end exit perform end-read
      *****
      *****                 if tpr-ini-dpo  <= fine-mese and
      *****                    tpr-fine-dpo >= inizio-mese
      *****                    set idx-promo to 1
      *****                    search elemento
      *****                    when el-volantino(idx-promo) = tpr-codice
      *****                         move 0 to idx-promo
      *****                    end-search
      *****
      *****                    if idx-promo not = 0
      *****                       add 1 to idx
      *****                       move tpr-codice to el-volantino(idx)
      *****                    end-if
      *****                 end-if
      *****
      *****              end-perform
      *****        end-start
      *****     end-if.

           if idx = 0
              display message"Nessun volantino DPO nel mese selezionato"
                        title titolo
                         icon 2
           else

              move idx to tot-idx

              |Innanzitutto scrivo le righe con
              |qta totali prese dai volantini
              perform SCRIVI-RIFERIMENTI-PROMO

              perform varying idx from 1 by 1 
                        until idx > tot-idx

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 20
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 15,00 line 06,00
                    move 0 to counter2
                 end-if

                 move low-value         to mro-rec
                 move el-volantino(idx) to mro-promo
                 start mrordini key = mro-k-promo
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read mrordini next 
                               at end exit perform 
                          end-read
                          if mro-promo not = el-volantino(idx)
                             exit perform
                          end-if
                          set trovato to true
                          initialize tms-rec art-rec
                                     replacing numeric data by zeroes
                                          alphanumeric data by spaces
                          move mro-cod-articolo to tms-articolo 
                          read tmp-sellout no lock
                               invalid
                               move mro-cod-articolo to art-codice
                               read articoli invalid continue end-read
                               move art-descrizione      to tms-descr
                               move art-imballo-standard to tms-imb
                    
                               move el-volantino(idx) to rpr-codice
                               move mro-cod-articolo  to rpr-articolo
                               read rpromo no lock 
                                    invalid continue
                                not invalid 
                                    move  rpr-qta      to tms-qta-t
                                    perform AGGIUNGI-QTA-T
                               end-read
                          end-read
070509                    move mro-promo        to tsq-promo
070509                    move mro-cod-articolo to tsq-articolo
070509                    read tmp-sellout-qta 
070509                         invalid continue
070509                     not invalid
070509                         compute como-qta = mro-qta + tsq-qta-b
070509                         if como-qta > tsq-qta-t
070509                            compute mro-qta = tsq-qta-t - 
070509                                              tsq-qta-b
070509                            move tsq-qta-t to tsq-qta-b
070509                         else
070509                            move como-qta to tsq-qta-b
070509                         end-if
070509                         rewrite tsq-rec             
070509                    end-read

                          add mro-qta to tms-qta-b
                          write tms-rec 
                                invalid rewrite tms-rec 
                          end-write
                       end-perform
                 end-start
              end-perform

              perform STAMPA
           end-if.

      ***---
       SCRIVI-RIFERIMENTI-PROMO.
           perform varying idx from 1 by 1
                     until idx > tot-idx
              move low-value         to rpr-rec
              move el-volantino(idx) to rpr-codice
              start rpromo key >= rpr-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rpromo next at end exit perform end-read
                       if rpr-codice not = el-volantino(idx)
                          exit perform
                       end-if
                       initialize tms-rec art-rec
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move rpr-articolo to tms-articolo
                       |Guardo se è un blister
                       move tms-articolo to art-codice
                       read articoli no lock 
                            invalid
                            move art-codice to bli-codice
                            read blister no lock
                                 invalid continue
                             not invalid
                                 move rpr-qta to como-qta
                                 move 0 to bli-idx
                                 perform until 1 = 2
                                    initialize tms-rec art-rec
                                        replacing numeric data by zeroes
                                             alphanumeric data by spaces
                                    add 1 to bli-idx
                                    if bli-el-articolo(bli-idx) = 0
                                       exit perform
                                    end-if
                                    move bli-el-articolo(bli-idx)
                                      to tms-articolo rpr-articolo
                                    compute rpr-qta  = 
                                            como-qta * 
                                            bli-el-qta(bli-idx)
                                    perform DATI-1
                                 end-perform
                            end-read
                        not invalid
                            perform DATI-1
                       end-read
                    end-perform
              end-start
           end-perform.

      ***---
       DATI-1.                                          
           read tmp-sellout no lock
                invalid
                move rpr-articolo to art-codice
                read articoli invalid continue end-read
                move art-descrizione      to tms-descr
                move art-imballo-standard to tms-imb
           end-read.
           add rpr-qta to tms-qta-t.
           write tms-rec invalid rewrite tms-rec end-write.
           perform AGGIUNGI-QTA-T.

070509***---
070509 AGGIUNGI-QTA-T.
070509     move  tms-articolo to tsq-articolo.
070509     move  rpr-codice   to tsq-promo.
070509     read tmp-sellout-qta
070509          invalid move rpr-qta to tsq-qta-t
070509      not invalid add  rpr-qta to tsq-qta-t
070509     end-read.
070509     write tsq-rec invalid rewrite tsq-rec end-write.

      ***---
       STAMPA.
           initialize spooler-link.
           call   "selprint" using selprint-linkage.
           cancel "selprint".
                                                           
           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              move "GESLUX - Sell Out Sicuro" to spl-nome-job
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
              move 0 to num-righe
              move low-value to tms-rec
              start tmp-sellout key >= k-ord
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tmp-sellout next 
                            at end exit perform 
                       end-read
      
                       if num-righe >= 78-MaxRows
                          perform SALTO-PAGINA
                       end-if

                       perform STAMPA-FRAMES
                                                           
                       move tms-articolo      to r-articolo
                       move tms-descr         to r-descr
                       move tms-imb           to r-imb
                       move tms-qta-t         to r-qta-t
                       move tms-qta-b         to r-qta-b
                       move r-riga            to spl-riga-stampa
                       set  spl-nero          to true
                       move 85                to spl-tipo-colonna
                       move Verdana8          to spl-hfont
                       
                       perform SCRIVI
                                                                 
                       move Verdana8B         to spl-hfont
                       subtract passo from save-riga
                       move 85,5     to spl-tipo-colonna
                       set spl-rosso to true
                       compute tms-qta-r = tms-qta-t - tms-qta-b
                       move tms-qta-r         to r-qta-r
                       move r-qta-r           to spl-riga-stampa
                       perform SCRIVI

                       add 0,4 to save-riga
                       perform STAMPA-LINEA

                       subtract 0,4 from save-riga

                       add 1 to num-righe

                    end-perform
              end-start

              perform PIE-DI-PAGINA

              set spl-chiusura to true
              call   "spooler" using spooler-link

           end-if.

      ***---
       STAMPA-FRAMES.
           move 4     to spl-pen-with.

           add 0,45   to save-riga giving spl-riga.
           add 0,36   to spl-riga  giving spl-riga-fine.

           move 1,6                to spl-colonna.
           move 1,6                to spl-colonna-fine.
           set  spl-blu            to true.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-nero          to true.
           set  spl-brush-null    to true.

           move 11,52 to spl-colonna.
           move 13,38 to spl-colonna-fine.
           call "spooler"         using spooler-link.

           move 13,54 to spl-colonna.
           move 15,86 to spl-colonna-fine.
           call "spooler"         using spooler-link.

           move 16,04 to spl-colonna.
           move 18,50 to spl-colonna-fine.
           call "spooler"         using spooler-link.


           add      0,02   to spl-riga.
           subtract 0,02 from spl-riga-fine.

           move 16,06 to spl-colonna.
           move 18,48 to spl-colonna-fine.
           set  spl-giallo        to true.
           set  spl-brush-solid   to true.
           call "spooler"         using spooler-link.

      ***---
       SCRIVI-INTESTAZIONE.
           move 58                       to spl-tipo-colonna.
           set  spl-blu                  to true.
           move 0,4                      to save-riga.
           move "Lubex - ** Sell Out Sicuro **" to spl-riga-stampa.
           move Verdana20BI              to spl-hfont 
           perform SCRIVI.
                                         
           move 0                        to spl-tipo-colonna.
           subtract passo from save-riga.
           set  spl-rosso                to true.
           move 8,71                     to spl-colonna.
           move "S"                      to spl-riga-stampa. 
           perform SCRIVI.

           subtract passo from save-riga.
           move 10,40                    to spl-colonna.
           move "O"                      to spl-riga-stampa.
           perform SCRIVI.

           subtract passo from save-riga.
           set  spl-rosso                to true.
           move 12,07                    to spl-colonna.
           move "S"                      to spl-riga-stampa. 
           perform SCRIVI.
                          
           move 58                       to spl-tipo-colonna.
           move 0,9 to save-riga.
           move Verdana12BI to spl-hfont.
           set  spl-rosso   to true.
                               
      *****     move 1,4 to save-riga.
      *****     move mese-esteso to spl-riga-stampa.
      *****     perform SCRIVI.
                   
           move 2,0 to save-riga.
           set  spl-blu to true.
           move "Quantità con data inizio DPO compresa tra" 
             to spl-riga-stampa.
           perform SCRIVI.
                 
           move 2,6 to save-riga.
           initialize spl-riga-stampa.
           string ini     delimited size
                  "  e  " delimited size
                  fine    delimited size
                  into spl-riga-stampa
           end-string.
           set spl-nero to true.
           perform SCRIVI.

           perform STAMPA-FRAME-TESTA.
           perform STAMPA-LINEA-TESTA.
                                              
           move 84          to spl-tipo-colonna.
           set  spl-nero    to true.
           move Verdana8BI  to spl-hfont.

           move 3,45 to save-riga.
           move r-intesta1 to spl-riga-stampa.
           perform SCRIVI.

           move Verdana6BI to spl-hfont.
           subtract 0,1    from save-riga.
           move r-intesta2 to spl-riga-stampa.
           perform SCRIVI.

           move 4,25 to save-riga.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.
            
      ***---
       STAMPA-LINEA.                  
           move 6                  to spl-pen-with.
           move 1,6                to spl-colonna.
           move 18,5               to spl-colonna-fine.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-blu            to true.
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
           move 0 to num-righe.
       
      ***---
       STAMPA-FRAME-TESTA.
           move 8     to spl-pen-with.
           move 1,6   to spl-colonna.
           move 18,50 to spl-colonna-fine.

           move 3,8   to spl-riga.
           move 4,64  to spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-blu           to true.
           set  spl-brush-null    to true.
           call "spooler"         using spooler-link.
            
      ***---
       STAMPA-LINEA-TESTA.
           move 12                 to spl-pen-with.
           move 1,65               to spl-colonna.
           move 18,46              to spl-colonna-fine.

           move 4,62               to spl-riga spl-riga-fine.

           set  spl-rosso          to true.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.
          
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
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Verdana 6BI
           initialize wfont-data Verdana6BI.
           move 6 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana6BI, wfont-data
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
           close articoli mrordini tmp-sellout tmp-sellout-qta
                 rpromo tpromo blister
           delete file tmp-sellout.
           delete file tmp-sellout-qta.

      ***---
       CONTA-RIGHE.
           |Cancello prima quelle con qta TOTALI a 0
           move low-value to tms-rec.
           start tmp-sellout key >= k-ord
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-sellout next at end exit perform end-read
                    if tms-qta-t = 0
                       delete tmp-sellout record
                              invalid continue 
                       end-delete
                    end-if
                 end-perform
           end-start.

           move 0 to num-righe.
           move low-value to tms-rec.
           start tmp-sellout key >= k-ord
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-sellout next at end exit perform end-read
                    add 1 to num-righe
                 end-perform
           end-start.


      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana20BI.
           destroy Verdana12BI.
           destroy Verdana8BI. 
           destroy Verdana8B.  
           destroy Verdana8.
           destroy Verdana6BI.
           destroy font-pie-pagina.

           cancel "spooler".
           initialize spooler-link.

           display "                                           "
                   upon link-handle at column 15,00 line 06,00.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "pie-di-pagina.cpy".
