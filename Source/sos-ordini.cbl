       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      sos-ordini.
       AUTHOR.                          Andrea.
       REMARKS. Vengono considerate le promo con data INIZIO DPO compreso 
                tra la data di oggi e +20gg. --> PROMO URGENTI
                Inoltre stesso ragionamento del sellout "LAB" ma usato 
                dal programma di generazione auto ordini.
                Range di date: data odierna fino a quella indicata.
                In caso di notturna solo oggi ovviamente.
                Nessuna stampa

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "mrordini.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".
           copy "articoli.sl".
           copy "blister.sl".
           copy "ordfor2.sl".
           copy "tmp-sellout.sl".
070509     copy "tmp-sellout-qta.sl".
      *****     copy "user.sl".
      *****     copy "useravv.sl".
      *****     copy "ttipoavv.sl".
           copy "param.sl".
           copy "clienti.sl".
           copy "tgrupgdo.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "mrordini.fd". 
           copy "tpromo.fd".
           copy "rpromo.fd".
           copy "articoli.fd".
           copy "blister.fd".
           copy "ordfor2.fd".
           copy "tmp-sellout.fd".
070509     copy "tmp-sellout-qta.fd".
      ****     copy "user.fd".
      ****     copy "useravv.fd".
      ****     copy "ttipoavv.fd".
           copy "param.fd".
           copy "clienti.fd".
           copy "tgrupgdo.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "trova-parametro.def".     
       01  r-inizio              pic x(25).

       77  status-mrordini                pic xx.
       77  status-tpromo                  pic xx.
       77  status-rpromo                  pic xx.
       77  status-articoli                pic xx.
       77  status-blister                 pic xx.
       77  status-ordfor2                 pic xx.
       77  status-lineseq                 pic xx.
       77  status-tmp-sellout             pic xx.
070509 77  status-tmp-sellout-qta         pic xx.
       77  path-tmp-sellout               pic x(256).
070509 77  path-tmp-sellout-qta           pic x(256).
      ***** 77  status-ttipoavv                pic xx.
      ***** 77  status-useravv                 pic xx.
      ***** 77  status-user                    pic xx.
       77  status-param                   pic xx.
       77  status-clienti                 pic xx.
       77  status-tgrupgdo                pic xx.
       77  wstampa                        pic x(256).

      * COSTANTI
       78  titolo                value "Sell Out Sicuro".

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 scritto-avviso     value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 bisestile          value 1 false 0.
       77  filler                pic 9.
           88 CurrMonthOrNext    value 1 false 0.
       77  filler                pic 9.
           88 RichiamoSchedulato     value 1 false 0.

      * VARIABILI
       77  PgmChiamante          pic x(15).
       77  idx                   pic 9(3).
       77  idx-mese              pic 9(3).
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
           10 el-mese            pic 99.
           10 el-ini-dpo         pic 9(8).

       77  como-riga             pic x(150).
       77  data-oggi             pic 9(8).
       77  como-data             pic 9(8).
       77  Plus20gg              pic 9(8).
       77  como-ora              pic 9(8).
       77  como-mese             pic 99.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).

       77  mese                  pic 99.
       77  mese-1a               pic 99.
       77  mese-1b               pic 99.
       77  mese-2                pic 99.
       77  mese-3                pic 99.
       77  mese-4                pic 99.
       77  mese-5                pic 99.
       77  mese-6                pic 99.

       77  nargs                 pic 9(3) comp-1.

       LINKAGE SECTION.
       77  link-data             pic 9(8).
       77  link-handle           handle of window.
       77  link-scheduler        pic x.

      ******************************************************************
       PROCEDURE DIVISION using link-data,
                                link-handle
                                link-scheduler.

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
       ORDFOR2-ERR SECTION.
           use after error procedure on ordfor2.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ordfor2
           when "39"
                set errori to true
                display message "File [ORDFOR2] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ORDFOR2] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [ORDFOR2] inesistente"
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
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept data-oggi from century-date.
           compute Plus20gg = function integer-of-date (data-oggi).
           add 20 to Plus20gg.
           compute Plus20gg = function date-of-integer (Plus20gg).
                                                
           set RichiamoSchedulato to false.
           set scritto-avviso to false.
           call "C$CALLEDBY" using PgmChiamante.
           if PgmChiamante = "pordini"
              call "C$NARG" using nargs
              if nargs = 3
                 if link-scheduler = "X"
                    set RichiamoSchedulato to true
                    accept wstampa 
                           from environment "SCHEDULER_PATH_LOG"
                    inspect wstampa 
                            replacing trailing spaces by low-value
                    string wstampa            delimited low-value
                           "LOG_SOS-ORDINI_"  delimited size
                           como-data          delimited size
                           "_"                delimited size
                           como-ora           delimited size
                           ".log"             delimited size
                           into wstampa
                    end-string
                    open output lineseq
                    move "INIZIO PROGRAMMA" to como-riga
                    perform SCRIVI-RIGA-LOG
                 end-if
              end-if
           end-if.              

           move 0    to idx.
           move 0        to counter counter2.
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.
           initialize path-tmp-sellout.
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

           move link-data(5:2) to mese-6.
           subtract 1 from mese-6 giving mese-5.
           if mese-5 = 0
              move 12 to mese-5
           end-if.
           subtract 1 from mese-5 giving mese-4.
           if mese-4 = 0
              move 12 to mese-4
           end-if.
           subtract 1 from mese-4 giving mese-3.
           if mese-3 = 0
              move 12 to mese-3
           end-if.
           subtract 1 from mese-3 giving mese-2.
           if mese-2 = 0
              move 12 to mese-2
           end-if.
           subtract 1 from mese-2 giving mese-1b.
           if mese-1b = 0
              move 12 to mese-1b
           end-if.
           move como-data(5:2) to mese-1a.

      ***---
       OPEN-FILES.      
           if RichiamoSchedulato            
              move "OPEN-FILES" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.

           open output tmp-sellout tmp-sellout-qta.
           if tutto-ok
              close tmp-sellout tmp-sellout-qta
              open i-o tmp-sellout     allowing readers
              open i-o tmp-sellout-qta allowing readers
              open input articoli mrordini rpromo tpromo blister
                         param clienti tgrupgdo |ttipoavv user 
              open i-o ordfor2 |useravv
           end-if.

      ***---
       ELABORAZIONE.     
           if RichiamoSchedulato            
              move "ELABORAZIONE" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.

           if not RichiamoSchedulato
              modify link-handle visible true
           end-if.

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

           perform PROMO-URGENTI.

           move 0 to counter2.

           accept inizio-mese from century-date.
           move link-data to fine-mese.

           move low-value      to tpr-rec.
           start tpromo key >= tpr-k-data-ins
                 invalid continue
             not invalid
                 perform until 1 = 2
           
                    read tpromo next at end exit perform end-read

                    if not RichiamoSchedulato
                       add 1 to counter
                       add 1 to counter2
                       if counter2 = 10
                          move counter to counter-edit
                          display counter-edit
                             upon link-handle at column 53
                                                   line 04
                          move 0 to counter2
                       end-if
                    end-if

                    move tpr-gdo     to cli-gdo gdo-codice
                    read tgrupgdo no lock
                         invalid  continue
                    end-read
                    move gdo-tipocli to cli-tipo

                    perform TROVA-PARAMETRO-GDO
                    
                    compute como-data = 
                            function integer-of-date(data-oggi)
                    |23/02/2011 6 mesi fisso (richiesta di Walter)
                    add 180 to como-data
      *****              add prm-gg-inizio-vol to como-data
                    compute como-data = 
                            function date-of-integer(como-data)
           
                    if tpr-fine-volantino >= 20100601   and
                       tpr-ini-volantino  <  como-data  and
                       tpr-fine-volantino >= data-oggi
      
                       add 1 to idx
                       move tpr-codice to el-volantino(idx)
      
                       if tpr-ini-dpo < inizio-mese
                          move inizio-mese(5:2) to mese
                       else
                          move tpr-ini-dpo(5:2) to mese
                       end-if
      
                       evaluate true
                       when mese = mese-1a
                            move 1 to el-mese(idx)
                       when mese = mese-1b
                            move 1 to el-mese(idx)
                       when mese = mese-2 
                            move 2 to el-mese(idx)
                       when mese = mese-3 
                            move 3 to el-mese(idx)
                       when mese = mese-4 
                            move 4 to el-mese(idx)
                       when mese = mese-5 
                            move 5 to el-mese(idx)
                       when other
                            move 6 to el-mese(idx)
                       end-evaluate

                    end-if
                 end-perform
           end-start.                   

           if idx not = 0         

              move idx to tot-idx

              |Innanzitutto scrivo le righe con
              |qta totali prese dai volantini
              perform SCRIVI-RIFERIMENTI-PROMO
                                   
              move 0 to counter2
              perform varying idx from 1 by 1 
                        until idx > tot-idx 

                 move low-value         to mro-rec
                 move el-volantino(idx) to mro-promo
                 move el-mese(idx)      to idx-mese

                 start mrordini key >= mro-k-promo
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read mrordini next 
                               at end exit perform 
                          end-read
                          if mro-promo not = el-volantino(idx)
                             exit perform
                          end-if      

                          if not RichiamoSchedulato
                             add 1 to counter
                             add 1 to counter2
                             if counter2 = 10
                                move counter to counter-edit
                                display counter-edit
                                   upon link-handle at column 53
                                                         line 04
                                move 0 to counter2
                             end-if
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
                                    move rpr-qta to tms-qta-t   
                                    move rpr-qta to 
                                         tms-qta-t-mese(idx-mese)
070509                              perform AGGIUNGI-QTA-T
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
070509                            move como-qta  to tsq-qta-b
070509                         end-if
070509                         rewrite tsq-rec
070509                    end-read

                          add mro-qta to tms-qta-b     
                          add mro-qta to tms-qta-b-mese(idx-mese)

                          write tms-rec
                                invalid rewrite tms-rec
                          end-write
                       end-perform
                 end-start
              end-perform
              perform ASSEGNA-QTA
           end-if.

           perform QTA-PROMO-URGENTI.

      ***---
       SCRIVI-RIFERIMENTI-PROMO.
           if RichiamoSchedulato            
              move "SCRIVI-RIFERIMENTI-PROMO" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           move 0 to counter2.
           perform varying idx from 1 by 1
                     until idx > tot-idx
              move low-value         to rpr-rec
              move el-volantino(idx) to rpr-codice
              move el-mese(idx)      to idx-mese
              start rpromo key >= rpr-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read rpromo next at end exit perform end-read
                       if rpr-codice not = el-volantino(idx)
                          exit perform
                       end-if 

                       if not RichiamoSchedulato
                          add 1 to counter
                          add 1 to counter2
                          if counter2 = 10
                             move counter to counter-edit
                             display counter-edit
                                upon link-handle at column 53
                                                      line 04
                             move 0 to counter2
                          end-if
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
                                    compute rpr-qta = 
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
      *****     if tms-articolo = 588 stop "K" end-if
           read tmp-sellout no lock
                invalid
                move rpr-articolo to art-codice
                read articoli invalid continue end-read
                move art-descrizione      to tms-descr
                move art-imballo-standard to tms-imb
           end-read.
           add rpr-qta to tms-qta-t.
           add rpr-qta to tms-qta-t-mese(idx-mese).
           write tms-rec invalid rewrite tms-rec end-write.
           perform AGGIUNGI-QTA-T.

070509***---
070509 AGGIUNGI-QTA-T.
070509     move  tms-articolo to tsq-articolo.
070509     move  rpr-codice   to tsq-promo.
070509     read tmp-sellout-qta
070509          invalid 
070509          move rpr-qta to tsq-qta-t
070509          move rpr-qta to tsq-qta-t-mese(idx-mese)
070509      not invalid 
070509          add  rpr-qta to tsq-qta-t
070509          add  rpr-qta to tsq-qta-t-mese(idx-mese)
070509     end-read.
070509     write tsq-rec invalid rewrite tsq-rec end-write.

      ***---
       ASSEGNA-QTA.             
           if RichiamoSchedulato            
              move "ASSEGNA-QTA" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           move 0 to counter2.
           move low-value to tms-rec.
           start tmp-sellout key >= tms-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read tmp-sellout next at end exit perform end-read
              move "LBX"        to ord2-mag
              move tms-articolo to ord2-articolo
      *****        if tms-articolo = 588 stop "K" end-if
              read ordfor2 key ord2-chiave
                   invalid continue
               not invalid
                   move 0 to ord2-promo
                   perform varying idx-mese from 1 by 1 
                             until idx-mese > 6
                      if tms-qta-t-mese(idx-mese) >
                         tms-qta-b-mese(idx-mese)
                         compute ord2-promo-mese(idx-mese) = 
                                  tms-qta-t-mese(idx-mese) - 
                                  tms-qta-b-mese(idx-mese)
                         add ord2-promo-mese(idx-mese) to ord2-promo
                      else
                         move 0 to ord2-promo-mese(idx-mese)
                      end-if
                   end-perform

                   rewrite ord2-rec
              end-read
           end-perform.

      ***---
       PROMO-URGENTI.           
           if RichiamoSchedulato            
              move "PROMO-URGENTI" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           move 0 to counter2.
           move low-value to ord2-rec.
           start ordfor2 key >= ord2-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read ordfor2 next at end exit perform end-read 

                    if not RichiamoSchedulato
                       add 1 to counter
                       add 1 to counter2
                       if counter2 = 10
                          move counter to counter-edit
                          display counter-edit
                             upon link-handle at column 53
                                                   line 04
                          move 0 to counter2
                       end-if
                    end-if

                    move 0 to ord2-promo
                    move 0 to ord2-promo-urgente
                    set ord2-no-urgente to true
                    rewrite ord2-rec invalid continue end-rewrite
                 end-perform
           end-start.

      *****     move low-value to uav-rec.
      *****     start useravv key >= uav-chiave
      *****           invalid continue
      *****       not invalid
      *****           perform until 1 = 2
      *****              read useravv next at end exit perform end-read
      *****              move uav-tipologia to tav-codice
      *****              read ttipoavv no lock invalid continue end-read
      *****              if tav-promo
      *****                 delete useravv record
      *****              end-if
      *****           end-perform
      *****     end-start.

           move 0 to counter2.
           move low-value   to tpr-rec.
           move inizio-mese to tpr-ini-dpo.
           start tpromo key >= tpr-chiave-ini
                 invalid continue
             not invalid
                 perform until 1 = 2
                    set record-ok to false
                    read tpromo next at end    exit perform end-read

                    if not RichiamoSchedulato
                       add 1 to counter
                       add 1 to counter2
                       if counter2 = 10
                          move counter to counter-edit
                          display counter-edit
                             upon link-handle at column 53
                                                   line 04
                          move 0 to counter2
                       end-if
                    end-if

                    if tpr-ini-dpo  > Plus20gg exit perform end-if

                    if tpr-ini-dpo >= data-oggi and
                       tpr-ini-dpo <=  Plus20gg
                       move low-value   to rpr-chiave
                       move tpr-codice  to rpr-codice
                       start rpromo key >= rpr-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read rpromo next 
                                     at end exit perform 
                                end-read
                                if rpr-codice not = tpr-codice
                                   exit perform
                                end-if
                                move rpr-articolo to ord2-articolo
                                move "LBX"        to ord2-mag
                                read ordfor2 no lock 
                                     invalid continue
                                 not invalid
      *****                               add rpr-qta to ord2-promo-urgente
                                     set ord2-si-urgente to true
      *****                               if ord2-fabb-qta(1) > 0 
      *****                                  if not scritto-avviso
      *****                                     set scritto-avviso to true
      *****                                     perform SCRIVI-AVVISO
      *****                                  end-if
      *****                               end-if
                                     rewrite ord2-rec
                                end-read
                             end-perform
                       end-start
                    end-if

                 end-perform
           end-start.

      ********---
      ***** SCRIVI-AVVISO.
      *****     move low-value to tav-codice.
      *****     start ttipoavv key >= tav-chiave
      *****           invalid continue
      *****       not invalid
      *****           perform until 1 = 2
      *****              read ttipoavv next at end exit perform end-read
      *****              if tav-promo
      *****                 move low-value to user-rec
      *****                 start user key >= user-chiave
      *****                       invalid continue
      *****                   not invalid
      *****                       perform until 1 = 2
      *****                          read user next 
      *****                               at end exit perform 
      *****                          end-read
      *****                          if user-si-msg
      *****                             initialize uav-rec
      *****                             move user-cod   to uav-utente
      *****                             move tav-codice to uav-tipologia
      *****                             accept uav-data-creazione 
      *****                                    from century-date
      *****                             accept uav-ora-creazione 
      *****                                    from century-date
      *****                             move "AUTO" to uav-utente-creazione
      *****                             write uav-rec 
      *****                                   invalid continue 
      *****                             end-write
      *****                          end-if
      *****                       end-perform
      *****                 end-start
      *****              end-if
      *****           end-perform
      *****     end-start.

      ***---
       QTA-PROMO-URGENTI.         
           if RichiamoSchedulato            
              move "QTA-PROMO-URGENTI" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
           move 0 to counter2.
           move low-value   to tpr-rec.
           start tpromo key >= tpr-chiave-ini
                 invalid continue
             not invalid
                 perform until 1 = 2
                    set record-ok to false
                    read tpromo next at end    exit perform end-read
                    if tpr-ini-dpo  > Plus20gg exit perform end-if
      
                    if tpr-ini-dpo >= data-oggi and
                       tpr-ini-dpo <=  Plus20gg
                       move low-value   to rpr-chiave
                       move tpr-codice  to rpr-codice
                       start rpromo key >= rpr-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read rpromo next 
                                     at end exit perform 
                                end-read
                                if rpr-codice not = tpr-codice
                                   exit perform
                                end-if       

                                if not RichiamoSchedulato
                                   add 1 to counter
                                   add 1 to counter2
                                   if counter2 = 10
                                      move counter to counter-edit
                                      display counter-edit
                                         upon link-handle at column 53
                                                               line 04
                                      move 0 to counter2
                                   end-if
                                end-if

                                move rpr-articolo to ord2-articolo
                                move "LBX"        to ord2-mag
                                read ordfor2 no lock 
                                     invalid continue
                                 not invalid
                                     move rpr-codice   to tsq-promo
                                     move rpr-articolo to tsq-articolo
                                     read tmp-sellout-qta
                                          invalid
                                          move rpr-qta to como-qta
                                      not invalid
                                          compute como-qta =
                                                  tsq-qta-b -tsq-qta-t
                                     end-read
                                     add como-qta to ord2-promo-urgente
                                     rewrite ord2-rec
                                end-read
                             end-perform
                       end-start
                    end-if

                 end-perform
           end-start.

      ***---
       CLOSE-FILES.                   
           if RichiamoSchedulato            
              move "CLOSE-FILES" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.

           close articoli mrordini ordfor2 tmp-sellout param clienti
                 rpromo tpromo blister tgrupgdo. |useravv ttipoavv user 
           delete file tmp-sellout.

      ***---
       EXIT-PGM.
           if not RichiamoSchedulato
              display "                                           "
                      upon link-handle at column 15,00 line 06,00
           end-if.                                        
                    
           if RichiamoSchedulato            
              move "FINE PROGRAMMA" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.

           goback.         

      ***---
       SCRIVI-RIGA-LOG.        
           perform SETTA-INIZIO-RIGA.
           initialize line-riga.       
           string r-inizio  delimited size
                  como-riga delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       PARAGRAFO-COPY.
           copy "trova-parametro.cpy".  
           copy "setta-inizio-riga.cpy".
