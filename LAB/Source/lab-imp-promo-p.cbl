       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-imp-promo-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Importazione Volantini da "LAB". I volantini verranno prima
           cancellati e poi ricreati. Questo perchè la gestione dei 
           volantini promo passerà al GESLUX. Perciò l'import verrà 
           fatta soltanto al momento dell'installazione.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tgrupgdo.sl".
           copy "articoli.sl".
           copy "blister.sl".
           copy "tpromo.sl".
           copy "rpromo.sl".
           copy "lineseq.sl".
           copy "rep-listini.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "tgrupgdo.fd".
           copy "articoli.fd".
           copy "blister.fd".
           copy "tpromo.fd".
           copy "rpromo.fd". 
           copy "lineseq.fd".
           copy "rep-listini.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Importazione Volantini".

      * FILE-STATUS
       77  status-tgrupgdo           pic xx.
       77  status-articoli           pic xx.
       77  status-blister            pic xx.
       77  status-tpromo             pic xx.
       77  status-rpromo             pic xx.
       77  status-lineseq            pic xx.
       77  status-rep-listini        pic xx.
       77  wstampa                   pic x(256).
       77  path-rep-listini          pic x(256).

      * VARIABILI
       01  testata.
           05 t-codice               pic 9(15).
           05 t-gdo                  pic x(5).
           05 t-ini-dpo              pic 9(8).
           05 t-fine-dpo             pic 9(8).
           05 t-tipo                 pic x.
           05 t-nome                 pic x(50).
           05 t-ini-vol              pic 9(8).
           05 t-fine-vol             pic 9(8).
           05 t-sett                 pic 9(3).

       01  riga.
           05 r-codice               pic 9(15).
           05 r-articolo             pic 9(6).
           05 r-prz                  pic 9(10).
           05 r-qta                  pic 9(9).

       77  articolo-ed               pic z(6).
       77  como-data                 pic 9(8).
       77  como-ora                  pic 9(8).
       77  rec-ko-t                  pic 9(5) value 0.
       77  rec-new-t                 pic 9(5) value 0.
       77  num-rec-t                 pic 9(5) value 0.
       77  rec-ko-r                  pic 9(5) value 0.
       77  rec-new-r                 pic 9(5) value 0.
       77  num-rec-r                 pic 9(5) value 0.
       77  rec-bli-r                 pic 9(5) value 0.
       77  num-rec-ed                pic zz.zzz.
       77  counter                   pic 9(10).
       77  counter2                  pic 9(10).
       77  counter-edit              pic z(10).

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

       01  filler                    pic 9.
         88 record-ok                value 1, false 0.

       01  tipo-errore               pic x.
         88 err-articolo             value "A".
         88 err-gdo                  value "G".
         88 err-tpromo               value "T".

      *****************************************************************

       LINKAGE SECTION.
       01  link-handle    handle of window.
       01  link-user      pic x(10).

       PROCEDURE DIVISION USING link-handle.

       DECLARATIVES.
      ***---
       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "35"
                set errori to true
                display message "File [TGRUPGDO] not found!"
                          title titolo
                           icon 3
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
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                set errori to true
                display message "File [ARTICOLI] not found!"
                          title titolo
                           icon 3
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
           end-evaluate.

      ***---
       BLISTER-ERR SECTION.
           use after error procedure on blister.
           set tutto-ok  to true.
           evaluate status-blister
           when "35"
                set errori to true
                display message "File [BLISTER] not found!"
                          title titolo
                           icon 3
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
           end-evaluate.

      ***---
       TPROMO-ERR SECTION.
           use after error procedure on tpromo.
           set tutto-ok  to true.
           evaluate status-tpromo
           when "35"
                set errori to true
                display message "File [TPROMO] not found!"
                          title titolo
                           icon 3
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
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "tpromo"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output tpromo
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       RPROMO-ERR SECTION.
           use after error procedure on rpromo.
           set tutto-ok  to true.
           evaluate status-rpromo
           when "35"
                set errori to true
                display message "File [RPROMO] not found!"
                          title titolo
                           icon 3
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
           when "93"
           when "99"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "rpromo"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open output rpromo
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                set errori to true
                display message "File [LINESEQ] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [LINESEQ] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
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
           accept como-data from century-date.
           accept como-ora  from time.
           move 0 to counter counter2.
           set tutto-ok      to true.

           initialize wstampa.
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa      delimited low-value
                   "tpromo.csv" delimited size
                   into wstampa
           end-string.
           inspect wstampa replacing trailing low-value by spaces.

           initialize path-rep-listini.
           accept  path-rep-listini from environment "PATH_ST".
           inspect path-rep-listini replacing trailing 
                                    spaces by low-value.
           string  path-rep-listini delimited low-value
                   "REP_LISTINI"    delimited size
                   "_"              delimited size
                   como-data        delimited size
                   "_"              delimited size
                   como-ora         delimited size
                   ".txt"           delimited size
                   into path-rep-listini
           end-string.

      ***---
       OPEN-FILES.
           open output tpromo.
           if tutto-ok
              open output rpromo
              if tutto-ok
                 open output rep-listini
                 if tutto-ok
                    open input tgrupgdo articoli blister lineseq
                    if errori
                       close  tpromo rpromo
                       close  rep-listini
                       delete file rep-listini
                    end-if
                 else
                    close tpromo rpromo
                 end-if
              else
                 close tpromo
              end-if
           end-if.

           if errori goback end-if.

      ***---
       ELABORAZIONE.
           move 0 to num-rec-t.
           perform until 1 = 2 
              add 1 to counter
              add 1 to counter2
              if counter2 = 100
                 if counter = 100
                    display "T" upon link-handle at column 08 line 06
                 end-if
                 move counter to counter-edit
                 display counter-edit
                    upon link-handle at column 11 line 06
                 move 0 to counter2
              end-if

              set record-ok to true
              initialize tipo-errore

              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              unstring line-riga
                       delimited by ";"
                       into t-codice  
                            t-gdo     
                            t-ini-dpo 
                            t-fine-dpo
                            t-tipo    
                            t-nome    
                            t-ini-vol 
                            t-fine-vol
                            t-sett
              end-unstring
              move t-gdo to gdo-codice
              read tgrupgdo no lock 
                   invalid  set record-ok to false
                            set err-gdo   to true
              end-read

              add 1 to num-rec-t

              if record-ok
                 initialize tpr-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move t-codice    to tpr-codice
                 move t-gdo       to tpr-gdo
                 move t-ini-dpo   to tpr-ini-dpo
                 move t-fine-dpo  to tpr-fine-dpo
                 move t-tipo      to tpr-tipo
                 move t-ini-vol   to tpr-ini-volantino
                 move t-fine-vol  to tpr-fine-volantino
                 move t-nome      to tpr-descrizione
                 move t-sett      to tpr-sett-uscita
                 accept tpr-ora-creazione  from time
                 accept tpr-data-creazione from century-date
                 move   link-user          to tpr-utente-creazione
                 write tpr-rec invalid continue end-write
                 add 1 to rec-new-t
              else
                 add 1 to rec-ko-t
                 move num-rec-t to num-rec-ed
                 initialize rlst-rec
                 evaluate true
                 when err-gdo
                      string "Riga "                  delimited size
                             num-rec-ed               delimited size
                             " non importata in "     delimited size
                             "quanto gruppo GDO "     delimited size
                             t-gdo                    delimited size
                             " non trovato in GESLUX" delimited size
                             into rlst-rec
                      end-string
                 end-evaluate
                 write rlst-rec
              end-if

           end-perform.
           close lineseq.

           if rec-ko-t > 0
              write rlst-rec from spaces
              move all "-" to rlst-rec
              write rlst-rec 
              write rlst-rec from spaces
              move rec-ko-t to num-rec-ed
              string "Totale righe TESTA scartate: " delimited size
                     num-rec-ed                      delimited size
                     into rlst-rec
              end-string
              write rlst-rec
              write rlst-rec from spaces after 2
           end-if.     

           move 0 to counter counter2.
           display "                                                   "
              upon link-handle at column 08 line 06.

           initialize wstampa.
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa      delimited low-value
                   "rpromo.csv" delimited size
                   into wstampa
           end-string.
           inspect wstampa replacing trailing low-value by spaces.

           open input lineseq.

           move 0 to num-rec-r.
           perform until 1 = 2 
              add 1 to counter
              add 1 to counter2
              if counter2 = 100
                 if counter = 100
                    display "R" upon link-handle at column 08 line 06
                 end-if
                 move counter to counter-edit
                 display counter-edit
                    upon link-handle at column 11 line 06
                 move 0 to counter2
              end-if

              set record-ok to true
              initialize tipo-errore

              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              inspect line-riga replacing trailing spaces by ";"
              unstring line-riga
                       delimited by ";"
                       into r-codice  
                            r-articolo
                            r-prz
                            r-qta
              end-unstring
              move r-articolo to art-codice
              read articoli no lock 
                   invalid  
                   move r-articolo to bli-codice
                   read blister no lock
                        invalid set record-ok    to false
                                set err-articolo to true
                    not invalid add 1 to rec-bli-r
                   end-read
              end-read

              if record-ok
                 move rpr-codice to tpr-codice
                 read tpromo no lock
                      invalid set record-ok  to false
                              set err-tpromo to true
                 end-read
              end-if

              add 1 to num-rec-r

              if record-ok
                 initialize rpr-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move r-codice    to rpr-codice
                 move r-articolo  to rpr-articolo
                 compute rpr-prz-acq = r-prz / 100
                 move r-qta       to rpr-qta
                 accept rpr-ora-creazione  from time
                 accept rpr-data-creazione from century-date
                 move   link-user          to rpr-utente-creazione
                 write rpr-rec invalid continue end-write
                 add 1 to rec-new-r
              else
                 add 1 to rec-ko-r
                 move num-rec-r to num-rec-ed
                 initialize rlst-rec
                 evaluate true
                 when err-articolo
                      string "Riga "                  delimited size
                             num-rec-ed               delimited size
                             " non importata in "     delimited size
                             "quanto articolo "       delimited size
                             r-articolo               delimited size
                             " non trovato in GESLUX" delimited size
                             into rlst-rec
                      end-string
                 when err-tpromo
                      string "Riga "                  delimited size
                             num-rec-ed               delimited size
                             " non importata in "     delimited size
                             "testata volantino "     delimited size
                             r-codice                 delimited size
                             " non trovato"           delimited size
                             into rlst-rec
                      end-string
                 end-evaluate
                 write rlst-rec
              end-if

           end-perform.

           if rec-ko-r > 0
              write rlst-rec from spaces
              move all "-" to rlst-rec
              write rlst-rec 
              write rlst-rec from spaces
              move rec-ko-r to num-rec-ed
              string "Totale righe scartate: " delimited size
                     num-rec-ed                delimited size
                     into rlst-rec
              end-string
              write rlst-rec
           end-if.

           if rec-ko-t > 0 or 
              rec-ko-r > 0
              display message "Operazione terminata con errori!"
                      x"0d0a""====================="
                      x"0d0a""RIEPILOGO:"
                      x"0d0a"
                      x"0d0a""Totale righe testa " num-rec-t, " di cui:"
                      x"0d0a"" - " rec-new-t,      " inserimenti"
                      x"0d0a"" - " rec-ko-t,       " errate"
                      x"0d0a""Totale righe testa " num-rec-r, " di cui:"
                      x"0d0a"" - " rec-new-r,      " inserimenti"
                      x"0d0a"" - " rec-bli-r,      " blister"
                      x"0d0a"" - " rec-ko-r,       " errate"
                      x"0d0a""Sarà visualizzato un report riepilogativo"
                      title titolo
                       icon 2
           else
              display message "Operazione conclusa con sucesso!"
                      x"0d0a""====================="
                      x"0d0a""RIEPILOGO:"
                      x"0d0a"
                      x"0d0a""Totale righe testata " num-rec-t,
                      x"0d0a""Totale righe         " num-rec-r,
                      x"0d0a""Blister              " rec-bli-r
                      title titolo
           end-if.

      ***---
       CLOSE-FILES.
           close articoli blister lineseq rep-listini 
                 tgrupgdo tpromo rpromo.
           if rec-ko-t > 0 or rec-ko-r > 0
              call   "spooler-a" using "A", path-rep-listini, "O"
              cancel "spooler-a"
           end-if.
              
           delete file rep-listini.

      ***---
       EXIT-PGM.
           display "                                                   "
              upon link-handle at column 08 line 06.
           goback.
