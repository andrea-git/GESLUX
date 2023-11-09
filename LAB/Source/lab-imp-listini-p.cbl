       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      lab-imp-listini-p.
       AUTHOR.                          Andrea.
       REMARKS.                         
           Importazione Listini da "LAB". Le righe già esistenti vengono
           sovrasrcitte ed usate quindi a scopo di rettifica di prezzo
           e/o codice articolo cliente. Le righe nuove invece verranno 
           inserite. La gestione dei listini rimarrà interna al "LAB".
           Le righe con prezzo a 0 significano un recupera dalle promo,
           mentre quelle senza corrispondenza su garticoli o su tgrupgdo
           verranno scartate.
           Dalla rel 1.3.3: se trovassi un prezzo 999.999,99 = "FA".

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tgrupgdo.sl".
           copy "articoli.sl".
           copy "blister.sl".
           copy "listini.sl".
           copy "lineseq.sl".
           copy "rep-listini.sl".


      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "tgrupgdo.fd".
           copy "articoli.fd".
           copy "blister.fd".
           copy "listini.fd".
           copy "lineseq.fd".
           copy "rep-listini.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Importazione Listini".

      * FILE-STATUS
       77  status-tgrupgdo           pic xx.
       77  status-articoli           pic xx.
       77  status-blister            pic xx.
       77  status-listini            pic xx.
       77  status-lineseq            pic xx.
       77  status-rep-listini        pic xx.
       77  wstampa                   pic x(256).
       77  path-rep-listini          pic x(256).

      * VARIABILI
       01  riga-rettifica            occurs 99999.
           05 el-r-codice            pic z(6).
           05 el-r-descr             pic x(50).
           05 el-r-cod-art-cli       pic x(15).
           05 el-r-prz               pic zzz.zz9,99.

       01  riga-inser                occurs 99999.
           05 el-i-codice            pic z(6).
           05 el-i-descr             pic x(50).
           05 el-i-cod-art-cli       pic x(15).
           05 el-i-prz               pic zzz.zz9,99.

       01  riga-blister              occurs 99999.
           05 el-b-codice            pic z(6).
           05 el-b-descr             pic x(50).
           05 el-b-cod-art-cli       pic x(15).
           05 el-b-prz               pic zzz.zz9,99.

       77  idx-r                     pic 99999  value 0.
       77  idx-i                     pic 99999  value 0.
       77  idx-b                     pic 99999  value 0.
       77  idx                       pic 99999  value 0.

       01  riga-listino.
           05 r-gdo                  pic x(5).
           05 r-data                 pic 9(8).
           05 r-articolo             pic 9(6).
           05 r-cod-art-cli          pic x(15).
           05 r-prezzo.
              10 r-prezzo-int        pic X(8).
              10 r-prezzo-dec        pic x(2).
              
       01 save-lst-prg-chiave.
          05 save-lst-prg-cod-articolo         PIC  9(6).
          05 save-lst-prg-cod-magazzino        PIC  X(3).
          05 save-lst-prg-tipo-imballo         PIC  X(3).
          05 save-lst-prg-peso                 PIC  9(5)V9(3).

       77  prezzo-int                pic z(8)vzz.
       77  articolo-ed               pic z(6).
       77  como-data                 pic 9(8).
       77  como-ora                  pic 9(8).
       77  rec-ko                    pic 9(5) value 0.
       77  rec-rettifiche            pic 9(5) value 0.
       77  rec-new                   pic 9(5) value 0.
       77  num-rec                   pic 9(5) value 0.
       77  rec-blister               pic 9(5) value 0.
       77  num-rec-ed                pic zz.zzz.
       77  counter                   pic 9(10).
       77  counter2                  pic 9(10).
       77  counter-edit              pic z(10).
       77  PgmCalling                pic x(20).

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

       01  filler                    pic 9.
         88 record-ok                value 1, false 0.

       01  tipo-errore               pic x.
         88 err-articolo             value "A".
         88 err-gdo                  value "G".

      *****************************************************************

       LINKAGE SECTION.
       01  link-handle    handle of window.
       01  link-user      pic x(10).

       PROCEDURE DIVISION USING link-handle link-user.

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
       LISTINI-ERR SECTION.
           use after error procedure on listini.
           set tutto-ok  to true.
           evaluate status-listini
           when "35"
                set errori to true
                display message "File [LISTINI] not found!"
                          title titolo
                           icon 3
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
                move   "listini"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o listini allowing readers
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
           call "C$CALLEDBY" using PgmCalling.
           accept como-data  from century-date.
           accept como-ora   from time.
           move 0 to counter counter2.
           set tutto-ok to true.

           initialize wstampa.
           accept  wstampa from environment "PATH_CSV".            
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa       delimited low-value
                   "listini.csv" delimited size
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
           open i-o listini allowing readers.
           if tutto-ok
              open output rep-listini
              if tutto-ok
                 open input tgrupgdo articoli lineseq blister
                 if errori
                    close  listini
                    close  rep-listini
                    delete file rep-listini
                 end-if
              else
                 close listini
              end-if
           end-if.

           if errori goback end-if.

      ***---
       ELABORAZIONE.
           move 0 to num-rec.
           perform until 1 = 2 

              if PgmCalling = "lab-imp-listini"
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon link-handle at column 14 line 08
                    move 0 to counter2
                 end-if
              end-if

              set record-ok to true
              initialize tipo-errore

              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              unstring line-riga
                       delimited by ";"
                       into r-gdo
                            r-data
                            r-articolo
                            r-cod-art-cli
                            r-prezzo
              end-unstring
              move r-articolo to art-codice

              read articoli no lock
                   invalid  
                   move r-articolo to bli-codice
                   read blister no lock
                        invalid set record-ok    to false
                                set err-articolo to true
                    not invalid
                        add 1 to rec-blister

                        add 1 to idx-b
                        move r-articolo       to el-r-codice(idx-b)
                        move art-descrizione  to el-r-descr(idx-b)
                        move r-cod-art-cli    to el-r-cod-art-cli(idx-b)
                        move lst-prezzo       to el-r-prz(idx-b)
                   end-read
              end-read

              if record-ok
                 move r-gdo to gdo-codice
                 read tgrupgdo no lock 
                      invalid  set record-ok to false
                               set err-gdo   to true
                 end-read
              end-if

              add 1 to num-rec

              if record-ok
LUBEXX           |Richiesta di Walter 18/09/07. Se il cod art cli
LUBEXX           |è vuoto recupero l'ultimo in base alla data
LUBEXX           if r-cod-art-cli = spaces
                    move r-gdo      to lst-gdo
                    move r-articolo to lst-articolo
                    move high-value to lst-data
                    start listini key <= lst-k-gdo-articolo
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read listini previous 
                                  at end exit perform 
                             end-read
                             if lst-gdo      not = r-gdo   or
                                lst-articolo not = r-articolo
                                exit perform
                             end-if
                             if lst-cod-art-cli not = spaces
                                move lst-cod-art-cli to r-cod-art-cli
                                exit perform
                             end-if
                          end-perform
                    end-start
                 end-if
                 ||||

                 initialize lst-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move r-gdo          to lst-gdo
                 move r-data         to lst-data
                 move r-articolo     to lst-articolo
                 move r-cod-art-cli  to lst-cod-art-cli 
                 move r-prezzo-int   to lst-prezzo convert
                 move r-prezzo(9:2)  to lst-prezzo(13:2)
                 write lst-rec 
                       invalid
      *    Luciano 09/06/2010
      *    metto in linea il vecchio record per mantenere il vecchio prg
                       read LISTINI
                          invalid
                             continue
                       end-read
                       move r-gdo          to lst-gdo
                       move r-data         to lst-data
                       move r-articolo     to lst-articolo
                       move r-cod-art-cli  to lst-cod-art-cli 
                       move r-prezzo-int   to lst-prezzo convert
                       move r-prezzo(9:2)  to lst-prezzo(13:2)
      *    Luciano fine
                       add 1 to rec-rettifiche
                       accept lst-ora-modifica  from time
                       accept lst-data-modifica from century-date
                       if PgmCalling = "lab-imp-listini"
                          move link-user to lst-utente-modifica
                       else
                          move "BATCH"   to lst-utente-modifica
                       end-if                     
                       rewrite lst-rec

                       add 1 to idx-r
                       move r-articolo       to el-r-codice(idx-r)
                       move art-descrizione  to el-r-descr(idx-r)
                       move r-cod-art-cli    to el-r-cod-art-cli(idx-r)
                       move lst-prezzo       to el-r-prz(idx-r)
                   not invalid
                       add 1 to rec-new
                       accept lst-ora-creazione  from time
                       accept lst-data-creazione from century-date
                       if PgmCalling = "lab-imp-listini"
                          move link-user to lst-utente-creazione
                       else
                          move "BATCH"   to lst-utente-creazione
                       end-if
                       rewrite lst-rec
                       move lst-chiave to save-lst-prg-chiave

                       add 1 to idx-i
                       move r-articolo       to el-r-codice(idx-i)
                       move art-descrizione  to el-r-descr(idx-i)
                       move r-cod-art-cli    to el-r-cod-art-cli(idx-i)
                       move lst-prezzo       to el-r-prz(idx-i)

                       start listini key < lst-k-gdo-articolo
                             invalid continue
                         not invalid
                             read listini previous
                             if lst-gdo      = r-gdo  and
                                lst-articolo = r-articolo           
                                move r-gdo          to lst-gdo
                                move r-data         to lst-data
                                move r-articolo     to lst-articolo
                                move lst-prg-chiave 
                                  to save-lst-prg-chiave
                                read listini no lock
                                move save-lst-prg-chiave 
                                  to lst-prg-chiave
                                rewrite lst-rec
                             end-if
                       end-start

                 end-write
              else
                 add 1 to rec-ko
                 move num-rec to num-rec-ed
                 initialize rlst-rec
                 evaluate true
                 when err-articolo
                      move r-articolo to articolo-ed
                      string "Riga "                  delimited size
                             num-rec-ed               delimited size
                             " non importata in"      delimited size
                             " quanto articolo "      delimited size
                             articolo-ed              delimited size
                             " non trovato in GESLUX" delimited size
                             into rlst-rec
                      end-string
                 when err-gdo
                      string "Riga "                  delimited size
                             num-rec-ed               delimited size
                             " non importata in "     delimited size
                             "quanto gruppo GDO "     delimited size
                             r-gdo                    delimited size
                             " non trovato in GESLUX" delimited size
                             into rlst-rec
                      end-string
                 end-evaluate
                 write rlst-rec
              end-if

           end-perform.

           if rec-ko > 0
              write rlst-rec from spaces
              move all "-" to rlst-rec
              write rlst-rec 
              write rlst-rec from spaces
              move rec-ko to num-rec-ed
              string "Totale righe scartate: " delimited size
                     num-rec-ed                delimited size
                     into rlst-rec
              end-string
              write rlst-rec
           end-if.

           if rec-ko > 0
              display message "Operazione terminata con errori!"
                      x"0d0a""====================="
                      x"0d0a""RIEPILOGO:"
                      x"0d0a"
                      x"0d0a""Totale righe listino " num-rec, " di cui:"
                      x"0d0a"" - " rec-rettifiche, " rettifiche"
                      x"0d0a"" - " rec-new,        " inserimenti"
                      x"0d0a"" - " rec-blister     " blister"
                      x"0d0a"" - " rec-ko,         " errate"
                      x"0d0a""Sarà visualizzato report riepilogativo..."
                      title titolo
                       icon 2
           else
              display message "Operazione conclusa con sucesso!"
                      x"0d0a""====================="
                      x"0d0a""RIEPILOGO:"
                      x"0d0a"
                      x"0d0a""Totale righe listino " num-rec, " di cui:"
                      x"0d0a"" - " rec-rettifiche, " rettifiche"
                      x"0d0a"" - " rec-new,        " inserimenti"
                      x"0d0a"" - " rec-blister     " blister"
                      x"0d0a""Sarà visualizzato report riepilogativo..."
                      title titolo
           end-if.

           if idx-r not = 0
            
              write rlst-rec from spaces

              initialize rlst-rec
              string "ARTICOLI RETTIFICATI (" delimited size
                     como-data(7:2)           delimited size
                     "/"                      delimited size
                     como-data(5:2)           delimited size
                     "/"                      delimited size
                     como-data(1:4)           delimited size
                     ")"                      delimited size
                     into rlst-rec
              end-string
              write rlst-rec

              perform varying idx from 1 by 1
                           until idx > idx-r
                 initialize rlst-rec
                 string el-r-codice(idx)      delimited size
                        " "                   delimited size
                        el-r-descr(idx)       delimited size
                        "   "                 delimited size
                        el-r-cod-art-cli(idx) delimited size
                        "   "                 delimited size
                        el-r-prz(idx)         delimited size
                        into rlst-rec
                 end-string
                 write rlst-rec
              end-perform

              move "---------------------------------------"
                to rlst-rec
              write rlst-rec
                                      
              move idx-r to num-rec-ed
              initialize rlst-rec
              string "Totale righe rettificate: " delimited size
                     num-rec-ed                   delimited size
                     into rlst-rec
              end-string
              write rlst-rec
           end-if.

           if idx-i not = 0
              write rlst-rec from spaces

              initialize rlst-rec
              string "ARTICOLI INSERITI (" delimited size
                     como-data(7:2)        delimited size
                     "/"                   delimited size
                     como-data(5:2)        delimited size
                     "/"                   delimited size
                     como-data(1:4)        delimited size
                     ")"                   delimited size
                     into rlst-rec
              end-string
              write rlst-rec

              perform varying idx from 1 by 1
                           until idx > idx-i
                 initialize rlst-rec
                 string el-r-codice(idx)      delimited size
                        " "                   delimited size
                        el-r-descr(idx)       delimited size
                        "   "                 delimited size
                        el-r-cod-art-cli(idx) delimited size
                        "   "                 delimited size
                        el-r-prz(idx)         delimited size
                        into rlst-rec
                 end-string
                 write rlst-rec
              end-perform

              move "---------------------------------------"
                to rlst-rec
              write rlst-rec

              move idx-i to num-rec-ed
              initialize rlst-rec
              string "Totale righe inserite: " delimited size
                     num-rec-ed                delimited size
                     into rlst-rec
              end-string
              write rlst-rec

           end-if.

           if idx-b not = 0
              write rlst-rec from spaces

              initialize rlst-rec
              string "BLISTER ("      delimited size
                     como-data(7:2)   delimited size
                     "/"              delimited size
                     como-data(5:2)   delimited size
                     "/"              delimited size
                     como-data(1:4)   delimited size
                     ")"              delimited size
                     into rlst-rec
              end-string
              write rlst-rec

              perform varying idx from 1 by 1
                           until idx > idx-b
                 initialize rlst-rec
                 string el-r-codice(idx)      delimited size
                        " "                   delimited size
                        el-r-descr(idx)       delimited size
                        "   "                 delimited size
                        el-r-cod-art-cli(idx) delimited size
                        "   "                 delimited size
                        el-r-prz(idx)         delimited size
                        into rlst-rec
                 end-string
                 write rlst-rec
              end-perform

              move "---------------------------------------"
                to rlst-rec
              write rlst-rec

              move idx-b to num-rec-ed
              initialize rlst-rec
              string "Totale righe blister: " delimited size
                     num-rec-ed               delimited size
                     into rlst-rec
              end-string
              write rlst-rec

           end-if.

      ***---
       CLOSE-FILES.
           close articoli lineseq rep-listini tgrupgdo listini blister.
           call   "spooler-a" using "A", path-rep-listini, "O".
           cancel "spooler-a".
              
           delete file rep-listini.

      ***---
       EXIT-PGM.
           if PgmCalling = "lab-imp-listini"
              display "                                                "
                 upon link-handle at column 14 line 08
           end-if.
           goback.
