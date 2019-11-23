       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stgdo-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "articoli.sl".
           copy "tmp-gdo.sl".
           copy "tgrupgdo.sl".
           copy "assorcli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "clienti.fd".
           copy "destini.fd". 
           copy "articoli.fd".
           copy "tmp-gdo.fd".
           copy "tgrupgdo.fd".
           copy "assorcli.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".

       78  titolo                value "Stampa Articoli Cliente GDO".

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  user-codi             pic x(10).

       77  status-destini        pic xx.
       77  status-clienti        pic xx.
       77  status-articoli       pic xx.
       77  status-tmp-gdo        pic xx.
       77  status-tgrupgdo       pic xx.
       77  status-assorcli       pic xx.
       77  status-lineseq        pic xx.
       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).
       77  filler                pic 9.
           88  prima-volta       value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  prg-riga              pic 9(10).

       LINKAGE SECTION.
       copy "link-stgdo-p.def".

      ******************************************************************
       PROCEDURE DIVISION using stgdo-p-linkage.

       DECLARATIVES.
       TMP-GDO SECTION.
           use after error procedure on tmp-gdo.
           set tutto-ok  to true.
           evaluate status-tmp-gdo
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tmp [TMP-GDO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMP-GDO] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMP-GDO] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 0 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                set errori to true
           end-evaluate.  

       LINESEQ SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
                string   "Chiudere file Excel!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File CSV"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                set errori to true
                if riprova
                   open output lineseq
                   set tutto-ok to true
                end-if
           end-evaluate

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2
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
                display message "Impossibile procedere."
                  x"0d0a""File destini [DESTINI] inesistente"
                          title titolo
                           icon 2
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
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                          title titolo
                           icon 2
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
       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File gruppi GDO [TGRUPGDO] inesistente"
                          title titolo
                           icon 2
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
       ASSORCLI-ERR SECTION.
           use after error procedure on assorcli.
           set tutto-ok  to true.
           evaluate status-assorcli
           when "35"
                display message "Impossibile procedere."
               x"0d0a""File assortimento clienti [ASSORCLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [ASSORCLI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[ASSORCLI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.

       END DECLARATIVES.

      ***--- 
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform EXIT-PGM
           end-if.

      ***---
       INIT.
      *-
           move stgdo-p-user to user-codi.
           accept como-data from century-date.
           accept como-ora  from time.
           perform ACCETTA-SEPARATORE.
           move 0 to prg-riga.                        
           initialize path-tmp stgdo-p-path wstampa.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  stgdo-p-path from environment "PATH-ST".
           inspect stgdo-p-path replacing trailing spaces by low-value.
           inspect user-codi    replacing trailing spaces by low-value.
           string  stgdo-p-path delimited by low-value
                   "stgdo"      delimited by size
                   "_"          delimited by size
                   user-codi    delimited by low-value
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   into path-tmp
           end-string.
           string  stgdo-p-path delimited by low-value
                   "stgdo"      delimited by size
                   "_"          delimited by size
                   user-codi    delimited by low-value
                   ".csv"       delimited by size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-TMP-GDO.
           if tutto-ok
              open input articoli 
                         destini 
                         clienti 
                         tgrupgdo
                         assorcli
           end-if.

      ***---
       OPEN-OUTPUT-TMP-GDO.
           open output tmp-gdo.
      
      ***---
       ELABORAZIONE.
           move stgdo-p-codice     to asc-cod-gruppo-gdo.
           if stgdo-p-cli
              move stgdo-p-cliente to asc-cod-cliente
           end-if.
           start assorcli key is >= asc-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 initialize tgdo-rec 
                            cli-rec
                            des-rec  
                            gdo-rec
                            art-rec
                            replacing numeric data by zeroes
                                 alphanumeric data by spaces
                 read assorcli next at end exit perform end-read
                 if stgdo-p-gdo
                    if stgdo-p-codice not = spaces
                       if asc-cod-gruppo-gdo not = stgdo-p-codice
                          exit perform
                       end-if
                    end-if                                 
                 else
                    if stgdo-p-cliente not = 0
                       if asc-cod-cliente not = stgdo-p-cliente
                          exit perform
                       end-if
                    end-if
                 end-if

                 set trovato to true
                 |GRUPPO GDO
                 if asc-cod-gruppo-gdo not = spaces
                    move asc-cod-gruppo-gdo to gdo-codice
                    read tgrupgdo no lock
                         invalid continue
                    end-read
                 end-if

                 |CLIENTE
                 if asc-cod-cliente not = 0
                    move asc-cod-cliente to cli-codice
                    set cli-Tipo-C to true
                    read clienti no lock
                         invalid initialize cli-rec
                    end-read
                 end-if

                 |DESTINO
                 if asc-progressivo-destino not = 0
                    move asc-cod-cliente         to des-codice
                    move asc-progressivo-destino to des-prog
                    read destini no lock
                         invalid initialize des-rec
                    end-read
                 end-if

                 |ARTICOLO
                 move asc-cod-articolo to art-codice
                 read articoli no lock 
                      invalid  move "** NON TROVATO **" 
                                 to art-descrizione
                 end-read
                 perform VALORIZZA-RIGA

              end-perform
           end-if.

           if not trovato
              display message "Nessun assortimento trovato!"
                        title titolo
                         icon 2
           else
              close tmp-gdo
              open input  tmp-gdo
              open output lineseq
              if tutto-ok
                 move low-value to tgdo-rec
                 start tmp-gdo key is >= tgdo-chiave invalid continue
                 end-start
                 perform until 1 = 2
                    read tmp-gdo next at end exit perform end-read
                    initialize line-riga
                    if prima-volta
                       set prima-volta          to false
                       string "COD. GDO"
                              separatore
                              "INTESTAZIONE"
                              separatore
                              "CLIENTE"
                              separatore
                              "RAGIONE SOCIALE"
                              separatore
                              "DESTINO"
                              separatore
                              "DESTINAZIONE"
                              separatore
                              "ARTICOLO"
                              separatore
                              "DESCRIZIONE"
                              separatore
                              "COD. ART. CLI." delimited by size
                              into line-riga
                       end-string
                       write line-riga
                       initialize line-riga
                       write line-riga
                    end-if

                    string tgdo-gdo
                           separatore
                           tgdo-intestazione
                           separatore
                           tgdo-cli
                           separatore
                           tgdo-cli-ragsoc
                           separatore
                           tgdo-des
                           separatore
                           tgdo-des-localita
                           separatore
                           tgdo-art
                           separatore
                           tgdo-art-des
                           separatore
                           tgdo-cod-art-cli delimited by size
                           into line-riga
                    end-string
                    write line-riga
                 end-perform
                 close lineseq
                 perform CALL-EXCEL
              end-if
           end-if.

      ***---
       VALORIZZA-RIGA.
           add 1 to prg-riga.
           inspect cli-ragsoc-1 replacing trailing spaces by low-value.
           initialize tgdo-cli-ragsoc.
           string cli-ragsoc-1 delimited by low-value
                  " "          delimited by size
                  cli-ragsoc-2 delimited by size
                  into tgdo-cli-ragsoc
           end-string.
           if des-localita = spaces
              move cli-localita to des-localita
           end-if.
           move prg-riga         to tgdo-prog.
           move gdo-codice       to tgdo-gdo.
           move gdo-intestazione to tgdo-intestazione.
           move cli-codice       to tgdo-cli.         
           move des-prog         to tgdo-des.
           move des-localita     to tgdo-des-localita.
           move art-codice       to tgdo-art.
           move art-descrizione  to tgdo-art-des.
           move asc-cod-articolo-per-cliente to tgdo-cod-art-cli.

           write tgdo-rec invalid continue end-write.

      ***---
       EXIT-PGM.
           close clienti 
                 destini 
                 articoli 
                 tgrupgdo
                 assorcli.
           close tmp-gdo.
           delete file tmp-gdo.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
