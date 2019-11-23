       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stt-tariffe-p.
       AUTHOR.                          Filippo.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "tarifvet.sl".
           copy "tvettori.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "tprov.sl".
           copy "tregioni.sl".
           copy "tmp-tarifvet.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "tarifvet.fd".
           copy "tvettori.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "tprov.fd".
           copy "tregioni.fd".
           copy "tmp-tarifvet.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".

       78  titolo                value "Stampa tabella tariffe".
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  scelta                pic 9.

       77  status-lineseq        pic xx.
       77  status-tarifvet       pic xx.
       77  status-tvettori       pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-tprov          pic xx.
       77  status-tregioni       pic xx.
       77  status-tmp-tarifvet   pic xx.

       77  path-csv              pic x(256).
       77  path-txt              pic x(256).
       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).
       77  tariffe-path          pic x(256).
       77  SaveMarca             pic 9(5) value 0.
       77  prg-cod-articolo-edit pic z(6).
       77  prg-peso-edit         pic zz9,999.
       77  imballi-ed            pic z.zz9.
       77  wk-movimenti          pic s9(12)v99.
       77  qta-edit              pic z.zzz.
       77  prg-riga              pic 9(10).
       77  user-codi             pic x(10).

       77  tipo-file             pic x.
           88 FileExcel          value "E".
           88 FileTxt            value "T".

       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88  prima-volta       value 1, false 0.
       77  filler                pic 9.
           88  record-ok         value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       LINKAGE SECTION.
       copy "link-stt-tariffe.def".

      ******************************************************************
       PROCEDURE DIVISION using stt-tariffe-linkage.

       DECLARATIVES.

       TMP-TARIFVET-ERR SECTION.
           use after error procedure on tmp-tarifvet.
           set tutto-ok  to true.
           evaluate status-tmp-tarifvet
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tmp [TMP-TARIFVET] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMP-TARIFVET] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMP-TARIFVET] Indexed file corrupt!"
                          title titolo
                           icon 3
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
                     perform OPEN-OUTPUT-TMP-TARIFVET
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

       TARIFVET-ERR SECTION.
           use after error procedure on tarifvet.
           set tutto-ok  to true.
           evaluate status-tarifvet
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tariffe vettori [TARIFVET] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TARIFVET] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TARIFVET] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File vettori [TVETTORI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TVETTORI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TVETTORI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TPROV-ERR SECTION.
           use after error procedure on tprov.
           set tutto-ok  to true.
           evaluate status-tprov
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella provincie [TPROV] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TPROV] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TPROV] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TREGIONI-ERR SECTION.
           use after error procedure on tregioni.
           set tutto-ok  to true.
           evaluate status-tprov
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella regioni [TREGIONI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TREGIONI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TREGIONI] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       LINESEQ-ERR SECTION.
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
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File CSV"   to geslock-nome-file
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
           move sta-user to user-codi.
           accept como-data from century-date.
           accept como-ora  from time.
           perform ACCETTA-SEPARATORE.
           move 0 to prg-riga.
           initialize path-tmp wstampa path-txt.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  tariffe-path  from environment "PATH-ST".
           inspect tariffe-path  replacing trailing spaces by low-value.
           inspect user-codi     replacing trailing spaces by low-value.
           string  tariffe-path  delimited by low-value
                   "stt-tariffe" delimited by size
                   "_"           delimited by size
                   user-codi     delimited by low-value
                   "_"           delimited by size
                   como-data     delimited by size
                   "_"           delimited by size
                   como-ora      delimited by size
                   ".tmp"        delimited by size
                   into path-tmp
           end-string.

           string  tariffe-path  delimited by low-value
                   "stt-tariffe" delimited by size
                   "_"           delimited by size
                   user-codi     delimited by low-value
                   ".csv"        delimited by size
                   into path-csv
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-TMP-TARIFVET.
           if tutto-ok
              open input tarifvet
                         tvettori
                         clienti 
                         destini 
                         tprov   
                         tregioni
           end-if.

      ***---
       OPEN-OUTPUT-TMP-TARIFVET.
           open output tmp-TARIFVET.

      ***---
       ELABORAZIONE.
           move low-value      to tfv-rec.
           move sta-codice-da  to tfv-codice.
           start tarifvet key is >= tfv-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read tarifvet next no lock at end exit perform end-read

                 if tfv-codice > sta-codice-a      exit perform end-if

                 perform VALORIZZA-RIGA

              end-perform
           end-if.

           if not trovato
              display message "Nessuna tariffa trovata"
                        title titolo
                         icon 2
           else
              close tmp-tarifvet
              open input tmp-tarifvet
              if tutto-ok
                 perform OPEN-OUTPUT-LINESEQ
                 if tutto-ok
                    perform GENERA-FILE-EXCEL
                 end-if
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           move path-csv to wstampa.
           open output lineseq.

      ***---
       GENERA-FILE-EXCEL.
           set FileExcel to true.
           move low-value to ttar-rec.
           start tmp-tarifvet key is >= ttar-chiave
              invalid
                 continue
           end-start
           perform until 1 = 2
              read tmp-tarifvet next at end exit perform end-read

              if prima-volta
                 perform SCRIVI-INTESTAZIONE
              end-if

              initialize line-riga
              string ttar-vet-cod
                     separatore
                     ttar-vet-des
                     separatore
                     ttar-vet-tariffa
                     separatore
                     ttar-prov-cod
                     separatore
                     ttar-prov-des
                     separatore
                     ttar-reg-cod
                     separatore
                     ttar-reg-des
                     separatore
                     ttar-cli-cod
                     separatore
                     ttar-cli-des
                     separatore
                     ttar-dest-cod
                     separatore
                     ttar-dest-luogo
                     separatore
                     ttar-qli-da
                     separatore
                     ttar-qli-a
                     separatore
                     ttar-euro delimited by size
                     into line-riga
              end-string
              write line-riga
           end-perform.

           close lineseq.
           perform CALL-EXCEL.

      ***---
       SCRIVI-INTESTAZIONE.
           set prima-volta          to false.
           initialize line-riga.

           string "Cod. Vettore"
                  separatore
                  "Descrizione Vettore"
                  separatore
                  "Tipo Tariffa"
                  separatore
                  "Cod. Provincia"
                  separatore
                  "Descrizione Provincia"
                  separatore
                  "Cod. Regione"
                  separatore
                  "Descrizione Regione"
                  separatore
                  "Cod. Cliente"
                  separatore
                  "Descrizione Cliente"
                  separatore
                  "Cod. Destino"
                  separatore
                  "Luogo Destino"
                  separatore
                  "Scaglione da"
                  separatore
                  "Scaglione a"
                  separatore
                  "Tariffa" delimited by size
                  into line-riga
           end-string

           write line-riga.

      ***---
       VALORIZZA-RIGA.
           add 1             to prg-riga.
           set trovato       to true.
           initialize ttar-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           move prg-riga            to ttar-prog.

           move tfv-codice          to ttar-vet-cod.
           initialize vet-rec
           move tfv-codice          to vet-codice
           read tvettori no lock
              invalid
                 continue
           end-read.
           move vet-descrizione     to ttar-vet-des.
           move vet-tariffa         to ttar-vet-tariffa
           evaluate true
              when vet-regione
                 initialize reg-rec
                 move tfv-campo1 to reg-codice
                                    ttar-reg-cod
                 read tregioni no lock
                    invalid
                       continue
                 end-read
                 move reg-descrizione to ttar-reg-des

              when vet-prov
                 initialize prv-rec
                 move tfv-prov to prv-codice
                                  ttar-prov-cod
                 read tprov no lock
                    invalid
                       continue
                 end-read
                 move prv-descrizione to ttar-prov-des

              when vet-cliente
                 initialize cli-rec
                 move "C"        to cli-tipo-cf
                 move tfv-campo1 to cli-codice
                                    ttar-cli-cod
                 read clienti no lock
                    invalid
                       move "** NON TROVATO **" to cli-ragsoc-1
                 end-read
                 move cli-ragsoc-1 to ttar-cli-des

              when vet-clides
                 initialize cli-rec
                 move "C"        to cli-tipo-cf
                 move tfv-campo1 to cli-codice
                                    ttar-cli-cod
                 read clienti no lock
                    invalid
                       move "** NON TROVATO **" to cli-ragsoc-1
                 end-read
                 move cli-ragsoc-1 to ttar-cli-des

                 initialize des-rec
                 move tfv-campo1 to des-codice
                 move tfv-campo2 to des-prog
                                    ttar-dest-cod
                 read destini no lock
                    invalid
                       move "** NON TROVATO **" to des-localita
                 end-read
                 move des-localita to ttar-dest-luogo

           end-evaluate

           move tfv-qli-da to ttar-qli-da
           move tfv-qli-a  to ttar-qli-a
           move tfv-euro   to ttar-euro

           write ttar-rec invalid continue end-write.

      ***---
       CLOSE-FILES.
           close tarifvet tvettori clienti destini tprov tregioni.
           close tmp-tarifvet.
           delete file tmp-tarifvet.
  
      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
