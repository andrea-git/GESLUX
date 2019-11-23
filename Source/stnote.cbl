       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stnote.
       AUTHOR.                          Andrea.
       REMARKS. Stampa Note di Consegna del Cliente.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "note.sl".
           copy "clienti.sl".
           copy "destini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "note.fd".
           copy "clienti.fd".
           copy "destini.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".

       78  titolo                value "Stampa Note".
       78  tot-righe             value 65.
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  num-page              pic 999 value 0.

       77  status-lineseq        pic xx.
       77  status-note           pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  wstampa               pic x(256).

       77  WrittenRows           pic 99 value 0.
       77  path-txt              pic x(256).

       77  save-codice           pic 9(5).
       77  user-codi             pic x(10).

       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       01  filler                pic 9.
         88 prima-volta          value 1, false 0.

       01  filler                pic 9.
         88 record-ok            value 1, false 0.

      * RIGHE PER LA STAMPA TXT
       01  riga-div              pic x(78) value all "^".

       01  intestazione.
         05 filler               pic x(16) value "Data di stampa: ".
         05 int-data             pic x(10).
         05 filler               pic x(5).
         05 filler               pic x(16) value "Note di Consegna".
         05 filler               pic x(23).
         05 filler               pic x(5)  value "Pag. ".
         05 int-page             pic zz9.

       01  intestazione-2.
         05 filler               pic x(7)  value "Cliente".
         05 filler               pic x.
         05 filler               pic x(7)  value "Destino".

       01  riga-1.
         05 filler               pic x(2).
         05 r-codice             pic z(5).
         05 filler               pic x(3).
         05 r-cli-ragsoc         pic x(40).

       01 riga-2.
         05 filler               pic x(10).
         05 r-prog               pic z(5).
         05 filler               pic x(1).
         05 r-des-ragsoc         pic x(40).

       01 riga-3.
         05 filler               pic x(16).
         05 r-note-1             pic x(19).
         05 filler               pic x.
         05 r-note-data          pic x(10).
         05 filler               pic x(2).
         05 r-note-2             pic x(30). |78

       01  riga-4.
         05 filler               pic x(16).
         05 r-note-3             pic x(30).
         05 filler               pic x(2).
         05 r-note-4             pic x(30).

       LINKAGE SECTION.
       copy "link-stampa.def".

      ******************************************************************
       PROCEDURE DIVISION using stampa-linkage.

       DECLARATIVES.
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
                move   "File TXT"   to geslock-nome-file
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
           end-evaluate.

      ***---
       NOTE-ERR SECTION.
           use after error procedure on note.
           set tutto-ok  to true.
           evaluate status-note
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File note [NOTE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [NOTE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[NOTE] Indexed file corrupt!"
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
           move stampa-user to user-codi.
           accept como-data from century-date.
           accept como-ora  from time.
           string como-data(7:2) delimited size
                  "/"            delimited size
                  como-data(5:2) delimited size
                  "/"            delimited size
                  como-data(1:4) delimited size
                  into int-data
           end-string.
           initialize wstampa.
           set tutto-ok         to true.
           set prima-volta      to true.
           accept  wstampa      from environment "PATH-ST".
           inspect wstampa      replacing trailing spaces by low-value.
           if StampaExcel
              inspect user-codi  replacing trailing spaces by low-value
              string  wstampa      delimited by low-value
                      "stnote"     delimited by size
                      "_"          delimited by size
                      user-codi    delimited by low-value
                      ".csv"       delimited by size
                      into wstampa
              end-string
           else
              string  wstampa      delimited by low-value
                      "stnote"     delimited by size
                      "_"          delimited by size
                      como-data    delimited by size
                      "_"          delimited by size
                      como-ora     delimited by size
                      ".txt"       delimited by size
                      into wstampa
              end-string
           end-if.
           move wstampa to path-txt.

      ***---
       OPEN-FILES.
           open output lineseq.
           if tutto-ok
              open input note clienti destini
           end-if.

      ***---
       ELABORAZIONE.
           move stampa-codice-da  to not-codice.
           move stampa-destino-da to not-prog.
           start note key is >= not-chiave
                 invalid  set errori to true
           end-start.
           if tutto-ok
              move 0 to cli-codice
              perform until 1 = 2
                 read note next at end            exit perform end-read
                 if not-codice > stampa-codice-a  exit perform end-if  
                 set record-ok to true
                 if not-codice not = cli-codice
                    move not-codice to cli-codice
                    set cli-tipo-C  to true
                    read clienti no lock invalid continue end-read
                 end-if
                 move not-chiave to des-chiave
                 read destini no lock 
                      invalid initialize des-rec 
                 end-read
                 if stampa-tipo-cli not = spaces and
                    stampa-tipo-cli not = cli-tipo
                    set record-ok to false
                 end-if
                 if record-ok
                    if stampa-cod-gdo not = spaces and
                       stampa-cod-gdo not = cli-gdo
                       set record-ok to false
                    end-if
                 end-if
                 if record-ok
                    if stampa-agente not = 0 and
                       stampa-agente not = cli-agente
                       set record-ok to false
                    end-if
                 end-if
                 if record-ok
                    if StampaExcel perform GENERA-FILE-EXCEL
                    else           perform GENERA-FILE-TXT
                    end-if
                 end-if
              end-perform
           end-if.
      
           if StampaExcel perform CALL-EXCEL end-if.

      ***---
       GENERA-FILE-EXCEL.
           if prima-volta
              perform ACCETTA-SEPARATORE
              set prima-volta to false
              initialize line-riga
              string separatore                delimited size
                     separatore                delimited size
                     "DATA DI STAMPA: "        delimited size
                     int-data                  delimited size
                     into line-riga
              end-string
              write line-riga
              initialize line-riga
              string separatore                delimited size
                     separatore                delimited size
                     "** NOTE DI CONSEGNA **"  delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces            
              initialize line-riga
              string "CLIENTE"        delimited size
                     separatore       delimited size
                     separatore       delimited size
                     "DESTINO"        delimited size
                     into line-riga
              end-string
              write line-riga
           end-if.

           if not-codice not = save-codice
              move not-codice to save-codice
              initialize line-riga
              move not-codice      to r-codice
              initialize line-riga
              string r-codice     delimited size
                     separatore   delimited size
                     cli-ragsoc-1 delimited size
                     into line-riga
              end-string
              write line-riga
           end-if.

           initialize line-riga
           move not-prog       to r-prog.
           string separatore   delimited size
                  r-prog       delimited size
                  separatore   delimited size
                  des-ragsoc-1 delimited size
                  into line-riga
           end-string.
           write line-riga.
           
           inspect not-note-1   replacing trailing spaces by low-value.
           initialize r-note-data.
           string not-data(7:2) delimited size
                  "/"           delimited size
                  not-data(5:2) delimited size
                  "/"           delimited size
                  not-data(1:4) delimited size
                  into r-note-data
           end-string.

           initialize line-riga.
           string separatore    delimited size
                  separatore    delimited size     
                  not-note-1    delimited low-value
                  " "           delimited size
                  r-note-data   delimited size
                  separatore    delimited size
                  not-note-2    delimited size
                  into line-riga
           end-string.
           write line-riga.

           initialize line-riga.
           string " "           delimited size
                  separatore    delimited size
                  " "           delimited size
                  separatore    delimited size
                  not-note-3    delimited size
                  separatore    delimited size
                  not-note-4    delimited size
                  into line-riga
           end-string.
           write line-riga.
           write line-riga from spaces.

      ***---
       GENERA-FILE-TXT.
           if prima-volta
              write line-riga from spaces after 1
              add 1 to WrittenRows
              set prima-volta to false
              perform SCRIVI-INTESTAZIONE
           end-if.
           
           if WrittenRows > tot-righe - 4
              perform SALTO-PAGINA
              perform SCRIVI-INTESTAZIONE
           end-if.

           if not-codice not = save-codice
              move not-codice   to save-codice
              move not-codice   to r-codice
              move cli-ragsoc-1 to r-cli-ragsoc
              write line-riga from riga-1
              add 1 to WrittenRows
           end-if.
                                           
           move not-prog        to r-prog.
           move des-ragsoc-1    to r-des-ragsoc.
           write line-riga from riga-2.
           add 1 to WrittenRows.

           move not-note-1      to r-note-1.
           initialize r-note-data.
           string not-data(7:2) delimited size
                  "/"           delimited size
                  not-data(5:2) delimited size
                  "/"           delimited size
                  not-data(1:4) delimited size
                  into r-note-data
           end-string.
           move not-note-2      to r-note-2.
           write line-riga    from riga-3.
           add 1 to WrittenRows.

           move not-note-3      to r-note-3.
           move not-note-4      to r-note-4.
           write line-riga    from riga-4.
           add 1 to WrittenRows.

           if WrittenRows <= tot-righe - 1
              write line-riga    from spaces
              add 1 to WrittenRows
           end-if.

      ***---
       SALTO-PAGINA.
           write line-riga from spaces after page.
           write line-riga from x"09"  after 1.
           move 0 to WrittenRows.

      ***---
       SCRIVI-INTESTAZIONE.
           add 1 to num-page.
           move  num-page      to int-page.
           write line-riga   from intestazione.
           write line-riga   from riga-div.         
           write line-riga   from intestazione-2.
           add 3 to WrittenRows.

      ***---
       CLOSE-FILES.
           close note lineseq clienti destini.

      ***---
       EXIT-PGM.
           move path-txt to stampa-path.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
