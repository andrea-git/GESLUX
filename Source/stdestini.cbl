       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stdestini.
       AUTHOR.                          Andrea.
       REMARKS. Stampa CODICE RAGSOC-INDIRIZZO
                              TEL-EMAIL        dei Destini
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "destini.sl".
           copy "clienti.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "destini.fd".
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".

       78  titolo                value "Stampa Destini".
       78  tot-righe             value 65.
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  num-page              pic 999 value 0.
       77  user-codi             pic x(10).

       77  status-lineseq        pic xx.
       77  status-destini        pic xx.
       77  status-clienti        pic xx.
       77  wstampa               pic x(256).

       77  WrittenRows           pic 99 value 0.
       77  path-txt              pic x(256).

       77  save-codice           pic 9(5).

       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       01  filler                pic 9.
         88 prima-volta          value 1, false 0.

       01  filler                pic 9.
         88 record-ok            value 1, false 0.

      * RIGHE PER LA STAMPA TXT
       01  riga-div              pic x(94) value all "^".

       01  intestazione.
         05 filler               pic x(16) value "Data di stampa: ".
         05 int-data             pic x(10).
         05 filler               pic x(14).
         05 filler               pic x(14) value "Elenco Destini".
         05 filler               pic x(32).
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

       01  riga-2.
         05 filler               pic x(10).
         05 r-prog               pic z(5).
         05 filler               pic x(2).
         05 r-ragsoc-1           pic x(38).
         05 filler               pic x.
         05 r-ragsoc-2           pic x(38). |94

       01  riga-3.
         05 filler               pic x(17).
         05 r-indirizzo          pic x(40).
          
       01  riga-4.
         05 filler               pic x(17).
         05 filler               pic x(7) value "Città: ".
         05 r-localita           pic x(31).
         05 filler               pic x(1). 
         05 filler               pic x(8) value "C.A.P.: ".
         05 r-cap                pic x(05).
          
       01  riga-5.
         05 filler               pic x(17).
         05 filler               pic x(7) value "Tel 1: ".
         05 r-tel-1              pic x(15).
         05 filler               pic x(17).
         05 filler               pic x(8) value "Tel 2 : ".
         05 r-tel-2              pic x(15).

       01  riga-6.
         05 filler               pic x(17).
         05 filler               pic x(7) value "Fax  : ".
         05 r-fax                pic x(15).
         05 filler               pic x(17).
         05 filler               pic x(8) value "E-mail: ".
         05 r-email              pic x(30).

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
           end-evaluate

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
              inspect user-codi replacing trailing spaces by low-value
              string  wstampa      delimited by low-value
                      "stdestini"  delimited by size
                      "_"          delimited by size
                      user-codi    delimited by low-value
                      ".csv"       delimited by size
                      into wstampa
              end-string
           else
              string  wstampa      delimited by low-value
                      "stdestini"  delimited by size
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
              open input destini clienti
           end-if.

      ***---
       ELABORAZIONE.
           move stampa-codice-da  to des-codice.
           move stampa-destino-da to des-prog.
           start destini key is >= des-chiave
                 invalid  set errori to true
           end-start.
           if tutto-ok
              move 0 to cli-codice
              perform until 1 = 2
                 read destini next at end         exit perform end-read
                 if des-codice > stampa-codice-a  exit perform end-if  
                 if des-prog   >= stampa-destino-da and
                    des-prog   <= stampa-destino-a
                    perform VALUTA-RECORD
                 end-if
              end-perform
           end-if.
      
           if StampaExcel perform CALL-EXCEL end-if.

      ***---
       VALUTA-RECORD.
           if des-codice not = cli-codice
              move des-codice to cli-codice
              set cli-tipo-C  to true
              read clienti no lock invalid continue end-read
           end-if.
           set record-ok to true.
           if record-ok
              if stampa-tipo-cli not = spaces and
                 stampa-tipo-cli not = cli-tipo
                 set record-ok to false
              end-if
           end-if.
           if record-ok
              if stampa-cod-gdo not = spaces and
                 stampa-cod-gdo not = cli-gdo
                 set record-ok to false
              end-if
           end-if.
           if record-ok
              if stampa-agente not = 0 and
                 stampa-agente not = cli-agente
                 set record-ok to false
              end-if
           end-if.
           if record-ok
              if StampaExcel perform GENERA-FILE-EXCEL
              else           perform GENERA-FILE-TXT
              end-if
           end-if.

      ***---
       GENERA-FILE-EXCEL.
           if prima-volta
              perform ACCETTA-SEPARATORE
              set prima-volta to false
              initialize line-riga
              string separatore        delimited size
                     separatore        delimited size
                     "DATA DI STAMPA " delimited size
                     int-data          delimited size
                     into line-riga
              end-string
              write line-riga
              initialize line-riga
              string separatore       delimited size
                     separatore       delimited size
                     "ELENCO DESTINI" delimited size
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

           if des-codice not = save-codice
              move des-codice to save-codice
              move des-codice to r-codice
              initialize line-riga
              string r-codice      delimited size
                     separatore    delimited size
                     cli-ragsoc-1  delimited size
                     into line-riga
              end-string
              write line-riga
           end-if.

           move des-prog        to r-prog.
           initialize line-riga.
           string separatore    delimited size
                  r-prog        delimited size
                  separatore    delimited size
                  des-ragsoc-1  delimited size
                  separatore    delimited size
                  des-ragsoc-2  delimited size
                  into line-riga
           end-string.
           write line-riga.
           initialize line-riga.
           string " "           delimited size
                  separatore    delimited size
                  " "           delimited size
                  separatore    delimited size
                  "Indirizzo: " delimited size
                  des-indirizzo delimited size
                  into line-riga
           end-string.
           write line-riga.
           initialize line-riga.
           string " "           delimited size
                  separatore    delimited size
                  " "           delimited size
                  separatore    delimited size
                  "Città: "     delimited size
                  des-localita  delimited size
                  separatore    delimited size
                  "C.A.P.: "    delimited size
                  des-cap       delimited size
                  into line-riga
           end-string.
           write line-riga.
           initialize line-riga.
           string " "           delimited size
                  separatore    delimited size
                  " "           delimited size
                  separatore    delimited size
                  "Tel. 1: "    delimited size
                  des-telef-1   delimited size
                  separatore    delimited size
                  "Tel. 2: "    delimited size
                  des-telef-2   delimited size
                  into line-riga
           end-string.
           write line-riga.
           initialize line-riga.
           string " "           delimited size
                  separatore    delimited size
                  " "           delimited size
                  separatore    delimited size
                  "Fax: "       delimited size
                  des-fax       delimited size
                  separatore    delimited size
                  "E-mail: "    delimited size
                  des-mail      delimited size
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
           
           if WrittenRows > tot-righe - 6
              perform SALTO-PAGINA
              perform SCRIVI-INTESTAZIONE
           end-if.

           if des-codice not = save-codice
              move des-codice      to save-codice
              move des-codice      to r-codice
              move cli-ragsoc-1    to r-cli-ragsoc
              write line-riga    from riga-1
              add 1 to WrittenRows
           end-if.
                                            
           move des-prog        to r-prog.
           move des-ragsoc-1    to r-ragsoc-1.
           move des-ragsoc-2    to r-ragsoc-2. 
           write line-riga    from riga-2.
           add 1 to WrittenRows.

           move des-indirizzo   to r-indirizzo.
           write line-riga    from riga-3.
           add 1 to WrittenRows.

           move des-localita    to r-localita.
           move des-cap         to r-cap.
           write line-riga    from riga-4. 
           add 1 to WrittenRows.

           move des-telef-1     to r-tel-1.
           move des-telef-2     to r-tel-2.
           write line-riga    from riga-5. 
           add 1 to WrittenRows.

           move des-mail        to r-email.
           move des-fax         to r-fax.
           write line-riga    from riga-6. 
           add 1 to WrittenRows.

           if WrittenRows < tot-righe
              write line-riga from spaces
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
           close destini lineseq clienti.

      ***---
       EXIT-PGM.
           move path-txt to stampa-path.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
