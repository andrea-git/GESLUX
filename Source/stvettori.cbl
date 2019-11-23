       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stvettori.
       AUTHOR.                          Andrea.
       REMARKS. Stampa CODICE-DESCRIZIONE-INDIRIZZO dei vettori
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "tvettori.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "tvettori.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".

       78  titolo                value "Stampa Vettori".
       78  tot-righe             value 65.
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  status-lineseq        pic xx.
       77  status-tvettori       pic xx.
       77  wstampa               pic x(256).
       77  user-codi             pic x(10).

       77  WrittenRows           pic 99 value 0.

       77  path-txt              pic x(256).

       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       01  filler                pic 9.
         88 prima-volta          value 1, false 0.

      * RIGHE PER LA STAMPA TXT
       01  riga-div.
         05 filler               pic xx.
         05 filler               pic x(133) value all "=".

       01  intestazione.                                  
         05 filler               pic x(2).
         05 filler               pic x(6)  value "CODICE".
         05 filler               pic x(2).
         05 filler               pic x(11) value "DESCRIZIONE".
         05 filler               pic x(28).
         05 filler               pic x(9)  value "INDIRIZZO".
         05 filler               pic x(72).
         05 filler               pic x(5)  value "SIGLA".

       01  riga.
         05 filler               pic xxx.
         05 r-codice             pic z(5).
         05 filler               pic x(2).
         05 r-descrizione        pic x(37).
         05 filler               pic x(2).
         05 r-indirizzo          pic x(80).
         05 filler               pic x(2).
         05 r-sigla              pic x(3).

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
           initialize wstampa.
           set tutto-ok         to true.
           set prima-volta      to true.
           accept  wstampa      from environment "PATH-ST".
           inspect wstampa      replacing trailing spaces by low-value.
           if StampaExcel
              inspect user-codi replacing trailing spaces by low-value
              string  wstampa      delimited by low-value
                      "stvettori"  delimited by size
                      "_"          delimited by size
                      user-codi    delimited by low-value
                      ".csv"       delimited by size
                      into wstampa
              end-string
           else
              string  wstampa      delimited by low-value
                      "stvettori"  delimited by size
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
              open input tvettori
           end-if.

      ***---
       ELABORAZIONE.
           move stampa-codice-da to vet-codice.
           start tvettori key is >= vet-chiave
                 invalid  set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tvettori next at end       exit perform end-read
                 if vet-codice > stampa-codice-a exit perform end-if
                 if StampaExcel perform GENERA-FILE-EXCEL
                 else           perform GENERA-FILE-TXT
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
              string "CODICE"      delimited size
                     separatore    delimited size
                     "DESCRIZIONE" delimited size
                     separatore    delimited size
                     "INDIRIZZO"   delimited size
                     separatore    delimited size
                     "SIGLA"       delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces
           end-if.
           move vet-codice      to r-codice.
           initialize line-riga.
           string r-codice        delimited size
                  separatore      delimited size
                  vet-descrizione delimited size
                  separatore      delimited size
                  vet-indirizzo   delimited size
                  separatore      delimited size
                  vet-sigla       delimited size
                  into line-riga
           end-string.
           write line-riga.

      ***---
       GENERA-FILE-TXT.
           if prima-volta
              set prima-volta to false
              perform SCRIVI-INTESTAZIONE
           end-if.              
           if WrittenRows = tot-righe
              move 0 to WrittenRows
              perform SCRIVI-INTESTAZIONE
           end-if.
           move vet-codice      to r-codice.
           move vet-descrizione to r-descrizione.
           move vet-indirizzo   to r-indirizzo.
           move vet-sigla       to r-sigla.
           write line-riga from riga.
           add 1 to WrittenRows.

      ***---
       SCRIVI-INTESTAZIONE.
           write line-riga from riga-div.
           write line-riga from intestazione.
           write line-riga from riga-div.
           add 3 to WrittenRows.

      ***---
       CLOSE-FILES.
           close tvettori lineseq.
  
      ***---
       EXIT-PGM.
           move path-txt to stampa-path.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
