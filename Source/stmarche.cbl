       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stmarche.
       AUTHOR.                          Andrea.
       REMARKS. Stampa CODICE-DESCRIZIONE-VARIAZIONI-IMPOSTE delle marche
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "tmarche.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "tmarche.fd".

       WORKING-STORAGE SECTION.
       copy "link-geslock.def".
       copy "Acugui.def".
       copy "common-excel.def".

       78  titolo                value "Stampa Marche".
       78  tot-righe             value 65.
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  status-lineseq        pic xx.
       77  status-tmarche        pic xx.
       77  wstampa               pic x(256).
       77  user-codi             pic x(10).

       77  WrittenRows           pic 99 value 0.

       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

       01  filler                pic 9.
         88 prima-volta          value 1, false 0.

      * RIGHE PER LA STAMPA
       01  riga-div              pic x(93) value all "=".

       01  intestazione.
         05 filler               pic x(6)  value "CODICE".
         05 filler               pic x(2).
         05 filler               pic x(30) value "DESCRIZIONE".
         05 filler               pic x(2).
         05 filler               pic x(17)
                                 value "% VAR ACQ LISTINO".
         05 filler               pic x(3).
         05 filler               pic x(17)
                                 value "% VAR VEN LISTINO".
         05 filler               pic x(4).
         05 filler               pic x(11) value "TRATTAMENTI".
         
       01  intestazione-2.
         05 filler               pic x(43).
         05 filler               pic x(4)  value "NEG.".
         05 filler               pic x(5).
         05 filler               pic x(4)  value "POS.".
         05 filler               pic x(7).
         05 filler               pic x(4)  value "NEG.".
         05 filler               pic x(5).
         05 filler               pic x(4)  value "POS.".
         05 filler               pic x(5).
         05 filler               pic x(5)  value "CONS.".
         05 filler               pic x(3).
         05 filler               pic x(3)  value "COU".

       01  riga.
         05 filler               pic x(2).
         05 r-codice             pic z(4).
         05 filler               pic x(2).
         05 r-descrizione        pic x(30).
         05 filler               pic x(3).
         05 r-v-acq-neg          pic z9,99.
         05 filler               pic x(4).
         05 r-v-acq-pos          pic z9,99.
         05 filler               pic x(6).
         05 r-v-ven-neg          pic z9,99.
         05 filler               pic x(4).
         05 r-v-ven-pos          pic z9,99.
         05 filler               pic x(8).
         05 r-cons               pic x.
         05 filler               pic x(6).
         05 r-cou                pic x.

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
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File marche [TMARCHE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMARCHE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMARCHE] Indexed file corrupt!"
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
           inspect user-codi    replacing trailing spaces by low-value.
           if StampaExcel
              string  wstampa      delimited by low-value
                      "stmarche"   delimited by size
                      "_"          delimited by size
                      user-codi    delimited by low-value
                      ".csv"       delimited by size
                      into wstampa
              end-string
           else
              string  wstampa      delimited by low-value
                      "stmarche"   delimited by size
                      "_"          delimited by size
                      como-data    delimited by size
                      "_"          delimited by size
                      como-ora     delimited by size
                      ".txt"       delimited by size
                      into wstampa
              end-string
           end-if.

      ***---
       OPEN-FILES.
           open output lineseq.
           if tutto-ok
              open input tmarche
           end-if.

      ***---
       ELABORAZIONE.
           move stampa-codice-da to mar-codice.
           start tmarche key is >= mar-chiave
                 invalid  set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tmarche next at end       exit perform end-read
                 if mar-codice > stampa-codice-a exit perform end-if
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
              string "CODICE"                  delimited size
                     separatore                delimited size
                     "DESCRIZIONE"             delimited size
                     separatore                delimited size
                     "% NEG. VAR ACQ LISTINO"  delimited size
                     separatore                delimited size
                     "% POS. VAR ACQ LISTINO"  delimited size
                     separatore                delimited size
                     "% NEG. VAR VEN LISTINO"  delimited size
                     separatore                delimited size
                     "% POS. VAR VEN LISTINO"  delimited size
                     separatore                delimited size
                     "SOGG. IMPOSTA CONSUMO"   delimited size
                     separatore                delimited size
                     "SOGG. IMPOSTA COU"       delimited size
                     into line-riga
              end-string
              write line-riga
              write line-riga from spaces
           end-if.
           move mar-codice                to r-codice.
           move mar-acq-var-listino-meno  to r-v-acq-neg.
           move mar-acq-var-listino-piu   to r-v-acq-pos.
           move mar-ven-var-listino-meno  to r-v-ven-neg.
           move mar-ven-var-listino-piu   to r-v-ven-pos.
           initialize line-riga.
           string r-codice                   delimited size
                  separatore                 delimited size
                  mar-descrizione            delimited size
                  separatore                 delimited size
                  r-v-acq-neg                delimited size
                  separatore                 delimited size
                  r-v-acq-pos                delimited size
                  separatore                 delimited size
                  r-v-ven-neg                delimited size
                  separatore                 delimited size
                  r-v-ven-pos                delimited size
                  separatore                 delimited size
                  mar-tratt-imposta-consumo  delimited size
                  separatore                 delimited size
                  mar-trattamento-cou        delimited size
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
           move mar-codice                to r-codice.
           move mar-descrizione           to r-descrizione.
           move mar-acq-var-listino-meno  to r-v-acq-neg.
           move mar-acq-var-listino-piu   to r-v-acq-pos.
           move mar-ven-var-listino-meno  to r-v-ven-neg.
           move mar-ven-var-listino-piu   to r-v-ven-pos.
           move mar-tratt-imposta-consumo to r-cons.
           move mar-trattamento-cou       to r-cou.
           write line-riga from riga.
           add 1 to WrittenRows.

      ***---
       SCRIVI-INTESTAZIONE.
           write line-riga from riga-div.
           write line-riga from intestazione.
           write line-riga from intestazione-2.
           write line-riga from riga-div.
           add 4 to WrittenRows.

      ***---                                             
       CLOSE-FILES.
           close tmarche lineseq.
  
      ***---
       EXIT-PGM.
           move wstampa to stampa-path.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
