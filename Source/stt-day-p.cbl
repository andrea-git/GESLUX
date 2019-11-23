       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stt-day-p.
       AUTHOR.                          Filippo.
       REMARKS.  Stampa giornaliera di controllo
      ******************************************************************
       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "trasporti.sl".
           copy "clienti.sl".
           copy "lineseq.sl".
           copy "tvettori.sl".
           copy "tmp-stt-day.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "trasporti.fd".
           copy "clienti.fd".
           copy "tvettori.fd".
           copy "lineseq.fd".
           copy "tmp-stt-day.fd".

       WORKING-STORAGE SECTION.
           copy "acucobol.def".
           copy "link-geslock.def".
           copy "comune.def".

       77  status-trasporti      pic x(2).
       77  status-clienti        pic x(2).
       77  status-tvettori       pic x(2).
       77  status-lineseq        pic x(2).
       77  status-tmp-stt-day    pic x(2).
       77  path-tmp-stt-day      pic x(256).
       77  wstampa               pic x(256).

       77  sw-trasporti          pic 9 value 0.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  data6                 pic 9(6).
       77  tot-bolle             pic 9(5)      value 0.
       77  tot-kg                pic s9(9)v999 value 0.

       78  titolo value "Stampa giornaliera di controllo".

       01  st-line-riga     pic x(80) value all "-".

       77  i                pic 9(5) value 0.      
       78  max-righe        value 66.
       77  num-righe        pic 99 value 0.
       77  diff-righe       pic 99 value 0.
       77  n-vuote          pic 99 value 0.
       77  sav-riga         pic x(900).

       01  filler                     pic 9.
         88 prima-volta               value 1, false 0.

       01  r-riga.
           05 r-data-bolla      pic 99/99/99.
           05 filler            pic x(02).
           05 r-num-bolla       pic z(08).
           05 filler            pic x.
           05 r-cliente         pic x(30).
           05 filler            pic x(01).
           05 r-qta-kg          pic ----.--9,999.
           05 filler            pic x(01).
           05 r-vettore         pic x(18).

       01  r-totali.                
           05 filler            pic x(10).
           05 r-tot-bolle       pic z(08).
           05 filler            pic x(01).
           05 filler            pic x(05) value "bolle".
           05 filler            pic x(13) value spaces.
           05 filler            pic x(11) value "TOTALE Kg. ".
           05 r-tot-kg          pic --.---.--9,999.

       01  r-intesta.
           05 filler            pic x(18).
           05 filler            pic x(35) value
              "Elenco BOLLE FATTURATE il giorno: ".
           05 int-data          pic 99/99/99.

       LINKAGE SECTION.
       copy "link-stt-day.def".

      ******************************************************************
       PROCEDURE DIVISION using stt-day-linkage.

       DECLARATIVES.
       TRASPORTI-ERR SECTION.
           use after error procedure on trasporti.
           set tutto-ok  to true.
           evaluate status-trasporti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File trasporti [TRASPORTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TRASPORTI] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TRASPORTI] Indexed file corrupt!"
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

       TMP-STT-DAY-ERR SECTION.
           use after error procedure on tmp-stt-day.
           set tutto-ok  to true.
           evaluate status-tmp-stt-day
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tmp [TMP-STT-DAY-ERR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMP-STT-DAY-ERR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message"[TMP-STT-DAY-ERR] Indexed file corrupt!"
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
                     open output tmp-stt-day
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
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
           set tutto-ok      to true.
           set trovato       to false.
           set prima-volta   to true.
           accept  wstampa      from environment "PATH-ST".
           accept  como-data         from century-date.
           accept  como-ora          from time.
           inspect wstampa      replacing trailing 
                                     spaces by low-value.
           string wstampa       delimited by low-value
                  "stt-day"          delimited by size
                  "_"                delimited by size
                  como-data          delimited by size
                  "_"                delimited by size
                  como-ora           delimited by size
                  ".txt"             delimited by size
                  into wstampa
           end-string.

           accept  path-tmp-stt-day from environment "PATH-ST".
           inspect path-tmp-stt-day replacing trailing 
                                       spaces by low-value.
           string path-tmp-stt-day  delimited by low-value
                  "stt-day"         delimited by size
                  "_"               delimited by size
                  como-data         delimited by size
                  "_"               delimited by size
                  como-ora          delimited by size
                  ".tmp"            delimited by size
                  into path-tmp-stt-day
           end-string.

       OPEN-FILES.
           perform OPEN-OUTPUT-TMP-STT-DAY.
           if tutto-ok
              open input trasporti clienti tvettori
              if errori
                 close       lineseq
                 delete file lineseq
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-TMP-STT-DAY.
           open output tmp-stt-day.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.
           if tutto-ok
              write line-riga from space after 0
           end-if.
      
      ***---
       ELABORAZIONE.
           |lettura trasporti
           move 0 to num-righe 
           initialize trs-rec.
           move sta-data     to trs-data-fattura.

           start trasporti key is >= k-data-fattura
              invalid set errori to true 
           end-start
           if tutto-ok
              perform until 1 = 2
                 read trasporti next no lock 
                    at end
                       exit perform
                 end-read

                 if trs-data-fattura = sta-data
                    perform STAMPA-MOVIMENTATO
                 end-if
                 if errori 
                    exit perform 
                 end-if

              end-perform
              if trovato perform CREA-TXT end-if
           end-if.

           if not trovato
              display message "Nessun movimento in uscita"
                              " per il giorno selezionato"
                        title titolo
                         icon 2
           end-if.

      ***---
       STAMPA-MOVIMENTATO.
           if tutto-ok
              initialize tstt-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces
              move trs-data-bolla(3:2) to data6(5:2)
              move trs-data-bolla(5:2) to data6(3:2)
              move trs-data-bolla(7:2) to data6(1:2)
              move data6               to tstt-data-bolla

              move trs-num-bolla       to tstt-num-bolla

              initialize cli-rec
              set cli-tipo-C   to true
              move trs-cliente to cli-codice
              read clienti no lock
                   invalid move "** NON TROVATO **" to cli-ragsoc-1
              end-read
              move cli-ragsoc-1        to tstt-cliente
              move trs-qta-kg          to tstt-qta-kg

              initialize vet-rec
              move trs-vettore  to vet-codice tstt-vet-codice
              read tvettori no  lock invalid continue end-read
              move vet-descrizione to tstt-vet-descrizione

              perform until 1 = 2
                 add 1 to tstt-progressivo
                 write tstt-rec 
                       invalid continue
                   not invalid exit perform
                 end-write
              end-perform

              set trovato to true

           end-if.

      ***---
       CREA-TXT.
           close      tmp-stt-day.
           open input tmp-stt-day.

           set prima-volta to true.
           set trovato     to false.
           move 0 to tot-bolle.

           move low-value to tstt-chiave.
           start tmp-stt-day key is >= tstt-chiave
                 invalid continue
             not invalid set trovato to true
           end-start.
           if trovato
              perform until 1 = 2

                 read tmp-stt-day next at end exit perform end-read
      
                 if prima-volta
                    perform OPEN-OUTPUT-LINESEQ
                    if errori exit perform end-if

                    write line-riga from spaces after 1
                    add 1 to num-righe

                    set prima-volta to false
                    perform INTESTAZIONE
                 end-if

                 initialize r-riga
                 move tstt-data-bolla      to r-data-bolla
                 move tstt-num-bolla       to r-num-bolla
                 move tstt-cliente         to r-cliente
                 move tstt-qta-kg          to r-qta-kg
                 move tstt-vet-descrizione to r-vettore

                 initialize line-riga
                 move r-riga to line-riga
                 perform STAMPA-RIGA
                 
                 move tstt-qta-kg to trs-qta-kg
            
                 add 1          to tot-bolle
                 add trs-qta-kg to tot-kg
                 set trovato to true

              end-perform
              if tutto-ok
                 perform STAMPA-TOTALI
              end-if

           end-if.
           close lineseq.

      ***---
       INTESTAZIONE.
           initialize line-riga
           write line-riga from st-line-riga.

           initialize line-riga
           move sta-data(3:2) to data6(5:2)
           move sta-data(5:2) to data6(3:2)
           move sta-data(7:2) to data6(1:2)
           move data6         to int-data

           write line-riga from r-intesta.

           initialize line-riga
           write line-riga from st-line-riga.

           move 3 to num-righe.

      ***---
       STAMPA-TOTALI.
           initialize line-riga
           move st-line-riga to line-riga
           perform STAMPA-RIGA.

           initialize line-riga
           move tot-bolle to r-tot-bolle
           move tot-kg    to r-tot-kg
           move r-totali  to line-riga
           perform STAMPA-RIGA.

           initialize line-riga
           move st-line-riga to line-riga
           perform STAMPA-RIGA.

      ***---
       SALTO-PAGINA.
      *     compute diff-righe = max-righe - num-righe.
      *     move diff-righe to n-vuote
      *     perform RIGHE-VUOTE  
           move 0 to num-righe.
           write line-riga from spaces after page.
           write line-riga from x"09"  after 1.

      ***---
       RIGHE-VUOTE.
           perform n-vuote times
              write line-riga from spaces
           end-perform.

      ***---
       STAMPA-RIGA.
           initialize sav-riga
           move line-riga to sav-riga
           if num-righe > max-righe - 5
              perform SALTO-PAGINA
              perform INTESTAZIONE
           end-if
           move sav-riga to line-riga
           write line-riga
           add 1 to num-righe. 

      ***---
       CLOSE-FILES.
           close trasporti clienti tvettori tmp-stt-day.
           delete file tmp-stt-day.
           if not trovato
              move spaces to wstampa
           end-if.

      ***---
       EXIT-PGM.
           move wstampa to sta-path.
           goback.
