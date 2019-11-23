       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      listmov-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "anautf.sl". 
           copy "movutf.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "anautf.fd". 
           copy "movutf.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           copy "link-geslock.def".
           copy "comune.def".

       78  titolo value "Lista Entrate/Uscite per totali giornalieri".
       78  MaxRows                 value 65.

       77  status-anautf           pic x(2).
       77  status-movutf           pic x(2).
       77  status-lineseq          pic x(2).
       77  wstampa                 pic x(256).
       
       77  iniziale                pic s9(12)v999.
       77  finale                  pic s9(12)v999.
       77  tot-entrata             pic s9(12)v999 value 0.
       77  tot-uscita              pic s9(12)v999 value 0.
       77  totale-entrata          pic s9(12)v999 value 0.
       77  totale-uscita           pic s9(12)v999 value 0.
       77  RowCounter              pic 9(3) value 0.
       77  save-data               pic 9(8) value 0.
       77  como-data               pic 9(8).
       77  como-ora                pic 9(8).

       01  filler                  pic 9.
         88 record-ok              value 1, false 0.

       01  filler                  pic 9.
         88 prima-volta            value 1, false 0.

      * RIGHE PER LA STAMPA
       01  intestazione-77         pic x(77) value "MOVIMENTAZIONE UTF".
       01  intestazione-54         pic x(54) value "MOVIMENTAZIONE UTF".

       01  titolo-2.
         03 filler                 pic x(12) value "Registro n.:".
         03 filler                 pic x(1).
         03 t2-num-reg             pic z(8).
         03 filler                 pic x(2).
         03 t2-desc1               pic x(30).

       01  r-titolo.
         03 filler                 pic x.
         03 filler                 pic x(6)  value "Giorno".
         03 filler                 pic x(13).
         03 filler                 pic x(11) value "Entrate Kg.".
         03 filler                 pic x(13).
         03 filler                 pic x(10) value "Uscite Kg.". 
         03 filler                 pic x(14).
         03 tit-giacenza           pic x(8).

       01  r-riga.
         03 r-data                 pic x(8).
         03 filler                 pic x(3).
         03 r-entrata              pic zzz.zzz.zzz.zz9,999- blank zero.
         03 filler                 pic x(3).
         03 r-uscita               pic zzz.zzz.zzz.zz9,999- blank zero.
         03 filler                 pic x(3).
         03 r-giacenza             pic zzz.zzz.zzz.zz9,999- blank zero.

       01  r-totali.
         03 filler                 pic x(11) value "T O T A L I".
         03 r-tot-entrata          pic zzz.zzz.zzz.zz9,999-.
         03 filler                 pic x(3).
         03 r-tot-uscita           pic zzz.zzz.zzz.zz9,999-.
         03 filler                 pic x(3).
         03 r-tot-giacenza         pic zzz.zzz.zzz.zz9,999- blank zero.

       LINKAGE SECTION.
       copy "link-listmov.def".

      ******************************************************************
       PROCEDURE DIVISION using listmov-linkage.

       DECLARATIVES.

      ***---
       ANAUTF-ERR SECTION.
           use after error procedure on anautf.
           set tutto-ok  to true.
           evaluate status-anautf
           when "35"
                display message "File [ANAUTF] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [ANAUTF] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[ANAUTF] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       MOVUTF-ERR SECTION.
           use after error procedure on movutf.
           set tutto-ok  to true.
           evaluate status-movutf
           when "35"
                display message "File [MOVUTF] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [MOVUTF] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[MOVUTF] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                set errori to true
                display message "File not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "Indexed file corrupt!"
                          title titolo
                           icon 3
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

       END DECLARATIVES.

      ***---
       MAIN-PRG.
      *     move 0 to tot-uscita.
      *     open i-o movutf.
      *     move low-value to mov-rec.
      *     start movutf key is >= mov-chiave.
      *     perform until 1 = 2
      *        read movutf next
      *        if mov-data > 20050607 exit perform end-if
      *        if mov-uscita
      *           add mov-kg to tot-uscita
      *        end-if
      *     end-perform.
      *     close movutf
      *     display message tot-uscita
      *     exit program.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set RecLocked   to false.
           set tutto-ok    to true.
           set trovato     to false.
           set prima-volta to true.

           initialize wstampa.
           accept     como-data from century-date.
           accept     como-ora  from time.
           accept     wstampa   from environment "PATH_ST".
           inspect    wstampa   replacing trailing spaces by low-value.
           string     wstampa   delimited low-value
                      "listmov" delimited size
                      "_"       delimited size
                      como-data delimited size
                      "_"       delimited size
                      como-ora  delimited size
                      ".txt"    delimited size
                      into wstampa
           end-string.
           if listmov-data-from-anautf
              compute como-data = 
                      function INTEGER_OF_DATE(listmov-data-from)
              add 1 to como-data
              compute listmov-data-from = 
                      function DATE_OF_INTEGER(como-data)
           end-if.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              open input anautf movutf
              if errori
                 close       lineseq
                 delete file lineseq
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.
      
      ***---
       ELABORAZIONE.
           move low-value to mov-rec.
           move listmov-data-from(1:4) to mov-anno.
           move listmov-num-reg        to mov-num-reg.
           move listmov-data-from      to mov-data.
           start movutf key is >= k-data
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read movutf next at end exit perform end-read
                 if mov-anno not = listmov-data-from(1:4)
                    exit perform
                 end-if
                 if mov-num-reg not = listmov-num-reg
                    exit perform
                 end-if
                 if mov-data > listmov-data-to
                    exit perform
                 end-if
                 if prima-volta
                    perform SCRIVI-INTESTAZIONE
                    if listmov-data-from-anautf
                       perform SCRIVI-RIGA-GIACENZA-INIZIALE
                    end-if
                    set prima-volta to false
                 end-if
                 if save-data    not = mov-data
                    if save-data not = 0
                       if tot-entrata + tot-uscita not = 0
                          perform VALORIZZA-RIGA
                       end-if
                       move 0 to tot-entrata tot-uscita
                    end-if
                    move mov-data to save-data
                 end-if
                 if mov-kg not = 0
                    set trovato to true
                    if mov-entrata
                       add mov-kg to tot-entrata totale-entrata
                    else
                       add mov-kg to tot-uscita  totale-uscita
                    end-if
                 end-if
              end-perform
           end-if.

           if trovato
              if tot-entrata + tot-uscita not = 0
                 perform VALORIZZA-RIGA
              end-if
              if totale-entrata + totale-uscita not = 0
                 perform SCRIVI-TOTALI
              end-if
           else
              display message "Nessun movimento UTF da elaborare!"
                        title titolo
                         icon 2
           end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           if listmov-data-from-anautf
              call "C$JUSTIFY" using intestazione-77, "C"
              move intestazione-77 to line-riga
              write line-riga
              add 1 to RowCounter
           else
              call "C$JUSTIFY" using intestazione-54, "C"
              move intestazione-54 to line-riga
              write line-riga
              add 1 to RowCounter
           end-if.
           move listmov-data-from(1:4) to utf-anno.
           move listmov-num-reg        to utf-num-reg.
           read anautf no lock
                invalid continue
            not invalid
                move utf-desc1 to t2-desc1
           end-read.

           move listmov-num-reg to t2-num-reg.

           move titolo-2 to line-riga.
           write line-riga.
           add 1 to Rowcounter.       

           write line-riga from spaces.
           add 1 to Rowcounter.

           perform SCRIVI-TITOLO.

      ***---
       SCRIVI-TITOLO.
           if listmov-data-from-anautf
              move all "-" to line-riga(1:77)
              write line-riga
              add 1 to RowCounter
              move "Giacenza" to tit-giacenza
           else
              move all "-" to line-riga(1:54)
              write line-riga
              add 1 to RowCounter
              move spaces    to tit-giacenza
           end-if.
           write line-riga from r-titolo.
           add 1 to RowCounter.
           if listmov-data-from-anautf
              move all "-" to line-riga(1:77)
              write line-riga
              add 1 to RowCounter
           else                              
              move all "-" to line-riga(1:54)
              write line-riga
              add 1 to RowCounter
           end-if.

      ***---
       SCRIVI-RIGA-GIACENZA-INIZIALE.
           compute iniziale = utf-giac-bianchi  +
                              utf-giac-petrolio +
                              utf-giac-gasolio  +
                              utf-giac-lubrif.
           string utf-data-ult-stampa(7:2) delimited size
                  "/"                      delimited size
                  utf-data-ult-stampa(5:2) delimited size
                  "/"                      delimited size
                  utf-data-ult-stampa(3:2) delimited size
                  into r-data
           end-string.
           move spaces   to r-entrata.
           move spaces   to r-uscita. 
           move iniziale to r-giacenza.
           move r-riga   to line-riga.
           write line-riga.
           add 1 to RowCounter.

      ***---
       VALORIZZA-RIGA. 
           if RowCounter >= MaxRows
              perform SALTO-PAGINA
              perform SCRIVI-TITOLO
           end-if.
           initialize r-riga.
           string save-data(7:2) delimited size
                  "/"            delimited size
                  save-data(5:2) delimited size
                  "/"            delimited size
                  save-data(3:2) delimited size
                  into r-data
           end-string.
           move tot-entrata to r-entrata.
           move tot-uscita  to r-uscita. 
           move r-riga      to line-riga.
           write line-riga.
           add 1 to RowCounter.

      ***---
       SCRIVI-TOTALI.
           if RowCounter > MaxRows - 4
              perform SALTO-PAGINA
           end-if.
           write line-riga from spaces.
           add 1 to RowCounter.
           if listmov-data-from-anautf
              move all "-" to line-riga(1:77)
              write line-riga
              add 1 to RowCounter
           else                              
              move all "-" to line-riga(1:54)
              write line-riga
              add 1 to RowCounter
           end-if.
           move totale-entrata to r-tot-entrata.
           move totale-uscita  to r-tot-uscita.
           if listmov-data-from-anautf
              compute finale = iniziale       + 
                               totale-entrata -
                               totale-uscita
              move finale         to r-tot-giacenza link-finale
              write line-riga   from r-totali
              add 1 to RowCounter
              move all "-" to line-riga(1:77)
              write line-riga
              add 1 to RowCounter
           else   
              write line-riga   from r-totali
              add 1 to RowCounter                           
              move all "-" to line-riga(1:54)
              write line-riga
              add 1 to RowCounter
           end-if.

      ***---
       SALTO-PAGINA.
           write line-riga from spaces after page.
           move 0 to RowCounter.

      ***---
       CLOSE-FILES.
           close lineseq anautf movutf.
           if not trovato
              delete file lineseq
              move spaces to wstampa
           end-if.

      ***---
       EXIT-PGM.
           move wstampa to listmov-path.
           goback.
