       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      LAB-IMP-EAN.
       AUTHOR.                          Andrea.
       REMARKS.                         
           Importazione codici EAN da "LAB". Viene lanciato una volta 
           sola dopodiché la gestione prosegue all'interno del GESLUX.
           Il match col file passato da "LAB" viene fatto per descrizione.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "lineseq.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "lineseq.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Importazione codici EAN".

      * FILE-STATUS
       77  status-articoli           pic xx.
       77  status-lineseq            pic xx.
       77  wstampa                   pic x(256).

      * VARIABILI
       77  ean-1                     pic x(20).
       77  ean-2                     pic x(20).
       77  ean-3                     pic x(20).
       77  ean-4                     pic x(20).
       77  ean-5                     pic x(20).
       77  num-rec                   pic 9(5).
       77  counter                   pic 9(10).
       77  counter2                  pic 9(10).
       77  counter-edit              pic z(10).       

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

      *****************************************************************

       LINKAGE SECTION.
       01  link-handle    handle of window.
       01  link-user      pic x(10).

       PROCEDURE DIVISION USING link-handle link-user.

       DECLARATIVES.
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
                move   "articoli"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     open i-o articoli allowing readers
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
           move 0 to counter counter2.
           set tutto-ok to true.
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa           delimited low-value
                   "codici_ean.csv"  delimited size
                   into wstampa
           end-string.
           inspect wstampa replacing trailing low-value by spaces.

      ***---
       OPEN-FILES.
           open i-o articoli allowing readers.
           if tutto-ok
              open input lineseq
              if errori
                 close articoli
              end-if
           end-if.

           if errori goback end-if.

      ***---
       ELABORAZIONE.
           move 0 to num-rec.
           perform until 1 = 2 
              add 1 to counter
              add 1 to counter2
              if counter2 = 50
                 move counter to counter-edit
                 display counter-edit
                    upon link-handle at column 14 line 07
                 move 0 to counter2
              end-if

              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              unstring line-riga
                       delimited by ";"
                       into art-descrizione
                            ean-1
                            ean-2
                            ean-3
                            ean-4
                            ean-5
              end-unstring
              read articoli key is art-k1
                   invalid continue
               not invalid
                   call "C$JUSTIFY" using ean-1, "R"
                   call "C$JUSTIFY" using ean-2, "R"
                   call "C$JUSTIFY" using ean-3, "R"
                   call "C$JUSTIFY" using ean-4, "R"
                   call "C$JUSTIFY" using ean-5, "R"
                   inspect ean-1 replacing leading x"20" by x"30"
                   inspect ean-2 replacing leading x"20" by x"30"
                   inspect ean-3 replacing leading x"20" by x"30"
                   inspect ean-4 replacing leading x"20" by x"30"
                   inspect ean-5 replacing leading x"20" by x"30"
                   move ean-1 to art-codice-ean-1
                   move ean-2 to art-codice-ean-2
                   move ean-3 to art-codice-ean-3
                   move ean-4 to art-codice-ean-4
                   move ean-5 to art-codice-ean-5
                   move   link-user to art-utente-ultima-modifica
                   accept art-ora-ultima-modifica  from century-date
                   accept art-data-ultima-modifica from century-date
                   rewrite art-rec 
                           invalid continue 
                       not invalid add 1 to num-rec
                   end-rewrite
              end-read
           end-perform.
           display message "Operazione terminata!"
                    x"0d0a""Aggiornati " num-rec, " articoli"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close articoli lineseq.

      ***---
       EXIT-PGM.
           display "                                                   "
              upon link-handle at column 14 line 07.
           goback.
