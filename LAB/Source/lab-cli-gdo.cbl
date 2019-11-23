       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      LAB-CLI-GDO.
       AUTHOR.                          Andrea.
       REMARKS.
           Rettifica le associazioni Cliente - Gruppo GDO attuali 
           in base a quelle indicate in un file di input

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "tgrupgdo.sl".
           copy "lineseq.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "tgrupgdo.fd".
           copy "lineseq.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Importazione associazioni Clienti - GDO".

      * FILE-STATUS
       77  status-clienti            pic xx.
       77  status-tgrupgdo           pic xx.
       77  status-lineseq            pic xx.
       77  wstampa                   pic x(256).

      * VARIABILI
       77  num-rec                   pic 9(5) value 0.
       77  tot-rec                   pic 9(5) value 0.

       77  r-cliente                 pic 9(5).
       77  r-gdo                     pic x(5).

      * FLAGS
       01  controlli                 pic xx.
         88 tutto-ok                 value "OK".
         88 errori                   value "ER".

      *****************************************************************

       PROCEDURE DIVISION.

       DECLARATIVES.
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                set errori to true
                display message "File [CLIENTI] not found!"
                          title titolo
                           icon 3
           when "39"
                set errori to true
                display message "File [CLIENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CLIENTI] Indexed file corrupt!"
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
                     open i-o clienti allowing readers
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

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
           set tutto-ok to true.
           accept  wstampa from environment "PATH_ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa       delimited low-value
                   "cli-gdo.csv" delimited size
                   into wstampa
           end-string.
           inspect wstampa replacing trailing low-value by spaces.

      ***---
       OPEN-FILES.
           open i-o clienti allowing readers.
           if tutto-ok
              open input lineseq tgrupgdo
              if errori
                 close clienti
              end-if
           end-if.

           if errori goback end-if.

      ***---
       ELABORAZIONE.
           move 0 to num-rec tot-rec.
           perform until 1 = 2 

              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              unstring line-riga
                       delimited by ";"
                       into r-cliente
                            r-gdo
              end-unstring
              set cli-tipo-C to true
              move r-cliente to cli-codice
              read clienti no lock
                   invalid continue
               not invalid
                   add 1 to tot-rec
                   if cli-gdo not = r-gdo
                      move r-gdo to gdo-codice
                      read tgrupgdo no lock
                           invalid
                           display message r-gdo, " NON VALIDO"
                       not invalid
                           move r-gdo to cli-gdo
                           rewrite cli-rec
                           add 1 to num-rec
                      end-read
                   end-if
              end-read
           end-perform.

           display message "Operazione terminata!"
                    x"0d0a""Aggiornati " num-rec, " clienti"
                    x"0d0a""su " tot-rec, " totali"
                     title titolo.

      ***---
       CLOSE-FILES.
           close clienti tgrupgdo lineseq.

      ***---
       EXIT-PGM.
           goback.
