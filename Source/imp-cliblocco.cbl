       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-cliblocco.
       AUTHOR.                          Andrea.
       REMARKS.
           Data una lista di codici su file csv (dentro la cartella Archivi)
           imposta lo stato di bloccato con causale "Blocco amministrativo"

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "clienti.sl".
           copy "lineseq.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "lineseq.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Blocco amministrativo clienti".

      * FILE-STATUS
       77  status-clienti            pic xx.
       77  status-lineseq            pic xx.
       77  wstampa                   pic x(256).

      * VARIABILI
       77  num-rec                   pic 9(5).

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
                move   "CLIENTI"   to geslock-nome-file
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
           move "bloccocli.csv" to wstampa.
           inspect wstampa replacing trailing low-value by spaces.

      ***---
       OPEN-FILES.
           open i-o clienti.
           if tutto-ok
              open input lineseq
              if errori
                 close clienti
              end-if
           end-if.

           if errori goback end-if.

      ***---
       ELABORAZIONE.
           move 0 to num-rec.
           perform until 1 = 2 

              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              unstring line-riga
                       delimited by ";"
                       into cli-codice
              end-unstring
              set cli-tipo-C to true
              read clienti no lock
                   invalid continue
               not invalid
                   set cli-bloccato   to true
                   set cli-blocco-amm to true
                   accept cli-ora-ultima-modifica  from time
                   accept cli-data-ultima-modifica from century-date
                   rewrite cli-rec 
                           invalid continue 
                       not invalid add 1 to num-rec
                   end-rewrite
              end-read
           end-perform.
           display message "Operazione terminata!"
                    x"0d0a""Aggiornati " num-rec, " clienti"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close clienti lineseq.

      ***---
       EXIT-PGM.
           goback.
