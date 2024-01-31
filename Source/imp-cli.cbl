       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      imp-cli.
       AUTHOR.                          Andrea.
       REMARKS.
           Data una lista di codici su file csv (dentro la cartella Archivi)
           scribe i clienti e crea un destino (1) con gli stessi dati

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.          
           copy "clienti.sl".
           copy "destini.sl".
           copy "lineseq.sl".

      *****************************************************************

       DATA DIVISION.
       FILE SECTION.    
           copy "clienti.fd".
           copy "destini.fd".
           copy "lineseq.fd".

      *****************************************************************

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".

      * COSTANTI
       78  titolo value "Blocco amministrativo clienti".

      * FILE-STATUS     
       77  status-destini            pic xx.
       77  status-clienti            pic xx.
       77  status-lineseq            pic xx.
       77  wstampa                   pic x(256).

      * VARIABILI
       77  num-rec                   pic 9(5) value 0.
       77  last-cli                  pic 9(5) value 0.

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
           move "clienti.csv" to wstampa.
           inspect wstampa replacing trailing low-value by spaces.

      ***---
       OPEN-FILES.
           open i-o clienti destini.
           if tutto-ok
              open input lineseq
              if errori
                 close clienti destini
              end-if
           end-if.

           if errori goback end-if.

      ***---
       ELABORAZIONE.
           move high-value to cli-rec.
           set cli-tipo-c to true.
           start clienti key <= cli-chiave
                 invalid continue
             not invalid read clienti previous
           end-start.
           move cli-codice to last-cli.

           read lineseq next |Salto intestazione

           move 0 to num-rec.
           perform until 1 = 2 

              initialize line-riga
              read lineseq next at end exit perform end-read
              if line-riga = spaces    exit perform end-if
              initialize cli-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              add 1 to last-cli
              move last-cli to cli-codice
              set cli-tipo-c to true
              unstring line-riga
                       delimited by ";"
                       into 
                       cli-agente        
                       cli-ragsoc-1   
                       cli-piva       
                       cli-codfis
                       cli-localita   
                       cli-indirizzo  
                       cli-codice-SDI 
                       cli-cap        
                       cli-prov       
                       cli-nazione    
                       cli-tipo       
                       cli-gdo        
              end-unstring
              move "B01" to cli-pag
              set cli-esigibilita-iva-immediata to true
              set cli-tipo-art-diretti          to true
              set cli-attivo                    to true
              accept cli-ora-creazione  from time
              accept cli-data-creazione from century-date
              move "IMP-CLI" to cli-utente-creazione

              perform until 1 = 2
                 write cli-rec
                       invalid add 1 to last-cli
                               move last-cli to cli-codice
                   not invalid 
                       add 1 to num-rec
                       initialize des-rec 
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move cli-codice         to des-codice
                       move 1                  to des-prog
                       move cli-ragsoc-1       to des-ragsoc-1
                       move cli-piva           to des-piva    
                       move cli-localita       to des-localita
                       move cli-indirizzo      to des-indirizzo
                       move cli-cap            to des-cap
                       move cli-prov           to des-prov
                       move cli-nazione        to des-nazione
                       move cli-ora-creazione  to cli-ora-creazione 
                       move cli-data-creazione to cli-data-creazione
                       move "IMP-CLI"          to des-utente-creazione
                       set  des-attivo            to true
                       set  des-tipo-art-diretti  to true
                       set  des-no-invio          to true

                       write des-rec end-write
                       exit perform
                 end-write
              end-perform
           end-perform.
           display message "Operazione terminata!"
                    x"0d0a""Importati " num-rec, " clienti"
                     title titolo.

      ***---                   
       CLOSE-FILES.
           close clienti destini lineseq.

      ***---
       EXIT-PGM.
           goback.
