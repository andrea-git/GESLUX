       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      opti-dhl.
       AUTHOR.                          Andrea.
       REMARKS.  Stampa lista ordini per DHL.
      ******************************************************************
       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tordini.sl".
           copy "rordini.sl".
           copy "line-dhl.sl".
           copy "clienti.sl".
           copy "destini.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tordini.fd".
           copy "rordini.fd".
           copy "line-dhl.fd".
           copy "clienti.fd".
           copy "destini.fd".

       WORKING-STORAGE SECTION.
      * COSTANTI
       78  titolo               value "Creazione lista DHL".

      * FILE STATUS AND VARIABLES
       77  status-line-dhl       pic x(2).
       77  status-tordini        pic x(2).
       77  status-rordini        pic x(2).
       77  status-clienti        pic x(2).
       77  status-destini        pic x(2).
       77  path-dhl              pic x(256).

      * FLAGS
       01  filler                pic 9.
         88 trovato              value 1, false 0.

       01  controlli             pic xx.
         88 tutto-ok             value "OK".
         88 errori               value "ER".

      * VARIABILI
       77  tot-peso              pic 9(9)v999.
       77  tot-colli             pic 9(5).
       77  como-riga             pic x(80).

       77  como-ora              pic 9(8).
       77  como-data             pic 9(8).

       77  tot-testate           pic 9(5).
       77  tot-testate-valide    pic 9(5).

       77  tot-testate-edit        pic zz.zz9.
       77  tot-testate-valide-edit pic zz.zz9.

       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.

       01  r-inizio              pic x(25).

      * VARIABILI DI CHAINING
       77  start-data           pic 9(8).
       77  end-data             pic 9(8).
       77  vettore              pic 9(5).
       77  path-fisso-txt       pic x(256).

      ******************************************************************
       PROCEDURE DIVISION CHAINING start-data 
                                   end-data 
                                   vettore 
                                   path-fisso-txt.

       DECLARATIVES.
      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TORDINI] non trovato!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TORDINI] indexed file corrupt!" 
                       delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TORDINI] File locked!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TORDINI] record locked!" delimited size
                       " P-KEY: " tor-chiave           delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RORDINI] non trovato!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RORDINI] indexed file corrupt!" 
                       delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RORDINI] File locked!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RORDINI] record locked!" delimited size
                       " P-KEY: " ror-chiave           delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [CLIENTI] non trovato!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [CLIENTI] mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [CLIENTI] indexed file corrupt!" 
                       delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [CLIENTI] File locked!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [CLIENTI]"                delimited size
                       " P-KEY: " cli-chiave           delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.  

      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [DESTINI] non trovato!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [DESTINI] non trovato!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TORDINI] non trovato!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [DESTINI] File locked!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [DESTINI] record locked!" delimited size
                       " P-KEY: " des-chiave           delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       LINE-DHL-ERR SECTION.
           use after error procedure on line-dhl.
           set tutto-ok  to true.
           evaluate status-line-dhl
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "File TXT [LINE-DHL] non trovato!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "File TXT [LINE-DHL] non trovato!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "File TXT [LINE-DHL] non trovato!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "93"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "File TXT [LINE-DHL] File locked!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "99"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [LINE-DHL] record locked!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
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
           move 0 to tot-testate tot-testate-valide.
           move path-fisso-txt to path-dhl.
           set tutto-ok        to true.
           set trovato         to false.

           perform SETTA-INIZIO-RIGA.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute start-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
           
           initialize como-riga.
           string r-inizio              delimited size
                  "INIZIO ELABORAZIONE" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

      ***---
       OPEN-FILES.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                        delimited size
                  "APERTURA FILES "               delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           open input tordini rordini clienti destini.
           if tutto-ok

              call "C$DELETE" using path-fisso-txt, "S"
              open output line-dhl
              if errori
                 close tordini rordini clienti destini

                 perform SETTA-INIZIO-RIGA
                 initialize como-riga
                 string r-inizio                        delimited size
                        "APERTURA FILES NON ESEGUITA"   delimited size
                        into como-riga
                 end-string
                 display como-riga upon syserr

              else
                                     
                 perform SETTA-INIZIO-RIGA
                 initialize como-riga
                 string r-inizio                        delimited size
                        "APERTURA FILES ESEGUITA"       delimited size
                        into como-riga
                 end-string
                 display como-riga upon syserr

              end-if
           end-if.
      
      ***---
       ELABORAZIONE.
           initialize tor-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           set tor-bolla-si-prenotata to true.
           move start-data(1:4) to tor-anno-bolla.
           move start-data      to tor-data-bolla.
           start tordini key is >= k3
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end     exit perform end-read
                    if tor-data-bolla > end-data exit perform end-if
                    add 1 to tot-testate
                    if tor-vettore = vettore
                       add 1 to tot-testate-valide
                       perform TROVA-DATI-CONSEGNA
                       set trovato to true
                       perform LOOP-RORDINI
                       perform SCRIVI-LINE-DHL
                    end-if
                 end-perform
                 if trovato
                    move all "9" to dhl-rec
                    write dhl-rec
                 end-if
           end-start.

      ***---
       TROVA-DATI-CONSEGNA.
           initialize cli-rec des-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces.
           if tor-prg-destino = 0
              set cli-tipo-C to true
              move tor-cod-cli to cli-codice
              read clienti no lock 
                   invalid 
                   move "** CLIENTE NON TROVATO **" to cli-ragsoc-1
              end-read
           else
              move tor-cod-cli     to des-codice
              move tor-prg-destino to des-prog
              read destini no lock
                   invalid
                   move "** DESTINO NON TROVATO **" to cli-ragsoc-1
               not invalid
                   move des-ragsoc-1  to cli-ragsoc-1
                   move des-indirizzo to cli-indirizzo
                   move des-localita  to cli-localita
                   move des-prov      to cli-prov
                   move des-cap       to cli-cap
              end-read
           end-if.

      ***---
       LOOP-RORDINI.
           move 0 to tot-colli tot-peso.
           move low-value  to ror-rec.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           start rordini key is >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    compute tot-peso = tot-peso +
                          ( ror-prg-peso * ror-qta )
                    compute tot-colli = tot-colli + ror-num-colli
                 end-perform
           end-start.

      ***--- 
       SCRIVI-LINE-DHL.
           initialize dhl-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move tor-num-bolla  to dhl-num-bolla.
           move tor-data-bolla to dhl-data-bolla.
           move cli-ragsoc-1   to dhl-ragsoc-c.
           move cli-indirizzo  to dhl-indirizzo-c.
           move cli-localita   to dhl-localita-c.
           move cli-prov       to dhl-prov-c.
           move cli-cap        to dhl-cap-c.
           move tot-colli      to dhl-tot-colli.
           move tot-peso       to dhl-tot-peso.
           move tor-numero     to dhl-num-evasione.
           write dhl-rec.

      ***---
       CLOSE-FILES.
           close tordini rordini line-dhl clienti destini.
           if not trovato
              delete file line-dhl
           end-if.

      ***---
       EXIT-PGM.
           perform SETTA-INIZIO-RIGA.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
           compute tot-secondi = end-secondi - start-secondi.
           initialize como-riga.
           move tot-testate        to tot-testate-edit.
           move tot-testate-valide to tot-testate-valide-edit.
           string r-inizio                        delimited size
                  "ELABORATA(E) "                 delimited size
                  tot-testate-edit                delimited size
                  " TESTATA(E) DI CUI VALIDA(E) " delimited size
                  tot-testate-valide-edit         delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           initialize como-riga.

           if tot-secondi < 60
              move tot-secondi to ss
              string r-inizio                      delimited size
                     "ELABORAZIONE TERMINATA IN: " delimited size
                     ss                            delimited size
                     " SECONDI"                    delimited size
                     into como-riga
              end-string
           else
              divide tot-secondi by 60 giving mm remainder ss
              string r-inizio                      delimited size
                     "ELABORAZIONE TERMINATA IN: " delimited size
                     mm                            delimited size
                     " MINUTI E "                  delimited size
                     ss                            delimited size
                     " SECONDI"                    delimited size
                     into como-riga
              end-string
           end-if.
           display como-riga upon syserr.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "setta-inizio-riga.cpy".
