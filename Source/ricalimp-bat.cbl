       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      ricalimp-bat.
       AUTHOR.                          Andrea.
       REMARKS. Ricalcolo impegnato da:
                - note credito non fatturate
                - ordini inevasi

                - master (maggiore tra ord e eva) 
                  non chiusi (aggiornamento stato, pezzi e prezzi)
                
                NON VIENE LANCIATO IN CASO DI EVASIONE CLIENTI IN CORSO
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.          
           copy "tsetinvio.sl".
           copy "mtordini.sl".
           copy "mrordini.sl".
           copy "tordini.sl".
           copy "rordini.sl". 
           copy "progmag.sl".
           copy "lineseq.sl".
           copy "clienti.sl".
           copy "destini.sl".
           copy "ttipocli.sl".
           copy "articoli.sl".
           copy "tcaumag.sl".
           copy "multigest.sl".

       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.          
           copy "tsetinvio.fd".
           copy "mtordini.fd".
           copy "mrordini.fd".
           copy "tordini.fd".
           copy "rordini.fd". 
           copy "progmag.fd".
           copy "lineseq.fd".
           copy "clienti.fd".
           copy "destini.fd".
           copy "ttipocli.fd".
           copy "articoli.fd".
           copy "tcaumag.fd".
           copy "multigest.fd".

       FD  lineseq1.
       01 line-riga        PIC  x(900).

       WORKING-STORAGE SECTION.
           copy "link-wprogmag.def".
           copy "link-geslock.def".
           copy "mail.def".

       78  user-codi             value "BATCH".
       78  titolo                value"Batch Ricalcolo impegnato". 
       77  status-progmag        pic xx.
       77  status-tsetinvio      pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-clienti        pic xx.
       77  status-destini        pic xx.
       77  status-ttipocli       pic xx.
       77  status-tscorte        pic xx.
       77  status-articoli       pic xx.
       77  status-tcaumag        pic xx.
       77  status-lineseq        pic xx.
       77  status-multigest      pic xx.
       77  wstampa               pic x(256).   
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).

       01  r-inizio.
         05 filler              pic x(2)  value " [".
         05 r-data.
            10 r-gg             pic xx.
            10 filler           pic x     value "/".
            10 r-mm             pic xx.
            10 filler           pic x     value "/".
            10 r-aa             pic xx.
         05 filler              pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh             pic xx.
            10 filler           pic x     value X"22".
            10 r-min            pic xx.
            10 filler           pic x     value "'".
            10 r-sec            pic xx.
         05 filler              pic x(2)  value "] ".

       77  start-secondi         pic 9(18).
       77  end-secondi           pic 9(18).
       77  tot-secondi           pic 9(18).
       77  hh                    pic 99.
       77  mm                    pic 99.
       77  ss                    pic 99.       
       77  tentativi             pic 99.

       77  counter               pic 9(9).
       77  counter2              pic 9(9).
       77  counter-edit          pic zzz.zzz.zz9.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8). 

       77  mese-oggi             pic 99.
       77  mese-consegna         pic 99.

       77  como-data-1           pic 9(8).
       77  como-data-2           pic 9(8).
       77  resto                 pic 9(3).
       77  diff-giorni           pic 9(5).

       77  link-data             pic 9(8).
       77  como-riga             pic x(80).

       01  como-rec              pic x(5000).

       77  filler            pic 9.
           88  nessun-errore value 1, false 0.

       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
           
       77  CosaElaborare         pic x.
           88  ElaboraGiacenza   value "G".
           88  ElaboraImpegnato  value "I".
      
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.
       01  GdoInUsoFlag          pic x.
           88 GdoInUso           value "S". 
           88 GdoNonInUso        value " ".

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.

      ***---
       MTORDINI-ERR SECTION.
           use after error procedure on mtordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mtordini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [MTORDINI] inesistente!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [MTORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[MTORDINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       MRORDINI-ERR SECTION.
           use after error procedure on mrordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-mrordini
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                       delimited size
                       "File [MRORDINI] inesistente!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                         delimited size
                       "File [MRORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                           delimited size
                       "[MRORDINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordini 
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [TORDINI] inesistente!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [TORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[TORDINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordini 
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [RORDINI] inesistente!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [RORDINI] mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[RORDINI] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                      delimited size
                       "File [PROGMAG] inesistente!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "39"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                        delimited size
                       "File [PROGMAG] mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "98"
                initialize como-riga
                perform SETTA-INIZIO-RIGA
                string r-inizio                          delimited size
                       "[PROGMAG] indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
                set nessun-errore to false
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       MULTIGEST-ERR SECTION.
           use after error procedure on multigest.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-multigest
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           perform CONTROLLA-ESEGUIBILITA.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set nessun-errore to true.
           display "Ricalcolo impegnato "
                   "progressivi di magazzino in corso...".
           set tutto-ok    to true.
           set prima-volta to true.
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
           set tutto-ok  to true.

      ***---
       OPEN-FILES.
           |Quando si consolida il magazzino NESSUNO deve essere
           |dentro al file dei movimenti (tranne che in visua) e mi
           |sembra il minimo dato che devo fare operazioni in massa

LUBEXX     |I files devono essere aperti in input. Corriamo il rischio
LUBEXX     |di elaborare intanto che ci lavorano, ma non importa
           open input tordini rordini clienti destini
                      ttipocli tcaumag mtordini mrordini.

           open i-o progmag multigest.

      ***---
       CONTROLLA-ESEGUIBILITA.
           move spaces to mul-chiave.
           read multigest lock invalid continue end-read.
           if RecLocked
              set errori to true
           end-if.

      ***---
       ELABORAZIONE.
           set mul-ricalcolo to true.
           rewrite mul-rec.
           read multigest lock.
           perform AZZERA-PROGMAG.
           perform ELABORA-ORDINI-MASTER.
           perform ELABORA-INEVASI-E-BOLLA-EMESSA-NON-FATTURATI.

      ***---
       AZZERA-PROGMAG.
           set tutto-ok to true.
           display "Azzeramento in corso...".
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                  delimited size
                  "AZZERAMENTO PROGRESSIVI" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           move 0 to counter counter2.

           move low-value to prg-rec.
           start progmag key is >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2 

                    read progmag next at end exit perform end-read

                    move 0 to prg-impegnato
                    move 0 to prg-imp-master
                    move 0 to prg-imp-TRAD
                    move 0 to prg-imp-GDO
                    rewrite prg-rec invalid continue end-rewrite

                 end-perform
           end-start.

           unlock progmag all records.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                       delimited size
                  "FINE AZZERAMENTO PROGRESSIVI" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

      ***---
       ELABORA-ORDINI-MASTER.
           set tutto-ok to true.
           display "Elaborazione impegnato master in corso...".

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                     delimited size
                  "ELABORAZIONE ORDINI MASTER" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           move low-value to mto-rec.
           set mto-registrato to true.
           start mtordini key >= k-mto-stato
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mtordini next at end exit perform end-read
                    move mto-rec to como-rec
                    if mto-chiuso
                       exit perform
                    end-if
                    perform LOOP-RIGHE-MRORDINI
      *****              set ricalcolo to true
      *****              perform AGGIORNA-STATO-MASTER
      *****              move como-rec to mto-rec
      *****              start mtordini key > k-mto-stato
      *****                    invalid  exit perform
      *****              end-start

                 end-perform
           end-start.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                          delimited size
                  "FINE ELABORAZIONE ORDINI MASTER" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

      ***---
       LOOP-RIGHE-MRORDINI.
           move mto-chiave to mro-chiave-testa.
           move low-value  to mro-riga.
           start mrordini  key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    if mro-chiuso
                       continue
                    else
                       perform AGGIORNA-IMPEGNATO-MASTER
                    end-if
                 end-perform
           end-start.
                 
      ***---     
       ELABORA-INEVASI-E-BOLLA-EMESSA-NON-FATTURATI.
           display "Elaborazione inevasi e bolla emessa "
                   "non fatturati in corso...".

           move 0 to counter counter2.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                         delimited size
                  "ELABORAZIONE INEVASI E BOLLA "  delimited size
                  "EMESSA NON FATTURATI"           delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           move 0 to tor-anno-fattura.
           move 0 to tor-num-fattura.

           start tordini key is >= k-fattura
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordini next at end exit perform end-read
                    if tor-data-fattura not = 0 or
                       tor-num-fattura  not = 0
                       exit perform
                    end-if

                    if tor-num-bolla  = 0 and
                       tor-data-bolla = 0
                       perform LOOP-RIGHE-RORDINI
                    end-if
                    
                    if errori exit perform end-if
                 end-perform
           end-start.
           perform SETTA-INIZIO-RIGA.
           display counter upon syserr.
           initialize como-riga.
           string r-inizio                   delimited size
                  "FINE ELABORAZIONE BOLLA " delimited size
                  "EMESSA NON FATTURATI"     delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

      ***---
       LOOP-RIGHE-RORDINI.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           move low-values to ror-num-riga.
           start rordini key is >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini  next at end exit perform end-read
                    if tor-anno   not = ror-anno    or
                       tor-numero not = ror-num-ordine
                       exit perform
                    end-if
                    move 0                  to link-impegnato
                    move "BATCH"            to link-user
                    move ror-prg-chiave     to link-key
                    evaluate true
                    when ElaboraGiacenza
                         |AGISCO SULLA GIACENZA (NORMALE E BLOCCATA)
                         move "1000000000000010"  to link-array
                    when ElaboraImpegnato
                         |AGISCO SULL'IMPEGNATO
                         move "0100000000000000"  to link-array
                    end-evaluate
                    move ror-qta               to link-valore
                    move tor-causale           to link-causale
                    move ror-prg-chiave        to link-key
                    set  link-update           to true
                    set  link-open-with-lock   to false
                    set  link-update-um        to true
                    set  link-update-peso      to false
                    set  link-update-valore    to false
                    set  link-chiamata-batch   to true
                    if prima-volta
                       set prima-volta to false
                       close progmag |wprogmag aprirà con lock il file!!!
                    end-if
                    call "wprogmag" using link-wprogmag
                    evaluate link-wprogmag-status
                    when -1
                         perform SETTA-INIZIO-RIGA
                         initialize como-riga
                         string r-inizio             delimited size
                                link-msg-err-ritorno delimited size
                                into como-riga
                         end-string
                         display como-riga upon syserr
                         set nessun-errore to false
                    when -2
                         perform SETTA-INIZIO-RIGA
                         inspect link-msg-err-ritorno replacing trailing
                                                     spaces by low-value
                         initialize como-riga
                         string r-inizio             delimited size
                                link-msg-err-ritorno delimited low-value
                                " --> IMPOSSIBILE "  delimited size
                                "PROSEGUIRE!"        delimited size
                                into como-riga
                         end-string
                         display como-riga upon syserr
                         set nessun-errore to false
                         exit perform
                    end-evaluate
                 end-perform
           end-start.

      ***----
       AGGIORNA-IMPEGNATO-MASTER.
           set  cli-tipo-C  to true.
           move mto-cod-cli to cli-codice.
           read clienti     no lock.

           |Ad aumentare l'impegnato sulle qta evase 
           |ci penseranno poi le evasioni
           if mro-qta > mro-qta-e
              compute link-valore = mro-qta - mro-qta-e
           else
              move 0       to link-valore
           end-if.
           move link-valore to link-impegnato.
           perform DIREZIONA-IMPEGNATO.

           move "BATCH"               to link-user.
           move "0100000000000000"    to link-array.
           move mto-causale           to link-causale.
           move mro-prg-chiave        to link-key.
           set  link-update           to true.
           set  link-open-with-lock   to false.
           set  link-update-um        to true.
           set  link-update-peso      to false.
           set  link-update-valore    to false.
           set  link-chiamata-batch   to true.
           if prima-volta
              set prima-volta to false
              close progmag |wprogmag aprirà con lock il file!!!
           end-if.
           call "wprogmag" using link-wprogmag.
           evaluate link-wprogmag-status
           when -1
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio             delimited size
                       link-msg-err-ritorno delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set nessun-errore to false
           when -2
                perform SETTA-INIZIO-RIGA
                inspect link-msg-err-ritorno replacing trailing
                                             spaces by low-value
                initialize como-riga
                string r-inizio             delimited size
                       link-msg-err-ritorno delimited low-value
                       " --> IMPOSSIBILE "  delimited size
                       "PROSEGUIRE!"        delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set nessun-errore to false
           end-evaluate.

      ***--
       CLOSE-FILES.
           unlock multigest all records.
           close mtordini mrordini clienti destini tcaumag multigest.

      ***---
       EXIT-PGM.
           perform SETTA-INIZIO-RIGA.
           move como-ora(1:2) to hh.
           move como-ora(3:2) to mm.
           move como-ora(5:2) to ss.

           compute end-secondi = ( hh * 3600 ) + ( mm * 60   ) + ss.
           compute tot-secondi = end-secondi - start-secondi.

           if tot-secondi < 60
              move tot-secondi to ss
              display "ELABORAZIONE TERMINATA IN: ",
                      ss, " SECONDI"
                 upon syserr
           else
              divide tot-secondi by 60 giving mm remainder ss
              display "ELABORAZIONE TERMINATA IN: ",
                      mm, " MINUTI E ", ss, " SECONDI"
                 upon syserr

           end-if.
      *****     perform INVIO-MAIL.
           goback.

      ***---
       INVIO-MAIL.
           display "Invio mail in corso...".

           move "INVIO MAIL IN CORSO..." to como-riga.
           perform SETTA-RIGA-STAMPA.

           initialize LinkBody.
           if nessun-errore
              move "RICALCOLO IMPEGNATO - OK" 
                to LinkSubject
           else
              move "RICALCOLO IMPEGNATO - ATTENZIONE" 
                to LinkSubject
           end-if.

           move "In allegato dettagli funzionamento programma" 
             to LinkBody.

           accept LinkAddress from environment "RICALDIN_ADDRESSES".


           accept FileOrig    from environment "RICALDIN_LOG".
           accept FileDest    from environment "RICALDIN_LOG_INVIO".
           call "C$COPY" using FileOrig, FileDest, "S".
           move FileDest to LinkAttach.

           set errori to true.
           move 0 to tentativi.
           move "ricalimp-bat" to NomeProgramma.
           perform 10 times
              add 1 to tentativi
              perform SEND-MAIL
              
              initialize como-riga
              if StatusInvioMail = -1
                 string r-inizio                      delimited size
                        "TENTATIVO N. "               delimited size
                        tentativi                     delimited size
                        ": "                          delimited size
                        "Chiamata InvioMail fallita!" delimited size
                        " STATUS -1"                  delimited size
                        into como-riga
                 end-string
              else
                 string r-inizio                       delimited size
                        "TENTATIVO N. "                delimited size
                        tentativi                      delimited size
                        ": "                           delimited size
                        "Chiamata InvioMail riuscita!" delimited size
                        into como-riga
                 end-string
              end-if
              perform SETTA-RIGA-STAMPA
                            
              call "C$DELETE" using FileDest
              open input lineseq1
              read  lineseq1 next
              if line-riga of lineseq1 = "True"
                 set tutto-ok to true
                 close lineseq1
                 exit perform
              end-if
              close lineseq1

              initialize como-riga
              string r-inizio              delimited size
                     "TENTATIVO N. "       delimited size
                     tentativi             delimited size
                     ": "                  delimited size
                     line-riga of lineseq1 delimited size
                     into como-riga
              end-string
              perform SETTA-RIGA-STAMPA

           end-perform
               
           initialize como-riga.
           if tutto-ok
              string r-inizio               delimited size
                     "INVIO MAIL RIUSCITO!" delimited size
                     into como-riga
              end-string
           else
              string r-inizio                   delimited size
                     "INVIO MAIL NON RIUSCITO!" delimited size
                     into como-riga
              end-string
           end-if.
           perform SETTA-RIGA-STAMPA.

           delete file lineseq.

      ***---
       SETTA-RIGA-STAMPA.
           perform SETTA-INIZIO-RIGA.
           display como-riga upon syserr.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "direziona-impegnato-common.cpy".
           copy "setta-inizio-riga.cpy".
