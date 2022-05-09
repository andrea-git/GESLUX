       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      calmar-p.
       AUTHOR.                          Andrea.
       REMARKS. Consolidamento movimenti aggiornando progmagric coi valori 
                indicati nella causale. Batch notturno che considera i
                movimenti dal giorno successivo al consolidamento
                al giorno stesso. Come valori di partenza verranno
                utilizzati quelli presenti nei progressivi consolidati
                ai quali verranno sommati/sottratti i nuovi movimenti.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "lineseq.sl".
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "progmagric.sl".
           copy "progmag.sl".
           copy "tparamge.sl".
           copy "articoli.sl".
           copy "tmarche.sl".
           copy "timposte.sl".

       select lineseq1
           assign       to  wstampa
           organization is line sequential
           access mode  is sequential
           file status  is status-lineseq.

       select rep-recupero
           assign       to path-rep-recupero
           organization is line sequential
           access mode  is sequential
           file status  is status-rep-recupero.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tsetinvio.fd".
           copy "lineseq.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd". 
           copy "progmagric.fd". 
           copy "progmag.fd".
           copy "tparamge.fd".
           copy "articoli.fd".
           copy "tmarche.fd".
           copy "timposte.fd".

       FD  lineseq1.
       01 line-riga        PIC  x(900).

       FD  rep-recupero.
       01 riga-recupero    pic x(100).

       WORKING-STORAGE SECTION.
           copy "link-wprogmag.def".
           copy "costo-medio.def".
           copy "imposte.def".
           copy "mail.def".

       01  r-inizio.
         05 filler                 pic x(2)  value " [".
         05 r-data.
            10 r-gg                pic xx.
            10 filler              pic x     value "/".
            10 r-mm                pic xx.
            10 filler              pic x     value "/".
            10 r-aa                pic xx.
         05 filler                 pic x(5)  value "] - [".
         05 r-ora.
            10 r-hh                pic xx.
            10 filler              pic x     value X"22".
            10 r-min               pic xx.
            10 filler              pic x     value "'".
            10 r-sec               pic xx.
         05 filler                 pic x(2)  value "] ".

       01 r-stampa.
         05 r-tipo         pic x.
         05 r-codice       pic z(6).
         05 filler         pic x(3) value " - ".
         05 r-descrizione  pic x(30).
         05 filler         pic x(2). 
         05 r-marca        pic x(25).
         05 filler         pic x(2).
         05 r-prz          pic ----.--9,99.
         05 filler         pic x(2).
         05 r-mese         pic x.

       77  status-tmovmag        pic xx.
       77  status-tsetinvio      pic xx.
       77  status-rmovmag        pic xx.
       77  status-progmag        pic xx.
       77  status-tparamge       pic xx.
       77  status-progmagric     pic xx.
       77  status-articoli       pic xx.
       77  status-timposte       pic xx.
       77  status-tmarche        pic xx.
       77  status-lineseq        pic xx.
       77  status-rep-recupero   pic xx.

       77  wstampa               pic x(256).
       77  path-rep-recupero     pic x(256) value spaces.
                                            
       77  FileDest              pic x(256).
       77  FileOrig              pic x(256).
       77  link-mese             pic 9(2) value 0.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  como-riga             pic x(100).
       77  riga-stampa           pic x(100).
       77  var1                  pic 9(4).
       77  anno-verifica         pic 9(4).

       77  counter               pic 9(9).
       77  counter2              pic 9(9).
       77  counter-edit          pic zzz.zzz.zz9.
       77  nargs                 pic 99 comp-1 value 0.

       01  fine-mese.
           05 anno               pic 9999.
           05 mese               pic 99.
           05 giorno             pic 99.
       77  data-rical            pic 9(8).
       77  tentativi             pic 99.

      * FLAGS
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88 trovato            value 1, false 0.
       77  filler                pic 9.
           88 prima-volta        value 1, false 0.
       77  filler                pic 9.
           88 trovato-movim      value 1, false 0.
       77  stato                 pic 9.
           88 nessun-errore      value 1.
           88 ok-recupero        value 2.
           88 errore-ko          value 3.

       01  filler                pic 9 value 0.
           88 RichiamoSchedulato       value 1, false 0.   
                                              
       77  st-tendenza-status    signed-short.
       77  st-delta-status       signed-short.
   
       LINKAGE SECTION.
       copy "link-batch.def".

      ******************************************************************
       PROCEDURE DIVISION USING batch-linkage.

       DECLARATIVES.
      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag 
           when "35"
                move "File [TMOVMAG] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [TMOVMAG] mismatch size!" to como-riga
                set errori to true
           when "98"
                move "[TMOVMAG] Indexed file corrupt!" to como-riga
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag 
           when "35"
                move "File [RMOVMAG] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [RMOVMAG] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[RMOVMAG] Indexed file corrupt!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
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
                move "File [PROGMAG] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [PROGMAG] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[PROGMAG] Indexed file corrupt!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       PROGMAGRIC-ERR SECTION.
           use after error procedure on progmagric.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmagric 
           when "35"
                move "File [PROGMAGRIC] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [PROGMAGRIC] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[PROGMAGRIC] Indexed file corrupt!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tparamge 
           when "35"
                move "File [TPARAMGE] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [TPARAMGE] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[TPARAMGE] Indexed file corrupt!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                move "File [ARTICOLI] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [ARTICOLI] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[ARTICOLI] Indexed file corrupt!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                move "File [TIMPOSTE] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [TIMPOSTE] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[TIMPOSTE] Indexed file corrupt!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                move "File [TMARCHE] inesistente" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "39"
                move "File [TMARCHE] mismatch size!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "98"
                move "[TMARCHE] Indexed file corrupt!" to como-riga
                perform SETTA-RIGA-STAMPA
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
              perform CHIAMA-PROGRAMMI
              if not RichiamoSchedulato
                 perform INVIO-MAIL
              end-if
           else
              set errore-ko to true
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set tutto-ok      to true.
           set nessun-errore to true.
           set prima-volta   to true.  

           CALL "C$NARG" USING NARGS.
           if nargs not = 0
              set RichiamoSchedulato to true
           else
              set RichiamoSchedulato to false
           end-if.
           if RichiamoSchedulato
              initialize wstampa
              accept como-data from century-date
              accept como-ora  from time
              accept  wstampa from environment "SCHEDULER_PATH_LOG"
              inspect wstampa replacing trailing spaces by low-value
              string  wstampa      delimited low-value
                      "CALMAR_"    delimited size
                      como-data    delimited size
                      "_"          delimited size
                      como-ora     delimited size
                      ".log"       delimited size
                      into wstampa
              end-string
              set RichiamoSchedulato to true
              move wstampa to batch-log
              open output lineseq
           end-if.
           move "INIZIO PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.

      ***---
       OPEN-FILES.
           move "APERTURA FILES" to como-riga.
           perform SETTA-RIGA-STAMPA.

           |Lanciando di notte non devo farte 
           |particolari controlli sul lock
           if tutto-ok
              open input tmovmag  rmovmag  progmag 
                         tparamge articoli tmarche timposte
              if tutto-ok
                 perform OPEN-OUTPUT-PROGMAGRIC-LOCK
                 if errori
                    close tmovmag  rmovmag  progmag 
                          tparamge articoli tmarche timposte
                 end-if
              end-if
           end-if.
           if errori
              move "APERTURA FILES NON RIUSCITA" to como-riga
           else
              move "APERTURA FILES RIUSCITA" to como-riga
           end-if.
           perform SETTA-RIGA-STAMPA.

      ***---
       OPEN-OUTPUT-PROGMAGRIC-LOCK.
           |Lo apro in lock per verificare che attualmente non ci 
           |sia dentro nessuno. Il controllo dell'apertura con
           |lock passa poi direttamente al pgm. "wprogmagric".
           open output progmagric.
           if RecLocked
              set errori to true
              move "[PROGMAGRIC] GIA' IN USO!!!" to como-riga
              perform SETTA-RIGA-STAMPA
           end-if.

      ***---
       ELABORAZIONE.
           move "CREAZIONE FILE PARALLELO PARTENDO DAI CONSOLIDATI"
             to como-riga.
           perform SETTA-RIGA-STAMPA.
           move 0 to counter counter2.
           |1. Creo il file parallelo dei progressivi x ricalcolo
           |   partendo da quello consolidato
           move low-value to prg-chiave.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if                                        
                    initialize prr-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move prg-chiave       to prr-chiave
                    move prg-peso-utf     to prr-peso-utf
                    move prg-peso-non-utf to prr-peso-non-utf

                    move prg-costo-medio  to prr-costo-inizio
                    move prg-costo-ultimo to prr-costo-ultimo

                    move prg-sezione-consolidati
                      to prr-consolidati
                    write prr-rec invalid continue end-write
                 end-perform
                 close    progmagric
                 open i-o progmagric
           end-start.

           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.

           |2. Valorizzo i progressivi ricalcolati partendo dal 
           |   giorno successivo alla data di consolidamento
           accept como-data  from century-date.

           move tge-data-consolid-progmag to fine-mese.
           if mese of fine-mese = 12
              move 1 to mese of fine-mese
              add  1 to anno of fine-mese
           else
              add 1 to mese  of fine-mese
           end-if.

           evaluate mese of fine-mese
           when  1  move 31 to giorno of fine-mese
           when  2
                 divide   anno of fine-mese  by 4 giving var1
                 multiply var1               by 4 giving var1
                 if var1 = anno of fine-mese
                    move 29  to giorno of fine-mese
                 else
                    move 28  to giorno of fine-mese
                 end-if
           when  3  move 31 to giorno of fine-mese
           when  4  move 30 to giorno of fine-mese
           when  5  move 31 to giorno of fine-mese
           when  6  move 30 to giorno of fine-mese
           when  7  move 31 to giorno of fine-mese
           when  8  move 31 to giorno of fine-mese
           when  9  move 30 to giorno of fine-mese
           when 10  move 31 to giorno of fine-mese
           when 11  move 30 to giorno of fine-mese
           when 12  move 31 to giorno of fine-mese
           end-evaluate.

           if como-data > fine-mese
              move fine-mese to data-rical
           else
              compute data-rical = function INTEGER-OF-DATE(como-data)
              subtract 1 from data-rical
              compute data-rical = function DATE-OF-INTEGER(data-rical)
           end-if.
           move data-rical(5:2) to link-mese.

           move low-value                         to tmo-rec.
           add 1 to tge-data-consolid-progmag giving tmo-data-movim.
           initialize como-riga.
           string "SCANSIONE MOVIMENTI DAL " delimited size
                  tmo-data-movim(7:2)        delimited size
                  "/"                        delimited size
                  tmo-data-movim(5:2)        delimited size
                  "/"                        delimited size
                  tmo-data-movim(1:4)        delimited size
                  " AL "                     delimited size
                  data-rical(7:2)            delimited size
                  "/"                        delimited size
                  data-rical(5:2)            delimited size
                  "/"                        delimited size
                  data-rical(1:4)            delimited size
                  into como-riga
           end-string.
           perform SETTA-RIGA-STAMPA.

           start tmovmag key is >= k-data
                 invalid
                 move "NESSUN MOVIMENTO DA CONSOLIDARE!" to como-riga
                 perform SETTA-RIGA-STAMPA
                 set errori    to true
                 set errore-ko to true
                 close progmagric
           end-start.

           if tutto-ok
                                  
              move 0 to counter counter2
              perform until 1 = 2
                 read tmovmag next at end exit perform end-read
                 if tmo-data-movim > data-rical
                    exit perform
                 end-if

                 perform LOOP-RIGHE-RMOVMAG

                 if errori 
                    set errore-ko to true
                    exit perform 
                 end-if
              end-perform

           end-if.

           move "FINE SCANSIONE MOVIMENTI" to como-riga
           perform SETTA-RIGA-STAMPA.

           cancel "wprogmagric".

           |3. Valorizzo il costo MP sul file calcolandolo sul padre
           set TrattamentoGDO to true.
           accept imp-data from century-date.

           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.
                            
           move 0 to counter counter2.
           open i-o progmagric.
           move low-value to prr-chiave.
           start progmagric key >= prr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmagric next at end exit perform end-read
                    if RichiamoSchedulato
                       perform CONTATORE-VIDEO
                    end-if                                        
                    if prr-peso = 0 |Sono sul padre

                       initialize prg-chiave
                       move 0 to art-prezzo-acquisto
                       set TrattamentoGDO    to true
                       move prr-cod-articolo to prg-cod-articolo
                       read progmag no lock invalid continue end-read
                       move prr-consolidati  to prg-sezione-consolidati
                       perform CALCOLA-COSTO-MP-COMPLETO
                       if recupero-anagrafica or 
                          recupero-iniziale   or
                          recupero-ultimo
                          perform VERIFICA-MOVIMENTAZIONE-PROGRESSIVI
                          move prr-cod-articolo to art-codice
                          read articoli no lock 
                               invalid continue 
                          end-read
                          move art-marca-prodotto to mar-codice
                          read tmarche no lock 
                               invalid continue
                          end-read
                          if path-rep-recupero = spaces
                             perform CREA-REPORT-RECUPERI
                          end-if

                          evaluate true
                          when recupero-iniziale   move "I" to r-tipo
                          when recupero-ultimo     move "U" to r-tipo
                          when recupero-anagrafica move "A" to r-tipo
                          end-evaluate

                          move art-codice       to r-codice
                          move art-descrizione  to r-descrizione
                          move mar-descrizione  to r-marca
                          move costo-mp         to r-prz

                          if trovato-movim
                             move "*"    to r-mese
                          else
                             move spaces to r-mese
                          end-if

                          write riga-recupero from r-stampa
                          set ok-recupero to true          

                       end-if
                    end-if

                    |Se ho recuperato da articoli non devo
                    |valorizzare il costo medio
      *****              if recupero-anagrafica
      *****                 move costo-mp   to prr-prz-anagrafica
      *****              else
      *****                 move costo-mp   to prr-costo-medio
      *****              end-if

                    evaluate true
                    when recupero-anagrafica
                         move costo-mp   to prr-prz-anagrafica
                    when recupero-iniziale
                         move costo-mp   to prr-costo-inizio
                    when recupero-ultimo
                         move costo-mp   to prr-costo-ultimo
                    when recupero-normale
                         move costo-mp   to prr-costo-medio
                    end-evaluate                           
                         
                    move prg-costo-medio to prr-costo-medio-progmag

                    rewrite prr-rec invalid continue end-rewrite

                 end-perform
           end-start.

      ***---
       VERIFICA-MOVIMENTAZIONE-PROGRESSIVI. |Nell'anno in corso (Walter)
           set trovato-movim to false.
           move prg-cod-articolo to rmo-articolo.
           initialize rmo-data-movim.

           move tge-data-consolid-progmag(1:4) to anno-verifica.

           if tge-data-consolid-progmag(5:2) = 12
              add 1 to anno-verifica
           end-if.

           string anno-verifica delimited size
                  "0101"        delimited size
                  into rmo-data-movim
           end-string.

           start rmovmag key >= k-art-data
                 invalid continue
             not invalid
                 read rmovmag next
                      at end continue
                  not at end
                      if rmo-articolo = prg-cod-articolo and
                         rmo-data-movim <= data-rical
                         set trovato-movim to true
                      end-if
                 end-read
           end-start.

      ***---
       LOOP-RIGHE-RMOVMAG.
           set trovato     to false.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           move low-values to rmo-riga.
           start rmovmag key is >= rmo-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rmovmag  next at end exit perform end-read

                 if tmo-anno   not = rmo-anno   or
                    tmo-numero not = rmo-movim
                    exit perform
                 end-if
                 if RichiamoSchedulato
                    perform CONTATORE-VIDEO
                 end-if                                        
                 move "BATCH"             to link-user
                 move rmo-chiave-progmag  to link-key
                 move "00011111111111111" to link-array

                 if rmo-qta not = 0
                    compute link-valore-monetario =
                          ( rmo-netto    +
                            rmo-imp-cons +
                            rmo-coubat ) * rmo-qta
                    compute link-valore-peso      =
                            rmo-peso * rmo-qta
                 else
                    move 0 to link-valore-peso
                    compute link-valore-monetario =
                           rmo-netto + rmo-imp-cons + rmo-coubat
                 end-if

                 move rmo-qta            to link-valore

                 move rmo-causale        to link-causale
                 set link-update         to true
                 set link-open-with-lock to true
                 set link-update-um      to true
                 set link-update-peso    to true
                 set link-update-valore  to true
                 if prima-volta
                    set prima-volta to false
                    close progmagric |wprogmagric aprirà con lock il file!!!
                 end-if
                 call "wprogmagric" using link-wprogmag
                 set trovato     to true
                 if link-wprogmag-status = -1
                    set errori to true
                    exit perform
                 end-if
              end-perform
           end-if.

      ***---
       CREA-REPORT-RECUPERI.
           accept path-rep-recupero from environment "CALMAR_RECUPERO".
           open output rep-recupero.
           initialize riga-recupero.
           string "PREZZI RECUPERATI AL " delimited size
                  data-rical(7:2)         delimited size
                  "/"                     delimited size
                  data-rical(5:2)         delimited size
                  "/"                     delimited size
                  data-rical(1:4)         delimited size
                  into riga-recupero
           end-string.
           write riga-recupero.
           write riga-recupero from spaces.

      ***---
       CONTATORE-VIDEO.
           add 1 to counter counter2

           if counter2 = 500
              move counter to counter-edit
              display counter-edit 
                      upon batch-win-handle
                      line 25,00
                    column 38,00
              move 0 to counter2
           end-if.

      ***--
       CLOSE-FILES.
           close tmovmag  rmovmag  progmag
                 tparamge articoli tmarche timposte.

      ***---
       CHIAMA-PROGRAMMI.
           |Genero anche il file di stampa.
           move "CREAZIONE STAMPA STATISTICHE NEW" to como-riga.
           perform SETTA-RIGA-STAMPA.
           call   "sttendenza-p" using link-mese 
                                       data-rical
                                       batch-linkage
           cancel "sttendenza-p".
           move batch-status to st-tendenza-status.
           
           move "CREAZIONE STAMPA DELTA" to como-riga.
           perform SETTA-RIGA-STAMPA.
           call   "st-delta-p"   using link-mese 
                                       data-rical
                                       batch-linkage.
           cancel "st-delta-p".                    
           move batch-status to st-delta-status.

      ***---
       SETTA-RIGA-STAMPA.
           initialize riga-stampa.
           perform SETTA-INIZIO-RIGA.
           string r-inizio  delimited size
                  como-riga delimited size
                  into riga-stampa
           end-string.   
           if RichiamoSchedulato
              write line-riga of lineseq from riga-stampa
           else
              display riga-stampa upon syserr
           end-if.

      ***---
       INVIO-MAIL.
           move "INVIO MAIL IN CORSO..." to como-riga.
           perform SETTA-RIGA-STAMPA.

           initialize LinkBody.
           evaluate true
           when nessun-errore
                move "RICALCOLO NOTTURNO EFFETUATO - OK" 
                  to LinkSubject
                move "In allegato dettaglio funzionamento programma" 
                  to LinkBody     
           when ok-recupero
                move "RICALCOLO NOTTURNO  EFFETTUATO - ATTENZIONE"
                  to LinkSubject
                move "In allegato dettaglio funzionamento programma e de
      -              "ttaglio recupero prezzi da anagrafica" 
                  to LinkBody   
           when errore-ko
                move "RICALOLO NOTTURNO NON EFFETTUATO - ERRORI" 
                  to LinkSubject
                move "In allegato dettaglio funzionamento programma" 
                  to LinkBody   
           end-evaluate.

           accept LinkAddress from environment "CALMAR_ADDRESSES".
           accept FileOrig    from environment "CALMAR_LOG".
           accept FileDest    from environment "CALMAR_LOG_INVIO".
           call "C$COPY" using FileOrig, FileDest, "S".
           move FileDest to LinkAttach.
           if ok-recupero
              initialize LinkAttach
              inspect FileDest replacing trailing spaces by low-value 
              string  FileDest          delimited low-value
                      ";"               delimited size
                      path-rep-recupero delimited size
                      into LinkAttach
              end-string
              close rep-recupero
           end-if.

           set errori to true.
           move 0 to tentativi.
           move "calmar-p" to NomeProgramma.
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
       EXIT-PGM.
           move "TERMINE PROGRAMMA" to como-riga.
           perform SETTA-RIGA-STAMPA.

           if RichiamoSchedulato
              evaluate true
              when nessun-errore  move  0 to batch-status
              when ok-recupero    
                   initialize como-riga
                   string "GENERATO REPORT: " delimited size
                          path-rep-recupero   delimited size
                          into como-riga
                   end-string
                   perform SETTA-RIGA-STAMPA
                   move  1 to batch-status
              when errore-ko      move -1 to batch-status
              end-evaluate     
              if batch-status = 0
                 evaluate st-tendenza-status also st-delta-status
                 when 0 also 0 move 0 to batch-status
                 when 1 also 1 
                 when 0 also 1 
                 when 1 also 0 move 1 to batch-status
                 end-evaluate
              end-if
              close lineseq
              display "                                             "
                 upon batch-win-handle
                   line 25,00
                 column 35,00
           end-if.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "costo-medio.cpy".
           copy "recupero-anagrafica.cpy".
           copy "mail.cpy".
           copy "setta-inizio-riga.cpy".
