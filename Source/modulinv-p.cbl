       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      modulinv-p.
       AUTHOR.                          Andrea.
       REMARKS. Semplice estrazione dei progressivi di magazzino coi 
           filtri passati in linkage. Viene utilizzata la giacenza day
           che è stata "fissata" dal programma inveday
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "articoli.sl".
           copy "progmag.sl".
           copy "tmp-modulinv.sl".
           copy "tmarche.sl".
           copy "timbalqta.sl".
           copy "timballi.sl".
           copy "timposte.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "tmp-modulinv.fd".
           copy "tmarche.fd".
           copy "timbalqta.fd".
           copy "timballi.fd".
           copy "timposte.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".
       copy "costo-medio.def".
       copy "imposte.def".

       78  titolo                value "Modulistica per Inventari".
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  scelta                pic 9.

       77  giac-day              pic s9(8).

       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.
       77  status-progmag        pic xx.
       77  status-tmp-modulinv   pic xx.
       77  status-tmarche        pic xx.
       77  status-timbalqta      pic xx.
       77  status-timballi       pic xx.
       77  status-timposte       pic xx.

       77  path-csv              pic x(256).
       77  path-txt              pic x(256).
       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).
       77  modulinv-path         pic x(256).
       77  SaveMarca             pic 9(5) value 0.
       77  prg-cod-articolo-edit pic z(6).
       77  prg-peso-edit         pic zz9,999.
       77  imballi-ed            pic z.zz9.
       77  qta-edit              pic z.zzz.
       77  giacenza-ed           pic ---.---.--9.
       77  como-giacenza         pic s9(8).
       77  como-peso             pic 9(5)v999.

       77  tipo-file             pic x.
           88 FileExcel          value "E".
           88 FileTxt            value "T".

       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88  prima-volta       value 1, false 0.
       77  filler                pic 9.
           88  record-ok         value 1, false 0.
       77  filler                pic 9.
           88  record-padre-ok   value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       LINKAGE SECTION.
       77  user-codi            pic x(10).
       77  modulinv-mag         pic x(3).
       77  modulinv-marca       pic 9(4).

      ******************************************************************
       PROCEDURE DIVISION using user-codi
                                modulinv-mag
                                modulinv-marca.

       DECLARATIVES.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "File [ARTICOLI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [ARTICOLI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                display message "File [PROGMAG] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [PROGMAG] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[PROGMAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message "File [TMARCHE] not found!"
                           title titolo
                            icon 3
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
 
      ***---
       TIMBALQTA-ERR SECTION.
           use after error procedure on timbalqta.
           set tutto-ok  to true.
           evaluate status-timbalqta
           when "35"
                display message "File [TIMBALQTA] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TIMBALQTA] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TIMBALQTA] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TIMBALLI-ERR SECTION.
           use after error procedure on timballi.
           set tutto-ok  to true.
           evaluate status-timballi
           when "35"
                display message "File [TIMBALLI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TIMBALLI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TIMBALLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.

      ***---
       TMP-MODULINV-ERR SECTION.
           use after error procedure on tmp-modulinv.
           set tutto-ok  to true.
           evaluate status-tmp-modulinv
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
                move   "File TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform OPEN-OUTPUT-TMP-MODULINV
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

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
                move   "File CSV"   to geslock-nome-file
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
           accept como-data from century-date.
           accept como-ora  from time.
           perform ACCETTA-SEPARATORE.
           initialize path-tmp wstampa path-txt.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  modulinv-path from environment "PATH-ST".
           inspect modulinv-path replacing trailing spaces by low-value.
           inspect user-codi     replacing trailing spaces by low-value.
           string  modulinv-path delimited by low-value
                   "modulinv"    delimited by size
                   "_"           delimited by size
                   user-codi     delimited by low-value
                   "_"           delimited by size
                   como-data     delimited by size
                   "_"           delimited by size
                   como-ora      delimited by size
                   ".tmp"        delimited by size
                   into path-tmp
           end-string.
           string  modulinv-path delimited by low-value
                   "modulinv"    delimited by size
                   "_"           delimited by size
                   user-codi     delimited by low-value
                   ".csv"        delimited by size
                   into path-csv
           end-string.
      *****     if inveday-std
      *****        string  inveday-path delimited by low-value
      *****                "modulinv"   delimited by size
      *****                "_"          delimited by size
      *****                inveday-user delimited by low-value
      *****                "_"          delimited by size
      *****                como-data    delimited by size
      *****                "_"          delimited by size
      *****                como-ora     delimited by size
      *****                ".txt"       delimited by size
      *****                into path-txt
      *****        end-string
      *****     end-if.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-TMP-MODULINV.
           if tutto-ok
              open input articoli
                         tmarche
                         timbalqta
                         timballi
                         progmag
                         timposte
           end-if.

      ***---
       OPEN-OUTPUT-TMP-MODULINV.
           open output tmp-modulinv.
           close    tmp-modulinv.
           open i-o tmp-modulinv.

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move low-value       to prg-rec.                                      

           start progmag key is >= prg-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read progmag next no lock at end exit perform end-read

                 |Lo calcolo comunque sul padre come da
                 |richiesta di Trivella in data 04/01/06
                 if prg-cod-magazzino = spaces and
                    prg-tipo-imballo  = spaces and
                    prg-peso          = 0
                    perform CALCOLA-COSTO-MP-COMPLETO
                    move prg-cod-articolo to art-codice
                    read articoli no lock 
                         invalid continue 
                    end-read                               

                    |La giace-day dev'essere la somma dei figli
                    |del magazzino richiesto
                    if modulinv-mag = spaces
                       move prg-giac-day to giac-day
                    else                           
                       move 0 to giac-day
                       perform until 1 = 2
                          read progmag next at end exit perform end-read
                          if prg-cod-articolo not = art-codice
                             exit perform
                          end-if
                          if prg-cod-magazzino = modulinv-mag
                             add prg-giac-day to giac-day
                          end-if
                       end-perform
                       |Mi riposiziono sul padre
                       initialize prg-chiave 
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move art-codice to prg-cod-articolo
                       start progmag key >= prg-chiave
                       read progmag next
                    end-if


                    if prg-ven-udm      not = 0 or
                       prg-ini-udm      not = 0 or
                       prg-acq-udm      not = 0 or
                       prg-giacenza     not = 0 or
                       prg-giacenza-udm not = 0 or
                       giac-day         not = 0
                       set record-padre-ok to true
                    else
                       set record-padre-ok to false
                    end-if

                 end-if

                 if record-padre-ok
                    if prg-cod-magazzino not = spaces and
                       prg-tipo-imballo  not = spaces and
                       prg-peso          not = 0 |E' un record figlio

                       if record-ok
                          if modulinv-mag not = spaces
                             if prg-cod-magazzino not = modulinv-mag
                                set record-ok to false
                             end-if
                          end-if

                          if record-ok
                             if modulinv-marca not = 0
                                if art-marca-prodotto not = 
                                   modulinv-marca
                                   set record-ok to false
                                end-if
                             end-if
                          end-if
                       end-if
                    else
                       set record-ok to false
                    end-if
                 else
                    set record-ok to false
                 end-if
                 
                 if record-ok perform VALORIZZA-RIGA end-if

              end-perform
           end-if.

           if not trovato
              display message "Nessun articolo trovato"
                        title titolo
                         icon 2
           else
              close tmp-modulinv
              open input tmp-modulinv
              if tutto-ok
                 perform OPEN-OUTPUT-LINESEQ
                 if tutto-ok perform GENERA-FILE-EXCEL end-if
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           move path-csv to wstampa.
           open output lineseq.

      ***---
       GENERA-FILE-EXCEL.
           set FileExcel to true.
           move low-value to tmod-rec.
           start tmp-modulinv key is >= k-ord invalid continue end-start
           perform until 1 = 2
              read tmp-modulinv next at end exit perform end-read

              if prima-volta
                 perform SCRIVI-INTESTAZIONE
              end-if

              move low-value     to prg-chiave       
              move tmod-articolo to art-codice prg-cod-articolo
              move tmod-mag      to prg-cod-magazzino
              move tmod-imballo  to prg-tipo-imballo
              move tmod-peso     to prg-peso

              move -99999999 to como-giacenza 
              move 0         to como-peso
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-articolo  not = art-codice  or
                          prg-cod-magazzino not = tmod-mag    or
                          prg-tipo-imballo  not = tmod-imballo or
                          prg-peso          not = tmod-peso
                          exit perform
                       end-if
                       if prg-giacenza > como-giacenza
                          move prg-giacenza to como-giacenza
                          move prg-peso     to como-peso
                       end-if
                    end-perform
              end-start

              move tmod-giacenza to giacenza-ed
              move como-peso     to prg-peso-edit

              initialize line-riga
              string tmod-mag
                     separatore
                     tmod-marca
                     separatore
                     tmod-des-marca
                     separatore
                     tmod-articolo
                     separatore            
                     tmod-art-descrizione
                     separatore            
                     tmod-imballo
                     separatore      
                     tmod-des-imballo
                     separatore
LUBEXX               tmod-qta-imballi      
LUBEXX               separatore            
                     prg-peso-edit
                     separatore      
LUBEXX               tmod-utf
LUBEXX               separatore
                     giacenza-ed 
                     separatore
                     tmod-prezzo 
                     separatore
                     tmod-mag-std     
                     separatore
                     tmod-codice-ean-1
                     separatore
                     tmod-codice-ean-2
                     separatore
                     tmod-codice-ean-3
                     separatore
                     tmod-codice-ean-4
                     separatore
                     tmod-codice-ean-5
                     delimited by size
                     into line-riga
              end-string
              write line-riga
           end-perform.

           close lineseq.
           perform CALL-EXCEL.

      ***---
       SCRIVI-INTESTAZIONE.
           set prima-volta          to false.
           initialize line-riga.
           if FileExcel
              string "Magazzino"
                     separatore
                     "Marca"
                     separatore
                     "Descrizione Marca"
                     separatore
                     "Cod. Articolo"
                     separatore
                     "Descrizione Articolo"
                     separatore
                     "Cod."                                
                     separatore
                     "Imballo"
                     separatore
                     "Q.tà imballo"
                     separatore
                     "Peso"            
                     separatore
                     "UTF"
                     separatore
                     "Giacenza"
                     separatore
                     "Prezzo"    
                     separatore
                     "Mag. Std"   
                     separatore
                     "Codice EAN 1"
                     separatore
                     "Codice EAN 2"
                     separatore
                     "Codice EAN 3"
                     separatore
                     "Codice EAN 4"
                     separatore
                     "Codice EAN 5"  delimited by size
                     into line-riga
               end-string
           else
              continue
      *****        move riga-intestazione to line-riga
           end-if.
           write line-riga.

      ***---
       VALORIZZA-RIGA.
           set trovato       to true.
           initialize tmod-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           move prg-cod-magazzino   to tmod-mag.
           move prg-cod-articolo    to tmod-articolo.
           move prg-tipo-imballo    to tmod-imballo.
           move prg-peso            to tmod-peso.

           read tmp-modulinv no lock
                invalid                                     
                move art-codice-ean-1   to tmod-codice-ean-1
                move art-codice-ean-2   to tmod-codice-ean-2
                move art-codice-ean-3   to tmod-codice-ean-3
                move art-codice-ean-4   to tmod-codice-ean-4
                move art-codice-ean-5   to tmod-codice-ean-5
                move art-marca-prodotto to tmod-marca mar-codice
                move art-descrizione    to tmod-art-descrizione
                read tmarche  no lock invalid continue end-read
                move mar-descrizione     to tmod-des-marca
LUBEXX          evaluate true
LUBEXX          when art-si-utf          move "S" to tmod-utf
LUBEXX          when art-no-utf          move "N" to tmod-utf
LUBEXX          when art-misto           move "M" to tmod-utf
LUBEXX          end-evaluate
                if costo-mp = 0 
                   move prg-costo-ultimo to tmod-prezzo
LUBEXX             if prg-costo-ultimo = 0
LUBEXX                move art-prezzo-acquisto to tmod-prezzo
LUBEXX             end-if
                else            
                   add 0,005 to costo-mp giving costo-mp-2dec
                   move costo-mp-2dec        to tmod-prezzo
                end-if
                move prg-tipo-imballo    to imq-codice
                read timbalqta 
                     invalid continue
                 not invalid move imq-tipo to imb-codice
                             read timballi no lock 
                                  invalid continue 
                             end-read
                end-read
                move imq-qta-imb to qta-edit
                inspect imb-descrizione 
                        replacing trailing spaces by low-value
                inspect qta-edit replacing leading x"30" by x"20"
                call "C$JUSTIFY" using qta-edit, "L"
                inspect qta-edit replacing trailing spaces by low-value
                initialize tmod-des-imballo
                string imb-descrizione delimited by low-value
                       " da "          delimited by size
                       qta-edit        delimited by low-value
                       " x "           delimited by size
                       art-udm-imballo delimited by size
                       into tmod-des-imballo
                end-string
                move qta-edit to tmod-qta-imballi
           end-read.
                            
           add prg-giac-day to tmod-giacenza.

           move art-mag-std to tmod-mag-std.

           write tmod-rec invalid rewrite tmod-rec end-write.

      ***---
       CLOSE-FILES.
           close articoli progmag tmarche timballi timbalqta timposte.
           close tmp-modulinv.
           delete file tmp-modulinv.
  
      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
       copy "costo-medio.cpy".
       copy "recupero-anagrafica.cpy".
