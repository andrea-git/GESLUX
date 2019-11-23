       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      starticoli-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "lisagente.sl".
           copy "articoli.sl".
           copy "tmp-starticoli.sl".
           copy "tmarche.sl".
           copy "tmp-promo.sl".
           copy "timposte.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "lisagente.fd".
           copy "articoli.fd".
           copy "tmp-starticoli.fd".
           copy "tmarche.fd".
           copy "tmp-promo.fd".
           copy "timposte.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".
       copy "promo.def".
       copy "imposte.def".
       copy "selprint.lks".

      * COSTANTI
       78  titolo value "Stampa Articoli (listini)".
       78  MaxRows                    value 44.

      * FILE STATUS                          
       77  status-lisagente           pic xx.
       77  status-articoli            pic xx.
       77  status-tparamge            pic xx.
       77  status-lineseq             pic xx.
       77  status-tmp-starticoli      pic xx.
       77  status-tmarche             pic xx.
       77  status-timposte            pic xx.
       77  status-tmp-promo           pic xx.

       77  wstampa                    pic x(256).
       77  path-tmp                   pic x(256).

      * VARIABILI
       77  como-data                  pic 9(8).
       77  como-ora                   pic 9(8).
       77  RowCounter                 pic 9(3).
       77  PageCounter                pic 9(5).
       77  path-csv                   pic x(256).
       77  int-data                   pic x(8).
       77  user-codi                  pic x(10).

       01  progmag.
           05  prg-peso-utf           pic 9(6)v999. |DUMMY
           05  prg-peso-non-utf       pic 9(6)v999.
       77  prezzo-listino             pic 9(6)v99.

      * FLAGS
       01  calcolo-imposte            pic 9.
         88 si-calcolo                value 1.
         88 no-calcolo                value 0.

       01  controllo                  pic xx.
         88 errori                    value "ER".
         88 tutto-ok                  value "OK".

       01  filler                     pic 9.
         88 RecLocked                 value 1, false 0.

       01  filler                     pic 9.
         88 record-ok                 value 1, false 0.

       01  filler                     pic 9.
         88 trovato                   value 1, false 0.

       01  filler                     pic 9.
         88 prima-volta               value 1, false 0.

      * RIGHE PER LA STAMPA
       01  riga-div-1.
         05 filler                    pic x(130) value all "-".

       01  intestazione.
         05 filler                    pic x(12) value "LUBEX S.p.A.".
         05 filler                    pic x(37).
         05 tit-int                   pic x(44).
         05 filler                    pic x(30).
         05 filler                    pic x(4)  value "Pag.".
         05 int-pag                   pic zz9.

       01  titolo-1.
         05 filler                    pic x(1).
         05 filler                    pic x(6)  value "Codice".
         05 filler                    pic x(2).
         05 filler                    pic x(8)  value "Articolo".
         05 filler                    pic x(33).
         05 filler                    pic x(5)  value "Marca".
         05 filler                    pic x(3).
         05 filler                    pic x(3)  value "UTF".
         05 filler                    pic x(3).
         05 filler                    pic x(9)  value "  Peso UM".
         05 filler                    pic x(5).
         05 filler                    pic x(7)  value "Listino".
         05 filler                    pic x(2).
         05 filler                    pic x(8)  value "Sconto %".
         05 filler                    pic x(6).
         05 filler                    pic x(5)  value "Netto".   
         05 filler                    pic x(8).
         05 filler                    pic x(5)  value "I.C.".
         05 filler                    pic x(8).
         05 filler                    pic x(5)  value "COU".

       01  r-riga.
         05 filler                    pic x.
         05 r-cod                     pic z(6).
         05 filler                    pic x(2).
         05 r-des                     pic x(40).
         05 filler                    pic x(2).
         05 r-marca                   pic z(4). 
         05 filler                    pic x(4).
         05 r-utf                     pic x.    
         05 filler                    pic x(4).
         05 r-peso                    pic z.zz9,999.
         05 filler                    pic x(2).
         05 r-listino                 pic zzz.zz9,99.
         05 filler                    pic x(3).
         05 r-sconto                  pic zz9,99.
         05 filler                    pic x(2).
         05 r-netto                   pic zzz.zz9,99.
         05 filler                    pic x(2).
         05 r-ic                      pic zzz.zz9,99.
         05 filler                    pic x(2).
         05 r-coubat                  pic zzz.zz9,99.

       LINKAGE SECTION.
       copy "link-starticoli.def".

      ******************************************************************
       PROCEDURE DIVISION using sta-linkage.

       DECLARATIVES.
      
      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           set tutto-ok  to true.
           evaluate status-timposte
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File imposte [TIMPOSTE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TIMPOSTE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TIMPOSTE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.  
      
      ***---
       LISAGENTE-ERR SECTION.
           use after error procedure on lisagente.
           set tutto-ok  to true.
           evaluate status-lisagente
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File listini agente [LISAGENTE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LISAGENTE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LISAGENTE] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           end-evaluate.   

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "Lineseq"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform OPEN-OUTPUT-LINESEQ
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       TMP-STARTICOLI-ERR SECTION.
           use after error procedure on tmp-starticoli.
           set tutto-ok  to true.
           evaluate status-tmp-starticoli
           when "35"
                display message "File [TMP-STARTICOLI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TMP-STARTICOLI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TMP-STARTICOLI] Indexed file corrupt!"
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
                move   "file TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform OPEN-OUTPUT-TMP-STARTICOLI
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       TMP-PROMO-ERR SECTION.
           use after error procedure on tmp-promo.
           set tutto-ok  to true.
           evaluate status-tmp-promo
           when "35"
                display message "File [TMP-PROMO] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TMP-PROMO] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TMP-PROMO] Indexed file corrupt!"
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
                move   "file TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform OPEN-OUTPUT-TMP-PROMO
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           end-evaluate.

      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File articoli [ARTICOLI] inesistente"
                          title titolo
                           icon 2
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
           when "93"
           when "99" set RecLocked to true
                     set errori    to true
           end-evaluate 
      
      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File marche [TMARCHE] inesistente"
                          title titolo
                           icon 2
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

       END DECLARATIVES.

      ***--- 
       MAIN-PRG.
           evaluate true
           when sta-promo
           when sta-pers
                perform LISTINO-PROMO-PERS  
           when other
                perform LISTINO-ACQUISTO-AGENTE
           end-evaluate.

      ***---
       LISTINO-ACQUISTO-AGENTE.
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
           move sta-user to user-codi.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa path-csv.
           move 0 to  RowCounter.
           move 0 to PageCounter.
           set tutto-ok       to true.
           set trovato        to false.
           set prima-volta    to true.
           accept  wstampa      from environment "PATH-ST".
           inspect wstampa      replacing trailing spaces by low-value.
           inspect user-codi    replacing trailing spaces by low-value.
           if sta-acquisto
              string  wstampa            delimited by low-value
                      "listino_acquisto" delimited by size
                      "_"                delimited by size
                      user-codi          delimited by low-value
                      ".csv"             delimited by size
                      into path-csv
              end-string
           else
              string  wstampa            delimited by low-value
                      "listino_agente"   delimited by size
                      "_"                delimited by size
                      user-codi          delimited by low-value
                      ".csv"             delimited by size
                      into path-csv
              end-string
           end-if.
           string  wstampa      delimited by low-value
                   "starticoli" delimited by size
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   ".txt"       delimited by size
                   into wstampa
           end-string.
           accept  path-tmp     from environment "PATH-ST".
           inspect path-tmp     replacing trailing spaces by low-value.
           string  path-tmp     delimited by low-value
                   "tmp-starticoli" delimited by size
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   ".tmp"       delimited by size
                   into path-tmp
           end-string.
           move como-data(7:2) to int-data(1:2).
           move "/"            to int-data(3:1).
           move como-data(5:2) to int-data(4:2).
           move "/"            to int-data(6:1).
           move como-data(3:2) to int-data(7:2).

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              perform OPEN-OUTPUT-TMP-STARTICOLI
              if tutto-ok
                 open input articoli tmarche timposte
                 if errori
                    close lineseq
                    delete file lineseq
                    close tmp-starticoli
                    delete file tmp-starticoli
                 end-if
              else
                 close lineseq
                 delete file lineseq
              end-if
           end-if.
           if errori goback end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.    

      ***---
       OPEN-OUTPUT-TMP-STARTICOLI.
           open output tmp-starticoli.

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move  sta-cod-da      to art-codice.
           start articoli key is >= art-chiave
                 invalid  set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read articoli next at end exit perform end-read

                 if art-codice > sta-cod-a exit perform end-if

                 set record-ok to false

                 if sta-marca not = 0
                    if sta-marca = art-marca-prodotto
                       set record-ok to true
                    else
                       set record-ok to false
                    end-if
                 else
                    set record-ok to true
                 end-if

                 if record-ok
                    if sta-sett not = 0
                       if sta-sett = art-settore-merceologico
                          set record-ok to true
                       else
                          set record-ok to false
                       end-if
                    else
                       set record-ok to true
                    end-if
                 end-if

                 if record-ok
                    if sta-classe not = 0
                       if sta-classe = art-classe-1
                          set record-ok to true 
                       else
                          set record-ok to false
                       end-if
                    else
                       set record-ok to true
                    end-if
                 end-if

                 if record-ok
                    perform VALORIZZA-RIGA
                 end-if

              end-perform

              if not trovato
                 display message 
                         "Nessun articolo trovato nei limiti richiesti"
                           title titolo
                            icon 2
              else
                 if sta-si-excel
                    perform GENERA-FILE-EXCEL
                    perform CALL-EXCEL
                 else
                    perform GENERA-FILE-TXT
                    move wstampa to sta-path
                 end-if
              end-if

           end-if.

      ***---
       VALORIZZA-RIGA.
           initialize tms-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move art-marca-prodotto to tms-marca mar-codice.
           move art-codice         to tms-articolo.
           move art-descrizione    to tms-descrizione.

           if art-si-utf or art-misto move "Y" to tms-utf
           else                       move "N" to tms-utf
           end-if.

           read tmarche no lock invalid continue end-read.

           add art-peso-utf to art-peso-non-utf giving tms-peso-um.
           evaluate true |sta-tipo-stampa
           when sta-acquisto 
                move art-prezzo-acquisto       to tms-listino
                move art-perce-sconto-acquisto to tms-sconto
                compute tms-netto =
                        tms-listino - 
                    ( ( tms-listino * tms-sconto ) / 100 )
           when sta-agente
                move art-prezzo-vendita      to tms-listino
                move art-perce-sconto-agente to tms-sconto
                compute tms-netto =
                        tms-listino - 
                    ( ( tms-listino * tms-sconto ) / 100 )
           when sta-promo continue
           end-evaluate.

      *****     if mar-si-imposta-consumo
      *****        compute tms-ic  = ( tms-netto * art-perce-imposte ) / 100
      *****     end-if.
      *****     if mar-si-cou
      *****        compute tms-cou = ( tms-netto * art-perce-cou     ) / 100
      *****     end-if.

           move art-peso-utf     to prg-peso-utf.
           move art-peso-non-utf to prg-peso-non-utf.

           |Da liberare ed esporre in seguito
           set TrattamentoPiombo to false.
           set TrattamentoGDO    to false.
           perform CALCOLA-IMPOSTE.
           move imposta-consumo to tms-ic.
           move imposta-cou     to tms-cou.

           write tms-rec invalid continue end-write.
           set trovato to true.

      ***---
       SCRIVI-INTESTAZIONE.
           if RowCounter = 0
              add 1 to PageCounter
              move PageCounter  to int-pag
              evaluate true |sta-tipo-stampa
              when sta-acquisto
                   move spaces to tit-int
                   string "Listino Acquisto in data: " delimited size
                          int-data                     delimited size
                          into tit-int
                   end-string
              when sta-agente
                   move spaces to tit-int
                   string "Listino Agenti Direzionali in data:"
                                                       delimited size
                          int-data                     delimited size
                          into tit-int
                   end-string
              end-evaluate
              write line-riga from intestazione
              add 1 to RowCounter
           end-if.
           write line-riga from riga-div-1.
           add 1 to RowCounter.
           write line-riga from titolo-1.
           add 1 to RowCounter.
           write line-riga from riga-div-1
           add 1 to RowCounter.

      ***---
       GENERA-FILE-TXT.
           set tutto-ok to true.
           close tmp-starticoli.
           open input tmp-starticoli.
           move low-value to tms-chiave.
           start tmp-starticoli key is >= tms-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tmp-starticoli next 
                      at end exit perform 
                 end-read
                 if prima-volta
                    perform SCRIVI-INTESTAZIONE
                    set prima-volta to false
                 end-if
                 add 1 to RowCounter
                 if RowCounter = MaxRows
                    perform SALTO-PAGINA
                 end-if
                 initialize line-riga
                 move tms-articolo    to r-cod
                 move tms-descrizione to r-des
                 move tms-marca       to r-marca
                 move tms-utf         to r-utf
                 move tms-peso-um     to r-peso
                 move tms-listino     to r-listino
                 move tms-sconto      to r-sconto
                 move tms-netto       to r-netto
                 move tms-ic          to r-ic
                 move tms-cou         to r-coubat
                 write line-riga from r-riga
              end-perform
           end-if.

      ***---
       SALTO-PAGINA.
           write line-riga from spaces after page.
           move 0 to RowCounter.
           perform SCRIVI-INTESTAZIONE.

      ***---
       GENERA-FILE-EXCEL.
           set prima-volta to true.
           set tutto-ok    to true.
           close tmp-starticoli.
           open input tmp-starticoli.
           move low-value to tms-chiave.
           start tmp-starticoli key is >= tms-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              move path-csv to wstampa
              close lineseq
              open output lineseq
              perform until 1 = 2
                 read tmp-starticoli next 
                      at end exit perform 
                 end-read
                 if prima-volta
                    perform ACCETTA-SEPARATORE
                    perform SCRIVI-INTESTAZIONE-EXCEL
                    set prima-volta to false
                 end-if
                 move tms-articolo    to r-cod
                 move tms-descrizione to r-des
                 move tms-marca       to r-marca
                 move tms-utf         to r-utf
                 move tms-peso-um     to r-peso
                 move tms-listino     to r-listino
                 move tms-sconto      to r-sconto
                 move tms-netto       to r-netto
                 move tms-ic          to r-ic
                 move tms-cou         to r-coubat

                 initialize line-riga
                 string r-cod         delimited size
                        separatore    delimited size
                        r-des         delimited size
                        separatore    delimited size
                        r-marca       delimited size
                        separatore    delimited size
                        r-utf         delimited size
                        separatore    delimited size
                        r-peso        delimited size
                        separatore    delimited size
                        r-listino     delimited size
                        separatore    delimited size
                        r-sconto      delimited size
                        separatore    delimited size
                        r-netto       delimited size
                        separatore    delimited size
                        r-ic          delimited size
                        separatore    delimited size
                        r-coubat      delimited size
                        into line-riga
                 end-string
                 write line-riga
              end-perform
              close lineseq
           end-if.

      ***---
       SCRIVI-INTESTAZIONE-EXCEL.
           initialize line-riga.
           evaluate true |sta-tipo-stampa
           when sta-acquisto
                string separatore                   delimited size
                       "** "                        delimited size
                       "LUBEX S.p.A."               delimited size
                       " - "                        delimited size
                       "Listino Acquisto in data: " delimited size
                       int-data                     delimited size
                       " **"                        delimited size
                       into line-riga
                end-string
                write line-riga
           when sta-agente
                string separatore                   delimited size
                       "** "                        delimited size
                       "LUBEX S.p.A."               delimited size
                       " - "                        delimited size
                       "Listino Agenti Direzionali" delimited size
                       " in data:"                  delimited size
                       int-data                     delimited size
                       " **"                        delimited size
                       into line-riga
                end-string
                write line-riga
           end-evaluate.

           initialize line-riga.
           string "Codice"      delimited size
                  separatore    delimited size
                  "Articolo"    delimited size
                  separatore    delimited size
                  "Marca"       delimited size
                  separatore    delimited size
                  "UTF"         delimited size
                  separatore    delimited size
                  "Peso UM"     delimited size
                  separatore    delimited size
                  "Listino"     delimited size
                  separatore    delimited size
                  "Sconto %"    delimited size
                  separatore    delimited size
                  "Netto"       delimited size
                  separatore    delimited size
                  "I.C."        delimited size
                  separatore    delimited size
                  "COU"         delimited size
                  into line-riga
           end-string.
           write line-riga.
           write line-riga from spaces.

      ***---
       CLOSE-FILES.
           close lineseq articoli tmarche timposte.
           close tmp-starticoli.
           delete file tmp-starticoli.
  
      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
           copy "promo.cpy".
           copy "imposte.cpy".
