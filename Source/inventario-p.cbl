       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      inventario-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "progmag.sl". 
           copy "tmovmag.sl".
           copy "rmovmag.sl".
           copy "tparamge.sl".
           copy "clienti.sl".
           copy "articoli.sl".
           copy "timposte.sl".
           copy "tcaumag.sl".
           copy "tmagaz.sl".
           copy "lineseq.sl".
           
       SELECT tmp-progmag
           ASSIGN       TO path-tmp-progmag
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-progmag
           RECORD KEY   IS tmp-prg-chiave
           ALTERNATE RECORD KEY IS tmp-k-mag = tmp-prg-cod-magazzino
           tmp-prg-cod-articolo, tmp-prg-tipo-imballo, tmp-prg-peso
           WITH DUPLICATES .

       SELECT tmp-cs
           ASSIGN       TO path-tmp-cs
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tmp-cs
           RECORD KEY   IS tcs-chiave.


      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "progmag.fd". 
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "tparamge.fd".
           copy "clienti.fd".
           copy "articoli.fd".
           copy "timposte.fd".
           copy "tcaumag.fd".
           copy "tmagaz.fd".
           copy "lineseq.fd".

       FD  tmp-progmag.
       01 tmp-prg-rec.
           05 tmp-prg-chiave.
              10 tmp-prg-cod-articolo PIC  9(6).
              10 tmp-prg-cod-magazzino            PIC  X(3).
              10 tmp-prg-tipo-imballo PIC  X(3).
              10 tmp-prg-peso         PIC  9(5)V9(3).
           05 tmp-prg-dati.
              10 tmp-prg-giacenza     PIC  S9(8).

       FD  tmp-cs.
       01 tcs-rec.
           05 tcs-chiave.
               10 tcs-magazzino    PIC  x(3).
               10 tcs-causale      PIC  x(4).
           05 tcs-dati.
               10 tcs-tot-utf      PIC  9(5)v999.
               10 tcs-elemento
                          OCCURS 1000 TIMES.
                   15 tcs-prg-chiave.
                       20 tcs-prg-cod-articolo         PIC  9(6).
                       20 tcs-prg-cod-magazzino        PIC  x(3).
                       20 tcs-prg-tipo-imballo         PIC  x(3).
                       20 tcs-prg-peso     PIC  9(5)v999.
                   15 tcs-qta          PIC  9(10).


       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "link-geslock.def".
           copy "comune.def".
           copy "link-nambar.def".
           copy "link-wprogmag.def".
           copy "costo-medio.def".
           copy "imposte.def".

       77  counter             pic 9(10).
       77  counter2            pic 9(10).
       77  counter-edit        pic z(10).

       78  titolo    value "GESLUX - Inventario".
       78  78-clear            value 
           "                                                          ".
       77  tot-idx             pic 9(5).
       77  idx-tmp             pic 9(4).
       77  numero-carico       pic 9(8).
       77  numero-scarico      pic 9(8).
       77  como-mag            pic x(3).
       77  como-cau            pic x(4).
       77  como-tipo           pic x.
       77  como-cod-clifor     pic 9(5).
       77  como-codpag         pic x(3).
       77  como-qta            pic 9(8).
       77  como-ora            pic 9(8).
       77  como-data           pic 9(8).

       77  save-articolo       pic 9(6).
       77  save-magazzino      pic x(3).

       77  giac-pos            pic 9(8).
       77  giac-neg            pic s9(8).
       77  giac-neg-assoluta   pic 9(8).

       77  como-articolo       pic 9(6).
       77  como-quantita       pic 9(8).
       77  como-imballo        pic x(3).
       77  quantita-x          pic x(10).
       77  como-peso           pic 9(7).
       77  save-giacenza       pic 9(8).
       77  save-giacenza3      pic s9(8).
       77  save-quantita       pic 9(8).

       01  filler              pic 9.
           88 trovato-negativo value 1 false 0.      

       01  tab-progressivi.
           10 elemento                      occurs 99999.
              15 el-prg-chiave.
                 20 el-prg-cod-articolo   pic 9(6).
                 20 el-prg-cod-magazzino  pic x(3).
                 20 el-prg-tipo-imballo   pic x(3).
                 20 el-prg-peso           pic 9(5)v999.
              15 el-giacenza              pic s9(8).

       01  save-key.
           05 save-prg-cod-articolo         pic 9(6).
           05 save-prg-cod-magazzino        pic x(3).
           05 save-prg-tipo-imballo         pic x(3).
           05 save-prg-peso                 pic 9(5)v9(3).

       01  save-per-costo-mp.
           05 save-pcmp-cod-articolo         pic 9(6).
           05 save-pcmp-cod-magazzino        pic x(3).
           05 save-pcmp-tipo-imballo         pic x(3).
           05 save-pcmp-peso                 pic 9(5)v9(3).

       77  status-progmag        pic xx.  
       77  status-tmovmag        pic xx.
       77  status-rmovmag        pic xx.  
       77  status-tparamge       pic xx.
       77  status-clienti        pic xx.
       77  status-articoli       pic xx.
       77  status-timposte       pic xx.
       77  status-tcaumag        pic xx.
       77  status-tmp-progmag    pic xx.
       77  status-tmp-cs         pic xx.
       77  status-tmagaz         pic xx.
       77  status-lineseq        pic xx.

       77  path-tmp-progmag      pic x(512).
       77  path-tmp-cs           pic x(512).
       77  wstampa               pic x(512).


       LINKAGE SECTION.
           copy "link-inventario.def".

      ******************************************************************
       PROCEDURE DIVISION USING inventario-linkage.

       DECLARATIVES.
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag 
           when "39"
                set errori to true
                display message "File [PROGMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [PROGMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag
           when "39"
                set errori to true
                display message "File [TMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TMOVMAG] inesistente"
                        title = titolo
                        icon 2
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
           when "39"
                set errori to true
                display message "File [RMOVMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RMOVMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [RMOVMAG] inesistente"
                        title = titolo
                        icon 2
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
           when "39"
                set errori to true
                display message "File [TPARAMGE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPARAMGE] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TPARAMGE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       END DECLARATIVES.

       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              move spaces to tge-chiave
              read tparamge no lock
              set TrattamentoGDO to false
              accept imp-data from century-date
              start timposte key <= imp-chiave
                    invalid continue
                not invalid
                    read timposte previous
              end-start


              evaluate inv-chiamata
              when 1 perform ELABORAZIONE-1
              when 2 perform ELABORAZIONE-2
              when 3 perform ELABORAZIONE-3
              when 4 perform ELABORAZIONE-4
              end-evaluate

              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set RecLocked to false.
           set tutto-ok  to true.
           move 0 to idx tot-idx numero-carico numero-scarico.
           move spaces to como-mag como-cau.
           accept esercizio-x  from environment "ESERCIZIO".
           move   esercizio-x to esercizio.
           initialize path-tmp-progmag.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-tmp-progmag from environment "PATH_ST".
           inspect path-tmp-progmag replacing trailing 
                                    spaces by low-value.
           string path-tmp-progmag delimited low-value
                  "TMP_PROGMAG_"   delimited size
                  como-data        delimited size
                  "_"              delimited size
                  como-ora         delimited size
                  ".tmp"           delimited size
                  into path-tmp-progmag
           end-string.


           accept  path-tmp-cs from environment "PATH_ST".
           inspect path-tmp-cs replacing trailing 
                                    spaces by low-value.
           string path-tmp-cs delimited low-value
                  "TMP_CS_"   delimited size
                  como-data   delimited size
                  "_"         delimited size
                  como-ora    delimited size
                  ".tmp"      delimited size
                  into path-tmp-cs
           end-string.

      ***---
       OPEN-FILES.
           open input progmag tparamge clienti articoli 
                      timposte tcaumag tmagaz
           if tutto-ok
              perform OPEN-IO-TMOVMAG
              if tutto-ok
                 perform OPEN-IO-RMOVMAG
                 if errori
                    close tmovmag progmag tparamge clienti tmagaz
                          articoli timposte tcaumag
                 end-if
              else
                 close progmag tparamge clienti articoli 
                       timposte tcaumag tmagaz
              end-if
           else
              goback
           end-if.

      ***---
       OPEN-IO-TMOVMAG.
           string   "Il file delle testate di magazzino" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "tmovmag" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o tmovmag
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform.

      ***---
       OPEN-IO-RMOVMAG.
           string   "Il file delle righe di magazzino" 
             x"0d0a""risulta in uso su altro terminale."
                 delimited size
                 into geslock-messaggio
           end-string.
           move "rmovmag" to geslock-nome-file.

           set tutto-ok   to true.
           perform until 1 = 2
              set RecLocked to false
              open i-o rmovmag
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when termina 
                   set errori to true
                   exit perform
              end-evaluate
           end-perform. 
      
      ***---
       ELABORAZIONE-1.
           open output tmp-progmag.
           close       tmp-progmag.
           open i-o    tmp-progmag.

           perform GIACENZA-INIZIALE-SU-TMP.

           move low-value                 to tmo-rec.
           add 1 to tge-data-consolid-progmag giving tmo-data-movim.
           start tmovmag key >= k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag next at end exit perform end-read
                    if tmo-data-movim > inv-data
                       exit perform
                    end-if
                    move tmo-causale to tca-codice
                    read tcaumag no lock 
                         invalid continue
                     not invalid 
                         if tca-cod-magaz = inv-mag-s
                            perform LOOP-RIGHE
                         end-if
                    end-read
                 end-perform
           end-start.

           |Quelli appartenenti al magazzini di scarico e con giacenza 
           |dinamica positiva, dovranno essere scaricati tornando a 
           |giacenza 0 e stornati da 1 movimento di carico complessivo 
           move low-value to tmp-prg-rec.
           move inv-mag-s to tmp-prg-cod-magazzino.
           start tmp-progmag key >= tmp-k-mag
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-progmag next at end exit perform end-read
                    if tmp-prg-cod-magazzino not = inv-mag-s
                       exit perform
                    end-if
                    
                    perform CONTATORE-SCREEN
                    if tmp-prg-giacenza > 0
                       add 1 to idx
                       move tmp-prg-giacenza to el-giacenza(idx)
                       move tmp-prg-chiave   to el-prg-chiave(idx) 
                                                   prg-chiave
                       read progmag no lock
                       compute link-peso-tot = 
                               link-peso-tot + 
                             ( tmp-prg-giacenza  * prg-peso-utf )
                    end-if
                 end-perform
           end-start.

           if idx not = 0
              move 0 to counter counter2
              move idx to tot-idx
              perform varying idx from 1 by 1 
                        until idx > tot-idx
                 perform CONTATORE-SCREEN
                 move el-prg-chiave(idx) to prg-chiave
                 read progmag no lock
                 move inv-mag-s        to como-mag
                 move inv-cau-s        to como-cau
                 move el-giacenza(idx) to como-qta
                 if numero-scarico = 0
                    move "F"              to como-tipo
                    move tge-forn-corrisp to como-cod-clifor
                                             cli-codice
                    set cli-tipo-F to true
                    read clienti no lock
                    move cli-pag to como-codpag
                    perform VALORIZZA-NUMERO
                    move link-numero to numero-scarico
                 end-if
                 perform CREA-RIGA-MAGAZZINO
              end-perform
                                        
              move 0 to counter counter2 riga
              perform varying idx from 1 by 1
                        until idx > tot-idx
                 perform CONTATORE-SCREEN

                 initialize prg-chiave 
                            save-key   replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 move 0 to save-giacenza
                 move el-prg-cod-articolo(idx) to prg-cod-articolo
                 move inv-mag-c                to prg-cod-magazzino
                 start progmag key >= prg-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read progmag next at end exit perform end-read
                          if   prg-cod-articolo  not = 
                            el-prg-cod-articolo(idx) or
                               prg-cod-magazzino not = inv-mag-c
                             exit perform
                          end-if
                          if save-prg-peso = 0
                             move prg-chiave to save-key
                          end-if
                          if prg-attivo and prg-giacenza > save-giacenza
                             move prg-chiave to save-key
                          end-if
                       end-perform
                 end-start
                 move save-key to prg-chiave
                 read progmag no lock
                 move inv-mag-c        to como-mag
                 move inv-cau-c        to como-cau
                 move el-giacenza(idx) to como-qta
                 if numero-carico = 0
                    move "F"              to como-tipo
                    move tge-forn-corrisp to como-cod-clifor
                                             cli-codice
                    set cli-tipo-F to true
                    read clienti no lock
                    move cli-pag to como-codpag
                    perform VALORIZZA-NUMERO
                    move link-numero to numero-carico
                 end-if   
                 perform CREA-RIGA-MAGAZZINO
              end-perform
           end-if.

           close       tmp-progmag.
           delete file tmp-progmag.

      ***---
       VALORIZZA-NUMERO.
           initialize tmo-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move esercizio    to link-anno tmo-anno.
           set  link-gmovmag to true.        
           set  link-crea    to true.

           call   "nambar" using link-nambar.
           cancel "nambar".

           if link-status-nambar = -1 set errori         to true
           else                       move link-numero   to tmo-numero
                                      move link-peso-tot to tmo-peso-utf
           end-if.

           move inv-data        to tmo-data-movim.
           move como-cau        to tmo-causale.
           move como-tipo       to tmo-tipo.
           move como-cod-clifor to tmo-cod-clifor.
           move como-codpag     to tmo-codpag.
           set tmo-attivo to true.
           accept tmo-data-creazione from century-date.
           accept tmo-ora-creazione  from time.
           move inv-user  to tmo-utente-creazione
           move esercizio to tmo-esercizio.
           write tmo-rec invalid continue end-write.

      ***---
       CREA-RIGA-MAGAZZINO.
           add 1 to riga.
           initialize rmo-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           move riga       to rmo-riga.
           move tmo-tipo   to rmo-tipo.
           move tmo-cod-clifor to rmo-cod-clifor.
           move como-cau   to rmo-causale.
           move inv-data   to rmo-data-movim.
           move prg-chiave to rmo-chiave-progmag.
           move prg-cod-articolo to art-codice.
           read articoli no lock invalid continue end-read.
           move art-unita-di-misura to rmo-udm.
           move como-qta            to rmo-qta.
           |PRENDO IL PADRE PER IL COSTO MP
           move prg-chiave          to save-per-costo-mp.
           move spaces to prg-cod-magazzino.
           move spaces to prg-tipo-imballo.
           move 0      to prg-peso.
           perform CALCOLA-COSTO-MP.
           move costo-mp to rmo-listino.
           move save-per-costo-mp to prg-chiave.
           read progmag no lock invalid continue end-read.
           |RECUPERO IL FIGLIO
           move rmo-listino to rmo-netto.
           move 0 to rmo-imp-cons rmo-coubat.

           move art-peso-standard to rmo-peso-udm.
           compute rmo-peso-tot-utf = prg-peso-utf * rmo-qta.
           compute rmo-peso-tot     = prg-peso     * rmo-qta.
           move art-marca-prodotto to rmo-marca-prodotto.
           set rmo-attivo to true.
           accept rmo-data-creazione from century-date.
           accept rmo-ora-creazione  from time.
           move inv-user to rmo-utente-creazione.
           write rmo-rec invalid continue end-write.

           initialize link-wprogmag.
           set link-update-um      to true.
           set link-update-peso    to false.
           set link-update-valore  to false.
           move "0000000000000000" to link-array.
           move 1 to multiplyer(1).
           set  link-update     to true.
           move prg-chiave      to link-key.
           move tmo-causale     to link-causale.
           move rmo-qta         to link-valore.
           move inv-user        to link-user of link-wprogmag.
           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".
      
      ***---
       ELABORAZIONE-2.
           open output tmp-progmag.
           close       tmp-progmag.
           open i-o    tmp-progmag.

           open output tmp-cs.
           close       tmp-cs.
           open i-o    tmp-cs.

           perform GIACENZA-INIZIALE-SU-TMP.

           move low-value                 to tmo-rec.
           add 1 to tge-data-consolid-progmag giving tmo-data-movim.
           start tmovmag key >= k-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmovmag next at end exit perform end-read
                    if tmo-data-movim > inv-data
                       exit perform
                    end-if
                    move tmo-causale to tca-codice
                    read tcaumag no lock 
                         invalid continue
                     not invalid 
                         if tca-cod-magaz = inv-mag-azz
                            perform LOOP-RIGHE
                         end-if
                    end-read
                 end-perform
           end-start.

           move 0 to counter counter2.
           move low-value to tmp-prg-rec.
           start tmp-progmag key >= tmp-prg-chiave
                 invalid continue
             not invalid
                 set trovato-negativo to false
                 move 0      to save-articolo
                 move spaces to save-magazzino
                 move 1 to idx
                 move 0 to giac-pos
                 move 0 to giac-neg
                 move 1 to tot-idx
                 perform until 1 = 2
                    read tmp-progmag next 
                         at end 
                         move giac-neg to giac-neg-assoluta
                         if giac-pos >= giac-neg-assoluta and 
                            trovato-negativo
                            perform CREA-TMP-CARICO-SCARICO
                         end-if
                         exit perform 
                    end-read

                    perform CONTATORE-SCREEN
                    if save-articolo = 0
                       move tmp-prg-cod-articolo  to save-articolo
                       move tmp-prg-cod-magazzino to save-magazzino
                    end-if
                    if tmp-prg-cod-articolo  not = save-articolo or
                       tmp-prg-cod-magazzino not = save-magazzino
                       move giac-neg to giac-neg-assoluta
                       if giac-pos >= giac-neg-assoluta and 
                          trovato-negativo
                          perform CREA-TMP-CARICO-SCARICO
                       end-if
                       move 1 to idx
                       set trovato-negativo to false
                       move 0 to giac-pos
                       move 0 to giac-neg
                       move 1 to tot-idx
                       initialize tab-progressivi
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move tmp-prg-cod-articolo  to save-articolo
                       move tmp-prg-cod-magazzino to save-magazzino
                    end-if
                    move tmp-prg-chiave   to el-prg-chiave(idx)
                    move tmp-prg-giacenza to el-giacenza(idx)
                    if tmp-prg-giacenza < 0
                       set trovato-negativo to true
                    end-if
                    if tmp-prg-giacenza < 0
                       add tmp-prg-giacenza to giac-neg
                    else
                       add tmp-prg-giacenza to giac-pos
                    end-if
                    add 1 to idx tot-idx

                 end-perform
           end-start.

           perform TMP-TO-MAGAZZINO.

           close       tmp-progmag.
           delete file tmp-progmag.

           close       tmp-cs.
           delete file tmp-cs.

      ***---
       GIACENZA-INIZIALE-SU-TMP.
           if inv-chiamata = 1
              move low-value to prg-rec
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       |SOLO I FIGLI
                       if prg-peso not = 0
                          move prg-chiave  to tmp-prg-chiave
                          move prg-ini-udm to tmp-prg-giacenza
                          write tmp-prg-rec
                       end-if
                    end-perform
              end-start
           else
              move low-value   to prg-rec
              move inv-mag-azz to prg-cod-magazzino
              start progmag key >= key01
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-magazzino not = inv-mag-azz
                          exit perform 
                       end-if
                       |SOLO I FIGLI
                       if prg-peso not = 0
                          move prg-chiave  to tmp-prg-chiave
                          move prg-ini-udm to tmp-prg-giacenza
                          write tmp-prg-rec
                       end-if
                    end-perform
              end-start
           end-if.

      ***---
       TMP-TO-MAGAZZINO.
           move low-value to tcs-rec.
           start tmp-cs key >= tcs-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-cs next at end exit perform end-read
                    move "F"                 to como-tipo
                    move tge-forn-corrisp    to como-cod-clifor
                                                cli-codice
                    set cli-tipo-F to true
                    read clienti no lock
                    move cli-pag       to como-codpag
                    move tcs-magazzino to como-mag
                    move tcs-causale   to como-cau
                    move tcs-tot-utf   to link-peso-tot
                    perform VALORIZZA-NUMERO

                    perform RESET-CONTATORE
                    move 0 to riga

                    perform varying idx-tmp from 1 by 1 until 1 = 2
                       if tcs-prg-cod-articolo(idx-tmp) = 0
                          exit perform
                       end-if
                       perform CONTATORE-SCREEN
                       move tcs-prg-chiave(idx-tmp) to prg-chiave
                       read progmag no lock
                       move tcs-qta(idx-tmp) to como-qta
                       perform CREA-RIGA-MAGAZZINO
                    end-perform
                 end-perform
           end-start.

      ***---
       LOOP-RIGHE.
           move low-value  to rmo-rec.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.
           start rmovmag key >= rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno  not = tmo-anno or
                       rmo-movim not = tmo-numero
                       exit perform
                    end-if
                    perform CONTATORE-SCREEN
                    move rmo-chiave-progmag to tmp-prg-chiave
                    read tmp-progmag no lock
                    evaluate true
                    when tca-movim-giac-pos
                         add rmo-qta        to tmp-prg-giacenza
                    when tca-movim-giac-neg
                         subtract rmo-qta from tmp-prg-giacenza
                    end-evaluate
                    rewrite tmp-prg-rec
                 end-perform
           end-start.

      ***---
       CREA-TMP-CARICO-SCARICO. 
           move el-prg-cod-magazzino(1) to mag-codice.
           read tmagaz no lock.

           perform varying idx from 1 by 1 
                     until idx > tot-idx
              if giac-neg = 0
                 exit perform
              end-if
              move mag-codice to tcs-magazzino
              if el-giacenza(idx) < 0
                 move mag-cau-c          to tcs-causale
                 perform TROVA-IDX-TMP
                 move el-giacenza(idx)   to tcs-qta(idx-tmp)
                 move el-prg-chiave(idx) to tcs-prg-chiave(idx-tmp)
                                                prg-chiave
                 read progmag no lock
                 compute tcs-tot-utf =
                         tcs-tot-utf + tcs-qta(idx-tmp) * prg-peso-utf
              end-if
              if el-giacenza(idx) > 0
                 move mag-cau-s            to tcs-causale
                 perform TROVA-IDX-TMP
                 if el-giacenza(idx) > giac-neg
                    move giac-neg   to tcs-qta(idx-tmp)
                    move 0 to giac-neg
                 else
                    move el-giacenza(idx) to tcs-qta(idx-tmp)
                    add  el-giacenza(idx) to giac-neg 
                 end-if
                 move el-prg-chiave(idx) to tcs-prg-chiave(idx-tmp)
                                                prg-chiave
                 read progmag no lock
                 compute tcs-tot-utf =
                         tcs-tot-utf + tcs-qta(idx-tmp) * prg-peso-utf
              end-if
              write tcs-rec invalid rewrite tcs-rec end-write
           end-perform.

      ***---
       TROVA-IDX-TMP.
           read tmp-cs no lock 
                invalid 
                initialize tcs-dati replacing numeric data by zeroes
                                         alphanumeric data by spaces
                move 1 to idx-tmp
            not invalid
                perform varying idx-tmp from 1 by 1 until 1 = 2
                   if tcs-prg-cod-articolo(idx-tmp) = 0
                      exit perform
                   end-if
                end-perform

           end-read.

      ***---
       ELABORAZIONE-3.
           open output tmp-cs.
           close       tmp-cs.
           open i-o    tmp-cs.

           move inv-file1 to wstampa.
           open input lineseq.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              unstring line-riga 
                       delimited by ";"
                            into como-articolo
                                 quantita-x
                                 como-imballo
              end-unstring
              unstring line-riga 
                       delimited by ";"
                            into como-articolo
                                 como-quantita
                                 como-imballo
              end-unstring
              inspect como-quantita replacing all x"3D" by x"30"
              call "C$JUSTIFY" using quantita-x, "L"
              move como-quantita to save-quantita
              initialize prg-chiave replacing numeric data by zeroes
                                         alphanumeric data by spaces
              
              if quantita-x(1:1) = "-"
                 move como-articolo to prg-cod-articolo
                 move inv-mag-s3    to prg-cod-magazzino
                 move como-imballo  to prg-tipo-imballo
                 start progmag key >= prg-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read progmag next
                               at end
                               exit perform
                          end-read
                          perform CONTATORE-SCREEN
                          if prg-cod-articolo  not = como-articolo or
                             prg-cod-magazzino not = inv-mag-s3    or
                             prg-tipo-imballo  not = como-imballo  or
                             como-quantita         = 0
                             exit perform
                          end-if
                          if prg-giacenza > 0
                             move prg-cod-magazzino  to mag-codice
                                                        tcs-magazzino
                             read tmagaz no lock
                             move mag-cau-s          to tcs-causale
                             perform TROVA-IDX-TMP
                             if prg-giacenza >= como-quantita
                                move como-quantita to tcs-qta(idx-tmp)
                                move 0 to como-quantita
                             else
                                move prg-giacenza to tcs-qta(idx-tmp)
                                subtract prg-giacenza from como-quantita
                             end-if
                             move prg-chiave to tcs-prg-chiave(idx-tmp)
                             compute tcs-tot-utf =
                                     tcs-tot-utf + 
                                     tcs-qta(idx-tmp) * prg-peso-utf
                             write tcs-rec 
                                   invalid rewrite tcs-rec 
                             end-write
                          end-if
                       end-perform
                 end-start
              else
                 move -99999999 to save-giacenza3
                 initialize save-key   replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 move como-articolo to prg-cod-articolo
                 move inv-mag-c3    to prg-cod-magazzino
                 move como-imballo  to prg-tipo-imballo
                 start progmag key >= prg-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read progmag next 
                               at end
                               exit perform 
                          end-read
                          perform CONTATORE-SCREEN
                          if prg-cod-articolo  not = como-articolo or
                             prg-cod-magazzino not = inv-mag-c3    or
                             prg-tipo-imballo  not = como-imballo
                             exit perform
                          end-if
                          if |prg-attivo and TOLTO RICHIESTA DI WALTER (09/01)
                             prg-giacenza >= save-giacenza3
                             move prg-giacenza to save-giacenza3
                             move prg-chiave   to save-key
                          end-if
                       end-perform
                 end-start

                 if save-prg-peso = 0 |Non ho trovato il progressivo e lo creo
                    set link-accept to true
                    move inv-user   to link-user
                    move como-articolo to link-articolo art-codice
                    read articoli no lock invalid continue end-read
                    move art-descrizione      to link-des-articolo
                    move como-imballo         to link-imballo
                    move art-peso-utf         to link-utf
                    move art-peso-non-utf     to link-non-utf
                    add link-utf to link-non-utf giving link-peso
                    call   "wprogmag" using link-wprogmag
                    cancel "wprogmag"
                    move link-key to save-key
                 end-if

                 move save-key to prg-chiave
                 read progmag  no lock
                 move prg-cod-magazzino  to mag-codice tcs-magazzino
                 read tmagaz   no lock invalid continue end-read
                 move mag-cau-c     to tcs-causale
                 perform TROVA-IDX-TMP
                 move save-quantita to tcs-qta(idx-tmp)
                 move prg-chiave    to tcs-prg-chiave(idx-tmp)
                 compute tcs-tot-utf =
                         tcs-tot-utf + 
                         tcs-qta(idx-tmp) * prg-peso-utf
                 write tcs-rec invalid rewrite tcs-rec end-write
              end-if

           end-perform.

           perform TMP-TO-MAGAZZINO.

           close       tmp-cs.
           delete file tmp-cs.

      ***---
       ELABORAZIONE-4.  
           open output tmp-cs.
           close       tmp-cs.
           open i-o    tmp-cs.

           move inv-file2 to wstampa.
           open input lineseq.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              unstring line-riga 
                       delimited by ";"
                            into save-prg-cod-articolo
                                 save-prg-cod-magazzino
                                 save-prg-tipo-imballo
                                 como-peso
                                 quantita-x
              end-unstring
              unstring line-riga 
                       delimited by ";"
                            into save-prg-cod-articolo
                                 save-prg-cod-magazzino
                                 save-prg-tipo-imballo
                                 como-peso
                                 como-quantita
              end-unstring
              inspect como-quantita replacing all x"3D" by x"30"
              divide como-peso    by 1000 giving save-prg-peso
              move save-key       to prg-chiave
              call "C$JUSTIFY" using quantita-x, "L"
              read progmag no lock
              perform CONTATORE-SCREEN
              if quantita-x(1:1) = "-"
                 perform CREA-TMP-SCARICO
              else
                 perform CREA-TMP-CARICO
              end-if
           end-perform.

           perform TMP-TO-MAGAZZINO.

           close       tmp-cs.
           delete file tmp-cs.

      ***---
       CREA-TMP-CARICO.
           move save-key to prg-chiave.
           move prg-cod-magazzino to mag-codice.
           read tmagaz no lock.
           move mag-codice to tcs-magazzino.
           move mag-cau-c  to tcs-causale.
           perform TROVA-IDX-TMP.
           move como-quantita to tcs-qta(idx-tmp).
           move prg-chiave    to tcs-prg-chiave(idx-tmp).
           compute tcs-tot-utf =
                   tcs-tot-utf + tcs-qta(idx-tmp) * prg-peso-utf.
           write tcs-rec invalid rewrite tcs-rec end-write.

      ***---
       CREA-TMP-SCARICO.
           move save-key to prg-chiave.
           move prg-cod-magazzino to mag-codice.
           read tmagaz no lock.
           move mag-codice to tcs-magazzino.
           move mag-cau-s  to tcs-causale.
           perform TROVA-IDX-TMP.
           move como-quantita to tcs-qta(idx-tmp).
           move prg-chiave    to tcs-prg-chiave(idx-tmp).
           compute tcs-tot-utf =
                   tcs-tot-utf + tcs-qta(idx-tmp) * prg-peso-utf.
           write tcs-rec invalid rewrite tcs-rec end-write.

      ***---
       CONTATORE-SCREEN.
           add 1 to counter
           add 1 to counter2
           if counter2 = 10
              move counter to counter-edit
              display counter-edit
                 upon inv-handle at column 35,00
                                      line 40,00
              move 0 to counter2
           end-if.

      ***---
       RESET-CONTATORE.
           move 0 to counter.
           move 0 to counter2.
           move counter to counter-edit
           display counter-edit
             upon inv-handle at column 35,00
                                  line 40,00.


      ***---
       CLOSE-FILES.
           close progmag tparamge tmovmag rmovmag clienti 
                 articoli timposte tcaumag tmagaz.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "costo-medio.cpy".
       RECUPERO-ANAGRAFICA. |DUMMY
