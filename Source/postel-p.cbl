       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      postel-p.
       AUTHOR.                          Andrea.
       REMARKS. Aggiornamento del numero di ultimo documento trattato
                nella tabella dei contatori TCONTAT.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "assorcli.sl".
           copy "tordini.sl".
           copy "rordini.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "tcontat.sl".
           copy "articoli.sl".
           copy "tivaese.sl".
           copy "clienti.sl".
           copy "CLI.sl".
           copy "destini.sl".
           copy "agenti.sl".
           copy "tcodpag.sl".
           copy "tvettori.sl".
           copy "recapiti.sl".
           copy "lineseq.sl".
           copy "ttipocli.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.        
           copy "assorcli.fd".
           copy "tordini.fd".
           copy "rordini.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "tcontat.fd". 
           copy "articoli.fd".
           copy "tivaese.fd".
           copy "clienti.fd".
           copy "CLI.fd".
           copy "destini.fd".
           copy "agenti.fd".
           copy "tcodpag.fd".
           copy "tvettori.fd".
           copy "recapiti.fd".
           copy "lineseq.fd".
           copy "ttipocli.fd".

       WORKING-STORAGE SECTION.
      *    COPY
           copy "postel.def".
           copy "link-geslock.def".
           copy "comune.def".
           copy "varsca".

      *    COSTANTI
       78  titolo                value "Generazione file invio Postel".
       78  78-MaxRows            value 30.

      *    FILE-STATUS
       77  status-assorcli       pic xx.
       77  status-tordini        pic xx.
       77  status-rordini        pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-tcontat        pic xx.
       77  status-tivaese        pic xx.
       77  status-clienti        pic xx.
       77  status-CLI            pic xx.
       77  status-destini        pic xx.
       77  status-agenti         pic xx.
       77  status-tcodpag        pic xx.
       77  status-articoli       pic xx.
       77  status-tvettori       pic xx.
       77  status-recapiti       pic xx.
       77  status-lineseq        pic xx.
       77  status-ttipocli       pic xx.
       77  wstampa               pic x(256).

      *    VARIABILI
       01  como-record-t.
           05 como-anno           pic 9(4).
           05 como-numero         pic 9(8).
           05 como-data-doc       pic 9(8).
           05 como-cod-cli        pic 9(5).
           05 como-prg-destino    pic 9(5).
           05 como-num-ord-cli    pic X(50).
           05 como-cod-agente     pic 9(5).
           05 como-cod-pagamento  pic x(3).
           05 como-vettore        pic 9(5).
           05 como-anno-bolla     pic 9(4).
           05 como-num-bolla      pic 9(8).
           05 como-data-bolla     pic 9(8).
           05 como-anno-fattura   pic 9(4).
           05 como-num-fattura    pic 9(8).
           05 como-data-fattura   pic 9(8).
           05 como-note-nc-fm     pic x(500). |INTESTAZIONE

       01  como-record-r.
           05 como-cod-articolo   pic 9(6).
           05 como-des-libera     pic x(150).
           05 como-qta            pic 9(8).
           05 como-prz-unitario   pic 9(9)v9(2).
           05 como-imp-consumo    pic 9(4)v9(2).
           05 como-imp-cou-cobat  pic 9(4)v9(2).
           05 como-imponib-merce  pic 9(9)v9(2).
           05 como-omaggio        pic X(1).
              88 como-si-omaggio  value "S". 
              88 como-no-omaggio  value "N". 
           05 como-cod-iva        pic x(3).

       77  como-data6             pic x(6).
       77  RowCounter             pic 9(3).
       77  visualizza-totali      pic x.

       77  codice-ed              pic z(5).

       77  righe-finali           pic 9.

       01  intestazioni-note pic x(5000).
       01  intestazione-note pic x(50)   occurs 100 
                                         redefines intestazioni-note.
       77  num-righe-note                pic 9(2) value 0.
       77  idx-sca                       pic 9(2).
       77  cont-inizio                   pic 9(3).
       77  cont-per                      pic 9(3).
       77  cont-char                     pic 9(3).

       77  importo-netto         pic 9(9)v99.
       77  imponibile-merce      pic 9(9)v99.
       77  tot-imponibile        pic 9(9)v99.
       77  tot-consumo           pic 9(9)v99.
       77  tot-cou               pic 9(9)v99.
       77  tot-iva               pic 9(9)v99.
       77  tot-fattura           pic 9(9)v99.
       77  tot-solo-cou          pic 9(9)v99.
       77  tot-cobat             pic 9(9)v99.
       77  tot-piombo            pic 9(9)v99.

       01  tabella-iva           occurs 3. 
         05 cod-iva              pic x(3).
         05 tipo-iva             pic 9.
            88 iva-sigla         value 1, false 0.
         05 imponibile-iva       pic 9(9)v99.
         05 importo-iva          pic 9(15)v99.
         05 articolo-iva         pic x(30).

       77  perce-iva-x           pic x(3).
       77  perce-iva-9di3        pic 9(3).
       77  iva-z                 pic zz.

       01 st-riga-totali.
         05 st-importo             pic zzz.zzz.zz9,99.
         05 st-aliquota-tot        pic xxx.
         05 st-imponibile          pic zzz.zzz.zz9,99 blank zero.
         05 st-importo-iva         pic z.zzz.zzz.zz9,99 blank zero.
         05 st-importo-totale      pic zzz.zzz.zz9,99.

       01 st-riga-totali2.
         05 st-importo2            pic zzz.zzz.zz9,99.
         05 st-aliquota-tot2       pic xxx.
         05 st-imponibile2         pic zzz.zzz.zz9,99 blank zero.
         05 st-articolo-iva2       pic x(16).
         05 st-importo-totale2     pic zzz.zzz.zz9,99.

       77  como-iva              pic 9(9)v999.
       77  como-iva-2dec         pic 9(9)v99.

       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  como-valore           pic zz.zz9,99.

LUBEXX 77  como-scadenzarb       pic xx/xx/xx.

      *    FLAGS
       01  filler                pic 9 value 0.
           88 RigheFinali        value 1, false 0.

       01  filler                pic 9.
           88 trovato-assorcli   value 1, false 0.

       01  filler                pic 9.
           88 prima-volta        value 1, false 0.

       01  filler                pic 9.
           88  EsisteRecapito    value 1, false 0. 

       01  filler                pic 9.
           88  EsisteDestino     value 1, false 0.

       01  filler                pic 9.
           88  RelazioneTestaGiaFatte     value 1, false 0.

       01  filler                pic 9.
           88  exit-perform-int  value 1, false 0.

       77  FlagTrovataIVA        pic 9.
           88  TrovataIVA        value 1, false 0.

       77  filler                pic 9.
           88  UsaPrimaRiga      value 1.
           88  UsaSecondaRiga    value 2.

       77  filler                pic 9.
           88  no-cou            value 0.
           88  si-cou            value 1.
       77  filler                pic 9.
           88  no-cobat          value 0.
           88  si-cobat          value 1.
       77  filler                pic 9.
           88  no-piombo         value 0.
           88  si-piombo         value 1.

       LINKAGE SECTION.
       copy "link-postel.def".

      ******************************************************************
       PROCEDURE DIVISION using postel-linkage.

       DECLARATIVES.
      ***---
       ASSORCLI-ERR SECTION.
           use after error procedure on assorcli.
           set tutto-ok  to true.
           evaluate status-assorcli
           when "35"
                display message "File [ASSORCLI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [ASSORCLI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[ASSORCLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TORDINI-ERR SECTION.
           use after error procedure on tordini.
           set tutto-ok  to true.
           evaluate status-tordini
           when "35"
                display message "File [TORDINI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TORDINI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       RORDINI-ERR SECTION.
           use after error procedure on rordini.
           set tutto-ok  to true.
           evaluate status-rordini
           when "35"
                display message "File [RORDINI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [RORDINI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[RORDINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           set tutto-ok  to true.
           evaluate status-tnotacr
           when "35"
                display message "File [TNOTACR] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           set tutto-ok  to true.
           evaluate status-rnotacr
           when "35"
                display message "File [RNOTACR] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [RNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[RNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
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
       TIVAESE-ERR SECTION.
           use after error procedure on tivaese.
           set tutto-ok  to true.
           evaluate status-tivaese
           when "35"
                display message "File [TIVAESE] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TIVAESE] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TIVAESE] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       CLI-ERR SECTION.
           use after error procedure on CLI.
           set tutto-ok  to true.
           evaluate status-cli
           when "35"
                display message "File [CLI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [CLI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[CLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "File [CLIENTI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       DESTINI-ERR SECTION.
           use after error procedure on destini.
           set tutto-ok  to true.
           evaluate status-destini
           when "35"
                display message "File [DESTINI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [DESTINI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[DESTINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       AGENTI-ERR SECTION.
           use after error procedure on agenti.
           set tutto-ok  to true.
           evaluate status-agenti
           when "35"
                display message "File [AGENTI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [AGENTI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[AGENTI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TCODPAG-ERR SECTION.
           use after error procedure on tcodpag.
           set tutto-ok  to true.
           evaluate status-tcodpag
           when "35"
                display message "File [TCODPAG] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TCODPAG] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TCODPAG] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       TVETTORI-ERR SECTION.
           use after error procedure on tvettori.
           set tutto-ok  to true.
           evaluate status-tvettori
           when "35"
                display message "File [TVETTORI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TVETTORI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TVETTORI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.
 
      ***---
       RECAPITI-ERR SECTION.
           use after error procedure on recapiti.
           set tutto-ok  to true.
           evaluate status-recapiti
           when "35"
                display message "File [RECAPITI] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [RECAPITI] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[RECAPITI] Indexed file corrupt!"
                           title titolo
                            icon 3
                set errori to true
           end-evaluate.

      ***---
       TCONTAT-ERR SECTION.
           use after error procedure on tcontat.
           set tutto-ok  to true.
           evaluate status-tcontat
           when "35"
                display message "File [TCONTAT] not found!"
                           title titolo
                            icon 3
                set errori to true
           when "39"
                display message "File [TCONTAT] Mismatch size!"
                           title titolo
                            icon 3
                set errori to true
           when "98"
                display message "[TCONTAT] Indexed file corrupt!"
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
                move   "tcontat"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform OPEN-IO-TCONTAT
                when termina
                     set errori to true
                     display message "Operazione interrotta!"
                               title titolo
                                icon 2
                end-evaluate
           when "99"
                initialize geslock-messaggio
                string   "Il record per l'anno: " con-anno
           x"0d0a""risulta in uso su altro terminale."    delimited size
           x"0d0a""Questo comporta l'impossibiltà ad"     delimited size
           x"0d0a""aggiornare la tabella dei contatori."  delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "tcontat"    to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform READ-TCONTAT-LOCK
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
           when "93"
                initialize geslock-messaggio
                string   "File già in uso!"
                  x"0d0a""Impossibile procedere!" delimited size
                      into geslock-messaggio
                end-string
                move 1 to geslock-v-riprova
                move 0 to geslock-v-ignora
                move 1 to geslock-v-termina
                move   "File TXT"   to geslock-nome-file
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
           end-evaluate.

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              if postel-tipo-doc = 1 perform ELABORAZIONE-FATTURE
              else                   perform ELABORAZIONE-NC
              end-if
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to postel-tot-doc.
           move 0 to postel-doc-da.
           move 0 to postel-doc-a.
           move 0 to counter counter2.
           set tutto-ok     to true.
           set prima-volta  to true.
           accept visualizza-totali from environment "VISUALIZZA_TOTALI"
           initialize wstampa.
           accept  wstampa from environment "PATH_POSTEL".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa delimited low-value
                   "fatture.txt" delimited size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-IO-TCONTAT.
           if tutto-ok
              perform READ-TCONTAT-LOCK
              if tutto-ok
                 if postel-tipo-doc = 1
                    open input tordini rordini
                 else
                    open input tnotacr rnotacr
                 end-if
                 open input tivaese  assorcli  clienti  recapiti  agenti
                            tcodpag  articoli  destini  tvettori  
                            CLI ttipocli

                 if errori
                    close tcontat
                 end-if
              else
                 close tcontat
              end-if
           end-if.

      ***---
       OPEN-IO-TCONTAT.
           open i-o tcontat.

      ***---
       READ-TCONTAT-LOCK.
           move postel-anno  to con-anno.
           read tcontat lock invalid set errori to true end-read.

      ***---
       ELABORAZIONE-FATTURE.
           move postel-anno   to tor-anno-fattura.
           move postel-num-da to tor-num-fattura.
           start tordini key  is >= k-fattura
                 invalid
                 display message "Nessun documento da inviare!"
                           title titolo
                            icon 2
                 set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tordini next at end exit perform end-read
                 if tor-num-fattura > postel-num-a
                    exit perform
                 end-if
                 if tor-anno-fattura not = postel-anno
                    exit perform
                 end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon postel-handle at column 15
                                               line 03
                    move 0 to counter2
                 end-if

                 if tor-invio-postel
                    initialize como-record-t replacing numeric by zeroes
                                                  alphanumeric by spaces
                    move tor-anno          to como-anno
                    move tor-numero        to como-numero
                    move tor-data-ordine   to como-data-doc
                    move tor-cod-cli       to como-cod-cli      
                    move tor-prg-destino   to como-prg-destino  
                    move tor-num-ord-cli   to como-num-ord-cli  
                    move tor-cod-agente    to como-cod-agente   
                    move tor-cod-pagamento to como-cod-pagamento
                    move tor-vettore       to como-vettore      
                    move tor-anno-bolla    to como-anno-bolla   
                    move tor-num-bolla     to como-num-bolla    
                    move tor-data-bolla    to como-data-bolla   
                    move tor-anno-fattura  to como-anno-fattura 
                    move tor-num-fattura   to como-num-fattura  
                    move tor-data-fattura  to como-data-fattura 
                    move tor-note          to como-note-nc-fm
                    set  RelazioneTestaGiaFatte to false
                    perform LOOP-RIGHE-RORDINI
                    |NON SONO RIUSCITO AD APRIRE IL FILE DI TESTO
                    if errori exit perform end-if

                    if trovato
                       add 1 to postel-tot-doc
                       if tor-num-fattura < postel-doc-da
                          move tor-num-fattura to postel-doc-da
                       end-if
                       if tor-num-fattura > postel-doc-a
                          move tor-num-fattura to postel-doc-a
                       end-if
                       perform VALORIZZA-POSTEL-TOTALI
                       perform VALORIZZA-POSTEL-CONAI
                       perform VALORIZZA-POSTEL-PIEDE
                       perform VALORIZZA-POSTEL-SCADENZE
                    end-if
                 end-if
                 if errori exit perform end-if
              end-perform
           end-if.

           if tutto-ok
              if postel-tot-doc > 0
                 move postel-doc-a to con-ult-num-postel
                 rewrite con-rec invalid continue end-rewrite
              else
                 display message "Nessun documento da inviare!"
                           title titolo
                            icon 2
              end-if
           end-if.

      ***---
       LOOP-RIGHE-RORDINI.
           if visualizza-totali = "S"
              |Guardo i totali delle imposte in maniera preventiva
              |x' devo sapere prima uante righe di totalizzatori
              |finali dovrò poi stampare
              move low-values   to ror-rec
              move tor-anno     to ror-anno
              move tor-numero   to ror-num-ordine
              start rordini key is >= ror-chiave
                    invalid set errori to true
                not invalid
                    perform until 1 = 2
                       read rordini next at end exit perform end-read
                       if ror-anno       not = tor-anno or
                          ror-num-ordine not = tor-numero
                          exit perform
                       end-if
                       move ror-cod-articolo to art-codice
                       read articoli no lock invalid continue end-read
                       if ror-add-piombo > 0
                          set si-piombo to true
                          compute tot-piombo = 
                                  tot-piombo +
                                  ror-add-piombo  * 
                                ( ror-qta - ror-qta-omaggi )
                       end-if
                       if ror-imp-cou-cobat > 0
                          if art-si-cobat
                             set si-cobat to true
                             compute tot-cobat = 
                                     tot-cobat +
                                     ror-imp-cou-cobat * 
                                   ( ror-qta - ror-qta-omaggi )
                          else
                             set si-cou   to true
                             compute tot-solo-cou = 
                                     tot-solo-cou +
                                     ror-imp-cou-cobat * 
                                   ( ror-qta - ror-qta-omaggi )
                          end-if
                       end-if
                    end-perform
              end-start
              if si-piombo add 1 to righe-finali end-if
              if si-cou    add 1 to righe-finali end-if
              if si-cobat  add 1 to righe-finali end-if
           else
              move 0 to righe-finali
           end-if.

           move 0            to RowCounter.
           set  trovato      to false.
           move low-values   to ror-rec.
           move tor-anno     to ror-anno.
           move tor-numero   to ror-num-ordine.
           start rordini key is >= ror-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rordini next at end exit perform end-read
                 if tor-anno   not = ror-anno  or
                    tor-numero not = ror-num-ordine
                    exit perform
                 end-if
                 if postel-doc-da = 0
                    move tor-num-fattura to postel-doc-da
                 end-if
                 if postel-doc-a = 0
                    move tor-num-fattura to postel-doc-a
                 end-if
                 if prima-volta
                    set  prima-volta to false
                    open output lineseq
                    if errori exit perform end-if
                 end-if
                 initialize como-record-r replacing numeric by zeroes
                                               alphanumeric by spaces
                 move ror-cod-articolo  to como-cod-articolo 
                 move ror-des-libera    to como-des-libera   
                 move ror-qta           to como-qta          
                 move ror-prz-unitario  to como-prz-unitario 
                 move ror-imp-consumo   to como-imp-consumo
                 add  ror-imp-cou-cobat to ror-add-piombo
                 giving como-imp-cou-cobat
                 move ror-imponib-merce to como-imponib-merce
                 move ror-omaggio       to como-omaggio
                 move ror-cod-iva       to como-cod-iva
                 set  trovato           to true
OMAGGI           if ror-qta-omaggi not = 0
OMAGGI              subtract ror-qta-omaggi from como-qta
OMAGGI              perform VALORIZZA-POSTEL
OMAGGI              set como-si-omaggio to true
OMAGGI              move ror-qta-omaggi to como-qta
OMAGGI              perform VALORIZZA-POSTEL
                 else
                    perform VALORIZZA-POSTEL
OMAGGI           end-if
              end-perform
           end-if.

      ***---
       ELABORAZIONE-NC.
           move postel-anno   to tno-anno-fattura.
           move postel-num-da to tno-num-fattura.
           start tnotacr key  is >= k-fattura
                 invalid
                 display message "Nessun documento da inviare!"
                           title titolo
                            icon 2
                 set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read tnotacr next at end exit perform end-read
                 if tno-num-fattura > postel-num-a
                    exit perform
                 end-if
                 if tno-anno-fattura not = postel-anno
                    exit perform
                 end-if
                 if tno-invio-postel
                    initialize como-record-t replacing numeric by zeroes
                                                  alphanumeric by spaces
                    move tno-anno          to como-anno  
                    move tno-numero        to como-numero
                    move tno-data          to como-data-doc
                    move tno-cod-cli       to como-cod-cli      
                    move tno-prg-destino   to como-prg-destino
                    move tno-cod-agente    to como-cod-agente   
                    move tno-cod-pagamento to como-cod-pagamento
                    move tno-anno-fattura  to como-anno-fattura 
                    move tno-num-fattura   to como-num-fattura  
                    move tno-data-fattura  to como-data-fattura 
                    move tno-note          to como-note-nc-fm
                    set  RelazioneTestaGiaFatte to false
                    perform LOOP-RIGHE-RNOTACR
                    |NON SONO RIUSCITO AD APRIRE IL FILE DI TESTO
                    if errori exit perform end-if

                    if trovato
                       add 1 to postel-tot-doc
                       if tno-num-fattura < postel-doc-da
                          move tno-num-fattura to postel-doc-da
                       end-if
                       if tno-num-fattura > postel-doc-a
                          move tno-num-fattura to postel-doc-a
                       end-if
                       perform VALORIZZA-POSTEL-TOTALI
                       perform VALORIZZA-POSTEL-CONAI
                       perform VALORIZZA-POSTEL-PIEDE
                       perform VALORIZZA-POSTEL-SCADENZE
                    end-if
                 end-if
                 if errori exit perform end-if
              end-perform
           end-if.

           if tutto-ok
              if postel-tot-doc > 0
                 move postel-doc-a to con-ult-num-nc-postel
                 rewrite con-rec invalid continue end-rewrite
              else
                 display message "Nessun documento da inviare!"
                           title titolo
                            icon 2
              end-if
           end-if.

      ***---
       LOOP-RIGHE-RNOTACR.
           if visualizza-totali = "S" and righe-finali > 1
              |Guardo i totali delle imposte in maniera preventiva
              |x' devo sapere prima uante righe di totalizzatori
              |finali dovrò poi stampare
              move low-values   to rno-rec
              move tno-anno     to rno-anno
              move tno-numero   to rno-numero
              start rnotacr key is >= rno-chiave
                    invalid set errori to true
                not invalid
                    perform until 1 = 2
                       read rnotacr  next at end exit perform end-read
                       if rno-anno   not = tno-anno or
                          rno-numero not = tno-numero
                          exit perform
                       end-if
                       move rno-cod-articolo to art-codice
                       read articoli no lock invalid continue end-read
                       if rno-add-piombo > 0
                          set si-piombo to true
                          compute tot-piombo = 
                                  tot-piombo +
                                  rno-add-piombo  * rno-qta
                       end-if
                       if rno-imp-cou-cobat > 0
                          if art-si-cobat
                             set si-cobat to true
                             compute tot-cobat = 
                                     tot-cobat +
                                     rno-imp-cou-cobat * rno-qta
                          else
                             set si-cou   to true
                             compute tot-solo-cou = 
                                     tot-solo-cou +
                                     rno-imp-cou-cobat * rno-qta
                          end-if
                       end-if
                    end-perform
              end-start
              if si-piombo add 1 to righe-finali end-if
              if si-cou    add 1 to righe-finali end-if
              if si-cobat  add 1 to righe-finali end-if
           else
              move 0 to righe-finali
           end-if.

           move 0            to RowCounter.
           set  trovato      to false.
           move low-values   to rno-rec.
           move tno-anno     to rno-anno.
           move tno-numero   to rno-numero.
           start rnotacr key is >= rno-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read rnotacr next at end exit perform end-read
                 if tno-anno   not = rno-anno  or
                    tno-numero not = rno-numero
                    exit perform
                 end-if
                 if postel-doc-da = 0
                    move tno-num-fattura to postel-doc-da
                 end-if
                 if postel-doc-a = 0
                    move tno-num-fattura to postel-doc-a
                 end-if
                 if prima-volta
                    set  trovato     to true
                    set  prima-volta to false
                    open output lineseq
                    if errori exit perform end-if
                 end-if
                 initialize como-record-r replacing numeric by zeroes
                                               alphanumeric by spaces
                 move rno-cod-articolo  to como-cod-articolo 
                 move rno-des-libera    to como-des-libera   
                 move rno-qta           to como-qta          
                 move rno-prz-unitario  to como-prz-unitario 
                 move rno-prz-unitario  to como-imponib-merce
                 move rno-imp-consumo   to como-imp-consumo
                 add  rno-imp-cou-cobat to rno-add-piombo
                 giving como-imp-cou-cobat
                 move rno-cod-iva       to como-cod-iva
                 if rno-prz-unitario  = 0
                    set como-si-omaggio to true
                 else
                    set como-no-omaggio to true
                 end-if
                 set trovato to true
                 perform VALORIZZA-POSTEL
              end-perform
           end-if.

      ***---
       VALORIZZA-POSTEL.
           if not RelazioneTestaGiaFatte
              move 0 to num-righe-note

              move 0 to  tot-imponibile tot-consumo
                         tot-cou        tot-fattura
                         tot-iva        imponibile-merce

              initialize cli-rec
                         rec-rec
                         record-tblpa
                         vet-rec
                         age-rec
                         replacing numeric by zeroes
                              alphanumeric by spaces

              set RelazioneTestaGiaFatte to true
              move como-cod-cli     to cli-codice rec-codice des-codice
              move como-prg-destino to des-prog
              set  cli-tipo-C    to true
              read clienti  no lock invalid continue end-read

              move cli-codice to cli-codice-G2
              read CLI no lock invalid continue end-read

              set des-no-invio to true

              read destini  no lock
                   invalid  set EsisteDestino to false
               not invalid  set EsisteDestino to true
              end-read

              if des-si-invio
                 set EsisteRecapito to true
                 move des-ragsoc-1  to rec-ragsoc-1
                 move des-indirizzo to rec-indirizzo
                 move des-cap       to rec-cap
                 move des-localita  to rec-localita
                 move des-prov      to rec-provincia
              else
                 read recapiti no lock
                      invalid  set EsisteRecapito to false
                  not invalid  
                      if rec-ragsoc-1       = spaces and
                         rec-indirizzo      = spaces and
                         rec-cap            = spaces and
                         rec-localita       = spaces and
                         rec-provincia      = spaces
                         set EsisteRecapito to false
                      else
                         set EsisteRecapito to true
                      end-if
                 end-read
              end-if

              move "PA"               to tblpa-codice1
              move como-cod-pagamento to tblpa-codice2
              read tcodpag no lock invalid continue end-read

              move como-vettore to vet-codice
              read tvettori     no lock invalid continue end-read

              move como-cod-agente to age-codice
              read agenti          no lock invalid continue end-read

              if EsisteRecapito
                 move rec-ragsoc-1       to ragio-dx
                 move rec-indirizzo      to indi-dx
                 move rec-cap            to cap-dx
                 move rec-localita       to citta-dx
                 move rec-provincia      to prov-dx
                 move cli-ragsoc-1       to ragio-sx
                 move cli-indirizzo      to indi-sx
                 move cli-cap of clienti to cap-sx
                 move cli-localita       to citta-sx
              else
                 move cli-ragsoc-1       to ragio-dx
                 move cli-indirizzo      to indi-dx
                 move cli-cap of clienti to cap-dx
                 move cli-localita       to citta-dx
                 move cli-prov           to prov-dx
                 move spaces             to ragio-sx
                 move spaces             to indi-sx
                 move spaces             to cap-sx
                 move spaces             to citta-sx
              end-if

              move como-num-fattura       to nr-fatt
              move como-data-fattura(3:2) to como-data6(5:2)
              move como-data-fattura(5:2) to como-data6(3:2)
              move como-data-fattura(7:2) to como-data6(1:2)
              move como-data6             to t-data
              move tblpa-descrizione1     to despaga
              move cli-abi                to t-abi
              move cli-cab                to t-cab
              move como-num-ord-cli       to numord-cli
              move como-data-doc(3:2)     to como-data6(5:2)
              move como-data-doc(5:2)     to como-data6(3:2)
              move como-data-doc(7:2)     to como-data6(1:2)
              move como-data6             to dataord-cli
              move como-num-bolla         to numerobolla
              move como-data-bolla(3:2)   to como-data6(5:2)
              move como-data-bolla(5:2)   to como-data6(3:2)
              move como-data-bolla(7:2)   to como-data6(1:2)
              move como-data6             to databolla
              move como-cod-cli           to codcliente

              move como-cod-agente        to codice-ed
              call "C$JUSTIFY"         using codice-ed, "L"
              move codice-ed              to agente

              if cli-fisica
                 move cli-codfis to clfisc
              else
                 move cli-piva   to clfisc
              end-if

              if EsisteDestino
                 move des-ragsoc-1  to dest-1
                 move des-indirizzo to dest-2
                 move des-localita  to dest-3
              else
                 move spaces to dest-1
                 move spaces to dest-2
                 move spaces to dest-3
              end-if

              write line-riga from spaces   after 13

              
LUBEXX*****DA APRIRE DOPO AVER RICEVUTO IN PRODUZIONE 
LUBEXX*****IL NUOVO PROGRAMMA DA SABER SYSTEM
LUBEXX        if postel-tipo-doc = 1 move "F" to line-riga
LUBEXX        else                   move "N" to line-riga
LUBEXX        end-if
LUBEXX        write line-riga

              write line-riga from rec-rag1 after 1
              write line-riga from rec-rag2 after 2
              write line-riga from rec-rag3 after 1

              write line-riga from rec-tes1 after 8
              write line-riga from rec-tes2 after 2
              write line-riga from rec-tes3 after 2
              write line-riga from spaces   after 3

              if como-note-nc-fm not = spaces
                 perform STAMPA-INTESTAZIONE-NC
              end-if

           end-if.

           initialize record-tbliv
                      art-rec 
                      replacing numeric by zeroes
                                   alphanumeric by spaces.

           move spaces to rec-mer1.

           if Rowcounter >= ( 78-MaxRows - 3 )|- num-righe-note)
              |Dato che dopo la prima pagina l'intestazione
              |non viene più stampata non la devo considerare
              |come limite di righe nelle pagine successive
              move 0 to num-righe-note
              write line-riga from spaces after 2
              move "Contributo CONAI assolto ove dovuto" to dex-note
              write line-riga from rec-mer1

              write line-riga from spaces after 16

              move 0 to RowCounter
              initialize r-note replacing numeric by zeroes
                                     alphanumeric by spaces

              write line-riga from spaces   after 13

              
LUBEXX*****DA APRIRE DOPO AVER RICEVUTO IN PRODUZIONE 
LUBEXX*****IL NUOVO PROGRAMMA DA SABER SYSTEM
LUBEXX        if postel-tipo-doc = 1 move "F" to line-riga
LUBEXX        else                   move "N" to line-riga
LUBEXX        end-if
LUBEXX        write line-riga

              write line-riga from rec-rag1 after 1
              write line-riga from rec-rag2 after 2
              write line-riga from rec-rag3 after 1

              write line-riga from rec-tes1 after 8
              write line-riga from rec-tes2 after 2
              write line-riga from rec-tes3 after 2
              write line-riga from spaces   after 3

LUBEX******              if como-note-nc-fm not = spaces
LUBEXX*****                 perform STAMPA-INTESTAZIONE-NC
LUBEX******              end-if

           end-if.

           move como-cod-articolo to art-codice.
           read articoli no lock invalid continue end-read.

           move "IV"          to tbliv-codice1.
           move como-cod-iva  to tbliv-codice2.
           read tivaese no lock invalid continue end-read.

           if como-cod-articolo = 0
              move como-des-libera to desarticolo
              move spaces          to unimis
           else
              move art-codice      to codiceart
              if postel-tipo-doc not = 1
                 move rno-cod-articolo to ror-cod-articolo
                 move tno-prg-destino  to tor-prg-destino
              end-if
              perform TROVA-CODICE-ARTICOLO-ON-ASSORCLI
              if trovato-assorcli
                 move art-descrizione              to dex-a
                 move "-"                          to dex-s
                 move asc-cod-articolo-per-cliente to dex-b
              else
                 move art-descrizione              to desarticolo
              end-if
           end-if.
           move como-qta             to quantita.

           if RigheFinali
              move spaces to quantita 
              initialize rec-totali
              move como-des-libera to r-totale
              move como-valore     to r-tot-imp
              write line-riga from rec-totali after 1
           else
              if como-si-omaggio
                 move "Cessione gratuita esclusa IVA art.15" to gratis
                 write line-riga from r-gratis after 1
              else
                 perform VALORIZZA-RIGA
                 write line-riga from rec-mer1 after 1
              end-if
           end-if.
           add 1 to RowCounter.

      ***---
       VALORIZZA-RIGA.
           move como-imponib-merce   to prezzo.
           move como-imp-consumo     to r-imposta.
           move como-imp-cou-cobat   to r-cou.

           if como-qta = 0 move 1 to como-qta end-if.

           compute importo-netto = como-qta *
           (como-imponib-merce + como-imp-consumo + como-imp-cou-cobat).

           move importo-netto to importo.

           compute imponibile-merce = imponibile-merce + 
                                    ( como-qta * como-imponib-merce ).
           compute tot-consumo      = tot-consumo      + 
                                    ( como-qta * como-imp-consumo ).
           compute tot-cou          = tot-cou          + 
                                    ( como-qta * como-imp-cou-cobat ). 
           compute tot-imponibile   = tot-imponibile   + importo-netto.

           move 0 to como-iva.

           if tbliv-percentuale not = 0
              move tbliv-percentuale to perce-iva-9di3
              move perce-iva-9di3    to perce-iva-x
              call "C$JUSTIFY" using perce-iva-x, "R"
              inspect perce-iva-x replacing leading x"30" by x"20"
           else
              move tbliv-codice2     to perce-iva-x
           end-if.
           
           move         1 to idx.
           set TrovataIVA to false.
           perform until idx > 3
              if cod-iva(idx) = perce-iva-x
                 set TrovataIVA to true
                 exit perform
              end-if
              add  1 to idx
           end-perform.

           if not TrovataIVA
              evaluate true
              when cod-iva(1) = spaces move perce-iva-x to cod-iva(1)
                                       move 1 to idx
              when cod-iva(2) = spaces move perce-iva-x to cod-iva(2)
                                       move 2 to idx
              when cod-iva(3) = spaces move perce-iva-x to cod-iva(3)
                                       move 3 to idx
              end-evaluate
              if tbliv-percentuale = 0 set iva-sigla(idx) to true
              else                     set iva-sigla(idx) to false
              end-if
           end-if.

           compute imponibile-iva(idx) = 
                   imponibile-iva(idx) + importo-netto.

           move tbliv-percentuale to iva-z.

           move iva-z to iva.

      ***---
       VALORIZZA-POSTEL-TOTALI.
           set RigheFinali to true.
           if si-cou
              move 0                           to art-codice
                                                  como-cod-articolo
              move "T O T A L E  C. O. U. "    to como-des-libera
              move tot-solo-cou                to como-valore
              perform VALORIZZA-POSTEL
           end-if.

           if si-cobat
              move 0                           to art-codice
                                                  como-cod-articolo
              move "T O T A L E  C O B A T"    to como-des-libera
              move tot-cobat                   to como-valore
              perform VALORIZZA-POSTEL
           end-if.

           if si-piombo
              move 0                           to art-codice
                                                  como-cod-articolo
              move "T O T A L E  P I O M B O"  to como-des-libera
              move tot-piombo                  to como-valore
              perform VALORIZZA-POSTEL
           end-if.
           set RigheFinali to false.


      ***---
       VALORIZZA-POSTEL-CONAI.
           write line-riga from spaces.
           write line-riga from spaces.
           move spaces to rec-mer1.
           move "Contributo CONAI assolto ove dovuto" to dex-note.
           write line-riga from rec-mer1 after 1.
           initialize r-note replacing numeric by zeroes
                                  alphanumeric by spaces.
           add 3 to RowCounter.
           perform until 1 = 2
              if Rowcounter > 78-MaxRows
                 exit perform
              end-if
              add 1 to RowCounter
              write line-riga from spaces
           end-perform.

      ***---
       VALORIZZA-POSTEL-PIEDE.
           perform CALCOLA-IVA.

           move imponibile-merce to st-importo.
           perform VALUTA-IVA.
           move tot-imponibile   to st-importo-totale st-importo-totale2

           if UsaPrimaRiga
              move st-importo        to impo-gdo
              move st-aliquota-tot   to iva1
              move st-imponibile     to impon1
              move st-importo-iva    to impor1
              move st-importo-totale to totfat
              write line-riga from rec-pie1 after 2
           else
              move st-importo2        to ximpo-gdo
              move st-aliquota-tot2   to xiva1
              move st-imponibile2     to ximpon1
              move articolo-iva(1)    to dex-iva
              move st-importo-totale2 to xtotfat
              write line-riga from rec-pie1-x after 2
           end-if.

           add tot-imponibile  to tot-iva giving tot-fattura.
           move tot-consumo    to st-importo.
           perform VALUTA-IVA.
           move tot-iva        to st-importo-totale st-importo-totale2.

           if UsaPrimaRiga
              move st-importo        to impo-gdo
              move st-aliquota-tot   to iva1
              move st-imponibile     to impon1
              move st-importo-iva    to impor1
              move st-importo-totale to totfat
              write line-riga from rec-pie1 after 2
           else
              move st-importo2        to ximpo-gdo
              move st-aliquota-tot2   to xiva1
              move st-imponibile2     to ximpon1
              move articolo-iva(2)    to dex-iva
              move st-importo-totale2 to xtotfat
              write line-riga from rec-pie1-x after 2
           end-if.
           
           add tot-imponibile  to tot-iva giving tot-fattura.
           move tot-cou        to st-importo.
           perform VALUTA-IVA.
           move tot-fattura    to st-importo-totale st-importo-totale2.

           if UsaPrimaRiga
              move st-importo        to impo-gdo
              move st-aliquota-tot   to iva1
              move st-imponibile     to impon1
              move st-importo-iva    to impor1
              move st-importo-totale to totfat
              write line-riga from rec-pie1 after 2
           else
              move st-importo2        to ximpo-gdo
              move st-aliquota-tot2   to xiva1
              move st-imponibile2     to ximpon1
              move articolo-iva(3)    to dex-iva
              move st-importo-totale2 to xtotfat
              write line-riga from rec-pie1-x after 2
           end-if.

      ***---
       CALCOLA-IVA.
           move    1 to idx.
           perform 3 times
              if not iva-sigla(idx)
                 move cod-iva(idx)    to tbliv-percentuale convert
                 move 0 to como-iva
                 compute como-iva = 
                   ( ( imponibile-iva(idx) * tbliv-percentuale ) / 100 )
                 add 0,005          to como-iva
                 move como-iva      to como-iva-2dec
              else
                 move "IV"          to tbliv-codice1
                 move cod-iva(idx)  to tbliv-codice2
                 read tivaese no lock 
                      invalid move spaces to tbliv-descrizione1
                 end-read
                 move tbliv-descrizione1 to articolo-iva(idx)
                 move 0 to como-iva-2dec
              end-if
              move como-iva-2dec to importo-iva(idx)
              add 1 to idx
           end-perform.

           compute tot-iva = importo-iva(1) +
                             importo-iva(2) +
                             importo-iva(3).

      ***---
       VALORIZZA-POSTEL-SCADENZE.
           initialize variabili-varsca replacing numeric data by zeroes
                                            alphanumeric data by spaces.
           if postel-tipo-doc not = 1
              move tno-cod-pagamento to tor-cod-pagamento
              move tno-data-fattura  to tor-data-fattura
           end-if.

           move tor-cod-pagamento to sca-codice-pa.
           move tor-data-fattura  to sca-data-fattura.
           move tor-data-fattura  to sca-data-conteggio.

           move tot-fattura       to sca-importo-fattura.
           move tot-fattura       to sca-importo-fattura-va.
           move tot-iva           to sca-iva.
           move tot-iva           to sca-iva-va.
           
           move cli-mese1         to sca-mese1.
           move cli-giorno1       to sca-giorno1.
           move cli-mese2         to sca-mese2.
           move cli-giorno2       to sca-giorno2.

           move cli-escluso-dal-giorno1 to sca-escluso-dal-giorno1.
           move cli-escluso-dal-giorno2 to sca-escluso-dal-giorno2.

           call   "calsca" using variabili-varsca. 
           cancel "calsca".

           initialize IMPORTORB1 SCADENZARB1 IMPORTORB2 SCADENZARB2
                      IMPORTORB3 SCADENZARB3 NOTE.

           move 0 to idx-sca.
           perform varying idx from 1 by 1 until idx > 3
              if sca-importo(idx) not = 0 and
                 sca-data(idx)    not = spaces
                 add 1 to idx-sca
                 if sca-a-vista(idx) = "S"
                    move tor-data-fattura to sca-data(idx)
                 end-if
              else
                 exit perform
              end-if
           end-perform.

           evaluate idx-sca
           when 1
                move sca-importo(1)    to importorb1
                move gg of sca-data(1) to como-data6(1:2)
                move mm of sca-data(1) to como-data6(3:2)
                move aa of sca-data(1) to anno
                move anno(3:2)         to como-data6(5:2)
LUBEXX          move como-data6        to como-scadenzarb
LUBEXX          move como-scadenzarb   to scadenzarb1

                write line-riga from rec-pie2 after 2
                write line-riga from spaces   after 7

           when 2
                move sca-importo(1)    to importorb1
                move gg of sca-data(1) to como-data6(1:2)
                move mm of sca-data(1) to como-data6(3:2)
                move aa of sca-data(1) to anno
                move anno(3:2)         to como-data6(5:2)
LUBEXX          move como-data6        to como-scadenzarb
LUBEXX          move como-scadenzarb   to scadenzarb1

                move sca-importo(2)    to importorb2
                move gg of sca-data(2) to como-data6(1:2)
                move mm of sca-data(2) to como-data6(3:2)
                move aa of sca-data(2) to anno
                move anno(3:2)         to como-data6(5:2)
LUBEXX          move como-data6        to como-scadenzarb
LUBEXX          move como-scadenzarb   to scadenzarb2

                write line-riga from rec-pie2 after 2
                write line-riga from spaces   after 7
            
           when 3
                move sca-importo(1)    to importorb1
                move gg of sca-data(1) to como-data6(1:2)
                move mm of sca-data(1) to como-data6(3:2)
                move aa of sca-data(1) to anno
                move anno(3:2)         to como-data6(5:2)
LUBEXX          move como-data6        to como-scadenzarb
LUBEXX          move como-scadenzarb   to scadenzarb1

                move sca-importo(2)    to importorb2
                move gg of sca-data(2) to como-data6(1:2)
                move mm of sca-data(2) to como-data6(3:2)
                move aa of sca-data(2) to anno
                move anno(3:2)         to como-data6(5:2)
LUBEXX          move como-data6        to como-scadenzarb 
LUBEXX          move como-scadenzarb   to scadenzarb2

                move sca-importo(3)    to importorb3
                move gg of sca-data(3) to como-data6(1:2)
                move mm of sca-data(3) to como-data6(3:2)
                move aa of sca-data(3) to anno
                move anno(3:2)         to como-data6(5:2)
LUBEXX          move como-data6        to como-scadenzarb
LUBEXX          move como-scadenzarb   to scadenzarb3

                write line-riga from rec-pie2 after 2
                write line-riga from spaces   after 7

           end-evaluate.
           
           move 0 to tot-consumo
                     tot-cou
                     tot-iva
                     tot-fattura.

           perform INIT-TABELLA-IVA.

      ***---
       INIT-TABELLA-IVA.
           move 0 to idx.
           perform 3 times
              add 1 to idx
              initialize tabella-iva(idx)replacing numeric data by zeros
                                              alphanumeric data by space
           end-perform.

      ***---
       VALUTA-IVA.
           initialize rec-pie1 replacing   numeric data by zeroes
                                      alphanumeric data by spaces.
           initialize rec-pie1-x replacing numeric data by zeroes
                                      alphanumeric data by spaces.

           move space to st-aliquota-tot st-aliquota-tot2
                                         st-articolo-iva2.
           move     0 to st-imponibile   st-importo-iva idx.
           move     0 to st-imponibile2.

           if cod-iva(1) not = spaces
              if iva-sigla(1)
                 move spaces to cod-iva(1) 
              end-if
              if articolo-iva(1) not = spaces
                 if imponibile-iva(1) = 0
                    move spaces to articolo-iva(1)
                 end-if
                 set  UsaSecondaRiga    to true
                 move st-importo        to st-importo2
                 move articolo-iva(1)   to st-articolo-iva2
                 move imponibile-iva(1) to st-imponibile2
                 move cod-iva(1)        to st-aliquota-tot2
              else
                 set  UsaPrimaRiga      to true
                 move cod-iva(1)        to st-aliquota-tot
                 move importo-iva(1)    to st-importo-iva
                 move imponibile-iva(1) to st-imponibile
              end-if
              move spaces to cod-iva(1)
           else
              if cod-iva(2) not = spaces
                 if iva-sigla(2)
                    move spaces to cod-iva(2) 
                 end-if
                 if articolo-iva(2) not = spaces
                    if imponibile-iva(2) = 0
                       move spaces to articolo-iva(2)
                    end-if
                    set  UsaSecondaRiga    to true
                    move st-importo        to st-importo2
                    move articolo-iva(2)   to st-articolo-iva2
                    move imponibile-iva(2) to st-imponibile2
                    move cod-iva(2)        to st-aliquota-tot2
                 else
                    set  UsaPrimaRiga      to true
                    move cod-iva(2)        to st-aliquota-tot
                    move importo-iva(2)    to st-importo-iva
                    move imponibile-iva(2) to st-imponibile
                 end-if
                 move spaces to cod-iva(2)
              else
                 if cod-iva(3) not = spaces
                    if iva-sigla(3)
                       move spaces to cod-iva(3)
                    end-if
                    if articolo-iva(3) not = spaces
                       if imponibile-iva(3) = 0
                          move spaces to articolo-iva(3)
                       end-if
                       set UsaSecondaRiga     to true
                       move st-importo        to st-importo2
                       move articolo-iva(3)   to st-articolo-iva2
                       move imponibile-iva(3) to st-imponibile2
                       move cod-iva(3)        to st-aliquota-tot2
                    else
                       set  UsaPrimaRiga      to true
                       move cod-iva(3)        to st-aliquota-tot
                       move importo-iva(3)    to st-importo-iva
                       move imponibile-iva(3) to st-imponibile
                    end-if
                    move spaces to cod-iva(3)
                 end-if
              end-if
           end-if.
      *****     move space to st-aliquota-tot.
      *****     move     0 to st-imponibile st-importo-iva idx.
      *****
      *****     if cod-iva(1) not = spaces
      *****        move cod-iva(1)        to st-aliquota-tot
      *****        move importo-iva(1)    to st-importo-iva
      *****        move imponibile-iva(1) to st-imponibile
      *****        move spaces            to cod-iva(1)
      *****     else
      *****        if cod-iva(2) not = spaces
      *****           move cod-iva(2)        to st-aliquota-tot
      *****           move importo-iva(2)    to st-importo-iva
      *****           move imponibile-iva(2) to st-imponibile
      *****           move cod-iva(2)        to st-aliquota-tot
      *****           move spaces            to cod-iva(2)
      *****        else
      *****           if cod-iva(3) not = spaces
      *****              move cod-iva(3)        to st-aliquota-tot
      *****              move importo-iva(3)    to st-importo-iva
      *****              move imponibile-iva(3) to st-imponibile
      *****              move cod-iva(3)        to st-aliquota-tot
      *****              move spaces            to cod-iva(3)
      *****           end-if
      *****        end-if
      *****     end-if.

      ***---
       STAMPA-INTESTAZIONE-NC.
           initialize intestazioni-note.

           move 0               to idx.
           move 1               to cont-inizio.
           set exit-perform-int to false.

           perform 100 times
              if cont-inizio >= 500 exit perform end-if
              add  1 to idx                 
              move 0 to cont-char
      *    controllo di non andare oltre i 500 caratteri
              if cont-inizio > 451
                 compute cont-per = 500 - cont-inizio
                 set exit-perform-int to true
              else
                 move 50  to cont-per
              end-if

              inspect como-note-nc-fm(cont-inizio:cont-per) 
                      tallying cont-char for all x"0D"
              if cont-char = zero
                 |move 50  to cont-per
                 continue
              else
                 initialize cont-per
                 inspect como-note-nc-fm(cont-inizio:50) 
                         tallying cont-per for characters before x"0D"
              end-if
              if cont-per not = zero
                 move como-note-nc-fm(cont-inizio:cont-per) 
                   to intestazione-note(idx)
      *    se appena dopo i 50 caratteri premo invio devo ignorarlo
                 if cont-per = 50
                    add cont-per  to cont-inizio
                    if cont-inizio < 499 and
                       como-note-nc-fm(cont-inizio:1) = x"0D"
                       add 2 to cont-inizio
                    end-if
                    subtract cont-per from cont-inizio
                 end-if
              else
                 move space to intestazione-note(idx)
              end-if
              if cont-char = zero
                 add 50   to cont-inizio
              else
                 compute cont-inizio = cont-inizio + cont-per + 2                                                         
              end-if
              if exit-perform-int
                 exit perform
              end-if
           end-perform.

           initialize num-righe-note
           perform varying idx from 1 by 1 until idx > 100
              if intestazione-note(idx) not = space
                 move idx to num-righe-note
              end-if
           end-perform.

           perform varying idx from 1 by 1 until idx > num-righe-note
              move intestazione-note(idx) to dex-note
              write line-riga from rec-mer1
              add 1 to RowCounter
           end-perform.

      ***--
       CLOSE-FILES.
           if postel-tipo-doc = 1 close tordini rordini
           else                   close tnotacr rnotacr
           end-if.
           close tivaese  assorcli  clienti  recapiti  agenti
                 tcodpag  articoli  destini  tvettori  CLI ttipocli.

           if tutto-ok close lineseq end-if.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "trova-su-assorcli.cpy".
