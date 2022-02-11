       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      stgiormag-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl".
           copy "articoli.sl".
           copy "tmagaz.sl".
           copy "giormag.sl".
           copy "clienti.sl".
           copy "tcaumag.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "articoli.fd".
           copy "tmagaz.fd".
           copy "giormag.fd".
           copy "clienti.fd".
           copy "tcaumag.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "comune.def".
       copy "link-geslock.def".

      * COSTANTI
       78  titolo                value "Stampa Giornale di Magazzino".
       78  max-righe             value 43.

      * FILE STATUS
       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.
       77  status-tmagaz         pic xx.
       77  status-tcaumag        pic xx.
       77  status-clienti        pic xx.
       77  status-giormag        pic xx.
       77  wstampa               pic x(256).

      * VARIABILI
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  SaveArticolo          pic 9(6).
       77  SaveMagazzino         pic x(3).
       77  num-righe             pic 99 value 0.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  LastIdx               pic 9(5) value 0.

       01  occurs-articoli       occurs 90000 indexed by art-idx.
         05 el-articolo          pic 9(6).
         05 el-qta               pic s9(6).

      * FLAGS
       01  filler                pic 9.
         88 prima-volta          value 1 false 0.

       01  filler                pic 9.
         88 record-ok            value 1 false 0.

      * RIGHE PER LA STAMPA
       01  intestazione-1.
         05 filler               pic x(18)  value "** LUBEX S.p.A. **".
         05 int-filler           pic x(68).
         05 int-mese             pic x(9).
         05 filler               pic x(3)   value " - ".
         05 int-anno             pic 9(4).
         05 filler               pic x(14).
         05 filler               pic x(4)   value "Pag.".
         05 int-pag              pic zz.zz9. 

       01  intestazione-2.
         05 filler               pic x(38)  value 
           "*---> Prodotto (Codice e descrizione): ".
         05 int-2-articolo       pic z(6).
         05 filler               pic x.
         05 int-2-desart         pic x(34).
         05 filler               pic x(12)  value " Magazzino: ".
         05 int-2-mag            pic x(3).
         05 filler               pic x.
         05 int-2-desmag         pic x(34).

       01  riga-div-1            pic x(130) value all "-".

       01  intestazione-3.
         05 filler               pic x(2).
         05 filler               pic x(4)   value "Data".
         05 filler               pic x(2).
         05 filler               pic x(2).
         05 filler               pic x(7)   value "Causale".
         05 filler               pic x(18).
         05 filler               pic x(2)   value "UM".
         05 filler               pic x(6).
         05 filler               pic x(7)   value "Entrate".
         05 filler               pic x(8).
         05 filler               pic x(6)   value "Uscite".
         05 filler               pic x(5).
         05 filler               pic x(7)   value "Nr.Doc.".
         05 filler               pic x(3).
         05 filler               pic x(7)   value "cod.C/F".
         05 filler               pic x(3).
         05 filler               pic x(15)  value "Ragione Sociale".

       01  riga-div-2            pic x(130) value all "=".
       01  riga-div-3            pic x(64)  value all "-".

       01  r-riga.
         05 r-data               pic x(8).
         05 filler               pic x(2).
         05 r-causale            pic x(23).
         05 filler               pic x(2).
         05 r-um                 pic x(2).
         05 filler               pic x(2).
         05 r-entrata            pic ---.---.--9.
         05 filler               pic x(3).       
         05 r-uscita             pic ---.---.--9.
         05 filler               pic x(2).
         05 r-numdoc             pic z(10).
         05 filler               pic x(5).
         05 r-clifor             pic z(5).
         05 filler               pic x(3).
         05 r-ragsoc             pic x(40).

       LINKAGE SECTION.
       copy "link-stgiormag.def".

      ******************************************************************
       PROCEDURE DIVISION using stgiormag-linkage.

       DECLARATIVES.
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
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***---
       TMAGAZ-ERR SECTION.
           use after error procedure on tmagaz.
           set tutto-ok  to true.
           evaluate status-tmagaz
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""Tabella magazzini [TMAGAZ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMAGAZ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMAGAZ] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***---
       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message "Impossibile procedere."
             x"0d0a""Tabella causali di magazzino [TCAUMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TCAUMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***---
       GIORMAG-ERR SECTION.
           use after error procedure on giormag.
           set tutto-ok  to true.
           evaluate status-giormag
           when "35"
                display message "Impossibile procedere."
               x"0d0a""File giornali di magazzino [GIORMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [GIORMAG] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[GIORMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File lineseq [LINESEQ] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [LINESEQ] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[LINESEQ] Indexed file corrupt!"
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
           end-evaluate
       END DECLARATIVES.

      ***--- 
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform PRE-ELABORAZIONE
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.

           perform EXIT-PGM.

      ***---
       INIT.
      *-    
           move 0      to counter counter2.
           move 0      to SaveArticolo.
           move spaces to SaveMagazzino.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  wstampa      from environment "PATH-ST".
           inspect wstampa      replacing trailing spaces by low-value.
           string  wstampa      delimited by low-value
                   "stgiormag"  delimited by size
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   ".txt"       delimited by size
                   into wstampa
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-LINESEQ.
           if tutto-ok
              open input clienti
                         articoli
                         tcaumag
                         tmagaz
                         giormag
              if errori
                 close lineseq
                 delete file lineseq
                 goback
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-LINESEQ.
           open output lineseq.

      ***---
      * In questa fase riempio un occurs che contiene il codice articolo
      * e la quantità movimentata così da scartare quelli ZERO
       PRE-ELABORAZIONE.
           move low-value to gio-rec.
           move stg-anno  to gio-aaaa
           move stg-mese  to gio-mm. 
           move stg-mag   to gio-mag.
           start giormag key is >= k-data
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 set record-ok to true

                 add 1 to counter counter2
                 if counter2 = 200
                    move 0 to counter2
                    move counter to counter-edit
                    display counter-edit
                       upon stg-handle at column 01 line 18
                 end-if

                 read giormag next at end    exit perform end-read
                 if gio-aaaa  not = stg-anno exit perform end-if
                 if gio-mm    not = stg-mese exit perform end-if
                 if gio-mag = stg-mag
                    set record-ok to false
                    set art-idx   to 1
                    search occurs-articoli
                    when el-articolo(art-idx) = gio-art
                         set record-ok to true
                    end-search
                    if not record-ok
                       add 1 to LastIdx
                       move LastIdx to art-idx
                    end-if                 
                    move gio-art to el-articolo(art-idx)
                    add  gio-qta to el-qta(art-idx)
                 end-if
              end-perform
           end-if.

      ***---
       ELABORAZIONE.
           move 0 to counter counter2.
           move counter to counter-edit.
           display counter-edit upon stg-handle at column 01 line 18.

           move low-value to gio-rec.
           move stg-anno  to gio-aaaa
           move stg-mese  to gio-mm. 
           move stg-mag   to gio-mag.
           start giormag key is >= k-data
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 set record-ok to true

                 add 1 to counter counter2
                 if counter2 = 200
                    move 0 to counter2
                    move counter to counter-edit
                    display counter-edit
                       upon stg-handle at column 01 line 18
                 end-if

                 read giormag next at end    exit perform end-read
                 if gio-aaaa  not = stg-anno exit perform end-if
                 if gio-mm    not = stg-mese exit perform end-if
                 if gio-mag not = stg-mag and stg-mag not = spaces
                    set record-ok to false
                 end-if

                 if record-ok
                    set art-idx       to 1
                    search occurs-articoli
                    when el-articolo(art-idx) = gio-art
                         if el-qta(art-idx) = 0
                            set record-ok to false
                         end-if
                    end-search
                 end-if

                 if record-ok
                    set trovato to true
                    if prima-volta
                       perform SCRIVI-INTESTAZIONE
                    end-if
                    if gio-art not = SaveArticolo or
                       gio-mag not = SaveMagazzino
                       perform SCRIVI-TESTATA-ARTICOLO
                       move gio-mag to SaveMagazzino
                       move gio-art to SaveArticolo
                    end-if
                    set prima-volta to false
                    perform VALORIZZA-RIGA
                 end-if
              end-perform
           end-if.

           if not trovato
              subtract 1 from stg-pag
              display message "Nessun movimento trovato"
                        title titolo
                         icon 2
           end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           if prima-volta
              initialize line-riga
              perform STAMPA-RIGA
           else
              add  1 to stg-pag
           end-if.
           move gio-mag  to mag-codice.
           read tmagaz no lock invalid continue end-read.
           if mag-tutte
              move " MOVIMENTAZIONE MAGAZZINO BENI PROPRI PRESSO TERZI  
      -            "per il mese di: " to int-filler
           else
              move " MOVIMENTAZIONE MAGAZZINO PRODOTTI NON SOGGETTI UTF 
      -            "per il mese di: " to int-filler
           end-if.
           move stg-anno to int-anno.
           evaluate stg-mese
           when  1  move "GENNAIO"    to int-mese
           when  2  move "FEBBRAIO"   to int-mese
           when  3  move "MARZO"      to int-mese
           when  4  move "APRILE"     to int-mese
           when  5  move "MAGGIO"     to int-mese
           when  6  move "GIUGNO"     to int-mese
           when  7  move "LUGLIO"     to int-mese
           when  8  move "AGOSTO"     to int-mese
           when  9  move "SETTEMBRE"  to int-mese
           when 10  move "OTTOBRE"    to int-mese
           when 11  move "NOVEMBRE"   to int-mese
           when 12  move "DICEMBRE"   to int-mese
           end-evaluate.
           move stg-pag to int-pag.
           initialize line-riga.
           move intestazione-1 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           string "                   " delimited size
                  "MAGAZZINO: "         delimited size
                  mag-codice            delimited size
                  " - "                 delimited size
                  mag-descrizione       delimited size
                  into line-riga
           end-string.
           perform STAMPA-RIGA.

      ***---
       SCRIVI-TESTATA-ARTICOLO.
           if num-righe >= max-righe - 7
              perform SALTO-PAGINA
           end-if.
           move spaces to line-riga.
           perform STAMPA-RIGA.

           move gio-art to int-2-articolo art-codice.
           read articoli
                invalid move spaces to art-descrizione
           end-read.
           move art-descrizione  to int-2-desart.
           move gio-mag          to int-2-mag mag-codice.
           read tmagaz invalid move spaces to mag-descrizione end-read.
           move mag-descrizione  to int-2-desmag.
           initialize line-riga.
           move intestazione-2 to line-riga.
           perform STAMPA-RIGA.

           perform SCRIVI-DIVISIONE-ARTICOLO.

      ***---
       SCRIVI-DIVISIONE-ARTICOLO.
           if num-righe >= max-righe - 3
              perform SALTO-PAGINA
           end-if.
           initialize line-riga.
           move riga-div-1 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move intestazione-3 to line-riga.
           perform STAMPA-RIGA.

           initialize line-riga.
           move riga-div-1 to line-riga.
           perform STAMPA-RIGA.

      ***---
       VALORIZZA-RIGA.
           evaluate true
           when gio-iniziale
           when gio-entrata
           when gio-uscita
                if num-righe >= max-righe
                   perform SALTO-PAGINA
                   move spaces to line-riga
                   perform STAMPA-RIGA
                end-if
           when gio-finale
                if num-righe >= max-righe - 2
                   perform SALTO-PAGINA
                   move spaces to line-riga
                   perform STAMPA-RIGA
                end-if
           end-evaluate.

           move spaces to r-riga.
           string gio-gg        delimited size
                  "/"           delimited size
                  gio-mm        delimited size
                  "/"           delimited size
                  gio-aaaa(3:2) delimited size
                  into r-data
           end-string.
           evaluate true
           when gio-iniziale move "Giac. Iniziale" to r-causale
           when gio-entrata
           when gio-uscita
                move gio-causale to tca-codice
                move spaces  to tca-descrizione
                read tcaumag no lock invalid continue end-read
                move tca-descrizione to r-causale
           when gio-finale
                initialize line-riga
                move riga-div-3 to line-riga
                perform STAMPA-RIGA
                move "Giac. Finale"   to r-causale
           end-evaluate.
           move gio-um to r-um.
           if gio-uscita move gio-qta to r-uscita
           else          move gio-qta to r-entrata
           end-if.
           move gio-num-doc     to r-numdoc.
           move gio-cod-clifor  to r-clifor.
           move gio-tipo-CF     to cli-tipo-CF.
           move gio-cod-clifor  to cli-codice.
           read clienti no lock
                invalid move spaces       to r-ragsoc
            not invalid move cli-ragsoc-1 to r-ragsoc
           end-read.

           initialize line-riga.
           move r-riga to line-riga.
           perform STAMPA-RIGA.

           if gio-finale
              if num-righe < max-righe
                 initialize line-riga
                 move riga-div-2 to line-riga
                 perform STAMPA-RIGA
              end-if
           end-if.

      ***---
       SALTO-PAGINA.
           write line-riga from spaces after page.
           write line-riga from x"09" after 1.
           move 0 to num-righe.
           perform SCRIVI-INTESTAZIONE.

      ***---
       STAMPA-RIGA.
           write line-riga.
           add 1 to num-righe.

      ***---
       CLOSE-FILES.
           close articoli
                 clienti
                 tmagaz
                 tcaumag
                 giormag
                 lineseq.

      ***---
       EXIT-PGM.
           move wstampa to stg-path.
           goback.
