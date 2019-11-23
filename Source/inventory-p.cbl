       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      inventory-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "articoli.sl".
           copy "progmag.sl".
           copy "tmp-inventory.sl".
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
           copy "tmp-inventory.fd".
           copy "tmarche.fd".
           copy "timbalqta.fd".
           copy "timballi.fd".
           copy "timposte.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "common-excel.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "costo-medio.def".
       copy "imposte.def".

       78  titolo                value "Stampa Inventario".
       77  max-righe             pic 99.
       77  max-righe-x           pic x(2).
       77  righe-diff            pic 99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  passo                 pic 9v99.
       77  user-codi             pic x(10).

       77  messaggio             pic x(150) value SPACES.
       77  font-size-dply        pic z(5).
       77  Verdana12B            handle of font.
       77  Verdana12I            handle of font.
       77  Verdana10             handle of font.
       77  WFONT-STATUS          pic s9(5) value ZERO.
       
       77  data-inventario       pic x(10).
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  scelta                pic 9.
       77  art-peso-tot          pic 9(5)v999.

       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.
       77  status-progmag        pic xx.
       77  status-tmp-inventory  pic xx.
       77  status-tmarche        pic xx.
       77  status-timbalqta      pic xx.
       77  status-timballi       pic xx.
       77  status-timposte       pic xx.

       77  path-csv              pic x(256).
       77  path-txt              pic x(256).
       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).
       77  tipo-costo            pic x(6).
       77  SaveMarca             pic 9(5) value 0.
       77  marca-ed              pic z(4)9.
       77  SaveMagazzino         pic x(3) value spaces.
       77  prg-cod-articolo-edit pic z(6).
       77  prg-peso-edit         pic zz9,999.
       77  imballi-ed            pic z.zz9.

       77  tinv-qta-edit         pic ---.---.--9.
       77  tinv-prezzo-edit      pic ---.---.---.--9,99.
       77  tinv-valore-edit      pic ----.---.---.--9,99.
       77  tinv-kg-edit          pic      ---.---.--9,999.

       77  TotValore             pic s9(12)v99 value 0.
       77  TotValoreEdit         pic ----.---.---.--9,99.

       77  TotValoreMag          pic s9(12)v99 value 0.
       77  TotValoreMagEdit      pic ----.---.---.--9,99.

       77  TotUtf                pic s9(9)v999 value 0.
       77  TotUtfEdit            pic ----.---.--9,999.

       77  TotUtfMag             pic s9(9)v999 value 0.
       77  TotUtfMagEdit         pic ----.---.--9,999.

       77  TotUtfGen             pic s9(9)v999 value 0.
       77  TotUtfGenEdit         pic ----.---.--9,999.

       77  TotNonUtf             pic s9(9)v999 value 0.
       77  TotNonUtfEdit         pic ----.---.--9,999.

       77  TotNonUtfMag          pic s9(9)v999 value 0.
       77  TotNonUtfMagEdit      pic ----.---.--9,999.

       77  TotNonUtfGen          pic s9(9)v999 value 0.
       77  TotNonUtfGenEdit      pic ----.---.--9,999.

       77  TotKg                 pic s9(9)v999 value 0.
       77  TotKgEdit             pic ----.---.--9,999.

       77  TotKgMag              pic s9(9)v999 value 0.
       77  TotKgMagEdit          pic ----.---.--9,999.

       77  TotValoreGen          pic s9(12)v999 value 0.
       77  TotValoreGenEdit      pic ----.---.---.--9,99.
       77  prg-riga              pic 9(10).

       77  tipo-file             pic x.
           88 FileExcel          value "E".
           88 FileTxt            value "T".

       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88  SalvaRecArticoli  value 1, false 0.
       77  filler                pic 9.
           88  prima-volta       value 1, false 0.
       77  filler                pic 9.
           88  record-ok         value 1, false 0.
       77  filler                pic 9.
           88 si-intestazione    value 1, false 0.
       77  filler                pic 9.
           88  trovato-movim     value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

       77  num-pagina            pic 999.
       77  num-righe             pic 99.

      * RIGHE PER LA STAMPA
       01  riga-intestazione.
           05 int-inventario        pic x(131).

       01  riga-pagina.
           05 filler                pic x(5) value "Pag. ".
           05 int-pag               pic z(3).

       01  riga-trattini.
           05 filler                pic x(2).
           05 filler                pic x(126) value all "_".

       01  riga-titolo.
           05 filler                pic x(6)  value "Codice".
           05 tit-mag               pic x(3).
           05 filler                pic x(11) value "Descrizione".
           05 filler                pic x(2)  value "UM".
           05 filler                pic x(4)  value "Q.tà".
           05 filler                pic x(6)  value "Prezzo".
           05 filler                pic x(6)  value "Valore".
           05 filler                pic x(3)  value "Kg.".

       01  riga-titolo-2.
           05 filler                pic x(88).
           05 como-titolo           pic x(6) value "Medio".

       01  r-riga.
           05 r-articolo            pic z(6).
           05 r-asterisk            pic x.
           05 r-mag-codice          pic x(3).
           05 r-art-descrizione     pic x(48).
           05 r-udm                 pic x(2).
           05 r-qta-edit            pic ---.---.--9.
           05 r-prezzo-edit         pic ---.---.--9,99.
           05 r-valore-edit         pic ----.---.--9,99.
           05 r-kg-edit             pic --.---.--9,999.

       01  r-marca.
           05 r-marca-tot           pic x(45).
           05 r-marca-totale        pic ----.---.--9,99.
           05 r-marca-tot-kg        pic ----.---.--9,999.

       01  r-marca-utf.
           05 filler                pic x(17) value "Totale Kg. U T F".
           05 r-marca-tot-utf       pic ----.---.--9,999.

       01  r-marca-non-utf.
           05 filler                pic x(17) value "Totale Kg. Esenti".
           05 r-marca-tot-non-utf   pic ----.---.--9,999.

       01  r-mag.
           05 filler                pic x(16) value "Totale Magazzino".
           05 filler                pic x(2)  value " (".
           05 r-mag-tot             pic x(3).
           05 filler                pic x     value ")".
           05 r-mag-tot-valore      pic ----.---.--9,99.
           05 r-mag-tot-kg          pic ----.---.--9,999.

       01  r-mag-utf.
           05 filler                pic x(17) value "Totale Kg. U T F".
           05 r-mag-tot-utf         pic ----.---.--9,999.

       01  r-mag-non-utf.
           05 filler                pic x(17) value "Totale Kg. Esenti".
           05 r-mag-tot-non-utf     pic ----.---.--9,999.

       01  r-gen-utf.
           05 filler                pic x(24) 
                                    value "Totale Soggetti UTF --->".
           05 r-gen-tot-utf         pic ----.---.--9,999.

       01  r-gen-non-utf.
           05 filler                pic x(24) 
                                    value "Totale Esenti UTF --->".
           05 r-gen-tot-non-utf     pic ----.---.--9,999.

       01  r-gen.
           05 filler                pic x(24) 
                                    value "Totale VALORE --->".
           05 r-gen-tot-valore      pic ----.---.--9,99.

       LINKAGE SECTION.
       copy "link-inveday.def".

      ******************************************************************
       PROCEDURE DIVISION using inveday-linkage.

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
       TMP-INVENTORY-ERR SECTION.
           use after error procedure on tmp-inventory.
           set tutto-ok  to true.
           evaluate status-tmp-inventory
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
                     perform OPEN-OUTPUT-TMP-INVENTORY
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
                string   "Chiudere file Excel!"
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
           move inveday-user to user-codi.
           set environment "PRINTER" to "-P SPOOLER".
           move 0,5 to passo.
           move inveday-da-data(7:2) to data-inventario(1:2).
           move "/"                  to data-inventario(3:1).
           move inveday-da-data(5:2) to data-inventario(4:2).
           move "/"                  to data-inventario(6:1).
           move inveday-da-data(1:4) to data-inventario(7:4).
           accept max-righe-x from environment "NUM-MAX-RIGHE-ORIZ".
           move   max-righe-x to max-righe with convert.
           accept como-data from century-date.
           accept como-ora  from time.
           perform ACCETTA-SEPARATORE.
           move 0 to prg-riga.
           move 0 to num-righe.
           move 1 to num-pagina.
           initialize path-tmp wstampa path-txt.
           set tutto-ok         to true.
           set prima-volta      to true.
           set SalvaRecArticoli to true.
           set trovato          to false.
           set inveday-excel    to false.
           accept  inveday-path from environment "PATH-ST".
           inspect inveday-path replacing trailing spaces by low-value.
           inspect user-codi    replacing trailing spaces by low-value.
           string  inveday-path delimited by low-value
                   "inventory"  delimited by size
                   "_"          delimited by size
                   user-codi    delimited by low-value
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   ".tmp"       delimited by size
                   into path-tmp
           end-string.
           string  inveday-path delimited by low-value
                   "inventory"  delimited by size
                   "_"          delimited by size
                   user-codi    delimited by low-value
                   ".csv"       delimited by size
                   into path-csv
           end-string.
           string  inveday-path delimited by low-value
                   "inventory"  delimited by size
                   "_"          delimited by size
                   user-codi    delimited by low-value
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   ".txt"       delimited by size
                   into path-txt
           end-string.

      ***---
       CARICA-FONT.
      * Verdana 12B
           initialize wfont-data Verdana12B.
           move 12 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana12B, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

      * Verdana 10
           initialize wfont-data Verdana10.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

      * Verdana 12I
           initialize wfont-data Verdana12I.
           move 12 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana12I, wfont-data
                        giving WFONT-STATUS.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if WFONT-STATUS not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect WFONT-NAME replacing trailing SPACE by LOW-VALUE.
           move WFONT-SIZE    to FONT-SIZE-DPLY.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited LOW-VALUE
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio

           inspect messaggio replacing trailing SPACE by LOW-VALUE.

           display message messaggio.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-TMP-INVENTORY.
           if tutto-ok
              open input progmag articoli tmarche 
                         timbalqta timballi timposte
           end-if.

      ***---
       OPEN-OUTPUT-TMP-INVENTORY.
           open output tmp-inventory.
           write line-riga from space after 0.
           if tutto-ok
              close    tmp-inventory
              open i-o tmp-inventory
           end-if.

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
                 end-if

                 evaluate true
                 when inveday-padre
                      if prg-cod-magazzino not = spaces or
                         prg-tipo-imballo  not = spaces or
                         prg-peso          not = 0 |E' un record figlio
                         set record-ok to false
                      end-if
                 when inveday-figlio
                      if prg-cod-magazzino = spaces or
                         prg-tipo-imballo  = spaces or
                         prg-peso          = 0 |E' un record padre
                         set record-ok to false
                      end-if
                 end-evaluate

                 if prg-giacenza-udm = 0
                    set record-ok to false
                 end-if

                 if record-ok
                    if inveday-mag not = spaces
                       if prg-cod-magazzino not = inveday-mag
                          set record-ok to false
                       end-if
                    end-if
                 end-if

                 if record-ok
                    move prg-cod-articolo to art-codice
                    read articoli no lock invalid continue end-read
                    if inveday-marca not = 0
                       if inveday-marca not = art-marca-prodotto
                          set record-ok to false
                       end-if
                    end-if
                 end-if

                 if record-ok
                    perform VALORIZZA-RIGA
                    if errori exit perform end-if
                 end-if

              end-perform
           end-if.

           if not trovato
              close       lineseq
              delete file lineseq
              display message "Nessun progressivo trovato"
                        title titolo
                         icon 2
           else
              close      tmp-inventory
              open input tmp-inventory
              if tutto-ok
                 if inveday-day 
                    perform GENERA-FILE-EXCEL
                 else
                    perform GENERA-FILE-TXT
                 end-if
              end-if
           end-if.

      ***---
       GENERA-FILE-EXCEL.
           move path-csv to wstampa.
           open output lineseq.
           if tutto-ok
              set FileExcel  to true
              move low-value to tinv-rec
              start tmp-inventory key is >= k-ord
                    invalid continue
              end-start
              perform until 1 = 2
                 read tmp-inventory next
                   at end
                      perform TOTALI-MARCA
                      perform TOTALI-MAGAZZINO
                      perform TOTALI-GENERALI
                      exit perform
                 end-read

                 if SaveMarca = 0
                    move tinv-marca to SaveMarca
                 end-if

                 if SaveMagazzino = spaces
                    move tinv-mag to SaveMagazzino
                 end-if

                 if prima-volta
                    perform SCRIVI-INTESTAZIONE
                 end-if

                 if tinv-mag not = SaveMagazzino
                    perform TOTALI-MARCA
                    perform TOTALI-MAGAZZINO
                 end-if

                 if tinv-marca not = SaveMarca
                    perform TOTALI-MARCA
                 end-if

                 initialize line-riga
                 move tinv-qta    to tinv-qta-edit
                 move tinv-prezzo to tinv-prezzo-edit
                 move tinv-valore to tinv-valore-edit
                 move tinv-kg     to tinv-kg-edit

                 if inveday-padre
                    string tinv-articolo        delimited size
                           separatore           delimited size
                           tinv-asterisk        delimited size
                           separatore           delimited size
                           tinv-art-descrizione delimited size
                           separatore           delimited size
                           tinv-um              delimited size
                           separatore           delimited size
                           tinv-qta-edit        delimited size
                           separatore           delimited size
                           tinv-prezzo-edit     delimited size
                           separatore           delimited size
                           tinv-valore-edit     delimited size
                           separatore           delimited size
                           tinv-kg-edit         delimited size
                           into line-riga
                    end-string
                 else
                    string tinv-articolo        delimited size
                           separatore           delimited size
                           tinv-asterisk        delimited size
                           separatore           delimited size
                           tinv-mag             delimited size
                           separatore           delimited size
                           tinv-art-descrizione delimited size
                           separatore           delimited size
                           tinv-um              delimited size
                           separatore           delimited size
                           tinv-qta-edit        delimited size
                           separatore           delimited size
                           tinv-prezzo-edit     delimited size
                           separatore           delimited size
                           tinv-valore-edit     delimited size
                           separatore           delimited size
                           tinv-kg-edit         delimited size
                           into line-riga
                    end-string
                 end-if

                 write line-riga
                 add tinv-valore  to TotValore
                                     TotValoreMag
                                     TotValoreGen
                 add tinv-utf     to TotUtf
                                     TotUtfMag
                                     TotUtfGen
                 add tinv-non-utf to TotNonUtf
                                     TotNonUtfMag
                                     TotNonUtfGen
                 add tinv-kg      to TotKg
                                     TotKgMag
              end-perform

              close lineseq
              perform CALL-EXCEL
           end-if.

      ***---
       GENERA-FILE-TXT.
           set si-intestazione to true.
           display message "Generare il file CSV per Excel?"
                     title titolo
                      type mb-yes-no
                    giving scelta.

           if scelta = mb-yes
              move 0 to TotValore
                        TotValoreMag
                        TotValoreGen
                        TotUtf
                        TotUtfMag
                        TotUtfGen
                        TotNonUtf
                        TotNonUtfMag
                        TotNonUtfGen
                        TotKg
                        TotKgMag
              move spaces to SaveMagazzino
              move 0      to SaveMarca
              set  prima-volta to true
              set inveday-excel to true
              perform GENERA-FILE-EXCEL
           end-if.
           if not inveday-excel
              move path-txt to wstampa
              open output lineseq
              set FileTxt to true
              move low-value to tinv-rec
              start tmp-inventory key is >= k-ord 
                 invalid 
                    continue 
              end-start
              perform until 1 = 2
                 read tmp-inventory next
                    at end
                       move 16 to righe-diff
                       set si-intestazione to false
                       perform PRONOSTICO
                       perform TOTALI-MARCA
                       perform TOTALI-MAGAZZINO
                       perform TOTALI-GENERALI
                       exit perform
                 end-read

                 if SaveMarca = 0
                    move tinv-marca to SaveMarca
                 end-if

                 if SaveMagazzino = spaces
                    move tinv-mag to SaveMagazzino
                 end-if

                 initialize spooler-link
                 if prima-volta
                    call   "selprint" using selprint-linkage
                    cancel "selprint"

                    if selprint-stampante not = space
                       move selprint-num-copie to SPL-NUM-COPIE
                       move selprint-stampante to SPL-NOME-STAMPANTE

                       move "GESLUX - Stampa Inventario" to spl-nome-job
                       set spl-apertura   to true
                       set spl-horizontal to true
                       set WFDEVICE-WIN-PRINTER    to true
                       call "spooler" using spooler-link
                       if spl-sta-annu
                          set errori to true
                          exit perform
                       else
                          move spl-altezza to save-altezza-pagina
                          perform CARICA-FONT
                          perform SCRIVI-INTESTAZIONE
                       end-if
                    else
                       set spl-sta-annu to true
                       exit paragraph
                    end-if
                 end-if
      *              perform SCRIVI-INTESTAZIONE
      *           end-if

                 if tinv-mag not = SaveMagazzino
                    move 11 to righe-diff
                    perform PRONOSTICO
                    perform TOTALI-MARCA
                    perform TOTALI-MAGAZZINO
                 end-if

                 if tinv-marca not = SaveMarca
                    move 6 to righe-diff
                    perform PRONOSTICO
                    perform TOTALI-MARCA
                 end-if

                 initialize line-riga
                 move tinv-qta             to r-qta-edit
                 move tinv-prezzo          to r-prezzo-edit
                 move tinv-valore          to r-valore-edit
                 move tinv-kg              to r-kg-edit
                 move tinv-articolo        to r-articolo
                 move tinv-asterisk        to r-asterisk
                 move tinv-mag             to r-mag-codice
                 move tinv-art-descrizione to r-art-descrizione
                 move tinv-um              to r-udm
                 move r-riga               to line-riga

                 move 43                   to spl-tipo-colonna
                 move Verdana10            to spl-hfont
                 perform SCRIVI
                                
      *           add 1 to num-righe
      *           write line-riga
              
                 move 2 to righe-diff
                 perform PRONOSTICO

                 add tinv-valore  to TotValore
                                     TotValoreMag
                                     TotValoreGen
                 add tinv-utf     to TotUtf
                                     TotUtfMag
                                     TotUtfGen
                 add tinv-non-utf to TotNonUtf
                                     TotNonUtfMag
                                     TotNonUtfGen
                 add tinv-kg      to TotKg
                                     TotKgMag
              end-perform
              close lineseq
           end-if.

      ***---
       SCRIVI-INTESTAZIONE.
           perform STAMPA-FRAME.
           set prima-volta          to false.
           initialize line-riga.
           move num-pagina          to int-pag.
           inspect int-pag replacing trailing low-value by space.
           call "C$JUSTIFY" using int-pag, "R"
           if FileExcel
              if inveday-padre
                 string separatore              delimited size
                        separatore              delimited size
                        "-- STAMPA INVENTARIO"  delimited size
                        " GLOBALE AL "          delimited size
                        data-inventario         delimited size
                        " --"                   delimited size
                        into line-riga
                 end-string
              else
                 string separatore              delimited size
                        separatore              delimited size
                        separatore              delimited size
                        "-- STAMPA INVENTARIO"  delimited size
                        " DETTAGLIATO AL "      delimited size
                        data-inventario         delimited size
                        " --"                   delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
              write line-riga from space
           else
              initialize int-inventario
              if inveday-padre
                 string "STAMPA INVENTARIO"     delimited size
                        " GLOBALE AL "          delimited size
                        data-inventario         delimited size
                        into int-inventario
                 end-string
              else
                 string "STAMPA INVENTARIO"     delimited size
                        " DETTAGLIATO AL "      delimited size
                        data-inventario         delimited size
                        into int-inventario
                 end-string
              end-if
      *        call "C$JUSTIFY" using int-inventario, "C"
              move riga-pagina       to riga-intestazione(120:)
              move riga-intestazione to line-riga
              move 0                 to save-riga
              move 41                to spl-tipo-colonna
              move Verdana12B        to spl-hfont
              perform SCRIVI

              add 0,35               to save-riga
              perform STAMPA-LINEA
           end-if.

      *     write line-riga.
      *     add 1 to num-righe.
      *     initialize line-riga.
      *     write line-riga from space.
      *     add 1 to num-righe.
      *     initialize line-riga.

           if FileExcel
              if inveday-medio move "Prezzo" to tipo-costo
              else             move "Costo"  to tipo-costo
              end-if

              if inveday-padre
                 string "Codice"        delimited size
                        separatore      delimited size
                        separatore      delimited size
                        "Descrizione"   delimited size
                        separatore      delimited size
                        "UM"            delimited size
                        separatore      delimited size
                        "Q.tà"          delimited size
                        separatore      delimited size
                        tipo-costo
                        separatore      delimited size
                        "VALORE"        delimited size
                        separatore      delimited size
                        "Kg."           delimited size
                        into line-riga
                 end-string
              else
                 string "Codice"        delimited size
                        separatore      delimited size
                        separatore      delimited size
                        "Mag."          delimited size
                        separatore      delimited size
                        "Descrizione"   delimited size
                        separatore      delimited size
                        "UM"            delimited size
                        separatore      delimited size
                        "Q.tà"          delimited size
                        separatore      delimited size
                        tipo-costo      delimited size
                        separatore      delimited size
                        "VALORE"        delimited size
                        separatore      delimited size
                        "Kg."           delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
           else
              if inveday-padre move spaces to tit-mag
              else             move "Mag"  to tit-mag
              end-if
              move riga-titolo       to line-riga
              move 1,4               to save-riga
              move 42                to spl-tipo-colonna
              perform SCRIVI

             perform STAMPA-LINEA
           end-if.
      *     write line-riga.
      *     add 1 to num-righe.
      *     initialize line-riga.
           if inveday-medio move " Medio" to como-titolo
                            move "Medio"  to tipo-costo
           else             move "Ultimo" to como-titolo
                            move "Ultimo" to tipo-costo
           end-if
           if FileExcel
              if inveday-padre
                 string separatore     delimited size
                        separatore     delimited size
                        separatore     delimited size
                        separatore     delimited size
                        separatore     delimited size
                        tipo-costo     delimited size
                        into line-riga
                 end-string
              else
                 string separatore     delimited size
                        separatore     delimited size
                        separatore     delimited size
                        separatore     delimited size
                        separatore     delimited size
                        separatore     delimited size
                        tipo-costo     delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
           else
              move riga-titolo-2     to line-riga
              move 1,4               to save-riga
              move 42                to spl-tipo-colonna
              perform SCRIVI

              perform STAMPA-LINEA
           end-if.
      *     write line-riga.
      *     add 1 to num-righe.

      ***---
       TOTALI-MARCA.
           move SaveMarca  to mar-codice
           read tmarche no lock invalid continue.
           inspect SaveMarca replacing leading x"30" by x"20".
           initialize line-riga.
           move TotValore to TotValoreEdit.
           move TotKg     to TotKgEdit.
      ***  RIGA VUOTA
      *     write line-riga from space.
      *     add 1 to num-righe.

           if FileExcel
              write line-riga from space
              if inveday-padre
                 string separatore         delimited size
                        separatore         delimited size
                        "Totale Marca "    delimited size
                        mar-descrizione    delimited size
                        separatore         delimited size
                        SaveMarca          delimited size
                        separatore         delimited size
                        separatore         delimited size
                        separatore         delimited size
                        TotValoreEdit      delimited size
                        separatore         delimited size
                        TotKgEdit          delimited size
                        into line-riga
                 end-string
              else
                 string separatore         delimited size
                        separatore         delimited size
                        separatore         delimited size
                        "Totale Marca "    delimited size
                        mar-descrizione    delimited size
                        separatore         delimited size
                        SaveMarca          delimited size
                        separatore         delimited size
                        separatore         delimited size
                        separatore         delimited size
                        TotValoreEdit      delimited size
                        separatore         delimited size
                        TotKgEdit          delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
           else
              initialize r-marca-tot
              inspect mar-descrizione replacing trailing spaces 
                                                      by low-value
              move SaveMarca to marca-ed
              call "C$JUSTIFY" using marca-ed, "L"
              inspect marca-ed replacing trailing space by low-value
              string "Totale Marca " delimited size
                     mar-descrizione delimited low-value
                     " ("            delimited size
                     marca-ed        delimited low-value
                     ")"             delimited size
                     into r-marca-tot
              end-string
      *        move SaveMarca     to r-marca-codice
              move TotValoreEdit to r-marca-totale
              move TotKgEdit     to r-marca-tot-kg
              move r-marca       to line-riga
              move 5 to righe-diff
              perform PRONOSTICO
              add 0,25 to save-riga
              perform STAMPA-LINEA
              subtract 0,25 from save-riga

              move Verdana12I to spl-hfont
              move 44         to spl-tipo-colonna
              perform SCRIVI
           end-if.

      ***     perform STAMPA-LINEA.

      *     write line-riga.
      *     add 1 to num-righe.

           move tinv-marca to SaveMarca.

           initialize line-riga.
           move TotUtf to TotUtfEdit.
           if FileExcel
              if inveday-padre
                 string separatore          delimited size
                        separatore          delimited size
                        "Totale Kg. U T F"  delimited size
                        separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        TotUtfEdit          delimited size
                        into line-riga
                 end-string
              else
                 string separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        "Totale Kg. U T F"  delimited size
                        separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        TotUtfEdit          delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
           else
              move TotUtfEdit  to r-marca-tot-utf
              move r-marca-utf to line-riga
              move Verdana12I  to spl-hfont
              move 45          to spl-tipo-colonna
              perform SCRIVI
           end-if.
      ***     add 0,5 to save-riga.

      ***     perform STAMPA-LINEA.
      *     write line-riga.
      *     add 1 to num-righe.

           initialize line-riga.
           move TotNonUtf to TotNonUtfEdit.
           if FileExcel
              if inveday-padre
                 string separatore           delimited size
                        separatore           delimited size
                        "Totale Kg. Esenti"  delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        TotNonUtfEdit        delimited size
                        into line-riga
                 end-string
              else
                 string separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        "Totale Kg. Esenti"  delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        TotNonUtfEdit        delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
              write line-riga from space
           else
              move TotNonUtfEdit   to r-marca-tot-non-utf
              move r-marca-non-utf to line-riga
              move Verdana12I      to spl-hfont
              move 45              to spl-tipo-colonna
              perform SCRIVI

              perform STAMPA-LINEA
           end-if.
      ***     add 0,5 to save-riga.

      *     write line-riga.
      *     add 1 to num-righe.

      ***  LINEA TRATTINI
      ***     initialize line-riga.
      ***     if FileTxt
      ***        move riga-trattini to line-riga
      ***     end-if.
      ***     add 0,25 to save-riga.
      ***     move Verdana12I to spl-hfont.
      ***     move 45         to spl-tipo-colonna.
      ***     perform SCRIVI.
      ***
      ***     perform STAMPA-LINEA.
      *     write line-riga.
      *     add 1 to num-righe.
      ***  RIGA BIANCA
      *     initialize line-riga.
      *     write line-riga.
      *     add 1 to num-righe.
           move 0 to TotValore TotKg TotUtf TotNonUtf.

      ***---
       TOTALI-MAGAZZINO.
           if inveday-padre exit paragraph end-if.
           initialize line-riga.
           move TotValoreMag to TotValoreMagEdit.
           move TotKgMag     to TotKgMagEdit.
           if FileExcel
              if inveday-padre
                 string separatore          delimited size
                        separatore          delimited size
                        "Totale Magazzino"  delimited size
                        separatore          delimited size
                        SaveMagazzino       delimited size
                        separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        TotValoreMagEdit    delimited size
                        separatore          delimited size
                        TotKgMagEdit        delimited size
                        into line-riga
                 end-string
              else
                 string separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        "Totale Magazzino"  delimited size
                        separatore          delimited size
                        SaveMagazzino       delimited size
                        separatore          delimited size
                        separatore          delimited size
                        separatore          delimited size
                        TotValoreMagEdit    delimited size
                        separatore          delimited size
                        TotKgMagEdit        delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
           else
              move SaveMagazzino    to r-mag-tot
              move TotValoreMagEdit to r-mag-tot-valore
              move TotKgMagEdit     to r-mag-tot-kg
              move r-mag            to line-riga
              move 5 to righe-diff
              perform PRONOSTICO

              add 0,25 to save-riga
              perform STAMPA-LINEA
              subtract 0,25 from save-riga
      ***        add 0,5 to save-riga
              move Verdana12I to spl-hfont
              move 47         to spl-tipo-colonna
              perform SCRIVI
           end-if.

      ***     perform STAMPA-LINEA.

      *     write line-riga.
      *     add 1 to num-righe.
           move tinv-mag to SaveMagazzino.

           initialize line-riga.
           move TotUtfMag to TotUtfMagEdit.
           if FileExcel
              if inveday-padre
                 string separatore           delimited size
                        separatore           delimited size
                        "Totale Kg. U T F"   delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        TotUtfMagEdit        delimited size
                        into line-riga
                 end-string
              else
                 string separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        "Totale Kg. U T F"   delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        TotUtfMagEdit        delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
           else
              move TotUtfMagEdit to r-mag-tot-utf
              move r-mag-utf     to line-riga
              move Verdana12I to spl-hfont
              move 45         to spl-tipo-colonna
              perform SCRIVI
           end-if.
      ***     add 0,5 to save-riga.

      ***     perform STAMPA-LINEA.

      *     write line-riga.
      *     add 1 to num-righe.

           initialize line-riga.
           move TotNonUtfMag to TotNonUtfMagEdit.
           if FileExcel
              if inveday-padre
                 string separatore           delimited size
                        separatore           delimited size
                        "Totale Kg. Esenti"  delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        TotNonUtfMagEdit     delimited size
                        into line-riga
                 end-string
              else
                 string separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        "Totale Kg. Esenti"  delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        separatore           delimited size
                        TotNonUtfMagEdit     delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
              write line-riga from space
           else
              move TotNonUtfMagEdit to r-mag-tot-non-utf
              move r-mag-non-utf    to line-riga
              move Verdana12I       to spl-hfont
              move 45               to spl-tipo-colonna
              perform SCRIVI

              perform STAMPA-LINEA
           end-if.
      ***     add 0,5 to save-riga.

      *     write line-riga.
      *     add 1 to num-righe.
           
      ***  RIGA TRATTINI + RIGA VUOTA
      ***     initialize line-riga.       
      ***     if FileTxt
      ***        move riga-trattini to line-riga
      ***     end-if.
      ***     write line-riga.
      ***     add 1  to num-righe.
      ***     initialize line-riga.
      ***     write line-riga.
      ***     add  1 to num-righe.
           move 0 to TotValoreMag
                     TotKgMag
                     TotUtfMag
                     TotNonUtfMag.

      ***---
       TOTALI-GENERALI.
           initialize line-riga.
           move TotUtfGen to TotUtfGenEdit.
           if FileExcel
              if inveday-padre
                 string separatore                    delimited size
                        separatore                    delimited size
                        "TOTALE Soggetti U T F --->"  delimited size
                        separatore                    delimited size
                        separatore                    delimited size
                        separatore                    delimited size
                        separatore                    delimited size
                        separatore                    delimited size
                        TotUtfGenEdit                 delimited size
                        into line-riga
                 end-string
              else
                 string separatore                    delimited size
                        separatore                    delimited size
                        separatore                    delimited size
                        "TOTALE Soggetti U T F --->"  delimited size
                        separatore                    delimited size
                        separatore                    delimited size
                        separatore                    delimited size
                        separatore                    delimited size
                        separatore                    delimited size
                        TotUtfGenEdit                 delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
           else
              move TotUtfGenEdit to r-gen-tot-utf
              move r-gen-utf     to line-riga
              move 5 to righe-diff
              perform PRONOSTICO
              add 0,25 to save-riga
              perform STAMPA-LINEA
              subtract 0,25 from save-riga

      ***        add 0,5 to save-riga
              move Verdana12I to spl-hfont
              move 46         to spl-tipo-colonna
              perform SCRIVI
           end-if.

      ***     perform STAMPA-LINEA.

      *     write line-riga.
      *     add 1 to num-righe.

           initialize line-riga.
           move TotNonUtfGen to TotNonUtfGenEdit.
           if FileExcel
              if inveday-padre
                 string separatore                  delimited size
                        separatore                  delimited size
                        "TOTALE Esenti U T F --->"  delimited size
                        separatore                  delimited size
                        separatore                  delimited size
                        separatore                  delimited size
                        separatore                  delimited size
                        separatore                  delimited size
                        TotNonUtfGenEdit            delimited size
                        into line-riga
                 end-string
              else
                 string separatore                  delimited size
                        separatore                  delimited size
                        separatore                  delimited size
                        "TOTALE Esenti U T F --->"  delimited size
                        separatore                  delimited size
                        separatore                  delimited size
                        separatore                  delimited size
                        separatore                  delimited size
                        separatore                  delimited size
                        TotNonUtfGenEdit            delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
           else
              move TotNonUtfGenEdit to r-gen-tot-non-utf
              move r-gen-non-utf    to line-riga
              move Verdana12I to spl-hfont
              move 46         to spl-tipo-colonna
              perform SCRIVI
           end-if.
      ***     add 0,5 to save-riga.

      ***     perform STAMPA-LINEA.

      *     write line-riga.
      *     add 1 to num-righe.

           initialize line-riga.
           move TotValoreGen to TotValoreGenEdit.
           if FileExcel
              if inveday-padre
                 string separatore             delimited size
                        separatore             delimited size
                        "TOTALE VALORE --->"   delimited size
                        separatore             delimited size
                        separatore             delimited size
                        separatore             delimited size
                        separatore             delimited size
                        separatore             delimited size
                        TotValoreGenEdit       delimited size
                        into line-riga
                 end-string
              else
                 string separatore             delimited size
                        separatore             delimited size
                        separatore             delimited size
                        "TOTALE VALORE --->"   delimited size
                        separatore             delimited size
                        separatore             delimited size
                        separatore             delimited size
                        separatore             delimited size
                        separatore             delimited size
                        TotValoreGenEdit       delimited size
                        into line-riga
                 end-string
              end-if
              write line-riga
              initialize line-riga
              write line-riga from space
           else
              move TotValoreGenEdit to r-gen-tot-valore
              move r-gen            to line-riga
              move Verdana12I       to spl-hfont
              move 46               to spl-tipo-colonna
              perform SCRIVI

              perform STAMPA-LINEA
           end-if.
      ***     add 0,5 to save-riga.

      *     write line-riga.
      *     add 1 to num-righe.
           
      ***  RIGA BIANCA + TRATTINI
      ***     initialize line-riga.
      ***     if FileTxt
      ***        move riga-trattini to line-riga
      ***     end-if.
      ***     write line-riga.
      ***     add 1 to num-righe.
      ***     initialize line-riga.
      ***     write line-riga.
      ***     add 1 to num-righe.

      ***---
       VALORIZZA-RIGA.
           add 1             to prg-riga.
           set trovato       to true.
           initialize tinv-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.

           move prg-riga            to tinv-prog.
           move prg-cod-articolo    to tinv-articolo.
           move prg-cod-magazzino   to tinv-mag.
           move prg-tipo-imballo    to tinv-imballo.
           move prg-peso            to tinv-peso.
           move prg-giacenza-udm    to tinv-qta.

           if inveday-figlio
              move prg-tipo-imballo to imq-codice
              read timbalqta
                   invalid continue
               not invalid
                   move imq-tipo  to imb-codice
                   read timballi no lock invalid continue end-read
              end-read
              inspect art-descrizione replacing trailing spaces 
                                                      by low-value
              inspect imb-descrizione replacing trailing spaces 
                                                      by low-value
              initialize tinv-art-descrizione
              move imq-qta-imb       to imballi-ed
              call "C$JUSTIFY"    using imballi-ed, "L"
              string art-descrizione delimited by low-value
                     " - "           delimited by size
                     imb-descrizione delimited by low-value
                     " da "          delimited by size
                     imballi-ed      delimited by spaces
                     " x "           delimited by size
                     art-udm-imballo delimited by size
                     into tinv-art-descrizione
              end-string
              compute tinv-kg       = prg-peso         * tinv-qta
              compute tinv-utf      = prg-peso-utf     * tinv-qta
              compute tinv-non-utf  = prg-peso-non-utf * tinv-qta
LUBEXX        if prg-peso-utf = 0
LUBEXX           move "*" to tinv-asterisk
LUBEXX        end-if
           else

LUBEXX        |Prendo il peso dall-articolo
LUBEXX        compute art-peso-tot = 
LUBEXX                art-peso-utf + art-peso-non-utf

              move art-descrizione to tinv-art-descrizione
              compute tinv-kg       = art-peso-tot     * tinv-qta
              compute tinv-utf      = art-peso-utf     * tinv-qta
              compute tinv-non-utf  = art-peso-non-utf * tinv-qta

LUBEXX        if art-peso-utf = 0
LUBEXX           move "*" to tinv-asterisk
LUBEXX        end-if

           end-if.

           move art-unita-di-misura   to tinv-um.
           move art-marca-prodotto    to tinv-marca.
           if inveday-ultimo
              move art-prezzo-vendita to tinv-prezzo
           else
      *****        perform CALCOLA-COSTO-MP
              add 0,005 to costo-mp giving costo-mp-2dec
              move costo-mp-2dec        to tinv-prezzo
           end-if.

           compute tinv-valore = tinv-qta * tinv-prezzo.

           write tinv-rec invalid continue end-write.

      ***---
       CLOSE-FILES.
           close articoli progmag tmarche
                 timballi timbalqta timposte.
           close tmp-inventory.
           delete file tmp-inventory.
  
      ***---
       EXIT-PGM.
           move path-txt to inveday-path.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           cancel "spooler".
           initialize spooler-link.
           goback.

      ***---
       PRONOSTICO.
           if FileTxt
             if save-altezza-pagina < (save-riga + (passo * righe-diff))
                 perform SALTO-PAGINA
                 if si-intestazione
                    perform SCRIVI-INTESTAZIONE
                 end-if
              end-if
           end-if.

      ***---
       SALTO-PAGINA.
      *     move 0 to num-righe.
      *     add 1 to num-pagina.
      *     write line-riga from spaces after page.
      *     initialize line-riga.
      *     write line-riga from x"09" after 1.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           add 1 to num-pagina.
           perform SCRIVI-INTESTAZIONE.

      ***---

       STAMPA-FRAME.
           move 2                    to spl-pen-with.
           move  8,1                 to spl-colonna.
           move 22,5                 to spl-colonna-fine.
           move 0,4                  to spl-riga.
           move 1,2                  to spl-riga-fine.
           set  spl-oggetto          to true.
           set  spl-rettangolo-round to true.
           set  spl-brush-ltgray     to true.
           call "spooler"         using spooler-link.

      ***---
       STAMPA-LINEA.
           move 3                  to spl-pen-with.
           move 0,7                to spl-colonna.
           move 28,3               to spl-colonna-fine.
           add  passo 0,3          to save-riga.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           move line-riga     to spl-riga-stampa.
           call "spooler"  using spooler-link.
           initialize spl-riga-stampa.

      ***---
       PARAGRAFO-COPY.
       copy "common-excel.cpy".
       copy "costo-medio.cpy".
       copy "recupero-anagrafica.cpy".
