       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      statgruppi.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "lineseq.sl". 
           copy "articoli.sl".
           copy "progmag.sl".
           copy "tmp-stat.sl".
           copy "tmarche.sl". 
           copy "tparamge.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "tmp-stat.fd".
           copy "tmarche.fd".
           copy "tparamge.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "opensave.def".
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".

       78  titolo                value "Stampa Statistiche Gruppi".
       
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  scelta                pic 9.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  passo                 pic 9v99.

       77  messaggio             pic x(150) value SPACES.
       77  font-size-dply        pic z(5).
       77  Verdana12B            handle of font.
       77  Verdana10             handle of font.
       77  Verdana10B            handle of font.
       77  WFONT-STATUS          pic s9(5) value ZERO.

       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.
       77  status-progmag        pic xx.
       77  status-tmp-stat       pic xx.
       77  status-tmarche        pic xx.
       77  status-tparamge       pic xx.

       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).

       77  tot-idx               pic 9(4).
       77  como-valore           pic s9(15)v999.
       01  como-chiave.
         05 SaveMagazzino        pic x(3).
         05 SaveMarca            pic 9(4).

       01  marche-occurs.
        05 marche-elemento       occurs 9999 indexed by idx-marca.
           10 el-chiave.
              15 el-mag          pic x(3).
              15 el-marca        pic 9(4).
           10 el-ven             pic s9(12)v99.
           10 el-ven-1000        pic s9(12)v99.
           10 el-acq             pic s9(12)v99.
           10 el-acq-1000        pic s9(12)v99.
           10 el-gia-utf         pic s9(12)v99.
           10 el-gia-non-utf     pic s9(12)v99.

       77  TotVendite            pic s9(12)v999 value 0.
       77  TotVendite1000        pic s9(12)v99 value 0.
       77  TotAcquisti           pic s9(12)v999 value 0.
       77  TotAcquisti1000       pic s9(12)v99 value 0.
       77  TotUtf                pic s9(12)v999 value 0.
       77  TotNonUtf             pic s9(12)v999 value 0.

       77  filler                pic 9.
           88 trovata-marca      value 1, false 0.
       77  filler                pic 9.
           88 RecLocked          value 1, false 0.
       77  filler                pic 9.
           88  prima-volta       value 1, false 0.
       77  filler                pic 9.
           88  record-ok         value 1, false 0.
       77  FlagTrovato           pic 9.
           88  trovato           value 1, false 0.
       77  controllo             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".

      * RIGHE PER LA STAMPA
       01  riga-titolo              pic x(135).

       01  riga-trattini            pic x(135) value all "-".

       01  riga-intestazione.
           05 filler                pic x(3)  value "Mag".
           05 filler                pic x(16) value "Marca Statistica".
           05 filler                pic x(10) value "VENDITE Kg".
           05 filler                pic x(10) value "Val / 1000".
           05 filler                pic x(11) value "ACQUISTI Kg".
           05 filler                pic x(10) value "Val / 1000".
           05 filler                pic x(9)  value "Giac. UTF". 
           05 filler                pic x(11) value "Giac. Altre".

       01  r-riga.
           05 r-mag                 pic x(3).
           05 r-marca               pic z(4).
           05 r-des-marca           pic x(30).
           05 r-ven                 pic ---.---.--9,999.|53
           05 r-ven-1000            pic ---.---.--9,99. |67
           05 r-acq                 pic ---.---.--9,999.|82
           05 r-acq-1000            pic ---.---.--9,99. |96
           05 r-gia-utf             pic ---.---.--9,999.|111
           05 r-gia-non-utf         pic ---.---.--9,999.|126

       01  r-totali.
           05 filler                pic x(27) 
                                    value "   T O T A L I ------>".
           05 r-tot-ven             pic ---.---.--9,999.
           05 r-tot-ven-1000        pic ---.---.--9,99. 
           05 r-tot-acq             pic ---.---.--9,999.
           05 r-tot-acq-1000        pic ---.---.--9,99. 
           05 r-tot-utf             pic ---.---.--9,999.
           05 r-tot-non-utf         pic ---.---.--9,999.

       78  max-righe        value 46.
       77  num-righe        pic 99 value 0.
       77  diff-righe       pic 99 value 0.
       77  n-vuote          pic 99 value 0.
       77  sav-riga         pic x(900).
       77  como-riga        pic x(900).
       77  sw-corpo         pic 9  value 0.
       77  num-pagina       pic 999.
       77  num-pagina-ed    pic zz9.
       77  tot-valori       pic s9(15)v999.
       77  righe-diff       pic 99.

       LINKAGE SECTION.
       77  path-txt                 pic x(256).
       77  link-user                pic x(10).

      ******************************************************************
       PROCEDURE DIVISION using path-txt.

       DECLARATIVES.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File progressivi [PROGMAG] inesistente"
                          title titolo
                           icon 2
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
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

      ***---
       TMP-STAT-ERR SECTION.
           use after error procedure on tmp-stat.
           set tutto-ok  to true.
           evaluate status-tmp-stat
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tmp [TMP-STAT-ERR] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMP-STAT-ERR] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMP-STAT-ERR] Indexed file corrupt!"
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
                move   "File TMP"   to geslock-nome-file
                call   "geslock" using geslock-linkage
                cancel "geslock"
                evaluate true
                when riprova
                     perform OPEN-OUTPUT-TMP-STAT
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
           end-evaluate.

      ***---
       TPARAMGE-ERR SECTION.
           use after error procedure on tparamge.
           set tutto-ok  to true.
           evaluate status-tparamge
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File parametri [TPARAMGE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TPARAMGE] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TPARAMGE] Indexed file corrupt!"
                          title titolo
                           icon 3
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
      *-
           set environment "PRINTER" to "-P SPOOLER".
           move 0,5 to passo.
           move 0 to tot-idx.
           move 1 to num-pagina.
           initialize marche-occurs replacing numeric data by zeroes
                                         alphanumeric data by spaces.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-tmp wstampa path-txt.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           accept  path-tmp     from environment "PATH-ST".
           inspect path-tmp     replacing trailing spaces by low-value.
           inspect link-user    replacing trailing spaces by low-value.
           string  path-tmp     delimited by low-value
                   "statgruppi" delimited by size
                   "_"          delimited by size
                   link-user    delimited by low-value
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   ".txt"       delimited by size
                   into path-txt
           end-string.
           string  path-tmp     delimited by low-value
                   "statgruppi" delimited by size
                   "_"          delimited by size
                   link-user    delimited by low-value
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   ".tmp"       delimited by size
                   into path-tmp
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

      * Verdana 10
           initialize wfont-data Verdana10B.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10B, wfont-data
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

           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   wfont-name       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size
                   font-size-dply,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-TMP-STAT.
           if tutto-ok
              perform OPEN-OUTPUT-LINESEQ
              if tutto-ok
                 open input progmag
                            articoli
                            tmarche
                            tparamge
              else
                 close       tmp-stat
                 delete file tmp-stat
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-TMP-STAT.
           open output tmp-stat.

      ***---
       OPEN-OUTPUT-LINESEQ.
           move path-txt to wstampa.
           open output lineseq.
           write line-riga from space after 0.

      ***---
       ELABORAZIONE.
           move spaces to tge-codice.
           read tparamge no lock invalid continue end-read.
           move low-value       to prg-rec.
           start progmag key is >= prg-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read progmag next no lock at end exit perform end-read

                 if prg-cod-magazzino not = spaces or
                    prg-tipo-imballo  not = spaces or
                    prg-peso          not = 0 |E' un record figlio
                    perform VALORIZZA-RIGA
                 end-if
                     
                 if errori
                    exit perform
                 end-if

              end-perform
           end-if.

           if not trovato
              close lineseq 
              delete file lineseq
              display message "Nessun movimento trovato"
                        title titolo
                         icon 2
           else
              if tutto-ok
                 perform GENERA-FILE-TXT
              end-if
           end-if.

      ***---
       GENERA-FILE-TXT.
           move path-txt to wstampa.
           perform varying idx-marca from 1 by 1 
                     until idx-marca > tot-idx
              if el-mag  (idx-marca) = spaces and 
                 el-marca(idx-marca) = spaces
                 exit perform
              end-if
              move el-mag(idx-marca)         to tst-mag
              move el-marca(idx-marca)       to tst-marca
              move el-ven(idx-marca)         to tst-ven-kg
              move el-ven-1000(idx-marca)    to tst-ven-1000
              move el-acq(idx-marca)         to tst-acq-kg
              move el-acq-1000(idx-marca)    to tst-acq-1000
              move el-gia-utf(idx-marca)     to tst-gia-utf
              move el-gia-non-utf(idx-marca) to tst-gia-non-utf 
              
              add tst-ven-1000          to TotVendite1000
              add tst-acq-1000          to TotAcquisti1000

              compute tst-ven-1000 rounded = tst-ven-1000 / 1000
              compute tst-acq-1000 rounded = tst-acq-1000 / 1000

              move tst-marca to mar-codice
              read tmarche no lock
                   invalid move "*** NON TROVATA ***" to mar-descrizione
              end-read
              move mar-descrizione to tst-des-marca

              write tst-rec invalid rewrite tst-rec end-write
           end-perform.

           close tmp-stat.
           open input tmp-stat.

           move low-value to tst-rec.
           start tmp-stat key is >= tst-kmarca-alfa
                 invalid continue
           end-start.

           perform until 1 = 2
              read tmp-stat next
                   at end
                   perform TOTALI-GENERALI
                   exit perform
              end-read

              set record-ok to true

              initialize spooler-link
              if prima-volta
                 call   "selprint" using selprint-linkage
                 cancel "selprint"

                 if selprint-stampante not = space
                    move selprint-num-copie to SPL-NUM-COPIE
                    move selprint-stampante to SPL-NOME-STAMPANTE

                    move "GESLUX - Statistiche Gruppi" to spl-nome-job
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

              initialize line-riga
              move tst-mag         to r-mag
              move tst-marca       to r-marca
              move tst-des-marca   to r-des-marca
              move tst-ven-kg      to r-ven
              move tst-ven-1000    to r-ven-1000
              move tst-acq-kg      to r-acq
              move tst-acq-1000    to r-acq-1000
              move tst-gia-utf     to r-gia-utf
              move tst-gia-non-utf to r-gia-non-utf

              if tst-ven-kg   = 0 and
                 tst-ven-1000 = 0 and
                 tst-acq-kg   = 0 and
                 tst-acq-1000 = 0 and
                 tst-gia-utf  = 0 and
                 tst-gia-non-utf
                 set record-ok to false
              end-if

              if record-ok
                 move r-riga          to line-riga
                 move 50              to spl-tipo-colonna
                 move Verdana10       to spl-hfont
                 perform SCRIVI
                 move 2 to righe-diff
                 perform PRONOSTICO

                 add tst-ven-kg            to TotVendite
                 add tst-acq-kg            to TotAcquisti
                 add tst-gia-utf           to TotUtf
                 add tst-gia-non-utf       to TotNonUtf
              end-if

           end-perform.

           close lineseq.

      ***---
       SCRIVI-INTESTAZIONE.
           perform STAMPA-FRAME.
           set prima-volta        to false.
           initialize riga-titolo.
           string "ACQUISTI - VENDITE  al"       delimited by size
                  " "                            delimited by size
                  tge-data-consolid-progmag(7:2) delimited by size
                  "/"                            delimited by size
                  tge-data-consolid-progmag(5:2) delimited by size
                  "/"                            delimited by size
                  tge-data-consolid-progmag(3:2) delimited by size
                  into riga-titolo
           end-string.

           move "Pag. "         to riga-titolo(120:5).
           move num-pagina      to num-pagina-ed.
           move num-pagina-ed   to riga-titolo(125:3).

           initialize line-riga.
           move riga-titolo to line-riga.

           move 0           to save-riga.
           move 48          to spl-tipo-colonna.
           move Verdana12B  to spl-hfont.
           perform SCRIVI.

           add 0,35               to save-riga.
           perform STAMPA-LINEA.

           initialize line-riga.
           move riga-intestazione to line-riga.
           move 49          to spl-tipo-colonna.
           move Verdana10B  to spl-hfont.
           perform SCRIVI.
           perform STAMPA-LINEA.

      ***---
       TOTALI-GENERALI.
           move 4 to righe-diff.
           perform PRONOSTICO.
           add 0,25 to save-riga.
           perform STAMPA-LINEA.
           subtract 0,25 from save-riga.

           compute TotVendite1000  rounded = TotVendite1000  / 1000.
           compute TotAcquisti1000 rounded = TotAcquisti1000 / 1000.

           move TotVendite      to r-tot-ven.
           move TotVendite1000  to r-tot-ven-1000.
           move TotAcquisti     to r-tot-acq.
           move TotAcquisti1000 to r-tot-acq-1000.
           move TotUtf          to r-tot-utf.
           move TotNonUtf       to r-tot-non-utf.
           initialize line-riga.
           move r-totali   to line-riga.
           move Verdana10B to spl-hfont.
           move 51         to spl-tipo-colonna.
           perform SCRIVI.

           perform STAMPA-LINEA.

      ***---
       VALORIZZA-RIGA.
           move prg-cod-articolo   to art-codice.
           read articoli no lock invalid continue end-read.
           move prg-cod-magazzino  to SaveMagazzino.
           move art-marca-prodotto to SaveMarca.
           set trovato             to true.
           set trovata-marca       to false.
           set idx-marca           to 1.

           search marche-elemento
           when el-chiave(idx-marca) = como-chiave
                set trovata-marca to true
           end-search.

           if not trovata-marca
              add  1             to tot-idx
              move tot-idx       to idx-marca
              move SaveMagazzino to el-mag(idx-marca)
              move SaveMarca     to el-marca(idx-marca)
           end-if.

           compute como-valore = prg-ven-kg - prg-resi-da-cli-kg.
           add como-valore to el-ven(idx-marca).

           compute como-valore = prg-ven-valore - prg-resi-da-cli-valore
           add como-valore to el-ven-1000(idx-marca).


           compute como-valore = prg-acq-kg - prg-resi-fornitori-kg +
LUBEXX                           prg-kg-el - prg-kg-ul + prg-var-inv-kg.
           add como-valore to el-acq(idx-marca).

           compute como-valore =
                   prg-acq-valore - prg-resi-fornitori-valore +
LUBEXX             prg-valore-el  - prg-valore-ul + prg-var-inv-valore.
           add como-valore to el-acq-1000(idx-marca).


           compute como-valore = prg-giacenza-udm * prg-peso-utf.
           add como-valore to el-gia-utf(idx-marca).

           compute como-valore = prg-giacenza-udm * prg-peso-non-utf.
           add como-valore to el-gia-non-utf(idx-marca).

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           add 1 to num-pagina.
           perform SCRIVI-INTESTAZIONE.

      ***---
       STAMPA-RIGA.
           initialize sav-riga
           move line-riga to sav-riga
           if num-righe > max-righe - 5
              move sav-riga  to como-riga
              perform SALTO-PAGINA
              perform SCRIVI-INTESTAZIONE
              move como-riga to sav-riga
           end-if
           move sav-riga to line-riga
           write line-riga
           add 1 to num-righe.

      ***---
       CLOSE-FILES.
           close articoli progmag tmarche tparamge.
           close tmp-stat.
           delete file tmp-stat.
  
      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           cancel "spooler".
           initialize spooler-link.
           goback.

      ***---
       STAMPA-FRAME.
           move 2                    to spl-pen-with.
           move  9,1                 to spl-colonna.
           move 21,5                 to spl-colonna-fine.
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
       PRONOSTICO.
           if save-altezza-pagina < (save-riga + (passo * righe-diff))
              perform SALTO-PAGINA
           end-if.
