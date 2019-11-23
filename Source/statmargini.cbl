       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      statmargini.
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
           copy "timposte.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "lineseq.fd".
           copy "articoli.fd".
           copy "progmag.fd".
           copy "tmp-stat.fd".
           copy "tmarche.fd".
           copy "tparamge.fd".
           copy "timposte.fd".

       WORKING-STORAGE SECTION.
       copy "Acugui.def".
       copy "link-geslock.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".
       copy "costo-medio.def".
       copy "imposte.def".

       78  titolo           value "Stampa Statistiche Gruppi - Margini".

       77  messaggio             pic x(150) value SPACES.
       77  font-size-dply        pic z(5).
       77  Verdana12B            handle of font.
       77  Verdana10B            handle of font.
       77  Verdana10             handle of font.
       77  WFONT-STATUS          pic s9(5) value ZERO.

       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).
       77  scelta                pic 9.
       77  passo                 pic 9v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.

       77  status-articoli       pic xx.
       77  status-lineseq        pic xx.
       77  status-progmag        pic xx.
       77  status-tmp-stat       pic xx.
       77  status-tmarche        pic xx.
       77  status-tparamge       pic xx.
       77  status-timposte       pic xx.

       77  path-tmp              pic x(256).
       77  wstampa               pic x(256).

       77  tot-idx               pic 9(4).
       01  como-chiave.
         05 SaveMagazzino        pic x(3).
         05 SaveMarca            pic 9(4).

       01  marche-occurs.
        05 marche-elemento       occurs 9999 indexed by idx-marca.
           10 el-chiave.
              15 el-mag          pic x(3).
              15 el-marca        pic 9(4).
           10 el-ini             pic s9(12)v99.
           10 el-acq             pic s9(12)v99.
           10 el-ven             pic s9(12)v99.
           10 el-fin             pic s9(12)v99.
           10 el-margine         pic s9(12)v99.

       77  TotIniziali           pic s9(12)v99 value 0.
       77  TotAcquisti           pic s9(12)v99 value 0.
       77  TotVendite            pic s9(12)v99 value 0.
       77  TotFinali             pic s9(12)v99 value 0.
       77  TotMargine            pic s9(12)v99 value 0.

       77  como-acquisti         pic s9(12)v99 value 0.
       77  como-vendite          pic s9(12)v99 value 0.
       77  como-finali           pic s9(12)v99 value 0.
       77  como-iniziali         pic s9(12)v99 value 0.
       77  como-margine          pic s9(12)v99 value 0.

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
       77  num-pagina            pic 999.
       77  num-pagina-ed         pic zz9.

      * RIGHE PER LA STAMPA
       01  riga-titolo              pic x(131).

       01  riga-pagina.
           05 filler                pic x(5) value "Pag. ".
           05 st-pag                pic z(3).

       01  riga-trattini            pic x(131) value all "-".

       01  riga-intestazione.
           05 filler                pic x(3)  value "Mag".
           05 filler                pic x(5)  value "MARCA".
           05 filler                pic x(8)  value "INIZIALI".
           05 filler                pic x(8)  value "ACQUISTI".
           05 filler                pic x(7)  value "VENDITE".
           05 filler                pic x(6)  value "FINALI".
           05 filler                pic x(7)  value "MARGINE". 

       01  r-riga.
           05 r-mag                 pic x(3).              |4
           05 r-marca               pic z(4).              |8
           05 r-des-marca           pic x(27).             |35
           05 r-ini                 pic ---.---.---.--9,99.|53
           05 r-acq                 pic ---.---.---.--9,99.|71
           05 r-ven                 pic ---.---.---.--9,99.|89
           05 r-fin                 pic ---.---.---.--9,99.|107
           05 r-margine             pic ---.---.---.--9,99.|125

       01  r-totali.
           05 filler                pic x(22)
                                    value "   T O T A L I ------>".|23
           05 r-tot-ini             pic ---.---.---.--9,99.|41
           05 r-tot-acq             pic ---.---.---.--9,99.|59
           05 r-tot-ven             pic ---.---.---.--9,99.|77
           05 r-tot-fin             pic ---.---.---.--9,99.|95
           05 r-tot-margine         pic ---.---.---.--9,99.|113

       LINKAGE SECTION.
       77  path-txt                 pic x(256).
       77  link-user                pic x(10).

      ******************************************************************
       PROCEDURE DIVISION using path-txt.

       DECLARATIVES.

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
                display message "[PROGMAG] File Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "Indexed [PROGMAG] file corrupt!"
                          title titolo
                           icon 3
                set errori to true
           when "93"
           when "99"
                set RecLocked to true
                set errori    to true
           end-evaluate.

       TMP-STAT-ERR SECTION.
           use after error procedure on tmp-stat.
           set tutto-ok  to true.
           evaluate status-tmp-stat
           when "35"
                display message "Impossibile procedere."
                  x"0d0a""File tmp [TMP-STAT] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                display message "File [TMP-STAT] Mismatch size!"
                          title titolo
                           icon 3
                set errori to true
           when "98"
                display message "[TMP-STAT] Indexed file corrupt!"
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
           move 0   to tot-idx.
           move 1   to num-pagina.
           initialize marche-occurs replacing numeric data by zeroes
                                         alphanumeric data by spaces.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-tmp wstampa path-txt.
           set tutto-ok          to true.
           set trovato           to false.
           set prima-volta       to true.
           accept  path-tmp      from environment "PATH-ST".
           inspect path-tmp      replacing trailing spaces by low-value.
           inspect link-user     replacing trailing spaces by low-value.
           string  path-tmp      delimited low-value
                   "statmargini" delimited size
                   "_"           delimited size
                   link-user     delimited low-value
                   "_"           delimited size
                   como-data     delimited size
                   "_"           delimited size
                   como-ora      delimited size
                   ".tmp"        delimited size
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
           perform OPEN-OUTPUT-TMP-STAT.
           if tutto-ok
              if tutto-ok
                 open input progmag
                            articoli
                            tmarche
                            tparamge
                            timposte
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

      ***---
       ELABORAZIONE.
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move spaces to tge-codice.
           read tparamge no lock invalid continue end-read.
           move low-value       to prg-rec.
           start progmag key is >= prg-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read progmag next no lock at end exit perform end-read
                                                                       
                 |Lo calcolo comunque sul padre come da
                 |richiesta di Trivella in data 04/01/06
                 if prg-cod-magazzino = spaces and
                    prg-tipo-imballo  = spaces and
                    prg-peso          = 0
                    set record-ok to true
                    perform CALCOLA-COSTO-MP-COMPLETO
                    add 0,005 to costo-mp giving costo-mp-2dec
                 end-if

                 if prg-cod-magazzino not = spaces or
                    prg-tipo-imballo  not = spaces or
                    prg-peso          not = 0 |E' un record figlio
                    perform VALORIZZA-RIGA
                 end-if                                    

                 if errori exit perform end-if

              end-perform
           end-if.

           if not trovato
              close lineseq 
              delete file lineseq
              display message "Nessun movimento trovato"
                        title titolo
                         icon 2
           else
              if tutto-ok perform GENERA-FILE-TXT end-if
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
              move el-mag(idx-marca)     to tst-mag
              move el-marca(idx-marca)   to tst-marca
              move el-ini(idx-marca)     to tst-ini
              move el-acq(idx-marca)     to tst-acq
              move el-ven(idx-marca)     to tst-ven
              move el-fin(idx-marca)     to tst-fin
LUBEXX*****              move el-margine(idx-marca) to tst-margine

              move tst-marca to mar-codice
              read tmarche no lock
                   invalid
                   move "*** NON TROVATA ***" to mar-descrizione
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
                   if save-riga > save-altezza-pagina - 1,5
                      perform SALTO-PAGINA
                   end-if
                   perform TOTALI-GENERALI
                   exit perform
              end-read

              initialize spooler-link
              if prima-volta
                 call   "selprint" using selprint-linkage
                 cancel "selprint"

                 if selprint-stampante not = space
                    move selprint-num-copie to spl-num-copie
                    move selprint-stampante to spl-nome-stampante

                    move "GESLUX - Stampa Marginalità" to spl-nome-job
                    set spl-apertura         to true
                    set spl-horizontal       to true
                    set wfdevice-win-printer to true
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

              if save-riga > ( save-altezza-pagina - passo )
                 perform SALTO-PAGINA
              end-if

              set record-ok to true
              initialize line-riga
              move tst-mag         to r-mag
              move tst-marca       to r-marca
              move tst-des-marca   to r-des-marca
              move tst-ini         to r-ini
              move tst-acq         to r-acq
              move tst-ven         to r-ven
              move tst-fin         to r-fin

LUBEXX        compute tst-margine = ( tst-ven + tst-fin ) -
LUBEXX                              ( tst-ini + tst-acq )

              move tst-margine     to r-margine
              perform JUSTIFY-RIGHT

              if tst-ini     = 0 and  tst-acq = 0 and
                 tst-ven     = 0 and  tst-fin = 0 and
                 tst-margine = 0
                 set record-ok to false
              end-if

              if record-ok
                 move r-riga          to line-riga

                 move 15              to spl-tipo-colonna
                 move Verdana10       to spl-hfont
                 perform SCRIVI

                 add tst-ini          to TotIniziali
                 add tst-acq          to TotAcquisti
                 add tst-ven          to TotVendite
                 add tst-fin          to TotFinali
                 add tst-margine      to TotMargine
              end-if

           end-perform.

           if not spl-sta-annu
              set spl-chiusura to true
              call   "spooler" using spooler-link
              cancel "spooler"
           end-if.

           close lineseq.

      ***---
       SCRIVI-INTESTAZIONE.
           perform STAMPA-FRAME.

           set prima-volta        to false.
           initialize riga-titolo.
           string "CALCOLO MARGINI al"           delimited by size
                  " "                            delimited by size
                  tge-data-consolid-progmag(7:2) delimited by size
                  "/"                            delimited by size
                  tge-data-consolid-progmag(5:2) delimited by size
                  "/"                            delimited by size
                  tge-data-consolid-progmag(3:2) delimited by size
                  into riga-titolo
           end-string.

           move num-pagina        to num-pagina-ed.
           move num-pagina-ed     to st-pag.
           move riga-pagina       to riga-titolo(120:).

           move 0                 to save-riga.
           move 13                to spl-tipo-colonna.
           move riga-titolo       to line-riga.
           move Verdana12B        to spl-hfont.
           perform SCRIVI.

           add 0,35               to save-riga.

           perform STAMPA-LINEA.
                                               
           move Verdana10B        to spl-hfont.
           move 1,4               to save-riga.
           move 14                to spl-tipo-colonna.
           move riga-intestazione to line-riga.
           perform SCRIVI.

           perform STAMPA-LINEA.

      ***---
       TOTALI-GENERALI.
           move spaces to line-riga.
           perform SCRIVI.

           add 0,25 to save-riga.
           perform STAMPA-LINEA.
           subtract 0,25 from save-riga.

           move TotIniziali to r-tot-ini.
           move TotAcquisti to r-tot-acq.
           move TotVendite  to r-tot-ven.
           move TotFinali   to r-tot-fin.
           move TotMargine  to r-tot-margine.

           move Verdana10B to spl-hfont.
           move r-totali   to line-riga.
           move 16         to spl-tipo-colonna.
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

      *****     perform CALCOLA-COSTO-MP.

           add prg-ini-valore    to el-ini(idx-marca).     
      *****     compute como-iniziali = prg-giacenza-udm * prg-costo-medio.
      *****     add como-iniziali    to el-ini(idx-marca).

           compute como-vendite = 
                   prg-ven-valore - prg-resi-da-cli-valore.
           add como-vendite to el-ven(idx-marca).

           compute como-acquisti =
                   prg-acq-valore - prg-resi-fornitori-valore +
LUBEXX             prg-valore-el  - prg-valore-ul + prg-var-inv-valore.
           add como-acquisti to el-acq(idx-marca).   

           compute como-finali = prg-giacenza-udm * costo-mp-2dec.
           add como-finali       to el-fin(idx-marca).
                          
LUBEXX*****           compute como-margine = prg-ini-valore +
LUBEXX*****                                  como-acquisti - |E' già valorizzato 
LUBEXX*****                                                  |con FINALE dalla 
LUBEXX*****                                                  |precedente COMPUTE
LUBEXX*****                                  como-vendite -
LUBEXX*****                                  como-finali.
LUBEXX*****           add como-margine       to el-margine(idx-marca).

      ***---
       STAMPA-FRAME.
           move 2                    to spl-pen-with.
           move  7,8                 to spl-colonna.
           move 19,5                 to spl-colonna-fine.
           move 0,4                  to spl-riga.
           move 1,2                  to spl-riga-fine.
           set  spl-oggetto          to true.
           set  spl-rettangolo-round to true.
           set  spl-brush-ltgray     to true.
           call "spooler"         using spooler-link.

      ***---
       SCRIVI.
           add  passo         to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           move line-riga     to spl-riga-stampa.
           call "spooler"  using spooler-link.
           initialize spl-riga-stampa.

      ***---
       STAMPA-LINEA.
           move 3                  to spl-pen-with.
           move 0,7                to spl-colonna.
           move 28,29              to spl-colonna-fine.
           add  passo 0,3          to save-riga.
           move save-riga          to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           call "spooler"       using spooler-link.

      ***---
       JUSTIFY-RIGHT.
           call "C$JUSTIFY" using r-marca,  "R".
           call "C$JUSTIFY" using r-ini,    "R".
           call "C$JUSTIFY" using r-acq,    "R".
           call "C$JUSTIFY" using r-ven,    "R".
           call "C$JUSTIFY" using r-fin,    "R".
           call "C$JUSTIFY" using r-margine,"R".

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.
           add 1 to num-pagina.
           perform SCRIVI-INTESTAZIONE.

      ***---
       CLOSE-FILES.
           close articoli progmag tmarche tparamge timposte.
           close tmp-stat.
           delete file tmp-stat.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Verdana12B.
           destroy Verdana10.
           destroy Verdana10B.

           cancel "spooler".
           initialize spooler-link.

           goback.

      ***---
       PARAGRAFO-COPY.
       copy "costo-medio.cpy".
       copy "recupero-anagrafica.cpy".
