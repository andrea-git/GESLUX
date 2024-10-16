       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      mail-giacenze2-old.
       AUTHOR.                          Andrea.
       REMARKS. Programma batch da lanciare la notte senza linkage che
                genera un csv contenente i progmag LBX con giacenza < 0.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tsetinvio.sl".
           copy "ordfor-old.sl".
           copy "lineseq.sl".
           copy "progmag.sl".
           copy "articoli.sl".
           copy "timballi.sl".
           copy "timbalqta.sl".
           copy "tmagaz.sl".
           copy "tmp-progmag.sl".
           copy "tmarche.sl".
           copy "timposte.sl".
           copy "tparamge.sl".
           copy "lineseq-mail.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.         
           copy "tsetinvio.fd".
           copy "ordfor-old.fd".
           copy "lineseq.fd".
           copy "progmag.fd".
           copy "articoli.fd".
           copy "timballi.fd".
           copy "timbalqta.fd".
           copy "tmagaz.fd".
           copy "tmp-progmag.fd".
           copy "tmarche.fd".
           copy "timposte.fd".
           copy "tparamge.fd".
           copy "lineseq-mail.fd".

       WORKING-STORAGE SECTION.
           copy "mail.def".
           copy "costo-medio.def".
           copy "imposte.def".
      * COSTANTI
       78  78-MTN                     value "MTN".
       78  78-LBX                     value "LBX".
       78  78-SLI                     value "SLI".
       78  78-GIC                     value "GIC".

      * FILE STATUS
       77  status-ordfor-old              pic xx.
       77  status-tsetinvio           pic xx.
       77  status-lineseq             pic xx.
       77  status-progmag             pic xx.
       77  status-articoli            pic xx.
       77  status-timballi            pic xx.
       77  status-timbalqta           pic xx.
       77  status-tmagaz              pic xx.
       77  status-tmp-progmag         pic xx.
       77  status-tmarche             pic xx.
       77  status-timposte            pic xx.
       77  status-tparamge            pic xx.
       77  status-lineseq-mail        pic xx.
       77  path-lineseq-mail          pic x(256).

       77  wstampa                    pic x(256).
       77  path-tmp-progmag           pic x(256).

      * VARIABILI
       01  r-inizio              pic x(25).

       77  como-riga                  pic x(80).

       77  path-backup                pic x(256).
       77  path-ini                   pic x(256).
       77  path-csv                   pic x(256).
       77  path-log-source            pic x(256).
       77  path-log-dest              pic x(256).

       77  copy-status                signed-short.

       77  giacenza                   pic s9(8).
       77  como-data                  pic 9(8).
       77  como-ora                   pic 9(8).
       77  qta-edit                   pic z.zzz.
       77  giacenza-ed                pic --.---.--9.
       77  imballi-ed                 pic --.---.--9,9.
       77  peso-ed                    pic zz9,999.
       77  start-data                 pic 9(8).
       77  end-data                   pic 9(8).
       77  media-anno                 pic S9(9).
       77  mese-start                 pic 99.
       77  mese-end                   pic 99.
       77  anno-corr                  pic 9(4).
       77  anno-past                  pic 9(4).
       77  mese                       pic 99.
       77  idx                        pic 9(3).
       01  occurs-qta.
         03 el-qta                    pic s9(9) occurs 12.
       77  wk-campo                   pic s9(12)v99.
       77  mesi-utili                 pic 99.

       77  GiacenzaLBX                pic s9(8).
       77  GiacenzaSLI                pic s9(8).
       77  PalletSLI                  pic s9(8).
       77  GiacenzaMTN                pic s9(8).
       77  PalletMTN                  pic s9(8).
       77  GiacenzaGIC                pic s9(8).
       77  PalletGIC                  pic s9(8).
       77  impegnato                  pic s9(8).
       77  giac-utile                 pic s9(8).
       77  comodo                     pic s9(8)v99.
       77  resto                      pic 9(3).
       77  tot-giac-altri             pic s9(8).

       77  GiacenzaLBX-ed             pic z.zzz.zz9.
       77  GiacenzaSLI-ed             pic z.zzz.zz9.
       77  PalletSLI-ed               pic z.zzz.zz9.
       77  GiacenzaMTN-ed             pic z.zzz.zz9.
       77  PalletMTN-ed               pic z.zzz.zz9.
       77  GiacenzaGIC-ed             pic z.zzz.zz9.
       77  PalletGIC-ed               pic z.zzz.zz9.
       77  giac-utile-ed              pic z.zzz.zz9.
       77  mancante-ed                pic z.zzz.zz9.

       77  MagazzinoPrincipale        pic x(3).

      * FLAGS
       01  controllo                  pic xx.
         88 errori                    value "ER".
         88 tutto-ok                  value "OK".

       01  filler                     pic 9.
         88 RecLocked                 value 1, false 0.

       01  filler                     pic 9.
         88 trovato                   value 1, false 0.

       01  filler                     pic 9.
         88 trovato-prese             value 1, false 0.

       01  filler                     pic 9.
         88 CreatoFile                value 1, false 0.

       01  filler                     pic 9.
         88 prima-volta               value 1, false 0.
                                                       
       01  filler                     pic 9.
         88 ExitPerform               value 1, false 0.
                                                       
       01  filler                     pic 9.
         88 InVariMagazzini           value 1, false 0.

      ******************************************************************
       PROCEDURE DIVISION.

       DECLARATIVES.
       copy "mail-decl.cpy". 

      ***---
       TMP-PROGMAG-ERR SECTION.
           use after error procedure on tmp-progmag.
           set tutto-ok  to true.
           evaluate status-tmp-progmag
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                   delimited size
                       "[TMP-PROGMAG] not found!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                       delimited size
                       "[TMP-PROGMAG] Mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "[TMP-PROGMAG] Indexed corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       LINESEQ-ERR SECTION.
           use after error procedure on lineseq.
           set tutto-ok  to true.
           evaluate status-lineseq
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                    delimited size
                       "File [LINESEQ] not found!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                        delimited size
                       "File [LINESEQ] Mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                          delimited size
                       "[LINESEQ] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set tutto-ok  to true.
           evaluate status-progmag
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [PROGMAG] not found!"  delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                        delimited size
                       "File [PROGMAG] Mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                          delimited size
                       "[PROGMAG] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.
 
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [ARTICOLI] not found!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "File [ARTICOLI] Mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                           delimited size
                       "[ARTICOLI] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.
 
      ***---
       TIMBALLI-ERR SECTION.
           use after error procedure on timballi.
           set tutto-ok  to true.
           evaluate status-timballi
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [TIMBALLI] not found!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "File [TIMBALLI] Mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                           delimited size
                       "[TIMBALLI] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       TIMBALQTA-ERR SECTION.
           use after error procedure on timbalqta.
           set tutto-ok  to true.
           evaluate status-timbalqta
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                      delimited size
                       "File [TIMBALQTA] not found!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                          delimited size
                       "File [TIMBALQTA] Mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                           delimited size
                       "[TIMBALQTA] Indexed file corrupt!"delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.
 
      ***---
       TMAGAZ-ERR SECTION.
           use after error procedure on tmagaz.
           set tutto-ok  to true.
           evaluate status-tmagaz
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                   delimited size
                       "File [TMAGAZ] not found!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                       delimited size
                       "File [TMAGAZ] Mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                         delimited size
                       "[TMAGAZ] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                    delimited size
                       "File [TMARCHE] not found!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                        delimited size
                       "File [TMARCHE] Mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                          delimited size
                       "[TMARCHE] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           end-evaluate.

      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           set tutto-ok  to true.
           evaluate status-timposte
           when "35"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                     delimited size
                       "File [TIMPOSTE] not found!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "39"
                perform SETTA-INIZIO-RIGA
                initialize como-riga
                string r-inizio                         delimited size
                       "File [TIMPOSTE] Mismatch size!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
                set errori to true
           when "98"
                perform SETTA-INIZIO-RIGA
                initialize como-riga                      
                string r-inizio                           delimited size
                       "[TIMPOSTE] Indexed file corrupt!" delimited size
                       into como-riga
                end-string
                display como-riga upon syserr
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
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio              delimited size
                  "INIZIO ELABORAZIONE" delimited size
                  into como-riga
           end-string.

           set CreatoFile  to false.
           set prima-volta to true.
           set tutto-ok    to true.
           initialize wstampa.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  wstampa   from environment "PATH_ST".
           inspect wstampa   replacing trailing spaces by low-value.
           string  wstampa          delimited low-value
                   "giacenze_"      delimited size
                   como-data        delimited size
                   ".csv"           delimited size
                   into wstampa
           end-string.

           initialize path-tmp-progmag.
           accept  path-tmp-progmag from environment "PATH_ST".
           inspect path-tmp-progmag replacing trailing
                                    spaces by low-value.
           string  path-tmp-progmag delimited low-value
                   "tmp-progmag_"   delimited size
                   como-data        delimited size
                   "_"              delimited size
                   como-ora         delimited size
                   ".tmp"           delimited size
                   into path-tmp-progmag
           end-string.

      ***---
       OPEN-FILES.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio         delimited size
                  "Apertura FILES" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           open output lineseq.
           if tutto-ok
              open output tmp-progmag
              if tutto-ok
                 close tmp-progmag
                 open i-o tmp-progmag
                 open input progmag articoli timballi ordfor-old
                            timbalqta tmagaz tmarche  timposte
                            tparamge
                 if errori
                    close       lineseq tmp-progmag
                    delete file lineseq tmp-progmag
                 end-if
              else
                 close       lineseq
                 delete file lineseq
              end-if
           end-if.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           if tutto-ok
              string r-inizio                  delimited size
                     "Apertura FILES RIUSCITA" delimited size
                     into como-riga
              end-string
           else
              string r-inizio                      delimited size
                     "Apertura FILES NON RIUSCITA" delimited size
                     into como-riga
              end-string
           end-if.
           display como-riga upon syserr.

      ***---
       ELABORAZIONE.
           |Cerco il magazzino principale
           move low-value to mag-rec.
           start tmagaz key >= mag-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read tmagaz next at end exit perform end-read
              if si-mag-principale
                 move mag-codice to MagazzinoPrincipale
                 exit perform
              end-if
           end-perform.

           |Salvo la data di consolidamento
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           compute start-data =
                   function INTEGER-OF-DATE(tge-data-consolid-progmag).
           subtract 364 from start-data.
           move tge-data-consolid-progmag to end-data.
           compute start-data =
                   function DATE-OF-INTEGER(start-data).

           move start-data(5:2) to mese-start.
           move end-data(5:2)   to mese-end.
           move end-data(1:4)   to anno-corr.
           move start-data(1:4) to anno-past.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                             delimited size
                  "INIZIO ELABORAZIONE MERCE MANCANTE" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           move low-value to mag-rec.
           start tmagaz key >= mag-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmagaz next at end exit perform end-read
                    if si-mag-principale
                       exit perform
                    end-if
                 end-perform
           end-start.

           move 0 to art-codice.
           move 0 to giacenza.

           move low-value  to prg-rec.
           start progmag key >= prg-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read progmag next
                      at end
                      if giacenza < 0 and not InVariMagazzini
                         set  trovato         to true
                         move giacenza        to tmp-prg-giacenza
                         move art-descrizione to tmp-prg-art-des

                         perform IMBALLO
                         |Viene memorizzato in negativo, ma con
                         |la start da high-value per <= � ok
                         compute tmp-prg-costo-medio rounded =
                                 costo-mp-2dec * giacenza
                         write tmp-prg-rec
                      end-if
                      exit perform
                 end-read

                 if prg-cod-articolo  not = art-codice

                    if giacenza < 0 and not InVariMagazzini
                       set trovato to true
                       move giacenza        to tmp-prg-giacenza
                       move art-descrizione to tmp-prg-art-des
                       perform IMBALLO
                       |Viene memorizzato in negativo, ma con
                       |la start da high-value per <= � ok
                       compute tmp-prg-costo-medio rounded =
                               costo-mp-2dec * giacenza

                       write tmp-prg-rec
                    end-if
                    initialize tmp-prg-rec 
                               replacing numeric data by zeroes
                                    alphanumeric data by spaces

                    |Calcolo il costo medio sul padre
                    move 0 to  risultato-imposte
                               imposta-cou
                               imposta-cobat
                               imposta-consumo

                    set TrattamentoGDO to false
                    perform CALCOLA-COSTO-MP
                    perform CALCOLA-COSTO-MP-WHEN-ZERO
                    add 0,005 to costo-mp giving costo-mp-2dec

                    set InVariMagazzini to false
                    move prg-chiave to tmp-prg-chiave
                    move 0 to giacenza
                    move prg-cod-articolo to art-codice
                    read articoli no lock invalid continue end-read
                 else
                    if not InVariMagazzini
                       if art-scorta not = 2 and
                          art-scorta not = 4 and
                          art-scorta not = 6 and
                          art-scorta not = 0
                          move prg-tipo-imballo to imq-codice
                          if prg-cod-magazzino = mag-codice
                             add prg-giacenza to giacenza
                             subtract prg-impegnato from giacenza
                          else
                             if prg-giacenza > 0
                                set InVariMagazzini to true
                             end-if
                          end-if
                       end-if
                    end-if
                 end-if
              end-perform
           end-if.

           if trovato

              write line-riga of lineseq from spaces

              initialize line-riga of lineseq
              string ";"                             delimited size
                     "** AVVISO MERCE MANCANTE AL: " delimited size
                     como-data(7:2)                  delimited size
                     "/"                             delimited size
                     como-data(5:2)                  delimited size
                     "/"                             delimited size
                     como-data(1:4)                  delimited size
                     " **"                           delimited size
                     into line-riga of lineseq
              end-string
              write line-riga of lineseq
              initialize line-riga of lineseq
              string ";"                         delimited size
                     "FILTRI SU SCORTA: 1/3/5/7" delimited size
                     into line-riga of lineseq
              end-string
              write line-riga of lineseq
              write line-riga of lineseq from spaces
              initialize line-riga of lineseq
              string "Codice"        delimited size
                     ";"             delimited size
                     "Descrizione"   delimited size
                     ";"             delimited size
                     "Imballo"       delimited size
                     ";"             delimited size
                     "N. Imballi"    delimited size
                     ";"             delimited size
                     "Giacenza"      delimited size
                     ";"             delimited size
                     "Arrivi: Data"  delimited size
                     ";"             delimited size
                     "Arrivi: Pezzi" delimited size
                     into line-riga of lineseq
              end-string
              write line-riga of lineseq
              move high-value to tmp-prg-rec
              start tmp-progmag key <= key-costo
                    invalid continue
              end-start

              perform until 1 = 2
                 read tmp-progmag previous at end exit perform end-read
                 move tmp-prg-giacenza to giacenza-ed
                 move tmp-prg-imballi  to imballi-ed
                 move tmp-prg-peso     to peso-ed
                 initialize line-riga  of lineseq
                 string tmp-prg-cod-articolo  delimited size
                        ";"                   delimited size
                        tmp-prg-art-des(1:30) delimited size
                        ";"                   delimited size
                        tmp-prg-imb-des(1:20) delimited size
                        ";"                   delimited size
                        imballi-ed            delimited size
                        ";"                   delimited size
                        giacenza-ed           delimited size
                        into line-riga of lineseq
                 end-string
                 write line-riga of lineseq
              end-perform
              set CreatoFile to true

              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                             delimited size
                     "AVVISO PER MERCE MANCANTE GENERATO" delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

           else

              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                           delimited size
                     "NESSUN AVVISO PER MERCE MANCANTE" delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

           end-if.

           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                           delimited size
                  "INIZIO ELABORAZIONE RIFORNIMENTI" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           set tutto-ok      to true.
           set trovato       to false.
           set trovato-prese to false.
           set prima-volta   to true.
           set ExitPerform   to false.
           move low-value to prg-rec.
           start progmag key >= prg-chiave
                 invalid set errori to true
           end-start.

           move 0 to GiacenzaLBX.
           move 0 to GiacenzaMTN.
           move 0 to GiacenzaGIC.
           move 0 to GiacenzaSLI.
           move 0 to impegnato.
           move 0 to giac-utile.
           move 0 to tot-giac-altri.

           if tutto-ok
              perform until 1 = 2
                 read progmag next
                      at end
                      move spaces to prg-cod-magazzino
                      set ExitPerform to true
                 end-read
                 if prg-cod-magazzino = spaces |Sono sul padre
                    if impegnato > GiacenzaLBX
                       if GiacenzaSLI > 0 or
                          GiacenzaMTN > 0 or
                          GiacenzaGIC > 0
                          perform SOMMA-MEDIA-ANNO
                          compute giac-utile = impegnato   -
                                               GiacenzaLBX +
                                               media-anno
                          evaluate true
                          when GiacenzaSLI < 0
                               compute tot-giac-altri =
                                       GiacenzaGIC + GiacenzaMTN
                          when GiacenzaMTN < 0
                               compute tot-giac-altri =
                                       GiacenzaSLI + GiacenzaGIC
                          when GiacenzaGIC < 0
                               compute tot-giac-altri =
                                       GiacenzaSLI + GiacenzaMTN
                          when other
                               compute tot-giac-altri =
                                       GiacenzaSLI +
                                       GiacenzaMTN +
                                       GiacenzaGIC
                          end-evaluate
                          perform VALUTA-TOTALI
                       end-if
                    end-if
                    if ExitPerform
                       exit perform
                    end-if
                    move 0 to GiacenzaLBX
                    move 0 to GiacenzaMTN
                    move 0 to GiacenzaSLI
                    move 0 to GiacenzaGIC
                    move 0 to impegnato
                    perform CALCOLA-COSTO-MP
                    perform CALCOLA-COSTO-MP-WHEN-ZERO
                    add 0,005 to costo-mp giving costo-mp-2dec
                 else
                    move prg-cod-articolo to art-codice
                    evaluate prg-cod-magazzino
                    when 78-LBX add prg-giacenza  to GiacenzaLBX
                                add prg-impegnato to impegnato
                    when 78-SLI add prg-giacenza  to GiacenzaSLI
                    when 78-MTN add prg-giacenza  to GiacenzaMTN
                    when 78-GIC add prg-giacenza  to GiacenzaGIC
                    end-evaluate
                 end-if
              end-perform
           end-if.

           if trovato-prese
              write line-riga of lineseq from spaces after 4
              set prima-volta to false
              initialize line-riga of lineseq
              string ";"                                delimited size
                     "** RIFORNIMENTI DA DEPOSITI AL: " delimited size
                     como-data(7:2)                     delimited size
                     "/"                                delimited size
                     como-data(5:2)                     delimited size
                     "/"                                delimited size
                     como-data(1:4)                     delimited size
                     " **"                              delimited size
                     into line-riga of lineseq
              end-string
              write line-riga of lineseq

              set CreatoFile  to true

              write      line-riga of lineseq from spaces
              initialize line-riga of lineseq
              string "Codice"            delimited size
                     ";"                 delimited size
                     "Descrizione"       delimited size
                     ";"                 delimited size
                     "Q.ta rifornimento" delimited size
                     ";"                 delimited size
                     "da MTN"            delimited size
                     ";"                 delimited size
                     "(in pallet)"       delimited size
                     ";"                 delimited size
                     "da SLI"            delimited size
                     ";"                 delimited size
                     "(in pallet)"       delimited size
                     ";"                 delimited size
                     "da GIC"            delimited size
                     ";"                 delimited size
                     "(in pallet)"       delimited size
                     ";"                 delimited size
                     "Mancante"          delimited size
                     ";"                 delimited size
                     "Arrivi: Data"      delimited size
                     ";"                 delimited size
                     "Arrivi: Pezzi"     delimited size
                     into line-riga of lineseq
              end-string
              write line-riga of lineseq

              close      tmp-progmag
              open input tmp-progmag
              move high-value to tmp-prg-rec
              start tmp-progmag key <= key-costo
                    invalid continue
              end-start

              perform until 1 = 2
                 read tmp-progmag previous at end exit perform end-read

                 move tmp-prg-cod-articolo to art-codice
                 move tmp-prg-giacenza     to giac-utile-ed
      *           move tmp-prg-giac-MTN     to GiacenzaMTN-ed
      *           move tmp-prg-giac-SLI     to GiacenzaSLI-ed
      *           move tmp-prg-giac-GIC     to GiacenzaGIC-ed

                 read articoli no lock invalid continue end-read
                 perform CALCOLA-PALLET

                 move PalletMTN            to PalletMTN-ed
                 move PalletSLI            to PalletSLI-ed
                 move PalletGIC            to PalletGIC-ed

      *           compute comodo = tmp-prg-giacenza  -
      *                            tmp-prg-giac-MTN  -
      *                            tmp-prg-giac-SLI  -
      *                            tmp-prg-giac-GIC

                 if comodo > 0
                    move comodo to mancante-ed
                 else
                    move 0      to mancante-ed
                 end-if

                 initialize line-riga of lineseq
                 string art-codice            delimited size
                        ";"                   delimited size
                        art-descrizione(1:30) delimited size
                        ";"                   delimited size
                        giac-utile-ed         delimited size
                        ";"                   delimited size
                        GiacenzaMTN-ed        delimited size
                        ";"                   delimited size
                        PalletMTN-ed          delimited size
                        ";"                   delimited size
                        GiacenzaSLI-ed        delimited size
                        ";"                   delimited size
                        PalletSLI-ed          delimited size
                        ";"                   delimited size
                        GiacenzaGIC-ed        delimited size
                        ";"                   delimited size
                        PalletGIC-ed          delimited size
                        ";"                   delimited size
                        mancante-ed           delimited size
                        into line-riga of lineseq
                 end-string
                 write line-riga of lineseq
              end-perform

              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                           delimited size
                     "AVVISO PER RIFORNIMENTI GENERATO" delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

           else

              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                         delimited size
                     "NESSUN AVVISO PER RIFORNIMENTI" delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

           end-if.

      ***---
       SOMMA-MEDIA-ANNO.
           move 0 to media-anno mesi-utili.
           move MagazzinoPrincipale to ord-mag-old.
           move art-codice          to ord-articolo-old.
           read ordfor-old no lock 
                invalid continue
            not invalid
                move 1 to idx
                move 0 to wk-campo
                initialize occurs-qta
                move mese-start to mese
                perform 12 times
                   if mese > 12
                      move 1 to mese
                   end-if

                   if mese-end = 12
                      move ord-qta-past-m-old(mese) to el-qta(mese)
                   else
                      |Superando il mese finale significa
                      |che siamo nell'anno precedente
                      if mese > mese-end
                         move ord-qta-past-m-old(mese) to el-qta(mese)
                      else
                         move ord-qta-corr-m-old(mese) to el-qta(mese)
                      end-if
                   end-if

                   add el-qta(mese) to wk-campo
                   add 1 to mese idx

                   |SBLOCCARE QUI SE VIENE RICHIESTA LA MEDIA SOLO
                   |SUI MESI IN CUI SONO STATI FATTI RITIRI
      *****             if el-qta(mese) not = 0
      *****                add 1 to mesi-utili
      *****             end-if
                end-perform
                move 12 to mesi-utili
                move 0 to resto
                divide wk-campo by mesi-utili 
                            giving media-anno
                         remainder resto
                if resto not = 0
                   add 1 to media-anno
                end-if
           end-read.

      ***---
       CALCOLA-PALLET.
           move 0 to comodo.
           move 0 to PalletMTN.
           move 0 to PalletSLI.
           move 0 to PalletGIC.
           if art-qta-epal not = 0
              move art-qta-epal to comodo
           end-if.
           if art-qta-std  not = 0
              move art-qta-std to comodo
           end-if.

           |Nel caso non sia valorizzato evito 
           |di mandare in errore il run-time
      *     if comodo not = 0
      *        if tmp-prg-giac-MTN not = 0
      *           move 0 to resto
      *           divide tmp-prg-giac-MTN by comodo giving PalletMTN
      *                                          remainder resto
      *           if resto not = 0 
      *              add 1 to PalletMTN 
      *           end-if
      *        end-if
      *        if tmp-prg-giac-SLI not = 0
      *           move 0 to resto
      *           divide tmp-prg-giac-SLI by comodo giving PalletSLI
      *                                          remainder resto
      *           if resto not = 0 
      *              add 1 to PalletSLI
      *           end-if
      *        end-if
      *        if tmp-prg-giac-GIC not = 0
      *           move 0 to resto
      *           divide tmp-prg-giac-GIC by comodo giving PalletGIC
      *                                          remainder resto
      *           if resto not = 0 
      *              add 1 to PalletGIC
      *           end-if
      *        end-if
      *     end-if.

      ***---
       VALUTA-TOTALI.
           |In questo prf valuto solamente aunta merce devo PRENDERE 
           |dai vari magazzini per coprire la quantit� utile.

           |Insieme non coprono il dislivello per cui li prendo TUTTI
           if giac-utile > tot-giac-altri
              |Se per� un magazzino ha giacenza < 0
              |non prendo nulla da quel magazzino
              if GiacenzaMTN > 0
                 set  trovato     to true
              else
                 move 0 to GiacenzaMTN
              end-if
              if GiacenzaSLI > 0
                 set  trovato     to true
              else
                 move 0 to GiacenzaSLI
              end-if
              if GiacenzaGIC > 0
                 set  trovato     to true
              else
                 move 0 to GiacenzaGIC
              end-if
           else
              set  trovato to true
              |Se la giacenza di un solo magazzino mi
              |copre l'utile prendo TUTTO da quel 
              |magazzino e niente dagli altri
              if GiacenzaGIC > giac-utile
                 if GiacenzaGIC > 0
                    move giac-utile to GiacenzaGIC
                    move 0          to GiacenzaMTN
                    move 0          to GiacenzaSLI
                 end-if
              else
                 if GiacenzaMTN > giac-utile
                    if GiacenzaMTN > 0
                       move giac-utile to GiacenzaMTN
                       move 0          to GiacenzaSLI
                       move 0          to GiacenzaGIC
                    end-if
                 else
                    if GiacenzaSLI > giac-utile
                       if GiacenzaSLI > 0
                          move giac-utile to GiacenzaSLI
                          move 0          to GiacenzaMTN
                          move 0          to GiacenzaGIC
                       end-if
                    else
                       perform DA-QUALE-MAGAZZINO
                    end-if
                 end-if
              end-if
           end-if.

           if trovato
              if prima-volta
                 set prima-volta to false
                 close tmp-progmag
                 open output tmp-progmag
                 set trovato-prese to true
              end-if
              read articoli no lock invalid continue end-read
              initialize tmp-prg-rec replacing numeric data by zeroes
                                          alphanumeric data by spaces

              move art-codice      to tmp-prg-cod-articolo
              move art-descrizione to tmp-prg-art-des
              move giac-utile      to tmp-prg-giacenza
      *        move GiacenzaMTN     to tmp-prg-giac-MTN
      *        move GiacenzaSLI     to tmp-prg-giac-SLI
      *        move GiacenzaGIC     to tmp-prg-giac-GIC
              compute tmp-prg-costo-medio =
                      costo-mp-2dec * giac-utile
              write tmp-prg-rec invalid continue end-write
           end-if.

      ***---
       DA-QUALE-MAGAZZINO.
           |Da qui in poi la giacenza � coperta da pi� magazzini
           |Mi � stata data una scala d'importanza (MTN, SLI, GIC)
           compute comodo = giac-utile - GiacenzaMTN.
           |La parte mancante la prendo da SLI (se copre)...
           if GiacenzaSLI > comodo
              move comodo to GiacenzaSLI
              move 0      to GiacenzaGIC
           else
              |...altrimenti la sottraggo dal totale
              |e il restante la prendo da GIC
              compute comodo = comodo - GiacenzaSLI
              move comodo to GiacenzaGIC
           end-if.

      ***---
       IMBALLO.
           read timbalqta no lock 
                invalid continue end-read.
           move imq-tipo  to imb-codice.
           read timballi  no lock
                invalid move spaces to imb-descrizione
           end-read.

           move 0 to tmp-prg-imballi.
           compute tmp-prg-imballi =
                 ( tmp-prg-giacenza / imq-qta-imb ).

           inspect imb-descrizione replacing trailing 
                                   spaces by low-value.
           move imq-qta-imb    to qta-edit.
           call "C$JUSTIFY" using qta-edit, "L".
           inspect qta-edit replacing trailing spaces by low-value.
           string imb-descrizione delimited by low-value
                  " da "          delimited by size
                  qta-edit        delimited by low-value
                  " x "           delimited by size
                  art-udm-imballo delimited by size
                  into tmp-prg-imb-des
           end-string.

      ***---
       CLOSE-FILES.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio         delimited size
                  "CHIUSURA FILES" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           close lineseq articoli progmag 
                 timballi timbalqta tmagaz
                 tmp-progmag tmarche timposte
                 ordfor-old tparamge.

           if not CreatoFile
              delete file lineseq

              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio               delimited size
                     "NESSUNA MAIL INVIATA" delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

           else

              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                 delimited size
                     "INVIO MAIL IN CORSO..." delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

              move "Invio automatico GESLUX prese giornaliere" 
                to LinkSubject
              accept LinkAddress from environment "NIGHT_ADDRESSES"
              move "In allegato dettaglio giacenze."    to LinkBody
              move wstampa                              to LinkAttach

              move 5 to tentativi-mail
              move "mail-giacenze2-old" to NomeProgramma
              perform CICLO-SEND-MAIL
                  
              perform SETTA-INIZIO-RIGA
              initialize como-riga
              if mail-ok
                 string r-inizio               delimited size
                        "INVIO MAIL RIUSCITO!" delimited size
                        into como-riga
                 end-string
              else
                 string r-inizio                   delimited size
                        "INVIO MAIL NON RIUSCITO!" delimited size
                        into como-riga
                 end-string
              end-if
              display como-riga upon syserr

              |FACCIO LA COPIA DEI FILE (INI, CSV)
              perform COPIA-FILES

              delete file lineseq

              |Cos� cancella anche il csv
              move LinkAttach to wstampa

           end-if.
           delete file tmp-progmag lineseq.

      ***---
       AFTER-SEND-MAIL.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga
           string r-inizio        delimited size
                  "TENTATIVO N. " delimited size
                  tentativo-mail  delimited size
                  ": "            delimited size
                  line-riga-mail  delimited size
                  " - STATUS: "   delimited size
                  StatusInvioMail delimited size
             into como-riga
           end-string
           display como-riga upon syserr.

      ***---
       COPIA-FILES.
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                delimited size
                  "INIZIO COPIA FILES..." delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-backup.
           accept  path-backup from environment "PATH_BACKUP".
           inspect path-backup replacing trailing spaces by low-value.

           |FILE INI
           initialize path-ini.
           string  path-backup delimited low-value
                   "InvioMail" delimited size
                   "_"         delimited size
                   como-data   delimited size
                   "_"         delimited size
                   como-ora    delimited size
                   ".ini"      delimited size
                   into path-ini
           end-string.
           move 0 to copy-status.
           call "C$COPY" using path-lineseq-mail, path-ini, "S"
                        giving copy-status.

           if copy-status not = 0
              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                      delimited size
                     "COPIA FILE INI NON RIUSCITA" delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

              initialize como-riga
              string r-inizio            delimited size
                     "FILE DI PARTENZA:" delimited size
                     wstampa             delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

              initialize como-riga
              string r-inizio                delimited size
                     "FILE DI DESTINAZIONE:" delimited size
                     path-ini                delimited size
                     into como-riga
              end-string
              display como-riga upon syserr
           end-if.

           |FILE CSV
           initialize path-csv.
           string  path-backup delimited low-value
                   "giacenze"  delimited size
                   "_"         delimited size
                   como-data   delimited size
                   "_"         delimited size
                   como-ora    delimited size
                   ".csv"      delimited size
                   into path-csv
           end-string.
           move 0 to copy-status.
           call "C$COPY" using LinkAttach, path-csv, "S"
                        giving copy-status.

           if copy-status not = 0
              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                      delimited size
                     "COPIA FILE CSV NON RIUSCITA" delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

              initialize como-riga
              string r-inizio            delimited size
                     "FILE DI PARTENZA:" delimited size
                     LinkAttach          delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

              initialize como-riga
              string r-inizio                delimited size
                     "FILE DI DESTINAZIONE:" delimited size
                     path-csv                delimited size
                     into como-riga
              end-string
              display como-riga upon syserr
           end-if.

           |FILE LOG
           perform SETTA-INIZIO-RIGA.
           initialize como-riga.
           string r-inizio                 delimited size
                  "ELABORAZIONE TERMINATA" delimited size
                  into como-riga
           end-string.
           display como-riga upon syserr.

           initialize path-log-source.
           accept path-log-source from environment "PATH_LOG_SOURCE".

           initialize path-log-dest.
           string  path-backup delimited low-value
                   "InvioMail" delimited size
                   "_"         delimited size
                   como-data   delimited size
                   "_"         delimited size
                   como-ora    delimited size
                   ".log"      delimited size
                   into path-log-dest
           end-string.

           move 0 to copy-status.
           call "C$COPY" using path-log-source, path-log-dest, "S"
                        giving copy-status.

           if copy-status not = 0
              perform SETTA-INIZIO-RIGA
              initialize como-riga
              string r-inizio                      delimited size
                     "COPIA FILE LOG NON RIUSCITA" delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

              initialize como-riga
              string r-inizio            delimited size
                     "FILE DI PARTENZA:" delimited size
                     path-log-source     delimited size
                     into como-riga
              end-string
              display como-riga upon syserr

              initialize como-riga
              string r-inizio                delimited size
                     "FILE DI DESTINAZIONE:" delimited size
                     path-log-dest           delimited size
                     into como-riga
              end-string
              display como-riga upon syserr
           end-if.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "mail.cpy".
           copy "costo-medio.cpy".
           copy "calcola-costo-mp-when-zero.cpy".
           copy "setta-inizio-riga.cpy".
