       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      crea-ordfor.
       AUTHOR.                          Andrea.
       REMARKS. Partendo dal file coperfab (copertura fabbisogno)
                creo ordini a fornitori raggruppando per fornitore.

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "coperfab.sl".
           copy "coperfab-mag.sl".
           copy "tordforn.sl".
           copy "rordforn.sl".
           copy "nlistini.sl".
           copy "nordforn.sl".
           copy "tcontat.sl".
           copy "tparamge.sl".
           copy "ordfor2.sl".
           copy "progmag.sl".
           copy "articoli.sl".
           copy "tcaumag.sl".
           copy "timbalqta.sl".
           copy "timballi.sl".
           copy "tscorte.sl".
           copy "clienti.sl".
           copy "destinif.sl".
           copy "rlistini.sl".
           copy "tlistini.sl".
           copy "impforn.sl".
           copy "timposte.sl".
           copy "tpiombo.sl".
           copy "distinteb.sl".
           copy "tmp-tof-auto.sl".
           copy "tmp-rof-auto.sl".
           copy "param.sl".
           copy "tnumordf.sl".
           copy "genlog.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.             
           copy "coperfab.fd".
           copy "coperfab-mag.fd".
           copy "tordforn.fd".
           copy "rordforn.fd".
           copy "nlistini.fd".
           copy "nordforn.fd".
           copy "tcontat.fd".
           copy "tparamge.fd".
           copy "ordfor2.fd".
           copy "progmag.fd".
           copy "articoli.fd".
           copy "tcaumag.fd".
           copy "timbalqta.fd".
           copy "timballi.fd".
           copy "tscorte.fd".
           copy "clienti.fd".
           copy "destinif.fd".
           copy "rlistini.fd".
           copy "tlistini.fd".
           copy "impforn.fd".
           copy "timposte.fd".
           copy "tpiombo.fd".
           copy "distinteb.fd".
           copy "tmp-tof-auto.fd".
           copy "tmp-rof-auto.fd".
           copy "param.fd".   
           copy "tnumordf.fd".
           copy "genlog.fd".

       WORKING-STORAGE SECTION.
      * COPY
       copy "link-geslock.def".
       copy "imposte-fornitore.def".
       copy "prz-finito-forn.def".
       copy "utydata.def".
       copy "costo-medio.def".
       copy "trova-parametro.def".

       77  status-coperfab       pic xx.
       77  status-coperfab-mag   pic xx.
       77  status-tordforn       pic xx.
       77  status-rordforn       pic xx.
       77  status-nlistini       pic xx.
       77  status-nordforn       pic xx.
       77  status-tcontat        pic xx.
       77  status-tparamge       pic xx.
       77  status-ordfor2        pic xx.
       77  status-progmag        pic xx.
       77  status-articoli       pic xx.
       77  status-tcaumag        pic xx.
       77  status-timbalqta      pic xx.
       77  status-timballi       pic xx.
       77  status-tscorte        pic xx.
       77  status-clienti        pic xx.
       77  status-destinif       pic xx.
       77  status-rlistini       pic xx.
       77  status-tlistini       pic xx.
       77  status-impforn        pic xx.
       77  status-timposte       pic xx.
       77  status-tpiombo        pic xx.
       77  status-distinteb      pic xx.
       77  status-tmp-tof-auto   pic xx.
       77  status-tmp-rof-auto   pic xx.
       77  status-param          pic xx.
       77  status-tnumordf       pic xx.
       77  status-genlog         pic xx.

       77  path-tmp-tof-auto     pic x(256).
       77  path-tmp-rof-auto     pic x(256).
       77  path-genlog           pic x(256) value spaces.

       77  path-coperfab-mag     pic x(256).
                                          
       77  tot-qta-mese2         pic 9(8).
       77  tot-qta-mese3         pic 9(8).
       77  tot-qta-mese4         pic 9(8).
       77  tot-qta-mese5         pic 9(8).
       77  tot-qta-mese6         pic 9(8).

       77  r-inizio              pic x(25).
       77  como-riga             pic x(800).

      * COSTANTI
       78  titolo                value "Creazione Ordini a Fornitori".

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 EseguiCheck        value 1 false 0.

      * VARIABILI          
       01  s-cpfm-qta.
         03 s-cpfm-qta-m       pic  9(8) occurs 6 times.
                                                     
       01 FILLER.
           05 articolo-fisso     pic 9(6).
           05 mese1-fisso        pic 9(5).
           05 mese2-fisso        pic 9(5).
           05 mese3-fisso        pic 9(5).
           05 mese4-fisso        pic 9(5).
           05 mese5-fisso        pic 9(5).
           05 mese6-fisso        pic 9(5).
           05 qta-epal-fisso     pic 9(5).
           05 no-qta-fisso       pic x.
       77  pordini-fisso         pic x(100).

       77  qta-ordinata          pic 9(12).
       77  qta-eccesso           pic 9(12).
       77  qta-da-ordinare       pic 9(12).
       77  tot-qta-m             pic 9(12).
       77  como-mese             pic 99.
       77  primo-numero          pic 9(8) value 0.
       77  mese-oggi             pic 99.
       77  anno-oggi             pic 9(4).
       77  mese-consegna         pic 99.
       77  counter               pic 9(10).
       77  counter2              pic 9(10).
       77  counter-edit          pic z(10).
       77  idx                   pic 9(5).
       77  qta                   pic s9(8) occurs 6.
       77  numero-ordf           pic 9(8)  occurs 6.

       77  num-bancali           pic 9(6).
       77  mese-rif              pic 9(6).
       77  ris                   pic 9(8).
       77  ris2                  pic 9(8).
       77  resto                 pic 9(8).
       77  como-qta              pic 9(8).
       77  como-qta-rical        pic s9(8).
       77  como-qta-ordinata     pic 9(8).
       77  data-consegna         pic 9(8).
       77  giorno                pic 9.
       77  como-numero           pic 9(15)v999.
       77  save-fornitore        pic 9(5).
       77  save-destino          pic 9(5).
       77  save-causale          pic x(4).
       77  save-articolo         pic 9(6).
       77  tot-qta               pic 9(5). 
       77  tot-qta-moq           pic 9(15).
       77  diff-moq              pic 9(15).
       77  como-qta-moq          pic 9(15).
       77  ultimo-mese-moq       pic 9(2).
       77  idx-mese              pic 99.
       77  SaveRiga              pic 9(10).
       77  codice-ed             pic z(6).
       77  giacenza              pic s9(8).
       77  como-giacenza         pic 9(8). |VALORE ASSOLUTO
       77  impegnato             pic s9(8).
       77  qta-utile-LBX         pic s9(8).
       77  save-riga             pic 9(5).
       77  primo-mese            pic 99.

       01  sav-chiave.
           05 sav-cod-articolo   pic 9(6).
           05 sav-cod-magazzino  pic x(3).
           05 sav-tipo-imballo   pic x(3).
           05 sav-peso           pic 9(5)v9(3).

       01  s-prg-chiave.
           05 s-cod-articolo     pic 9(6).
           05 s-cod-magazzino    pic x(3).
           05 s-tipo-imballo     pic x(3).
           05 s-peso             pic 9(5)v9(3).

      * contenente TUTTI i fornitori con qta ossia validi per la generazione
       01  tab-fornitori         occurs 999 indexed by idx-forn. 
           05 el-forn            pic 9(5).
           05 el-dest            pic 9(5).
                                               
       77  calcolo-piombo        pic x.
         88 nuovo-calcolo-piombo value "N".

       LINKAGE SECTION.
       77  link-handle           handle of window.
       77  LinkFirst             pic 9(8).
       77  LinkLast              pic 9(8).
       77  LinkUser              pic x(20).
       77  LinkAuto              pic 9.

      ******************************************************************
       PROCEDURE DIVISION USING link-handle 
                                LinkFirst 
                                LinkLast 
                                LinkUser 
                                LinkAuto.

       DECLARATIVES.
      ***---
       COPERFAB-ERR SECTION.
           use after error procedure on coperfab.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-coperfab
           when "39"
                set errori to true
                display message "File [COPERFAB] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[COPERFAB] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [COPERFAB] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       COPERFAB-MAG-ERR SECTION.
           use after error procedure on coperfab-mag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-coperfab-mag
           when "39"
                set errori to true
                display message "File [COPERFAB-MAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[COPERFAB-MAG] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [COPERFAB-MAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TORDFORN-ERR SECTION.
           use after error procedure on tordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tordforn
           when "39"
                set errori to true
                display message "File [TORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       RORDFORN-ERR SECTION.
           use after error procedure on rordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rordforn
           when "39"
                set errori to true
                display message "File [RORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       NORDFORN-ERR SECTION.
           use after error procedure on nordforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-nordforn
           when "39"
                set errori to true
                display message "File [NORDFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[NORDFORN] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [NORDFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TCONTAT-ERR SECTION.
           use after error procedure on tcontat.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcontat
           when "39"
                set errori to true
                display message "File [TCONTAT] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCONTAT] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TCONTAT] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TIMBALQTA-ERR SECTION.
           use after error procedure on timbalqta.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-timbalqta
           when "39"
                set errori to true
                display message "File [TIMBALQTA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMBALQTA] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TIMBALQTA] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TIMBALLI-ERR SECTION.
           use after error procedure on timballi.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-timballi
           when "39"
                set errori to true
                display message "File [TIMBALLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMBALLI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TIMBALLI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TSCORTE-ERR SECTION.
           use after error procedure on tscorte.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tscorte
           when "39"
                set errori to true
                display message "File [TSCORTE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TSCORTE] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TSCORTE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "39"
                set errori to true
                display message "File [CLIENTI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[CLIENTI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [CLIENTI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       DESTINIF-ERR SECTION.
           use after error procedure on destinif.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-destinif
           when "39"
                set errori to true
                display message "File [DESTINIF] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DESTINIF] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [DESTINIF] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       RLISTINI-ERR SECTION.
           use after error procedure on rlistini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rlistini
           when "39"
                set errori to true
                display message "File [RLISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RLISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [RLISTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TLISTINI-ERR SECTION.
           use after error procedure on tlistini.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tlistini
           when "39"
                set errori to true
                display message "File [TLISTINI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TLISTINI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TLISTINI] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       IMPFORN-ERR SECTION.
           use after error procedure on impforn.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-impforn
           when "39"
                set errori to true
                display message "File [IMPFORN] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[IMPFORN] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [IMPFORN] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TIMPOSTE-ERR SECTION.
           use after error procedure on timposte.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-timposte
           when "39"
                set errori to true
                display message "File [TIMPOSTE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TIMPOSTE] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TIMPOSTE] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       TPIOMBO-ERR SECTION.
           use after error procedure on tpiombo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tpiombo
           when "39"
                set errori to true
                display message "File [TPIOMBO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPIOMBO] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [TPIOMBO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate. 

      ***---
       DISTINTEB-ERR SECTION.
           use after error procedure on distinteb.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-distinteb
           when "39"
                set errori to true
                display message "File [DISTINTEB] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[DISTINTEB] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File [DISTINTEB] inesistente"
                        title = titolo
                        icon 2
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
              if EseguiCheck
                 perform CONTROLLA-ORDINI
              end-if
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.                                           
           accept pordini-fisso from environment "ART_PORDINI".
           if pordini-fisso = spaces
              move 0 to articolo-fisso
           else
              unstring pordini-fisso delimited by ";"
                  into articolo-fisso
                       mese1-fisso
                       mese2-fisso
                       mese3-fisso
                       mese4-fisso
                       mese5-fisso
                       mese6-fisso
                       qta-epal-fisso
                       no-qta-fisso
              end-unstring
           end-if.                               

           set EseguiCheck to false.
           move 0 to idx-forn.
           initialize path-coperfab-mag.
           accept como-data from century-date.
           accept como-ora  from time.
           accept  path-coperfab-mag from environment "PATH_ST".
           inspect path-coperfab-mag replacing trailing 
                                     spaces by low-value.
           string path-coperfab-mag delimited low-value
                  "COPERFAB-MAG_"   delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".tmp"            delimited size
             into path-coperfab-mag
           end-string.

           initialize path-tmp-tof-auto.
           accept como-data from century-date.
           accept como-ora  from time.
           accept  path-tmp-tof-auto from environment "PATH_ST".
           inspect path-tmp-tof-auto replacing trailing 
                                     spaces by low-value.
           string path-tmp-tof-auto delimited low-value
                  "TMP-TOF-AUTO"    delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".tmp"            delimited size
             into path-tmp-tof-auto
           end-string.

           initialize path-tmp-rof-auto.
           accept como-data from century-date.
           accept como-ora  from time.
           accept  path-tmp-rof-auto from environment "PATH_ST".
           inspect path-tmp-rof-auto replacing trailing 
                                     spaces by low-value.
           string path-tmp-rof-auto delimited low-value
                  "TMP-ROF-AUTO"    delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".tmp"            delimited size
             into path-tmp-rof-auto
           end-string.     
           if linkAuto = 1
              accept  path-genlog from environment "SCHEDULER_PATH_LOG"
              inspect path-genlog replacing trailing spaces by low-value
              string  path-genlog        delimited low-value
                      "LOG-CREA-ORDFOR_" delimited size
                      como-data          delimited size
                      "-"                delimited size
                      como-ora           delimited size
                      ".log"             delimited size
                 into path-genlog
              end-string
           end-if.


      ***---
       OPEN-FILES.
           perform OPEN-TCONTAT-LOCK.
           if tutto-ok
              perform OPEN-COPERFAB-LOCK
           end-if.
           if tutto-ok
              open i-o tordforn rordforn nlistini nordforn
              if tutto-ok
                 open input tparamge  tcaumag tscorte clienti
                            timballi  progmag articoli timbalqta 
                            destinif rlistini impforn timposte tpiombo
                            tlistini distinteb param
                 open output coperfab-mag tmp-tof-auto tmp-rof-auto
                 close       coperfab-mag tmp-tof-auto tmp-rof-auto
                 open i-o    coperfab-mag tmp-tof-auto tmp-rof-auto
                             ordfor2 tnumordf
              end-if
           end-if.
           if linkAuto = 1
              open output genlog
           end-if.

      ***---
       OPEN-TCONTAT-LOCK.
           set tutto-ok to true.
           perform until 1 = 2
              set RecLocked to false
              initialize geslock-linkage
              move "tcontat" to geslock-nome-file

              set tutto-ok  to true
              open i-o tcontat allowing readers

              if RecLocked   
                 string "La tabella dei contatori risulta"
                 x"0d0a""in uso su altro terminale."
                 x"0d0a""Generazione impossibile." delimited size
                       into geslock-messaggio
                 end-string
                 move 1 to geslock-v-riprova
                 move 0 to geslock-v-ignora
                 move 1 to geslock-v-termina
                 call   "geslock" using geslock-linkage
                 cancel "geslock"
                 evaluate true
                 when riprova continue
                 when ignora  
                 when termina set errori to true
                              exit perform
                 end-evaluate
              else
                 exit perform
              end-if
           end-perform.

      ***---
       OPEN-COPERFAB-LOCK.
           set tutto-ok to true.
           perform until 1 = 2
              set RecLocked to false
              initialize geslock-linkage
              move "coperfab" to geslock-nome-file

              set tutto-ok  to true
              open i-o coperfab allowing readers

              if RecLocked   
                 string "La tabella [COPERFAB] risulta"
                 x"0d0a""in uso su altro terminale."
                 x"0d0a""Generazione impossibile." delimited size
                       into geslock-messaggio
                 end-string
                 move 1 to geslock-v-riprova
                 move 0 to geslock-v-ignora
                 move 1 to geslock-v-termina
                 call   "geslock" using geslock-linkage
                 cancel "geslock"
                 evaluate true
                 when riprova continue
                 when ignora  
                 when termina set errori to true
                              exit perform
                 end-evaluate
              else
                 exit perform
              end-if
           end-perform.

      ***---
       ELABORAZIONE.
           move "INIZIO ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.
           |Se le qta sono a zero non devo scrivere niente per 
           |quel fornitore perciò conto quanti pezzi totali comprende
           |e nel caso sia > 0 lo metto in un occurs
           move low-value to cpf-rec.
           start coperfab key >= k-forn 
                 invalid set errori to true
           end-start.
           if tutto-ok
              move 0 to save-fornitore save-destino
              perform until 1 = 2
                 read coperfab next 
                      at end 
                      if tot-qta > 0
                         add 1 to idx-forn
                         move save-fornitore to el-forn(idx-forn)
                         move save-destino   to el-dest(idx-forn)
                      end-if
                      exit perform 
                 end-read
                 if articolo-fisso not = 0
                    if cpf-articolo not = articolo-fisso
                       exit perform cycle
                    end-if
                 end-if
                 if cpf-qta <= 0
                    delete coperfab record invalid continue end-delete
                 else
                    if save-fornitore = 0
                       move cpf-fornitore to save-fornitore
                       move cpf-destino   to save-destino
                       move 0 to tot-qta
                    end-if
                    if cpf-fornitore not = save-fornitore or
                       cpf-destino   not = save-destino
                       if tot-qta > 0

                          initialize como-riga
                          string "AGGIUNTO ARTICOLO: " cpf-articolo
                                 " - FORNITORE: "      cpf-fornitore
                                 " - QTA: "            tot-qta
                            into como-riga
                          end-string
                          perform SCRIVI-RIGA-LOG

                          add 1 to idx-forn
                          move save-fornitore to el-forn(idx-forn)
                          move save-destino   to el-dest(idx-forn)
                       end-if
                       move cpf-fornitore to save-fornitore
                       move cpf-destino   to save-destino
                       move cpf-qta       to tot-qta 
                    else
                       add cpf-qta to tot-qta
                    end-if
                 end-if
              end-perform
           end-if.
           if idx-forn not = 0
              perform CREA-ORDINI
           else                    
              move "NESSUN ORDINE DA CREARE" to como-riga
              perform SCRIVI-RIGA-LOG
           end-if.
                                   
           move "FINE ELABORAZIONE" to como-riga.
           perform SCRIVI-RIGA-LOG.

      ***---
       CREA-ORDINI.  
           move "INIZIO CREAZIONE ORDINI" to como-riga.
           perform SCRIVI-RIGA-LOG.

           |Fase 1: separo gli ordini nei due magazzini (LBX e std)
           move low-value to cpf-rec.
           start coperfab key >= cpf-chiave
                 invalid set errori to true
           end-start.
           if tutto-ok

              move spaces   to tge-chiave
              read tparamge no lock invalid continue end-read

              move 0 to save-articolo
              perform until 1 = 2
                 read coperfab next at end exit perform end-read
                 if articolo-fisso not = 0
                    if articolo-fisso not = cpf-articolo
                       exit perform cycle 
                    end-if
                 end-if
                 if cpf-articolo not = save-articolo               
                    move cpf-articolo to art-codice save-articolo
                    read articoli no lock invalid continue end-read
                    move cpf-articolo to ord2-articolo
                    move "LBX"        to ord2-mag
                    read ordfor2 no lock invalid continue end-read 

                    |SE IL FORNITORE E' STRANIERO SEMPRE SU LBX
                    if art-mag-std not = "LBX"                 
                       set  cli-tipo-F    to true
                       move cpf-fornitore to cli-codice
                       read clienti no lock invalid continue end-read
                       if cli-nazione not = "ITA"
                          |TROVO IL PROGRESSIVO A MAGGIOR GIACENZA VALORE ASSOLUTO
                          move 0 to giacenza 
                          initialize sav-chiave replacing 
                                                numeric data by zeroes
                                           alphanumeric data by spaces
                          move low-value   to prg-rec
                          move art-codice  to prg-cod-articolo
                          move "LBX"       to prg-cod-magazzino
                          start progmag key >= prg-chiave
                                invalid continue
                            not invalid
                                perform until 1 = 2
                                   read progmag next 
                                        at end exit perform 
                                   end-read
                                   if prg-cod-articolo  not = art-codice
                                   or prg-cod-magazzino not = "LBX"
                                      exit perform
                                   end-if
                                   if prg-attivo
                                      if sav-cod-articolo = 0
                                         move prg-chiave   to sav-chiave
                                         move prg-giacenza to giacenza
                                      end-if
                                      |VALORE ASSOLUTO
                                      move prg-giacenza to como-giacenza
                                      if como-giacenza > giacenza
                                         move prg-chiave   to sav-chiave
                                         move prg-giacenza to giacenza
                                      end-if
                                   end-if
                                end-perform
                          end-start         

                          move art-mag-std to tca-cod-magaz
                          set tca-si-ord-forn to true
                          read tcaumag no lock key k-mag
                               invalid continue
                          end-read

                          move "LBX" to art-mag-std
                          move sav-tipo-imballo to ord2-imballo
                          move sav-peso         to ord2-peso
                          write ord2-rec 
                                invalid rewrite ord2-rec 
                          end-write
                       end-if
                    else                      
                       |2307: REIMPOSTO LA CAUSALE
                       move tge-causale-ord-forn to tca-codice
                       read tcaumag                               
                       |TROVO IL PROGRESSIVO A MAGGIOR GIACENZA VALORE ASSOLUTO
                       move 0 to giacenza 
                       initialize sav-chiave replacing 
                                             numeric data by zeroes
                                        alphanumeric data by spaces
                       move low-value   to prg-rec
                       move art-codice  to prg-cod-articolo
                       move art-mag-std to prg-cod-magazzino
                       start progmag key >= prg-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read progmag next 
                                     at end exit perform 
                                end-read
                                if prg-cod-articolo  not = art-codice
                                or prg-cod-magazzino not = "LBX"
                                   exit perform
                                end-if
                                if prg-attivo
                                   if sav-cod-articolo = 0
                                      move prg-chiave   to sav-chiave
                                      move prg-giacenza to giacenza
                                   end-if
                                   |VALORE ASSOLUTO
                                   if prg-giacenza > giacenza
                                      move prg-chiave   to sav-chiave
                                      move prg-giacenza to giacenza
                                   end-if
                                end-if
                             end-perform
                       end-start
                    end-if

                    if art-mag-std not = "LBX"
                       |CONTROLLO QUALE SIA LA QTA 
                       |UTILE PER COPRIRE LBX
                       move 0 to giacenza impegnato
                       move low-value  to prg-rec
                       move art-codice to prg-cod-articolo
                       move "LBX"      to prg-cod-magazzino
                       start progmag key >= prg-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read progmag next 
                                     at end exit perform 
                                end-read
                                if prg-cod-articolo  not = art-codice or
                                   prg-cod-magazzino not = "LBX"
                                   exit perform
                                end-if
                                if prg-attivo
                                   add prg-giacenza  to giacenza
                                   add prg-impegnato to impegnato
                                end-if
                             end-perform
                       end-start
                       compute qta-utile-LBX = impegnato -
                                               giacenza  +
                                               ord2-media-vend

                       |RECUPERO IL PROGRESSIVO CON > GIACENZA 
                       |IN VALORE ASSOLUTO SU CUI FARE L'ORDINE
                       move 0 to giacenza 

                       initialize sav-chiave replacing 
                                             numeric data by zeroes
                                        alphanumeric data by spaces
                       move low-value   to prg-rec
                       move art-codice  to prg-cod-articolo
                       move art-mag-std to prg-cod-magazzino
                       start progmag key >= prg-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read progmag next 
                                     at end exit perform 
                                end-read
                                if prg-cod-articolo  not = art-codice or
                                   prg-cod-magazzino not = art-mag-std
                                   exit perform
                                end-if  
                                if prg-attivo
                                   if sav-cod-articolo = 0
                                      move prg-chiave   to sav-chiave
                                      move prg-giacenza to giacenza
                                   end-if
                                   |VALORE ASSOLUTO
                                   move prg-giacenza to como-giacenza
                                   if como-giacenza > giacenza
                                      move prg-chiave   to sav-chiave
                                      move prg-giacenza to giacenza
                                   end-if
                                end-if
                             end-perform
                       end-start

                       |RECUPERO LA CAUSALE       
                       move art-mag-std to tca-cod-magaz
                       set tca-si-ord-forn to true
                       read tcaumag no lock key k-mag
                            invalid continue
                       end-read                              
                    else
                       move 99999999 to qta-utile-LBX
                    end-if
                 end-if

                 |LBX va considerato...
                 if qta-utile-LBX > 0

      **              230719: TUTTO SUL MAGAZZINO DELL'ARTICOLO
      **              move tge-causale-ord-forn to cpfm-causale
      **              |...E' tutto su lubex perciò devo valutare 
      **              |solamente se la qta per quell'articolo serve 
      **              |per coprire oppure se è un "nativo" LBX.
      **
      **              |Se è SOLO articolo "nativo" allora la qta LBX è 
      **              |stata impostat al max ed entrerà sempre qua
      **
      **              if qta-utile-LBX > cpf-qta
      **                 |SE non è "nativo" genero la copertura...
      **                 if art-mag-std not = "LBX"
      **                    set cpfm-copertura-lbx-si to true
      **                 else
      **                    |...altrimenti faccio la programmazione
      **                    set cpfm-copertura-lbx-no to true
      **                 end-if
      **                 move cpf-qta       to como-qta
      **                 subtract cpf-qta from qta-utile-LBX
      **                 move ord2-articolo to cpfm-cod-articolo
      **                 move ord2-mag      to cpfm-cod-magazzino
      **                 move ord2-imballo  to cpfm-tipo-imballo
      **                 move ord2-peso     to cpfm-peso
      **                 perform VALORIZZA-CPFM
      **
      **              else
      **                 |LBX è coperto per una parte e devo quindi splittare
      **                 |La prima parte è copertura...
      **                 set cpfm-copertura-lbx-si to true
      **
      **                 move qta-utile-LBX to como-qta
      **                 move ord2-articolo to cpfm-cod-articolo
      **                 move ord2-mag      to cpfm-cod-magazzino
      **                 move ord2-imballo  to cpfm-tipo-imballo
      **                 move ord2-peso     to cpfm-peso
      **                 perform VALORIZZA-CPFM

                       |... il resto faccio programmazione
                       set cpfm-copertura-lbx-no to true
                       move tca-codice  to cpfm-causale
                       move cpf-qta     to como-qta
                       move sav-chiave  to cpfm-prg-chiave 
      **                 subtract qta-utile-LBX from cpf-qta 
      **                   giving como-qta
                       perform VALORIZZA-CPFM
      **              end-if
                 else
                    |LBX non ha copertura perciò posso fare programmazione
                    set cpfm-copertura-lbx-no to true
                    move tca-codice  to cpfm-causale
                    move cpf-qta     to como-qta
                    move sav-chiave  to cpfm-prg-chiave
                    perform VALORIZZA-CPFM
                 end-if
              end-perform

              move tge-anno to con-anno
              read tcontat  no lock invalid continue end-read
              add 1 to con-num-ord-forn giving primo-numero    
                    
              move "CICLO COPERTURA FABBISGNO NO PROGRAMMAZIONE" 
                to como-riga
              perform SCRIVI-RIGA-LOG

              move low-value to cpfm-rec
              start coperfab-mag key >= k-forn
              move 0 to LinkFirst LinkLast
              move 0 to save-fornitore 
              move 0 to save-destino
              move spaces to save-causale
              perform until 1 = 2
                 read coperfab-mag next at end exit perform end-read  
                 
                 if articolo-fisso not = 0
                    if articolo-fisso not = cpfm-articolo
                       exit perform cycle 
                    end-if
                 end-if
                                             
                 if cpfm-programmazione-no
                    move cpfm-articolo to ord2-articolo
                    move "LBX"         to ord2-mag
                    read ordfor2 no lock invalid continue end-read
                    |Il mese 1 è SEMPRE valorizzato
                    if ord2-si-conferma and cpfm-qta-m(1) > 0
                       if cpfm-causale not = save-causale
                          move cpfm-causale to save-causale
                          move 0 to save-fornitore
                          move 0 to save-destino
                       end-if
                       |Indico per ogni fornitore quanti ordini 
                       |dovrò creare come programmazione
                       if cpfm-fornitore not = save-fornitore or
                          cpfm-destino   not = save-destino
                          move 0 to numero-ordf(1)
                          move 0 to numero-ordf(2)
                          move 0 to numero-ordf(3)
                          move 0 to numero-ordf(4)
                          move 0 to numero-ordf(5)
                          move 0 to numero-ordf(6)
                          perform CERCA-FORNITORE
                          move cpfm-fornitore to save-fornitore
                          move cpfm-destino   to save-destino
                          if trovato
                             move 0 to SaveRiga
                             perform READ-FORNITORI-DESTINIF
                             perform SCRIVI-TESTA-ORDINE
                          end-if
                       end-if
                       if trovato
                          perform SCRIVI-RIGA-ORDINE
                       end-if
                    end-if
                    |I record di programmazione li tratto dopo
                    delete coperfab-mag record
                 end-if
              end-perform         

              move "CICLO COPERTURA FABBISGNO PROGRAMMAZIONE" 
                to como-riga
              perform SCRIVI-RIGA-LOG

              |Solo quelli di programmazione
              |ciclo creo gli ordini in programmazione
              move low-value to cpfm-rec
              start coperfab-mag key >= k-forn
                    invalid continue
                not invalid move "00" to status-coperfab-mag
              end-start
              move 0 to save-fornitore 
              move 0 to save-destino
              move spaces to save-causale
              perform until 1 = 2
                 if status-coperfab-mag not = "00"
                    exit perform
                 end-if
                 read coperfab-mag next at end exit perform end-read 
                 move cpfm-articolo to ord2-articolo   
                                          
                 if articolo-fisso not = 0
                    if articolo-fisso not = cpfm-articolo
                       exit perform cycle 
                    end-if
                 end-if
                 
                 move "LBX"         to ord2-mag
                 read ordfor2 no lock invalid continue end-read
                 if ord2-si-conferma and cpfm-programmazione-si   

                    if ( cpfm-qta-m(1) + 
                         cpfm-qta-m(2) +
                         cpfm-qta-m(3) +
                         cpfm-qta-m(4) +
                         cpfm-qta-m(5) +
                         cpfm-qta-m(6) ) > 0                     

                       initialize como-riga
                       string "TRATTO ARTICOLO: "
                              ord2-articolo
                              " - MAG: "
                              ord2-mag
                         into como-riga
                       end-string
                       perform SCRIVI-RIGA-LOG

                       if LinkAuto = 1             
                          move cpfm-articolo to art-codice     
                          read articoli no lock
                          move art-scorta to sco-codice
                          read tscorte no lock
                          perform RICALCOLA-SCORTA-PROGRAMMAZIONE
                       end-if

                       if cpfm-causale not = save-causale
                          move cpfm-causale to save-causale
                          move 0 to save-fornitore
                          move 0 to save-destino
                          move 0 to numero-ordf(1)
                          move 0 to numero-ordf(2)
                          move 0 to numero-ordf(3)
                          move 0 to numero-ordf(4)
                          move 0 to numero-ordf(5)
                          move 0 to numero-ordf(6)
                       end-if
                       |Indico per ogni fornitore quanti ordini 
                       |dovrò creare come programmazione
                       if cpfm-fornitore not = save-fornitore or
                          cpfm-destino   not = save-destino
                          perform CERCA-FORNITORE
                          move cpfm-fornitore to save-fornitore
                          move cpfm-destino   to save-destino
                          if trovato
                             perform READ-FORNITORI-DESTINIF
                             perform SCRIVI-TESTA-PROGRAMMAZIONE
                          end-if
                       end-if
                       if trovato
                          perform SCRIVI-RIGA-PROGRAMMAZIONE
                       end-if
                    end-if
                 end-if
              end-perform

              |Alla fine di tutto scrivo il contatore
              rewrite con-rec invalid continue end-rewrite
           end-if.
                      
           move "FINE CREAZIONE ORDINI" to como-riga.
           perform SCRIVI-RIGA-LOG.

      ***---
       RICALCOLA-SCORTA-PROGRAMMAZIONE.
      *****     move 0 to ultimo-mese-moq.
      *****     |1. arrotondo tutte le quantiaà al bancale successivo senza
      *****     |   tener conto della % arrotondamento dei par generali
      *****     perform varying idx from 1 by 1 
      *****               until idx > 6
      *****        if cpfm-qta-m(idx) > 0
      *****           move idx to ultimo-mese-moq
      *****           move cpfm-qta-m(idx) to como-qta-moq
      *****           perform QTA-BANCALE-SUCCESSIVO-MOQ  
      *****           move como-qta-moq to cpfm-qta-m(idx)
      *****        end-if
      *****     end-perform.
      *****     |2. Verifico che la somma delle qta sia >= a MOQ
      *****     move 0 to tot-qta-moq.
      *****     perform varying idx from 1 by 1 
      *****               until idx > 6
      *****        add cpfm-qta-m(idx) to tot-qta-moq
      *****     end-perform.                    
      *****     |Sto ordinando di più rispetto al MOQ, a posto così
      *****     if tot-qta-moq < art-moq
      *****        compute diff-moq = art-moq - tot-qta-moq
      *****        |3. Aggiungo la differenza alla quantità dell'ultimo mese
      *****        add diff-moq to cpfm-qta-m(ultimo-mese-moq)
      *****     end-if.
      *****     |4. La arrotono comunque nuovamente al bancale successivo
      *****     move cpfm-qta-m(ultimo-mese-moq) to como-qta-moq.
      *****     perform QTA-BANCALE-SUCCESSIVO-MOQ.
      *****     move como-qta-moq to cpfm-qta-m(ultimo-mese-moq).
           
      *****     if cpfm-articolo = 15689  
      *****        move  5 to cpfm-qta-m(1)
      *****        move 15 to cpfm-qta-m(2)
      *****        move 15 to cpfm-qta-m(3)
      *****        move 15 to cpfm-qta-m(4)
      *****        move "C09" to cpfm-tipo-imballo
      *****     end-if.     

           |1. prendo il maggiore tra moq e tot
           move 0 to tot-qta-moq.
           perform varying idx from 1 by 1 
                     until idx > 6
              add cpfm-qta-m(idx) to tot-qta-moq
              if cpfm-qta-m(idx) not = 0
                 move idx to ultimo-mese-moq
              end-if
           end-perform.                    
           |Sto ordinando di più rispetto al MOQ
           if tot-qta-moq < art-moq
              move art-moq to tot-qta-moq    

              initialize como-riga
              string "QTA TOT: "          tot-qta-moq
                     " - MOQ ARTICOLO: "  art-moq
                     ". UTILIZZO ART-MOQ"
                into como-riga
              end-string
              perform SCRIVI-RIGA-LOG

           end-if.                           
                  
           if art-qta-epal = 0 and art-qta-std not = 0
              move art-qta-std to art-qta-epal
           end-if.
           if art-qta-epal = 0 
              move cpfm-tipo-imballo to imq-codice
              read timbalqta no lock invalid continue end-read
              move imq-tipo          to imb-codice
              read timballi  no lock invalid continue end-read
              move imq-qta-imb     to art-qta-epal
              move imq-qta-imb to art-qta-epal
           end-if.

           if tot-qta-moq > art-qta-epal
              move 0 to resto
              divide tot-qta-moq by art-qta-epal
                          giving num-bancali
                       remainder resto
              if resto not = 0
                 add 1 to num-bancali
              end-if
           else
              move 1 to num-bancali
           end-if.    

           initialize como-riga
           string "ULTIMO MESE MOQ: " ultimo-mese-moq
                  " - TOT QTA MOQ: "  tot-qta-moq
                  " - QTA EPAL: "     art-qta-epal
                  " - BANCALI: "      num-bancali
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.
               
           |4. trasformo la qta in pezzi
           compute tot-qta-moq = num-bancali * art-qta-epal.
                                                                 
           compute tot-qta-mese2 = cpfm-qta-m(1) + 
                                   cpfm-qta-m(2).
           compute tot-qta-mese3 = cpfm-qta-m(1) + 
                                   cpfm-qta-m(2) +
                                   cpfm-qta-m(3).
           compute tot-qta-mese4 = cpfm-qta-m(1) + 
                                   cpfm-qta-m(2) +
                                   cpfm-qta-m(3) + 
                                   cpfm-qta-m(4).
           compute tot-qta-mese5 = cpfm-qta-m(1) + 
                                   cpfm-qta-m(2) +
                                   cpfm-qta-m(3) + 
                                   cpfm-qta-m(4) + 
                                   cpfm-qta-m(5).
           compute tot-qta-mese6 = cpfm-qta-m(1) + 
                                   cpfm-qta-m(2) +
                                   cpfm-qta-m(3) + 
                                   cpfm-qta-m(4) + 
                                   cpfm-qta-m(5) + 
                                   cpfm-qta-m(6).
                                   
           initialize como-riga
           string "TOT QTA MOQ: " tot-qta-moq  
                  " - MESE 2: "   tot-qta-mese2
                  " - MESE 3: "   tot-qta-mese3
                  " - MESE 4: "   tot-qta-mese4
                  " - MESE 5: "   tot-qta-mese5
                  " - MESE 6: "   tot-qta-mese6
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           perform varying idx-mese from 1 by 1 
                     until idx-mese > ultimo-mese-moq
                 
              |Calcolo la qta precedentemente ordinata
              move 0 to como-qta-ordinata
              perform varying idx from 1 by 1 
                        until idx = idx-mese
                 add cpfm-qta-m(idx) to como-qta-ordinata
              end-perform

              if ultimo-mese-moq = idx-mese
                 
                 compute como-qta-rical = 
                         tot-qta-moq -     |Totale qta trasformata
                         como-qta-ordinata |Differenza con ordinata precedentemente

                 if como-qta-rical > 0 
                    |Arrotondo a bancale
                    if como-qta-rical > art-qta-epal
                       move 0 to resto
                       divide como-qta-rical by art-qta-epal
                                   giving num-bancali
                                remainder resto
                       if resto not = 0
                          add 1 to num-bancali
                       end-if
                    else
                       move 1 to num-bancali
                    end-if
                    compute cpfm-qta-m(idx-mese) = 
                            art-qta-epal * num-bancali
                                        
                    |Arrotondo a imballo
                    move cpfm-qta-m(idx-mese) to rof-qta-ord
                    perform CHECK-IMBALLO
                    move rof-qta-ord to cpfm-qta-m(idx-mese)
           
                 else
                    move 0 to cpfm-qta-m(idx-mese)
                 end-if 
              else
                 |Totale richiesta - qta ordinata prima
                 evaluate idx-mese
                 when 1 
                      move cpfm-qta-m(1) to como-qta-rical
                 when 2
                      compute como-qta-rical = 
                              tot-qta-mese2 - como-qta-ordinata
                 when 3
                      compute como-qta-rical = 
                              tot-qta-mese3 - como-qta-ordinata
                 when 4
                      compute como-qta-rical = 
                              tot-qta-mese4 - como-qta-ordinata
                 when 5
                      compute como-qta-rical = 
                              tot-qta-mese5 - como-qta-ordinata
                 when 6
                      compute como-qta-rical = 
                              tot-qta-mese6 - como-qta-ordinata
                 end-evaluate
                 if como-qta-rical > 0 
                    |Arrotondo a bancale
                    if como-qta-rical > art-qta-epal
                       move 0 to resto
                       divide como-qta-rical by art-qta-epal
                                   giving num-bancali
                                remainder resto
                       if resto not = 0
                          add 1 to num-bancali
                       end-if
                    else
                       move 1 to num-bancali
                    end-if
                    compute cpfm-qta-m(idx-mese) = 
                            art-qta-epal * num-bancali
              
                    |Arrotondo a imballo
                    move cpfm-qta-m(idx-mese) to rof-qta-ord
                    perform CHECK-IMBALLO
                    move rof-qta-ord to cpfm-qta-m(idx-mese)
              
                 else
                    move 0 to cpfm-qta-m(idx-mese)
                 end-if
              end-if
           end-perform.                            
                                   
           initialize como-riga
           string "DOPO RICALCOLO MOQ:"        
                  " - MESE 1: "   cpfm-qta-m(1)
                  " - MESE 2: "   cpfm-qta-m(2)
                  " - MESE 3: "   cpfm-qta-m(3)
                  " - MESE 4: "   cpfm-qta-m(4)
                  " - MESE 5: "   cpfm-qta-m(5)
                  " - MESE 6: "   cpfm-qta-m(6)
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

      ***---
       PROGRAMMAZIONE-ORDINI.  
           set cpfm-programmazione-si to true.
           compute qta(1) = ord2-fabb-qta(1).
           compute qta(2) = ord2-fabb-qta(2) - ord2-fabb-qta(1).
           compute qta(3) = ord2-fabb-qta(3) - ord2-fabb-qta(2).
           compute qta(4) = ord2-fabb-qta(4) - ord2-fabb-qta(3).
           compute qta(5) = ord2-fabb-qta(5) - ord2-fabb-qta(4).
           compute qta(6) = ord2-fabb-qta(6) - ord2-fabb-qta(5).

           initialize cpfm-data.
           perform varying idx from 1 by 1
                     until idx > ord2-mese-rif
              if idx = mese-rif
                 move tof-data-consegna to cpfm-data-m(idx)
              else
                 move idx to mese-rif
                 perform CALCOLA-DATA-CONSEGNA
                 move data-consegna to cpfm-data-m(idx)
              end-if
              move idx to cpfm-mese-rif(idx)            
           end-perform.

           perform varying idx from 1 by 1 
                     until idx > ord2-mese-rif
              if qta(idx) > 0
                 if como-qta > qta(idx)
                    move qta(idx) to cpfm-qta-m(idx)
                    subtract qta(idx) from como-qta
                 else
                    move como-qta to cpfm-qta-m(idx)
                    move 0 to como-qta
                    exit perform
                 end-if
              else
                 move 0 to cpfm-qta-m(idx)
              end-if
           end-perform.
           |Se ho scelto quantità superiore la metto nell'ultimo mese
           if como-qta > 0
              add como-qta to cpfm-qta-m(ord2-mese-rif)
           end-if.

      ***---
       CERCA-FORNITORE.
           set trovato to false.
           set idx-forn to 1.
           search tab-fornitori
           when el-forn(idx-forn) = cpfm-fornitore and
                el-dest(idx-forn) = cpfm-destino
                set trovato to true
           end-search.

      ***---
       VALORIZZA-CPFM.
           move cpf-articolo  to cpfm-articolo art-codice.
           read articoli no lock invalid continue end-read.

           initialize cpfm-qta.
           move como-qta to cpfm-qta-m(1).

           set cpfm-programmazione-no to true.
           if cpfm-copertura-LBX-no
              move cpfm-articolo to ord2-articolo
              move "LBX"         to ord2-mag
              read ordfor2 no  lock invalid continue end-read

      *****        move ord2-mese-rif to mese-rif

              set cli-tipo-F to true
              move cpf-fornitore  to cli-codice desf-codice
              read clienti no lock invalid continue end-read

              move cpf-destino  to desf-prog
              read destinif no lock invalid continue end-read

              |Gli ordini devono essere sempre urgenti
              move 1 to mese-rif
              perform CALCOLA-DATA-CONSEGNA
              move data-consegna to tof-data-consegna

      *****        perform CALCOLA-MESE-RIF
              move art-scorta to sco-codice
              read tscorte no lock
                   invalid continue
               not invalid
                   if sco-programmazione-si
                      perform PROGRAMMAZIONE-ORDINI
                   else
                      move tof-data-consegna to cpfm-data-m(1)
                      move mese-rif          to cpfm-mese-rif(1)
                   end-if
              end-read
           end-if.

           if cpfm-causale = spaces exit paragraph end-if.

           move cpf-listino       to cpfm-listino.
           move cpf-tipo          to cpfm-tipo.
           move cpf-fornitore     to cpfm-fornitore.
           move cpf-destino       to cpfm-destino.
           move cpf-data-ini      to cpfm-data-ini.
           move cpf-data-fine     to cpfm-data-fine.
           move cpf-totale        to cpfm-totale.
           move cpf-prz-listino   to cpfm-prz-listino.
           move cpf-lead-time     to cpfm-lead-time.
           write cpfm-rec invalid continue end-write.     
                    
           initialize como-riga.
           string "SCRITTA COPERTURA PER ARTICOLO: " 
                  cpfm-articolo
                  " - FORNITORE: "
                  cpfm-fornitore
                  " - LISTINO: "
                  cpfm-listino
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.


      ***---
       CALCOLA-DATA-CONSEGNA.
           accept como-data from century-date.
           if mese-rif = 1
              compute como-data = function integer-of-date(como-data)
              add desf-gg-consegna to como-data
              compute como-data = function date-of-integer(como-data)

              call   "utydata" using dom-lun,
                                     1,
                                     como-data,
                                     giorno
              cancel "utydata"
              compute como-data = function integer-of-date(como-data)
              evaluate giorno 
              when 6 add 2 to como-data
              when 7 add 1 to como-data
              end-evaluate
              compute data-consegna = 
                      function date-of-integer(como-data)  


              accept como-data from century-date
           else
              move   como-data(5:2) to mese-oggi
              move   como-data(1:4) to anno-oggi
              compute mese-consegna =  mese-oggi + mese-rif - 1
              if mese-consegna > 12
                 subtract 12 from mese-consegna
                 add 1 to anno-oggi
              end-if
              move anno-oggi     to data-consegna(1:4)
              move mese-consegna to data-consegna(5:2)
                                                      
      *****     compute como-numero =
      *****             function integer-of-date(como-data)
      *****     compute como-numero = ( 30 * mese-rif ) + como-numero.
      *****     compute data-consegna =
      *****             function date-of-integer(como-numero).
              move 10 to data-consegna(7:2)

              call   "utydata" using dom-lun,
                                     1,
                                     data-consegna,
                                     giorno
              cancel "utydata"
              evaluate giorno
              when 6
                   move 12 to data-consegna(7:2)
              when 7
                   move 11 to data-consegna(7:2)
              when other continue
              end-evaluate
           end-if.

      ********---
      ***** CALCOLA-MESE-RIF.
      *****     if como-data >= tof-data-consegna
      *****        move 1   to mese-rif
      *****     else
      *****        move 0 to mese-rif
      *****        compute como-numero = 
      *****                function integer-of-date(tof-data-consegna) -
      *****                function integer-of-date(como-data)
      *****        divide como-numero by 30 giving mese-rif
      *****                              remainder resto
      *****        if resto not = zero
      *****           add 1 to mese-rif
      *****        end-if
      *****     end-if.

      ***---
       READ-FORNITORI-DESTINIF.
           set tutto-ok   to true.
           set cli-tipo-F to true.
           move cpfm-fornitore to cli-codice.
           read clienti no lock invalid continue end-read.

           initialize desf-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           move cpfm-fornitore to desf-codice.
           move cpfm-destino   to desf-prog.
           read destinif no lock invalid continue end-read.

      ***---
       SCRIVI-TESTA-ORDINE.
           add 1 to con-num-ord-forn.
           if LinkFirst = 0
              move con-num-ord-forn  to LinkFirst
           end-if.
           initialize tof-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           move con-num-ord-forn      to LinkLast.
           move tge-anno              to tof-anno.
           move con-num-ord-forn      to tof-numero.
           move cpfm-causale          to tof-causale.
           move como-data             to tof-data-ordine.
           move cpfm-fornitore        to tof-cod-forn.
           move cpfm-destino          to tof-destino.
           move desf-referente-ord    to tof-referente.
           move desf-tel-dir-ref-ord  to tof-tel-dir.
           move desf-mail-ref-ord     to tof-email.
           move desf-fax              to tof-fax.
           if desf-pag not = spaces
              move desf-pag           to tof-cod-pagamento
           else 
              move cli-pag            to tof-cod-pagamento
           end-if.
           move cli-iva-ese           to tof-cod-ese-iva.

           if cpfm-copertura-LBX-si
              move 1 to mese-rif tof-mese-rif
              accept tof-data-consegna from century-date
      *****        perform CALCOLA-DATA-CONSEGNA
      *****        move data-consegna to tof-data-consegna
           else
              move cpfm-data-m(1)   to tof-data-consegna
              move cpfm-mese-rif(1) to tof-mese-rif   
           end-if.
      *****     if tof-mese-rif = 1
      *****        set tof-urgente to true
      *****     end-if.
           if tof-email not = space
              set tof-invio-mail to true
           else
              if tof-fax not = space
                 set tof-invio-fax to true
              else
                 set tof-invio-man to true
              end-if
           end-if.
           set tof-automatico to true.
           set tof-inserito   to true.
           set tof-inevaso    to true.
           accept tof-data-creazione from century-date.
           accept tof-ora-creazione  from time.
           move   LinkUser  to tof-utente-creazione.
           move 0 to tof-data-ultima-modifica
                     tof-ora-ultima-modifica.
           move spaces to tof-utente-ultima-modifica.
           write tof-rec invalid continue end-write.

           initialize como-riga.
           string "SCRITTA TESTATA SINGOLA: "
                  tof-anno
                  " - "
                  tof-numero
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           perform VAL-NOTE-LISTINO.
           perform SCRIVI-TMP-CONTROLLO.

      ***---
       VAL-NOTE-LISTINO.
           move low-value      to nlis-rec.
           move cpfm-listino   to nlis-tlis-codice.
           start nlistini key >=  nlis-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read nlistini next at end exit perform end-read
                    if nlis-tlis-codice not = cpfm-listino
                       exit perform
                    end-if
                    add   1           to nof-num-nota
                    move tof-chiave   to nof-chiave-ordine
                    move nlis-nota    to nof-nota
                    accept nof-data-creazione from century-date
                    accept nof-ora-creazione  from time
                    move   LinkUser  to nof-utente-creazione
                    move 0 to nof-data-ultima-modifica
                              nof-ora-ultima-modifica
                    move spaces to nof-utente-ultima-modifica 
                    write nof-rec
                 end-perform
           end-start.

      ***---
       SCRIVI-RIGA-ORDINE.                                
           initialize rof-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           move tof-chiave to rof-chiave-testa.
           add 1 to SaveRiga.
           move SaveRiga     to rof-riga.

           move cpfm-articolo      to rof-cod-articolo  
                                      prg-cod-articolo.
           move cpfm-cod-magazzino to rof-cod-magazzino 
                                      prg-cod-magazzino.
           move cpfm-tipo-imballo  to rof-tipo-imballo  
                                      rof-imb-ordinato
                                      prg-tipo-imballo.
           move cpfm-peso          to rof-peso prg-peso.

           perform TROVA-PROGRESSIVO-ATTIVO. 

           move prg-chiave       to rof-prg-chiave.
           move rof-tipo-imballo to rof-imb-ordinato.                          

           move cpfm-articolo to art-codice.
           read articoli no lock invalid continue end-read.

           perform VALORI-PREZZO-IMPOSTE.

           move cpfm-qta-m(1) to rof-qta-ord.

           perform QTA-SCORTA-NON-PROGRAMMATA.

           if cli-iva-ese = spaces
              move art-codice-iva to rof-cod-iva
           else
              move cli-iva-ese to rof-cod-iva
           end-if.

           move prg-peso-utf     to rof-peso-utf.
           move prg-peso-non-utf to rof-peso-non-utf.
           accept rof-data-creazione from century-date.
           accept rof-ora-creazione  from time.
           move   LinkUser  to rof-utente-creazione.
           move 0 to rof-data-ultima-modifica
                     rof-ora-ultima-modifica.
           move spaces to rof-utente-ultima-modifica.
           write rof-rec invalid continue end-write.

           initialize como-riga.
           string "SCRITTA RIGA NO PROGRAMMAZIONE: " 
                  rof-anno
                  " - "
                  rof-numero
                  " - "
                  rof-riga
                  " - ARTICOLO: "
                  rof-cod-articolo
                  " - QTA: "
                  rof-qta-ord
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG

           add rof-qta-ord       to tof-pz-tot.

           move rlis-codice to tlis-codice.
           read tlistini no lock invalid continue end-read.
           if tlis-trasp-f-escluso
              set tof-franco-part-si to true
           end-if.
           rewrite tof-rec  invalid continue end-rewrite. 

      ***---
       VALORI-PREZZO-IMPOSTE.
           move cpfm-listino  to rlis-codice 
                                 rof-cod-listino.
           move cpfm-articolo to rlis-articolo.
           read rlistini no lock invalid continue end-read.

           perform CALCOLA-PRZ-FINITO.
           perform RECUPERA-PREZZO.
           perform RECUPERA-SCONTO.
           perform VALORIZZA-IMPOSTE.

           move imponibile-merce to rof-imponib-merce.

      ***---
       RECUPERA-PREZZO.   
           if rlis-sconti-zero-no
              move rlis-prz-acq to rof-prz-unitario
           else
              move rlis-scelta to rof-prz-unitario
           end-if.

      ***---
       RECUPERA-SCONTO.
           if rlis-sconti-zero-no
              move rlis-sconto-1 to rof-sconto-1
           else
              move 0             to rof-sconto-1
           end-if.
           move rlis-sconto-2    to rof-sconto-2.
           move rlis-sconto-3    to rof-sconto-3.
           move rlis-sconto-4    to rof-sconto-4.
           move rlis-sconto-5    to rof-sconto-5.

           move rlis-costi-agg-tot  to rof-costi-aggiuntivi.

      ***---
       VALORIZZA-IMPOSTE.                               
           move imposta-consumo  to rof-imp-consumo.
           add imposta-cou       to 
               imposta-cobat giving rof-imp-cou-cobat.
           move add-piombo       to rof-add-piombo.

      ***---
       SCRIVI-TESTA-PROGRAMMAZIONE.
           perform varying idx from 1 by 1
                     until idx > 6
      *****        if cpfm-qta-m(idx) > 0
                 perform SCRIVI-TESTA-MESE
      *****        end-if
           end-perform.
           unlock destinif all records.


      ***---
       SCRIVI-TESTA-MESE.
           initialize tof-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           add 1 to con-num-ord-forn.
           if LinkFirst = 0
              move con-num-ord-forn to LinkFirst
           end-if.
           move con-num-ord-forn     to numero-ordf(idx).
           move con-num-ord-forn     to LinkLast.

           move tge-anno             to tof-anno.
           move con-num-ord-forn     to tof-numero.
           move cpfm-causale         to tof-causale.
           move como-data            to tof-data-ordine.
           move cpfm-fornitore       to tof-cod-forn.
           move cpfm-destino         to tof-destino.
           move desf-referente-ord   to tof-referente.
           move desf-tel-dir-ref-ord to tof-tel-dir.
           move desf-mail-ref-ord    to tof-email.
           move desf-fax             to tof-fax.
           if desf-pag not = spaces
              move desf-pag          to tof-cod-pagamento
           else 
              move cli-pag           to tof-cod-pagamento
           end-if.
           move cli-iva-ese          to tof-cod-ese-iva.
           move cpfm-data-m(idx)     to tof-data-consegna.
           move cpfm-mese-rif(idx)   to tof-mese-rif.
      *****     if tof-mese-rif = 1
      *****        set tof-urgente        to true
      *****     end-if.
           if tof-email not = space
              set tof-invio-mail to true
           else
              if tof-fax not = space
                 set tof-invio-fax to true
              else
                 set tof-invio-man to true
              end-if
           end-if.
           set tof-automatico to true.
           set tof-inserito   to true.
           set tof-inevaso    to true.
           set tof-programmazione-si to true.
           accept tof-data-creazione from century-date.
           accept tof-ora-creazione  from time.               
           move   LinkUser  to tof-utente-creazione.
           move 0 to tof-data-ultima-modifica
                     tof-ora-ultima-modifica.
           move spaces to tof-utente-ultima-modifica.
           write tof-rec invalid continue end-write.
                           
           initialize como-riga.
           string "SCRITTA TESTATA PROGRAMMAZIONE: "
                  tof-anno
                  " - "
                  tof-numero
             into como-riga
           end-string.
           perform SCRIVI-RIGA-LOG.

           perform VAL-NOTE-LISTINO.
           perform SCRIVI-TMP-CONTROLLO.

      ***---
       SCRIVI-RIGA-PROGRAMMAZIONE.
           |Partendo SEMPRE dalla somma delle qta da ordinare per i mesi 
           |di riferimento, devo fare un controllo in tempo reale tra 
           |quello che ho ordinato e quello che serve, considerando 
           |sempre le quantità a bancali.    
           
           if articolo-fisso > 0    
              if no-qta-fisso not = "S"
                 move mese1-fisso to cpfm-qta-m(1)
                 move mese2-fisso to cpfm-qta-m(2)
                 move mese3-fisso to cpfm-qta-m(3)
                 move mese4-fisso to cpfm-qta-m(4)
                 move mese5-fisso to cpfm-qta-m(5)
                 move mese6-fisso to cpfm-qta-m(6)
                 if qta-epal-fisso > 0
                    move qta-epal-fisso to art-qta-epal
                 end-if
              end-if
           end-if.

           move 0 to tot-qta-m primo-mese.
           perform varying idx from 1 by 1 
                     until idx > sco-m-rif
              if cpfm-qta-m(idx) > 0 and primo-mese = 0
                 move idx to primo-mese
              end-if
              add cpfm-qta-m(idx) to tot-qta-m
           end-perform.
                                          
           move cpfm-qta to s-cpfm-qta.

           if art-qta-epal > 0 and tot-qta-m > art-qta-epal 
              if sco-m-rif < 6
                 add 1 to sco-m-rif
                 perform varying idx from sco-m-rif by 1 
                           until idx > 6
                    move 0 to cpfm-qta-m(idx)
                 end-perform
                 subtract 1 from sco-m-rif
              end-if
           
      *****        |La testate degli ordini in programmazione vengono
      *****        |scritte in anticipo in base alla presenza delle qta mese.
      *****        |Siccome col nuovo ciclo posso cancellare delle qta devo
      *****        |resettare la situazione 
      *****        perform varying idx from 1 by 1 
      *****                  until idx > 6      
      *****           if numero-ordf(idx) > 0
      *****              move tge-anno         to tof-anno
      *****              move numero-ordf(idx) to tof-numero
      *****              delete tordforn invalid continue end-delete
      *****              move tof-chiave to tta-chiave
      *****              delete tmp-tof-auto record 
      *****                     invalid continue 
      *****              end-delete
      *****              move 0 to numero-ordf(idx)
      *****           end-if
      *****        end-perform
           
              |Mese [primo] = sempre un pallet
              if cpfm-qta-m(primo-mese) > art-qta-epal
                 move 0 to resto
                 divide cpfm-qta-m(primo-mese) by art-qta-epal 
                           giving ris
                        remainder resto
                 if resto > 0
                    add 1 to ris
                 end-if
                 compute cpfm-qta-m(primo-mese) = art-qta-epal * ris
              else
                 move art-qta-epal to cpfm-qta-m(primo-mese)                       
              end-if                         
              add 1 to primo-mese
              perform varying como-mese from primo-mese by 1 
                        until como-mese > sco-m-rif
                 if cpfm-qta-m(como-mese) = 0
                    exit perform cycle
                 end-if
                 perform CALCOLA-QTA
                 perform CALCOLA-QTA-ORD
              end-perform                       
           end-if.                          
           perform varying idx from 1 by 1 
                     until idx > 6  
                 
              if cpfm-qta-m(idx) > 0
                 initialize rof-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces

                 move tof-anno           to rof-anno

                 |Succede quando la prima volta che entro nel fornitore
                 |assegno i numeri ma parto dalla qta del mese 2 mentre
                 |l'articolo dopo parte dal mese 1 che si trova quindi scoperto
                 if numero-ordf(idx) = 0
                    perform SCRIVI-TESTA-MESE
                 end-if

                 move numero-ordf(idx)   to rof-numero
                 |La riga la trovo dopo col sistema di write   

                 move cpfm-articolo      to rof-cod-articolo
                                            prg-cod-articolo
                 move cpfm-cod-magazzino to rof-cod-magazzino
                                            prg-cod-magazzino
                 move cpfm-tipo-imballo  to rof-tipo-imballo
                                            rof-imb-ordinato
                                            prg-tipo-imballo
                 move cpfm-peso          to rof-peso prg-peso

                 perform TROVA-PROGRESSIVO-ATTIVO

                 move prg-chiave       to rof-prg-chiave
                 move rof-tipo-imballo to rof-imb-ordinato
                                         
                 move cpfm-articolo to art-codice
                 read articoli no lock invalid continue end-read

                 perform VALORI-PREZZO-IMPOSTE

                 move cpfm-qta-m(idx) to rof-qta-ord

                 if LinkAuto = 0 |ho già arrotondato al successivo
                    perform QTA-BANCALE-EPAL
                 end-if

                 if cli-iva-ese = spaces
                    move art-codice-iva to rof-cod-iva
                 else
                    move cli-iva-ese to rof-cod-iva
                 end-if

                 move prg-peso-utf     to rof-peso-utf
                 move prg-peso-non-utf to rof-peso-non-utf
                 move cpfm-qta-imb     to rof-qta-imballi
                 accept rof-data-creazione from century-date
                 accept rof-ora-creazione  from time
                 move   LinkUser  to rof-utente-creazione
                 move 0 to rof-data-ultima-modifica
                           rof-ora-ultima-modifica
                 move spaces to rof-utente-ultima-modifica
                 move 1 to rof-riga
                 perform until 1 = 2
                    write rof-rec 
                          invalid add 1 to rof-riga
                      not invalid exit perform
                    end-write
                 end-perform

                 move rof-chiave-testa to tof-chiave
                 read tordforn no lock
                 add rof-qta-ord  to tof-pz-tot

                 move rlis-codice to tlis-codice
                 read tlistini no lock invalid continue end-read
                 if tlis-trasp-f-escluso
                    set tof-franco-part-si to true
                 end-if
                 rewrite tof-rec  invalid continue end-rewrite
              end-if
           end-perform.             
                                    
      *****     move 0 to numero-ordf(1).
      *****     move 0 to numero-ordf(2).
      *****     move 0 to numero-ordf(3).
      *****     move 0 to numero-ordf(4).
      *****     move 0 to numero-ordf(5).
      *****     move 0 to numero-ordf(6).

      ***---
       CALCOLA-QTA.
           move 0 to qta-da-ordinare qta-ordinata.  
           perform varying idx from 1 by 1 
                     until idx = como-mese
              add cpfm-qta-m(idx) to qta-ordinata
           end-perform.
           perform varying idx from 1 by 1 
                     until idx > como-mese
              add s-cpfm-qta-m(idx) to qta-da-ordinare
           end-perform.
      
      ***---
       CALCOLA-QTA-ORD. 
           if qta-ordinata    > qta-da-ordinare or
              qta-da-ordinare < art-qta-epal
              move 0 to cpfm-qta-m(como-mese)
           else                      
              compute qta-eccesso = 
                      qta-da-ordinare - qta-ordinata
              move 0 to resto
              divide qta-eccesso by art-qta-epal giving ris
                     remainder resto
              if resto > 0
                 add 1 to ris
              end-if
              compute cpfm-qta-m(como-mese) = art-qta-epal * ris        
           end-if.

      ***---
       TROVA-PROGRESSIVO-ATTIVO.
           read progmag no lock invalid continue end-read.
           |Se trovo subito il progressivo attivo non devo fare nulla
           if prg-bloccato or
              prg-disattivo
              |Me la salvo comunque
              move prg-chiave to s-prg-chiave
              initialize prg-chiave replacing numeric data by zeroes
                                         alphanumeric data by spaces
              move rof-cod-articolo  to prg-cod-articolo
              move rof-cod-magazzino to prg-cod-magazzino
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-articolo  not = rof-cod-articolo or
                          prg-cod-magazzino not = rof-cod-magazzino
                          exit perform
                       end-if
                       if prg-attivo
                          |Al primo progressivo attivo che trovo
                          |memorizzo la chiave
                          move prg-chiave to s-prg-chiave
                          exit perform
                       end-if
                    end-perform
              end-start
              |mi riposiziono sul record salvato
              move s-prg-chiave to prg-chiave
              read progmag no lock
           end-if.

      ***---
       QTA-BANCALE-EPAL.
           move cpfm-tipo-imballo to imq-codice.
           read timbalqta no lock invalid continue end-read.
           move imq-tipo          to imb-codice.
           read timballi  no lock invalid continue end-read.
           move imq-qta-imb       to rof-qta-imballi.
      
           move 0 to resto.

           if art-qta-epal = 0
              move art-qta-std to art-qta-epal
           end-if.

           if rof-qta-ord < rof-qta-imballi
              move rof-qta-imballi to rof-qta-ord
           else
              divide rof-qta-ord by rof-qta-imballi giving ris
                                                 remainder resto
              if resto not = 0
                 compute rof-qta-ord = rof-qta-imballi * ( ris + 1 )
              end-if
           end-if.

           |11/03: Gli articoli "su richiesta" devono 
           |arrotondare solo gli imballi
           move art-scorta to sco-codice.
           read tscorte no lock
                invalid continue
            not invalid
                if not sco-immediato-si
                   if art-qta-epal not = 0
                      if rof-qta-ord > art-qta-epal
                         |resto diventa una variabile di comodo
                         move 0 to resto
                         divide rof-qta-ord by art-qta-epal
                                        giving num-bancali
                                      remainder resto
                         if resto not = 0
      *****                      if cpfm-cod-magazzino = "LBX"
                               compute resto = 
                                       rof-qta-ord - 
                                     ( art-qta-epal * num-bancali )
                   
                               compute ris2 =
                                     ( art-qta-epal * 
                                       tge-perce-arrot-bancale / 100 )
                               compute ris = art-qta-epal - ris2
                   
                               if resto >= ris
                                  add 1 to num-bancali
                               end-if
      *****                      else
      *****                         add 1 to num-bancali
      *****             
      *****                      end-if
                   
                            compute rof-qta-ord  =
                                    art-qta-epal * num-bancali
                         end-if
                      else
      *****                   if cpfm-cod-magazzino = "LBX"
                            compute ris = art-qta-epal -
                                  ( art-qta-epal * 
                                    tge-perce-arrot-bancale / 100 )
                   
                            if rof-qta-ord >= ris
                               move art-qta-epal to rof-qta-ord
                            end-if
      *****                   else
      *****                      move art-qta-epal to rof-qta-ord
      *****                   end-if
                      end-if
                      
                   end-if
                end-if
           end-read.       

      ***---
       QTA-SCORTA-NON-PROGRAMMATA.
           perform CHECK-BANCALE.
           perform CHECK-MOQ.
           perform CHECK-IMBALLO.

      ***---
       CHECK-BANCALE.
           |CHECK-BANCALE

           if art-qta-epal = 0
              move art-qta-std to art-qta-epal
           end-if.

           |11/03: Gli articoli "su richiesta" devono 
           |arrotondare solo gli imballi
           move art-scorta to sco-codice.
           read tscorte no lock
                invalid continue
            not invalid
                if not sco-immediato-si
                   if art-qta-epal not = 0
                      if rof-qta-ord > art-qta-epal
                         |resto diventa una variabile di comodo
                         move 0 to resto
                         divide rof-qta-ord by art-qta-epal
                                               giving num-bancali
                                            remainder resto
                         if resto not = 0
      *****                      if cpfm-cod-magazzino = "LBX"
                               compute resto = 
                                       rof-qta-ord - 
                                     ( art-qta-epal * num-bancali )
                   
                               compute ris2 =
                                     ( art-qta-epal * 
                                       tge-perce-arrot-bancale / 100 )
                               compute ris = art-qta-epal - ris2
                   
                               if resto >= ris
                                  add 1 to num-bancali
                               end-if
      *****                      else
      *****                         add 1 to num-bancali
      *****             
      *****                      end-if
                   
                            compute rof-qta-ord  =
                                    art-qta-epal * num-bancali
                         end-if
                      else
      *****                   if cpfm-cod-magazzino = "LBX"
                            compute ris = art-qta-epal -
                                  ( art-qta-epal * 
                                    tge-perce-arrot-bancale / 100 )
                   
                            if rof-qta-ord >= ris
                               move art-qta-epal to rof-qta-ord
                            end-if
      *****                   else
      *****                      move art-qta-epal to rof-qta-ord
      *****                   end-if
                      end-if
                      
                   end-if
                end-if
           end-read.    
   
      ***---
       CHECK-MOQ.
           if LinkAuto = 1
              if rof-qta-ord < art-moq
                 move art-moq to rof-qta-ord
              end-if
           end-if.

      ***---
       CHECK-IMBALLO.
           move cpfm-tipo-imballo to imq-codice.
           read timbalqta no lock invalid continue end-read.
           move imq-tipo          to imb-codice.
           read timballi  no lock invalid continue end-read.
           move imq-qta-imb     to rof-qta-imballi.

           move 0 to resto.
           if rof-qta-ord < rof-qta-imballi
              move rof-qta-imballi to rof-qta-ord
           else
              divide rof-qta-ord by rof-qta-imballi giving ris
                                                 remainder resto
              if resto not = 0
                 compute rof-qta-ord = rof-qta-imballi * ( ris + 1 )
              end-if
           end-if.

      ***---
       SCRIVI-TMP-CONTROLLO.
           move tof-chiave        to tta-chiave.
           move tof-data-consegna to tta-data-cons.
           move tof-cod-forn      to tta-fornitore.
           move tof-destino       to tta-destino.
           write tta-rec.
           set EseguiCheck to true.

      ***---
       CONTROLLA-ORDINI.     
           |Cancello le testate create ma senza righe: questo succede
           |quando incontro delle qta in programmazione che inizilamente
           |hanno delle qta, che vengono poi ripulite dalle nuove
           |logiche di ordini a bancale
           if primo-numero     not = 0         and 
              primo-numero <= con-num-ord-forn and
              con-num-ord-forn not = 0       
              move 0 to LinkFirst LinkLast
              perform varying primo-numero from primo-numero by 1
                        until primo-numero > con-num-ord-forn

                 move low-value    to rof-rec
                 move tge-anno     to rof-anno   tof-anno
                 move primo-numero to rof-numero tof-numero
                 start rordforn key >= rof-chiave
                       invalid 
                       delete tordforn record 
                              invalid continue 
                       end-delete
                   not invalid
                       read rordforn next
                       if rof-anno   not = tof-anno or
                          rof-numero not = tof-numero
                          delete tordforn record 
                                 invalid continue 
                          end-delete
                          move tof-chiave to tta-chiave
                          delete tmp-tof-auto record 
                                 invalid continue 
                          end-delete
                       else
                          if LinkFirst = 0
                             move primo-numero to LinkFirst
                          end-if
                          move primo-numero    to LinkLast
                       end-if
                 end-start
              end-perform
           end-if.

           set trovato to false.
           move low-value to tta-rec.
           start tmp-tof-auto key >= tta-chiave.
           perform until 1 = 2
              read tmp-tof-auto next at end exit perform end-read
              move low-value to rof-rec
              move tta-chiave to rof-chiave-testa
              start rordforn key >= rof-chiave
                    invalid 
                    delete tmp-tof-auto record end-delete
                not invalid
                    perform until 1 = 2
                       read rordforn next at end exit perform end-read
                       if rof-chiave-testa not = tta-chiave
                          exit perform
                       end-if
                       move rof-cod-listino to tlis-codice
                       read tlistini no lock
                       if tlis-fornitore not = tta-fornitore or
                          tlis-destino   not = tta-destino
                          delete rordforn record
                          move tta-chiave to tof-chiave
                          read tordforn no lock
                          subtract rof-qta-ord from tof-pz-tot
                          rewrite tof-rec
                          move rof-rec        to tra-rec   
                          move tof-dati       to tra-tof-dati
                          move tlis-fornitore to tra-cod-forn
                          move tlis-destino   to tra-destino
                          write tra-rec
                          set trovato to true
                       end-if
                    end-perform
              end-start
           end-perform.
           if trovato
              move low-value to tra-rec
              start tmp-rof-auto key >= tra-chiave end-start
              perform until 1 = 2
                 read tmp-rof-auto next at end exit perform end-read
                 move tra-cod-forn      to tta-fornitore
                 move tra-destino       to tta-destino
                 move tra-data-consegna to tta-data-cons
                 read tmp-tof-auto key tta-k1
                      invalid
                      add 1 to LinkLast con-num-ord-forn
                      rewrite con-rec
                      move tge-anno          to tof-anno
                      move con-num-ord-forn  to tof-numero
                      move tra-tof-dati      to tof-dati

                      move tra-cod-listino  to tlis-codice
                      read tlistini no lock invalid continue end-read
                      if tlis-trasp-f-escluso
                         set tof-franco-part-si to true
                      end-if
                      move tlis-fornitore   to tof-cod-forn
                      move tlis-destino     to tof-destino

                      move tof-cod-forn  to cpfm-fornitore
                      move tof-destino   to cpfm-destino
                      perform READ-FORNITORI-DESTINIF
                      move desf-referente-ord    to tof-referente
                      move desf-tel-dir-ref-ord  to tof-tel-dir
                      move desf-mail-ref-ord     to tof-email
                      move desf-fax              to tof-fax
                      move rof-qta-ord           to tof-pz-tot

                      write tof-rec

                      move tra-cod-listino to cpfm-listino
                      perform SCRIVI-TMP-CONTROLLO

                      move tra-rec    to rof-rec
                      move tof-chiave to rof-chiave-testa
                      move 1          to rof-riga
                      write rof-rec invalid continue end-write

                  not invalid      
                      move low-value to rof-rec
                      move tta-chiave to rof-chiave-testa
                      start rordforn key >= rof-chiave
                            invalid continue
                        not invalid
                            set trovato to false
                            move 0 to save-riga
                            perform until 1 = 2
                               read rordforn next 
                                    at end exit perform 
                               end-read
                               if rof-chiave-testa not = tta-chiave
                                  exit perform
                               end-if
                               move rof-riga to save-riga
                               if rof-cod-articolo = tra-cod-articolo
                                  display message 
                                  "ARTICOLO " tra-cod-articolo
                           x"0d0a""ORDINE NON GENERATO!!!"
                           x"0d0a""CONTATTARE ASSISTENZA!!!"
                                            title "ERRORE CREA-ORDFOR"
                                             icon 2
                                  set trovato to true
                                  exit perform
                               end-if
                            end-perform
                      end-start
                      if not trovato
                         move tta-chiave to tof-chiave
                         read tordforn
                         add tra-qta-ord to tof-pz-tot
                         rewrite tof-rec
                         move tra-rec to rof-rec
                         add 1 to save-riga giving rof-riga
                         move tta-chiave to rof-chiave-testa
                         write rof-rec
                         delete tmp-rof-auto record
                      end-if
                 end-read
              end-perform
           end-if.

           |ULTIMO CONTROLLO DI CONGRUENZA CON MESSAGGIO
           move low-value to tof-rec.
           move tge-anno  to tof-anno.
           move LinkFirst to tof-numero.
           start tordforn key >= tof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordforn next at end exit perform end-read
                    if tof-anno not = tge-anno or
                       tof-numero > LinkLast
                       exit perform
                    end-if
                    move low-value to rof-rec
                    move tof-chiave to rof-chiave-testa
                    start rordforn key >= rof-chiave
                          invalid 
                          display message 
                                  "ATTENZIONE!!!"
                           x"0d0a""ORDINE N. " tof-numero
                           x"0d0a""SENZA ARTICOLI DI RIFERIMENTO!!!!"
                           x"0d0a""CONTATTARE ASSISTENZA!!!"
                                     title "ERRORE"
                                      icon 2
                      not invalid
                          perform until 1 = 2
                             read rordforn next 
                                  at end exit perform 
                             end-read
                             if rof-chiave-testa not = tof-chiave
                                exit perform
                             end-if
                             move rof-cod-listino to tlis-codice
                             read tlistini no lock
                             if tlis-fornitore not = tof-cod-forn or
                                tlis-destino   not = tof-destino
                                display message 
                                  "ATTENZIONE!!!"
                           x"0d0a""CONTROLLARE ORDINE N. " tof-numero
                           x"0d0a""ARTICOLO " rof-cod-articolo
                                  " CON LISTINO ERRATO!!!"
                           x"0d0a""CONTATTARE ASSISTENZA!!!"
                                     title "ERRORE"
                                      icon 2
                                exit perform
                             end-if
                          end-perform
                    end-start      
                 end-perform
           end-start.

           |ULTIMO CONTROLLO PER:
           |- PROGRESSIVI BLOCCATI
           |- DATA DI CONESEGNA ORDINI IN PROGRAMMAZIONE
           move low-value to tof-rec.
           move tge-anno  to tof-anno.
           move LinkFirst to tof-numero.
           start tordforn key >= tof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tordforn next at end exit perform end-read
                    if tof-anno not = tge-anno or
                       tof-numero > LinkLast
                       exit perform
                    end-if
                    if tof-programmazione-si
                       if tof-mese-rif not = 1
                          move tof-mese-rif to mese-rif
                          perform CALCOLA-DATA-CONSEGNA
                          if data-consegna not = tof-data-consegna
                             move data-consegna to tof-data-consegna
                             rewrite tof-rec
                          end-if
                       end-if
                    end-if
                    move low-value to rof-rec
                    move tof-chiave to rof-chiave-testa
                    start rordforn key >= rof-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read rordforn next 
                                  at end exit perform 
                             end-read
                             if rof-chiave-testa not = tof-chiave
                                exit perform
                             end-if
                             move rof-prg-chiave to prg-chiave
                             read progmag no lock
                                  invalid continue
                              not invalid
                                  if prg-bloccato or prg-disattivo
                                     perform TROVA-PROGRESSIVO-ATTIVO
                                     move s-prg-chiave to rof-prg-chiave
                                     move rof-tipo-imballo 
                                       to rof-imb-ordinato
                                     rewrite rof-rec
                                  end-if
                             end-read
                          end-perform
                    end-start      
                 end-perform
           end-start.                        
           accept tnf-data from century-date.
           accept tnf-ora  from time.      
           move LinkFirst  to tnf-da-num.
           move LinkLast   to tnf-a-num.
           write tnf-rec.        

      ***---
       CLOSE-FILES.
           close tcontat coperfab  tordforn timballi tscorte clienti
                 rordforn tparamge ordfor2 tcaumag timbalqta destinif
                 nlistini nordforn progmag articoli coperfab-mag 
                 rlistini impforn timposte tpiombo tlistini distinteb
                 tmp-tof-auto tmp-rof-auto param tnumordf.

           delete file coperfab-mag tmp-tof-auto tmp-rof-auto.

           if linkAuto = 1
              close genlog
           end-if.

      ***---
       SCRIVI-RIGA-LOG.
           if linkAuto not = 1 exit paragraph end-if.
           inspect como-riga replacing trailing spaces by low-value.
           perform SETTA-INIZIO-RIGA.
           initialize gl-riga.
           string r-inizio  delimited size
                  como-riga delimited low-value
             into gl-riga
           end-string.
           write gl-riga. 

      ***---
       EXIT-PGM.
           display "                                           "
                   upon link-handle at column 15,00 line 06,00.

           if primo-numero     not = 0         and 
              primo-numero <= con-num-ord-forn and
              con-num-ord-forn not = 0
              perform varying primo-numero from primo-numero by 1
                        until primo-numero > con-num-ord-forn
                 move tge-anno     to tof-anno
                 move primo-numero to tof-numero
                 call   "ordf-sol" using tof-chiave
                 cancel "ordf-sol"
              end-perform
           end-if.

           goback.                

      ***---
       PARAGRAFO-COPY.
           copy "imposte-fornitore.cpy".
           copy "addizionale-piombo-fornitore.cpy".
           copy "trova-parametro.cpy".
           copy "prz-finito-forn.cpy".
           copy "costo-medio.cpy".
           copy "setta-inizio-riga.cpy".

      ***--- DUMMY
       CALCOLA-TRASPORTO.
       RECUPERO-ANAGRAFICA.
