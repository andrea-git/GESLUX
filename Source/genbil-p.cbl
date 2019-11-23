       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      genbil-p.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "ttipocli.sl".
           copy "tgrupgdo.sl".
           copy "tmarche.sl".
           copy "tcaumag.sl".
           copy "articoli.sl".
           copy "clienti.sl".
           copy "tmovmag.sl". 
           copy "rmovmag.sl".
           copy "provvig.sl".
           copy "tmp-bil.sl".
           copy "lineseq.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "ttipocli.fd".
           copy "tgrupgdo.fd".
           copy "tmarche.fd".
           copy "tcaumag.fd".
           copy "articoli.fd".
           copy "clienti.fd".
           copy "tmovmag.fd".
           copy "rmovmag.fd".
           copy "provvig.fd".
           copy "tmp-bil.fd".
           copy "lineseq.fd".

       WORKING-STORAGE SECTION.
           COPY "acugui.def".
           copy "link-geslock.def".
           copy "comune.def".
           copy "common-excel.def".

      *    COSTANTI
       78  titolo    value "Genera File per bilanci gestionali".
       78  78-clear              value 
           "                                                          ".

      *    FILE STATUS
       77  status-ttipocli  pic xx.
       77  status-tgrupgdo  pic xx.
       77  status-tmarche   pic xx.
       77  status-tcaumag   pic xx.
       77  status-articoli  pic xx.
       77  status-clienti   pic xx.
       77  status-tmovmag   pic xx.  
       77  status-rmovmag   pic xx.
       77  status-provvig   pic xx.
       77  status-lineseq   pic xx.
       77  status-tmp-bil   pic xx.

       77  path-tmp-bil     pic x(256).
       77  wstampa          pic x(256).

      *    FLAGS
       01  filler           pic 9.
         88 prima-volta     value 1, false 0.

       01  filler           pic 9.
         88 trovata-provv   value 1, false 0.

       01  filler           pic 9.
         88 StessoAnno      value 1, false 0.

      *    VARIABILI
       77  como-data        pic 9(8).
       77  como-ora         pic 9(8).
       77  kg-edit          pic ----.---.--9,999.
       77  provv-edit       pic ----.---.---.--9,99.
       77  fatt-edit        pic ----.---.---.--9,99.
       77  user-codi        pic x(10).

       77  mese-esteso-from pic x(10).
       77  mese-esteso-to   pic x(10).
       77  mese-anno-from   pic x(20).
       77  mese-anno-to     pic x(20).
       77  tipo-file        pic x(20).
       77  como-valore      pic s9(12)v99.
       77  como-peso        pic s9(12)v999.

       77  counter          pic 9(10).
       77  counter2         pic 9(10).
       77  counter-edit     pic z(10).

       LINKAGE SECTION.
       copy "link-genbil.def".

      ******************************************************************
       PROCEDURE DIVISION using genbil-linkage.

       DECLARATIVES.
       ARTICOLI-ERR SECTION.
           use after error procedure on articoli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-articoli
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File degli articoli [ARTICOLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [ARTICOLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[ARTICOLI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       TGRUPGDO-ERR SECTION.
           use after error procedure on tgrupgdo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tgrupgdo
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File Gruppi GDO [TGRUPGDO] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TGRUPGDO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TGRUPGDO] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       TTIPOCLI-ERR SECTION.
           use after error procedure on ttipocli.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-ttipocli
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File tipologie clienti [TTIPOCLI] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TTIPOCLI] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TTIPOCLI] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       TMARCHE-ERR SECTION.
           use after error procedure on tmarche.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmarche
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle marche [TMARCHE] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TMARCHE] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TMARCHE] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       TCAUMAG-ERR SECTION.
           use after error procedure on tcaumag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tcaumag
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""Tabella Causali [TCAUMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [TCAUMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TCAUMAG] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       TMOVMAG-ERR SECTION.
           use after error procedure on tmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tmovmag 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle testate [TMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
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
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       RMOVMAG-ERR SECTION.
           use after error procedure on rmovmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rmovmag 
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle righe [RMOVMAG] inesistente"
                          title titolo
                           icon 2
                set errori to true
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
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       CLIENTI-ERR SECTION.
           use after error procedure on clienti.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-clienti
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File dei clienti [CLIENTI] inesistente"
                          title titolo
                           icon 2
                set errori to true
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
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       PROVVIG-ERR SECTION.
           use after error procedure on provvig.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-provvig
           when "35"
                display message box        "Impossibile procedere."
                  x"0d0a""File delle provvigioni [PROVVIG] inesistente"
                          title titolo
                           icon 2
                set errori to true
           when "39"
                set errori to true
                display message "File [PROVVIG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROVVIG] Indexed file corrupt!"
                          title titolo
                           icon 3 
           when "93"
           when "99" set RecLocked to true
           end-evaluate.

       TMP-BIL-ERR SECTION.
           use after error procedure on tmp-bil.
           set tutto-ok  to true.
           evaluate status-tmp-bil
           when "35"
                display message "File TMP not found!"
                          title titolo
                           icon 3
                set errori to true
           when "39"
                set errori to true
                display message "File TMP mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "TMP Indexed file corrupt!"
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
                     open output tmp-bil
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
                display message "File not found!"
                          title titolo
                           icon 3
                set errori to true
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
                string   "Chiudere file Excel "   delimited size
                         tipo-file                delimited low-value
                         "!"                      delimited size
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
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           move 0 to counter counter2.
           evaluate genbil-mese-from
           when  1  move "Gennaio"   to mese-esteso-from
           when  2  move "Febbraio"  to mese-esteso-from
           when  3  move "Marzo"     to mese-esteso-from
           when  4  move "Aprile"    to mese-esteso-from
           when  5  move "Maggio"    to mese-esteso-from
           when  6  move "Giugno"    to mese-esteso-from
           when  7  move "Luglio"    to mese-esteso-from
           when  8  move "Agosto"    to mese-esteso-from
           when  9  move "Settembre" to mese-esteso-from
           when 10  move "Ottobre"   to mese-esteso-from
           when 11  move "Novembre"  to mese-esteso-from
           when 12  move "Dicembre"  to mese-esteso-from
           end-evaluate.
           inspect mese-esteso-from replacing trailing spaces 
                                                    by low-value.
           string "* "              delimited size
                  mese-esteso-from  delimited low-value
                  " "               delimited size
                  genbil-anno-from  delimited size
                  " *"              delimited size
                  into mese-anno-from
           end-string.

           evaluate genbil-mese-to
           when  1  move "Gennaio"   to mese-esteso-to
           when  2  move "Febbraio"  to mese-esteso-to
           when  3  move "Marzo"     to mese-esteso-to
           when  4  move "Aprile"    to mese-esteso-to
           when  5  move "Maggio"    to mese-esteso-to
           when  6  move "Giugno"    to mese-esteso-to
           when  7  move "Luglio"    to mese-esteso-to
           when  8  move "Agosto"    to mese-esteso-to
           when  9  move "Settembre" to mese-esteso-to
           when 10  move "Ottobre"   to mese-esteso-to
           when 11  move "Novembre"  to mese-esteso-to
           when 12  move "Dicembre"  to mese-esteso-to
           end-evaluate.
           inspect mese-esteso-to replacing trailing spaces 
                                                    by low-value.
           string "* "            delimited size
                  mese-esteso-to  delimited low-value
                  " "             delimited size
                  genbil-anno-to  delimited size
                  " *"            delimited size
                  into mese-anno-to
           end-string.

           set RecLocked     to false.
           set trovata-provv to false.
           set tutto-ok      to true.
           set prima-volta   to true.
           set errori        to true.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize wstampa.
           set tutto-ok         to true.
           set trovato          to false.
           set prima-volta      to true.
           move genbil-user to user-codi.

           inspect user-codi replacing trailing spaces by low-value.

           if genbil-anno-from = genbil-anno-to
              set StessoAnno to true
           else
              set StessoAnno to false
           end-if.

           accept  path-tmp-bil from environment "PATH-ST".
           inspect path-tmp-bil replacing trailing spaces by low-value.
           string  path-tmp-bil delimited by low-value
                   "genbil"     delimited by size
                   "_"          delimited by size
                   como-data    delimited by size
                   "_"          delimited by size
                   como-ora     delimited by size
                   ".tmp"       delimited by size
                   into path-tmp-bil
           end-string.

      ***---
       OPEN-FILES.
           perform OPEN-OUTPUT-TMP-BIL.
           if tutto-ok
              open input tmovmag rmovmag provvig clienti articoli
                         tcaumag tmarche ttipocli tgrupgdo
              if errori
                 close       tmp-bil
                 delete file tmp-bil
                 close       lineseq
                 delete file lineseq
              end-if
           end-if.

      ***---
       OPEN-OUTPUT-TMP-BIL.
           open output tmp-bil.
           if tutto-ok
              close    tmp-bil
              open i-o tmp-bil
           end-if.

      ***---
       ELABORAZIONE.
           move low-value        to tmo-rec.
           move 01               to tmo-data-movim(7:2).
           move genbil-mese-from to tmo-data-movim(5:2).
           move genbil-anno-from to tmo-data-movim(1:4).
           start tmovmag key is >= k-data
                 invalid set errori to true
           end-start.
           if tutto-ok
              perform until 1 = 2
                 read tmovmag next at end exit perform end-read 

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    if counter = 100
                       display "ELABORAZIONE"
                          upon genbil-handle at column 02
                                                  line 06
                    end-if
                    move counter to counter-edit
                    display counter-edit
                       upon genbil-handle at column 17
                                               line 06
                    move 0 to counter2
                 end-if

                 if StessoAnno
                    if tmo-data-movim(5:2) > genbil-mese-to
                       exit perform
                    end-if
                 else
                    if tmo-data-movim(5:2) > genbil-mese-to and
                       tmo-data-movim(1:4) > genbil-anno-to
                       exit perform
                    end-if
                 end-if

                 if tmo-cliente
                    move tmo-causale to tca-codice
                    read tcaumag no  lock
                         invalid continue
                     not invalid
                         if tca-cliente 
LUBEXX                      if tca-si-stat
                               set  cli-tipo-C     to true
                               move tmo-cod-clifor to cli-codice
                               read clienti no lock
                                    invalid continue
                                not invalid
                                    perform LOOP-RIGHE
                               end-read
LUBEXX                      end-if
                         end-if
                    end-read
                 end-if
              end-perform
           end-if.

           if not trovato
              display message "Nessun movimento trovato"
                        title titolo
                         icon 2
           else
              
              move low-value        to pvv-rec
              move 1                to pvv-data-fat(7:2)
              move genbil-mese-from to pvv-data-fat(5:2)
              move genbil-anno-from to pvv-data-fat(1:4)
              start provvig key is >= k-data-fat
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read provvig next at end exit perform end-read
                       if StessoAnno
                          if pvv-data-fat(5:2) > genbil-mese-to
                             exit perform
                          end-if
                       else
                          if pvv-data-fat(5:2) > genbil-mese-to and
                             pvv-data-fat(1:4) > genbil-anno-to
                             exit perform
                          end-if
                       end-if
                       set  cli-tipo-C   to true
                       move pvv-cliente  to cli-codice
                       read clienti no lock
                            invalid continue
                        not invalid
                            move pvv-articolo to art-codice
                            read articoli no lock
                                 invalid continue
                             not invalid
                                 perform AGGIORNA-PROVVIGIONE
                            end-read
                       end-read
                    end-perform
              end-start
              perform ACCETTA-SEPARATORE
              perform CREA-CSV-TRASPORTATI
              perform CALL-EXCEL

              perform CREA-CSV-FATTURATI
              perform CALL-EXCEL

              if trovata-provv
                 perform CREA-CSV-PROVVIGIONI
                 perform CALL-EXCEL
              end-if

           end-if.

      ***---
       AGGIORNA-PROVVIGIONE.
           move cli-tipo           to tbil-tipo-cli.
           move cli-gdo            to tbil-gdo.
           move art-marca-prodotto to tbil-marca.
           read tmp-bil 
                invalid continue
            not invalid
                add pvv-val-provvig to tbil-provv
                if tbil-provv not = 0
                   set trovata-provv to true
                   rewrite tbil-rec invalid continue end-rewrite
                end-if
           end-read.

      ***---
       CREA-CSV-TRASPORTATI.
           move 0 to counter counter2.
           set prima-volta to true.
           initialize wstampa.
           accept  wstampa from environment "PATH-ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa delimited by low-value
                   "KG-TRASPORTATI"  delimited size
                   "_"               delimited size
                   user-codi         delimited low-value
                   ".csv"            delimited size
                   into wstampa
           end-string.
           move "TRASPORTATI" to tipo-file.
           inspect tipo-file  replacing trailing spaces by low-value.
           open output lineseq.
           if tutto-ok
              move low-value to tbil-rec
              start tmp-bil key is >= tbil-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2

                       add 1 to counter
                       add 1 to counter2
                       if counter2 = 50
                          if counter = 50
                             display "TRASPORTATI "
                                upon genbil-handle at column 02
                                                        line 06
                          end-if
                          move counter to counter-edit
                          display counter-edit
                             upon genbil-handle at column 17
                                                     line 06
                          move 0 to counter2
                       end-if

                       read tmp-bil next at end exit perform end-read
                       initialize line-riga tcl-rec mar-rec
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       if prima-volta
                          set prima-volta to false  
                          string "TIPO CLIENTE"      delimited size
                                 separatore          delimited size
                                 "DESCRIZIONE TIPO"  delimited size
                                 separatore          delimited size
                                 "GRUPPO GDO"        delimited size
                                 separatore          delimited size
                                 "MARCA"             delimited size
                                 separatore          delimited size
                                 "DESCRIZIONE MARCA" delimited size
                                 separatore          delimited size
                                 "KG. VENDUTI"       delimited size
                                 separatore          delimited size
                                 "DA MESE"           delimited size
                                 separatore          delimited size
                                 "A MESE"            delimited size
                                 into line-riga
                          end-string
                          write line-riga
                       end-if

                       move tbil-tipo-cli to tcl-codice
                       read ttipocli no lock invalid continue end-read

                       move tbil-marca to mar-codice
                       read tmarche no lock invalid continue end-read

                       move tbil-kg-vend to kg-edit
                       initialize line-riga
                       string tbil-tipo-cli    delimited size
                              separatore       delimited size
                              tcl-descrizione  delimited size
                              separatore       delimited size
                              tbil-gdo         delimited size
                              separatore       delimited size
                              tbil-marca       delimited size
                              separatore       delimited size
                              mar-descrizione  delimited size
                              separatore       delimited size
                              kg-edit          delimited size
                              separatore       delimited size
                              mese-anno-from   delimited size
                              separatore       delimited size
                              mese-anno-to     delimited size
                              into line-riga
                       end-string
                       write line-riga
                    end-perform
              end-start
              close lineseq
           end-if.

      ***---
       CREA-CSV-FATTURATI.
           move 0 to counter counter2.
           set prima-volta to true.
           initialize wstampa.
           accept  wstampa from environment "PATH-ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa delimited by low-value
                   "FATTURATI"       delimited size
                   "_"               delimited size
                   user-codi         delimited low-value
                   ".csv"            delimited size
                   into wstampa
           end-string.
           move "FATTURATI" to tipo-file.
           inspect tipo-file  replacing trailing spaces by low-value.
           open output lineseq.
           if tutto-ok
              move low-value to tbil-rec
              start tmp-bil key is >= tbil-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2

                       add 1 to counter
                       add 1 to counter2
                       if counter2 = 50
                          if counter = 50
                             display "FATTURATI   "
                                upon genbil-handle at column 02
                                                        line 06
                          end-if
                          move counter to counter-edit
                          display counter-edit
                             upon genbil-handle at column 02
                                                     line 17
                          move 0 to counter2
                       end-if

                       read tmp-bil next at end exit perform end-read
                       initialize line-riga tcl-rec mar-rec gdo-rec
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       if prima-volta
                          set prima-volta to false
                          string "TIPO CLIENTE"      delimited size
                                 separatore          delimited size
                                 "DESCRIZIONE TIPO"  delimited size
                                 separatore          delimited size
                                 "GRUPPO GDO"        delimited size
                                 separatore          delimited size
                                 "DESCRIZIONE GDO"   delimited size
                                 separatore          delimited size
                                 "MARCA"             delimited size
                                 separatore          delimited size
                                 "DESCRIZIONE MARCA" delimited size
                                 separatore          delimited size
                                 "KG. VENDUTI"       delimited size
                                 separatore          delimited size
                                 "FATTURATO"         delimited size
                                 separatore          delimited size
                                 "DA MESE"           delimited size
                                 separatore          delimited size
                                 "A MESE"            delimited size
                                 into line-riga
                          end-string
                          write line-riga
                       end-if

                       move tbil-tipo-cli to tcl-codice
                       read ttipocli no lock invalid continue end-read

                       move tbil-gdo   to gdo-codice
                       read tgrupgdo no lock invalid continue end-read

                       move tbil-marca to mar-codice
                       read tmarche no lock invalid continue end-read

                       move tbil-kg-vend   to kg-edit
                       move tbil-fatturato to fatt-edit
                       initialize line-riga
                       string tbil-tipo-cli    delimited size
                              separatore       delimited size
                              tcl-descrizione  delimited size
                              separatore       delimited size
                              tbil-gdo         delimited size
                              separatore       delimited size
                              gdo-intestazione delimited size
                              separatore       delimited size
                              tbil-marca       delimited size
                              separatore       delimited size
                              mar-descrizione  delimited size
                              separatore       delimited size
                              kg-edit          delimited size
                              separatore       delimited size
                              fatt-edit        delimited size
                              separatore       delimited size
                              mese-anno-from   delimited size
                              separatore       delimited size
                              mese-anno-to     delimited size
                              into line-riga
                       end-string
                       write line-riga
                    end-perform
              end-start
              close lineseq
           end-if.

      ***---
       CREA-CSV-PROVVIGIONI.
           move 0 to counter counter2.
           set prima-volta to true.
           initialize wstampa.
           accept  wstampa from environment "PATH-ST".
           inspect wstampa replacing trailing spaces by low-value.
           string  wstampa delimited by low-value
                   "PROVVIGIONI"     delimited size
                   "_"               delimited size
                   user-codi         delimited low-value
                   ".csv"            delimited size
                   into wstampa
           end-string.
           move "PROVVIGIONI" to tipo-file.
           inspect tipo-file  replacing trailing spaces by low-value.
           open output lineseq.
           if tutto-ok
              move low-value to tbil-rec
              start tmp-bil key is >= tbil-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2

                       add 1 to counter
                       add 1 to counter2
                       if counter2 = 50
                          if counter = 50
                             display "PROVVIGIONI "
                                upon genbil-handle at column 02
                                                        line 06
                          end-if
                          move counter to counter-edit
                          display counter-edit
                             upon genbil-handle at column 17
                                                     line 06
                          move 0 to counter2
                       end-if

                       read tmp-bil next at end exit perform end-read
                       if tbil-provv not = 0
                          initialize line-riga tcl-rec mar-rec
                                     replacing numeric data by zeroes
                                          alphanumeric data by spaces
                          if prima-volta
                             set prima-volta to false
                             string "TIPO CLIENTE"      delimited size
                                    separatore          delimited size
                                    "DESCRIZIONE TIPO"  delimited size
                                    separatore          delimited size
                                    "MARCA"             delimited size
                                    separatore          delimited size
                                    "DESCRIZIONE MARCA" delimited size
                                    separatore          delimited size
                                    "KG. VENDUTI"       delimited size
                                    separatore          delimited size
                                    "PROVVIGIONI"       delimited size
                                    separatore          delimited size
                                    "DA MESE"           delimited size
                                    separatore          delimited size
                                    "A MESE"            delimited size
                                    into line-riga
                             end-string
                             write line-riga
                          end-if

                          move tbil-tipo-cli to tcl-codice
                          read ttipocli no lock 
                               invalid continue 
                          end-read

                          move tbil-marca to mar-codice
                          read tmarche no lock invalid continue end-read

                          move tbil-kg-vend to kg-edit
                          move tbil-provv   to provv-edit
                          initialize line-riga
                          string tbil-tipo-cli    delimited size
                                 separatore       delimited size
                                 tcl-descrizione  delimited size
                                 separatore       delimited size
                                 tbil-marca       delimited size
                                 separatore       delimited size
                                 mar-descrizione  delimited size
                                 separatore       delimited size
                                 kg-edit          delimited size
                                 separatore       delimited size
                                 provv-edit       delimited size
                                 separatore       delimited size
                                 mese-anno-from   delimited size
                                 separatore       delimited size
                                 mese-anno-to     delimited size
                                 into line-riga
                          end-string
                          write line-riga
                       end-if
                    end-perform
              end-start
              close lineseq
           end-if.

      ***---
       LOOP-RIGHE.
           move low-value    to    rmo-rec.
           move tmo-anno     to    rmo-anno.
           move tmo-numero   to    rmo-movim.
           start rmovmag key is >= rmo-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-anno  not = tmo-anno or
                       rmo-movim not = tmo-numero
                       exit perform
                    end-if
                    move rmo-articolo to art-codice
                    read articoli no  lock
                         invalid  continue
                     not invalid
                         perform COMPONI-TMP-BIL
                    end-read
                 end-perform
           end-start.

      ***---
       COMPONI-TMP-BIL.
           initialize tbil-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces.
           move cli-tipo            to tbil-tipo-cli.
           move art-marca-prodotto  to tbil-marca.
           move cli-gdo             to tbil-gdo.
           read tmp-bil no lock invalid continue end-read.

           if rmo-qta not = 0
              compute como-peso   = ( rmo-qta * rmo-peso )
              compute como-valore = ( rmo-qta * 
                         ( rmo-netto + rmo-imp-cons + rmo-coubat ) )
           else
              move 0 to como-peso
              compute como-valore = rmo-netto    +
                                    rmo-imp-cons +
                                    rmo-coubat
           end-if.

           if tca-imponibile-pos

              compute tbil-fatturato =
                      tbil-fatturato +
                      como-valore

              compute tbil-kg-vend =
                      tbil-kg-vend +
                      como-peso

           else

              compute tbil-fatturato =
                      tbil-fatturato -
                      como-valore

              compute tbil-kg-vend =
                      tbil-kg-vend -
                      como-peso

           end-if.

           write tbil-rec invalid rewrite tbil-rec end-write.
           set   trovato  to true.

      ***---
       CLOSE-FILES.
           close tmovmag rmovmag provvig  tmp-bil clienti 
                 articoli tcaumag tmarche ttipocli tgrupgdo.
           delete file tmp-bil.

      ***---
       EXIT-PGM.
           display 78-clear
             upon genbil-handle at column 02
                                     line 06.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "common-excel.cpy".
