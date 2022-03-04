      ***---
       RIEMPI-TMP.
           move 0 to counter counter2.
           move 0 to tot-imp  tot-kg     tot-colimp 
                     tot-cons tot-coubat tot-piombo.
           perform SECCA-TMP.
           set tutto-ok to true.
           set trovato  to false.
           initialize path-tmp-ordini.
           accept  path-tmp-ordini from environment "PATH-ST".
           inspect path-tmp-ordini 
                   replacing trailing spaces by low-value.
           accept como-data from century-date.
           accept como-ora  from time.
           string path-tmp-ordini   delimited low-value
                  "tmp-ordini"      delimited size
                  "_"               delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".tmp"            delimited size
                  into path-tmp-ordini
           end-string.
           open output tmp-ordini.
           set FileOpen to true.
           perform CICLO-LETTURA-ORDINI.
           perform RIEMPI-GRIGLIA.
           move tot-colimp to lab-tot-colimp-buf
           move tot-cons   to lab-tot-cons-buf.
           move tot-coubat to lab-tot-coubat-buf.
           move tot-piombo to lab-tot-piombo-buf.
           move tot-kg     to lab-tot-kg-buf.
           move tot-imp    to lab-tot-imp-buf.
           display lab-tot-kg lab-tot-imp lab-tot-colimp
                   lab-tot-cons lab-tot-coubat lab-tot-piombo.

      ***---
       CICLO-LETTURA-ORDINI.
LUBEXX     move low-value to tor-rec.
LUBEXX     evaluate tipo-ord
LUBEXX     when 1|INEVASI
LUBEXX          move como-data-from to tor-data-creazione
LUBEXX          start tordini key is >= k-data
LUBEXX                invalid set errori to true
LUBEXX          end-start
LUBEXX     when 2|BOLLETTATI
LUBEXX          move como-data-from(1:4) to tor-anno-bolla
LUBEXX          move como-data-from      to tor-data-bolla
LUBEXX          start tordini key is >= k3
LUBEXX                invalid set errori to true
LUBEXX          end-start
LUBEXX     end-evaluate.

LUBEXX*****     move low-value      to tor-chiave.
LUBEXX*****     move como-data-from to tor-data-ordine.
LUBEXX*****     start tordini key is >= k-data
LUBEXX*****           invalid set errori to true
LUBEXX*****     end-start.
      *
           if tutto-ok
              perform until 1 = 2
                 read tordini next at end exit perform end-read
                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon form3-handle at column 22
                                              line 03
                    move 0 to counter2
                 end-if

LUBEXX           evaluate tipo-ord
LUBEXX           when 1 |INEVASI
LUBEXX                if tor-data-creazione > como-data-to
LUBEXX                   exit perform
LUBEXX                end-if
LUBEXX           when 2 |BOLLETTATI
LUBEXX                if tor-data-bolla >     como-data-to
LUBEXX                   exit perform
LUBEXX                end-if
LUBEXX           end-evaluate

LUBEXX*****           if tor-data-ordine > como-data-to 
LUBEXX*****              exit perform 
LUBEXX*****           end-if

                 set record-ok to false
LUBEXX           evaluate tipo-ord
LUBEXX           when 1|INEVASI
LUBEXX                if tor-anno-bolla = 0 and
LUBEXX                   tor-num-bolla  = 0 and
LUBEXX                   tor-data-bolla = 0
LUBEXX                   set record-ok to true
LUBEXX                end-if      

LUBEXX           when 2|BOLLETTATI
LUBEXX                if tor-anno-bolla   not = 0 and
LUBEXX                   tor-num-bolla    not = 0 and
LUBEXX                   tor-data-bolla   not = 0 and
LUBEXX                   tor-anno-fattura     = 0 and
LUBEXX                   tor-num-fattura      = 0 and
LUBEXX                   tor-data-fattura     = 0
LUBEXX                   set record-ok to true
LUBEXX                end-if

LUBEXX           end-evaluate

LUBEXX*****           if tor-anno-fattura = 0 and
LUBEXX*****              tor-data-fattura = 0 and
LUBEXX*****              tor-num-fattura  = 0
LUBEXX*****              evaluate tipo-ord
LUBEXX*****              |nel primo caso è sufficiente che non siano
LUBEXX*****              |valorizzati i dati di fatturazione che vengono
LUBEXX*****              |controllati in precedenza
LUBEXX*****              when 1 set record-ok to true
LUBEXX*****              when 2 if tor-anno-bolla not = 0 and
LUBEXX*****                        tor-data-bolla not = 0 and
LUBEXX*****                        tor-num-bolla  not = 0
LUBEXX*****                        set record-ok to true
LUBEXX*****                     end-if
LUBEXX*****              when 3 if tor-anno-bolla = 0 and
LUBEXX*****                        tor-data-bolla = 0 and
LUBEXX*****                        tor-num-bolla  = 0
LUBEXX*****                        set record-ok to true
LUBEXX*****                     end-if
LUBEXX*****              end-evaluate
LUBEXX*****           end-if
                 if record-ok
                    if como-vet-codice not = 0
                       if tor-vettore not = como-vet-codice
                          set record-ok to false
                       end-if
                    end-if
                 end-if

                 if record-ok
                    move tor-causale to tca-codice
                    read tcaumag no lock invalid continue end-read
                    if tca-cliente
                       if como-age-codice = 0
                          perform LOOP-RORDINI
                       else
                          if como-age-codice = tor-cod-agente
                             perform LOOP-RORDINI
                          end-if
                       end-if
                    end-if
                 end-if
              end-perform
           end-if.

      ***---
       LOOP-RORDINI.
           set tutto-ok      to true.
           move tor-chiave   to ror-chiave.
           move low-value    to ror-num-riga.
           start rordini key is >= ror-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read rordini next at end exit perform end-read
                 if ror-anno       not = tor-anno or
                    ror-num-ordine not = tor-numero
                    exit perform
                 end-if
                 set record-ok     to true
                 move ror-cod-articolo to art-codice
                 read articoli no lock
                      invalid set record-ok to false
                  not invalid
                      if como-art-codice not = 0 and
                         como-art-codice not = art-codice
                         set record-ok to false
                      end-if
                      if como-mar-codice not = 0 and
                         como-mar-codice not = art-marca-prodotto
                         set record-ok  to false
                      end-if
                 end-read
                 if record-ok
OMAGGI              if ror-qta-omaggi not = 0
OMAGGI                 subtract ror-qta-omaggi from ror-qta
                       initialize tmp-ord-rec 
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
OMAGGI                 perform MOVE-DATI
OMAGGI                 move ror-qta-omaggi to ror-qta
OMAGGI                 move 0 to ror-imponib-merce
OMAGGI                 move 0 to ror-imp-consumo
OMAGGI                 move 0 to ror-imp-cou-cobat
OMAGGI                 move 0 to ror-add-piombo
OMAGGI                 initialize tmp-ord-rec 
OMAGGI                            replacing numeric data by zeroes
OMAGGI                                 alphanumeric data by spaces
OMAGGI                 set tmp-ord-si-omaggi to true
OMAGGI                 perform MOVE-DATI
OMAGGI              else
                       initialize tmp-ord-rec 
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       perform MOVE-DATI 
OMAGGI              end-if
                 end-if
              end-perform
           end-if.

      ***---
       MOVE-DATI.
           set trovato to true.
           move tor-cod-cli       to tmp-ord-codice.
           move tor-prg-destino   to tmp-ord-destino.
           set cli-tipo-C   to true.
           move tor-cod-cli to cli-codice.
           read clienti no lock.
           move cli-localita to tmp-ord-localita.
           if tor-prg-destino not = 0
              move tor-cod-cli     to des-codice
              move tor-prg-destino to des-prog
              read destini no lock
              move des-localita   to tmp-ord-localita
           end-if.
           move tor-cod-agente    to age-codice tmp-ord-age.
           read agenti no lock
                invalid move "** NON TROVATO **" to age-ragsoc-1
           end-read.
           move age-ragsoc-1      to tmp-ord-age-d.
           move cli-tipo          to tmp-ord-cli-tipo tcl-codice.
           read ttipocli no lock invalid move spaces to tcl-descrizione.
           move tcl-descrizione   to tmp-ord-cli-tipo-d.
           move tor-numero        to tmp-ord-evasione.
           move art-descrizione   to tmp-ord-desart.
           move ror-anno          to tmp-ord-anno.
           move ror-num-ordine    to tmp-ord-movim.
           move ror-num-riga      to tmp-ord-riga.
           move tor-causale       to tmp-ord-causale.
           move tor-data-ordine   to tmp-ord-data-movim.
           move ror-qta           to tmp-ord-qta.
           move ror-imponib-merce to tmp-ord-imp.
           move ror-imp-consumo   to tmp-ord-cons.
           move ror-imp-cou-cobat to tmp-ord-coubat.
           move ror-add-piombo    to tmp-ord-add-piombo.  

           move "PA"              to tblpa-codice1.
           move tor-cod-pagamento to tblpa-codice2 tmp-ord-pag.
           read tcodpag invalid continue end-read.
           initialize lab-pag-buf.
           inspect tblpa-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tblpa-descrizione1 delimited by low-value
                   " "                delimited by size
                   tblpa-descrizione2 delimited by size
                   into tmp-ord-pag-d
           end-string.  

           compute tot-colimp = tot-colimp + tmp-ord-imp.
           compute tot-cons   = tot-cons   + tmp-ord-cons.
           compute tot-coubat = tot-coubat + tmp-ord-coubat.
           compute tot-piombo = tot-piombo + tmp-ord-add-piombo.

           compute tmp-ord-imp-calcolato = 
                   ror-imp-consumo + ror-imp-cou-cobat + ror-add-piombo.

           if tca-imponibile-pos
              add ror-peso-utf to tot-kg
              add tmp-ord-imp  to tmp-ord-imp-calcolato
              set tmp-ord-causale-pos to true
           else
              subtract ror-peso-utf from tot-kg
              subtract tmp-ord-imp  from tmp-ord-imp-calcolato
              set tmp-ord-causale-neg to true
           end-if.
           multiply tmp-ord-imp-calcolato by ror-qta
             giving tmp-ord-imp-calcolato.

           move art-marca-prodotto to tmp-ord-marca.
                                                      
           move tor-num-bolla    to tmp-ord-numdoc.
           move ror-cod-articolo to tmp-ord-articolo.
           move tor-data-bolla   to tmp-ord-datadoc.

           add ror-peso-utf  to ror-peso-non-utf 
           giving ror-peso-udm.
           move ror-peso-udm       to tmp-ord-peso.

           write tmp-ord-rec invalid continue end-write.

      ***---
       RIEMPI-GRIGLIA.
           close tmp-ordini.
           open input tmp-ordini.
           set tutto-ok to true.
           if not stampa-excel
              modify  gd-mov, mass-update = 1
              modify  gd-mov, reset-grid  = 1
              perform GD-MOV-CONTENT    
              move low-value to tmp-ord-rec
              start tmp-ordini key is >= tmp-ord-chiave
                    invalid set errori to true
              end-start
              if tutto-ok
                 move 1 to riga
                 perform until 1 = 2
                    read tmp-ordini next at end exit perform end-read
                    perform RIEMPI-COLUMNS
                 end-perform
              end-if
              modify  gd-mov, mass-update = 0
           else
              move low-value to tmp-ord-rec
              start tmp-ordini key is >= tmp-ord-chiave
                    invalid continue
                not invalid
                    perform GENERA-FILE-EXCEL
              end-start
           end-if.

      ***---
       RIEMPI-COLUMNS.
           add 1 to riga.
           initialize gd-mov-record GruppoHidden
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.
           move tmp-ord-causale      to col-causale.
           move tmp-ord-desart       to col-desart.
           move tmp-ord-movim        to col-movim.
           move tmp-ord-data-movim   to como-data.
           perform DATE-TO-SCREEN.
           move como-data            to col-data-movim.
           move tmp-ord-qta          to col-qta.
           move tmp-ord-imp          to col-imp.
           move tmp-ord-cons         to col-cons.
           move tmp-ord-coubat       to col-coubat.
           move tmp-ord-add-piombo   to col-add.
           move tmp-ord-riga         to hid-riga.
           move tmp-ord-anno         to hid-anno.
           move tmp-ord-movim        to hid-movim.

           move "Cliente"   to hid-clifor.

           move tmp-ord-codice to hid-codice cli-codice.
           set cli-tipo-C to true.
           read clienti no lock 
                invalid continue
            not invalid
                inspect cli-ragsoc-1 replacing trailing spaces 
                                                     by low-value
                string  cli-ragsoc-1 delimited low-value
                        " "          delimited size
                        cli-ragsoc-2 delimited size
                        into hid-ragsoc
                end-string
           end-read.
           move tmp-ord-codice  to des-codice
           move tmp-ord-destino to des-prog hid-des-prog
           read destini no lock
                invalid continue
            not invalid
                inspect des-ragsoc-1 replacing trailing spaces 
                                                     by low-value
                string  des-ragsoc-1 delimited low-value
                        " "          delimited size
                        des-ragsoc-2 delimited size
                        into hid-ragsoc-des
                end-string
                move des-localita to hid-citta-des
           end-read.

           move tmp-ord-marca to mar-codice hid-cod-marca.
           read tmarche no lock 
                invalid continue
            not invalid move mar-descrizione to hid-marca
           end-read.

           modify gd-mov, record-to-add = gd-mov-record.
           modify gd-mov(riga, 1), hidden-data = hid-riga.
           modify gd-mov(riga, 2), hidden-data = hid-anno.
           modify gd-mov(riga, 3), hidden-data = hid-movim.
           modify gd-mov(riga, 4), hidden-data = GruppoHidden.
      
           if tmp-ord-causale-pos
              modify gd-mov(riga), row-color = 513
           else
              modify gd-mov(riga), row-color = 176
           end-if.

           compute tot-imp = tot-imp + tmp-ord-imp-calcolato.

      ***---
       GENERA-FILE-EXCEL.
           perform ACCETTA-SEPARATORE.
           perform COMPONI-PATH-STAMPA.
           if tutto-ok
              perform SCRIVI-RIGHE-EXCEL
           end-if.

      ***---
       COMPONI-PATH-STAMPA.
           move user-codi to como-user.
           initialize wstampa.
           accept  wstampa   from environment "PATH_ST".
           inspect wstampa   replacing trailing spaces by low-value.
           inspect como-user replacing trailing spaces by low-value.
           string  wstampa           delimited low-value
                   "gestione_scorte" delimited size
                   "_"               delimited size
                   como-user         delimited low-value
                   ".csv"            delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       SCRIVI-RIGHE-EXCEL.
           set prima-volta to true.
           set tutto-ok    to true.
           perform until 1 = 2
              read tmp-ordini next at end exit perform end-read
              initialize r-riga replacing numeric data by zeroes
                                     alphanumeric data by spaces
              move tmp-ord-causale to r-causale
              set cli-tipo-C to true
              move tmp-ord-codice  to r-codice cli-codice
              read clienti no lock
                   invalid continue
               not invalid
                   |Il file è già stato riempito coi record attivi
                   inspect cli-ragsoc-1 replacing trailing spaces 
                                                        by low-value
                   initialize r-ragsoc
                   string cli-ragsoc-1 delimited low-value
                          " "          delimited size
                          cli-ragsoc-2 delimited size
                          into r-ragsoc
                   end-string
              end-read
              move tmp-ord-age        to r-age
              move tmp-ord-age-d      to r-age-d
              move tmp-ord-localita   to r-destino
              move tmp-ord-evasione   to r-evasione
              move tmp-ord-data-movim to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-data-mov
              move tmp-ord-numdoc     to r-numdoc
              move tmp-ord-datadoc    to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-datadoc
              move tmp-ord-marca      to r-marca
              move tmp-ord-magazz     to r-mag
              move tmp-ord-articolo   to r-articolo art-codice
              read articoli no lock
                   invalid continue
               not invalid
                   |Il file è già stato riempito coi record attivi
                   move art-descrizione to r-desart
              end-read
              move tmp-ord-qta        to r-qta
              compute como-numero = tmp-ord-imp    + 
                                    tmp-ord-cons   + 
                                    tmp-ord-coubat +
                                    tmp-ord-add-piombo
              move como-numero to r-prezzo
              multiply como-numero by tmp-ord-qta giving como-numero
              move como-numero to r-tot

              compute como-numero = tmp-ord-peso * tmp-ord-qta
              move como-numero to r-peso
              if prima-volta
                 initialize line-riga
                 string "Causale"           delimited size
                        separatore          delimited size
                        "Codice"            delimited size
                        separatore          delimited size
                        "Ragione Sociale"   delimited size
                        separatore          delimited size
                        "Destinazione"      delimited size
                        separatore          delimited size  
                        "Agente"            delimited size
                        separatore          delimited size
                        "Ragione Sociale"   delimited size
                        separatore          delimited size
                        "Data Ordine"       delimited size
                        separatore          delimited size
                        "Nr. Evasione"      delimited size
                        separatore          delimited size
                        "Nr. Bolla"         delimited size
                        separatore          delimited size
                        "Data Bolla"        delimited size
                        separatore          delimited size
                        "Marca"             delimited size
                        separatore          delimited size
                        "Articolo"          delimited size
                        separatore          delimited size
                        "Prodotto"          delimited size
                        separatore          delimited size
                        "Q.tà"              delimited size
                        separatore          delimited size
                        "Prezzo"            delimited size
                        separatore          delimited size
                        "Totale"            delimited size
                        separatore          delimited size
                        "Peso"              delimited size
                        separatore          delimited size
                        "Tipol. cli."       delimited size
                        separatore          delimited size
                        "Descrizione"       delimited size
                        separatore          delimited size
                        "Cod. Pag."         delimited size
                        separatore          delimited size
                        "Descrizione"       delimited size
                        into line-riga
                 end-string
                 write line-riga
                 set prima-volta to false
              end-if
              initialize line-riga
              string r-causale  delimited size
                     separatore delimited size
                     r-codice   delimited size
                     separatore delimited size
                     r-ragsoc   delimited size
                     separatore delimited size
                     r-destino  delimited size
                     separatore delimited size 
                     r-age      delimited size
                     separatore delimited size
                     r-age-d    delimited size
                     separatore delimited size
                     r-data-mov delimited size
                     separatore delimited size
                     r-evasione delimited size
                     separatore delimited size
                     r-numdoc   delimited size
                     separatore delimited size
                     r-datadoc  delimited size
                     separatore delimited size
                     r-marca    delimited size
                     separatore delimited size
                     r-articolo delimited size
                     separatore delimited size
                     r-desart   delimited size
                     separatore delimited size
                     r-qta      delimited size
                     separatore delimited size
                     r-prezzo   delimited size
                     separatore delimited size
                     r-tot      delimited size
                     separatore delimited size
                     r-peso     delimited size  
                     separatore delimited size
                     tmp-ord-cli-tipo   delimited size
                     separatore         delimited size
                     tmp-ord-cli-tipo-d delimited size
                     separatore         delimited size
                     tmp-ord-pag        delimited size
                     separatore         delimited size
                     tmp-ord-pag-d      delimited size
                     into line-riga
              end-string
              write line-riga
           end-perform.
           close lineseq.

           perform CALL-EXCEL.
