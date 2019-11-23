      ***---
       RIEMPI-TMP.
           move 0 to counter counter2.
           move 0 to RecCounter.
           move 0 to tot-kg tot-imp tot-colimp tot-add-pb
                     tot-cons tot-coubat.
           perform SECCA-TMP.
           set tutto-ok to true.
           set trovato  to false.
           initialize path-tmp-movmagforn.
           accept  path-tmp-movmagforn from environment "PATH-ST".
           inspect path-tmp-movmagforn 
                   replacing trailing spaces by low-value.
           accept como-data from century-date.
           accept como-ora  from time.
           string path-tmp-movmagforn   delimited low-value
                  "tmp-ordforn"     delimited size
                  "_"               delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".tmp"            delimited size
                  into path-tmp-movmagforn
           end-string.
           open output tmp-movmagforn.
           set FileOpen to true.
           perform CICLO-LETTURA-ORDINI.
           perform RIEMPI-GRIGLIA.
           move tot-colimp to lab-tot-colimp-buf
           move tot-cons   to lab-tot-cons-buf.
           move tot-coubat to lab-tot-coubat-buf.
           move tot-add-pb to lab-tot-add-buf.
           move tot-kg     to lab-tot-kg-buf
           move tot-imp    to lab-tot-imp-buf.
           display lab-tot-kg lab-tot-imp lab-tot-colimp
                   lab-tot-cons lab-tot-coubat.

      ***---
       CICLO-LETTURA-ORDINI.
           move low-value to tof-chiave.
           move como-data-from to tof-data-ordine.
LUBEXX     if cli-codice of clienti not = 0
LUBEXX        set cli-tipo-F of clienti to true
LUBEXX        move cli-codice of clienti  to tof-cod-forn
LUBEXX        if desf-prog not = 0
LUBEXX           move desf-prog to tof-destino
LUBEXX        end-if
LUBEXX        start tordforn key is >= k-fornitore
                 invalid 
                    set errori to true
LUBEXX        end-start
LUBEXX     else
LUBEXX        start tordforn key is >= tof-k-data
                 invalid 
                    set errori to true
LUBEXX        end-start
LUBEXX     end-if.
      *
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read tordforn next 
                    at end 
                       exit perform 
                 end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon form3-handle at column 22
                                              line 03
                    move 0 to counter2
                 end-if

LUBEXX           if cli-codice of clienti not = 0
LUBEXX              if cli-codice of clienti  not = tof-cod-forn
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX              if desf-prog not = 0
LUBEXX                 if desf-prog not = tof-destino
LUBEXX                    exit perform
LUBEXX                 end-if
LUBEXX              end-if
                 else
                    if tof-data-ordine > como-data-to
                       exit perform
                    end-if
LUBEXX           end-if
                 
LUBEXX           if tof-data-ordine >= como-data-from and
LUBEXX              tof-data-ordine <= como-data-to

                    if cli-codice of clienti    not = 0
                       if cli-codice of clienti not = tof-cod-forn
                          set record-ok to false
                       end-if
                       if desf-prog not = 0 and
                          desf-prog not = tof-destino
                          set record-ok to false
                       end-if
                    end-if



                    if cli-codice of clienti1    not = 0
                       if cli-codice of clienti1 not = tof-cliente
                          set record-ok to false
                       end-if
                       if des-prog not = 0 and
                          des-prog not = tof-destino-c
                          set record-ok to false
                       end-if
                    end-if

LUBEXX*              if record-ok
      *                 set cli-tipo-F to true
LUBEXX*                 move tof-cod-forn to cli-codice
LUBEXX*                 read clienti no lock
LUBEXX*                      invalid initialize cli-rec
LUBEXX*                 end-read
LUBEXX*              end-if
                    if record-ok
                       perform LOOP-RORDFORN
                    end-if
LUBEXX           else
LUBEXX              if cli-codice of clienti = 0
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX           end-if                    

              end-perform
           end-if.

      ***---
       LOOP-rordforn.
           set tutto-ok to true.
           move tof-chiave to rof-chiave.
           move low-value  to rof-riga.
           start rordforn key is >= rof-chiave
              invalid 
                 set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read rordforn next 
                    at end 
                       exit perform 
                 end-read
                 if rof-anno    not = tof-anno or
                    rof-numero  not = tof-numero
                    exit perform
                 end-if
                 set record-ok     to true
                 move rof-cod-articolo to art-codice
                 read articoli no lock 
                    invalid 
                       set record-ok to false
                    not invalid
                       if SaveArticolo not = 0 and
                          SaveArticolo not = art-codice
                          set record-ok to false
                       end-if
                       if mar-codice not = 0 and
                          mar-codice not = art-marca-prodotto
                          set record-ok  to false
                       end-if
                 end-read
                 if record-ok 
                    perform MOVE-DATI 
                 end-if
              end-perform
           end-if.

      ***---
       MOVE-DATI.
           set trovato to true.
           initialize tmp-movf-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move art-descrizione    to tmp-movf-desart.
           move rof-anno           to tmp-movf-anno.
           move rof-numero         to tmp-movf-movim.
           move rof-riga           to tmp-movf-riga.
           move tof-causale        to tca-codice
           read tcaumag no lock 
              invalid
                 initialize tca-cod-magaz
           end-read
           move tca-cod-magaz      to tmp-movf-mag.
           move tof-data-ordine    to tmp-movf-data-movim.
           move rof-qta-ord        to tmp-movf-qta-ord.
           move rof-qta-evasa      to tmp-movf-qta-eva.
           move rof-imp-consumo    to tmp-movf-cons.
           move rof-imp-cou-cobat  to tmp-movf-coubat.
           move tof-promo          to tmp-movf-promo.
           move tof-stato          to tmp-movf-stato.

           move rof-imponib-merce  to tmp-movf-imp.

           move rof-add-piombo     to tmp-movf-add.

           if rof-qta-ord = 0
              move 1 to rof-qta-ord
           end-if.
           compute tmp-movf-imp-calcolato =  
                 ( rof-imponib-merce + 
                   rof-add-piombo    +
                   rof-imp-consumo   + 
                   rof-imp-cou-cobat ) * rof-qta-ord.
      
      *     move tmp-movf-causale to tca-codice.
      *     read tcaumag no lock 
      *        invalid 
      *           continue 
      *     end-read.
           move tca-cod-magaz to tmo-codmag.

           if tca-imponibile-pos
           
              compute tot-colimp = 
                      tot-colimp + ( tmp-movf-imp    * tmp-movf-qta-ord)
           
              compute tot-add-pb = 
                      tot-add-pb + ( tmp-movf-add    * tmp-movf-qta-ord)

              compute tot-cons   = 
                      tot-cons   + ( tmp-movf-cons   * tmp-movf-qta-ord)
              compute tot-coubat = 
                      tot-coubat + (tmp-movf-coubat * tmp-movf-qta-ord )

              compute tot-kg  = tot-kg + 
                   (( rof-peso-utf + rof-peso-non-utf ) * rof-qta-ord )

              add tmp-movf-imp-calcolato to tot-imp
              set tmp-movf-causale-pos   to true
           else
           
              compute tot-colimp = 
                      tot-colimp - ( tmp-movf-imp    * tmp-movf-qta-ord)
           
              compute tot-add-pb = 
                      tot-add-pb + ( tmp-movf-add    * tmp-movf-qta-ord)

              compute tot-cons   = 
                      tot-cons   - ( tmp-movf-cons   * tmp-movf-qta-ord)
              compute tot-coubat = 
                      tot-coubat - ( tmp-movf-coubat * tmp-movf-qta-ord)

              compute tot-kg  = tot-kg -
                   (( rof-peso-utf + rof-peso-non-utf ) * rof-qta-ord )

              subtract tmp-movf-imp-calcolato from tot-imp
              set tmp-movf-causale-neg to true

              compute tmp-movf-qta-ord = tmp-movf-qta-ord - 
                                         (tmp-movf-qta-ord * 2 )

           end-if.

           move tof-cod-forn        to tmp-movf-codice.
           move tof-destino    to tmp-movf-destino.

           move rof-cod-articolo to art-codice.
           read articoli no lock 
              invalid 
                 continue 
           end-read.

           move art-marca-prodotto to tmp-movf-marca.

           move rof-cod-articolo      to tmp-movf-articolo.
LUBEXX     |Devo utilizzare il peso della riga
LUBEXX     move rof-peso              to tmp-movf-peso.

           write tmp-movf-rec 
              invalid 
                 continue 
           end-write.

           perform BUG-FIX-THIN-CLIENT.

      ***---
       RIEMPI-GRIGLIA.
           move 0 to RecCounter.
           close tmp-movmagforn.
           open input tmp-movmagforn.
           set tutto-ok to true.
           modify  gd-mov, mass-update = 1.
           modify  gd-mov, reset-grid  = 1.
           perform GD-MOV-CONTENT.
           move low-value to tmp-movf-rec.
           start tmp-movmagforn key is >= tmp-movf-k1|tmp-movf-chiave
              invalid 
                 set errori to true
           end-start
           if tutto-ok
              move 1 to riga
              perform until 1 = 2
                 read tmp-movmagforn next 
                    at end 
                       exit perform 
                 end-read
                 perform RIEMPI-COLUMNS
              end-perform
           end-if.
           modify  gd-mov, mass-update = 0.
           if stampa-excel
              move low-value to tmp-movf-rec
              start tmp-movmagforn key is >= tmp-movf-k1|tmp-movf-codice
                 invalid 
                    continue
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
           move tmp-movf-mag          to col-magazzino.
           move tmp-movf-desart       to col-desart.
           move tmp-movf-movim        to col-movim.
           move tmp-movf-data-movim   to como-data.
           perform DATE-TO-SCREEN.
           move como-data             to col-data-movim.
           move tmp-movf-qta-ord      to col-qta-ord.
           move tmp-movf-qta-eva      to col-qta-eva.
           move tmp-movf-imp          to col-imp.
           move tmp-movf-add          to col-add.
           move tmp-movf-cons         to col-cons.
           move tmp-movf-coubat       to col-coubat.
           move tmp-movf-riga         to hid-riga.
           move tmp-movf-anno         to hid-anno.
           move tmp-movf-movim        to hid-movim.

           compute como-numero = tmp-movf-imp  + 
                                 tmp-movf-add  +
                                 tmp-movf-cons + 
                                 tmp-movf-coubat
           move como-numero to col-prz-finito

           move tmp-movf-articolo   to col-cod-art


           move tmp-movf-mag to r-mag
           set cli-tipo-F of clienti to true
           move tmp-movf-codice to r-codice 
                                   cli-codice of clienti
           read clienti no lock
              invalid 
                 continue
              not invalid
                |Il file è già stato riempito coi record attivi
                inspect cli-ragsoc-1 of clienti
                                 replacing trailing spaces by low-value
                initialize r-ragsoc
                string cli-ragsoc-1 of clienti delimited low-value
                       " "                     delimited size
                       cli-ragsoc-2 of clienti delimited size
                       into col-forn
                end-string
           end-read


      ***     if tmp-mov-cliente move "Cliente"   to hid-clifor
      ***     else               move "Fornitore" to hid-clifor
      ***     end-if.

           move "Fornitore" to hid-clifor

           initialize cli-rec of clienti 
           replacing numeric data by zeroes alphanumeric data by spaces.

           set cli-tipo-F of clienti to true.
           move tmp-movf-codice to hid-codice cli-codice of clienti.
           read clienti no lock 
              invalid 
                 continue
              not invalid
                inspect cli-ragsoc-1 of clienti 
                          replacing trailing spaces by low-value
                string  cli-ragsoc-1 of clienti   delimited low-value
                        " "                       delimited size
                        cli-ragsoc-2 of clienti   delimited size
                        into hid-ragsoc
                end-string
           end-read.

           initialize desf-rec desf-rec mar-rec
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.

           move tmp-movf-codice    to desf-codice
           move tmp-movf-destino   to desf-prog
                                      hid-des-prog
           read destinif no lock
              invalid 
                 continue
              not invalid
                 inspect desf-ragsoc-1 replacing trailing spaces 
                                                      by low-value
                 string  desf-ragsoc-1 delimited low-value
                         " "           delimited size
                         desf-ragsoc-2 delimited size
                         into hid-ragsoc-des
                 end-string
                 move desf-localita to hid-citta-des
           end-read

           move tmp-movf-marca to mar-codice hid-cod-marca.
           read tmarche no lock 
              invalid 
                 continue
              not invalid 
                 move mar-descrizione to hid-marca
           end-read.

           modify gd-mov, record-to-add = gd-mov-record.
           modify gd-mov(riga, 1), hidden-data = hid-riga.
           modify gd-mov(riga, 2), hidden-data = hid-anno.
           modify gd-mov(riga, 3), hidden-data = hid-movim.
           modify gd-mov(riga, 4), hidden-data = GruppoHidden.

           move tmp-movf-anno  to tof-anno.
           move tmp-movf-movim to tof-numero.
           read tordforn no lock invalid continue end-read.
                  
           if tof-chiuso
              modify gd-mov(riga), row-color = 304
           else
              if tmp-movf-causale-pos
                 modify gd-mov(riga), row-color = 513
              else
                 modify gd-mov(riga), row-color = 176
              end-if
           end-if.

           if tmp-movf-qta-eva = 0
              modify gd-mov(riga, 78-col-evava), 
                                      cell-color = 78-colore-inevasa
           else
              if tmp-movf-qta-eva >= tmp-movf-qta-ord
                 modify gd-mov(riga, 78-col-evava) 
                                      cell-color = 78-colore-evasa
              else
                 modify gd-mov(riga, 78-col-evava) 
                                      cell-color = 78-colore-paraziale
              end-if
           end-if

           perform BUG-FIX-THIN-CLIENT.

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
           string  wstampa       delimited low-value
                   "ordf_art"    delimited size
                   "_"           delimited size
                   como-user     delimited low-value
                   ".csv"        delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       SCRIVI-RIGHE-EXCEL.
           set prima-volta to true.
           set tutto-ok    to true.
           perform until 1 = 2
              read tmp-movmagforn next 
                 at end exit 
              perform end-read
              initialize r-riga cli-rec of clienti art-rec
                         replacing numeric data by zeroes
                              alphanumeric data by spaces
              move tmp-movf-mag to r-mag
              set cli-tipo-F of clienti to true
              move tmp-movf-codice to r-codice 
                                      cli-codice of clienti
              read clienti no lock
                 invalid 
                    continue
                 not invalid
                   |Il file è già stato riempito coi record attivi
                   inspect cli-ragsoc-1 of clienti
                                replacing trailing spaces by low-value
                   initialize r-ragsoc
                   string cli-ragsoc-1 of clienti delimited low-value
                          " "                     delimited size
                          cli-ragsoc-2 of clienti delimited size
                          into r-ragsoc
                   end-string
                   move cli-localita of clienti to r-localita
              end-read

              if tmp-movf-destino not = 0
                 move tmp-movf-codice  to desf-codice
                 move tmp-movf-destino to desf-prog
                 read destinif no lock
              end-if

              move tmp-movf-destino   to r-destino
                                         desf-prog
              move tmp-movf-codice    to desf-codice
              read destinif no lock
                 invalid 
                    continue
                 not invalid
                    inspect desf-ragsoc-1 replacing trailing spaces 
                                                      by low-value
                    string  desf-ragsoc-1 delimited low-value
                            " "           delimited size
                            desf-ragsoc-2 delimited size
                            into r-ragsoc-dest
                    end-string
              end-read

              

              move tmp-movf-movim      to r-numero

              move tmp-movf-data-movim to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-data-mov

              move tmp-movf-marca      to r-marca
              move tmp-movf-articolo   to r-articolo art-codice
LUBEXX        move "N"                 to r-utf
              move tmp-movf-promo      to r-promo
              evaluate tmp-movf-stato
              when "C" move "CHIUSO"   to r-stato
              when "L" move "IN LAV."  to r-stato
              when "S" move "INVIATO"  to r-stato
              when "I" move "INSERITO" to r-stato
              end-evaluate
              read articoli no lock
                   invalid continue
               not invalid
                   |Il file è già stato riempito coi record attivi
                    move art-descrizione  to r-desart
LUBEXX              if art-peso-utf not = 0
LUBEXX                 move "S" to r-utf
LUBEXX              end-if
              end-read
              move tmp-movf-imp        to r-imp-merce
              move tmp-movf-add        to r-add-pb
              move tmp-movf-cons       to r-cons
              move tmp-movf-coubat     to r-coubat
              move tmp-movf-qta-ord    to r-qta-ord
              move tmp-movf-qta-eva    to r-qta-eva
              compute como-numero = tmp-movf-imp  + 
                                    tmp-movf-add  +
                                    tmp-movf-cons + 
                                    tmp-movf-coubat
              move como-numero to r-prezzo
              multiply como-numero by tmp-movf-qta-ord 
                                                     giving como-numero
              move como-numero to r-tot

              compute como-numero = tmp-movf-peso * tmp-movf-qta-ord
              move como-numero to r-peso
              if prima-volta
                 initialize line-riga
                 string "Magazzino"         delimited size
                        separatore          delimited size
                        "Codice"            delimited size
      *                  separatore          delimited size
      *                  "Gruppo GDO"        delimited size
                        separatore          delimited size
                        "Ragione Sociale"   delimited size
                        separatore          delimited size
                        "Destino"           delimited size
                        separatore          delimited size
                        "Ragione Sociale"   delimited size
                        separatore          delimited size
                        "Località"          delimited size
                        separatore          delimited size
                        "Nr. Ord."          delimited size
                        separatore          delimited size
                        "Data Creazione"    delimited size
                        separatore          delimited size
      *                  "Nr. Bolla"         delimited size
      *                  separatore          delimited size
      *                  "Data Bolla"        delimited size
      *                  separatore          delimited size
      *                  "Nr. Fattura"       delimited size
      *                  separatore          delimited size
      *                  "Data Fattura"      delimited size
      *                  separatore          delimited size
                        "Marca"             delimited size
                        separatore          delimited size
      *                  "Mag."              delimited size
      *                  separatore          delimited size
                        "Articolo"          delimited size
                        separatore          delimited size
                        "Prodotto"          delimited size
                        separatore          delimited size
                        "Imp. Merce"        delimited size
                        separatore          delimited size
                        "Add. Pb"           delimited size
                        separatore          delimited size
                        "I. Consumo"        delimited size
                        separatore          delimited size
                        "COU/Cobat"         delimited size
                        separatore          delimited size
                        "Q.tà ord."         delimited size
                        separatore          delimited size
                        "Q.tà eva."         delimited size
                        separatore          delimited size
                        "Prezzo"            delimited size
                        separatore          delimited size
                        "Totale"            delimited size
                        separatore          delimited size
                        "Peso"              delimited size
                        separatore          delimited size
                        "UTF"               delimited size
                        separatore          delimited size
                        "Promo"             delimited size
                        separatore          delimited size
                        "Stato"             delimited size
                        into line-riga
                 end-string
                 write line-riga
                 set prima-volta to false
              end-if
              initialize line-riga
              string r-mag         delimited size
                     separatore    delimited size
                     r-codice      delimited size
      *               separatore    delimited size
      *               r-gdo         delimited size
                     separatore    delimited size
                     r-ragsoc      delimited size
                     separatore    delimited size
                     r-destino     delimited size
                     separatore    delimited size
                     r-ragsoc-dest delimited size
                     separatore    delimited size
                     r-localita    delimited size
                     separatore    delimited size
                     r-numero      delimited size
                     separatore    delimited size
                     r-data-mov    delimited size
                     separatore    delimited size
      *               r-num-bolla   delimited size
      *               separatore    delimited size
      *               r-data-bolla  delimited size
      *               separatore    delimited size
      *               r-numdoc      delimited size
      *               separatore    delimited size
      *               r-datadoc     delimited size
      *               separatore    delimited size
                     r-marca       delimited size
                     separatore    delimited size
      *               r-mag2        delimited size
      *               separatore    delimited size
                     r-articolo    delimited size
                     separatore    delimited size
                     r-desart      delimited size
                     separatore    delimited size
                     r-imp-merce   delimited size
                     separatore    delimited size
                     r-add-pb      delimited size
                     separatore    delimited size
                     r-cons        delimited size
                     separatore    delimited size
                     r-coubat      delimited size
                     separatore    delimited size
                     r-qta-ord     delimited size
                     separatore    delimited size
                     r-qta-eva     delimited size
                     separatore    delimited size
                     r-prezzo      delimited size
                     separatore    delimited size
                     r-tot         delimited size
                     separatore    delimited size
                     r-peso        delimited size
                     separatore    delimited size
                     r-utf         delimited size
                     separatore    delimited size
                     r-promo       delimited size
                     separatore    delimited size
                     r-stato       delimited size
                     into line-riga
              end-string
              write line-riga
           end-perform.
           close lineseq.

           perform CALL-EXCEL.

      ***---
       BUG-FIX-THIN-CLIENT.
           |MODIFICA PER THIN CLIENT (BUG-FIX)
           add 1 to RecCounter.
           if RecCounter = num-rec-thin-client
              move 0 to RecCounter
              display form3
           end-if.
