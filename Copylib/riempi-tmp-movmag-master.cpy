      ***---
       RIEMPI-TMP.
           move 0 to counter counter2.
           move 0 to RecCounter.
           move 0 to tot-kg tot-imp tot-colimp tot-add-pb
                     tot-cons tot-coubat.
           perform SECCA-TMP.
           set tutto-ok to true.
           set trovato  to false.
           initialize path-tmp-movmag.
           accept  path-tmp-movmag from environment "PATH-ST".
           inspect path-tmp-movmag 
                   replacing trailing spaces by low-value.
           accept como-data from century-date.
           accept como-ora  from time.
           string path-tmp-movmag   delimited low-value
                  "tmp-master"      delimited size
                  "_"               delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".tmp"            delimited size
                  into path-tmp-movmag
           end-string.
           open output tmp-movmag.
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
           move low-value to mto-chiave.
           move como-data-from to mto-data-ordine.
LUBEXX     if cli-codice not = 0
LUBEXX        set cli-tipo-C to true
LUBEXX        move cli-codice  to mto-cod-cli
LUBEXX        if des-prog not = 0
LUBEXX           move des-prog to mto-prg-destino
LUBEXX        end-if
LUBEXX        start mtordini key is >= mto-k-clides
LUBEXX              invalid set errori to true
LUBEXX        end-start
LUBEXX     else
LUBEXX        start mtordini key is >= mto-k-data
LUBEXX              invalid set errori to true
LUBEXX        end-start
LUBEXX     end-if.
      *
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read mtordini next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon form3-handle at column 22
                                              line 03
                    move 0 to counter2
                 end-if   
                       
                 inquire ef-cod,  value cli-codice
                 inquire ef-des,  value des-prog
                 inquire ef-gdo,  value ef-gdo-buf
                 inquire ef-tipo, value tcl-codice

LUBEXX           if cli-codice not = 0
LUBEXX              if cli-codice  not = mto-cod-cli
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX              if des-prog not = 0
LUBEXX                 if des-prog not = mto-prg-destino
LUBEXX                    exit perform
LUBEXX                 end-if
LUBEXX              end-if
LUBEXX           end-if                   

                 if mto-data-ordine > como-data-to
                    exit perform
                 end-if
                 
                 if mto-data-creazione < como-data-from
                    exit perform cycle
                 end-if   

                 if cli-codice    not = 0
                    if cli-codice not = mto-cod-cli
                       exit perform cycle
                    end-if
                    if des-prog not = 0 and
                       des-prog not = mto-prg-destino
                       exit perform cycle
                    end-if
                 end-if

                 set cli-tipo-C to true
LUBEXX           move mto-cod-cli to cli-codice
LUBEXX           read clienti no lock
LUBEXX                invalid initialize cli-rec
LUBEXX           end-read
LUBEXX           if tcl-codice not = spaces and
LUBEXX              tcl-codice not = cli-tipo
LUBEXX              exit perform cycle
LUBEXX           end-if
                                                     
                 if ef-gdo-buf not = spaces and
                    ef-gdo-buf not = cli-gdo
                    exit perform cycle
                 end-if

                 move mto-causale to tca-codice
                 read tcaumag  no lock invalid continue end-read
                 move tca-tipo to Save-CF
                 perform LOOP-MRORDINI

              end-perform
           end-if.

      ***---
       LOOP-MRORDINI.
           set tutto-ok to true.
           move mto-chiave to mro-chiave.
           move low-value  to mro-riga.
           start mrordini key is >= mro-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read mrordini next at end exit perform end-read
                 if mro-anno   not = mto-anno or
                    mro-numero not = mto-numero
                    exit perform
                 end-if
                 set record-ok     to true
                 move mro-cod-articolo to art-codice
                 read articoli no lock 
                      invalid set record-ok to false
                  not invalid
                      if SaveArticolo not = 0 and
                         SaveArticolo not = art-codice
                         set record-ok to false
                      end-if
                      inquire ef-marca value in mar-codice
                      if mar-codice not = 0 and
                         mar-codice not = art-marca-prodotto
                         set record-ok  to false
                      end-if
                 end-read
                 if record-ok perform MOVE-DATI end-if
              end-perform
           end-if.

      ***---
       MOVE-DATI.
           set trovato to true.
           initialize tmp-mov-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move art-descrizione    to tmp-mov-desart.
           move mro-anno           to tmp-mov-anno.
           move mro-numero         to tmp-mov-movim.
           move mro-riga           to tmp-mov-riga.
           move mto-causale        to tmp-mov-causale.
           move mto-data-creazione to tmp-mov-data-movim.
           move mro-qta            to tmp-mov-qta.
           move mro-qta-e          to tmp-mov-qta-e.
           move mro-imp-consumo    to tmp-mov-cons.
           move mro-imp-cou-cobat  to tmp-mov-coubat.
           move mro-promo          to tmp-mov-promo. 
           evaluate true
           when mto-registrato     move "REGISTRATO" to tmp-mov-stato-m
           when mto-in-lavorazione move "IN LAVOR"   to tmp-mov-stato-m
           when mto-sped-parz      move "SPED PARZ"  to tmp-mov-stato-m
           when mto-sped-tot       move "SPED TOT"   to tmp-mov-stato-m
           when mto-chiuso         move "CHIUSO"     to tmp-mov-stato-m
           end-evaluate.
           evaluate true
           when mro-registrato     move "REGISTRATO" to tmp-mov-stato-r
           when mro-in-lavorazione move "IN LAVOR"   to tmp-mov-stato-r
           when mro-sped-parz      move "SPED PARZ"  to tmp-mov-stato-r
           when mro-sped-tot       move "SPED TOT"   to tmp-mov-stato-r
           when mro-chiuso         move "CHIUSO"     to tmp-mov-stato-r
           end-evaluate.
                                                    
           move mro-num-colli        to tmp-mov-colli.
           move mro-prg-tipo-imballo to tmp-mov-imballo.

           move mro-imponib-merce  to tmp-mov-imp.

           move mro-add-piombo     to tmp-mov-add.

           if mro-qta = 0
              move 1 to mro-qta
           end-if.
           compute tmp-mov-imp-calcolato =  
                 ( mro-imponib-merce + 
                   mro-add-piombo    +
                   mro-imp-consumo   + 
                   mro-imp-cou-cobat ) * mro-qta.
      
           move tmp-mov-causale to tca-codice.
           read tcaumag no lock invalid continue end-read.
           move tca-cod-magaz to tmo-codmag.

           if tca-imponibile-pos
           
              compute tot-colimp = 
                      tot-colimp + ( tmp-mov-imp    * tmp-mov-qta)
           
              compute tot-add-pb = 
                      tot-add-pb + ( tmp-mov-add    * tmp-mov-qta)

              compute tot-cons   = 
                      tot-cons   + ( tmp-mov-cons   * tmp-mov-qta)
              compute tot-coubat = 
                      tot-coubat + ( tmp-mov-coubat * tmp-mov-qta )

              compute tot-kg  = tot-kg + 
                   (( mro-peso-utf + mro-peso-non-utf ) * mro-qta )

              add tmp-mov-imp-calcolato to tot-imp
              set tmp-mov-causale-pos   to true
           else
           
              compute tot-colimp = 
                      tot-colimp - ( tmp-mov-imp    * tmp-mov-qta)
           
              compute tot-add-pb = 
                      tot-add-pb + ( tmp-mov-add    * tmp-mov-qta)

              compute tot-cons   = 
                      tot-cons   - ( tmp-mov-cons   * tmp-mov-qta)
              compute tot-coubat = 
                      tot-coubat - ( tmp-mov-coubat * tmp-mov-qta )

              compute tot-kg  = tot-kg -
                   (( mro-peso-utf + mro-peso-non-utf ) * mro-qta )

              subtract tmp-mov-imp-calcolato from tot-imp
              set tmp-mov-causale-neg to true

              compute tmp-mov-qta = tmp-mov-qta - ( tmp-mov-qta * 2 )

           end-if.

           move mto-cod-cli        to tmp-mov-codice.
           move mto-prg-destino    to tmp-mov-destino.

           move mro-cod-articolo to art-codice.
           read articoli no lock invalid continue end-read.

           move art-marca-prodotto to tmp-mov-marca.

           move mro-prg-cod-magazzino to tmp-mov-magazz.
           move mro-cod-articolo      to tmp-mov-articolo.
LUBEXX     |Devo utilizzare il peso della riga
LUBEXX     move mro-prg-peso              to tmp-mov-peso.

           write tmp-mov-rec invalid continue end-write.

           perform BUG-FIX-THIN-CLIENT.

      ***---
       RIEMPI-GRIGLIA.
           move 0 to RecCounter.
           close tmp-movmag.
           open input tmp-movmag.
           set tutto-ok to true.
           modify  gd-mov, mass-update = 1.
           modify  gd-mov, reset-grid  = 1.
           perform GD-MOV-CONTENT.
           move low-value to tmp-mov-rec.
           start tmp-movmag key is >= tmp-mov-chiave
                 invalid set errori to true
           end-start
           if tutto-ok
              move 1 to riga
              perform until 1 = 2
                 read tmp-movmag next at end exit perform end-read
                 perform RIEMPI-COLUMNS
              end-perform
           end-if.
           modify  gd-mov, mass-update = 0.
           if stampa-excel
              move low-value to tmp-mov-rec
              start tmp-movmag key is >= k-cod-cli
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
           move tmp-mov-causale      to col-causale.
           move tmp-mov-desart       to col-desart.
           move tmp-mov-movim        to col-movim.
           move tmp-mov-data-movim   to como-data.
           perform DATE-TO-SCREEN.
           move como-data            to col-data-movim.
           move tmp-mov-qta          to col-qta.
           move tmp-mov-imp          to col-imp.
           move tmp-mov-add          to col-add.
           move tmp-mov-cons         to col-cons.
           move tmp-mov-coubat       to col-coubat.
           move tmp-mov-riga         to hid-riga.
           move tmp-mov-anno         to hid-anno.
           move tmp-mov-movim        to hid-movim.

           if tmp-mov-cliente move "Cliente"   to hid-clifor
           else               move "Fornitore" to hid-clifor
           end-if.

           initialize cli-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.

           set cli-tipo-C to true.
           move tmp-mov-codice to hid-codice cli-codice.
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
                move cli-tipo to tcl-codice
                read ttipocli 
                     invalid set ClienteGDO to false
                 not invalid 
                     if ttipocli-gdo set ClienteGDO to true
                     else            set ClienteGDO to false
                     end-if
                end-read
           end-read.

           initialize des-rec desf-rec mar-rec
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.

           if tmp-mov-cliente
              move tmp-mov-codice  to des-codice
              move tmp-mov-destino to des-prog
              read destini no lock
                   invalid continue
               not invalid
                   move des-prog to hid-des-prog
                   inspect des-ragsoc-1 replacing trailing spaces 
                                                        by low-value
                   string  des-ragsoc-1 delimited low-value
                           " "          delimited size
                           des-ragsoc-2 delimited size
                           into hid-ragsoc-des
                   end-string
                   move des-localita to hid-citta-des
              end-read
           else
              move tmp-mov-codice  to desf-codice
              move tmp-mov-destino to desf-prog
              read destinif no lock
                   invalid continue
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
           end-if.

           move tmp-mov-marca to mar-codice hid-cod-marca.
           read tmarche no lock 
                invalid continue
            not invalid move mar-descrizione to hid-marca
           end-read.

           modify gd-mov, record-to-add = gd-mov-record.
           modify gd-mov(riga, 1), hidden-data = hid-riga.
           modify gd-mov(riga, 2), hidden-data = hid-anno.
           modify gd-mov(riga, 3), hidden-data = hid-movim.
           modify gd-mov(riga, 4), hidden-data = GruppoHidden.
      
           if tmp-mov-causale-pos
              modify gd-mov(riga), row-color = 513
           else
              modify gd-mov(riga), row-color = 176
           end-if.

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
                   "movim_mas"   delimited size
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
              read tmp-movmag next at end exit perform end-read
              initialize r-riga cli-rec art-rec
                         replacing numeric data by zeroes
                              alphanumeric data by spaces
              move tmp-mov-causale to r-causale
              set cli-tipo-C to true
              move tmp-mov-codice  to r-codice cli-codice
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
                   move cli-localita to r-localita
              end-read

              if tmp-mov-destino not = 0
                 move tmp-mov-codice  to des-codice
                 move tmp-mov-destino to des-prog
                 read destini no lock
              end-if

              move spaces   to gdo-intestazione
              move cli-gdo  to gdo-codice
              read tgrupgdo no lock invalid continue end-read
              move gdo-intestazione to r-gdo
                                  
              move tmp-mov-destino    to r-destino
              move tmp-mov-movim      to r-numero

              move tmp-mov-data-movim to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-data-mov
                                                 
              move tmp-mov-marca      to r-marca
              move tmp-mov-magazz     to r-mag
              move tmp-mov-articolo   to r-articolo art-codice
LUBEXX        move "N"                to r-utf
              move tmp-mov-promo      to r-promo
              read articoli no lock
                   invalid continue
               not invalid
                   |Il file è già stato riempito coi record attivi
                   move art-descrizione  to r-desart
LUBEXX             if art-peso-utf not = 0
LUBEXX                move "S" to r-utf
LUBEXX             end-if
              end-read                                

              move tmp-mov-colli      to r-colli
              move tmp-mov-imballo    to r-imballo

              move tmp-mov-imp        to r-imp-merce
              move tmp-mov-add        to r-add-pb
              move tmp-mov-cons       to r-cons
              move tmp-mov-coubat     to r-coubat
              move tmp-mov-qta        to r-qta
              move tmp-mov-qta-e      to r-evasa
              compute como-numero = tmp-mov-imp  + 
                                    tmp-mov-add  +
                                    tmp-mov-cons + 
                                    tmp-mov-coubat
              move como-numero to r-prezzo
              multiply como-numero by tmp-mov-qta giving como-numero
              move como-numero to r-tot

              move tmp-mov-stato-m to r-stato-m
              move tmp-mov-stato-r to r-stato-r

              compute como-numero = tmp-mov-peso * tmp-mov-qta
              move como-numero to r-peso
              if prima-volta
                 if Save-C
                    move "Nr. Fattura"  to tipo-doc
                    move "Data Fattura" to data-doc
                 else
                    move "Nr. Bolla"    to tipo-doc
                    move "Data Bolla"   to data-doc
                 end-if
                 initialize line-riga
                 string "Causale"           delimited size
                        separatore          delimited size
                        "Codice"            delimited size
                        separatore          delimited size
                        "Gruppo GDO"        delimited size
                        separatore          delimited size
                        "Ragione Sociale"   delimited size
                        separatore          delimited size
                        "Destino"           delimited size
                        separatore          delimited size
                        "Località"          delimited size
                        separatore          delimited size
                        "Nr. Ord."          delimited size
                        separatore          delimited size
                        "Data Creazione"    delimited size
                        separatore          delimited size
                        "Marca"             delimited size
                        separatore          delimited size
                        "Mag."              delimited size
                        separatore          delimited size
                        "Articolo"          delimited size
                        separatore          delimited size
                        "Prodotto"          delimited size
                        separatore          delimited size
                        "Imballo"           delimited size
                        separatore          delimited size
                        "Colli"             delimited size
                        separatore          delimited size
                        "Imp. Merce"        delimited size
                        separatore          delimited size
                        "Add. Pb"           delimited size
                        separatore          delimited size
                        "I. Consumo"        delimited size
                        separatore          delimited size
                        "COU/Cobat"         delimited size
                        separatore          delimited size
                        "Q.tà"              delimited size
                        separatore          delimited size
                        "Evasa"             delimited size
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
                        "Stato MASTER"      delimited size
                        separatore          delimited size
                        "Stato RIGA"        delimited size
                        into line-riga
                 end-string
                 write line-riga
                 set prima-volta to false
              end-if
              initialize line-riga
              string r-causale     delimited size
                     separatore    delimited size
                     r-codice      delimited size
                     separatore    delimited size
                     r-gdo         delimited size
                     separatore    delimited size
                     r-ragsoc      delimited size
                     separatore    delimited size
                     r-destino     delimited size
                     separatore    delimited size
                     r-localita    delimited size
                     separatore    delimited size
                     r-numero      delimited size
                     separatore    delimited size
                     r-data-mov    delimited size
                     separatore    delimited size
                     r-marca       delimited size
                     separatore    delimited size
                     r-mag         delimited size
                     separatore    delimited size
                     r-articolo    delimited size
                     separatore    delimited size
                     r-desart      delimited size
                     separatore    delimited size
                     r-imballo     delimited size
                     separatore    delimited size
                     r-colli       delimited size
                     separatore    delimited size
                     r-imp-merce   delimited size
                     separatore    delimited size
                     r-add-pb      delimited size
                     separatore    delimited size
                     r-cons        delimited size
                     separatore    delimited size
                     r-coubat      delimited size
                     separatore    delimited size
                     r-qta         delimited size
                     separatore    delimited size
                     r-evasa       delimited size
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
                     r-stato-m     delimited size
                     separatore    delimited size
                     r-stato-r     delimited size
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
