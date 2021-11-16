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
                  "tmp-ordini"      delimited size
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
           perform CICLO-LETTURA-NOTECR.
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
           move low-value to tor-chiave.
           move como-data-from to tor-data-creazione tor-anno.
LUBEXX     if cli-codice not = 0    
LUBEXX        set cli-tipo-C to true
LUBEXX        move cli-codice  to tor-cod-cli
LUBEXX        if des-prog not = 0
LUBEXX           move des-prog to tor-prg-destino
LUBEXX        end-if
LUBEXX        start tordini key is >= k1
LUBEXX              invalid set errori to true
LUBEXX        end-start
LUBEXX     else
LUBEXX        start tordini key is >= k-data
LUBEXX              invalid set errori to true
LUBEXX        end-start
LUBEXX     end-if.
      *
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
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

LUBEXX           if cli-codice not = 0
LUBEXX              if cli-codice  not = tor-cod-cli
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX              if des-prog not = 0
LUBEXX                 if des-prog not = tor-prg-destino
LUBEXX                    exit perform
LUBEXX                 end-if
LUBEXX              end-if
                 else
                    if tor-data-creazione > como-data-to
                       exit perform
                    end-if
LUBEXX           end-if
                 
LUBEXX           if tor-data-creazione >= como-data-from and
LUBEXX              tor-data-creazione <= como-data-to

                    if cli-codice    not = 0
                       if cli-codice not = tor-cod-cli
                          set record-ok to false
                       end-if
                       if des-prog not = 0 and
                          des-prog not = tor-prg-destino
                          set record-ok to false
                       end-if
                    end-if
LUBEXX              if record-ok
                       set cli-tipo-C to true
LUBEXX                 move tor-cod-cli to cli-codice
LUBEXX                 read clienti no lock
LUBEXX                      invalid initialize cli-rec
LUBEXX                 end-read
LUBEXX                 if tcl-codice not = spaces and
LUBEXX                    tcl-codice not = cli-tipo
LUBEXX                    set record-ok to false
LUBEXX                 end-if
LUBEXX                 inquire ef-cod,  value in cli-codice
LUBEXX                 inquire ef-tipo, value in tcl-codice
LUBEXX              end-if               

                    if record-ok
                       if ef-gdo-buf not = spaces and
                          ef-gdo-buf not = cli-gdo
                          set record-ok to false
                       end-if
                    end-if

                    if record-ok
                       move tor-causale to tca-codice
                       read tcaumag  no lock invalid continue end-read

                       move tor-vettore to vet-codice
                       read tvettori no lock
                            invalid move spaces to vet-sigla
                       end-read

      *****                 move tca-tipo to Save-CF
                       perform LOOP-RORDINI
                    end-if
LUBEXX           else
LUBEXX              if cli-codice = 0
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX           end-if                    

              end-perform
           end-if.

      ***---
       LOOP-RORDINI.
           set tutto-ok to true.
           move tor-chiave to ror-chiave.
           move low-value  to ror-num-riga.
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
                      if SaveArticolo not = 0 and
                         SaveArticolo not = art-codice
                         set record-ok to false
                      end-if
                      if mar-codice not = 0 and
                         mar-codice not = art-marca-prodotto
                         set record-ok  to false
                      end-if
                 end-read
                 if record-ok perform MOVE-DATI end-if
              end-perform
           end-if.

      ***---
       CICLO-LETTURA-NOTECR.
           move low-value to tor-chiave.
           move como-data-from to tno-data tno-anno.
LUBEXX     if cli-codice not = 0    
LUBEXX        set cli-tipo-C to true
LUBEXX        move cli-codice  to tno-cod-cli
LUBEXX        if des-prog not = 0
LUBEXX           move des-prog to tno-prg-destino
LUBEXX        end-if
LUBEXX        start tnotacr key is >= k1
LUBEXX              invalid set errori to true
LUBEXX        end-start
LUBEXX     else
LUBEXX        start tnotacr key is >= k-data
LUBEXX              invalid set errori to true
LUBEXX        end-start
LUBEXX     end-if.
      *
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read tnotacr next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon form3-handle at column 22
                                              line 03
                    move 0 to counter2
                 end-if

LUBEXX           if cli-codice not = 0
LUBEXX              if cli-codice  not = tno-cod-cli
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX              if des-prog not = 0
LUBEXX                 if des-prog not = tno-prg-destino
LUBEXX                    exit perform
LUBEXX                 end-if
LUBEXX              end-if
                 else
                    if tno-data > como-data-to
                       exit perform
                    end-if
LUBEXX           end-if
                 
LUBEXX           if tno-data >= como-data-from and
LUBEXX              tno-data <= como-data-to

                    if cli-codice    not = 0
                       if cli-codice not = tno-cod-cli
                          set record-ok to false
                       end-if
                       if des-prog not = 0 and
                          des-prog not = tno-prg-destino
                          set record-ok to false
                       end-if
                    end-if
LUBEXX              if record-ok
                       set cli-tipo-C to true
LUBEXX                 move tno-cod-cli to cli-codice
LUBEXX                 read clienti no lock
LUBEXX                      invalid initialize cli-rec
LUBEXX                 end-read
LUBEXX                 if tcl-codice not = spaces and
LUBEXX                    tcl-codice not = cli-tipo
LUBEXX                    set record-ok to false
LUBEXX                 end-if
LUBEXX                 inquire ef-cod,  value in cli-codice
LUBEXX                 inquire ef-tipo, value in tcl-codice
LUBEXX              end-if               

                    if record-ok
                       if ef-gdo-buf not = spaces and
                          ef-gdo-buf not = cli-gdo
                          set record-ok to false
                       end-if
                    end-if

                    if record-ok
                       move tno-causale to tca-codice
                       read tcaumag  no lock invalid continue end-read

                       move spaces to vet-sigla

      *****                 move tca-tipo to Save-CF
                       perform LOOP-RNOTACR
                    end-if
LUBEXX           else
LUBEXX              if cli-codice = 0
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX           end-if                    

              end-perform
           end-if.

      ***---
       LOOP-RNOTACR.
           set tutto-ok to true.
           move tno-chiave to rno-chiave.
           move low-value  to rno-num-riga.
           start rnotacr key is >= rno-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read rnotacr next at end exit perform end-read
                 if rno-anno     not = tno-anno or
                    rno-numero   not = tno-numero
                    exit perform
                 end-if
                 set record-ok     to true
                 move rno-cod-articolo to art-codice
                 read articoli no lock 
                      invalid set record-ok to false
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
                 if record-ok perform MOVE-DATI-NOTECR end-if
              end-perform
           end-if.     

      ***---
       MOVE-DATI.
           set trovato to true.
           initialize tmp-mov-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move art-descrizione    to tmp-mov-desart.
           move ror-anno           to tmp-mov-anno.
           move ror-num-ordine     to tmp-mov-movim.
           move ror-num-riga       to tmp-mov-riga.
           move tor-causale        to tmp-mov-causale.
           move tor-data-creazione to tmp-mov-data-movim.
           move ror-qta            to tmp-mov-qta.
           move ror-imp-consumo    to tmp-mov-cons.
           move ror-imp-cou-cobat  to tmp-mov-coubat.
           move ror-promo          to tmp-mov-promo.
                                                    
           move ror-num-colli        to tmp-mov-colli.
           move ror-prg-tipo-imballo to tmp-mov-imballo.

           move ror-imponib-merce  to tmp-mov-imp.

           move ror-add-piombo     to tmp-mov-add.

           if ror-qta = 0
              move 1 to ror-qta
           end-if.
           compute tmp-mov-imp-calcolato =  
                 ( ror-imponib-merce + 
                   ror-add-piombo    +
                   ror-imp-consumo   + 
                   ror-imp-cou-cobat ) * ror-qta.
      
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
                   (( ror-peso-utf + ror-peso-non-utf ) * ror-qta )

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
                   (( ror-peso-utf + ror-peso-non-utf ) * ror-qta )

              subtract tmp-mov-imp-calcolato from tot-imp
              set tmp-mov-causale-neg to true

              compute tmp-mov-qta = tmp-mov-qta * -1

           end-if.

           move tor-cod-cli        to tmp-mov-codice.
           move tor-prg-destino    to tmp-mov-destino.  

           move tor-gdo        to tmp-mov-gdo.
           move tor-tipocli    to tmp-mov-tipocli.

           move ror-cod-articolo to art-codice.
           read articoli no lock invalid continue end-read.

           move art-marca-prodotto to tmp-mov-marca.

           move ror-prg-cod-magazzino to tmp-mov-magazz.
           move ror-cod-articolo      to tmp-mov-articolo.
      *****     if Save-C
              move tor-num-bolla      to tmp-mov-num-bolla
              move tor-data-bolla     to tmp-mov-data-bolla

              move tor-num-fattura    to tmp-mov-numdoc
              move tor-data-fattura   to tmp-mov-datadoc
      *****     else
      *****        move tor-num-bolla      to tmp-mov-numdoc
      *****        move tor-data-bolla     to tmp-mov-datadoc
      *****     end-if.
LUBEXX     |Devo utilizzare il peso della riga
LUBEXX     move ror-prg-peso              to tmp-mov-peso.
                                                            
           move ror-chiave-ordine to tmp-mov-chiave-master.

           move tor-cod-agente    to tmp-mov-age-codice age-codice.
           read agenti no lock 
                invalid move spaces to age-ragsoc-1 
           end-read.
           move age-ragsoc-1 to tmp-mov-age-ragsoc.

           move vet-sigla to tmp-mov-vet-sigla.

           write tmp-mov-rec invalid continue end-write.

           perform BUG-FIX-THIN-CLIENT.

      ***---
       MOVE-DATI-NOTECR.
           set trovato to true.
           initialize tmp-mov-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move art-descrizione    to tmp-mov-desart.
           move rno-anno           to tmp-mov-anno.
           move rno-numero         to tmp-mov-movim.
           move rno-num-riga       to tmp-mov-riga.
           move tno-causale        to tmp-mov-causale.
           move tno-data           to tmp-mov-data-movim.
           move rno-qta            to tmp-mov-qta.
           move rno-imp-consumo    to tmp-mov-cons.
           move rno-imp-cou-cobat  to tmp-mov-coubat.
                                                    
      *     move rno-num-colli        to tmp-mov-colli.
           move rno-prg-tipo-imballo to tmp-mov-imballo.

           compute tmp-mov-imp = 
                   rno-prz-unitario  -
                   rno-imp-consumo   -
                   rno-imp-cou-cobat -
                   rno-add-piombo

           move rno-add-piombo     to tmp-mov-add.

           if rno-qta = 0
              move 1 to rno-qta
           end-if.
           compute tmp-mov-imp-calcolato =  
                   rno-prz-unitario * rno-qta.
      
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
                   (( ror-peso-utf + ror-peso-non-utf ) * ror-qta )

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
                   (( ror-peso-utf + ror-peso-non-utf ) * ror-qta )

              subtract tmp-mov-imp-calcolato from tot-imp
              set tmp-mov-causale-neg to true

              compute tmp-mov-qta = tmp-mov-qta - ( tmp-mov-qta * 2 )

           end-if.

           move tno-cod-cli        to tmp-mov-codice.
           move tno-prg-destino    to tmp-mov-destino.  

           move cli-gdo        to tmp-mov-gdo.
           move cli-tipo       to tmp-mov-tipocli.

           move rno-cod-articolo to art-codice.
           read articoli no lock invalid continue end-read.

           move art-marca-prodotto to tmp-mov-marca.

           move rno-prg-cod-magazzino to tmp-mov-magazz.
           move rno-cod-articolo      to tmp-mov-articolo.
      *****     if Save-C
      *        move tno-num-bolla      to tmp-mov-num-bolla
      *        move tno-data-bolla     to tmp-mov-data-bolla

              move tno-num-fattura    to tmp-mov-numdoc
              move tno-data-fattura   to tmp-mov-datadoc
      *****     else
      *****        move tor-num-bolla      to tmp-mov-numdoc
      *****        move tor-data-bolla     to tmp-mov-datadoc
      *****     end-if.
LUBEXX     |Devo utilizzare il peso della riga
LUBEXX     move rno-prg-peso              to tmp-mov-peso.
                                                            
      *     move rno-chiave-ordine to tmp-mov-chiave-master.

           move tno-cod-agente    to tmp-mov-age-codice age-codice.
           read agenti no lock 
                invalid move spaces to age-ragsoc-1 
           end-read.
           move age-ragsoc-1 to tmp-mov-age-ragsoc.

           move vet-sigla to tmp-mov-vet-sigla.

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
                   "movim_art"   delimited size
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
                   move cli-prov     to r-prov prv-codice
                   read tprov no lock 
                        invalid continue
                    not invalid
                        move prv-descrizione to r-prov-d
                        move prv-regione to reg-codice
                        read tregioni no lock
                             invalid continue
                        end-read
                        move reg-descrizione to r-regione
                   end-read
              end-read

              if tmp-mov-destino not = 0
                 move tmp-mov-codice  to des-codice
                 move tmp-mov-destino to des-prog
                 read destini no lock
                      invalid continue
                  not invalid 
                      move des-localita to r-localita
                      move des-prov     to r-prov prv-codice
                      read tprov no lock 
                           invalid continue
                       not invalid
                           move prv-descrizione to r-prov-d
                           move prv-regione to reg-codice
                           read tregioni no lock
                                invalid continue
                           end-read
                           move reg-descrizione to r-regione
                      end-read
                 end-read
              end-if

              move spaces   to gdo-intestazione
              move cli-gdo  to gdo-codice
              read tgrupgdo no lock invalid continue end-read
              move gdo-intestazione to r-gdo

              move spaces      to gdo-intestazione
              move tmp-mov-gdo to gdo-codice
              read tgrupgdo no lock invalid continue end-read

              move spaces to tcl-descrizione
              move tmp-mov-tipocli to tcl-codice
              read ttipocli no lock invalid continue end-read
                                  
              move tmp-mov-destino    to r-destino
              move tmp-mov-movim      to r-numero

              move tmp-mov-data-movim to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-data-mov

              move tmp-mov-num-bolla  to r-num-bolla
              move tmp-mov-data-bolla to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-data-bolla

              move tmp-mov-numdoc     to r-numdoc
              move tmp-mov-datadoc    to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-datadoc
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
              compute como-numero = tmp-mov-imp  + 
                                    tmp-mov-add  +
                                    tmp-mov-cons + 
                                    tmp-mov-coubat
              move como-numero to r-prezzo
              multiply como-numero by tmp-mov-qta giving como-numero
              move como-numero to r-tot
              
              move tmp-mov-anno-master   to r-anno-m  
              move tmp-mov-numero-master to r-numero-m
              move tmp-mov-progr-master  to r-riga-m

              move tmp-mov-age-codice    to r-age-codice
              move tmp-mov-age-ragsoc    to r-age-ragsoc

              compute como-numero = tmp-mov-peso * tmp-mov-qta
              move como-numero to r-peso

              move tmp-mov-vet-sigla to r-vet-sigla

              compute como-numero = tmp-mov-imp * tmp-mov-qta
              move como-numero to r-imp-tot

              compute como-numero = tmp-mov-add * tmp-mov-qta
              move como-numero to r-add-tot

              compute como-numero = tmp-mov-cons * tmp-mov-qta
              move como-numero to r-cons-tot

              compute como-numero = tmp-mov-coubat * tmp-mov-qta
              move como-numero to r-cou-tot

              if prima-volta
      *****           if Save-C
                    move "Nr. Fattura"  to tipo-doc
                    move "Data Fattura" to data-doc
      *****           else
      *****              move "Nr. Bolla"    to tipo-doc
      *****              move "Data Bolla"   to data-doc
      *****           end-if
                 initialize line-riga
                 string "Causale"              delimited size
                        separatore             delimited size
                        "Codice"               delimited size
                        separatore             delimited size
                        "Gruppo GDO"           delimited size
                        separatore             delimited size
                        "Gruppo GDO originale" delimited size
                        separatore             delimited size
                        "Tipol. cliente"       delimited size
                        separatore             delimited size
                        "Ragione Sociale"      delimited size
                        separatore             delimited size
                        "Destino"              delimited size
                        separatore             delimited size
                        "Località"             delimited size
                        separatore             delimited size
                        "Prov."                delimited size
                        separatore             delimited size
                        "Descrizione"          delimited size
                        separatore             delimited size
                        "Regione"              delimited size
                        separatore             delimited size
                        "Nr. Ord."             delimited size
                        separatore             delimited size
                        "Data Creazione"       delimited size
                        separatore             delimited size
                        "Nr. Bolla"            delimited size
                        separatore             delimited size
                        "Data Bolla"           delimited size
                        separatore             delimited size
                        "Nr. Fattura"          delimited size
                        separatore             delimited size
                        "Data Fattura"         delimited size
                        separatore             delimited size
                        "Marca"                delimited size
                        separatore             delimited size
                        "Mag."                 delimited size
                        separatore             delimited size
                        "Articolo"             delimited size
                        separatore             delimited size
                        "Prodotto"             delimited size
                        separatore             delimited size
                        "Imballo"              delimited size
                        separatore             delimited size
                        "Colli"                delimited size
                        separatore             delimited size
                        "Imp. Merce"           delimited size
                        separatore             delimited size
                        "Add. Pb"              delimited size
                        separatore             delimited size
                        "I. Consumo"           delimited size
                        separatore             delimited size
                        "COU/Cobat"            delimited size
                        separatore             delimited size
                        "Q.tà"                 delimited size
                        separatore             delimited size
                        "Prezzo"               delimited size
                        separatore             delimited size
                        "Totale"               delimited size
                        separatore             delimited size
                        "Peso"                 delimited size
                        separatore             delimited size
                        "UTF"                  delimited size
                        separatore             delimited size
                        "Promo"                delimited size
                        separatore             delimited size
                        "Anno M"               delimited size
                        separatore             delimited size
                        "Numero M"             delimited size
                        separatore             delimited size
                        "Riga M"               delimited size
                        separatore             delimited size
                        "Agente"               delimited size 
                        separatore             delimited size
                        "Ragione Sociale"      delimited size
                        separatore             delimited size
                        "Sigla vettore"        delimited size
                        separatore             delimited size
                        "Imp TOT"              delimited size
                        separatore             delimited size
                        "Add. Pb TOT"          delimited size
                        separatore             delimited size
                        "I. Cons. TOT"         delimited size
                        separatore             delimited size
                        "COU/Cobat TOT"        delimited size
                        into line-riga         
                 end-string                    
                 write line-riga
                 set prima-volta to false
              end-if
              initialize line-riga
              string r-causale        delimited size
                     separatore       delimited size
                     r-codice         delimited size
                     separatore       delimited size
                     r-gdo            delimited size
                     separatore       delimited size
                     gdo-intestazione delimited size
                     separatore       delimited size
                     tcl-descrizione  delimited size
                     separatore       delimited size
                     r-ragsoc         delimited size
                     separatore       delimited size
                     r-destino        delimited size
                     separatore       delimited size
                     r-localita       delimited size
                     separatore       delimited size
                     r-prov           delimited size
                     separatore       delimited size
                     r-prov-d         delimited size
                     separatore       delimited size
                     r-regione        delimited size
                     separatore       delimited size
                     r-numero         delimited size
                     separatore       delimited size
                     r-data-mov       delimited size
                     separatore       delimited size
                     r-num-bolla      delimited size
                     separatore       delimited size
                     r-data-bolla     delimited size
                     separatore       delimited size
                     r-numdoc         delimited size
                     separatore       delimited size
                     r-datadoc        delimited size
                     separatore       delimited size
                     r-marca          delimited size
                     separatore       delimited size
                     r-mag            delimited size
                     separatore       delimited size
                     r-articolo       delimited size
                     separatore       delimited size
                     r-desart         delimited size
                     separatore       delimited size
                     r-imballo        delimited size
                     separatore       delimited size
                     r-colli          delimited size
                     separatore       delimited size
                     r-imp-merce      delimited size
                     separatore       delimited size
                     r-add-pb         delimited size
                     separatore       delimited size
                     r-cons           delimited size
                     separatore       delimited size
                     r-coubat         delimited size
                     separatore       delimited size
                     r-qta            delimited size
                     separatore       delimited size
                     r-prezzo         delimited size
                     separatore       delimited size
                     r-tot            delimited size
                     separatore       delimited size
                     r-peso           delimited size
                     separatore       delimited size
                     r-utf            delimited size
                     separatore       delimited size
                     r-promo          delimited size
                     separatore       delimited size
                     r-anno-m         delimited size
                     separatore       delimited size
                     r-numero-m       delimited size
                     separatore       delimited size
                     r-riga-m         delimited size
                     separatore       delimited size
                     r-age-codice     delimited size
                     separatore       delimited size
                     r-age-ragsoc     delimited size
                     separatore       delimited size
                     r-vet-sigla      delimited size
                     separatore       delimited size
                     r-imp-tot        delimited size
                     separatore       delimited size
                     r-add-tot        delimited size
                     separatore       delimited size
                     r-cons-tot       delimited size
                     separatore       delimited size
                     r-cou-tot        delimited size

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
