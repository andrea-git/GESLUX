      ***---
       RIEMPI-TMP.
           move 0 to counter counter2.
           move 0 to RecCounter.
           move 0 to tot-kg tot-imp tot-colimp tot-cons tot-coubat.
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
                  "tmp-movmag"      delimited size
                  "_"               delimited size
                  como-data         delimited size
                  "_"               delimited size
                  como-ora          delimited size
                  ".tmp"            delimited size
                  into path-tmp-movmag
           end-string.
           open output tmp-movmag.
           set FileOpen to true.
           perform CICLO-LETTURA-MOVMAG.
           perform RIEMPI-GRIGLIA.
           move tot-colimp to lab-tot-colimp-buf
           move tot-cons   to lab-tot-cons-buf.
           move tot-coubat to lab-tot-coubat-buf.
           move tot-kg     to lab-tot-kg-buf
           move tot-imp    to lab-tot-imp-buf.
           display lab-tot-kg lab-tot-imp lab-tot-colimp
                   lab-tot-cons lab-tot-coubat.

      ***---
       CICLO-LETTURA-MOVMAG.
           move low-value to tmo-chiave.
           move como-data-from to tmo-data-movim tmo-anno.
LUBEXX     if cli-codice not = 0
LUBEXX        if tipo-rec = 1 set cli-tipo-C to true
LUBEXX        else            set cli-tipo-F to true
LUBEXX        end-if
LUBEXX        move cli-tipo-CF to tmo-tipo
LUBEXX        move cli-codice  to tmo-cod-clifor
LUBEXX        if des-prog not = 0
LUBEXX           move des-prog to tmo-destino
LUBEXX        end-if
LUBEXX        start tmovmag key is >= k2
LUBEXX              invalid set errori to true
LUBEXX        end-start
LUBEXX     else
LUBEXX        start tmovmag key is >= k-data
LUBEXX              invalid set errori to true
LUBEXX        end-start
LUBEXX     end-if.
      *
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read tmovmag next at end exit perform end-read

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
LUBEXX              if cli-codice  not = tmo-cod-clifor or              
LUBEXX                 cli-tipo-CF not = tmo-tipo
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX              if des-prog not = 0
LUBEXX                 if des-prog not = tmo-destino
LUBEXX                    exit perform
LUBEXX                 end-if
LUBEXX              end-if
LUBEXX           end-if

LUBEXX           if tmo-data-movim >= como-data-from and
LUBEXX              tmo-data-movim <= como-data-to

                    if tmo-tipo   not = Save-CF
                       set record-ok to false
                    else
                       if cli-codice    not = 0
                          if cli-codice not = tmo-cod-clifor
                             set record-ok to false
                          end-if
                          if des-prog not = 0 and
                             des-prog not = tmo-destino
                             set record-ok to false
                          end-if
                       end-if
                    end-if     
                    |31/05/2012
                    if record-ok
                       if SaveCausale not = spaces and
                          SaveCausale not = tmo-causale
                          set record-ok to false
                       end-if
                    end-if
                    |31/05/2012
LUBEXX              if record-ok
LUBEXX                 move tmo-tipo       to cli-tipo-CF
LUBEXX                 move tmo-cod-clifor to cli-codice
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
                          |ef-gdo-buf not = tmo-gdo
                          set record-ok to false
                       end-if
                    end-if

                    if record-ok     
                       move ef-age-buf to age-codice
                       if age-codice not = 0 and
                          cli-agente not = age-codice
                          |ef-gdo-buf not = tmo-gdo
                          set record-ok to false
                       end-if
                    end-if

                    if record-ok
                       perform LOOP-RMOVMAG
                    end-if
LUBEXX           else
LUBEXX              if cli-codice = 0
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX           end-if                    

              end-perform
           end-if.

      ***---
       LOOP-RMOVMAG.
           set tutto-ok to true.
           move tmo-chiave to rmo-chiave.
           move low-value to rmo-riga.
           start rmovmag key is >= rmo-chiave
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 read rmovmag next at end exit perform end-read
                 if rmo-anno  not = tmo-anno or
                    rmo-movim not = tmo-numero
                    exit perform
                 end-if
                 set record-ok     to true
                 move rmo-articolo to art-codice
                 read articoli no lock 
                      invalid set record-ok to false
                  not invalid
                      if SaveArticolo not = 0 and
                         SaveArticolo not = art-codice
                         set record-ok to false
                      end-if
                      if save-marca not = 0 and
                         save-marca not = art-marca-prodotto
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

           move art-descrizione to tmp-mov-desart.
           move rmo-anno        to tmp-mov-anno.
           move rmo-movim       to tmp-mov-movim.
           move rmo-riga        to tmp-mov-riga.
           move tmo-causale     to tmp-mov-causale.
           move rmo-imballo     to tmp-mov-imballo.
           move tmo-data-movim  to tmp-mov-data-movim.
           move rmo-qta         to tmp-mov-qta.
           move rmo-imp-cons    to tmp-mov-cons.
           move rmo-coubat      to tmp-mov-coubat.
           move rmo-netto       to tmp-mov-imp.

           move rmo-imballo to imq-codice.
           read timbalqta no lock
                invalid move 1 to imq-qta-imb
           end-read.
           move 0 to resto.
           divide rmo-qta by imq-qta-imb
                  giving tmp-mov-colli
                  remainder resto.
           if resto > 0
              add 1 to tmp-mov-colli
           end-if.

           if rmo-qta = 0
              move 1 to rmo-qta
           end-if.

           compute tmp-mov-imp-calcolato =  
                 ( rmo-netto + rmo-imp-cons + rmo-coubat ) * rmo-qta.

           move tmp-mov-causale to tca-codice.
           read tcaumag no lock invalid continue end-read.

           if tca-imponibile-pos

              compute tot-colimp = 
                      tot-colimp + ( tmp-mov-imp    * rmo-qta)
              compute tot-cons   = 
                      tot-cons   + ( tmp-mov-cons   * rmo-qta)
              compute tot-coubat = 
                      tot-coubat + ( tmp-mov-coubat * rmo-qta )

              compute tot-kg  = tot-kg + 
                   (( rmo-peso-tot-utf + rmo-peso-tot ))

              add tmp-mov-imp-calcolato to tot-imp
              set tmp-mov-causale-pos   to true
           else
           
              compute tot-colimp = 
                      tot-colimp - ( tmp-mov-imp    * rmo-qta)
              compute tot-cons   = 
                      tot-cons   - ( tmp-mov-cons   * rmo-qta)
              compute tot-coubat = 
                      tot-coubat - ( tmp-mov-coubat * rmo-qta )

              compute tot-kg  = tot-kg -
                   (( rmo-peso-tot-utf + rmo-peso-tot ))

              subtract tmp-mov-imp-calcolato from tot-imp
              set tmp-mov-causale-neg to true

              if tmp-mov-qta not = 0
                 compute tmp-mov-qta = tmp-mov-qta - ( tmp-mov-qta * 2 )
              end-if

           end-if.

           move tmo-tipo             to tmp-mov-tipo.
           move tmo-cod-clifor       to tmp-mov-codice.
           move tmo-destino          to tmp-mov-destino.
           move rmo-marca-prodotto   to tmp-mov-marca.

           move rmo-codmag           to tmp-mov-magazz.
           move rmo-articolo         to tmp-mov-articolo.
           if Save-C
              move tmo-num-fattura   to tmp-mov-numdoc  tor-num-fattura
              move tmo-data-fattura  to tmp-mov-datadoc 
              move tmo-data-fattura(1:4) to tor-anno-fattura
              if tor-num-fattura not = 0
                 move tmo-causale to tca-codice
                 read tcaumag no lock
                 if tca-nota-credito-no
                    read tordini no lock key k-fattura
                         invalid continue
                     not invalid
                         move tor-num-ord-cli to tmp-mov-num-ord-cli
                         move tor-data-ordine to tmp-mov-data-ordine
                
                         move tor-cod-agente  to tmp-mov-age-codice 
                                                 age-codice
                         read agenti no lock 
                              invalid move spaces to age-ragsoc-1 
                         end-read
                         move age-ragsoc-1   to tmp-mov-age-ragsoc
                         move tor-num-bolla  to tmp-mov-num-bolla
                         move tor-data-bolla to tmp-mov-data-bolla
                    end-read
                 else         
                    move tmo-num-fattura   to tno-num-fattura 
                    move tmo-data-fattura(1:4) to tno-anno-fattura
                    read tnotacr no lock key k-fattura
                         invalid continue
                     not invalid
                         move tno-cod-pagamento to tor-cod-pagamento
                         move tno-data to tmp-mov-data-ordine
                
                         move tno-cod-agente  to tmp-mov-age-codice 
                                                 age-codice
                         read agenti no lock 
                              invalid move spaces to age-ragsoc-1 
                         end-read
                         move age-ragsoc-1 to tmp-mov-age-ragsoc
                    end-read
                 end-if

                 move "PA"              to tblpa-codice1
                 move tor-cod-pagamento to tmp-mov-cod-pag tblpa-codice2
                 read tcodpag no lock
                      invalid 
                      move "** NON TROVATO **" to tmp-mov-des-pag
                  not invalid
                      initialize tmp-mov-des-pag
                      inspect tblpa-descrizione1 replacing trailing 
                                                 spaces by low-value
                      string  tblpa-descrizione1 delimited by low-value
                              " "                delimited by size
                              tblpa-descrizione2 delimited by size
                              into tmp-mov-des-pag
                      end-string
                 end-read
              else                                               
                 move 0      to tmp-mov-num-ord-cli tmp-mov-data-ordine
                 move spaces to tmp-mov-cod-pag tmp-mov-des-pag
              end-if
              move 0 to r-anno-m r-num-m r-riga-m
              move tmo-causale to tca-codice
              read tcaumag no lock
              if tca-nota-credito-si
                 move tmo-tor-anno   to ror-anno
                 move tmo-tor-numero to ror-num-ordine
                 move rmo-riga       to ror-num-riga
                 read rordini no lock
                      invalid continue
                  not invalid
                      move ror-anno-master   to tmp-mov-anno-master
                      move ror-numero-master to tmp-mov-numero-master
                      move ror-progr-master  to tmp-mov-progr-master
                 end-read
              end-if
           else
              move tmo-numdoc-clifor to tmp-mov-numdoc
              move tmo-data-doc      to tmp-mov-datadoc
              move spaces            to tmp-mov-cod-pag
              move spaces            to tmp-mov-des-pag
           end-if.
LUBEXX     |Devo utilizzare il peso della riga
LUBEXX     move rmo-peso             to tmp-mov-peso.

           move rmo-articolo to prg-cod-articolo.
           move spaces       to prg-cod-magazzino.
           move spaces       to prg-tipo-imballo.
           move 0            to prg-peso.
           read progmag no lock invalid continue end-read.

           set TrattamentoGDO to true.
           perform CALCOLA-COSTO-MP-COMPLETO.
           add 0,005 to costo-mp giving costo-mp-2dec.
           move costo-mp-2dec to tmp-mov-cmc.

           evaluate true
           when recupero-anagrafica  move "A" to tmp-mov-tipo-cmc
           when recupero-iniziale    move "I" to tmp-mov-tipo-cmc
           when recupero-ultimo      move "U" to tmp-mov-tipo-cmc
           when other                move " " to tmp-mov-tipo-cmc
           end-evaluate.

           move prg-chiave to prr-chiave.
           read progmagric no lock invalid continue end-read.

           if prr-costo-medio = 0
              if prr-acq-udm = 0 and prr-ini-udm = 0
                 move prr-costo-ultimo to prr-costo-medio
                 if prr-costo-medio = 0
                    move prr-costo-inizio to prr-costo-medio
                    if prr-costo-medio = 0
                       move prr-prz-anagrafica to prr-costo-medio
                    end-if
                 end-if
              end-if
           end-if.

           move prr-costo-medio to tmp-mov-cmr.

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

           move tmp-mov-tipo   to cli-tipo-CF.
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
                   move des-localita  to hid-citta-des
                   move des-indirizzo to hid-indirizzo-des
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
                   move desf-localita  to hid-citta-des
                   move desf-indirizzo to hid-indirizzo-des
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
                   "storico_mag" delimited size
                   "_"           delimited size
                   como-user     delimited low-value
                   ".csv"        delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       SCRIVI-RIGHE-EXCEL. 
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           set prima-volta to true.
           set tutto-ok    to true.
           perform until 1 = 2
              read tmp-movmag next at end exit perform end-read
              initialize r-riga cli-rec art-rec
                         replacing numeric data by zeroes
                              alphanumeric data by spaces
              move tmp-mov-causale to r-causale
              move tmp-mov-tipo    to cli-tipo-CF
              move tmp-mov-codice  to r-codice cli-codice
              read clienti no lock
                   invalid continue
               not invalid
                   move cli-gdo  to gdo-codice
                   read tgrupgdo invalid move spaces to gdo-intestazione
                   end-read

                   |Il file è già stato riempito coi record attivi
                   inspect cli-ragsoc-1 replacing trailing spaces 
                                                        by low-value
                   initialize r-ragsoc
                   string cli-ragsoc-1 delimited low-value
                          " "          delimited size
                          cli-ragsoc-2 delimited size
                          into r-ragsoc
                   end-string
                   move cli-localita  to r-localita
                   move cli-indirizzo to r-indirizzo

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
                      invalid move "** NON TROVATO **" to des-localita
                              move "** NON TROVATO **" to des-indirizzo
                 end-read
                 move des-localita  to r-localita
                 move des-indirizzo to r-indirizzo

                 move des-prov to r-prov prv-codice
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

              end-if

              move tmp-mov-destino    to r-destino
              move tmp-mov-movim      to r-numero

              move tmp-mov-num-ord-cli to r-num-ord-cli
              move tmp-mov-data-ordine to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-data-ordine
                                                   
              move tmp-mov-data-movim to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-data-mov

              move tmp-mov-data-bolla to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-data-bolla
              move tmp-mov-num-bolla  to r-num-bolla

              move tmp-mov-numdoc     to r-numdoc
              move tmp-mov-datadoc    to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-datadoc
              move tmp-mov-cod-pag    to r-cod-pag
              move tmp-mov-des-pag    to r-des-pag
              move tmp-mov-marca      to r-marca
              move tmp-mov-magazz     to r-mag
              move tmp-mov-articolo   to r-articolo art-codice
LUBEXX        move "N"                to r-utf
              move tmp-mov-cmc        to r-cmc
              move tmp-mov-tipo-cmc   to r-tipo-cmc
              move tmp-mov-cmr        to r-cmr

              if Save-C
                 move tmp-mov-anno-master   to r-anno-m
                 move tmp-mov-numero-master to r-num-m
                 move tmp-mov-progr-master  to r-riga-m
              end-if

              read articoli no lock
                   invalid continue
               not invalid
                   |Il file è già stato riempito coi record attivi
                   move art-descrizione  to r-desart
                   if art-codice-ean-1 not = 0
                      move art-codice-ean-1 to r-ean
                   else                             
                      if art-codice-ean-2 not = 0
                         move art-codice-ean-2 to r-ean
                      else                          
                         if art-codice-ean-3 not = 0
                            move art-codice-ean-3 to r-ean
                         else                          
                            if art-codice-ean-4 not = 0
                               move art-codice-ean-4 to r-ean
                            else   
                               if art-codice-ean-5 not = 0
                                  move art-codice-ean-5 to r-ean
                               end-if
                            end-if
                         end-if
                      end-if
                   end-if
LUBEXX             if art-peso-utf not = 0
LUBEXX                move "S" to r-utf
LUBEXX             end-if
              end-read
              move tmp-mov-imballo    to r-imballo
              move tmp-mov-imp        to r-imp-merce
              move tmp-mov-cons       to r-cons
              move tmp-mov-coubat     to r-coubat
              move tmp-mov-qta        to r-qta
              compute como-numero = tmp-mov-imp  + 
                                    tmp-mov-cons + 
                                    tmp-mov-coubat
              move como-numero to r-prezzo

              if tmp-mov-qta = 0
                 move tmp-mov-causale to tca-codice
                 read tcaumag no lock invalid continue end-read
                 if tca-imponibile-pos
                    move  1 to tmp-mov-qta
                 else
                    move -1 to tmp-mov-qta
                 end-if
              end-if

              move tmp-mov-colli         to r-colli         
                                          
              move tmp-mov-age-codice    to r-age-codice
              move tmp-mov-age-ragsoc    to r-age-ragsoc
                                          
              move gdo-codice            to r-gdo-codice
              move gdo-intestazione      to r-gdo-intestazione

              if save-C
                 move cli-tipo to tcl-codice r-cli-tipo
                 read ttipocli no lock
                      invalid move spaces to tcl-descrizione 
                 end-read
                 move tcl-descrizione to r-tcl-descrizione
              end-if

              multiply como-numero by tmp-mov-qta giving como-numero
              move como-numero to r-tot

              compute como-numero = tmp-mov-peso * tmp-mov-qta
              move como-numero to r-peso
              if prima-volta
                 if Save-C
                    move "Nr. Fattura"  to tipo-doc
                    move "Data Fattura" to data-doc
                    move "Anno M"       to anno-m  
                    move "Numero M"     to num-m  
                    move "Riga M"       to riga-m  
                    perform INTESTAZIONE
                 else
      *****              move "Nr. Bolla"    to tipo-doc
      *****              move "Data Bolla"   to data-doc
                    perform INTESTAZIONE
                 end-if                 
                 set prima-volta to false
              end-if
              perform SCRIVI-RIGA
           end-perform.
           close lineseq.

           perform CALL-EXCEL.

      ***---
       INTESTAZIONE.
           if Save-C
              initialize line-riga
              string "Causale"           delimited size
                     separatore          delimited size
                     "Codice"            delimited size
                     separatore          delimited size
                     "Ragione Sociale"   delimited size
                     separatore          delimited size
                     "Destino"           delimited size
                     separatore          delimited size
                     "Località"          delimited size
                     separatore          delimited size
                     "Indirizzo"         delimited size
                     separatore          delimited size
                     "Prov."             delimited size
                     separatore          delimited size
                     "Descrizione"       delimited size
                     separatore          delimited size
                     "Regione"           delimited size
                     separatore          delimited size
                     "Nr. Mov."          delimited size
                     separatore          delimited size
                     "Data Mov."         delimited size
                     separatore          delimited size
                     "Num. Ord. Cli"     delimited size
                     separatore          delimited size
                     "Data Ord. Cli"     delimited size
                     separatore          delimited size
                     "N. bolla"          delimited size
                     separatore          delimited size
                     "Data bolla"        delimited size
                     separatore          delimited size
                     tipo-doc            delimited size
                     separatore          delimited size
                     data-doc            delimited size
                     separatore          delimited size
                     "Pagamento"         delimited size
                     separatore          delimited size
                     "Condizioni"        delimited size
                     separatore          delimited size
                     "Marca"             delimited size
                     separatore          delimited size
                     "Mag."              delimited size
                     separatore          delimited size
                     "Articolo"          delimited size
                     separatore          delimited size
                     "Prodotto"          delimited size
                     separatore          delimited size
                     "EAN"               delimited size
                     separatore          delimited size
                     "Imballo"           delimited size
                     separatore          delimited size
                     "Colli"             delimited size
                     separatore          delimited size
                     "Imp. Merce"        delimited size
                     separatore          delimited size
                     "I. Consumo"        delimited size
                     separatore          delimited size
                     "COU/Cobat"         delimited size
                     separatore          delimited size
                     "Q.tà"              delimited size
                     separatore          delimited size
                     "Prezzo"            delimited size
                     separatore          delimited size
                     "Totale"            delimited size
                     separatore          delimited size
                     "Peso"              delimited size
                     separatore          delimited size
                     "UTF"               delimited size
                     separatore          delimited size
                     "CM CONS"           delimited size
                     separatore          delimited size
                     " "                 delimited size
                     separatore          delimited size
                     "CM RICAL"          delimited size
                     separatore          delimited size
                     anno-m              delimited size
                     separatore          delimited size
                     num-m               delimited size
                     separatore          delimited size
                     riga-m              delimited size
                     separatore          delimited size
                     "Agente"            delimited size
                     separatore          delimited size
                     "Ragione Sociale"   delimited size
                     separatore          delimited size
                     "Gruppo GDO"        delimited size  
                     separatore          delimited size
                     "Descrizione"       delimited size
                     separatore          delimited size
                     "Tipologia cliente" delimited size
                     separatore          delimited size
                     "Descrizione"       delimited size
                     into line-riga
              end-string
           else
              initialize line-riga
              string "Causale"           delimited size
                     separatore          delimited size
                     "Codice"            delimited size
                     separatore          delimited size
                     "Ragione Sociale"   delimited size
                     separatore          delimited size
                     "Destino"           delimited size
                     separatore          delimited size
                     "Località"          delimited size
                     separatore          delimited size
                     "Indirizzo"         delimited size
                     separatore          delimited size
                     "Nr. Mov."          delimited size
                     separatore          delimited size
                     "Data Mov."         delimited size
                     separatore          delimited size
                     "Num. Ord. Cli"     delimited size
                     separatore          delimited size
                     "Data Ord. Cli"     delimited size
                     separatore          delimited size
                     "N. bolla"          delimited size
                     separatore          delimited size
                     "Data bolla"        delimited size
                     separatore          delimited size
      *****               tipo-doc            delimited size
      *****               separatore          delimited size
      *****               data-doc            delimited size
      *****               separatore          delimited size
                     "Pagamento"         delimited size
                     separatore          delimited size
                     "Condizioni"        delimited size
                     separatore          delimited size
                     "Marca"             delimited size
                     separatore          delimited size
                     "Mag."              delimited size
                     separatore          delimited size
                     "Articolo"          delimited size
                     separatore          delimited size
                     "Prodotto"          delimited size
                     separatore          delimited size
                     "EAN"               delimited size
                     separatore          delimited size
                     "Imballo"           delimited size
                     separatore          delimited size
                     "Colli"             delimited size
                     separatore          delimited size
                     "Imp. Merce"        delimited size
                     separatore          delimited size
                     "I. Consumo"        delimited size
                     separatore          delimited size
                     "COU/Cobat"         delimited size
                     separatore          delimited size
                     "Q.tà"              delimited size
                     separatore          delimited size
                     "Prezzo"            delimited size
                     separatore          delimited size
                     "Totale"            delimited size
                     separatore          delimited size
                     "Peso"              delimited size
                     separatore          delimited size
                     "UTF"               delimited size
                     separatore          delimited size
                     "CM CONS"           delimited size
                     separatore          delimited size
                     " "                 delimited size
                     separatore          delimited size
                     "CM RICAL"          delimited size
                     separatore          delimited size
                     anno-m              delimited size
                     separatore          delimited size
                     num-m               delimited size
                     separatore          delimited size
                     riga-m              delimited size
                     separatore          delimited size
                     "Agente"            delimited size
                     separatore          delimited size
                     "Ragione Sociale"   delimited size
                     into line-riga
              end-string
           end-if.
           write line-riga.

      ***---
       SCRIVI-RIGA.
           initialize line-riga.
           if Save-C
              string r-causale     delimited size
                     separatore    delimited size
                     r-codice      delimited size
                     separatore    delimited size
                     r-ragsoc      delimited size
                     separatore    delimited size
                     r-destino     delimited size
                     separatore    delimited size
                     r-localita    delimited size
                     separatore    delimited size
                     r-indirizzo   delimited size
                     separatore    delimited size
                     r-prov        delimited size
                     separatore    delimited size
                     r-prov-d      delimited size
                     separatore    delimited size
                     r-regione     delimited size
                     separatore    delimited size
                     r-numero      delimited size
                     separatore    delimited size
                     r-data-mov    delimited size
                     separatore    delimited size
                     r-num-ord-cli delimited size
                     separatore    delimited size
                     r-data-ordine delimited size
                     separatore    delimited size
                     r-num-bolla   delimited size
                     separatore    delimited size
                     r-data-bolla  delimited size
                     separatore    delimited size
                     r-numdoc      delimited size
                     separatore    delimited size
                     r-datadoc     delimited size
                     separatore    delimited size
                     r-cod-pag     delimited size
                     separatore    delimited size
                     r-des-pag     delimited size
                     separatore    delimited size
                     r-marca       delimited size
                     separatore    delimited size
                     r-mag         delimited size
                     separatore    delimited size
                     r-articolo    delimited size
                     separatore    delimited size
                     r-desart      delimited size
                     separatore    delimited size
                     r-ean         delimited size
                     separatore    delimited size
                     r-imballo     delimited size
                     separatore    delimited size
                     r-colli       delimited size
                     separatore    delimited size
                     r-imp-merce   delimited size
                     separatore    delimited size
                     r-cons        delimited size
                     separatore    delimited size
                     r-coubat      delimited size
                     separatore    delimited size
                     r-qta         delimited size
                     separatore    delimited size
                     r-prezzo      delimited size
                     separatore    delimited size
                     r-tot         delimited size
                     separatore    delimited size
                     r-peso        delimited size
                     separatore    delimited size
                     r-utf         delimited size
                     separatore    delimited size
                     r-cmc         delimited size
                     separatore    delimited size
                     r-tipo-cmc    delimited size
                     separatore    delimited size
                     r-cmr         delimited size
                     separatore    delimited size
                     r-anno-m      delimited size
                     separatore    delimited size
                     r-num-m       delimited size
                     separatore    delimited size
                     r-riga-m      delimited size
                     separatore    delimited size
                     r-age-codice  delimited size
                     separatore    delimited size
                     r-age-ragsoc  delimited size  
                     separatore    delimited size
                     r-gdo-codice  delimited size
                     separatore    delimited size
                     r-gdo-intestazione delimited size
                     separatore    delimited size
                     r-cli-tipo    delimited size
                     separatore    delimited size
                     r-tcl-descrizione delimited size
                     into line-riga
              end-string
           else
              string r-causale     delimited size
                     separatore    delimited size
                     r-codice      delimited size
                     separatore    delimited size
                     r-ragsoc      delimited size
                     separatore    delimited size
                     r-destino     delimited size
                     separatore    delimited size
                     r-localita    delimited size
                     separatore    delimited size
                     r-indirizzo   delimited size
                     separatore    delimited size
                     r-numero      delimited size
                     separatore    delimited size
                     r-data-mov    delimited size
                     separatore    delimited size
                     r-num-ord-cli delimited size
                     separatore    delimited size
                     r-data-ordine delimited size
      *****               separatore    delimited size
      *****               r-num-bolla   delimited size
      *****               separatore    delimited size
      *****               r-data-bolla  delimited size
                     separatore    delimited size
                     r-numdoc      delimited size
                     separatore    delimited size
                     r-datadoc     delimited size
                     separatore    delimited size
                     r-cod-pag     delimited size
                     separatore    delimited size
                     r-des-pag     delimited size
                     separatore    delimited size
                     r-marca       delimited size
                     separatore    delimited size
                     r-mag         delimited size
                     separatore    delimited size
                     r-articolo    delimited size
                     separatore    delimited size
                     r-desart      delimited size
                     separatore    delimited size
                     r-ean         delimited size
                     separatore    delimited size
                     r-imballo     delimited size
                     separatore    delimited size
                     r-colli       delimited size
                     separatore    delimited size
                     r-imp-merce   delimited size
                     separatore    delimited size
                     r-cons        delimited size
                     separatore    delimited size
                     r-coubat      delimited size
                     separatore    delimited size
                     r-qta         delimited size
                     separatore    delimited size
                     r-prezzo      delimited size
                     separatore    delimited size
                     r-tot         delimited size
                     separatore    delimited size
                     r-peso        delimited size
                     separatore    delimited size
                     r-utf         delimited size
                     separatore    delimited size
                     r-cmc         delimited size
                     separatore    delimited size
                     r-tipo-cmc    delimited size
                     separatore    delimited size
                     r-cmr         delimited size
                     separatore    delimited size
                     r-anno-m      delimited size
                     separatore    delimited size
                     r-num-m       delimited size
                     separatore    delimited size
                     r-riga-m      delimited size
                     separatore    delimited size
                     r-age-codice  delimited size
                     separatore    delimited size
                     r-age-ragsoc  delimited size
                     into line-riga
              end-string
           end-if.
           write line-riga.

      ***---
       BUG-FIX-THIN-CLIENT.
           |MODIFICA PER THIN CLIENT (BUG-FIX)
           add 1 to RecCounter.
           if RecCounter = num-rec-thin-client
              move 0 to RecCounter
              display form3
           end-if.
