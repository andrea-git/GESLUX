      ***---
       RIEMPI-TMP.
           accept como-data from century-date.
           move tge-data-consolid-progmag to fine-mese.
           if mese of fine-mese = 12
              move 1 to mese of fine-mese
              add  1 to anno of fine-mese
           else
              add 1 to mese  of fine-mese
           end-if.

           evaluate mese of fine-mese
           when  1  move 31 to giorno of fine-mese
           when  2
                 divide   anno of fine-mese  by 4 giving var1
                 multiply var1               by 4 giving var1
                 if var1 = anno of fine-mese
                    move 29  to giorno of fine-mese
                 else
                    move 28  to giorno of fine-mese
                 end-if
           when  3  move 31 to giorno of fine-mese
           when  4  move 30 to giorno of fine-mese
           when  5  move 31 to giorno of fine-mese
           when  6  move 30 to giorno of fine-mese
           when  7  move 31 to giorno of fine-mese
           when  8  move 31 to giorno of fine-mese
           when  9  move 30 to giorno of fine-mese
           when 10  move 31 to giorno of fine-mese
           when 11  move 30 to giorno of fine-mese
           when 12  move 31 to giorno of fine-mese
           end-evaluate.

           if como-data > fine-mese
              move fine-mese to data-rical
           else
              compute data-rical = function INTEGER-OF-DATE(como-data)
              subtract 1 from data-rical
              compute data-rical = function DATE-OF-INTEGER(data-rical)
           end-if.

           move 0 to counter counter2.
           move 0 to RecCounter.
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

           close tmp-movmag.
           open input tmp-movmag.
           move low-value to tmp-mov-rec.
           start tmp-movmag key is >= k-cod-cli
                 invalid continue
             not invalid
                 perform GENERA-FILE-EXCEL
           end-start.

      ***---
       CICLO-LETTURA-MOVMAG.
           move low-value                 to tmo-chiave.
           add 1 to tge-data-consolid-progmag giving tmo-data-movim.
LUBEXX     start tmovmag key is >= k-data
LUBEXX           invalid set errori to true
LUBEXX     end-start.
      *
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read tmovmag next at end exit perform end-read
                 if tmo-data-movim > data-rical exit perform end-if

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon form3-handle at column 22
                                              line 03
                    move 0 to counter2
                 end-if

                 set record-ok to true

                 if tmo-fornitore
                    set record-ok to false
                 else
                    set cli-tipo-C to true
                    move tmo-cod-clifor to cli-codice
                    read clienti no lock invalid continue end-read
                    if cli-tipo not = SaveTipo
                       set record-ok to false
                    end-if
                 end-if

                 if record-ok
                    perform LOOP-RMOVMAG
                 end-if

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
                      if SaveMarca not = art-marca-prodotto
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
           initialize tmp-mov-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move art-descrizione to tmp-mov-desart.
           move rmo-anno        to tmp-mov-anno.
           move rmo-movim       to tmp-mov-movim.
           move rmo-riga        to tmp-mov-riga.
           move tmo-causale     to tmp-mov-causale.
           move tmo-data-movim  to tmp-mov-data-movim.
           move rmo-qta         to tmp-mov-qta.
           move rmo-imp-cons    to tmp-mov-cons.
           move rmo-coubat      to tmp-mov-coubat.

           move rmo-netto to tmp-mov-imp.

           if rmo-qta = 0
              move 1 to rmo-qta
           end-if.

           compute tmp-mov-imp-calcolato =  
                 ( rmo-netto + rmo-imp-cons + rmo-coubat ) * rmo-qta.
      
           move tmp-mov-causale to tca-codice.
           read tcaumag no lock invalid continue end-read.

           if tca-imponibile-neg

              if tmp-mov-qta not = 0
                 compute tmp-mov-qta = tmp-mov-qta - ( tmp-mov-qta * 2 )
              end-if

           end-if.

      *****     if tca-imponibile-pos
      *****     
      *****        compute tot-colimp = 
      *****                tot-colimp + ( tmp-mov-imp    * tmp-mov-qta)
      *****        compute tot-cons   = 
      *****                tot-cons   + ( tmp-mov-cons   * tmp-mov-qta)
      *****        compute tot-coubat = 
      *****                tot-coubat + ( tmp-mov-coubat * tmp-mov-qta )
      *****
      *****        compute tot-kg  = tot-kg + 
      *****             (( rmo-peso-tot-utf + rmo-peso-tot ))
      *****
      *****        add tmp-mov-imp-calcolato to tot-imp
      *****        set tmp-mov-causale-pos   to true
      *****     else
      *****     
      *****        compute tot-colimp = 
      *****                tot-colimp - ( tmp-mov-imp    * tmp-mov-qta)
      *****        compute tot-cons   = 
      *****                tot-cons   - ( tmp-mov-cons   * tmp-mov-qta)
      *****        compute tot-coubat = 
      *****                tot-coubat - ( tmp-mov-coubat * tmp-mov-qta )
      *****
      *****        compute tot-kg  = tot-kg -
      *****             (( rmo-peso-tot-utf + rmo-peso-tot ))
      *****
      *****        subtract tmp-mov-imp-calcolato from tot-imp
      *****        set tmp-mov-causale-neg to true
      *****             
      *****        compute tmp-mov-qta = tmp-mov-qta - ( tmp-mov-qta * 2 )
      *****
      *****     end-if.

           move tmo-tipo           to tmp-mov-tipo.
           move tmo-cod-clifor     to tmp-mov-codice.
           move tmo-destino        to tmp-mov-destino.
           move rmo-marca-prodotto to tmp-mov-marca.

           move rmo-codmag         to tmp-mov-magazz.
           move rmo-articolo       to tmp-mov-articolo.
           move tmo-num-fattura    to tmp-mov-numdoc.
           move tmo-data-fattura   to tmp-mov-datadoc.
LUBEXX     |Devo utilizzare il peso della riga
LUBEXX     move rmo-peso              to tmp-mov-peso.

           move rmo-articolo to prg-cod-articolo.
           move spaces       to prg-cod-magazzino.
           move spaces       to prg-tipo-imballo.
           move 0            to prg-peso.
           read progmag no lock invalid continue end-read.

      *****     set TrattamentoGDO to true.
      *****     perform CALCOLA-COSTO-MP-COMPLETO.
      *****     move costo-mp to tmp-mov-cmc.

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
                   "stmarcon_"   delimited size
                   "_"           delimited size
                   como-user     delimited low-value
                   ".csv"        delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       SCRIVI-RIGHE-EXCEL.
           move 0 to tot-marg.

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
              move tmp-mov-destino    to r-destino
              move tmp-mov-movim      to r-numero

              move tmp-mov-data-movim to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-data-mov
              move tmp-mov-numdoc     to r-numdoc
              move tmp-mov-datadoc    to como-data
              perform DATE-TO-SCREEN
              move como-data          to r-datadoc
              move tmp-mov-marca      to r-marca
              move tmp-mov-magazz     to r-mag
              move tmp-mov-articolo   to r-articolo art-codice
LUBEXX        move "N"                to r-utf
              move tmp-mov-tipo-cmc   to r-tipo-cmc
              move tmp-mov-cmr        to r-cmr

              read articoli no lock
                   invalid continue
               not invalid
                   |Il file è già stato riempito coi record attivi
                   move art-descrizione  to r-desart
LUBEXX             if art-peso-utf not = 0
LUBEXX                move "S" to r-utf
LUBEXX             end-if
              end-read
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
                    move 1  to tmp-mov-qta
                 else
                    move -1 to tmp-mov-qta
                 end-if

                 multiply como-numero by tmp-mov-qta giving como-marg
                 move como-marg to r-marginal
              else

                 compute como-marg =
                       ( como-numero - tmp-mov-cmr ) * tmp-mov-qta
                 move como-marg to r-marginal

              end-if

              add como-marg to tot-marg

              multiply como-numero by tmp-mov-qta giving como-numero
              move como-numero to r-tot

              compute como-numero = tmp-mov-peso * tmp-mov-qta
              move como-numero to r-peso

              if prima-volta
                 initialize line-riga
                 string "Causale"           delimited size
                        separatore          delimited size
                        "Codice"            delimited size
                        separatore          delimited size
                        "Ragione Sociale"   delimited size
                        separatore          delimited size
                        "Destino"           delimited size
                        separatore          delimited size
                        "Nr. Mov."          delimited size
                        separatore          delimited size
                        "Data Mov."         delimited size
                        separatore          delimited size
                        "N. Fattura"        delimited size
                        separatore          delimited size
                        "Data"              delimited size
                        separatore          delimited size
                        "Marca"             delimited size
                        separatore          delimited size
                        "Mag."              delimited size
                        separatore          delimited size
                        "Articolo"          delimited size
                        separatore          delimited size
                        "Prodotto"          delimited size
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
                        "COSTO STATISTICA"  delimited size
                        separatore          delimited size
                        "MARGINALITA'"      delimited size
                        into line-riga
                 end-string
                 write line-riga
                 set prima-volta to false
              end-if
              initialize line-riga
              string r-causale   delimited size
                     separatore  delimited size
                     r-codice    delimited size
                     separatore  delimited size
                     r-ragsoc    delimited size
                     separatore  delimited size
                     r-destino   delimited size
                     separatore  delimited size
                     r-numero    delimited size
                     separatore  delimited size
                     r-data-mov  delimited size
                     separatore  delimited size
                     r-numdoc    delimited size
                     separatore  delimited size
                     r-datadoc   delimited size
                     separatore  delimited size
                     r-marca     delimited size
                     separatore  delimited size
                     r-mag       delimited size
                     separatore  delimited size
                     r-articolo  delimited size
                     separatore  delimited size
                     r-desart    delimited size
                     separatore  delimited size
                     r-imp-merce delimited size
                     separatore  delimited size
                     r-cons      delimited size
                     separatore  delimited size
                     r-coubat    delimited size
                     separatore  delimited size
                     r-qta       delimited size
                     separatore  delimited size
                     r-prezzo    delimited size
                     separatore  delimited size
                     r-tot       delimited size
                     separatore  delimited size
                     r-peso      delimited size
                     separatore  delimited size
                     r-utf       delimited size
                     separatore  delimited size
                     r-cmr       delimited size
                     separatore  delimited size
                     r-marginal  delimited size
                     into line-riga
              end-string
              write line-riga
           end-perform.
           write line-riga from spaces.

           move tot-marg to r-marginal.
           initialize line-riga.
           string separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  r-marginal  delimited size
                  separatore  delimited size
                  "TOTALE MARGINALITA' (RESA/EURO)"  delimited size
                  into line-riga
           end-string.
           write line-riga.

           move SaveTipo  to ten-tipocli.
           move SaveMarca to ten-marca.
           read tendelta no lock invalid continue end-read.
           move ten-scostamento to r-marginal.
           initialize line-riga.
           string separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  r-marginal  delimited size
                  separatore  delimited size
                  "ADEGUAMENTO NEW (SCOSTAMENTO)"  delimited size
                  into line-riga
           end-string.
           write line-riga.

           compute como-numero = tot-marg + ten-scostamento.
           move como-numero to r-marginal.
           initialize line-riga.
           string separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  separatore  delimited size
                  r-marginal  delimited size
                  separatore  delimited size
                  "TENDENZA"  delimited size
                  into line-riga
           end-string.
           write line-riga.

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
