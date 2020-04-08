      ***---
       WPROGMAG-BATCH.
           move 0 to link-wprogmag-status.
           if link-magazzino = spaces
              move low-value to mag-rec
              start tmagaz key is >= mag-chiave invalid continue 
              end-start
              perform until 1 = 2
                 read tmagaz next at end exit perform end-read
                 if si-mag-principale exit perform end-if
              end-perform
              move mag-codice     to prg-cod-magazzino
           else
              move link-magazzino to prg-cod-magazzino mag-codice
           end-if.

           evaluate true
           when link-close
                close progmag

           when link-batch
                |scrive un record COMPLETO DI CHIAVE (figlio) di progmag
                initialize prg-rec
                set prg-attivo to true
                move link-articolo to prg-cod-articolo
                move link-imballo  to prg-tipo-imballo
                move link-peso     to prg-peso
                move link-utf      to prg-peso-utf
                move link-non-utf  to prg-peso-non-utf
                call "C$CALLEDBY" using nome-pgm
                if nome-pgm = "insdati"
                   move link-magazzino to prg-cod-magazzino
                else
                   move mag-codice     to prg-cod-magazzino
                end-if
                if nome-pgm = "garticoli"   
                   move prg-cod-articolo to art-codice
                   read articoli  no lock
                        invalid set art-disattivo to true |--> NUOVO
                   end-read                  
                   |SICUREZZA
                   if art-stato = spaces
                      set art-attivo to true
                   end-if
                   move art-stato to prg-stato
                end-if
                move link-user     to prg-utente-creazione
                accept prg-data-creazione from century-date
                accept prg-ora-creazione  from time
                write prg-rec invalid rewrite prg-rec end-write
                |scrive un record VUOTO (padre) di progmag
                initialize prg-rec             
                move link-articolo to prg-cod-articolo
                read progmag no lock
                     |Se il padre c'è già NON va creato 
                     |(es da "mov-car-rotta" dove viene creato solo
                     |il figlio sul magazzino ROT
                     |Da articoli invece dev'essere creato tutto
                     invalid
                     move art-stato     to prg-stato 
                     move link-utf      to prg-peso-utf
                     move link-non-utf  to prg-peso-non-utf
                     move link-user     to prg-utente-creazione
                     accept prg-data-creazione from century-date
                     accept prg-ora-creazione  from time
                     write prg-rec invalid rewrite prg-rec end-write
                end-read     

           when link-update |aggiorna i valori di progmag in base alla causale           
                perform AGGIORNA-PROGMAG-CAUSALE
                move link-articolo to art-codice
                read articoli no lock invalid continue end-read
                if multiplyer(1) not = 0
                   if art-scorta = 0 or 
                      art-scorta = 2
                      move 0 to como-giacenza
                      initialize prg-chiave 
                                 replacing numeric data by zeroes
                                      alphanumeric data by spaces
                      move link-articolo to prg-cod-articolo
                      start progmag key >=  prg-chiave
                      perform until 1 = 2
                         read progmag next at end exit perform end-read
                         if prg-cod-articolo not = link-articolo
                            exit perform
                         end-if
                         move prg-cod-magazzino to mag-codice
                         read tmagaz no lock 
                              invalid continue
                          not invalid
                              if mag-per-promo-si
                                 add prg-giacenza to como-giacenza
                              end-if
                         end-read
                      end-perform              
                      move art-imballo-standard to imq-codice
                      read timbalqta

                      if como-giacenza = 0 or 
                         como-giacenza < imq-qta-imb 
                         move 0 to art-scorta
                      else                 
                         move 2 to art-scorta
                      end-if
                      rewrite art-rec
                   end-if
                end-if


           when link-delete
                perform CANCELLA-PROGMAG

           when link-value-changed
                perform CAMBIO-VALORI-ARTICOLO

           when link-costo-ultimo
                perform AGGIORNA-COSTO-ULTIMO

           end-evaluate.
