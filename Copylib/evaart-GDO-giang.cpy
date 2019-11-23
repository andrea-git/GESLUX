      ***---
       TROVA-GIACENZA.
           move 0 to idx-giac.                      
           set idx-giac to 1.
           search el-riga
           at end
              add 1             to tot-idx-giac
              move tot-idx-giac to idx-giac
              initialize prg-chiave
              move tmro-art to prg-cod-articolo
              move tmro-mag to prg-cod-magazzino
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    move 0 to giacenza-LBX
                    move 0 to impegnato-LBX
                    move 0 to imp-master-LBX
                    move 0 to imp-TRAD-LBX
                    move 0 to imp-GDO-LBX
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-articolo  not = tmro-art or
                          prg-cod-magazzino not = tmro-mag
                          exit perform
                       end-if
                       compute giacenza-LBX   = 
                               giacenza-LBX   + prg-giacenza
                       compute impegnato-LBX  = 
                               impegnato-LBX  + prg-impegnato
                       compute imp-master-LBX = 
                               imp-master-LBX + prg-imp-master
                       compute imp-TRAD-LBX   = 
                               imp-TRAD-LBX   + prg-imp-TRAD
                       compute imp-GDO-LBX    = 
                               imp-GDO-LBX    + prg-imp-GDO
                    end-perform
                 end-start

              |17/09/09: SI E' DECISO DI EVADERE SEMPRE GDO SE HO GIACENZA!!!
              compute giacenza = giacenza-LBX  -
                               ( impegnato-LBX -
                               ( imp-TRAD-LBX  + imp-GDO-LBX) )

              move giacenza        to el-giacenza(tot-idx-giac)
              move tmro-prg-chiave to el-prg-chiave(tot-idx-giac)
              move tot-idx-giac    to idx-giac

           when tmro-prg-chiave = el-prg-chiave(idx-giac)
                move el-giacenza(idx-giac)   to giacenza
           end-search.

      ***---
       CICLO-EVADIBILITA-PROMO-CICLO-1.
           set trovata-quantita to true.
           perform until 1 = 2
              if not trovata-quantita
                 exit perform
              end-if
              set trovata-quantita to false
              move low-value to tmro-rec
              set tmro-promo-si to true
              start tmp-mrordini key >= k-promo
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tmp-mrordini next
                            at end exit perform 
                       end-read


                       add 1 to counter
                       add 1 to counter2
                       if counter2 = 100
                          move counter to counter-edit
                          display counter-edit
                             upon scr-elab-Handle at column 19
                                                       line 01
                          move 0 to counter2
                       end-if
                       
                       if tmro-promo-no
                          exit perform
                       end-if
                       set cli-tipo-C    to true
                       move tmro-cod-cli to cli-codice
                       read clienti no lock
                       move tmro-cod-promo to tpr-codice
                       read tpromo no lock invalid continue end-read

                       if vecchia-evasione
                          move tmro-cod-cli     to ecd-cliente-OLD
                          move tmro-prg-destino to ecd-destino-OLD
                          read evaclides no lock 
                               invalid move 0 to ecd-gg-scadenza-vol-OLD
                          end-read
                          if ecd-gg-scadenza-vol-OLD not = 0
                             move ecd-gg-scadenza-vol-OLD 
                               to tge-gg-scadenza-vol-OLD
                          end-if
                          compute como-data = function integer-of-date
                                                  ( tpr-fine-volantino )
                          add tge-gg-scadenza-vol-OLD to como-data
                          compute como-data = function date-of-integer
                                                     ( como-data )
                       end-if
                       if como-data > data-oggi
                          set idx-giac to 1
                          search el-riga
                          at end continue
                          when tmro-prg-chiave = el-prg-chiave(idx-giac)
                               compute da-evadere =
                                       tmro-qta-o -
                                       tmro-qta-evadibile

                               if da-evadere >= tmro-qta-imballi
                                  if el-giacenza(idx-giac) >= 
                                     tmro-qta-imballi
                                     subtract tmro-qta-imballi 
                                         from el-giacenza(idx-giac)
                                     add tmro-qta-imballi 
                                      to tmro-qta-evadibile
                                     compute tmro-qta = 
                                             tmro-qta-evadibile - 
                                             tmro-qta-e
                          
                                     if tmro-si-blister
                                        add 1 to tmro-bli-evasi
                                     end-if
                          
                                     rewrite tmro-rec
                                     set trovata-quantita to true
                                     start tmp-mrordini key > k-promo
                                           invalid exit perform
                                     end-start
                                  end-if
                               end-if
                          end-search
                       end-if
                    end-perform
              end-start
           end-perform.
           set como-promo-si to true.
           perform TRATTA-BLISTER.
