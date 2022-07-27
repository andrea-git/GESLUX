      ***---
       CALCOLA-FABBISOGNO.                         
           move 1               to idx.
           move start-data(5:2) to mese.
           move 0 to tot-anno
           perform 12 times
              if mese > 12
                 move 1 to mese
              end-if
              if mese-end = 12
                 move ord2-qta-past-m(mese) to el-qta(idx)
                 add  ord2-qta-past-m(mese) to tot-anno
              else
                 if mese > mese-end
                    move ord2-qta-past-m(mese) to el-qta(idx)
                    add  ord2-qta-past-m(mese) to tot-anno
                 else
                    move ord2-qta-corr-m(mese) to el-qta(idx)
                    add  ord2-qta-corr-m(mese) to tot-anno
                 end-if
              end-if
              add 1 to mese idx
           end-perform.
                                                 
           |IN CASO DI MODIFICA ALLA FORMULA
           |ALLINEARE CON mail_giacenze2.cbl
           compute SS1 = ord2-riordino - ord2-consegna.

           move ord2-articolo to prg-cod-articolo.
           perform TROVA-GIACENZA-IMPEGNATO.

           move ord2-articolo to prg-cod-articolo.
           move spaces        to prg-tipo-imballo.
           move spaces        to prg-cod-magazzino.
           move 0             to prg-peso.
           read progmag  no lock invalid continue end-read.

           move ord2-articolo to art-codice.
           read articoli no lock 
                invalid
                move 0 to SS2 ord2-mese-rif
            not invalid
                set ord2-programmazione-no to true
                move art-scorta to sco-codice
                read tscorte no lock 
                     invalid
                     move 0 to SS2
                 not invalid
                     if sco-programmazione-si
                        set ord2-programmazione-si to true
                     end-if
                     compute SS2 = SS1 * sco-liv-scorta / 100
                     if ricalcolo or ord2-mese-rif = 0
                        move sco-m-rif to ord2-mese-rif
      *****                  if ord2-mese-rif < 2 and 
      *****                     art-mag-std not = "LBX"
      *****                     move 2 to ord2-mese-rif
      *****                  end-if
                     end-if
                end-read
           end-read.
           if ord2-mese-scelto = 0
              move ord2-mese-rif to ord2-mese-scelto
           end-if.             
           if art-collegato not = 0
              move sco-codice to save-sco-codice
              move art-codice to save-art-codice
              perform until 1 = 2
                 move art-collegato to art-codice
                 if art-collegato = art-codice
                    exit perform
                 end-if
                 read articoli no lock
                 if art-collegato = 0
                    exit perform
                 end-if
              end-perform
              set sco-immediato-si to false
              move art-scorta to sco-codice
              read tscorte  no lock invalid continue end-read
              move sco-immediato   to ultimo-sco-immediato
              move save-art-codice to art-codice
              read articoli no lock
              move save-sco-codice to sco-codice
              read tscorte no lock
           else
              move sco-immediato to ultimo-sco-immediato
           end-if.
           if ultimo-sco-immediato-si |and art-collegato = 0
              |||||||||||||
              if articolo-singolo |nell'altro caso lo fa dopo
                 compute qta-utile = ord2-promo     +
                                     como-impegnato -
                                     como-giacenza  -
                                     prg-ordinato-6
                 if qta-utile > 0
                    compute F1 = qta-utile
                    move F1 to F2 F3 F4 F5 F6
                 else
                    move 0 to F1 F2 F3 F4 F5 F6
                 end-if
              end-if
              ||||||||||||||||||
      **********        compute qta-utile = ord2-promo     +
      **********                            como-impegnato -
      **********                            como-giacenza  -
      **********                            prg-ordinato-6
      **********        if qta-utile > 0
      **********           compute F1 = qta-utile
      **********           move F1 to F2 F3 F4 F5 F6
      **********        else
      **********           move 0 to F1 F2 F3 F4 F5 F6
      **********        end-if
           else
              compute QTA1 rounded = el-qta(2) * ord2-scost / 100
              compute QTA2 = ord2-riordino  - ord2-giac
              compute QTA3 = ord2-impegnato

              compute F1 rounded = QTA1 + QTA2 + QTA3 + SS2 - 
              prg-ordinato-1 + ord2-promo-mese(1)
              compute F2 rounded = F1 + (el-qta(3) * ord2-scost / 100) - 
              prg-ordinato-2 + 
              prg-ordinato-1 + ord2-promo-mese(2)
              compute F3 rounded = F2 + (el-qta(4) * ord2-scost / 100) - 
              prg-ordinato-3 +
              prg-ordinato-2 + ord2-promo-mese(3)
              compute F4 rounded = F3 + (el-qta(5) * ord2-scost / 100) - 
              prg-ordinato-4 +
              prg-ordinato-3 + ord2-promo-mese(4)
              compute F5 rounded = F4 + (el-qta(6) * ord2-scost / 100) - 
              prg-ordinato-5 +
              prg-ordinato-4 + ord2-promo-mese(5)
              compute F6 rounded = F5 + (el-qta(7) * ord2-scost / 100) - 
              prg-ordinato-6 +
              prg-ordinato-5 + ord2-promo-mese(6)
           end-if.               

      *     perform DETERMINA-CATENA

           evaluate true
           when articolo-singolo
                move F1 to ord2-fabb-qta(1)
                move F2 to ord2-fabb-qta(2)
                move F3 to ord2-fabb-qta(3)
                move F4 to ord2-fabb-qta(4)
                move F5 to ord2-fabb-qta(5)
                move F6 to ord2-fabb-qta(6)
                move 0  to ord2-sostituto
           when articolo-catena
                move como-ult-art  to ord2-sostituto
                                      tmp-ord-articolo
                move ord2-mag      to tmp-ord-mag

                read tmp-ordfor no lock
                   invalid 
                      move 0 to tmp-ord-media-vend
                                tmp-ord-media-vend-no-peaks
                                tmp-ord-fabb-qta(1)
                                tmp-ord-fabb-qta(2)
                                tmp-ord-fabb-qta(3)
                                tmp-ord-fabb-qta(4)
                                tmp-ord-fabb-qta(5)
                                tmp-ord-fabb-qta(6)
                                tmp-ord-promo
                                tmp-ord-impegnato
                                tmp-ord-giacenza
                                tmp-ord-ordinato
                end-read

                add ord2-promo     to tmp-ord-promo
                add como-impegnato to tmp-ord-impegnato
                add como-giacenza  to tmp-ord-giacenza
                add prg-ordinato-6 to tmp-ord-ordinato
                                       
                if not ultimo-sco-immediato-si
                   add F1 to tmp-ord-fabb-qta(1)
                   add F2 to tmp-ord-fabb-qta(2)
                   add F3 to tmp-ord-fabb-qta(3)
                   add F4 to tmp-ord-fabb-qta(4)
                   add F5 to tmp-ord-fabb-qta(5)
                   add F6 to tmp-ord-fabb-qta(6)
                end-if

                write tmp-ord-rec
                      invalid
                      rewrite tmp-ord-rec
                              invalid continue
                      end-rewrite
                end-write              

      *       metti i valori nella somma
                move 0 to ord2-fabb-qta(1)
                move 0 to ord2-fabb-qta(2)
                move 0 to ord2-fabb-qta(3)
                move 0 to ord2-fabb-qta(4)
                move 0 to ord2-fabb-qta(5)
                move 0 to ord2-fabb-qta(6)
           when articolo-ultimo
                move zero to ord2-sostituto

                move art-codice to tmp-ord-articolo
                move ord2-mag   to tmp-ord-mag

                read tmp-ordfor no lock
                   invalid 
                      move zero to tmp-ord-media-vend
                                   tmp-ord-media-vend-no-peaks  
                                   tmp-ord-fabb-qta(1)
                                   tmp-ord-fabb-qta(2)
                                   tmp-ord-fabb-qta(3)
                                   tmp-ord-fabb-qta(4)
                                   tmp-ord-fabb-qta(5)
                                   tmp-ord-fabb-qta(6)
                                   tmp-ord-promo
                                   tmp-ord-impegnato
                                   tmp-ord-giacenza
                                   tmp-ord-ordinato
                end-read

                ||||||||||||||||||
                add ord2-promo     to tmp-ord-promo
                add como-impegnato to tmp-ord-impegnato
                add como-giacenza  to tmp-ord-giacenza
                add prg-ordinato-6 to tmp-ord-ordinato
                
                write tmp-ord-rec
                      invalid
                      rewrite tmp-ord-rec
                              invalid continue
                      end-rewrite
                end-write

                if ultimo-sco-immediato-si
                   compute qta-utile = tmp-ord-promo     +
                                       tmp-ord-impegnato -
                                       tmp-ord-giacenza  -
                                       tmp-ord-ordinato
                   if qta-utile > 0
                      compute F1 = qta-utile
                      move F1 to F2 F3 F4 F5 F6
                   else
                      move 0 to F1 F2 F3 F4 F5 F6
                   end-if
                   move F1 to ord2-fabb-qta(1)
                   move F2 to ord2-fabb-qta(2) 
                   move F3 to ord2-fabb-qta(3) 
                   move F4 to ord2-fabb-qta(4) 
                   move F5 to ord2-fabb-qta(5) 
                   move F6 to ord2-fabb-qta(6) 
                else
                   add F1 to tmp-ord-fabb-qta(1) giving ord2-fabb-qta(1) 
                   add F2 to tmp-ord-fabb-qta(2) giving ord2-fabb-qta(2) 
                   add F3 to tmp-ord-fabb-qta(3) giving ord2-fabb-qta(3) 
                   add F4 to tmp-ord-fabb-qta(4) giving ord2-fabb-qta(4) 
                   add F5 to tmp-ord-fabb-qta(5) giving ord2-fabb-qta(5) 
                   add F6 to tmp-ord-fabb-qta(6) giving ord2-fabb-qta(6)
                end-if     
                ||||||||||||||||
                 
           end-evaluate.

      ***---
       CREA-ORDFOR2.     
           set tutto-ok to true.                                 
           accept data-calcolo from century-date.
           accept como-data    from century-date.  
           accept como-ora     from time.
                                
           open exclusive output ordfor2.
           close      ordfor2.
           open i-o   ordfor2 allowing readers.
           open input ordfor.        
           move low-value to ord-rec.
           start ordfor key >= ord-chiave
                 invalid set errori to true
           end-start.
           perform until 1 = 2 or errori

              read ordfor next at end exit perform end-read  
              initialize ord2-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces
              move ord-mag                       to ord2-mag
              move ord-articolo                  to ord2-articolo
              if articolo-fisso not = 0
                 if articolo-fisso not = ord2-articolo
                    exit perform cycle
                 end-if
              end-if
              move ord-art-descrizione           to ord2-art-descrizione
              move ord-marca                     to ord2-marca
              move ord-qta-imb                   to ord2-qta-imb
              move ord-scost                     to ord2-scost
              move ord-lead-time                 to ord2-lead-time
      *****        move ord-lead-time                 to ord2-lead-time-f
              |Imposto al max possibile
              move 120                           to ord2-lead-time-f
              move ord-giac                      to ord2-giac
              move ord-scorta                    to ord2-scorta
              move ord-qta-vendita-anno-corrente 
                to ord2-qta-vendita-anno-corrente
              move ord-qta-vendita-anno-passsato 
                to ord2-qta-vendita-anno-passsato
                                    
              perform AGGIUNGI-QTA-VENDUTI

              move ord-media-vend                to ord2-media-vend
              move ord-consegna                  to ord2-consegna
              move ord-riordino                  to ord2-riordino

              move ord2-articolo to art-codice
              read articoli no lock 
              
              if art-qta-epal not = 0
                 move art-qta-epal to ord2-qta-bancale
              else
                 move art-qta-std  to ord2-qta-bancale
              end-if

              accept ord2-ora-creazione  from time
              accept ord2-data-creazione from century-date
              move user-codi to ord2-utente-creazione
              write ord2-rec invalid continue end-write
           end-perform. 
           close ordfor.
                        
           move low-value to art-rec.                         
           start articoli key >= art-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read articoli next  at end exit perform end-read
              move "LBX"       to ord2-mag
              move art-codice  to ord2-articolo
              if art-attivo or art-bloccato
                 |Altrimenti non ho il progressivo di riferimento
                 read ordfor2 no lock 
                      invalid perform AGGIUNGI-ARTICOLO-ANAGRFICA
                 end-read
              end-if
           end-perform.  

      ***---
       AGGIUNGI-ARTICOLO-ANAGRFICA.
           initialize ord2-dati replacing numeric data by zeroes
                                     alphanumeric data by spaces.
           move art-codice           to ord2-articolo.
           move art-descrizione      to ord2-art-descrizione.
           move art-marca-prodotto   to ord2-marca.

           perform AGGIUNGI-QTA-VENDUTI.

           if art-qta-epal not = 0
              move art-qta-epal to ord2-qta-bancale
           else
              move art-qta-std  to ord2-qta-bancale
           end-if.

           perform IMBALLO-MAGGIOR-GIACENZA.
           move SaveImballo      to imq-codice ord2-imballo.
           move SavePeso         to ord2-peso.
           read timbalqta no lock
                invalid continue
            not invalid move imq-qta-imb to ord2-qta-imb
           end-read.
      
           move 0 to ord2-lead-time.
      *****     if art-cod-fornitore = 0
              move art-marca-prodotto  to mar-codice
              read tmarche no lock
                   invalid move 0 to mar-scostamento 
                                     mar-lead-time
               not invalid move mar-lead-time to ord2-lead-time
              end-read
      *****     else
      *****        move low-value         to rlis-rec
      *****        move art-cod-fornitore to rlis-fornitore
      *****        move art-codice        to rlis-articolo
      *****        start rlistini key >= rlis-k-art of rlistini
      *****              invalid
      *****          not invalid
      *****              perform until 1 = 2
      *****                 read rlistini next 
      *****                      at end exit perform 
      *****                 end-read
      *****                 if rlis-articolo  not = art-codice or
      *****                    rlis-fornitore not = art-cod-fornitore
      *****                    exit perform
      *****                 end-if
      *****                 if data-calcolo >= rlis-ini-val and
      *****                    data-calcolo <= rlis-fine-val
      *****                    move rlis-lead-time to ord2-lead-time
      *****                 end-if
      *****              end-perform
      *****        end-start
      *****        if ord2-lead-time = 0
      *****           move mar-lead-time to ord2-lead-time
      *****        end-if
      *****     end-if.
           move mar-scostamento to ord2-scost.              
           move ord2-articolo to prg-cod-articolo.
           perform TROVA-GIACENZA-IMPEGNATO.

           move art-codice to prg-cod-articolo.
           move spaces     to prg-cod-magazzino.
           move 0          to prg-peso.
           move spaces     to prg-tipo-imballo.
           read progmag no lock 
                invalid continue
            not invalid
                compute ord2-giac = como-giacenza - como-impegnato
                perform CALCOLA-COSTO-MP
                add 0,005 to costo-mp giving ord2-costo-mp
           end-read.
           move art-scorta          to ord2-scorta.
           write ord2-rec.

      ***---
       AGGIORNA-RECORD.
      *    controllo che tipo di articolo è
           perform DETERMINA-CATENA.

           accept data-calcolo from century-date.
           accept como-data    from century-date.
           if ricalcolo or ord2-lead-time = 0
      *****        if art-cod-fornitore = 0
                 move art-marca-prodotto  to mar-codice
                 read tmarche no lock
                      invalid move 0 to mar-scostamento mar-lead-time
                  not invalid move mar-lead-time to ord2-lead-time
                 end-read
      *****        else
      *****           move low-value         to rlis-rec
      *****           move art-cod-fornitore to rlis-fornitore
      *****           move art-codice        to rlis-articolo
      *****           start rlistini key >= rlis-k-art of rlistini
      *****                 invalid
      *****             not invalid
      *****                 perform until 1 = 2
      *****                    read rlistini next 
      *****                         at end exit perform 
      *****                    end-read
      *****                    if rlis-articolo  not = art-codice or
      *****                       rlis-fornitore not = art-cod-fornitore
      *****                       exit perform
      *****                    end-if
      *****                    if como-data >= rlis-ini-val and
      *****                       como-data <= rlis-fine-val
      *****                       move rlis-lead-time to ord2-lead-time
      *****                       exit perform
      *****                    end-if
      *****                 end-perform
      *****           end-start
      *****           move art-marca-prodotto  to mar-codice
      *****           read tmarche no lock
      *****                invalid move 0 to mar-scostamento mar-lead-time
      *****            not invalid move mar-lead-time to ord2-lead-time
      *****           end-read
      *****           if ord2-lead-time = 0
      *****              move mar-lead-time to ord2-lead-time
      *****           end-if
      *****        end-if
              move mar-scostamento     to ord2-scost
           end-if.
                           
           if ord2-lead-time-f = 0
      *****        move ord2-lead-time to ord2-lead-time-f
              |Imposto max valore possibile
              move 120 to ord2-lead-time-f
           end-if.
           perform CALCOLA-COSTO-MP.

           add 0,005 to costo-mp giving ord2-costo-mp.
           compute ord2-giac = como-giacenza - como-impegnato.

           |17/12/2007 Come da richiesta di Walter: invece di usare
           |l'imballo standard prendo quello con > giacenza assoluta
           perform IMBALLO-MAGGIOR-GIACENZA.
      ****     move art-imballo-standard to imq-codice.
           move SaveImballo      to imq-codice ord2-imballo.
           move SavePeso         to ord2-peso.

           read timbalqta no lock
                invalid move 0 to imq-qta-imb
           end-read.
           
           move art-descrizione     to ord2-art-descrizione.|SOLO PER L'ORDINAMENTO

           if art-qta-epal not = 0
              move art-qta-epal to ord2-qta-bancale
           else
              move art-qta-std  to ord2-qta-bancale
           end-if.

           move art-marca-prodotto  to ord2-marca.
           move imq-qta-imb         to ord2-qta-imb.
LUBEXX     move art-scorta          to ord2-scorta.
      *****     move mar-lead-time       to ord2-lead-time.
           perform VALORIZZA-OCCURS.
           perform CALCOLA-CONSEGNA-PERIODO.
           perform CALCOLA-PUNTO-RIORDINO.
           if ricalcolo
              move 0 to ord2-impegnato
           end-if.
           perform CALCOLA-FABBISOGNO.
           if ricalcolo
      *****        if ord2-mese-rif = 0
      *****           set ord2-fabbisogno-zero to true
      *****        else
                 perform COPRI-FABBISOGNO
                 perform SCRIVI-COPERTURA
      *****        end-if
           else
              if ord2-no-conferma
                 perform COPRI-FABBISOGNO
                 perform SCRIVI-COPERTURA
              end-if
           end-if.
           perform VALUTA-COPERTURA.
           move user-codi            to ord2-utente-modifica.
           accept  ord2-ora-modifica  from time.
           accept  ord2-data-modifica from century-date.
           rewrite ord2-rec invalid continue end-rewrite.

      ***---
       IMBALLO-MAGGIOR-GIACENZA.
           move 0 to giacenza.
           initialize prg-chiave replacing numeric data by zeroes
                                      alphanumeric data by spaces.
           move art-codice       to prg-cod-articolo.
           start progmag key >= prg-chiave
                 invalid move art-imballo-standard to SaveImballo
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo  not = art-codice
                       exit perform
                    end-if
                    if prg-cod-magazzino     = "LBX"  and
                       prg-tipo-imballo  not = spaces and
                       prg-peso          not = 0
                       move prg-giacenza to giac-positiva
                       if giac-positiva >= giacenza
                          move prg-giacenza     to giacenza
                          move prg-tipo-imballo to SaveImballo
                          move prg-peso         to SavePeso
                       end-if
                    end-if
                 end-perform
           end-start.
           move SaveImballo to prg-tipo-imballo.

      ***---
       VALORIZZA-OCCURS.
           move 1 to idx.
           move 0 to wk-campo.
           initialize occurs-qta.
           move mese-start to mese.
           perform 12 times
              if mese > 12
                 move 1 to mese
              end-if

LUBEXX        if mese-end = 12
                 move ord2-qta-past-m(mese) to el-qta(mese)
              else
                 |Superando il mese finale significa
                 |che siamo nell'anno precedente
                 if mese > mese-end
                    move ord2-qta-past-m(mese) to el-qta(mese)
                 else
                    move ord2-qta-corr-m(mese) to el-qta(mese)
                 end-if
              end-if
                               
              perform COMPONI-TITOLO-MESI
              add el-qta(mese) to wk-campo
              add 1 to mese idx
           end-perform.
           move 0 to ord2-media-vend.
           compute ord2-media-vend rounded = wk-campo / 12.
           move  9999999 to mese-min.
           move -9999999 to mese-max.
           move 0 to idx idx-max  idx-min 
                         ord2-media-vend-no-peaks.
           perform 12 times
              add 1 to idx
              if el-qta(idx) > mese-max
                 move el-qta(idx) to mese-max
                 move idx to idx-max
              end-if
              if el-qta(idx) < mese-min
                 move el-qta(idx) to mese-min
                 move idx to idx-min
              end-if
           end-perform.
           move ord2-media-vend to el-qta(idx-max).
           move ord2-media-vend to el-qta(idx-min).
           compute ord2-media-vend-no-peaks =
                 ( el-qta(1)  + el-qta(2)  + el-qta(3) + 
                   el-qta(4)  + el-qta(5)  + el-qta(6) + 
                   el-qta(7)  + el-qta(8)  + el-qta(9) + 
                   el-qta(10) + el-qta(11) + el-qta(12) ) / 12.
                                              
           evaluate true
           when articolo-singolo
                continue
           when articolo-catena
                move como-ult-art  to ord2-sostituto
                                      tmp-ord-articolo
                move ord2-mag      to tmp-ord-mag
                                            
                read tmp-ordfor no lock
                   invalid 
                      move zero to tmp-ord-media-vend
                                   tmp-ord-media-vend-no-peaks
                                   tmp-ord-fabb-qta(1)
                                   tmp-ord-fabb-qta(2)
                                   tmp-ord-fabb-qta(3)
                                   tmp-ord-fabb-qta(4)
                                   tmp-ord-fabb-qta(5)
                                   tmp-ord-fabb-qta(6)
                                   tmp-ord-promo
                                   tmp-ord-impegnato
                                   tmp-ord-giacenza
                                   tmp-ord-ordinato
                end-read

      *****          add ord2-media-vend   to tmp-ord-media-vend
      *****          add ord2-media-vend-no-peaks
      *****                                to tmp-ord-media-vend-no-peaks

                write tmp-ord-rec
                      invalid
                      rewrite tmp-ord-rec invalid continue end-rewrite
                end-write

      *       metti i valori nella somma
      *****          move zero to ord2-media-vend
      *****          move zero to ord2-media-vend-no-peaks
           when articolo-ultimo
                move zero to ord2-sostituto

                move art-codice to tmp-ord-articolo
                move ord2-mag   to tmp-ord-mag

                read tmp-ordfor no lock
                     invalid 
                     move zero to tmp-ord-media-vend
                                  tmp-ord-media-vend-no-peaks
                                  tmp-ord-fabb-qta(1)
                                  tmp-ord-fabb-qta(2)
                                  tmp-ord-fabb-qta(3)
                                  tmp-ord-fabb-qta(4)
                                  tmp-ord-fabb-qta(5)
                                  tmp-ord-fabb-qta(6)
                                  tmp-ord-promo
                                  tmp-ord-impegnato
                                  tmp-ord-giacenza
                                  tmp-ord-ordinato
                end-read

      ****          add tmp-ord-media-vend   to ord2-media-vend
      ****          add tmp-ord-media-vend-no-peaks   
      ****                                   to ord2-media-vend-no-peaks
           end-evaluate.

      ***---
       CALCOLA-CONSEGNA-PERIODO.
           move 0 to ord2-consegna.
           compute wk-campo =
            ( ( ord2-media-vend-no-peaks * ord2-scost ) / 100 ).
      *****     compute ord2-consegna = ( wk-campo / 20 )  * ord2-lead-time.
           compute ord2-consegna = wk-campo.
           |ARROTONDAMENTO ALL'UNITA: SONO PEZZI!!!
           add 0,5 to ord2-consegna.
           move  0 to ord2-consegna(10:2).

      ***---
       CALCOLA-PUNTO-RIORDINO.
           compute wk-campo = 12   *(el-qta(1)  ** 2 + el-qta(2)  ** 2 
                 + el-qta(3)  ** 2 + el-qta(4)  ** 2 + el-qta(5)  ** 2 
                 + el-qta(6)  ** 2 + el-qta(7)  ** 2 + el-qta(8)  ** 2 
                 + el-qta(9)  ** 2 + el-qta(10) ** 2 + el-qta(11) ** 2 
                 + el-qta(12) ** 2) -
                 ( el-qta(1) + el-qta(2)  + el-qta(3) + el-qta(4) + 
                   el-qta(5) + el-qta(6)  + el-qta(7) + el-qta(8) + 
                   el-qta(9) + el-qta(10) + el-qta(11) + el-qta(12) ) 
                   ** 2.

           if wk-campo not > 0
              move 0 to ord2-riordino
           else
              compute wk-campo = wk-campo / 132
              compute wk-campo = wk-campo ** ,5
              add ord2-consegna  to wk-campo
              move wk-campo      to ord2-riordino
              |ARROTONDAMENTO ALL'UNITA: SONO PEZZI!!!
              add 0,5            to ord2-riordino
              move  0            to ord2-riordino(10:2)
              move art-mag-std   to mag-codice
              read tmagaz no lock invalid continue end-read
              compute ord2-riordino = ord2-riordino +
                    ( ord2-riordino * mag-perce-riordino / 100 )
           end-if.

      ***---
       CALCOLA-PERIODO.
           set tutto-ok to true.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.

           move tge-data-consolid-progmag to start-data end-data.
           compute como-data =
                   function INTEGER-OF-DATE(end-data).
           move tge-data-consolid-progmag(1:4) to anno.
           divide   anno  by 4 giving anno2.
           multiply anno2 by 4 giving anno2.
           if anno2 = anno
              if tge-data-consolid-progmag(5:2) = "01"
                 subtract 364 from como-data
              else
                 subtract 365 from como-data
              end-if
           else
              subtract 364 from como-data
           end-if.

           compute start-data =
                   function DATE-OF-INTEGER(como-data).

           move start-data(5:2) to mese-start.
           move end-data(5:2)   to mese-end.
           move end-data(1:4)   to anno-corr.
           move start-data(1:4) to anno-past.

      ***---
       COMPONI-TITOLO-MESI.
           initialize tit-mese(idx).
           evaluate mese
           when 1   move "Mese1"  to tit-mese(idx)
           when 2   move "Mese2"  to tit-mese(idx)
           when 3   move "Mese3"  to tit-mese(idx)
           when 4   move "Mese4"  to tit-mese(idx)
           when 5   move "Mese5"  to tit-mese(idx)
           when 6   move "Mese6"  to tit-mese(idx)
           when 7   move "Mese7"  to tit-mese(idx)
           when 8   move "Mese8"  to tit-mese(idx)
           when 9   move "Mese9"  to tit-mese(idx)
           when 10  move "Mese10" to tit-mese(idx)
           when 11  move "Mese11" to tit-mese(idx)
           when 12  move "Mese12" to tit-mese(idx)
           end-evaluate.

      ***---
      * Da 0. Ricalcola le qta utili x coprire il fabbisogno
      * recuperando i valori dai listini fornitori
       COPRI-FABBISOGNO.
           move low-value        to rlis-rec.
           move ord2-articolo    to rlis-articolo.
           start rlistini key >= rlis-k-art of rlistini
                 invalid  continue
             not invalid
                 perform until 1 = 2
                    read rlistini next at end exit perform end-read
                    if rlis-articolo  not = ord2-articolo
                       exit perform
                    end-if
                    if rlis-lead-time = 0
                       move ord2-articolo to art-codice
                       read articoli no lock 
                            invalid continue 
                       end-read
                       move art-marca-prodotto to mar-codice
                       read tmarche no lock
                            invalid continue
                       end-read
                       move mar-lead-time to tmf-lead-time
                                            rlis-lead-time
                    end-if

                    if data-calcolo     >= rlis-ini-val  and
                       data-calcolo     <= rlis-fine-val and
                       ord2-lead-time-f >= rlis-lead-time
                       set tmf-inclusi to true
                    else
                       set tmf-esclusi to true
                    end-if
                    perform SCRIVI-RIGA-TMP-FORN
                 end-perform
           end-start.

      ***---
       SCRIVI-RIGA-TMP-FORN.
           set  cli-tipo-F      to true.
           move rlis-fornitore  to cli-codice.
           read clienti no lock invalid continue end-read.
           if rlis-destino not = 0
              move rlis-fornitore to desf-codice
              move rlis-destino   to desf-prog
              read destinif no lock invalid continue end-read
              move desf-ragsoc-1 to cli-ragsoc-1
           end-if.
           move cli-ragsoc-1       to tmf-ragsoc.
           move rlis-articolo      to tmf-articolo.
           move rlis-codice        to tmf-listino.
           move rlis-disponibilita to tmf-disponibilita.
           move rlis-fornitore     to tmf-fornitore.
           move rlis-destino       to tmf-destino.
           move rlis-lead-time     to tmf-lead-time.

           move art-marca-prodotto to mar-codice.
           read tmarche no lock invalid continue end-read.
           move art-peso-utf       to prg-peso-utf.
           move art-peso-non-utf   to prg-peso-non-utf.

           move rlis-codice to tlis-codice.
           read tlistini no lock.

           move rlis-tipo-tratt-imposte to imf-codice.
           read impforn no lock invalid continue end-read.
                                                
           move tlis-trasp-f to como-trasporto-f.
           move tlis-trasp-c to como-trasporto-c.

           perform CALCOLA-PRZ-FINITO.
           add 0,0005         to prz-confronto.
           add 0,005          to prz-confronto.
           move prz-confronto to tmf-prz-confronto.
           add 0,0005         to prz-reale.
           add 0,005          to prz-reale.
           move prz-reale     to tmf-prz-listino.
           add 0,0005         to premio-FA.
           add 0,005          to premio-FA.
           move premio-FA     to tmf-premio.
           compute tmf-totale = tmf-premio + tmf-prz-confronto.

           move rlis-ini-val  to tmf-data-ini.
           move rlis-fine-val to tmf-data-fine.
           write tmf-rec invalid rewrite tmf-rec end-write.

      ***---
       CALCOLA-TRASPORTO.
           move 0 to costo-trasporto.
           move spaces to tge-chiave.
           read tparamge no lock.

           move tlis-fornitore to desf-codice.
           move tlis-destino   to desf-prog.
           read destinif no lock invalid continue end-read.  
           if como-trasporto-f = 1
              compute costo-trasporto = 
                    ( art-peso-utf + art-peso-non-utf ) * tge-trasp-f
           end-if.
           if como-trasporto-c = 1
              compute costo-trasporto = costo-trasporto +
                    (( art-peso-utf + art-peso-non-utf ) * tge-trasp-c)
           end-if. 

      ***---
       VALUTA-PREZZO.         
           compute rlis-prz-acq =
                   rlis-prz-acq -
                 ( rlis-prz-acq * rlis-sconto-1 / 100).
                              
           compute rlis-prz-acq =
                   rlis-prz-acq -
                 ( rlis-prz-acq * rlis-sconto-2 / 100).
                              
           compute rlis-prz-acq =
                   rlis-prz-acq -
                 ( rlis-prz-acq * rlis-sconto-3 / 100).
                              
           compute rlis-prz-acq =
                   rlis-prz-acq -
                 ( rlis-prz-acq * rlis-sconto-4 / 100).
                              
           compute rlis-prz-acq =
                   rlis-prz-acq -
                 ( rlis-prz-acq * rlis-sconto-5 / 100).

           if rlis-netto < rlis-prz-acq and
              rlis-netto > 0
              move rlis-netto to rlis-prz-acq
           end-if.

      ***---
      * Separato in quanto al momento di associazione fornitori  
      * devo scrivere l'occurs e non subito l'effettivo
       SCRIVI-COPERTURA.
           if ord2-mese-rif < 2
              move low-value     to tmf-rec
              set  tmf-esclusi   to true
              move ord2-articolo to tmf-articolo
              start tmp-forn key >= tmf-k-articolo
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tmp-forn next at end exit perform end-read
                       if tmf-articolo not = ord2-articolo
                          exit perform
                       end-if 
      
                       if tmf-inclusi
                          move tmf-fornitore to desf-codice
                          move tmf-destino   to desf-prog
                          read destinif no lock
                          if desf-nazione not = "ITA"
                             move 2 to ord2-mese-rif
                             if ricalcolo and ord2-mese-scelto < 2
                                move 2 to ord2-mese-scelto
                             end-if
                             rewrite ord2-rec
                          end-if 
                          exit perform
                       end-if
                    end-perform
              end-start
           end-if.

           move 0 to qta-disp.
           move low-value     to tmf-rec.
           set  tmf-esclusi   to true.
           move ord2-articolo to tmf-articolo.
           start tmp-forn key >= tmf-k-articolo
                 invalid  continue
             not invalid
                 perform until 1 = 2
                    read tmp-forn next at end exit perform end-read
                    if tmf-articolo not = ord2-articolo
                       exit perform
                    end-if 

                    if tmf-inclusi
                       if ord2-fabb-qta(ord2-mese-rif) < 0
                          move 0 to qta-utile qta-disp cpf-qta
                       else
                          |Quante me ne servono ancora???
                          compute qta-utile = 
                                  ord2-fabb-qta(ord2-mese-rif) - 
                                  qta-disp
                          if qta-utile > tmf-disponibilita
                             move tmf-disponibilita to cpf-qta
                          else
                            move qta-utile to cpf-qta
                          end-if
                          add cpf-qta to qta-disp
                       end-if
                    else
                       move 0 to cpf-qta
                    end-if
                    
                    move tmf-lead-time     to cpf-lead-time
                    move ord2-articolo     to cpf-articolo
                    move tmf-fornitore     to cpf-fornitore
                    move tmf-destino       to cpf-destino
                    move tmf-prz-listino   to cpf-prz-listino
                    move tmf-prz-confronto to cpf-prz-confronto
                    move tmf-totale        to cpf-totale

                    move tmf-data-ini      to cpf-data-ini
                    move tmf-data-fine     to cpf-data-fine

                    move tmf-listino       to cpf-listino

                    move user-codi         to cpf-utente-creazione
                    accept cpf-data-creazione from century-date
                    accept cpf-ora-creazione  from time 

                    set  cli-tipo-F    to true
                    move cpf-fornitore to cli-codice
                    read clienti no lock invalid continue end-read
                                 
                    set cpf-auto to true
                    write cpf-rec invalid rewrite cpf-rec end-write
                 end-perform
           end-start.

      ***---
      * Somma le qta delle associazioni già presenti e valuta la 
      * copertura della quantità fabbisogno del mese.
      * Serve per aggiornarla sia ogni notte che ad ogni ingresso
      * con gli ordini effettuati che aumentano l'impegnato.
       VALUTA-COPERTURA.
           set singolo-copre     to false.
           set fornitore-singolo to true.
           move 0 to qta-disp save-fornitore forn-inclusi.
           move low-value to cpf-rec.
           move ord2-articolo to cpf-articolo.
           start coperfab key >= cpf-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read coperfab next at end exit perform end-read
                    if cpf-articolo not = ord2-articolo
                       exit perform
                    end-if
                    add cpf-qta to qta-disp
                    if cpf-fornitore not = save-fornitore
                       move cpf-fornitore to save-fornitore
                       if cpf-qta not = 0
                          add 1 to forn-inclusi
                          |Se non già coperto da un singolo
                          |e la quantità disponibile copre interamente
                          |quanto richiesto e quanto richiesto > 0
                          if not singolo-copre                       and 
                             cpf-qta >= ord2-fabb-qta(ord2-mese-rif) and
                             ord2-fabb-qta(ord2-mese-rif) > 0
                             set singolo-copre to true
                          end-if
                       else
                          if cpf-lead-time <= ord2-lead-time-f and
                             cpf-data-ini  <= data-calcolo and
                             cpf-data-fine >= data-calcolo
                             add 1 to forn-inclusi
                             |Se non già coperto da un singolo
                             |e la quantità disponibile copre interamente
                             |quanto richiesto e quanto richieso > 0
                             if not singolo-copre            and 
                                cpf-qta >= 
                                ord2-fabb-qta(ord2-mese-rif) and
                                ord2-fabb-qta(ord2-mese-rif) > 0
                                set singolo-copre to true
                             end-if
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.
           if forn-inclusi > 1
              set fornitore-singolo to false
           end-if.

      *****     if ord2-fabb-qta(ord2-mese-rif) <= 0
      **********        set ord2-fabbisogno-coperto-uno to true |NO FORNITORI MA QTA <= 0 = VERDE
      *****        set ord2-fabbisogno-zero to true |NO FORNITORI = ROSSO
      *****     else
      *****        if qta-disp = 0
      *****           set ord2-fabbisogno-zero to true
      *****        else
      *****           if singolo-copre
      *****              set ord2-fabbisogno-coperto-uno to true
      *****           else
      *****              if qta-disp >= ord2-fabb-qta(ord2-mese-rif)
      *****                 set ord2-fabbisogno-coperto-tanti to true
      *****              else
      *****                 set ord2-fabbisogno-parziale to true
      *****              end-if
      *****           end-if
      *****        end-if
      *****     end-if.

           move ord2-articolo to art-codice.
           read articoli   no lock invalid continue end-read.
           move art-scorta to sco-codice.
           read tscorte    no lock invalid continue end-read.
           if not sco-art-attivo
              set ord2-fabbisogno-no-attivo to true
           else
              if ord2-fabb-qta(1) > 0
                 set ord2-fabbisogno-mese1 to true
              else
                 if ord2-fabb-qta(2) > 0 and ord2-mese-rif >= 2
                    set ord2-fabbisogno-mese2 to true
                 else
                    if ord2-fabb-qta(3) > 0 and ord2-mese-rif >= 3
                       set ord2-fabbisogno-mese3 to true
                    else
                       set ord2-fabbisogno-other to true
                    end-if
                 end-if
              end-if
           end-if.

           if ord2-mese-rif = 0
              move 0 to ord2-qta-ord
           else
              if ricalcolo and ord2-fabb-qta(ord2-mese-rif) >= 0
                 if qta-disp >= ord2-fabb-qta(ord2-mese-rif)
                    move ord2-fabb-qta(ord2-mese-rif) to ord2-qta-ord
                 else
                    move qta-disp                     to ord2-qta-ord
                 end-if
              end-if
           end-if.
           rewrite ord2-rec invalid continue end-rewrite.
           
           perform COLORA-FABBISOGNO.

      ***---
       DETERMINA-CATENA.
           set articolo-singolo to true
      *    controllo se l'articolo sia o no presente in una catena articoli
           move art-codice   to cat-codice
           move low-value    to cat-princ

           start catart  key not < cat-chiave
                 invalid continue
             not invalid
                 read catart next
                      at end continue
                  not at end
                       if art-codice = cat-codice
                          set articolo-catena  to true
                          if cat-princ not = zero
                             move cat-princ to cat-codice
                             move zero   to cat-princ
                             read catart no lock
                                invalid
                                   continue
                             end-read
                             if art-codice = 
                                      cat-collegato(cat-num-el-catena)
                                set articolo-ultimo  to true
                             end-if
                          end-if
                          move cat-collegato(cat-num-el-catena)  
                                                     to como-ult-art
                       end-if
                 end-read
           end-start.

      ***---
       OPEN-OUTPUT-TMP-ORDFOR.
           accept como-ora  from time.
           accept como-data from century-date.
           initialize path-tmp-ordfor.
           accept  path-tmp-ordfor  from environment "PATH_ST".
           inspect path-tmp-ordfor  replacing trailing spaces 
                                         by low-value.
           string  path-tmp-ordfor delimited low-value
                   "TMP-ORDFOR"    delimited size
                   "_"             delimited size
                   como-data       delimited size
                   "_"             delimited size
                   como-ora        delimited size
                   ".tmp"          delimited size
                   into path-tmp-ordfor
           end-string.
           open output tmp-ordfor.
           close       tmp-ordfor.
           open i-o    tmp-ordfor.
                                  
      ***---
       TROVA-GIACENZA-IMPEGNATO. 
           move spaces to prg-tipo-imballo.
           move spaces to prg-cod-magazzino.
           move 0      to prg-peso.
           move 0 to como-giacenza como-impegnato.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid                            
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo not = ord2-articolo
                       exit perform 
                    end-if
                    if prg-cod-magazzino not = spaces
                       move prg-cod-magazzino to mag-codice
                       read tmagaz no lock
                       if mag-per-promo-si       
                          add prg-giacenza  to como-giacenza
                          add prg-impegnato to como-impegnato
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       AGGIUNGI-QTA-VENDUTI.
           if articolo-fisso not = 0
              exit paragraph 
           end-if.
           accept como-data    from century-date.  
           if tge-data-consolid-progmag = 0
              move spaces to tge-chiave
              read tparamge no lock
           end-if.                                            
           move tge-data-consolid-progmag(1:4) to CurrentYear.
           subtract 1 from CurrentYear giving PastYear.  

           move low-value     to qp-rec.
           move ord2-articolo to qp-articolo.
           start qta-pordini key >= qp-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read qta-pordini next 
                         at end exit perform 
                    end-read
                    if qp-articolo not = ord2-articolo
                       exit perform
                    end-if
                    if qp-anno = CurrentYear
                       if tge-data-consolid-progmag(5:2) = 12
                          if qp-qta > 0
                             add qp-qta to ord2-qta-past-m(qp-mese)
                          else
                             if qp-qta-ass < ord2-qta-past-m(qp-mese)
                                add qp-qta to ord2-qta-past-m(qp-mese)
                             else
                                move 0 to ord2-qta-past-m(qp-mese)
                             end-if
                          end-if
                       else                                      
                          if qp-qta > 0
                             add qp-qta to ord2-qta-corr-m(qp-mese)
                          else
                             if qp-qta-ass < ord2-qta-corr-m(qp-mese)
                                add qp-qta to ord2-qta-corr-m(qp-mese)
                             else
                                move 0 to ord2-qta-corr-m(qp-mese)
                             end-if
                          end-if
                       end-if
                    end-if
                    if qp-anno = PastYear                    
                       if tge-data-consolid-progmag(5:2) not = 12
                          if qp-qta > 0
                             add qp-qta to ord2-qta-past-m(qp-mese)
                          else
                             if qp-qta-ass < ord2-qta-past-m(qp-mese)
                                add qp-qta to ord2-qta-past-m(qp-mese)
                             else
                                move 0 to ord2-qta-past-m(qp-mese)
                             end-if
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.
