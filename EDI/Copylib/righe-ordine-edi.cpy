      ***---
       SCRIVI-RIGHE-ORDINE.
           set TrovataScortaForzata to false.
           move 0 to riga.
           move low-value   to emro-chiave.
           move emto-chiave to emro-chiave-testa.
           start EDI-mrordini key >= emro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read edi-mrordini next at end exit perform end-read
                    if emro-chiave-testa not = emto-chiave
                       exit perform
                    end-if
                    perform COUNTER-VIDEO

                    if emro-si-blister     
                      |Nessun conrollo: è come se ho registrato
                      |una riga non attiva e la blocco in seguito
      *****                 perform CONTROLLA-VALIDITA-COMPONENTI
      *****                 if bli-attivo
                          perform SPLIT-BLISTER
      *****                 end-if
                    else     
                        move emro-cod-articolo to mro-cod-articolo 
                                                  art-codice
                        read articoli no lock
                        |Nessun conrollo: è come se ho registrato
                        |una riga non attiva e la blocco in seguito
      *****                  if art-attivo
                    
                           move emro-prg-chiave to mro-prg-chiave 
                                                   prg-chiave
                           read progmag no lock                        
                           |Nessun conrollo: è come se ho registrato
                           |una riga non attiva e la blocco in seguito
      *****                     if prg-attivo
                              move art-marca-prodotto to mar-codice
                              read tmarche no lock
                    
                              move emro-qta          to mro-qta
                              move emro-prz          to mro-prz-unitario  
                    
                              move emro-peso-utf     to mro-peso-utf
                              move emro-peso-non-utf to mro-peso-non-utf
                    
                              move emro-num-colli    to mro-num-colli
                              move emro-des-imballo  to mro-des-imballo
                    
                              move emro-qta-imballi  to mro-qta-imballi
                              move emro-cod-art-cli  to mro-cod-art-cli  
                              move emro-prz-commle   to mro-prz-commle   
                                          
                              move emro-dati-blister to mro-dati-blister 
                              perform SCRIVI-RIGA-COMUNE
      *****                     end-if
      *****                  end-if
                    end-if
                 end-perform
           end-start.     

      ***---
       CONTROLLA-VALIDITA-COMPONENTI.
           move mto-causale to tca-codice.
           read tcaumag no lock.
           move emro-cod-articolo to bli-codice art-codice.
           read blister no lock invalid continue end-read.

           perform varying idx from 1 by 1 
                     until idx > 50
              if bli-el-articolo(idx) = 0 exit perform end-if
              move bli-el-articolo(idx) to art-codice
              read articoli no lock 
                   invalid continue 
              end-read
              if art-attivo            
                 set bli-bloccato   to true
                 move low-value     to prg-chiave
                 move art-codice    to prg-cod-articolo
                 move tca-cod-magaz to prg-cod-magazzino
                 start progmag key >= prg-chiave
                       invalid continue        
                   not invalid
                       perform until 1 = 2
                          read progmag next at end exit perform end-read
                          if prg-cod-articolo  not = art-codice or
                             prg-cod-magazzino not = tca-cod-magaz
                             exit perform
                          end-if
                          if prg-attivo
                             set bli-attivo to true
                             exit perform
                          end-if
                       end-perform
                 end-start
                 if bli-bloccato
                    exit perform
                 end-if
              else
                 set bli-bloccato to true
                 exit perform
              end-if
           end-perform.
    
      ***---
       VALORIZZA-ARRAY-CAUSALI.
           set link-update-um      to true.
           set link-update-peso    to false.
           set link-update-valore  to false.
           move "0000000000000000" to link-array.
           move link-causale       to tca-codice.
           read tcaumag no lock invalid continue end-read.
           |In caso sia permessa la stampa della bolla
           |agisco sull'impegnato, altrimenti l'ordine
           |è da considerarsi già bollettato ed agisco
           |direttamente sulla giacenza e non sull'impegnato
           if tca-si-stampa 
              move 1 to multiplyer(2)
              perform DIREZIONA-IMPEGNATO
           else
              move 1 to multiplyer(1)
              move 1 to multiplyer(15)
           end-if.
           
      ***---
       CALCOLA-IMPOSTE-ORDINE.
           move 0 to imposta-cobat imposta-cou add-piombo
                     mro-imp-cou-cobat mro-imp-consumo 
                     mro-add-piombo.
           |L'ho dovuta searare in quanto, se TROVO il movimento di
           |magazzino ma poi cambio il prezzo devo rifare il calcolo
           |delle imposte. Viene richiamato sull'after del campo
           |prezzo ossia quando viene cambiato

           if emto-prg-destino = 0
              move cli-nazione to naz-codice
           else
              move des-nazione to naz-codice
           end-if.
           read tnazioni no lock.

           if naz-imp-esenti-si
              perform CALCOLO-IMPOSTE-ESTERO
           else
              evaluate true
              when ttipocli-standard perform CALCOLO-IMPOSTE-STANDARD
              when ttipocli-gdo      perform CALCOLO-IMPOSTE-GDO                                 
              end-evaluate
           end-if.

      ***---
       CALCOLO-IMPOSTE-ESTERO.
           move 0 to mro-imp-consumo mro-imp-cou-cobat mro-add-piombo
                     imposta-cou imposta-consumo add-piombo.

      ***---
       CALCOLO-IMPOSTE-STANDARD.
           if art-si-imposte
              if mar-si-imposta-consumo
                 evaluate true
                 when art-misto
                 when art-si-utf
                      compute como-imposta =
                    (( prg-peso-utf * imp-imposta-consumo ) 
                                    * art-perce-imposte   ) / 100
                 when art-no-utf
                      compute como-imposta =
                    (( prg-peso-non-utf * imp-imposta-consumo ) 
                                        * art-perce-imposte) / 100
                 end-evaluate
                 add 0,005              to como-imposta
                 move como-imposta      to mro-imp-consumo
              end-if
      
              if mar-si-cou
                 evaluate true
                 when art-misto
                 when art-si-utf
                      compute como-imposta =
                    (( prg-peso-utf * imp-cou ) 
                                    * art-perce-cou   ) / 100
                 when art-no-utf
                      compute como-imposta =
                    (( prg-peso-non-utf * imp-cou )
                                        * art-perce-cou   ) / 100
                 end-evaluate
                 add 0,005              to como-imposta
                 move como-imposta      to mro-imp-cou-cobat
              end-if

           else
              move 0 to mro-imp-consumo
           end-if.

           if art-si-cobat
              perform CALCOLA-COBAT  
              if tcl-si-piombo
                 perform CALCOLA-ADD-PIOMBO
              end-if
           end-if.

      ***---
       CALCOLO-IMPOSTE-GDO.
           if art-si-imposte
              evaluate true
              when art-misto
              when art-si-utf
                   compute como-imposta =
                  (( prg-peso-utf * imp-imposta-consumo ) 
                                  * art-perce-imposte   ) / 100
              when art-no-utf
                   compute como-imposta =
                 (( prg-peso-non-utf * imp-imposta-consumo ) 
                                     * art-perce-imposte) / 100
              end-evaluate
              add 0,005              to como-imposta
              move como-imposta      to mro-imp-consumo
           else
              move 0 to mro-imp-consumo
           end-if.
      
           move 0 to imposta-cou.
           evaluate true
           when art-misto
           when art-si-utf
                compute como-imposta = 
                     (( prg-peso-utf * imp-cou ) 
                                     * art-perce-cou   ) / 100
           when art-no-utf
                compute como-imposta =
                 (( prg-peso-non-utf * imp-cou )
                                     * art-perce-cou   ) / 100
           end-evaluate.
           add 0,005              to como-imposta.
           move como-imposta      to mro-imp-cou-cobat.
                                                        
           if art-si-cobat
              perform CALCOLA-COBAT
              if tcl-si-piombo
                 perform CALCOLA-ADD-PIOMBO
              end-if
           end-if.

      ***---
       CALCOLA-COBAT.
           if ttipocli-gdo or mar-si-cobat
              perform SCAGLIONI-COBAT
           end-if.

      ***---
       SCAGLIONI-COBAT.
           evaluate true
           when art-auto-cobat
                evaluate true
                when art-amperaggio >= imp-cb-auto-sca-1-da and
                     art-amperaggio <= imp-cb-auto-sca-1-a
                     move imp-cb-auto-sca-1-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-auto-sca-2-da and
                     art-amperaggio <= imp-cb-auto-sca-2-a
                     move imp-cb-auto-sca-2-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-auto-sca-3-da and
                     art-amperaggio <= imp-cb-auto-sca-3-a
                     move imp-cb-auto-sca-3-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-auto-sca-4-da and
                     art-amperaggio <= imp-cb-auto-sca-4-a
                     move imp-cb-auto-sca-4-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-auto-sca-5-da and
                     art-amperaggio <= imp-cb-auto-sca-5-a
                     move imp-cb-auto-sca-5-euro 
                       to imposta-cobat
                end-evaluate
           
           when art-moto-cobat
                evaluate true
                when art-amperaggio >= imp-cb-scooter-sca-1-da 
                 and art-amperaggio <= imp-cb-scooter-sca-1-a
                     move imp-cb-scooter-sca-1-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-scooter-sca-2-da 
                 and art-amperaggio <= imp-cb-scooter-sca-2-a
                     move imp-cb-scooter-sca-2-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-scooter-sca-3-da 
                 and art-amperaggio <= imp-cb-scooter-sca-3-a
                     move imp-cb-scooter-sca-3-euro 
                       to imposta-cobat
                end-evaluate
           end-evaluate.
           
           add 0,005              to imposta-cobat.
           add  imposta-cou       to imposta-cobat.
           move imposta-cobat     to mro-imp-cou-cobat.

      ***---
       CALCOLA-ADD-PIOMBO. 
           move mro-prz-unitario   to como-prz-unitario.
           move mro-imp-cou-cobat  to como-imp-cou-cobat.

           move imposta-cobat      to mro-imp-cou-cobat.
           move art-marca-prodotto to tpb-marca.
           move mto-data-ordine    to como-data-ordine tpb-data.
           move mto-cod-cli        to como-prm-cliente.
           move mto-prg-destino    to como-prm-destino.
           perform ADDIZIONALE-PIOMBO.
           move add-piombo to mro-add-piombo.

      ***---
       CALCOLA-IMPONIBILE.
           evaluate true
           when ttipocli-gdo
                compute mro-imponib-merce = 
                        mro-prz-unitario  - 
                        mro-imp-cou-cobat - 
                        mro-imp-consumo   -
                        mro-add-piombo

                |Sono su un tradizionale, ma ho invertito 
                |quindi ricalcolo imponibile merce = prezzo unitario
                if emto-inversione-imposte-si
                   compute mro-prz-unitario = 
                           mro-imponib-merce
                end-if

           when other

                if mro-prz-unitario >= 9999999,99
                   move 9999999,99 to mro-imponib-merce
                else
                   compute mro-imponib-merce = 
                           mro-prz-unitario  
                end-if

      *****          |Sono su un GDO, ma ho invertito 
      *****          |quindi ricalcolo prezzo unitario = imponibile + imposte
      *****          if emto-01T60-inversione-imposte-si  
      *****             compute mro-prz-unitario = 
      *****                     mro-imponib-merce + 
      *****                     mro-imp-cou-cobat + 
      *****                     mro-imp-consumo   +
      *****                     mro-add-piombo
      *****          end-if
           end-evaluate.

      ***---
       SPLIT-BLISTER.
           move 0 to Sum.
           move emro-cod-articolo to bli-codice art-codice.
           read blister no lock invalid continue end-read.

           move 0 to LastIdx.
           perform varying LastIdx from 1 by 1
                     until LastIdx > 50
              if bli-el-articolo(LastIdx) = 0
                 subtract 1 from LastIdx
                 exit perform
              end-if
           end-perform.

           move 0 to idx.
           perform varying idx from 1 by 1
                     until idx > 50
              if bli-el-articolo(idx) = 0
                 exit perform
              end-if
              set mro-si-blister to true
              move bli-codice    to mro-bli-codice

              set tutto-ok to true
              set trovato  to false
              move bli-el-articolo(idx) to art-codice mro-cod-articolo

              if tcl-gdo-si or tcl-gdo-opz
                 perform TROVA-LISTINO
                 if no-prg-listino
                    perform TROVA-CLI-PRG
                 end-if
                 if tutto-ok
                    if si-prg-listino
                       move como-prg-chiave to GiacenzaKey
                       set trovato to true
                    else
                       perform VALORIZZA-MAGGIOR-GIACENZA
                    end-if
                 end-if
              else
                 perform VALORIZZA-MAGGIOR-GIACENZA
              end-if

              if trovato             
                 move bli-el-articolo(idx) to art-codice
                 move bli-el-qta(idx)      to mro-bli-qta
                 move bli-el-perce(idx)    to mro-bli-perce
                 move GiacenzaKey          to prg-chiave emro-prg-chiave 
                                              mro-prg-chiave
                 read progmag  no lock invalid continue end-read
                 read articoli no lock invalid continue end-read  
                    
                 move prg-peso-utf     to mro-peso-utf
                 move prg-peso-non-utf to mro-peso-non-utf

                 move "BLISTER" to mro-des-imballo
                 if idx = 1
                    move emro-qta  to mro-qta-imballi
                    move emro-prz  to TotPrzBlister
                    move emro-num-colli to mro-num-colli
                 else
                    move 0         to mro-qta-imballi mro-num-colli
                 end-if                                 
                 compute mro-qta = emro-qta * bli-el-qta(idx)
                 move 0        to mro-qta-omaggi
           
                 if bli-el-perce(idx) not = 0
           
                    compute como-numero =
                            TotPrzBlister * 
                            bli-el-perce(idx) / 100 / 
                            bli-el-qta(idx) 
      *    o fine              
                    perform ARROTONDA-PRZ-BLISTER
           
                    move emro-promo to mro-promo

                    move como-prezzo        to mro-prz-unitario
                                               mro-prz-commle
                 else         

                    move 0 to mro-prz-commle
                              mro-prz-unitario
                              mro-imp-consumo 
                              mro-imp-cou-cobat 
                              mro-add-piombo
                 end-if
              end-if
              perform SCRIVI-RIGA-COMUNE
           end-perform.

      ***---
       SCRIVI-RIGA-COMUNE.                     
           move art-scorta to sco-codice.
           read tscorte no lock.
           move art-marca-prodotto to mar-codice.
           read tmarche no lock.
           if sco-forzata-saldi-si
              set TrovataScortaForzata to true
           end-if.
           add 1 to riga.
           move mto-chiave to mro-chiave-testa.
           move riga       to mro-riga.

           move emro-promo to mro-promo.
           
           if mro-promo not = 0
              set mro-si-prz-promo to true
              set si-promo to true
           else 
              set mro-no-prz-promo to true
           end-if.
           
           move riga to mro-progr
           
           move emro-evadi-dal to mro-evadi-dal. 
           accept mro-data-creazione from century-date
           accept mro-ora-creazione  from time
           move user-codi to mro-utente-creazione
           
           set mro-registrato to true
           move emro-bloccato-prezzo to mro-bloccato-prezzo   
           
           if emro-bloccato-prezzo-si or 
              mro-prz-unitario >= 999999,99
              set mto-bloccato              to true
              set mto-causale-blocco-prezzo to true
              rewrite mto-rec
           end-if.

           if mro-prz-unitario = 0     
              set mro-si-omaggio    to true
              move tge-cod-iva-omag to mro-cod-iva
              move 0 to mro-imponib-merce mro-imp-cou-cobat
                        mro-imp-consumo   mro-add-piombo
           else                          
              perform CALCOLA-IMPOSTE-ORDINE
              perform CALCOLA-IMPONIBILE    
              set mro-no-omaggio    to true
              move iva-std          to mro-cod-iva
           end-if.
           
           write mro-rec.
           
           initialize link-wprogmag.
           set link-update      to true.
           move mro-prg-chiave  to link-key.
           move mto-causale     to link-causale.
           move emro-qta        to link-valore
                                   link-impegnato.
           move user-codi       to link-user 
                                of link-wprogmag
           perform VALORIZZA-ARRAY-CAUSALI.

           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".
           
           if mro-promo = 0 and mto-prenotazione-qta-si
              if save-tpr-codice = 0
                 perform PROMO-FITTIZIA-TESTA
              end-if
              move save-tpr-codice to mro-promo
           end-if.
           
           move mro-chiave   to emro-ordine.
           set emro-caricato to true.
           rewrite emro-rec.                                  

      ***---
       TROVA-LISTINO.
           initialize como-prg-chiave replacing numeric data by zeroes
                                           alphanumeric data by spaces.
           set no-prg-listino to true
           if tcl-gdo-no
              exit paragraph
           end-if

           move cli-gdo          to lst-gdo.
           move emto-data-ordine to lst-data.
           move art-codice       to lst-articolo.
           start listini key <= lst-k-gdo-articolo
                 invalid continue
             not invalid
                 read listini previous
                 if lst-gdo      = cli-gdo          and
                    lst-data    <= emto-data-ordine and
                    lst-articolo = art-codice

                    if lst-prg-cod-articolo = space
                       move zero   to lst-prg-cod-articolo
                    end-if

                    if lst-prg-cod-articolo not = zero
                       set si-prg-listino   to true
                    end-if
                       
                 end-if
           end-start.

           if si-prg-listino
              move lst-prg-chiave  to prg-chiave
              read progmag no lock
                   invalid set emro-progressivo-non-forzato to true
               not invalid 
                   if prg-bloccato or prg-disattivo or
                      prg-cod-magazzino not = "LBX"
                      move 0 to como-prg-cod-articolo
                   else
                      move lst-prg-chiave  to como-prg-chiave
                   end-if
              end-read
           end-if.

      ***---
       TROVA-CLI-PRG.
           set  cp-tipo-C        to true.
           move cli-codice       to cp-clifor.
           move art-codice       to cp-articolo.
           read cli-prg no lock
                invalid continue
            not invalid 
                set si-prg-listino   to true
                read progmag no lock
                     invalid set emro-progressivo-non-forzato to true
                 not invalid 
                     if prg-bloccato or prg-disattivo or
                        prg-cod-magazzino not = "LBX"
                        move 0 to como-prg-cod-articolo
                     else
                        move cp-prg-chiave  to como-prg-chiave
                     end-if
                end-read
           end-read.
                            
LABLAB***---
       VALORIZZA-MAGGIOR-GIACENZA.
           |Valorizzo prg-chiave con il record avente > giacenza
           move 0 to giacenza.
           set  trovato              to false.
           move low-value            to prg-chiave.
           move bli-el-articolo(idx) to prg-cod-articolo.
           move bli-magazzino        to prg-cod-magazzino.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo  not = bli-el-articolo(idx) or
                       prg-cod-magazzino not = bli-magazzino
                       exit perform
                    end-if
                    if prg-attivo
                       if not trovato
                          set trovato to true
                          |Se la prima ed unica volta ho una giacenza di -1
                          move prg-giacenza to giacenza
                          move prg-giacenza to giacenza
                          move prg-chiave to GiacenzaKey
                       end-if
                       if prg-giacenza > giacenza
                          move prg-giacenza to giacenza
                          move prg-chiave to GiacenzaKey
                       end-if
                    end-if
                 end-perform
           end-start.

           |Se non ho nemmeno un progerssivo attivo prendo il primo
           if not trovato
              set  trovato              to true
              move low-value            to prg-chiave
              move bli-el-articolo(idx) to prg-cod-articolo
              move bli-magazzino        to prg-cod-magazzino
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    read progmag next
                    move prg-chiave to GiacenzaKey
              end-start
           end-if.

      ***---
       ARROTONDA-PRZ-BLISTER.
           add 0,005 to como-numero giving como-prezzo.
      *    Luciano
      *    devo moltiplicare il prezzo singolo per la qta presente 
      *    all'interno del blister
      *     add como-prezzo to Sum.
           compute como-prezzo2 = como-prezzo * bli-el-qta(idx).
           add como-prezzo2 to Sum.
      *    Luciano fine

           if idx = LastIdx|sono sull'ultimo
              if Sum not = TotPrzBlister
      *    Luciano
      *           if Sum > TotPrzBlister
      *              compute como-prezzo = 
      *                      como-prezzo - (Sum - TotPrzBlister)
      *           else
      *
      *              compute como-prezzo = 
      *                      como-prezzo + (TotPrzBlister - Sum)
      *           end-if

                 if Sum > TotPrzBlister
                    compute como-prezzo2 = 
                            como-prezzo2 - (Sum - TotPrzBlister)
                 else
      
                    compute como-prezzo2 = 
                            como-prezzo2 + (TotPrzBlister - Sum)
                 end-if

      *    devo dividere il prezzo ottenuto per la qta dell'articolo
      *    all'interno del blister
                 compute como-prezzo = como-prezzo2 / bli-el-qta(idx)
      *    Luciano fine
              end-if
           end-if.
