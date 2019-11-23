      ***---
       AGGIORNA-STATO-MASTER.
           set no-cli to false.
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "mtordini" to geslock-nome-file.
      
           set tutto-ok  to true.
           read mtordini lock key mto-chiave invalid continue end-read.
      
           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              initialize geslock-messaggio
              string "L'ordine master anno: " mto-anno " n. " mto-numero
              x"0d0a""Risulta bloccato su un altro terminale."
              x"0d0a""Sarà impossibile aggiornarne lo stato."
              delimited size
                 into geslock-messaggio
              end-string
              set RecLocked to false
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 0 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   read mtordini lock key mto-chiave
                        invalid continue 
                   end-read
              when ignora exit perform
              end-evaluate
           end-perform.
      
           if not RecLocked
           
              set cli-tipo-C to true
              move mto-cod-cli to cli-codice
              read clienti no lock
                   invalid set no-cli to true
              end-read

              move mto-cod-cli     to como-prm-cliente
              move mto-prg-destino to como-prm-destino
              perform TROVA-PARAMETRO

              set tutto-banco           to true
              set RaggiuntaDataLimite   to false
              set ImpegnatoRipristinato to false
              move spaces   to tge-chiave
              read tparamge no lock

              accept DataOggi from century-date
              compute como-data = function integer-of-date(DataOggi)
              subtract tge-gg-master-chiuso from como-data
              compute como-data = function date-of-integer(como-data)
              if mto-data-ordine < como-data
                 set RaggiuntaDataLimite to true
                |280917: nelle righe tutti gli "EVADI DAL" siano inferiori ad oggi
                 move low-value  to mro-chiave
                 move mto-chiave to mro-chiave-testa
                 start mrordini key >= mro-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read mrordini next 
                               at end 
                               exit perform 
                          end-read
                          if mro-chiave-testa not = mto-chiave
                             exit perform
                          end-if
                          if mro-evadi-dal >= DataOggi
                             set RaggiuntaDataLimite to false
                             exit perform
                          end-if
                       end-perform
                 end-start         
              end-if

      *****        if prm-imb-minimo-si
      *****           perform QTA-MINIMA-IMBALLO
      *****        end-if      

              move mto-stato-ordine to stato-originale
              move 0 to link-wprogmag-status
              move low-value  to mro-chiave
              move mto-chiave to mro-chiave-testa
              start mrordini key >= mro-chiave
                    invalid continue
                not invalid
                    move 0 to mto-pz-tot
                    move 0 to mto-pz-eva
                    set chiudibile    to true
                    set boll-tutto    to true
                    set trovata-bolla to false
      *****              set fatt-tutto    to true
      *****              set trovata-fatt  to false
      *****              if vecchia-evasione
      *****                 move mto-cod-cli     to ecd-cliente-OLD
      *****                 move mto-prg-destino to ecd-destino-OLD
      *****                 read evaclides no lock
      *****                      invalid move 0 to ecd-gg-scadenza-vol-OLD
      *****                 end-read
      *****              end-if
                    perform until 1 = 2
                       read mrordini next at end exit perform end-read
                       if mro-chiave-testa not = mto-chiave
                          exit perform
                       end-if

                       add mro-qta to mto-pz-tot
                       perform QTA-EVASA
                       if chiudibile and mro-qta > mro-qta-e
                          |Basta una riga non evasa in stato di REG
                          if mro-qta-e  = 0 and mto-registrato
                             set chiudibile to false
                          else
                             move mro-cod-articolo to art-codice
                             read articoli no lock 
                                  invalid continue
                             end-read
                             move art-scorta to sco-codice
                             read tscorte no lock
                                  invalid set chiudibile to false
                              not invalid
                                  if sco-chiu-forzata-no

                                     if mro-promo not = 0
                                        set tutto-banco to false
                                        |Se NON tengo i saldi promo il master non
                                        |è chiudibile solamente se non ho evaso
                                        |nulla e sono in REG
                                        if mto-saldi-promo-si or 
                                           mro-qta-e = 0
                                           perform VALIDITA-PROMO
                                           if promo-valida
                                              set chiudibile to false
                                           end-if
                                        end-if
      *****                                  |Se c'è stato un intervento del giang
      *****                                  |il master non è chiudibile
      *****                                  if mro-tenere-saldo
      *****                                     set chiudibile to false
      *****                                  else
      *****                                     |Se NON tengo i saldi promo il master non
      *****                                     |è chiudibile solamente se non ho evaso
      *****                                     |nulla e sono in REG
      *****                                     if mto-saldi-promo-si or 
      *****                                        mro-qta-e = 0
      *****                                        perform VALIDITA-PROMO
      *****                                        if promo-valida
      *****                                           set chiudibile to false
      *****                                        end-if
      *****                                     end-if
      *****                                  end-if
                                     else
                                        if mto-saldi-banco-si
                                           set chiudibile to false
                                        end-if
                                     end-if
                                  else
                                     set chiudibile to true
                                  end-if
                             end-read
                          end-if
                       end-if
                       add mro-qta-e to mto-pz-eva
                       if link-wprogmag-status = -2
                          exit perform
                       end-if
                    end-perform

                    if mto-pz-eva = 0 |and tutto-banco
                       set chiudibile to false
                    end-if

                    if RaggiuntaDataLimite
                       set chiudibile to true
                    end-if   

                    if mto-chiuso-man
                       set mto-chiuso to true
                       if stato-originale not = 7
                          perform INVIO-MAIL-CHIUSURA
                       else
                          perform CHIUSURA-RIGHE
                       end-if
                    else
                       if chiudibile
                          |Sto chiudendo per la prima volta
                          if stato-originale not = 7
                             perform INVIO-MAIL-CHIUSURA
                          else
                             perform CHIUSURA-RIGHE
                          end-if
                          set mto-chiuso to true
                       else
                          set mto-registrato to true
                          |STATO ORDINE
                          if mto-pz-eva not = 0
                             set mto-in-lavorazione to true
                             if trovata-bolla
                                if boll-tutto and 
                                   mto-pz-tot = mto-pz-eva
                                   set mto-sped-tot  to true
                                else
                                   set mto-sped-parz to true
                                end-if
                             end-if
      *****                       if trovata-fatt
      *****                          if fatt-tutto and mto-pz-tot = mto-pz-eva
      *****                             set mto-fatt-tot  to true
      *****                          else     
      *****                             set mto-fatt-parz to true
      *****                          end-if
      *****                       end-if
                          end-if
                          move mto-saldi-promo to save-mto-saldi-promo
                          perform STATO-RIGHE
                          move save-mto-saldi-promo to mto-saldi-promo
                       end-if
                    end-if

                    |QUESTO FLAG SARA' ATTIVATO SOLO
                    |DAI PROGRAMMI DI EVASIONE MASTER
                    if ordine-evaso
                       accept mto-data-evasione from century-date
                       accept mto-ora-evasione  from time
                       move user-codi to mto-utente-evasione
                    end-if
                    if tot-righe-master = tot-righe-chiuse
                       set mto-chiuso to true
                    end-if

                    |HO APERTE SOLO RIGHE CON CHIUSURA FORZATA 2 ATTIVO
                    |CHIUDO IL MASTER E TUTTE LE RIGHE
                    if mto-chiuso or mto-registrato
                       continue
                    else
                       if tot-righe-aperte = tot-righe-chiusura2
                          set mto-chiuso to true
                          if stato-originale not = 7
                             perform INVIO-MAIL-CHIUSURA
                          else
                             perform CHIUSURA-RIGHE
                          end-if
                       end-if
                    end-if

                    |UN SALDO CHE NON ACCORPA DEV'ESSERE CHIUSO
                    |SE IL PESO E' INFERIORE AL BLOCCO KG
                    |INDICATO NEI PARAMETRI D'EVASIONE
                    if mto-chiuso or mto-registrato or prm-accorpa-si
                       continue
                    else
                       move 0 to tot-peso-saldo
                       move low-value  to mro-chiave
                       move mto-chiave to mro-chiave-testa
                       start mrordini key >= mro-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read mrordini next 
                                     at end exit perform 
                                end-read
                                if mro-chiave-testa not = mto-chiave
                                   exit perform
                                end-if
                                if mro-chiuso
                                   continue
                                else
                                   compute tot-peso-saldo =
                                           tot-peso-saldo +
                                       ( ( mro-qta - mro-qta-e ) *
                                         ( mro-peso-utf + 
                                           mro-peso-non-utf    ) )
                                end-if
                                if tot-peso-saldo >= prm-blocco-kg
                                   exit perform
                                end-if
                             end-perform
                             if tot-peso-saldo < prm-blocco-kg
                                set mto-chiuso to true
                                if stato-originale not = 7
                                   perform INVIO-MAIL-CHIUSURA
                                else
                                   perform CHIUSURA-RIGHE
                                end-if
                             end-if
                       end-start
                    end-if

                    if mto-promo-fittizia not = 0
                       move mto-promo-fittizia to tpr-codice
                       read tpromo no lock
                            invalid
                            move 0 to mto-promo-fittizia
                            set mto-no-promo to true
                       end-read
                    end-if

                    if mto-chiuso or mto-prenotazione-qta-no
                       if mto-promo-fittizia not = 0
                          perform CHIUSURA-PROMO-FITTIZIA
                       end-if
                    else
                       if mto-prenotazione-qta-si and 
                          mto-promo-fittizia = 0
                          perform APERTURA-PROMO-FITTIZIA
                       end-if
                    end-if

                    rewrite mto-rec invalid continue end-rewrite

              end-start
              unlock mtordini all records
           end-if.

      ***---
      * Se i tempi si allungano troppo fare questa operazioni in notturna
       QTA-EVASA.
           move 0 to mro-qta-e mro-qta-b mro-qta-f.
           move low-value to ror-rec of rordini.
           move mro-chiave-testa to ror-chiave-ordine-testa of rordini.
           move mro-progr        to ror-progr-master of rordini.
           start rordini key >= ror-k-master
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-chiave-ordine-testa
                       of rordini not = mro-chiave-testa or
                       ror-progr-master
                       of rordini not = mro-progr
                       exit perform
                    end-if
                    move ror-anno       of rordini to tor-anno
                    move ror-num-ordine of rordini to tor-numero
                    read tordini no lock invalid continue end-read
                    if tor-anno-bolla = 0 
                       set boll-tutto to false
                    else          
                       set trovata-bolla to true
                       add ror-qta of rordini to mro-qta-b
                    end-if
                    if tor-anno-fattura = 0
                       if mro-prz-unitario not = ror-prz-unitario 
                                                 of rordini 
                          perform READ-RORDINI-LOCK
                          if not RecLocked
                             move mro-prz-unitario
                               to ror-prz-unitario  of rordini
                             move mro-imp-consumo
                               to ror-imp-consumo   of rordini
                             move mro-imp-cou-cobat
                               to ror-imp-cou-cobat of rordini
                             move mro-add-piombo
                               to ror-add-piombo    of rordini
                             move mro-imponib-merce
                               to ror-imponib-merce of rordini
                             rewrite ror-rec of rordini
                                     invalid continue
                             end-rewrite
                          end-if
                       end-if
      *****                 set fatt-tutto to false
                    else
      *****                 set trovata-fatt to true
                       add ror-qta of rordini to mro-qta-f
                    end-if
                    add ror-qta of rordini to mro-qta-e
                 end-perform
           end-start.
           if mro-qta-e = 0
              set boll-tutto to false
           end-if.               
           if mro-qta > mro-qta-b
              set boll-tutto to false
           end-if.
      *****     if mro-qta-f = 0
      *****        set fatt-tutto to false
      *****     end-if.
      *****     if ordine-evaso        and
      *****        mro-qta < mro-qta-e and
      *****        mro-promo not = 0   and 
      *****        mro-attesa
      *****        set mro-tenere-saldo to true
      *****     end-if.
           rewrite mro-rec invalid continue end-rewrite.

      *****     if mro-chiuso
      *****        continue
      *****     else
      *****        if ricalcolo
      *****           perform AGGIORNA-IMPEGNATO-MASTER
      *****        end-if
      *****     end-if.

      ***---
       READ-RORDINI-LOCK.
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "rordini" to geslock-nome-file.
      
           set tutto-ok to true.
           read rordini lock key ror-k-master invalid continue end-read.
      
           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              set RecLocked to false
              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 0 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   read rordini lock key ror-k-master
                        invalid continue 
                   end-read
              when ignora exit perform
              end-evaluate
           end-perform.

      ***---
       INVIO-MAIL-CHIUSURA.
           perform STORNA-IMPEGNATO-E-TAGLI.
           if no-mail
              continue
           else
              perform MAIL-CHIUSURA
           end-if.

      ***---
       MAIL-CHIUSURA.
      *****     call   "mail-chiu-master" using mto-chiave.
      *****     cancel "mail-chiu-master".

      ***---
       STORNA-IMPEGNATO-E-TAGLI.
           move low-value  to mro-rec.
           move mto-chiave to mro-chiave-testa
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    if mro-chiuso
                       continue
                    else
                       set mro-chiuso to true
                       rewrite mro-rec
                       if mro-qta > mro-qta-e
                          perform STORNA-IMPEGNATO
                          perform TAGLI-LAB
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       STORNA-IMPEGNATO.
           |CHIUDO PERTANTO STORNO L'IMPEGNATO
           initialize link-wprogmag.
           compute link-valore    = mro-qta-e - mro-qta.
           compute link-impegnato = mro-qta-e - mro-qta.

           set link-update      to true.
           move mro-prg-chiave  to link-key.
           move mto-causale     to link-causale.

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
              set cli-tipo-C to true
              move mto-cod-cli to cli-codice
              read clienti no lock
                   invalid 
                   set no-cli to true
                   exit paragraph
              end-read
              perform DIREZIONA-IMPEGNATO
           else
              move 1 to multiplyer(1)
              move 1 to multiplyer(15)
           end-if.
           move user-codi to link-user of link-wprogmag.
           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".

      ***---
       TAGLI-LAB.
           if no-tagli exit paragraph end-if.

           open i-o tagli.
                        
           set trovato-taglio to false.
           accept DataOggi from century-date.
           move low-value        to tag-rec.
           move DataOggi         to tag-data.
           move cli-gdo          to tag-gdo.
           move mro-cod-articolo to tag-articolo.
           start tagli key >= tag-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tagli next at end exit perform end-read
                    if tag-data     not = DataOggi or
                       tag-gdo      not = cli-gdo  or
                       tag-articolo not = mro-cod-articolo
                       exit perform
                    end-if
                    if tag-qta = ( mro-qta - mro-qta-e ) and
                       tag-mro-chiave = mro-chiave
                       set trovato-taglio to true
                       exit perform
                    end-if
                 end-perform
           end-start.

           if not trovato-taglio
              initialize tag-rec 
                         replacing numeric data by zeroes
                              alphanumeric data by spaces
              accept tag-data from century-date
              |Anche su tradizionale, al massimo sarà vuoto
              move cli-gdo          to tag-gdo
              move mro-cod-articolo to tag-articolo
              move cli-tipo         to tag-cli-tipo

              if mro-prz-unitario < 99999
                 move mro-prz-unitario to tag-prz
              else                          
      *****        if cli-gdo = spaces
                 if ttipocli-gdo
                    set TrattamentoGDO to true
                 else
                    set TrattamentoGDO to false
                 end-if
                 initialize prg-chiave
                 move mro-cod-articolo to prg-cod-articolo art-codice
                 read articoli no lock
                 read progmag  no lock
                 perform CALCOLA-COSTO-MP-COMPLETO
                 move costo-mp to tag-prz
              end-if
              move mro-chiave to tag-mro-chiave
              compute tag-qta =  mro-qta - mro-qta-e
              accept tag-data-creazione from century-date
              accept tag-ora-creazione  from time
              move "AUTO" to tag-utente-creazione
              perform until 1 = 2
                 add 1 to tag-prog
                 write tag-rec 
                       invalid continue
                   not invalid exit perform
                 end-write
              end-perform
           end-if.
           close tagli.

      ***---
       STATO-RIGHE.
           move 0 to tot-righe-master 
                     tot-righe-chiuse
                     tot-righe-aperte
                     tot-righe-chiusura2.
           move low-value  to mro-rec.
           move mto-chiave to mro-chiave-testa
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    add 1 to tot-righe-master
                    move mro-stato to stato-originale-riga
                    if mro-si-blister
                       move mro-bli-qta      to como-qta-imballi
                    else
                       move mro-qta-imballi  to como-qta-imballi
                    end-if
                    compute qta-evadere = mro-qta - mro-qta-e
                    if qta-evadere < 0
                       move 0 to qta-evadere
                    end-if
                       
                    set mro-registrato to true 

      *****              |CANCELLARE ALLA PRIMA INSTALLAZIONE
      *****              |PRESSO LUBEX
      *****              if mro-promo = 0
      *****                |La scorta prevale su tutto
      *****                 move mro-cod-articolo to art-codice
      *****                 read articoli no lock invalid continue end-read
      *****                 move art-scorta to sco-codice
      *****                 read tscorte no lock
      *****                      invalid continue
      *****                  not invalid
      *****                      if sco-chiu-forzata-si
      *****                         set mro-chiuso to true
      *****                      end-if
      *****                 end-read
      *****
      *****                 if mro-chiuso
      *****                    continue
      *****                 else
      *****                   |Evasione 0 e registrato riga "REG"
      *****                    if mro-qta-e = 0 and mto-registrato
      *****                       set mro-registrato to true
      *****                    else
      *****                       if qta-evadere < como-qta-imballi
      *****                          set mro-chiuso to true
      *****                       else
      *****                          if mto-in-lavorazione or
      *****                             mto-sped-parz      or
      *****                             mto-sped-tot
      *****                             if mro-qta-e = 0
      *****                                if mto-saldi-banco-si
      *****                                   set mro-registrato to true
      *****                                else
      *****                                   set mro-chiuso     to true
      *****                                end-if
      *****                             else
      *****                                if qta-evadere >= como-qta-imballi
      *****                                   if mto-saldi-banco-si
      *****                                      if mro-qta-b = 0
      *****                                         set mro-in-lavorazione
      *****                                          to true
      *****                                      else
      *****                                         set mro-sped-parz to true
      *****                                      end-if
      *****                                   end-if
      *****                                end-if
      *****                             end-if
      *****                          end-if
      *****                       end-if
      *****                    end-if
      *****                 end-if
      *****              else
      *****                 |Evasione 0 e registrato riga "REG"
      *****                 if mro-qta-e = 0 and mto-registrato
      *****                    set mro-registrato to true
      *****                 else
      *****                    if qta-evadere < como-qta-imballi
      *****                       set mro-chiuso to true
      *****                    else
      *****                       evaluate true
      *****                       when mro-tenere-saldo
      *****                            set mto-saldi-promo-si to true
      *****                       when mro-tagliare-merce
      *****                            set mto-saldi-promo-no to true
      *****                       end-evaluate
      *****                       if mto-saldi-promo-no
      *****                          set mro-chiuso to true
      *****                       else
      *****                          perform VALIDITA-PROMO
      *****                          if promo-valida
      *****                             if mro-qta-e = 0
      *****                                set mro-registrato to true
      *****                             else
      *****                                if qta-evadere >= como-qta-imballi
      *****                                   if mro-qta-b = 0
      *****                                      set mro-in-lavorazione
      *****                                       to true
      *****                                   else
      *****                                      set mro-sped-parz to true
      *****                                   end-if
      *****                                end-if
      *****                             end-if
      *****                          else
      *****                             set mro-chiuso to true
      *****                          end-if
      *****                       end-if
      *****                    end-if
      *****                 end-if
      *****              end-if
      ***********        ||||||||

                    |LIBERARE ALLA PRIMA INSTALLAZIONE IN LUBEX
                    |LIBERATO IL 11/10/2010
                    |La scorta prevale su tutto
                    move mro-cod-articolo to art-codice
                    read articoli no lock invalid continue end-read
                    move art-scorta to sco-codice
                    read tscorte no lock
                         invalid continue
                     not invalid
                         if sco-chiu-forzata-si
                            set mro-chiuso to true
                         end-if
                    end-read
      
                    if mro-chiuso
                       continue
                    else
                       perform IMPOSTA-STATO-RIGA
                    end-if
                    ||||||||||||||||

                    if mro-chiuso
                       add 1 to tot-righe-chiuse
                    else
                       if sco-chiu-forzata2-si
                          add 1 to tot-righe-chiusura2
                       end-if
                       add 1 to tot-righe-aperte
                    end-if
                    rewrite mro-rec

                    |Devo ripristinare l'impegnato per le righe che 
                    |hanno qta ancora da evadere nel  caso in cui:

                    |1. Riapro un ordine percedentemente chiuso
                    if mro-qta > mro-qta-e
                       |Sto riaprendo l'ordine
                       |Settato a TRUE dal programma di gestione
                       |evasioni perchè è già al loro interno
                       if stato-originale = 7 and not NoRipristino
                          if mro-registrato     or
                             mro-in-lavorazione or
                             mro-sped-parz      or
                             mro-sped-tot
                             perform RIPRISTINA-IMPEGNATO
                          end-if
                       else
                       |2. Riapro una riga precedentemente chiusa
                          evaluate stato-originale-riga
                          when 7
                               if mro-registrato     or
                                  mro-in-lavorazione or
                                  mro-sped-parz      or
                                  mro-sped-tot
                                  perform RIPRISTINA-IMPEGNATO
                               end-if
                          |Ma devo stornare l'impegnato quando chiudo
                          |una riga precedenemente aperta
                          when other

                               if mro-chiuso
                                  perform STORNA-IMPEGNATO
                                  perform TAGLI-LAB
                               end-if

                          end-evaluate
                       end-if
                    end-if

                 end-perform
           end-start.

      ***---
       IMPOSTA-STATO-RIGA.
           if mro-promo = 0
              |Evasione 0 e registrato riga "REG"
              if mro-qta-e = 0 and mto-registrato
                 set mro-registrato to true
              else
                 if qta-evadere < como-qta-imballi
                    set mro-chiuso to true
                 else
                    if mto-in-lavorazione or
                       mto-sped-parz      or
                       mto-sped-tot
                       if mro-qta-e = 0
                          if mto-saldi-banco-si
                             set mro-registrato to true
                          else
                             set mro-chiuso     to true
                          end-if
                       else
                          if mto-saldi-banco-si
                             if mro-qta-b = 0
                                set mro-in-lavorazione to true
                             else
                                set mro-sped-parz      to true
                             end-if
                          else
                             if mro-qta-e not = 0
                                set mro-chiuso to true
                             end-if
                          end-if
                       end-if
                    end-if
                 end-if
              end-if
           else
              |Evasione 0 e registrato riga "REG"
              if mro-qta-e = 0 and mto-registrato
                 set mro-registrato to true
              else
                 if qta-evadere < como-qta-imballi
                    set mro-chiuso to true
                 else
                    move save-mto-saldi-promo to mto-saldi-promo
      *****              evaluate true


      *****              when mro-tenere-saldo
      *****                   set mto-saldi-promo-si to true
      *****              when mro-tagliare-merce
      *****                   set mto-saldi-promo-no to true
      *****              when other
      *****                   move save-mto-saldi-promo to mto-saldi-promo
      *****              end-evaluate
                    |Se il master non è in REG e non tengo saldi
                    |promo dopo la prima evasione chiudo
                    if mto-saldi-promo-no
                       set mro-chiuso to true
                    else
                       perform VALIDITA-PROMO
                       if promo-valida
                          if mro-qta-e = 0
                             set mro-registrato to true
                          else
                             if mro-qta-b = 0
                                set mro-in-lavorazione to true
                             else
                                set mro-sped-parz to true
                             end-if
                          end-if
                       else
                          set mro-chiuso to true
                       end-if
                    end-if
                 end-if
              end-if
           end-if.

      ********---
      ***** QTA-MINIMA-IMBALLO.
      *****     move 0 to link-wprogmag-status.
      *****     move low-value  to mro-chiave.
      *****     move mto-chiave to mro-chiave-testa.
      *****     start mrordini key >= mro-chiave
      *****           invalid continue
      *****       not invalid
      *****           perform until 1 = 2
      *****              read mrordini next at end exit perform end-read
      *****              if mro-chiave-testa not = mto-chiave
      *****                 exit perform
      *****              end-if
      *****              compute qta-evadere = mro-qta-e - mro-qta
      *****              if qta-evadere = 0
      *****                 continue
      *****              else
      *****
      *****                 if mro-si-blister
      *****                    move mro-bli-qta      to como-qta-imballi
      *****                 else
      *****                    move mro-qta-imballi  to como-qta-imballi
      *****                 end-if
      *****
      *****                 if qta-evadere < como-qta-imballi
      *****
      *****                    initialize link-wprogmag
      *****                    compute link-valore    = 
      *****                          ( mro-qta-e      +
      *****                            como-qta-imballi ) - mro-qta
      *****                    move link-valore     to link-impegnato
      *****                    set link-update      to true
      *****                    move mro-prg-chiave  to link-key
      *****                    move mto-causale     to link-causale
      *****      
      *****                    set link-update-um      to true
      *****                    set link-update-peso    to false
      *****                    set link-update-valore  to false
      *****                    move "0000000000000000" to link-array
      *****                    move link-causale       to tca-codice
      *****                    read tcaumag no lock invalid continue end-read
      *****                    |In caso sia permessa la stampa della bolla
      *****                    |agisco sull'impegnato, altrimenti l'ordine
      *****                    |è da considerarsi già bollettato ed agisco
      *****                    |direttamente sulla giacenza e non sull'impegnato
      *****                    if tca-si-stampa 
      *****                       move 1 to multiplyer(2)
      *****         
      *****                       set cli-tipo-C to true
      *****                       move mto-cod-cli to cli-codice
      *****                       read clienti no lock
      *****                       perform DIREZIONA-IMPEGNATO
      *****                    else
      *****                       move 1 to multiplyer(1)
      *****                       move 1 to multiplyer(15)
      *****                    end-if
      *****                    move user-codi to link-user of link-wprogmag
      *****                    call   "wprogmag" using link-wprogmag
      *****                    cancel "wprogmag"
      *****                    compute mro-qta = mro-qta-e + como-qta-imballi
      *****                    rewrite mro-rec
      *****                 end-if
      *****              end-if
      *****           end-perform
      *****     end-start.

      ***---
       RIPRISTINA-IMPEGNATO.
           set ImpegnatoRipristinato to true.
           |RIAPRO PERTANTO RIPRISTINO L'IMPEGNATO
           |LEVATO NELLA PRECEDENTE FASE DI CHIUSURA
           initialize link-wprogmag.
           compute link-valore    = mro-qta-e - mro-qta.
           compute link-impegnato = mro-qta-e - mro-qta.
           set link-update      to true.
           move mro-prg-chiave  to link-key.
           move mto-causale     to link-causale.
            
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
              move -1 to multiplyer(2)

              set cli-tipo-C to true
              move mto-cod-cli to cli-codice
              read clienti no lock
                   invalid
                   set no-cli to true
                   exit paragraph
              end-read    
              move cli-tipo to tcl-codice
              read ttipocli no lock invalid continue end-read
              perform DIREZIONA-IMPEGNATO
           else
              move -1 to multiplyer(1)
              move -1 to multiplyer(15)
           end-if.
           move user-codi to link-user of link-wprogmag.
           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".

      ***---
       VALIDITA-PROMO.
           set promo-valida to false.
           move mro-promo to tpr-codice.
           read tpromo no lock
                invalid
                move 0 to mro-promo
                rewrite mro-rec invalid continue end-rewrite
            not invalid
                if vecchia-evasione
      *****             if ecd-gg-scadenza-vol-OLD not = 0
      *****                move ecd-gg-scadenza-vol-OLD
      *****                  to tge-gg-scadenza-vol-OLD
      *****             end-if
      *****             accept DataOggi from century-date
      *****             compute como-data =
      *****                     function integer-of-date
      *****                            ( tpr-fine-volantino )
      *****             add tge-gg-scadenza-vol-OLD to como-data
      *****             compute como-data = function
      *****                     date-of-integer(como-data)
      *****             if como-data >= DataOggi
      *****                set promo-valida to true
      *****             end-if
                   continue
                else
                   accept DataOggi from century-date
                   compute como-data =
                           function integer-of-date
                                  ( tpr-fine-volantino )
                   add prm-gg-val-vol to como-data
                   compute como-data = function
                           date-of-integer(como-data)
                   if como-data >= DataOggi
                      set promo-valida to true
                   end-if
                end-if
           end-read.

      ***---
       CHIUSURA-RIGHE.
           |Chiudo comunque tutte le righe (retroattivo)
           move low-value  to mro-rec.
           move mto-chiave to mro-chiave-testa.
           start mrordini key >= mro-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              read mrordini next 
                   at end exit perform 
              end-read
              if mro-chiave-testa not = mto-chiave
                 exit perform
              end-if
              if mro-chiuso
                 continue
              else
                 |Ho ripristinato un impegnato di un master chiuso
                 |ma in seguito ho dovuto chiudere il master di nuovo
                 if ImpegnatoRipristinato
                    perform STORNA-IMPEGNATO
                 end-if
                 set mro-chiuso to true
                 if mto-promo-fittizia not = 0
                    move 0 to mro-promo
                 end-if
                 rewrite mro-rec
              end-if
           end-perform.
           set ImpegnatoRipristinato to false.

      ***---
       CHIUSURA-PROMO-FITTIZIA.
           move mto-promo-fittizia to tpr-codice.
           delete tpromo record invalid continue end-delete.
           move low-value to rpr-rec.
           move mto-promo-fittizia to rpr-codice.
           start rpromo key >= rpr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rpromo next at end exit perform end-read
                    if rpr-codice not = mto-promo-fittizia
                       exit perform
                    end-if
                    delete rpromo record invalid continue end-delete
                 end-perform
           end-start.
           move 0 to mto-promo-fittizia.
           set mto-no-promo to true.
           move low-value  to mro-rec.
           move mto-chiave to mro-chiave-testa.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    if mro-promo >= 999999
                       move mro-promo        to rpr-codice
                       move mro-cod-articolo to rpr-articolo
                       read rpromo no lock
                            invalid continue
                        not invalid
                            if mro-qta > rpr-qta
                               move 0 to rpr-qta
                            else
                               compute rpr-qta = rpr-qta - mro-qta
                            end-if
                            if rpr-qta = 0
                               delete rpromo record
                            else
                               rewrite rpr-rec
                            end-if
                       end-read
                       move 0 to mro-promo
                       rewrite mro-rec
                    end-if
                 end-perform
           end-start.

      ***---
       APERTURA-PROMO-FITTIZIA.
           |TESTA
           accept tpr-gdo from environment "GDO_FITTIZIA".
           move mto-data-ordine to tpr-ini-dpo.
           compute como-data = 
                   function integer-of-date (mto-data-ordine).
           add 730 to como-data.
           compute tpr-fine-dpo = function date-of-integer (como-data).
           move tpr-ini-dpo    to tpr-ini-volantino.
           move tpr-fine-dpo   to tpr-fine-volantino.
           set tpr-nazionale   to true.
           set tpr-fittizia-si to true.
           move mto-chiave     to tpr-chiave-master.

           set  cli-tipo-C  to true.
           move mto-cod-cli to cli-codice.
           read clienti no lock
                invalid set no-cli to true
           end-read.
           inspect cli-ragsoc-1 replacing trailing spaces by low-value.

           string mto-anno     delimited size
                  "_"          delimited size
                  mto-numero   delimited size
                  "_"          delimited size
                  cli-ragsoc-1 delimited low-value
                  into tpr-descrizione
           end-string.
           inspect cli-ragsoc-1 replacing trailing low-value by spaces.

           accept tpr-data-creazione from century-date.
           accept tpr-ora-creazione  from time.
           move user-codi to tpr-utente-creazione.
           
           move 999999 to tpr-codice.
           perform until 1 = 2
              write tpr-rec
                    invalid add 1 to tpr-codice
                not invalid exit perform
              end-write
           end-perform.
           set mto-si-promo to true.
           move tpr-codice  to mto-promo-fittizia.

           |RIGHE
           move low-value  to mro-rec.
           move mto-chiave to mro-chiave-testa.
           start mrordini key >= mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read mrordini next at end exit perform end-read
                    if mro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    if mro-chiuso
                       move 0 to mro-promo
                    else
                       move 0                  to rpr-prz-acq
                       move mto-promo-fittizia to rpr-codice
                       move mro-cod-articolo   to rpr-articolo
                       |In caso di articolo doppio
                       read rpromo no lock
                            invalid compute rpr-qta = 
                                            mro-qta - mro-qta-e
                        not invalid compute rpr-qta = 
                                            rpr-qta + 
                                          ( mro-qta - mro-qta-e )
                       end-read
                       move mro-prz-unitario to rpr-prz-ven
                       accept rpr-data-creazione from century-date
                       accept rpr-ora-creazione  from time
                       move user-codi to rpr-utente-creazione
                       move mro-chiave         to rpr-mro-chiave
                       write rpr-rec invalid   rewrite rpr-rec end-write
                       move mto-promo-fittizia to mro-promo
                    end-if
                    rewrite mro-rec
                 end-perform
           end-start. 

      ***---
       PARAGRAFO-COPY.
           copy "costo-medio.cpy".
           copy "recupero-anagrafica.cpy".
