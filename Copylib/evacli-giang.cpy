      ***---
       MASTER-NUOVI-CONTROLLI.
           set record-ok to false.
           set trovato   to false. 

           perform varying idx from 1 by 1 
                     until idx > 50
              if prm-mag-codice(idx) = save-magazzino
                 set trovato to true
                 exit perform
              end-if
           end-perform.

           if trovato
              if chk-tutto-buf = 0 or
               ( chk-tutto-buf = 1 and prm-escludi-tutto-si )
                 compute como-data = 
                         function integer-of-date (data-oggi)
                 if save-gg-cons-max > 0
                    add save-gg-cons-max to como-data
                 else
                    add prm-gg-cons-max to como-data
                 end-if
                 compute como-data = 
                         function date-of-integer (como-data)
                 if mto-data-note1 <= como-data
                    set record-ok to true
                 end-if
              else
                 set record-ok to true
              end-if
              if record-ok
                 move save-magazzino to mag-codice
                 read tmagaz no lock
                 if mto-ritira-si and mag-ritira-lbx-no
                    set record-ok to false
                 end-if
              end-if
           end-if.
           if record-ok
              perform VALUTA-RIGHE-SCORTA
              if record-ok
                 if righe-ok = 0
                    set record-ok to false
                 end-if
              end-if
           end-if.
           if record-ok
              move mto-chiave to k-mto-chiave
              move mto-promo  to k-mto-promo
              if mto-promo = 0
                 set k-mto-evadibile-si to true
              else
                 set k-mto-evadibile-no to true
              end-if
              if righe-blister = 0
                 set k-mto-blister-no to true
              else
                 set k-mto-blister-si to true
              end-if
              move 0 to k-mto-evasa k-mto-da-evadere
              write k-mto-rec invalid continue end-write
              add 1 to tot-master
           end-if.

      ***---
       VALUTA-RIGHE-SCORTA.
           move 0 to righe-ok.
           move 0 to righe-blister.
           move low-value  to mro-rec.
           move mto-chiave to mro-chiave-testa.
           start mrordini key >= mro-chiave
                 invalid continue 
           end-start.
           perform until 1 = 2
              read mrordini next at end exit perform end-read
              if mro-chiave-testa not = mto-chiave
                 exit perform
              end-if
              if mro-qta <= mro-qta-e or
                 mro-chiuso
                 continue
              else
                 if mro-si-blister
                    add 1 to righe-blister
                 end-if                    
                 move save-magazzino to mag-codice
                 read tmagaz no lock 
                 if mro-si-blister and mag-blister-no
                    set record-ok to false
                    exit perform
                 end-if
                 if mro-registrato     or
                    mro-in-lavorazione or
                    mro-sped-parz      or
                    mro-sped-tot
                    move mro-prg-cod-articolo to art-codice
                    read articoli no lock
                    set trovato to false
                    perform varying idx from 1 by 1 
                              until idx > 20
                       if mag-sco-codice(idx) not = spaces
                          call "C$JUSTIFY" using mag-sco-codice(idx),"R"
                          inspect mag-sco-codice(idx) 
                                  replacing leading x"20" by x"30"
                          move mag-sco-codice(idx) to sco-codice
                          if sco-codice = art-scorta
                             add 1 to righe-ok
                             exit perform
                          end-if
                       end-if
                    end-perform
                 end-if
              end-if
           end-perform.

      ***---
       CICLO-PROMO.
           close      promoeva.
           open input promoeva.
           move low-value to pev-rec.
           start promoeva key >= pev-chiave
                 invalid  continue
             not invalid
                 perform until 1 = 2
                    read promoeva next at end exit perform end-read
                    perform COUNTER-VIDEO
                    move pev-articolo to tqe-articolo
                    read tmp-qta-eva no lock
                         invalid move 0 to tqe-tot-pren
                    end-read
      *****              compute tqe-tot-pren  =
      *****                      tqe-tot-pren  + pev-rimanenza
                    compute tqe-tot-pren  =
                            tqe-tot-pren  + pev-giac-utile
                    write tqe-rec invalid rewrite tqe-rec end-write
                 end-perform
           end-start.

           |FILTRO SOLO LE RIGHE PROMO E BANCO VALIDE
           move low-value to k-mto-rec.
           start tmp-k-mtordini key >= k-mto-chiave
                 invalid continue
           end-start.
           perform until 1 = 2
              set record-ok to true
              read tmp-k-mtordini next at end exit perform end-read
              move k-mto-chiave to mro-chiave-testa mto-chiave
              read mtordini no lock
              move low-value    to mro-riga
              start mrordini key >= mro-chiave
                    invalid continue
              end-start
              perform until 1 = 2
                 read mrordini next at end exit perform end-read
                 if mro-chiave-testa not = k-mto-chiave
                    exit perform
                 end-if
                 if mro-bli-codice = 0
                    move mro-qta-imballi  to como-qta-imballi
                 else
                    move mro-bli-qta      to como-qta-imballi
                 end-if
                 if mro-promo not = 0
                    |Se il master è in stato di REG non controllo
                    |la scadenza della promo, è sempre valida
                    move mro-chiave-testa to mto-chiave
                    read mtordini no lock
                    if mto-registrato
                       move 99999999 to como-data
                    else
                       move mro-promo to tpr-codice
                       read tpromo no lock invalid continue end-read
                       compute como-data = 
                           function integer-of-date (tpr-fine-volantino)
                       move mto-cod-cli     to como-prm-cliente
                       move mto-prg-destino to como-prm-destino
                       perform TROVA-PARAMETRO
                       add prm-gg-val-vol to como-data
                       compute como-data = 
                               function date-of-integer(como-data)
                    end-if
                 else
                    move 99999999 to como-data
                    |Una riga banco su master promo lo rende evadibile FORSE
                    if k-mto-promo not = 0 and k-mto-evadibile-no
                       set k-mto-evadibile-forse to true
                       rewrite k-mto-rec
                    end-if
                 end-if
                 move mro-prg-cod-articolo to art-codice
                 perform VALORIZZA-GIACENZA-MAGAZZINO
                 if como-data      > data-oggi and
                    mro-evadi-dal <= data-oggi
                    if mro-chiuso        or
                       mro-qta <= mro-qta-e or not trovato-prog 
                       continue
                    else
                       set trovato to false
                       move mro-prg-cod-articolo to art-codice  
                       read articoli no lock invalid continue end-read
                       move save-magazzino to mag-codice
                       read tmagaz no lock
                       perform varying idx from 1 by 1 
                                 until idx > 20
                          if mag-sco-codice(idx) not = spaces
                             call "C$JUSTIFY" 
                               using mag-sco-codice(idx), "R"
                             inspect mag-sco-codice(idx) 
                                     replacing leading x"20" by x"30"
                             move mag-sco-codice(idx) to sco-codice
                             if sco-codice = art-scorta
                                set trovato to true
                                exit perform
                             end-if
                          end-if
                       end-perform
                       if trovato
                          move art-scorta           to sco-codice
                          read tscorte no lock
                          if sco-forzata-si
                             move mro-chiave       to k-mro-chiave
                             move mro-prg-cod-articolo to k-mro-articolo
                             move mro-qta          to k-mro-ordinata
                             move mro-qta-e        to k-mro-evasa
                             compute k-mro-evadibile = mro-qta -
                                                       mro-qta-e
                             move k-mro-evadibile  to k-mro-evadibile-ok
                             move mro-promo        to k-mro-promo
                             move mro-bli-codice   to k-mro-bli-codice
                             if mro-bli-codice = 0
                                set k-mro-blister-no to true
                             else
                                if mro-qta-imballi not  = 0
                                   set k-mro-blister-testa to true
                                else
                                   set k-mro-blister-righe to true
                                end-if
                                move mro-bli-qta    to mro-qta-imballi                          
                             end-if
                             move mro-qta-imballi      to k-mro-qta-imb
                             perform ASSEGNA-PROGRESSIVO
                             set k-mro-da-cancellare   to false
                             move mro-evadi-dal to k-mro-evadi-dal
                             write k-mro-rec
                          else
                             |Se quanto ho a disposizione non copre
                             |nemmeno un imballo non la considero 
                             if giac-utile >= como-qta-imballi
                                move mro-chiave    to k-mro-chiave
                                move mro-prg-cod-articolo 
                                                   to k-mro-articolo
                                move mro-qta       to k-mro-ordinata
                                move mro-qta-e     to k-mro-evasa
                                move 0             to k-mro-evadibile
                                move 0             to k-mro-evadibile-ok
                                move mro-promo     to k-mro-promo
                                move mro-bli-codice to k-mro-bli-codice
                                if mro-bli-codice = 0
                                   set k-mro-blister-no to true
                                else
                                   if mro-qta-imballi not  = 0
                                      set k-mro-blister-testa to true
                                   else
                                      set k-mro-blister-righe to true
                                   end-if
                                   move mro-bli-qta  to mro-qta-imballi                          
                                end-if
                                move mro-qta-imballi to k-mro-qta-imb
                                perform ASSEGNA-PROGRESSIVO
                                set k-mro-da-cancellare   to false
                                move mro-evadi-dal to k-mro-evadi-dal
                                write k-mro-rec
                             else
                                |La scrivo comunque, dopo verrà cancellata,
                                |ma mi serve per il controllo successivo dove
                                |un'evadibilità zero su una riga mi rende
                                |il master non evadibile (CICLO-EVADIBILITA-PROMO)
                                move mro-chiave    to k-mro-chiave
                                move mro-prg-cod-articolo 
                                                   to k-mro-articolo
                                move mro-qta       to k-mro-ordinata
                                move mro-qta-e     to k-mro-evasa
                                move 0             to k-mro-evadibile
                                move 0             to k-mro-evadibile-ok
                                move mro-promo     to k-mro-promo
                                move mro-bli-codice to k-mro-bli-codice
                                if mro-bli-codice = 0
                                   set k-mro-blister-no to true
                                else
                                   if mro-qta-imballi not  = 0
                                      set k-mro-blister-testa to true
                                   else
                                      set k-mro-blister-righe to true
                                   end-if
                                end-if
                                move mro-qta-imballi to k-mro-qta-imb
                                perform ASSEGNA-PROGRESSIVO
                                set k-mro-da-cancellare   to true
                                move mro-evadi-dal to k-mro-evadi-dal
                                write k-mro-rec
                             end-if
                          end-if
                       end-if
                    end-if
                 end-if
              end-perform
           end-perform.

           |CICLO PROMO 1: ASSEGNO QTA PRENOTATA
           perform until 1 = 2
              move 0 to righe-ok
              move low-value to k-mro-rec
              move 1 to k-mro-promo
              start tmp-k-mrordini key >= k-mro-promo
                    invalid continue
                not invalid
                    perform until 1 = 2

                       perform COUNTER-VIDEO

                       read tmp-k-mrordini next 
                            at end exit perform 
                       end-read
                       
                       if k-mro-ordinata > 
                        ( k-mro-evasa + k-mro-evadibile ) and 
                          not k-mro-da-cancellare         and
                              k-mro-evadi-dal <= data-oggi
                          move k-mro-articolo to tqp-articolo
                          move k-mro-promo    to tqp-promo
                          read tmp-qta-pren no lock
                               invalid continue
                           not invalid
                               if tqp-prenotata >= k-mro-qta-imb
                                  add k-mro-qta-imb to k-mro-evadibile
                                  subtract k-mro-qta-imb 
                                      from tqp-prenotata
                                  rewrite tqp-rec
                                  move k-mro-articolo to tqe-articolo
                                  read tmp-qta-eva no lock
                                       invalid
                                       display message "TQE"
                                                 title tit-err
                                   not invalid
                                       add k-mro-qta-imb 
                                        to tqe-tot-pren-assegnata
                                       subtract k-mro-qta-imb 
                                           from tqe-tot-pren
                                       rewrite tqe-rec
                                  end-read
                                  |NON DEVO SCALARE DALLA GIACENZA UTILE
                                  |PERCHE' VIENE FATTO ALL'INIZIO
      *****                            move k-mro-articolo to tqe-articolo
      *****                            read tmp-qta-eva no lock
      *****                            subtract k-mro-qta-imb from tqe-giac-utile
      *****                            rewrite tqe-rec
                                  move k-mro-evadibile 
                                    to k-mro-evadibile-ok
                                  rewrite k-mro-rec
                                  add 1 to righe-ok
                               end-if
                          end-read
                       end-if
                    end-perform
              end-start
              if righe-ok = 0
                 exit perform
              end-if
           end-perform.

           |SE LA QUANTITA' PRENOTATA RIMANENTE DOPO L'ASSEGNAZIONE
           |E' PRESENTE IN ALTRI MAGAZZINI LA LIBERO AUMENTANDO
           |LA DISPONIBILITA' DEL MAGAZZINO IN USO
           move low-value to tqe-rec.
           start tmp-qta-eva key >= tqe-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-qta-eva next at end exit perform end-read
                    move low-value    to prg-rec
                    move tqe-articolo to prg-cod-articolo
                    move 0 to como-giacenza GiacenzaMAG
                    start progmag key >= prg-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read progmag next 
                                  at end exit perform 
                             end-read
                             if prg-cod-articolo not = tqe-articolo
                                exit perform
                             end-if
                             if prg-cod-magazzino not = spaces
                                if prg-cod-magazzino = save-magazzino
                                   if GdoInUso
                                      compute GiacenzaMAG =
                                              GiacenzaMAG +
                                            ( prg-giacenza  -
                                            ( prg-impegnato -
                                            ( prg-imp-TRAD +
                                              prg-imp-GDO ) ) )
                                   else
                                      compute GiacenzaMAG = 
                                              GiacenzaMAG +
                                            ( prg-giacenza  -
                                            ( prg-impegnato -
                                              prg-imp-master ) )
                                   end-if
                                else
                                   move prg-cod-magazzino to mag-codice
                                   read tmagaz no lock
                                   if mag-per-promo-si
                                      if GdoInUso
                                         compute como-giacenza = 
                                                 como-giacenza +
                                               ( prg-giacenza  -
                                               ( prg-impegnato -
                                               ( prg-imp-TRAD +
                                                 prg-imp-GDO ) ) )
                                      else
                                         compute como-giacenza = 
                                                 como-giacenza +
                                               ( prg-giacenza  -
                                               ( prg-impegnato -
                                                 prg-imp-master ) )
                                      end-if
                                   end-if
                                end-if
                             end-if
                          end-perform
                    end-start
                    if GiacenzaMAG < 0
                       move 0 to GiacenzaMAG
                    end-if
                    if como-giacenza < 0
                       move 0 to como-giacenza
                    end-if
                    |PRINCIPIO: assegno al MASSIMO quanto ho in MAG

                    
                    |GiacenzaMAG al NETTO di quanto
                    |ho assegnato come qta prenotata
                    subtract tqe-tot-pren-assegnata from GiacenzaMag
                    |La disponibilità del magazzino copre PER INTERO
                    |assegno tutta la giacenza di altri magazzini...
                    if GiacenzaMag >= tqe-tot-pren
                       add como-giacenza to tqe-giac-utile
                       |...senza sforare da quanto ho nel MAG
                       if tqe-giac-utile > GiacenzaMAG
                          move GiacenzaMAG to tqe-giac-utile
                       end-if
                    else
                       |Se le qta coprono la qta prenotata
                       if GiacenzaMag + como-giacenza > tqe-tot-pren
                          |Il totale della qta prenotata è coperta
                          |totalmente dai magazzini esterni assegno
                          |quanto ho ancora disponibile in MAG
                          if como-giacenza > tqe-tot-pren
                             move GiacenzaMag to tqe-giac-utile
                          else
                             compute tqe-giac-utile =
                                     GiacenzaMAG - 
                                   ( tqe-tot-pren - como-giacenza )
                          end-if
                       end-if
                    end-if
      *****              |LA USO SOLO SE NEL MAGAZZINO IN USO
      *****              |HO COMUNQUE PRESENTE LA GIACENZA                    
      *****              if como-giacenza > GiacenzaMAG
      *****                 move GiacenzaMAG to como-giacenza
      *****              end-if
      *****              |AGGIUNGO ALLA DISPONIBILITA' QUANTO
      *****              |HO MESSO VIA NEGLI ALTRI MAGAZZINI
      *****              if como-giacenza > tqe-tot-pren
      *****                 add tqe-tot-pren  to tqe-giac-utile
      *****              else
      *****                 if como-giacenza > 0
      *****                    add como-giacenza to tqe-giac-utile
      *****                 end-if
      *****              end-if
                    rewrite tqe-rec
                 end-perform
           end-start.

           |CICLO PROMO 2: ASSEGNO QTA GIACENZA UTILE (RIMANENZA)
           perform until 1 = 2
              move 0 to righe-ok
              move low-value to k-mro-rec
              move 1 to k-mro-promo
              start tmp-k-mrordini key >= k-mro-promo
                    invalid continue
                not invalid
                    perform until 1 = 2

                       perform COUNTER-VIDEO

                       read tmp-k-mrordini next 
                            at end exit perform 
                       end-read
                       
                       if k-mro-ordinata > 
                        ( k-mro-evasa + k-mro-evadibile ) and 
                          not k-mro-da-cancellare         and
                              k-mro-evadi-dal <= data-oggi
                          move k-mro-articolo to tqe-articolo
                          read tmp-qta-eva no lock
                               invalid continue
                           not invalid
                               if tqe-giac-utile >= k-mro-qta-imb
                                  add k-mro-qta-imb to k-mro-evadibile
                                  subtract k-mro-qta-imb 
                                      from tqe-giac-utile
                                  move k-mro-evadibile 
                                    to k-mro-evadibile-ok
                                  rewrite k-mro-rec
                                  rewrite tqe-rec
                                  add 1 to righe-ok
                               end-if
                          end-read
                       end-if
                    end-perform
              end-start
              if righe-ok = 0
                 exit perform
              end-if
           end-perform.

      ***---
       ASSEGNA-PROGRESSIVO.
           |Se evado con lo stesso magazzino uso il progressivo del master
           if mro-prg-cod-magazzino = save-magazzino
              move mro-prg-chiave to k-mro-prg-chiave
           else
              |1. provo a cercare il corrispondente sul magazzino di evasione
              move mro-prg-chiave to prg-chiave
              move save-magazzino to prg-cod-magazzino
              read progmag no lock
                   invalid
                   |2. Se non c'è quello maggior giacenza valore assoluto.
                   |   Salvata prima durante la valorizzazione della giacenza
                   move save-prg-chiave to k-mro-prg-chiave
               not invalid
                   move prg-chiave to k-mro-prg-chiave
              end-read
           end-if.

      ***---
       VALORIZZA-GIACENZA-MAGAZZINO.
           set trovato-prog to false.
           initialize save-prg-chiave replacing numeric data by zeroes
                                           alphanumeric data by spaces.
           move 0 to giac-utile giac-assoluta save-giacenza-assoluta.
           move low-value to prg-rec.
           move art-codice     to prg-cod-articolo.
           move save-magazzino to prg-cod-magazzino.
           start progmag key > prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo  not = art-codice or
                       prg-cod-magazzino not = save-magazzino
                       exit perform
                    end-if           
                    set trovato-prog to true
                    move prg-giacenza to giac-assoluta
                    if giac-assoluta > save-giacenza-assoluta
                       move prg-chiave    to save-prg-chiave
                       move giac-assoluta to save-giacenza-assoluta
                    end-if

                    if GdoInUso
                       |Non dovrebbe succedere ma tutelo in caso di errore
                       if prg-impegnato < prg-imp-TRAD + prg-imp-GDO
                          move 0 to prg-impegnato 
                                    prg-imp-TRAD 
                                    prg-imp-GDO
                       end-if
                       compute como-giacenza = prg-giacenza  -
                                             ( prg-impegnato -
                                             ( prg-imp-TRAD +
                                               prg-imp-GDO ) )
                    else
                       |Non dovrebbe succedere ma tutelo in caso di errore
                       if prg-impegnato < prg-imp-master
                          move 0 to prg-impegnato
                                    prg-imp-master
                       end-if
                       compute como-giacenza = prg-giacenza  -
                                             ( prg-impegnato -
                                               prg-imp-master )
                    end-if
                    add como-giacenza to giac-utile
                 end-perform
           end-start.
           move art-codice to tqe-articolo.
           read tmp-qta-eva  no lock 
                invalid move 0 to tqe-tot-pren
           end-read.
           move mro-prg-cod-articolo to pev-articolo.
           move mro-promo            to pev-tpr-codice. 
           read promoeva no lock key pev-k-art
                invalid  move 0   to pev-prenotata 
                         move 0   to pev-giac-utile
           end-read.
           |Se la giacenza utile non copre la 
           |prenotata avrò 0 di disavanzo
           if giac-utile < tqe-tot-pren
              move 0 to tqe-giac-utile
           else
              |calcolo il disavanzo
              compute tqe-giac-utile =
                      giac-utile - tqe-tot-pren
           end-if.

           if mro-promo not = 0
              |Levo dalla giacenza utilizzabile quanto ho prenotato per
              |le promo di quell'articolo (saltando quella in gestione)
              move low-value     to tqp-rec
              move pev-articolo  to tqp-articolo
              start tmp-qta-pren key >= tqp-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tmp-qta-pren next 
                            at end exit perform 
                       end-read
                       if tqp-articolo not = pev-articolo
                          exit perform
                       end-if
                       if tqp-promo not = pev-tpr-codice
                          subtract tqp-prenotata from giac-utile
                       end-if
                    end-perform
              end-start
           end-if.

           move 0              to tqp-prenotata-orig.
           move pev-articolo   to tqp-articolo.
           move pev-tpr-codice to tqp-promo.
           compute tqp-prenotata = pev-giac-utile.

           if tqp-prenotata > giac-utile
              if giac-utile > 0
                 move giac-utile to tqp-prenotata
              else
                 move 0          to tqp-prenotata
              end-if
           end-if.
           move tqp-prenotata to tqp-prenotata-orig.

           write tqp-rec invalid rewrite tqp-rec end-write.
           write tqe-rec invalid rewrite tqe-rec end-rewrite.


      ***---
       EVASIONE-INTERA.
      * Per i magazzini COMPLETI, devo evadere solo i master
      * evadibili completamente
           move low-value to k-mto-rec.
           set k-mto-evadibile-si to true.
           start tmp-k-mtordini key >= tmp-k-valutare
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-k-mtordini next 
                         at end exit perform 
                    end-read
                    move k-mto-chiave to mto-chiave
                    read mtordini no lock
                    move mto-cod-cli     to como-prm-cliente
                    move mto-prg-destino to como-prm-destino
                    perform TROVA-PARAMETRO
                    set EvasioneIntera to false
                    perform varying mag-idx from 1 by 1 
                              until mag-idx > 50
                       if prm-mag-codice(mag-idx) = spaces
                          exit perform
                       end-if
                       if prm-mag-codice(mag-idx) = save-magazzino
                          if prm-mag-ev-intera-si(mag-idx)
                             set EvasioneIntera to true
                          end-if
                          exit perform
                       end-if
                    end-perform
                    if EvasioneIntera
                       move 0 to k-mto-da-evadere
                       move 0 to k-mto-evasa

                       move 0 to tot-righe-master
                       move low-value    to mro-rec
                       move k-mto-chiave to mro-chiave-testa
                       start mrordini key >= mro-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read mrordini next 
                                     at end exit perform 
                                end-read
                                if mro-chiave-testa not = k-mto-chiave
                                   exit perform
                                end-if
                                if mro-chiuso or 
                                   mro-evadi-dal > data-oggi
                                   continue
                                else
                                   if mro-qta > mro-qta-e
                                      add 1 to tot-righe-master
                                   end-if
                                end-if
                             end-perform
                       end-start

                       move 0 to tot-righe-evadibili
                       move low-value    to k-mro-rec
                       move k-mto-chiave to k-mro-chiave-testa
                       start tmp-k-mrordini key >= k-mro-chiave
                             invalid continue
                         not invalid
                             perform until 1 = 2
                                read tmp-k-mrordini next 
                                     at end exit perform 
                                end-read
                                if k-mro-chiave-testa not = k-mto-chiave
                                   exit perform
                                end-if
                                add 1 to tot-righe-evadibili
                       
                                compute k-mto-da-evadere = 
                                        k-mto-da-evadere + 
                                      ( k-mro-ordinata   - k-mro-evasa )
                                compute k-mto-evasa      = 
                                        k-mto-evasa  + k-mro-evadibile
                       
                             end-perform
                       end-start

                       if tot-righe-evadibili = tot-righe-master
                          if k-mto-evasa >= k-mto-da-evadere
                             set k-mto-evadibile-si    to true
                          else
                             set k-mto-evadibile-no to true
                          end-if
                       else
                          set k-mto-evadibile-no to true
                       end-if
                     
                       rewrite k-mto-rec invalid continue end-rewrite
                    end-if
                 end-perform
           end-start.

      ***---
       CICLO-EVADIBILITA-PROMO.
           |VALUTO QUALI MASTER PROMO SONO EVADIBILI
           move low-value to k-mro-rec.
           move 1 to k-mro-promo
           start tmp-k-mrordini key >= k-mro-promo
                 invalid continue
             not invalid
                 perform until 1 = 2

                    perform COUNTER-VIDEO

                    read tmp-k-mrordini next 
                         at end exit perform 
                    end-read

                    move k-mro-chiave-testa to k-mto-chiave
                    read tmp-k-mtordini no lock 
                         invalid continue 
                    end-read
                    
                    compute k-mto-da-evadere = 
                            k-mto-da-evadere + 
                          ( k-mro-ordinata   - k-mro-evasa )
                    compute k-mto-evasa      = 
                            k-mto-evasa      + k-mro-evadibile

                    if k-mto-evasa >= k-mto-da-evadere
                       set k-mto-evadibile-si    to true
                    else
                       set k-mto-evadibile-forse to true
                    end-if
                    rewrite k-mto-rec invalid continue end-rewrite
                    if k-mro-da-cancellare
                       delete tmp-k-mrordini record
                    end-if
                 end-perform
           end-start.

           move low-value to k-mto-rec.
           set k-mto-evadibile-forse to true.
           start tmp-k-mtordini key >= tmp-k-valutare
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-k-mtordini next 
                         at end exit perform 
                    end-read
                    move k-mto-chiave to mto-chiave
                    read mtordini no lock 
                         invalid display message "KK"
                    end-read
                    move mto-cod-cli     to como-prm-cliente
                    move mto-prg-destino to como-prm-destino
                    perform TROVA-PARAMETRO
                    compute como-data = 
                            function integer-of-date(mto-data-note1)
                    subtract prm-gg-parziale from como-data
                    compute como-data = 
                            function date-of-integer(como-data)
                    |14.12.2011: In evasione immediata non
                    |considerare il flag di prenotazione qta
                    if mto-prenotazione-qta-si and tipo-evasione = 2
                       set mto-prenotazione-qta-no to true
                    end-if
                    if mto-prenotazione-qta-si
                       set k-mto-evadibile-no to true
                    else   
                       if chk-tutto-buf = 1 and
                          prm-escludi-tutto-no
                          set k-mto-evadibile-si to true
                       else
                          if data-oggi >= como-data
                             set k-mto-evadibile-si to true
                          else
                             set k-mto-evadibile-no to true
                          end-if
                       end-if
                    end-if
                    rewrite k-mto-rec invalid continue end-rewrite
                    move low-value to k-mto-rec
                    set k-mto-evadibile-forse to true
                    start tmp-k-mtordini key >= tmp-k-valutare
                          invalid exit perform
                    end-start
                 end-perform
           end-start.

      ***---
       OPERAZIONI-BLISTER.
           initialize tab-blister-peso-utf.
           |IMBALLI EVADIBILI BLISTER
           move low-value to k-mto-rec.
           set k-mto-evadibile-si to true.
           set k-mto-blister-si   to true.
           start tmp-k-mtordini key >= tmp-k-valutare-blister
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-k-mtordini next 
                         at end exit perform 
                    end-read
                    if k-mto-evadibile-forse or k-mto-blister-no
                       exit perform 
                    end-if
                    move low-value    to mro-rec
                    move k-mto-chiave to mro-chiave-testa
                    start mrordini key >= mro-chiave
                          invalid continue
                      not invalid
                          set ValutaBlister to false
                          move 0 to idx tot-idx num-componenti
                          perform until 1 = 2
                             read mrordini next 
                                  at end exit perform 
                             end-read
                             if mro-chiave-testa not = k-mto-chiave
                                exit perform
                             end-if
                             if mro-si-blister and 
                                mro-evadi-dal <= data-oggi
                                move mro-chiave to k-mro-chiave
                                read tmp-k-mrordini no lock
                                     invalid set ValutaBlister to true
                                 not invalid
                                     if k-mro-blister-testa
                                        if ValutaBlister
                                           perform CONTROLLA-BLISTER
                                        end-if
                                        move mro-chiave  to k-mro-chiave
                                        read tmp-k-mrordini no lock
                                        set ValutaBlister   to true
                                        move 0 to idx num-componenti
                                        initialize tab-blister
                                        move mro-bli-codice 
                                          to bli-codice
                                        read blister no lock
                                             invalid continue
                                         not invalid
                                             perform varying idx 
                                                        from 1 by 1 
                                                       until idx > 50
                                                if bli-el-articolo(idx) 
                                                   not = 0
                                                add 1 to num-componenti
                                                end-if
                                             end-perform
                                        end-read  
                                        move 0 to idx
                                     else
                                        set ValutaBlister to true
                                     end-if
                                     add 1 to idx
                                     move k-mro-chiave    
                                       to el-bli-chiave-master(idx)
                                     move mro-bli-qta     
                                       to el-bli-qta-imb(idx)
                                     move k-mro-evadibile 
                                       to el-bli-evadibile(idx)
                                     move k-mro-ordinata  
                                       to el-bli-ordinata(idx)
                                     if el-bli-evadibile(idx) = 0
                                        move 0 
                                          to el-bli-imb-evadibile(idx)
                                     else
                                        compute 
                                             el-bli-imb-evadibile(idx) = 
                                             el-bli-evadibile(idx) / 
                                             el-bli-qta-imb(idx)
                                     end-if
                                end-read
                             else
                                if ValutaBlister
                                   perform CONTROLLA-BLISTER
                                end-if
                             end-if
                          end-perform
                          if ValutaBlister
                             perform CONTROLLA-BLISTER
                          end-if
                     end-start
                  end-perform
           end-start.

      ***---
       CONTROLLA-BLISTER.
           move idx to tot-idx.
           set ValutaBlister to false.
           |1. controllo che tutti i componenti abbiano un evadibilita minima
           if idx not = num-componenti
              perform BLISTER-NON-EVADIBILE
           else
              |2. Assegno la minor evadibilita possibile a tutti i componenti
              move 99999999 to min-imb-evadibile
              perform varying idx from 1 by 1
                        until idx > tot-idx
                 if el-bli-imb-evadibile(idx) < min-imb-evadibile
                    move el-bli-imb-evadibile(idx) to min-imb-evadibile
                 end-if
              end-perform
      *****        |3. Controllo che la minor evadibilita copra 
      *****        |almeno un imballo (tutti i pezzi) per tutti
      *****        perform varying idx from 1 by 1
      *****                  until idx > tot-idx
      *****          compute min-qta-evadibile =
      *****                  min-imb-evadibile * el-bli-qta-imb(idx)
      *****           if min-qta-evadibile < el-bli-evadibile(idx)
      *****              perform BLISTER-NON-EVADIBILE
      *****              move 0 to min-imb-evadibile
      *****              exit perform
      *****           end-if
      *****        end-perform
              |4. Assegno la minor evadibilita a tutti se sono tutti evadibili
              if min-imb-evadibile = 0
                 perform BLISTER-NON-EVADIBILE
                 move 0 to min-imb-evadibile
              else
                 add  1 to blister-id
                 move 0 to tot-utf tot-peso
                 perform varying idx from 1 by 1 
                           until idx > tot-idx 
                    move el-bli-chiave-master(idx) to k-mro-chiave
                    read tmp-k-mrordini no lock
                    compute k-mro-evadibile-ok =
                            min-imb-evadibile * el-bli-qta-imb(idx)
                    move k-mro-evadibile-ok to k-mro-evadibile
                    move blister-id to k-mro-blister-id
                    rewrite k-mro-rec
                    move k-mro-articolo to art-codice
                    read articoli no lock
                         invalid display message "AA"
                                           title tit-err
                                            icon 2
                     not invalid
                         if art-si-utf
                            compute tot-utf =
                                    tot-utf + 
                                    k-mro-evadibile-ok * k-mro-prg-peso
                         end-if
                         compute tot-peso =
                                 tot-peso + 
                                 k-mro-evadibile-ok * k-mro-prg-peso
                    end-read
                 end-perform
                 move tot-utf  to el-peso-utf-blister-ok(blister-id)
                 move tot-peso to el-peso-peso-blister-ok(blister-id)
              end-if
           end-if.
           move 0 to idx tot-idx num-componenti.

      ***---
       BLISTER-NON-EVADIBILE.
           perform varying idx from 1 by 1 
                     until idx > tot-idx 
              move el-bli-chiave-master(idx) to k-mro-chiave
              read tmp-k-mrordini no lock
              move 0 to k-mro-evadibile-ok
              rewrite k-mro-rec
           end-perform.

      ***---
       CICLO-BANCO.
           |I PROMO CON RIGHE BANCO
           perform until 1 = 2
              move 0 to righe-ok
              move low-value to k-mto-rec
              set k-mto-evadibile-si to true
              start tmp-k-mtordini key >= tmp-k-valutare
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tmp-k-mtordini next 
                            at end exit perform 
                       end-read
                       perform LOOP-RIGHE
                    end-perform
              end-start
              if righe-ok = 0
                 exit perform
              end-if
           end-perform.

           |GLI ORDINI BANCO  
           perform until 1 = 2
              move 0 to righe-ok
              move low-value to k-mto-rec
              start tmp-k-mtordini key >= tmp-k-promo
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tmp-k-mtordini next 
                            at end exit perform 
                       end-read
                       if k-mto-promo-si
                          exit perform
                       end-if
                       perform LOOP-RIGHE
                    end-perform
              end-start
              if righe-ok = 0
                 exit perform
              end-if
           end-perform.

      ***---
       LOOP-RIGHE.
           move low-value    to k-mro-rec.
           move k-mto-chiave to k-mro-chiave-testa.
           |CICLO PROMO 2: ASSEGNO QTA GIACENZA UTILE (RIMANENZA)
           start tmp-k-mrordini key >= k-mro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2

                    perform COUNTER-VIDEO

                    read tmp-k-mrordini next 
                         at end exit perform 
                    end-read
                    if k-mro-chiave-testa not = k-mto-chiave
                       exit perform
                    end-if
                    if k-mro-promo = 0
                       if k-mro-ordinata > 
                        ( k-mro-evasa + k-mro-evadibile ) and
                          k-mro-evadi-dal <= data-oggi

                          move k-mro-articolo to art-codice
                          read articoli no lock
                          move art-scorta     to sco-codice
                          read tscorte no lock
                          if sco-forzata-si
                             move k-mro-ordinata  to k-mro-evadibile
                             move k-mro-evadibile to k-mro-evadibile-ok
                             rewrite k-mro-rec
                          else
                             move k-mro-articolo to tqe-articolo
                             read tmp-qta-eva no lock
                                  invalid continue
                              not invalid
                                  compute da-evadere = 
                                          k-mro-ordinata - k-mro-evasa
                                  if tqe-giac-utile >= da-evadere
                                     divide da-evadere by k-mro-qta-imb
                                            giving como-giacenza
                                         remainder resto
                                     compute como-giacenza = 
                                             como-giacenza * 
                                             k-mro-qta-imb
                                     add como-giacenza 
                                      to k-mro-evadibile
                                     subtract da-evadere 
                                         from tqe-giac-utile
                                     move k-mro-evadibile 
                                       to k-mro-evadibile-ok
                                     rewrite k-mro-rec
                                     rewrite tqe-rec
                                  else
                                     if tqe-giac-utile >= k-mro-qta-imb
                                        divide tqe-giac-utile 
                                            by k-mro-qta-imb
                                               giving como-giacenza
                                            remainder resto
                                        compute como-giacenza = 
                                                como-giacenza * 
                                                k-mro-qta-imb
                                        add como-giacenza 
                                         to k-mro-evadibile
                                        subtract como-giacenza 
                                            from tqe-giac-utile
                                        move k-mro-evadibile 
                                          to k-mro-evadibile-ok
                                        rewrite k-mro-rec
                                        rewrite tqe-rec
                                     end-if
                                  end-if
                             end-read
                          end-if
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       CICLO-EVADIBILITA-BANCO.
           |VALUTO QUALI MASTER SOLO BANCO SONO EVADIBILI
           move low-value to k-mto-rec
           start tmp-k-mtordini key >= tmp-k-promo
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-k-mtordini next 
                         at end exit perform 
                    end-read
                    if k-mto-promo-si
                       exit perform
                    end-if
                    move 0 to k-mto-da-evadere
                    move 0 to k-mto-evasa
                    move low-value    to k-mro-rec
                    move k-mto-chiave to k-mro-chiave-testa
                    start tmp-k-mrordini key >= k-mro-chiave
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read tmp-k-mrordini next 
                                  at end exit perform 
                             end-read
                             if k-mro-chiave-testa not = k-mto-chiave
                                exit perform
                             end-if

                             compute k-mto-da-evadere = 
                                     k-mto-da-evadere + 
                                   ( k-mro-ordinata   - k-mro-evasa )
                             compute k-mto-evasa      = 
                                     k-mto-evasa      + k-mro-evadibile

                          end-perform
                    end-start
                    if k-mto-evasa >= k-mto-da-evadere
                       set k-mto-evadibile-si    to true
                    else
                       move k-mto-chiave to mto-chiave
                       read mtordini no lock 
                            invalid display message "GG"
                       end-read
                       |14.12.2011: In evasione immediata non
                       |considerare il flag di prenotazione qta
                       if mto-prenotazione-qta-si and tipo-evasione = 2
                          set mto-prenotazione-qta-no to true
                       end-if
                       if mto-prenotazione-qta-si
                          set k-mto-evadibile-no to true
                       else
                          set k-mto-evadibile-si to true
                       end-if
                    end-if
                    rewrite k-mto-rec invalid continue end-rewrite
                 end-perform
           end-start.
