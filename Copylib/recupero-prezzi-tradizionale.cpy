      * UTILIZZATO SOLAMENTE DAI MASTER!!!!
      ***---
       RECUPERO-PREZZI-TRADIZIONALE.
           move ef-cau-buf to tca-codice.
           read tcaumag no lock.
           if tca-si-zero
              exit paragraph 
           end-if.
           |SOLO PER AVERE IL TOTALE DEI COLLI
           move 0 to tot-colli.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying riga from 2 by 1
                     until riga > tot-righe
              inquire form1-gd-1(riga, 1), hidden-data in gruppo-hidden
              inquire form1-gd-1(riga, 4),   cell-data in ror-qta

              if hid-no-saldo
BLISTR           if hid-blister = 0
                    compute como-qta = ror-qta / hid-imballi
BLISTR           else
BLISTR              move hid-imballi to como-qta
BLISTR           end-if
                 add como-qta to tot-colli
              end-if
           end-perform.

           accept wstampa    from environment "PATH_RECUPERO".
           accept como-data  from century-date.
           accept como-ora   from time.
           inspect wstampa   replacing trailing spaces by low-value.
           string  wstampa   delimited low-value
                   como-data delimited size
                   "_"       delimited size
                   como-ora  delimited size
                   ".txt"    delimited size
                   into wstampa
           end-string.
           inspect wstampa   replacing trailing low-value by spaces.
           open output lineseq.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              initialize r-rec replacing numeric data by zeroes
                                    alphanumeric data by spaces
              inquire form1-gd-1(riga, 1), hidden-data in gruppo-hidden
              inquire form1-gd-1(riga, 4),   cell-data in ror-qta

              move HiddenKey        to prg-chiave
BLISTR        if hid-blister = 0
                 compute como-qta = ror-qta / hid-imballi
                 move prg-cod-articolo to r-blister
BLISTR        else
BLISTR           move hid-imballi      to como-qta
                 move hid-bli-codice   to r-blister
                 move hid-bli-perce    to r-perce-bli
BLISTR        end-if
              inspect r-blister replacing leading x"30" by x"20"

              move prg-cod-articolo to r-articolo
              inspect r-articolo replacing leading x"30" by x"20"
              move prg-peso to r-peso
              inspect r-peso replacing leading x"30" by x"20"
              move como-qta      to r-colli
              inquire ef-cli, value in tor-cod-cli
              move tor-cod-cli   to r-cod-cli
              inquire ef-des, value in tor-prg-destino
              move tor-prg-destino  to r-destino
              inspect r-cod-cli  replacing leading x"30" by x"20"
              inspect r-destino  replacing leading x"30" by x"20"
              move "X" to r-filler
              move tor-data-ordine  to r-data
              if chk-ritira-buf = 1
                 move "S" to r-ritiro
              else
                 move "N" to r-ritiro
              end-if
              move tot-colli to r-tot-colli

              move prg-cod-articolo to art-codice
              read articoli no lock invalid continue end-read
              read progmag  no lock invalid continue end-read

              if art-si-cobat
                 move 0 to col-add ef-add-buf add-piombo
                 move 0 to col-cou ef-cou-buf imposta-cou
                 perform SCAGLIONI-COBAT
                 move imposta-cobat to r-i-cou-cobat

                 move tor-data-ordine    to tpb-data
                 move art-marca-prodotto to tpb-marca
                 start tpiombo key <= tpb-chiave
                       invalid continue
                   not invalid
                       read tpiombo previous
                       if tpb-marca = art-marca-prodotto and
                          tpb-data <= tor-data-ordine
                          if art-auto-cobat
                             move tpb-perce-auto to r-perce-pb
                          else
                             move tpb-perce-moto to r-perce-pb
                          end-if
                       end-if
                 end-start
              else
                 |NON DEVO FARE IL TEST SU MARCA!!!
                 move art-codice to ef-art-buf
                 display ef-art
                 perform CALCOLO-IMPOSTE-GDO
                 move 0 to ef-art-buf
                 display ef-art
                 move ror-imp-consumo   to r-i-consumo
                 move ror-imp-cou-cobat to r-i-cou-cobat
              end-if
              |delimitatore fine riga
              move "X" to r-prz-promo
              
              move r-rec     to line-riga of lineseq
              write line-riga             of lineseq
           end-perform.
           |DEMARCATORE EOF RICHIESTO DA WALTER
           move "XXX" to line-riga of lineseq.
           write line-riga         of lineseq.
           close lineseq.

           initialize comando.
           accept  comando from environment "RECUPERO_EXE".
           inspect comando replacing trailing spaces by low-value.
           string  comando delimited low-value
                   " "     delimited size
                   wstampa delimited size
                   into comando
           end-string.

           move 0 to status-call.
           call "C$SYSTEM" using comando, 32
                          giving status-call.

           if status-call = 0
              perform APPLICA-PREZZI
           else
              display message "Chiamata non risucita!"
                       x"0d0a""Eseguire nuovamente il salvataggio!"
                       title tit-err
                        icon 2
           end-if.
        
      ***---
       APPLICA-PREZZI.
           move 2 to riga.
           open input lineseq.
           perform until 1 = 2
              read lineseq next at end exit perform end-read
              |DEMARCATORE EOF RICHIESTO DA WALTER
              if line-riga of lineseq = "XXX"
                 exit perform
              end-if

              inquire form1-gd-1(riga, 1), hidden-data in gruppo-hidden
              if hid-no-saldo
                 move line-riga   of lineseq to r-rec

                 inspect r-cod-cli     replacing leading x"20" by x"30"
                 inspect r-destino     replacing leading x"20" by x"30"
                 inspect r-blister     replacing leading x"20" by x"30"
                 inspect r-articolo    replacing leading x"20" by x"30"
                 inspect r-peso        replacing leading x"20" by x"30"
                 inspect r-colli       replacing leading x"20" by x"30"
                 inspect r-tot-colli   replacing leading x"20" by x"30"
                 inspect r-i-consumo   replacing leading x"20" by x"30"
                 inspect r-i-cou-cobat replacing leading x"20" by x"30"
                 inspect r-perce-pb    replacing leading x"20" by x"30"
                 inspect r-perce-bli   replacing leading x"20" by x"30"
                 inspect r-prezzo      replacing leading x"20" by x"30"

                 move r-prezzo    to ror-prz-unitario 
                 move r-prezzo    to mro-prz-unitario 
                 move r-prezzo    to col-uni          
                 move r-articolo  to art-codice   
                 move HiddenKey   to prg-chiave
                 read progmag  no lock invalid continue end-read
                 read articoli no lock invalid continue end-read
                 move art-marca-prodotto to mar-codice
                 read tmarche no lock invalid continue end-read
                 move r-prz-promo  to hid-prz-promo convert
                 move art-codice   to ef-art-buf
                 display ef-art
                 set NewRow to false
                 if ror-prz-unitario = 0
                    move tge-cod-iva-omag to col-iva
                    move 0 to col-uni col-cons col-cou col-add col-imp
                    move "S" to hid-omaggio
                    modify form1-gd-1(riga, 78-NumColMan),
                    bitmap        = conferma-bmp
                    bitmap-number = 1
                    bitmap-width  = 19
                 else
                    set  cli-tipo-C  to true
                    move tor-cod-cli to cli-codice
                    read clienti no lock invalid continue end-read
                    move cli-tipo to tcl-codice
                    read ttipocli no lock invalid continue end-read
                    perform CALCOLA-IMPOSTE-ORDINE
                    
                    if tcl-si-piombo and art-si-cobat
                       perform CALCOLA-ADD-PIOMBO
                    end-if

                    perform CALCOLA-IMPONIBILE
                    if ttipocli-standard
                       move col-imp to col-uni
                    end-if

                    modify form1-gd-1(riga, 78-NumColMan),
                    bitmap        = conferma-bmp
                    bitmap-number = 2
                    bitmap-width  = 19
                    perform RECUPERA-IVA
                    move "N" to hid-omaggio
                 end-if
                 move 0 to ef-art-buf
                 display ef-art
                                        
                 move col-cons to como-cons
                 move col-cou  to como-cou
                 move col-add  to como-add
                 move col-imp  to como-imp
                 move col-qta  to como-qta
                 compute como-tot = ( como-cou   + 
                                      como-cons  + 
                                      como-add   + 
                                      como-imp ) * como-qta
                 move como-tot to col-tot

                 if pgm-name = "ordine"
                    modify form1-gd-1(riga, 5),  cell-data = col-uni
                    modify form1-gd-1(riga, 6),  cell-data = col-tot
                    modify form1-gd-1(riga, 7),  cell-data = col-cons
                    modify form1-gd-1(riga, 8),  cell-data = col-cou
                    modify form1-gd-1(riga, 9),  cell-data = col-add
                    modify form1-gd-1(riga, 10), cell-data = col-imp
                    modify form1-gd-1(riga, 11), cell-data = col-iva
                 else
                    modify form1-gd-1(riga, 6),  cell-data = col-uni
                    modify form1-gd-1(riga, 7),  cell-data = col-tot
                    modify form1-gd-1(riga, 8),  cell-data = col-cons
                    modify form1-gd-1(riga, 9),  cell-data = col-cou
                    modify form1-gd-1(riga, 10), cell-data = col-add
                    modify form1-gd-1(riga, 11), cell-data = col-imp
                    modify form1-gd-1(riga, 12), cell-data = col-iva
                 end-if
                 modify form1-gd-1(riga, 1),  hidden-data gruppo-hidden
              end-if
              add 1 to riga
           end-perform.
           close lineseq.
           delete file lineseq.
