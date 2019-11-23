      **** USATO SOLO IN VGIACENZE

      ***---
       RIEMPI-TMP.
           move 0 to counter counter2.
           perform SECCA-TMP.
           set tutto-ok to true.
           set trovato  to false.
           initialize path-tmp-progmag.
           accept  path-tmp-progmag from environment "PATH-ST".
           inspect path-tmp-progmag 
                   replacing trailing spaces by low-value.
           accept como-data from century-date.
           accept como-ora  from time.
           string path-tmp-progmag   delimited by low-value
                  "tmp-progmag"      delimited by size
                  "_"                delimited by size
                  como-data          delimited by size
                  "_"                delimited by size
                  como-ora           delimited by size
                  ".tmp"             delimited by size
                  into path-tmp-progmag
           end-string.
           open output tmp-progmag.
           set FileOpen  to true.
           set ApriRighe to false.
LUBEXX     perform TROVA-MAGAZZINO-PRINCIPALE.
           perform CICLO-LETTURA-PROGMAG.
           perform RIEMPI-GRIGLIA.
           perform ELIMINA-RIGHE-SOLO-BASE.
           perform ESPLODI-RIGHE-UN-ELEMENTO.
           |23/05/2012
           if trovato and stampa-excel
              perform GENERA-FILE-EXCEL
           end-if.
           |23/05/2012


      ***---
LUBEXX TROVA-MAGAZZINO-PRINCIPALE.
           move spaces to MagPrincipale.
           move low-value to mag-rec.
           start tmagaz key >= mag-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmagaz next at end exit perform end-read
                    if si-mag-principale
                       move mag-codice to MagPrincipale
                       exit perform 
                    end-if
                 end-perform
           end-start.

      ***---
       CICLO-LETTURA-PROGMAG.             
           move low-value to prg-chiave.
           if tipo-elab = 2
              move ef-cod-from-buf to prg-cod-articolo
           end-if.
           start progmag key is >= prg-chiave 
                 invalid set errori to true
           end-start.

           if tutto-ok
              perform until 1 = 2
                 set record-ok to false
                 read progmag next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon form3-handle at column 22
                                              line 03
                    move 0 to counter2
                 end-if

LUBEXX*****                 if prg-attivo
                 if prg-giacenza      not = 0 or 
                    prg-ordinato-1    not = 0 or
                    prg-ordinato-2    not = 0 or
                    prg-ordinato-3    not = 0 or
                    prg-ordinato-4    not = 0 or
                    prg-ordinato-5    not = 0 or
                    prg-ordinato-6    not = 0 or
                    prg-impegnato     not = 0 or
                    prg-giacenza-bloc not = 0 |VALORE COMPLESSIVO
                    move prg-cod-articolo to art-codice
                    read articoli no lock invalid continue end-read
LUBEXX*****                    if art-attivo
                    if tipo-elab = 1
                       if art-descrizione >= ef-des-from-buf and
                          art-descrizione <= ef-des-to-buf
                          set record-ok to true
                       end-if
                    else
                       if prg-cod-articolo > ef-cod-to-buf
                          exit perform
                       end-if
                       set record-ok to true
                    end-if
LUBEXX*****                    end-if
                 end-if

                 if record-ok
                    if ef-marca-buf not = 0 and
                       ef-marca-buf not = art-marca-prodotto
                       set record-ok to false
                    end-if
                 end-if

                 if record-ok
                    if ef-settore-buf not = 0 and
                       ef-settore-buf not = art-settore-merceologico
                       set record-ok to false
                    end-if
                 end-if

                 if record-ok
                    if ef-dogana-buf not = 0 and
                       ef-dogana-buf not = art-cod-doganale
                       set record-ok to false
                    end-if
                 end-if

                 if record-ok
                    if ef-mag-buf        not = spaces and 
                       prg-cod-magazzino not = spaces
                       if ef-mag-buf not = prg-cod-magazzino
                          set record-ok to false
                       end-if
                    end-if
                 end-if

LUBEXX*****                 end-if

                 if record-ok
                    set trovato           to true
                    if prg-tipo-imballo not = spaces
                       move prg-tipo-imballo to imq-codice
                       read timbalqta no lock invalid continue end-read
                       move imq-tipo        to imb-codice
                       read timballi 
                            invalid move spaces to imb-descrizione
                       end-read

                       move 0 to num-imballi
                       compute num-imballi = 
                             ( prg-giacenza / imq-qta-imb )

                    end-if
                    perform MOVE-DATI
                 end-if
              end-perform
           end-if.

      ***---
       MOVE-DATI.
           initialize tmp-prg-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           move prg-cod-articolo  to tmp-prg-cod-articolo.
           move prg-cod-magazzino to tmp-prg-cod-magazzino.
           move prg-tipo-imballo  to tmp-prg-tipo-imballo.
           move prg-peso          to tmp-prg-peso.
           move art-descrizione   to tmp-prg-art-des.
           initialize tmp-prg-imb-des.
           if prg-tipo-imballo not = spaces
              inspect imb-descrizione replacing trailing spaces 
                                                      by low-value
              move imq-qta-imb    to qta-edit
              call "C$JUSTIFY" using qta-edit, "L"
              inspect qta-edit replacing trailing spaces by low-value
              string imb-descrizione delimited by low-value
                     " da "          delimited by size
                     qta-edit        delimited by low-value
                     " x "           delimited by size
                     art-udm-imballo delimited by size
                     into tmp-prg-imb-des
              end-string
              move num-imballi       to tmp-prg-imballi
           end-if.
           move prg-giacenza      to tmp-prg-giacenza.
           move prg-impegnato     to tmp-prg-impegnato.
      *****     move prg-ordinato-1    to tmp-prg-ordinato-1.
      *****     move prg-ordinato-2    to tmp-prg-ordinato-2.
      *****     move prg-ordinato-3    to tmp-prg-ordinato-3.
      *****     move prg-ordinato-4    to tmp-prg-ordinato-4.
      *****     move prg-ordinato-5    to tmp-prg-ordinato-5.
           move prg-ordinato-6    to tmp-prg-ordinato-6.
           move prg-giacenza-bloc to tmp-prg-giac-bloc.

           write tmp-prg-rec invalid continue end-write.

      ***---
       RIEMPI-GRIGLIA.
           close      tmp-progmag.
           open input tmp-progmag.
           set tutto-ok to true.
           modify  gd-giacenze, mass-update = 1.
           modify  gd-giacenze, reset-grid  = 1.
           perform GD-GIACENZE-CONTENT.
           move low-value to tmp-prg-rec.
      *****     if tipo-elab = 1
              start tmp-progmag key is >= key-des
                    invalid set errori to true
              end-start
      *****     else                          
      *****        start tmp-progmag key is >= tmp-prg-chiave
      *****              invalid set errori to true
      *****        end-start
      *****     end-if.
           if tutto-ok
              move 2 to riga
              perform until 1 = 2
                 read tmp-progmag next at end exit perform end-read
                 if   tmp-prg-cod-magazzino = spaces and
                      tmp-prg-tipo-imballo  = spaces and
                      tmp-prg-peso          = 0
                      perform RIEMPI-COLUMNS

                      set NoHasSons      to true
                      modify gd-giacenze(riga, 1),
                             hidden-data = hid-HasSons

                      set hid-close to true
                      modify gd-giacenze(riga, 2),
                             hidden-data = hid-OpenClose

                      set NoIsSon to true
                      modify gd-giacenze(riga, 3),
                             hidden-data = hid-IsSon

                      modify gd-giacenze, (riga, 1),
                             bitmap        = multi-bmp,
                             bitmap-number = 1,
                             bitmap-width  = 16
                 else
                    if SaveArticolo = tmp-prg-cod-articolo
                       set YesHasSons to true
                       modify gd-giacenze(riga, 1),
                              hidden-data = hid-HasSons
                       move 0 to SaveArticolo
                       modify gd-giacenze, (riga, 1),
                              bitmap        = multi-bmp,
                              bitmap-number = 3,
                              bitmap-width  = 16
                    end-if
                 end-if
              end-perform
           end-if.
           modify  gd-giacenze, mass-update = 0.

      ***---
       ELIMINA-RIGHE-SOLO-BASE.
           inquire gd-giacenze, last-row in tot-righe.
           perform varying riga from 3 by 1 until
                           riga > tot-righe
              move riga to como-riga
              inquire gd-giacenze(riga, 1), hidden-data in hid-HasSons
              if NoHasSons
                 modify  gd-giacenze, record-to-delete = riga
                 subtract 1 from tot-righe
                 subtract 1 from riga
              end-if
           end-perform.

      ***---
       ESPLODI-RIGHE-UN-ELEMENTO.
           inquire gd-giacenze, last-row in tot-righe.
           perform varying riga from 3 by 1 until
                           riga > tot-righe
LUBEXX        move 0 to diff giacenza-altri 
              move riga to como-riga
              inquire gd-giacenze(riga, 3), cell-data   in col-art
              inquire gd-giacenze(riga, 1), hidden-data in hid-HasSons
              inquire gd-giacenze(riga, 3), hidden-data in hid-IsSon
              inquire gd-giacenze(riga, 4), cell-data   in col-des
              if YesHasSons
                 perform CONTA-ELEMENTI
LUBEXX           if FigliInStessoMagazzino
LUBEXX              move SaveMagazzino to col-mag
LUBEXX              modify gd-giacenze(como-riga, 5), 
LUBEXX                     cell-data = col-mag
LUBEXX              move num-imballi   to col-imballi
LUBEXX              modify gd-giacenze(como-riga, 7), 
LUBEXX                     cell-data = col-imballi
LUBEXX           end-if

                 evaluate RowCounter
                 when 0 modify  gd-giacenze, record-to-delete = riga
                        inquire gd-giacenze, last-row in tot-righe
                 when 1 
                      modify gd-giacenze, (riga, 1),
                             bitmap        = multi-bmp,
                             bitmap-number = 2,
                             bitmap-width  = 16
                      set hid-open to true 
                      modify gd-giacenze(riga, 2), 
                             hidden-data = hid-OpenClose
                      perform VEDI-SLAVES
                      modify  gd-giacenze, record-to-delete = como-riga
                      inquire gd-giacenze, last-row in tot-righe
                      subtract 1 from riga 

                      set NoHasSons      to true
                      modify gd-giacenze(riga, 1),
                             hidden-data = hid-HasSons

                      set NoIsSon to true
                      modify gd-giacenze(riga, 3),
                             hidden-data = hid-IsSon

                      modify gd-giacenze, (riga, 2),
                             bitmap      = null

                 when other
                      move tot-giacenza   to col-giacenza
                      move tot-impegnato  to col-impegnato
      *****                move tot-ordinato-1 to col-ordinato-1
      *****                move tot-ordinato-2 to col-ordinato-2
      *****                move tot-ordinato-3 to col-ordinato-3
      *****                move tot-ordinato-4 to col-ordinato-4
      *****                move tot-ordinato-5 to col-ordinato-5
                      move tot-ordinato-6 to col-ordinato-6
                      move tot-giac-bloc  to col-giac-bloc

                      if tot-giacenza  < 0
                         modify gd-giacenze(riga,  9), 
                                cell-color = 176,
                                cell-data  = col-giacenza
                      else
                         modify gd-giacenze(riga,  9),
                                cell-color = 513,
                                cell-data  = col-giacenza
                      end-if

                      if tot-impegnato < 0
                         modify gd-giacenze(riga, 10),
                                cell-color = 176,
                                cell-data  = col-impegnato
                      else                                            
                         modify gd-giacenze(riga, 10),
                                cell-color = 513,
                                cell-data  = col-impegnato 
                      end-if

      *****                if tot-ordinato-1  < 0
      *****                   modify gd-giacenze(riga, 13),
      *****                          cell-color = 176,
      *****                          cell-data  = col-ordinato-1
      *****                else
      *****                   modify gd-giacenze(riga, 13),
      *****                          cell-color = 513,
      *****                          cell-data  = col-ordinato-1
      *****                end-if
      *****
      *****                if tot-ordinato-2  < 0
      *****                   modify gd-giacenze(riga, 14),
      *****                          cell-color = 176,
      *****                          cell-data  = col-ordinato-2
      *****                else
      *****                   modify gd-giacenze(riga, 14),
      *****                          cell-color = 513,
      *****                          cell-data  = col-ordinato-2
      *****                end-if
      *****
      *****                if tot-ordinato-3  < 0
      *****                   modify gd-giacenze(riga, 15),
      *****                          cell-color = 176,
      *****                          cell-data  = col-ordinato-3
      *****                else
      *****                   modify gd-giacenze(riga, 15),
      *****                          cell-color = 513,
      *****                          cell-data  = col-ordinato-3
      *****                end-if
      *****
      *****                if tot-ordinato-4  < 0
      *****                   modify gd-giacenze(riga, 16),
      *****                          cell-color = 176,
      *****                          cell-data  = col-ordinato-4
      *****                else
      *****                   modify gd-giacenze(riga, 16),
      *****                          cell-color = 513,
      *****                          cell-data  = col-ordinato-4
      *****                end-if
      *****
      *****                if tot-ordinato-5  < 0
      *****                   modify gd-giacenze(riga, 17),
      *****                          cell-color = 176,
      *****                          cell-data  = col-ordinato-5
      *****                else
      *****                   modify gd-giacenze(riga, 17),
      *****                          cell-color = 513,
      *****                          cell-data  = col-ordinato-5
      *****                end-if                               

                      if tot-ordinato-6  < 0
                         modify gd-giacenze(riga, 13),
                                cell-color = 176,
                                cell-data  = col-ordinato-6
                      else
                         modify gd-giacenze(riga, 13),
                                cell-color = 513,
                                cell-data  = col-ordinato-6
                      end-if

                      if tot-giac-bloc < 0
                         modify gd-giacenze(riga,  14), 
                                cell-color = 176,
                                cell-data  = col-giac-bloc
                      else
                         modify gd-giacenze(riga,  14),
                                cell-color = 513,
                                cell-data  = col-giac-bloc
                      end-if

LUBEXX                if diff > 0 and giacenza-altri > 0
LUBEXX                   modify gd-giacenze(riga, 9), cell-color = 481
LUBEXX                end-if

                 end-evaluate
              end-if
           end-perform.

      ***---
       CONTA-ELEMENTI.
           move         0 to tot-giacenza.
           move         0 to tot-impegnato.
           move         0 to tot-rim.
           move         0 to tot-gia.
      ****     move         0 to tot-ordinato-1.
      ****     move         0 to tot-ordinato-2.
      ****     move         0 to tot-ordinato-3.
      ****     move         0 to tot-ordinato-4.
      ****     move         0 to tot-ordinato-5.
           move         0 to tot-ordinato-6.
           move         0 to tot-giac-bloc.
           move         0 to RowCounter.
           move low-value to tmp-prg-chiave.
           move col-art   to tmp-prg-cod-articolo.
           move col-des   to tmp-prg-art-des.
           start tmp-progmag key is >= key-des
                 invalid set errori to true
           end-start.

           if tutto-ok
LUBEXX        move 0 to diff giacenza-altri
              move col-art to SaveArticolo
LUBEXX        move spaces  to SaveMagazzino
LUBEXX        move 0       to num-imballi
LUBEXX        set FigliInStessoMagazzino to true
              perform until 1 = 2
                 read tmp-progmag next at end exit perform end-read

LUBEXX           if tmp-prg-cod-magazzino not = spaces
LUBEXX              if tmp-prg-cod-magazzino = MagPrincipale
LUBEXX                 compute diff = diff + 
LUBEXX                        ( tmp-prg-impegnato - tmp-prg-giacenza )
LUBEXX              else
LUBEXX                 add tmp-prg-giacenza to giacenza-altri
LUBEXX              end-if
LUBEXX           end-if

LUBEXX           if tmp-prg-cod-articolo not = SaveArticolo
LUBEXX              exit perform
LUBEXX           end-if

LUBEXX           if SaveMagazzino = spaces
LUBEXX              move tmp-prg-cod-magazzino to SaveMagazzino
LUBEXX           end-if

LUBEXX           if SaveMagazzino = tmp-prg-cod-magazzino
LUBEXX              add tmp-prg-imballi to num-imballi
LUBEXX           else
LUBEXX              set FigliInStessoMagazzino to false
LUBEXX           end-if

                 if tmp-prg-cod-magazzino not = spaces and
                    tmp-prg-tipo-imballo  not = spaces and
                    tmp-prg-peso          not = 0
                    add 1 to RowCounter    
                    add tmp-prg-giacenza   to tot-giacenza
                    add tmp-prg-impegnato  to tot-impegnato
      *****              add tmp-prg-ordinato-1 to tot-ordinato-1
      *****              add tmp-prg-ordinato-2 to tot-ordinato-2
      *****              add tmp-prg-ordinato-3 to tot-ordinato-3
      *****              add tmp-prg-ordinato-4 to tot-ordinato-4
      *****              add tmp-prg-ordinato-5 to tot-ordinato-5
                    add tmp-prg-ordinato-6 to tot-ordinato-6
                    add tmp-prg-giac-bloc  to tot-giac-bloc
                 end-if
              end-perform
           end-if.

      ***---
       VEDI-MASTER.
           inquire gd-giacenze, last-row in tot-righe.
           perform varying riga from riga by -1 
                     until riga < 3
              inquire gd-giacenze(riga, 3), hidden-data in hid-IsSon
              if NoIsSon exit perform end-if
           end-perform.
           move riga to como-riga.
           add 1 to riga.

           perform varying riga from riga by 1 
                     until riga > tot-righe 
              inquire gd-giacenze(riga, 3), hidden-data in hid-IsSon
              if NoIsSon exit perform end-if

              modify gd-giacenze, record-to-delete = riga
              subtract 1 from riga

           end-perform.

      ***---
       VEDI-SLAVES.
           move low-value to tmp-prg-chiave.
           move col-art   to tmp-prg-cod-articolo.
                                         
      *****     if tipo-elab = 1
              move col-des to tmp-prg-art-des
              start tmp-progmag key is >= Key-Des
                    invalid set errori to true
              end-start
      *****     else            
      *****        start tmp-progmag key is >= tmp-prg-chiave
      *****              invalid set errori to true
      *****        end-start
      *****     end-if.

           if tutto-ok
              move col-art to SaveArticolo
              perform until 1 = 2
                 read tmp-progmag next at end exit perform end-read
                 if   tmp-prg-cod-articolo not = SaveArticolo
                      exit perform
                 end-if
                 if tmp-prg-cod-magazzino not = spaces and
                    tmp-prg-tipo-imballo  not = spaces and
                    tmp-prg-peso          not = 0
                    perform RIEMPI-COLUMNS 
                    |23/05/2012
                    if stampa-excel
                       add 1 to tot-righe
                    end-if
                    |23/05/2012
LUBEXX              if ApriRighe
LUBEXX                 modify gd-giacenze(riga, 3), cell-color = 481
LUBEXX                 modify gd-giacenze(riga, 4), cell-color = 481
LUBEXX                 modify gd-giacenze(riga, 5), cell-color = 481
LUBEXX                 modify gd-giacenze(riga, 6), cell-color = 481
LUBEXX                 modify gd-giacenze(riga, 7), cell-color = 481
LUBEXX                 modify gd-giacenze(riga, 8), cell-color = 481
LUBEXX              end-if

                    set NoHasSons      to true
                    modify gd-giacenze(riga, 1),
                           hidden-data = hid-HasSons

                    set hid-open to true
                    modify gd-giacenze(riga, 2),
                           hidden-data = hid-OpenClose

                    set YesIsSon to true
                    modify gd-giacenze(riga, 3),
                           hidden-data = hid-IsSon

                    modify gd-giacenze, (riga, 2),
                           bitmap        = multi-bmp,
                           bitmap-number = 4,
                           bitmap-width  = 16
                 end-if
              end-perform
           end-if.

      ***---
       RIEMPI-COLUMNS.
           add 1 to riga.
                  
           move 0 to tot-gia tot-rim.
           move low-value to pev-rec.
           move tmp-prg-cod-articolo to pev-articolo.
           start promoeva key >= pev-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read promoeva next at end exit perform end-read
                    if pev-articolo not = tmp-prg-cod-articolo
                       exit perform
                    end-if
                    if pev-impegnato <= pev-rpr-qta
                       compute tot-rim =
                               tot-rim + ( pev-rpr-qta - pev-impegnato)
                    end-if
      *****              add pev-rimanenza  to tot-rim
                    add pev-giac-utile to tot-gia
                 end-perform
           end-start.
           move tot-rim to col-rim.
           move tot-gia to col-gia.

           initialize gd-giacenze-record 
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.
           move tmp-prg-cod-articolo  to col-art SaveArticolo.
           move tmp-prg-cod-magazzino to col-mag.
           move tmp-prg-peso          to col-peso.
           move tmp-prg-art-des       to col-des.
           move tmp-prg-imb-des       to col-imb-des.
           move tmp-prg-imballi       to col-imballi.
           move tmp-prg-giacenza      to col-giacenza.
           move tmp-prg-impegnato     to col-impegnato.
      *****     move tmp-prg-ordinato-1    to col-ordinato-1.
      *****     move tmp-prg-ordinato-2    to col-ordinato-2.
      *****     move tmp-prg-ordinato-3    to col-ordinato-3.
      *****     move tmp-prg-ordinato-4    to col-ordinato-4.
      *****     move tmp-prg-ordinato-5    to col-ordinato-5.
           move tmp-prg-ordinato-6    to col-ordinato-6.
           move tmp-prg-giac-bloc     to col-giac-bloc.
           modify gd-giacenze(riga),
                  insertion-index = riga
                  record-to-add   = gd-giacenze-record.

           if tmp-prg-giacenza  < 0
              modify gd-giacenze(riga,  9), cell-color = 176
           else                                            
              modify gd-giacenze(riga,  9), cell-color = 513
           end-if.
           if tmp-prg-impegnato < 0
              modify gd-giacenze(riga, 10), cell-color = 176
           else                                            
              modify gd-giacenze(riga, 10), cell-color = 513
           end-if.
           if tot-rim < 0
              modify gd-giacenze(riga, 11), cell-color = 176
           else                                            
              modify gd-giacenze(riga, 11), cell-color = 513
           end-if.
           if tot-gia < 0
              modify gd-giacenze(riga, 12), cell-color = 176
           else                                            
              modify gd-giacenze(riga, 12), cell-color = 513
           end-if.
      *****     if tmp-prg-ordinato-1  < 0
      *****        modify gd-giacenze(riga, 13), cell-color = 176
      *****     else                                            
      *****        modify gd-giacenze(riga, 13), cell-color = 513
      *****     end-if.
      *****     if tmp-prg-ordinato-2  < 0
      *****        modify gd-giacenze(riga, 14), cell-color = 176
      *****     else                                            
      *****        modify gd-giacenze(riga, 14), cell-color = 513
      *****     end-if.
      *****     if tmp-prg-ordinato-3  < 0
      *****        modify gd-giacenze(riga, 15), cell-color = 176
      *****     else                                            
      *****        modify gd-giacenze(riga, 15), cell-color = 513
      *****     end-if.
      *****     if tmp-prg-ordinato-4  < 0
      *****        modify gd-giacenze(riga, 16), cell-color = 176
      *****     else                                            
      *****        modify gd-giacenze(riga, 16), cell-color = 513
      *****     end-if.
      *****     if tmp-prg-ordinato-5  < 0
      *****        modify gd-giacenze(riga, 17), cell-color = 176
      *****     else                                            
      *****        modify gd-giacenze(riga, 17), cell-color = 513
      *****     end-if.
           if tmp-prg-ordinato-6  < 0
              modify gd-giacenze(riga, 13), cell-color = 176
           else                                            
              modify gd-giacenze(riga, 13), cell-color = 513
           end-if.
           if tmp-prg-giac-bloc  < 0
              modify gd-giacenze(riga, 14), cell-color = 176
           else                                            
              modify gd-giacenze(riga, 14), cell-color = 513
           end-if.

      
      ***---|23/05/2012
       GENERA-FILE-EXCEL.
           perform ACCETTA-SEPARATORE.
           perform COMPONI-PATH-STAMPA.
           if tutto-ok
              perform APRI-TUTTE-RIGHE
              perform SCRIVI-RIGHE-EXCEL
           end-if.

      ***---
       COMPONI-PATH-STAMPA.
           move user-codi to como-user.
           initialize wstampa.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  wstampa   from environment "PATH_ST".
           inspect wstampa   replacing trailing spaces by low-value.
           inspect como-user replacing trailing spaces by low-value.
           string  wstampa       delimited low-value
                   "tmp-progmag" delimited size
                   "_"           delimited size
                   como-user     delimited low-value
                   "_"           delimited size
                   como-data     delimited size
                   "_"           delimited size
                   como-ora      delimited size
                   ".csv"        delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       APRI-TUTTE-RIGHE.
           inquire gd-giacenze, last-row in tot-righe.
           perform varying riga from 3 by 1 
                     until riga > tot-righe
              inquire gd-giacenze(riga, 3), cell-data in col-art
              inquire gd-giacenze(riga, 1), hidden-data in hid-HasSons
              inquire gd-giacenze(riga, 3), hidden-data in hid-IsSon  
              inquire gd-giacenze(riga, 4), cell-data   in col-des
              if YesHasSons
                 set ApriRighe to true
                 perform VEDI-SLAVES
              end-if
           end-perform.

      ***---
       SCRIVI-RIGHE-EXCEL.
           inquire gd-giacenze, last-row in tot-righe.
           perform varying riga from 3 by 1 
                     until riga > tot-righe
              if riga = 3
                 initialize line-riga
                 string "Articolo"    delimited size
                        separatore    delimited size
                        "Descrizione" delimited size
                        separatore    delimited size
                        "Mag."        delimited size
                        separatore    delimited size
                        "Imballo"     delimited size
                        separatore    delimited size
                        "N. Imballi"  delimited size
                        separatore    delimited size
                        "Peso"        delimited size
                        separatore    delimited size
                        "Giacenza"    delimited size
                        separatore    delimited size
                        "Impegnato"   delimited size
                        separatore    delimited size
                        "Prenotata"   delimited size
                        separatore    delimited size
                        "Ord-6"       delimited size
                        separatore    delimited size
                        "Bloccata"    delimited size
                        separatore    delimited size
                        "Valore"      delimited size 
                        into line-riga
                 end-string
                 write line-riga
              end-if
              inquire gd-giacenze(riga, 3),  cell-data col-art
              inquire gd-giacenze(riga, 4),  cell-data col-des
              inquire gd-giacenze(riga, 5),  cell-data col-mag
              inquire gd-giacenze(riga, 6),  cell-data col-imb-des
              inquire gd-giacenze(riga, 7),  cell-data col-imballi
              inquire gd-giacenze(riga, 8),  cell-data col-peso
              inquire gd-giacenze(riga, 9),  cell-data col-giacenza
              inquire gd-giacenze(riga, 10), cell-data col-impegnato
              inquire gd-giacenze(riga, 11), cell-data col-rim
              inquire gd-giacenze(riga, 12), cell-data col-gia
              inquire gd-giacenze(riga, 13), cell-data col-ordinato-6
              inquire gd-giacenze(riga, 14), cell-data col-giac-bloc
              
              move col-art    to art-codice           
              if col-mag not = spaces                           
                 move art-codice to prg-cod-articolo
                 move spaces     to prg-cod-magazzino
                 move spaces     to prg-tipo-imballo
                 move 0          to prg-peso
                 move 0 to como-giacenza
                 start progmag key >= prg-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read progmag next at end exit perform end-read
                          if prg-cod-articolo not = art-codice
                             exit perform 
                          end-if
                          if prg-cod-magazzino not = spaces
                             move prg-cod-magazzino to mag-codice
                             read tmagaz no lock
                             if mag-per-promo-si
                                add prg-giacenza to como-giacenza
                             end-if
                          end-if
                       end-perform                                     
                 end-start
               
                 move art-codice to prg-cod-articolo
                 move spaces     to prg-cod-magazzino
                 move spaces     to prg-tipo-imballo
                 move 0          to prg-peso
                 read progmag no lock
                      invalid continue
                 end-read
 
                 perform CALCOLA-COSTO-MP
                 add 0,005 to costo-mp giving como-costo-mp

                 compute como-valore = costo-mp * 
                       ( como-giacenza - prg-impegnato )

                 move como-valore to col-valore
              end-if

              initialize line-riga
              string col-art        delimited size
                     separatore     delimited size
                     col-des        delimited size
                     separatore     delimited size
                     col-mag        delimited size
                     separatore     delimited size
                     col-imb-des    delimited size
                     separatore     delimited size
                     col-imballi    delimited size
                     separatore     delimited size
                     col-peso       delimited size
                     separatore     delimited size
                     col-giacenza   delimited size
                     separatore     delimited size
                     col-impegnato  delimited size
                     separatore     delimited size
                     col-rim        delimited size
                     separatore     delimited size
                     col-ordinato-6 delimited size
                     separatore     delimited size
                     col-giac-bloc  delimited size
                     separatore     delimited size
                     col-valore     delimited size
                     into line-riga
              end-string
              write line-riga
           end-perform.
           close lineseq.

           perform CALL-EXCEL.
