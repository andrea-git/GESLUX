      ***---
       RIEMPI-TMP.
      *     perform SECCA-TMP.
      *     set tutto-ok to true.
      *     set trovato  to false.
      *     initialize path-tmp-progmag.
      *     accept  path-tmp-progmag from environment "PATH-ST".
      *     inspect path-tmp-progmag 
      *             replacing trailing spaces by low-value.
      *     accept como-data from century-date.
      *     accept como-ora  from time.
      *     string path-tmp-progmag   delimited by low-value
      *            "tmp-progmag"      delimited by size
      *            "_"                delimited by size
      *            como-data          delimited by size
      *            "_"                delimited by size
      *            como-ora           delimited by size
      *            ".tmp"             delimited by size
      *            into path-tmp-progmag
      *     end-string.
      *     open output tmp-progmag.
      *     set FileOpen to true.
      *     perform CICLO-LETTURA-PROGMAG.
      *     perform RIEMPI-GRIGLIA.

      ***---
       CICLO-LETTURA-PROGMAG.             
      *     move low-value to prg-chiave.
      *     if tipo-elab = 2
      *        move ef-cod-from-buf to prg-cod-articolo
      *     end-if.
      *     start progmag key is >= prg-chiave 
      *           invalid set errori to true
      *     end-start.
      *
      *     if tutto-ok
      *        perform until 1 = 2
      *           set record-ok to false
      *           read progmag next at end exit perform end-read
      *           move prg-cod-articolo to art-codice
      *           read articoli no lock invalid continue end-read
      *           if tipo-elab = 1
      *              if art-descrizione >= ef-des-from-buf and
      *                 art-descrizione <= ef-des-to-buf
      *                 set record-ok to true
      *              end-if
      *           else
      *              if prg-cod-articolo > ef-cod-to-buf
      *                 exit perform
      *              end-if
      *              set record-ok to true
      *           end-if
      *           if record-ok
      *              set trovato           to true
      *              move prg-tipo-imballo to imb-codice
      *              move spaces           to imb-descrizione
      *              read timballi no lock invalid continue end-read
      *              move 0 to num-imballi
      *              compute num-imballi = 
      *                    ( prg-giacenza / imb-qta-imb )
      *              perform MOVE-DATI
      *           end-if
      *        end-perform
      *     end-if.

      ***---
       MOVE-DATI.
      *     initialize tmp-prg-rec replacing numeric data by zeroes
      *                                 alphanumeric data by spaces.
      *
      *     move prg-cod-articolo  to tmp-prg-cod-articolo.
      *     move prg-cod-magazzino to tmp-prg-cod-magazzino.
      *     move prg-tipo-imballo  to tmp-prg-tipo-imballo.
      *     move prg-peso          to tmp-prg-peso.
      *     move art-descrizione   to tmp-prg-art-des.
      *     move imb-descrizione   to tmp-prg-imb-des.
      *     move prg-giacenza      to tmp-prg-giacenza.
      *     move prg-impegnato     to tmp-prg-impegnato.
      *     move prg-ordinato      to tmp-prg-ordinato.
      *     move num-imballi       to tmp-prg-imballi.
      *
      *     write tmp-prg-rec invalid continue end-write.

      ***---
       RIEMPI-GRIGLIA.
      *     close tmp-progmag.
      *     open input tmp-progmag.
      *     set tutto-ok to true.
      *     modify  gd-giacenze, mass-update = 1.
      *     modify  gd-giacenze, reset-grid  = 1.
      *     perform GD-GIACENZE-CONTENT.    
      *     move low-value to tmp-prg-rec.
      *     if tipo-elab = 1                
      *        start tmp-progmag key is >= key-des 
      *              invalid set errori to true
      *        end-start
      *     else                          
      *        start tmp-progmag key is >= tmp-prg-chiave
      *              invalid set errori to true
      *        end-start
      *     end-if.
      *     if tutto-ok
      *        move 1 to riga
      *        perform until 1 = 2
      *           read tmp-progmag next at end exit perform end-read
      *           if   tmp-prg-cod-magazzino = spaces and
      *                tmp-prg-tipo-imballo  = spaces and
      *                tmp-prg-peso          = 0
      *                perform RIEMPI-COLUMNS
      *
      *                set NoHasSons      to true
      *                modify gd-giacenze(riga, 1),
      *                       hidden-data = hid-HasSons
      *
      *                set hid-close to true
      *                modify gd-giacenze(riga, 2),
      *                       hidden-data = hid-OpenClose
      *
      *                set NoIsSon to true
      *                modify gd-giacenze(riga, 3),
      *                       hidden-data = hid-IsSon
      *
      *                modify gd-giacenze, (riga, 1),
      *                       bitmap        = multi-bmp,
      *                       bitmap-number = 1,
      *                       bitmap-width  = 16
      *           else
      *              if SaveArticolo = tmp-prg-cod-articolo
      *                 set YesHasSons to true                           
      *                 modify gd-giacenze(riga, 1), 
      *                        hidden-data = hid-HasSons
      *                 move 0 to SaveArticolo
      *                 modify gd-giacenze, (riga, 1),
      *                        bitmap        = multi-bmp,
      *                        bitmap-number = 3,
      *                        bitmap-width  = 16
      *              end-if
      *           end-if
      *        end-perform
      *     end-if.
      *     modify  gd-giacenze, mass-update = 0.

      ***---
       VEDI-MASTER.
      *     inquire gd-giacenze, last-row in totale-righe.
      *     perform varying riga from riga by -1 
      *               until riga < 2
      *        inquire gd-giacenze(riga, 3), hidden-data in hid-IsSon
      *        if NoIsSon exit perform end-if
      *     end-perform.
      *     move riga to como-riga.
      *     add 1 to riga.
      *
      *     perform varying riga from riga by 1 
      *               until riga > totale-righe 
      *        inquire gd-giacenze(riga, 3), hidden-data in hid-IsSon
      *        if NoIsSon exit perform end-if
      *
      *        modify gd-giacenze, record-to-delete = riga
      *        subtract 1 from riga
      *
      *     end-perform.

      ***---
       VEDI-SLAVES.                      
      *     move low-value to tmp-prg-chiave.
      *     move col-art   to tmp-prg-cod-articolo.
      *                                   
      *     if tipo-elab = 1
      *        move col-des to tmp-prg-art-des
      *        start tmp-progmag key is >= Key-Des
      *              invalid set errori to true
      *        end-start
      *     else            
      *        start tmp-progmag key is >= tmp-prg-chiave
      *              invalid set errori to true
      *        end-start
      *     end-if.
      *
      *     if tutto-ok
      *        move col-art to SaveArticolo
      *        perform until 1 = 2
      *           read tmp-progmag next at end exit perform end-read
      *           if   tmp-prg-cod-articolo not = SaveArticolo
      *                exit perform
      *           end-if
      *           if tmp-prg-cod-magazzino not = spaces and
      *              tmp-prg-tipo-imballo  not = spaces and
      *              tmp-prg-peso          not = 0
      *              perform RIEMPI-COLUMNS 
      *
      *              set NoHasSons      to true
      *              modify gd-giacenze(riga, 1),
      *                     hidden-data = hid-HasSons
      *
      *              set hid-open to true
      *              modify gd-giacenze(riga, 2),
      *                     hidden-data = hid-OpenClose
      *
      *              set YesIsSon to true
      *              modify gd-giacenze(riga, 3),
      *                     hidden-data = hid-IsSon
      *
      *              modify gd-giacenze, (riga, 2),
      *                     bitmap        = multi-bmp,
      *                     bitmap-number = 4,
      *                     bitmap-width  = 16
      *           end-if
      *        end-perform
      *     end-if.

      ***---
       RIEMPI-COLUMNS.
      *     add 1 to riga.
      *     initialize gd-giacenze-record 
      *                replacing numeric data by zeroes
      *                     alphanumeric data by spaces.
      *     move tmp-prg-cod-articolo  to col-art SaveArticolo.
      *     move tmp-prg-cod-magazzino to col-mag.
      *     move tmp-prg-peso          to col-peso.
      *     move tmp-prg-art-des       to col-des.
      *     move tmp-prg-imb-des       to col-imb-des.
      *     move tmp-prg-imballi       to col-imballi.
      *     move tmp-prg-giacenza      to col-giacenza.
      *     move tmp-prg-impegnato     to col-impegnato.
      *     move tmp-prg-ordinato      to col-ordinato.
      *     modify gd-giacenze(riga),
      *            insertion-index = riga
      *            record-to-add   = gd-giacenze-record.
      *
      *     if tmp-prg-giacenza  < 0
      *        modify gd-giacenze(riga,  9), cell-color = 176
      *     else                                            
      *        modify gd-giacenze(riga,  9), cell-color = 513
      *     end-if.
      *     if tmp-prg-impegnato < 0
      *        modify gd-giacenze(riga, 10), cell-color = 176
      *     else                                            
      *        modify gd-giacenze(riga, 10), cell-color = 513
      *     end-if.
      *     if tmp-prg-ordinato  < 0
      *        modify gd-giacenze(riga, 11), cell-color = 176
      *     else                                            
      *        modify gd-giacenze(riga, 11), cell-color = 513
      *     end-if.
