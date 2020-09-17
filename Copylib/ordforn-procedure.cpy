      ***---
       INTESTAZIONE.
           perform FORM1-GD-1-CONTENT.

      ***---
       VALORIZZA-RIGA-ARTICOLO.
           if art-attivo |or art-disattivo
              move art-codice to SaveArticolo
              move 0 to num-articoli

              if SaveArticolo not = 0
                 if CheckAfterZoom
                    perform FIND-PROGMAG
                    perform READ-TMARCHE
                    perform VALORIZZA-CELLE-CAMPI-MANUALE
                 else
                    perform FIND-MORE-ARTICOLI-ON-PROGMAG
                    if num-articoli = 0
                       set errori to true
                    end-if
                 end-if
              else
                 set errori to true
              end-if
           else
              set errori to true
           end-if.

      ***---
       FIND-PROGMAG.
      *****     move 0 to sum      SumKey.
      *****     move 0 to giacenza GiacenzaKey.
           move 0 to imq-qta-imb |hid-imballi.
           
           initialize HiddenKey replacing numeric data by zeroes
                                     alphanumeric data by spaces.
      *****     move art-codice     to prg-cod-articolo.
      *****     move storemagazzino to prg-cod-magazzino.
      *****     move low-value      to prg-tipo-imballo prg-peso.
      *****     start progmag key is >= prg-chiave invalid continue 
      *****     end-start.

      *****     if status-progmag = "00"
      *****
      *****        perform until 1 = 2
      *****           read progmag next no lock at end exit perform end-read
      *****           if prg-cod-articolo  not = art-codice or
      *****              prg-cod-magazzino not = StoreMagazzino
      *****              exit perform
      *****           end-if
      *****           if prg-attivo
      *****              if prg-giacenza > 0 and 
      *****                 prg-giacenza > giacenza
      *****                 move prg-giacenza to giacenza
      *****                 move prg-chiave   to GiacenzaKey
      *****              end-if
      *****              if prg-ini-udm + prg-acq-udm + prg-ven-udm > 0 and
      *****                 prg-ini-udm + prg-acq-udm + prg-ven-udm > sum
      *****                 compute sum = ( prg-ini-udm + 
      *****                                 prg-acq-udm + 
      *****                                 prg-ven-udm )
      *****                 move prg-chiave to SumKey
      *****              end-if
      *****           end-if
      *****        end-perform
      *****
      *****        if giacenza > 0
      *****           move GiacenzaKey to HiddenKey
      *****        else
      *****           if sum > 0
      *****              move SumKey to HiddenKey
      *****           else
      *****              move art-codice           to prg-cod-articolo
      *****              move StoreMagazzino       to prg-cod-magazzino
      *****              add art-peso-utf          to art-peso-non-utf 
      *****                                    giving prg-peso
      *****              move art-imballo-standard to prg-tipo-imballo
      *****              read progmag no lock invalid continue end-read
      *****              move prg-chiave to HiddenKey
      *****              if prg-bloccato or prg-disattivo
      *****                 move spaces  to hid-tipo-imballo
      *****              end-if
      *****           end-if
      *****        end-if
      *****
      *****     end-if.

           move prg-chiave to HiddenKey.
           if prg-bloccato
              move spaces  to hid-tipo-imballo
           end-if.

           if hid-tipo-imballo not = spaces
              move hid-tipo-imballo     to imq-codice
           else
              move art-imballo-standard to imq-codice
           end-if.

           read timbalqta no lock invalid continue end-read.
           move imq-qta-imb     to imballi-ed |hid-imballi.
           move imq-tipo        to imb-codice.
           read timballi no lock 
                invalid  initialize imb-descrizione
           end-read.               
           inspect imb-descrizione replacing trailing spaces 
                                                   by low-value.
           |move imb-descrizione to hid-des-imballo.

      *****     if pgm-name = "gordcvar"
      *****        move ror-qta-imballi to hid-imballi
      *****        move ror-des-imballo to hid-des-imballo
      *****     end-if.

           perform SOMMA-DINAMICI.

           perform LABEL-VALORI.

           move HiddenKey to prg-chiave.
           read progmag no lock.

      *    luciano 
           |move hid-imballi  to ef-qta-buf.

      ***---
       SOMMA-DINAMICI.
           move art-codice     to prg-cod-articolo.
           move art-codice     to prg-cod-articolo.
           move StoreMagazzino to prg-cod-magazzino.
           move low-value      to prg-tipo-imballo prg-peso.
           start progmag key >= prg-chiave 
                 invalid continue 
             not invalid
                 move 0 to hid-giacenza hid-impegnato hid-ordinato
                 perform until 1 = 2
                    read progmag next no lock 
                         at end exit perform 
                    end-read
                    if prg-cod-articolo  not = art-codice or
                       prg-cod-magazzino not = StoreMagazzino
                       exit perform
                    end-if
                    add prg-giacenza   to hid-giacenza
                    add prg-impegnato  to hid-impegnato
                    add prg-ordinato-1 to hid-ordinato
                 end-perform
           end-start.

      ***---
       VALORIZZA-CELLE-CAMPI-MANUALE.
           inquire form1-gd-1, last-row in tot-righe.
           move art-codice     to col-art.
           initialize lab-art-buf.
           move art-descrizione to lab-art-buf

           move lab-art-buf       to col-des.

           move 0                 to ef-qta-buf.

           perform RECUPERA-IVA.
           if rlis-codice not = 0
              perform CAMPI-PREZZI
           else
              perform RECUPERA-PROGMAG
           end-if.
           perform DISPLAY-SCREEN.

      ***---
       CAMPI-PREZZI.
           inquire ef-data value in como-data.
           perform DATE-TO-FILE.
           move como-data to como-data-ordine.
                   
           if chk-manuale-buf = 1
              move ef-impforn-buf to rlis-tipo-tratt-imposte
              perform VALORI-LISTINO-VIDEO
              move pgm-name to ProgrammaInUso
           else
              move spaces   to ProgrammaInUso
           end-if. 
           perform RECUPERA-PROGMAG.
           perform CALCOLA-PRZ-FINITO.
           move rof-prg-chiave to prg-chiave.
           read progmag no lock invalid continue end-read.
           perform RECUPERA-PREZZO.
           perform RECUPERA-SCONTO.

           move start-tasse      to imp-merce ef-imp-buf.

      *    luciano
           move imponibile-merce to ef-imp-buf.
      *    luciano fine

           move prz-reale        to prezzo-finale.

           move imposta-consumo  to ef-cons-buf.
           compute como-imposta = imposta-cobat + imposta-cou.
           move como-imposta     to ef-cou-buf.
           move add-piombo       to ef-add-buf.
           move costi-agg        to ef-costi-agg-buf.

      ***---
       RECUPERA-PROGMAG.
      *****     move 0             to figli-ok.
      *****     move art-codice    to prg-cod-articolo.
      *****     move tca-cod-magaz to prg-cod-magazzino.
      *****     move low-value     to prg-tipo-imballo
      *****                           prg-peso.
      *****
      *****     initialize hid-rof-prg-chiave.
      *****     move 0 to max-qta.
      *****     start PROGMAG key >= prg-chiave
      *****           invalid continue
      *****       not invalid
      *****           perform until 1 = 2
      *****              read PROGMAG next no lock 
      *****                   at end exit perform 
      *****              end-read
      *****              if prg-cod-articolo  not = art-codice or
      *****                 prg-cod-magazzino not = tca-cod-magaz
      *****                 exit perform
      *****              end-if
      *****              if prg-cod-magazzino not = space
      ******    luciano inizio                       
      *****                 if prg-attivo or prg-disattivo
      ******    luciano fine
      *****                    add 1             to figli-ok
      *****                    move prg-giacenza to como-giacenza-2
      *****                    if como-giacenza-2 > max-qta
      *****                       move prg-chiave      to hid-rof-prg-chiave
      *****                       move como-giacenza-2 to max-qta
      *****                    else
      *****                       if max-qta = zero
      *****                          move prg-chiave   to hid-rof-prg-chiave
      *****                          move como-giacenza-2 to max-qta
      *****                       end-if
      *****                    end-if
      *****                  end-if
      *****              end-if
      *****           end-perform
      *****     end-start.
      *****
      *****     if figli-ok = 0
      *****        set errori to true
      *****     else
              move prg-chiave    to hid-rof-prg-chiave
              move hid-rof-prg-chiave to prg-chiave rof-prg-chiave
              read progmag no lock invalid continue end-read

              move prg-tipo-imballo   to ef-imb-ord-buf
              modify ef-imb-ord value ef-imb-ord-buf.
      *****     end-if.

      ***---
       RECUPERA-PREZZO.
           if rlis-sconti-zero-no or chk-manuale-buf = 1
              move rlis-prz-acq to ef-uni-buf 
                                   hid-rof-prz-unitario
                                   rof-prz-unitario
           else
              move rlis-scelta to ef-uni-buf 
                                  hid-rof-prz-unitario
                                  rof-prz-unitario
           end-if.

      ***---
       RECUPERA-SCONTO.
           if rlis-sconti-zero-no
              move rlis-sconto-1   to ef-sconto-1-buf
                                      rof-sconto-1
                                      hid-rof-sconto-1
           else
              move 0 to ef-sconto-1-buf rof-sconto-1 hid-rof-sconto-1
           end-if.
           move rlis-sconto-2   to ef-sconto-2-buf
                                   rof-sconto-2
                                   hid-rof-sconto-2
           move rlis-sconto-3   to ef-sconto-3-buf
                                   rof-sconto-3
                                   hid-rof-sconto-3
           move rlis-sconto-4   to ef-sconto-4-buf
                                   rof-sconto-4
                                   hid-rof-sconto-4
           move rlis-sconto-5   to ef-sconto-5-buf
                                   rof-sconto-5
                                   hid-rof-sconto-5

           move rlis-costi-agg-tot  to ef-costi-agg-buf
                                       rof-costi-aggiuntivi
                                       hid-rof-costi-aggiuntivi.

      ***---
       RECUPERA-IVA.          
           if ef-cod-iva-buf not = spaces
              move ef-cod-iva-buf  to ef-cod-iva-buf 
                                      tbliv-codice2
                                      col-iva
           else
      *    propongo il codice esenzione iva dell'ordine, se vuoto prendo 
      *    quello dell'articolo
              inquire ef-ese-iva value ef-cod-iva-buf 
              if ef-cod-iva-buf not = space
                 move ef-cod-iva-buf  to ef-cod-iva-buf 
                                         tbliv-codice2
                                         col-iva
              else
                 move art-codice-iva  to ef-cod-iva-buf 
                                         tbliv-codice2
                                         col-iva
              end-if
           end-if.
           move "IV"   to tbliv-codice1.
           move spaces to tbliv-descrizione1
                          tbliv-descrizione2.
           read tivaese no lock invalid continue end-read.
           perform MOVE-DESCR-IVA-2.

      ***---
       IMBALLI-QTA.
           move 0 to resto.
           if rof-qta-ord >= imq-qta-imb|hid-imballi
              divide rof-qta-ord by imq-qta-imb giving ris
                                         remainder resto
              |divide rof-qta by hid-imballi giving ris
              |                           remainder resto
           else
              move 1 to resto
              move 0 to ris
           end-if.
      *
           if resto > 0
              move imq-qta-imb|hid-imballi 
                to imballi-ed
              display message "Imballo standard pezzi ", imballi-ed, "."
                       x"0d0a""Correzione automatica?"
                        title titolo
                         type mb-yes-no-cancel
                       giving scelta
                         icon mb-warning-icon
                      default mb-cancel

              if scelta = mb-yes
                 compute rof-qta-ord = ( imq-qta-imb * ( ris + 1 ) )
              end-if
           else
              move mb-yes to scelta
           end-if.
      
           evaluate scelta
           when mb-yes
                 move imq-qta-imb     to imballi-ed
                set tutto-ok to true
                move rof-qta-ord to ef-qta-buf col-qta
                display ef-qta
           when mb-no
                continue
           when mb-cancel
                set errori to true
                move 78-ID-ef-qta to control-id
                move 4 to accept-control
           end-evaluate.

      *    controllo su arrotondamento bancale
           if tutto-ok 
              move 0 to resto

              if art-qta-epal = 0
                 move art-qta-std to art-qta-epal
              end-if

              if art-qta-epal not = 0
                 divide rof-qta-ord by art-qta-epal giving ris
                                                 remainder resto

                 if resto not = 0
                    if ris = 0
                       add 1 to ris
                       |resto diventa una variabile di comodo
                       compute resto = art-qta-epal * ris 
                       compute resto = 
                               resto - 
                             ( art-qta-epal * 
                                         tge-perce-arrot-bancale / 100 )

                       if rof-qta-ord >= resto
                          compute resto = art-qta-epal * ris
                          move resto to imballi-ed
                          display message "Quantità per bancale ", 
                                          imballi-ed, "."
                                   x"0d0a""Correzione automatica?"
                                   title titolo
                                    type mb-yes-no-cancel
                                  giving scelta
                                    icon mb-warning-icon
                                 default mb-cancel
                          evaluate scelta
                          when mb-yes
                               compute rof-qta-ord = art-qta-epal * ris
                               move rof-qta-ord to ef-qta-buf col-qta
                               display ef-qta
                       
                          when mb-no
                               continue
                          when mb-cancel
                               set errori to true
                               move 78-ID-ef-qta to control-id
                               move 4 to accept-control
                          end-evaluate
                       end-if
                    else
                       add 1 to ris
                       |resto diventa una variabile di comodo
                       compute resto = art-qta-epal * ris 
                       compute resto = 
                               resto - 
                             ( art-qta-epal * 
                                         tge-perce-arrot-bancale / 100 )
                       if rof-qta-ord >= resto
                          compute resto = art-qta-epal * ris
                          move resto to imballi-ed
                          display message "Quantità per bancale ", 
                                          imballi-ed, "."
                                   x"0d0a""Correzione automatica?"
                                   title titolo
                                    type mb-yes-no-cancel
                                  giving scelta
                                    icon mb-warning-icon
                                 default mb-cancel
                          evaluate scelta
                          when mb-yes
                               compute rof-qta-ord = art-qta-epal * ris
                               move rof-qta-ord to ef-qta-buf col-qta
                               display ef-qta
                       
                          when mb-no
                               continue
                          when mb-cancel
                               set errori to true
                               move 78-ID-ef-qta to control-id
                               move 4 to accept-control
                          end-evaluate
                       else
                          compute resto = art-qta-epal * ( ris - 1 )
                          move resto to imballi-ed
                          display message "Quantità per bancale ", 
                                          imballi-ed, "."
                                   x"0d0a""Correzione automatica?"
                                   title titolo
                                    type mb-yes-no-cancel
                                  giving scelta
                                    icon 2
                                 default mb-cancel
                          evaluate scelta
                          when mb-yes
                               subtract 1 from ris
                               compute rof-qta-ord = art-qta-epal * ris
                               move rof-qta-ord to ef-qta-buf col-qta
                               display ef-qta
                       
                          when mb-no
                               continue
                          when mb-cancel
                               set errori to true
                               move 78-ID-ef-qta to control-id
                               move 4 to accept-control
                          end-evaluate
                       end-if
                    end-if
                 end-if
              end-if   
           end-if.
      
      ***---
       DESCRIZIONE-IMBALLO.
           call "C$JUSTIFY" using imballi-ed, "L".
           inspect art-descrizione replacing trailing spaces 
                                          by low-value.
           inspect link-des        replacing trailing spaces 
                                          by low-value.
           initialize lab-art-buf.

              string  art-descrizione delimited by low-value
                      " - "           delimited by size
                      link-des        delimited by low-value
                      " da "          delimited by size
                      imballi-ed      delimited by spaces
                      " x "           delimited by size
                      art-udm-imballo delimited by size
                      into lab-art-buf
              end-string
           inspect art-descrizione replacing trailing low-value by space
           inspect link-des        replacing trailing low-value by space
           display lab-art.
           move lab-art-buf to col-des.

      ***--- 
       VALUTA-CAMBIO-PREZZO.
           if hid-rof-prz-unitario = 0
              move art-prezzo-vendita to hid-rof-prz-unitario
           end-if.
           set tutto-ok to true.
      *     move rof-prz-unitario to SavePrezzo.
      *     perform IMPOSTE.
      *     evaluate true
      *     when rof-prz-unitario = hid-rof-prz-unitario
      *          move SavePrezzo to rof-prz-unitario
      *          perform IMPOSTE
      *     when rof-prz-unitario > hid-rof-prz-unitario
      *          compute como-prezzo =
      *          hid-rof-prz-unitario 
      *             move SavePrezzo to rof-prz-unitario
      *             perform IMPOSTE
      **          end-if

      *     when rof-prz-unitario < hid-rof-prz-unitario
      *          compute como-prezzo =
      *          ( hid-rof-prz-unitario - ( ( hid-rof-prz-unitario * hid-var-meno ) / 100 ) )
      *           hid-rof-prz-unitario 
      *          if SavePrezzo < como-prezzo
LUBEXX*             if si-controlla-scostamento |CBLCONFI
LUBEXX*                display message "Prezzo non nei limiti previsti."
LUBEXX*                         x"0d0a""Proseguire comunque?"
LUBEXX*                          title titolo
LUBEXX*                           icon mb-warning-icon
LUBEXX*                           type mb-yes-no
LUBEXX*                         giving scelta
LUBEXX*             else
LUBEXX*                move mb-yes to scelta
LUBEXX*             end-if
LUBEXX*          else
LUBEXX*             move mb-yes to scelta
      *          end-if
      *
      *          if scelta = mb-no 
      *             set errori to true 
      *          else
      *             move SavePrezzo to rof-prz-unitario
      *             perform IMPOSTE
      *          end-if

      *     end-evaluate.

      ***---
       PREZZO-ZERO.
LUBEXX     if tca-si-speciale
LUBEXX        move mb-yes to scelta
LUBEXX     else
              display message "Articolo omaggio?"
                        title titolo
                         type mb-yes-no
                      default mb-no
                       giving scelta
LUBEXX     end-if.

           if scelta = mb-no 
              set errori to true 
           else
              move "IV"        to tbliv-codice1
              move iva-omaggio to ef-cod-iva-buf col-iva tbliv-codice2
              read tivaese no lock invalid continue end-read
              perform MOVE-DESCR-IVA-2
              move 0 to rof-manuale chk-manuale-buf
              perform AZZERA-IMPORTI
              perform DISABLE-CAMPI-PREZZI
              perform DISPLAY-SCREEN
              perform AZZERA-MANUALE
              move 0 to chk-manuale-buf
              move 1 to ChkManualeBitmapNumber
              display chk-manuale
           end-if. 

      ***---
       AZZERA-IMPORTI.
           move 0 to ef-imp-buf 
                     ef-sconto-1-buf
                     ef-sconto-2-buf
                     ef-sconto-3-buf
                     ef-sconto-4-buf
                     ef-sconto-5-buf

                     ef-cons-buf 
                     ef-cou-buf
                     ef-add-buf
                     ef-costi-agg-buf

                     col-imp    
                     col-sconto-1
                     col-sconto-2
                     col-sconto-3
                     col-sconto-4
                     col-sconto-5.

      ***---
       DISABLE-CAMPI-PREZZI.
           modify ef-cod-iva    read-only 
           modify ef-sconto-1   read-only
           modify ef-sconto-2   read-only
           modify ef-sconto-3   read-only
           modify ef-sconto-4   read-only
           modify ef-sconto-5   read-only
           modify ef-cons       read-only
           modify ef-cou        read-only
           modify ef-add        read-only
           modify ef-costi-agg  read-only.
 
      ***---
       ENABLE-CAMPI-PREZZI.
           modify ef-cod-iva    not read-only
           modify ef-sconto-1   not read-only
           modify ef-sconto-2   not read-only
           modify ef-sconto-3   not read-only
           modify ef-sconto-4   not read-only
           modify ef-sconto-5   not read-only
           modify ef-cons       not read-only
           modify ef-cou        not read-only
           modify ef-add        not read-only
           modify ef-costi-agg  not read-only.
 
      ***---
       FIND-MORE-ARTICOLI-ON-PROGMAG.
           move 0             to num-articoli.
           move low-value     to prg-rec.
           move art-codice    to prg-cod-articolo.
           move tca-cod-magaz to prg-cod-magazzino.
           start progmag key  is >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform    end-read
                    if prg-cod-articolo      = art-codice    and
                       prg-cod-magazzino     = tca-cod-magaz
                       if prg-tipo-imballo  not = spaces and
                          prg-peso          not = 0      and
                          prg-attivo
                          add 1 to num-articoli
                          evaluate true
                          when num-articoli = 1 
                               move prg-chiave to GiacenzaKey
                          when num-articoli > 1 
                               exit perform
                          end-evaluate
                       end-if
                    else
                       exit perform
                    end-if
                 end-perform
           end-start.

      ********---
      ***** FIND-PROGMAG.
      **********     move 0 to sum      SumKey.
      **********     move 0 to giacenza GiacenzaKey.
      *****     move 0 to imq-qta-imb |hid-imballi.
      *****     
      *****     initialize hid-rof-prg-chiave
      *****                                replacing numeric data by zeroes
      *****                               alphanumeric data by spaces.
      **********     move art-codice     to prg-cod-articolo.
      **********     move storemagazzino to prg-cod-magazzino.
      **********     move low-value      to prg-tipo-imballo prg-peso.
      **********     start progmag key is >= prg-chiave invalid continue 
      **********     end-start.
      *****
      **********     if status-progmag = "00"
      **********
      **********        perform until 1 = 2
      **********           read progmag next no lock at end exit perform end-read
      **********           if prg-cod-articolo  not = art-codice or
      **********              prg-cod-magazzino not = StoreMagazzino
      **********              exit perform
      **********           end-if
      **********           if prg-attivo
      **********              if prg-giacenza > 0 and 
      **********                 prg-giacenza > giacenza
      **********                 move prg-giacenza to giacenza
      **********                 move prg-chiave   to GiacenzaKey
      **********              end-if
      **********              if prg-ini-udm + prg-acq-udm + prg-ven-udm > 0 and
      **********                 prg-ini-udm + prg-acq-udm + prg-ven-udm > sum
      **********                 compute sum = ( prg-ini-udm + 
      **********                                 prg-acq-udm + 
      **********                                 prg-ven-udm )
      **********                 move prg-chiave to SumKey
      **********              end-if
      **********           end-if
      **********        end-perform
      **********
      **********        if giacenza > 0
      **********           move GiacenzaKey to HiddenKey
      **********        else
      **********           if sum > 0
      **********              move SumKey to HiddenKey
      **********           else
      **********              move art-codice           to prg-cod-articolo
      **********              move StoreMagazzino       to prg-cod-magazzino
      **********              add art-peso-utf          to art-peso-non-utf 
      **********                                    giving prg-peso
      **********              move art-imballo-standard to prg-tipo-imballo
      **********              read progmag no lock invalid continue end-read
      **********              move prg-chiave to HiddenKey
      **********              if prg-bloccato or prg-disattivo
      **********                 move spaces  to hid-rof-tipo-imballo
      **********              end-if
      **********           end-if
      **********        end-if
      **********        
      **********     end-if.
      *****
      *****     move prg-chiave to hid-rof-prg-chiave.
      *****     if prg-bloccato or prg-disattivo
      *****        move spaces  to hid-rof-tipo-imballo
      *****     end-if.
      *****
      *****     if hid-rof-tipo-imballo not = spaces
      *****        move hid-rof-tipo-imballo     to imq-codice
      *****     else
      *****        move art-imballo-standard to imq-codice
      *****     end-if.
      *****
      *****     read timbalqta no lock 
      *****        invalid 
      *****           continue 
      *****     end-read.
      *****     move imq-qta-imb     to imballi-ed.
      *****     move imq-tipo        to imb-codice.
      *****     read timballi no lock 
      *****          invalid  initialize imb-descrizione
      *****     end-read.               
      *****     inspect imb-descrizione replacing trailing spaces 
      *****                                             by low-value.
      *****     |move imb-descrizione to hid-des-imballo.
      *****
      **********     if pgm-name = "gordfornvar"
      **********        move rof-qta-imballi to hid-imballi
      **********        move rof-des-imballo to hid-des-imballo
      **********     end-if.
      *****                          
      *****     move art-codice to prg-cod-articolo.
      **********     move 0          to prg-peso. 
      **********     move spaces     to prg-cod-magazzino.
      **********     move spaces     to prg-tipo-imballo.
      **********     read progmag    no lock invalid continue end-read.
      *****                          
      *****     move art-codice     to prg-cod-articolo.
      *****     move StoreMagazzino to prg-cod-magazzino.
      *****     move low-value      to prg-tipo-imballo prg-peso.
      *****     start progmag key >= prg-chiave 
      *****           invalid continue 
      *****       not invalid
      *****           move 0 to hid-giacenza hid-impegnato hid-ordinato
      *****           perform until 1 = 2
      *****              read progmag next no lock 
      *****                   at end exit perform 
      *****              end-read
      *****              if prg-cod-articolo  not = art-codice or
      *****                 prg-cod-magazzino not = StoreMagazzino
      *****                 exit perform
      *****              end-if
      *****              add prg-giacenza   to hid-giacenza
      *****              add prg-impegnato  to hid-impegnato
      *****              add prg-ordinato-1 to hid-ordinato
      *****           end-perform
      *****     end-start.
      *****
      *****     perform LABEL-VALORI.
      *****
      *****     move HiddenKey to prg-chiave.
      *****     read progmag no lock.
      *****
      ******    luciano 
      ******     move hid-imballi  to ef-qta-buf.

      ***---
       READ-TMARCHE.
           move art-marca-prodotto to mar-codice.
           read tmarche no lock 
              invalid 
                 continue 

           end-read.



      ***--- 
       SOMMA-PESO-UTF-MAN.           
           inquire form1-gd-1, last-row in tot-righe.
           move 0 to tot-peso.

           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 4), cell-data in como-qta

              inquire form1-gd-1(store-riga, 1), 
                                            hidden-data in hid-rof-rec-1
              inquire form1-gd-1(store-riga, 2), 
                                            hidden-data in hid-rof-rec-2
              inquire form1-gd-1(store-riga, 3), 
                                            hidden-data in hid-rof-rec-3
                   
      *        move hid-utf to como-peso
      *
      *        compute tot-peso = ( tot-peso + ( como-qta * como-peso) )
      *        if tot-peso > 500 
      *           set errori to true
      *           exit perform
      *        end-if
           end-perform.

      ****--- 
      * SOMMA-PESO-UTF.    
      *     inquire form1-gd-2, last-row in tot-righe.
      *     move 0 to tot-peso.
      *
      *     perform varying store-riga from 2 by 1 
      *               until store-riga > tot-righe
      *        inquire form1-gd-2(store-riga, 4),  cell-data in como-qta
      *        inquire form1-gd-2(store-riga, 1), 
      *                hidden-data in hid-rof-rec
      *        compute tot-peso = ( tot-peso + ( como-qta * hid-utf) )
      *        if tot-peso > 500 
      *           set errori to true
      *           exit perform
      *        end-if
      *     end-perform.

      ****---
      * SUPERAMENTO-500-UTF.
      *     if superamento-500
      *        if e-gui = 1
      *           perform SOMMA-PESO-UTF
      *        end-if
      *        if e-man = 1
      *           perform SOMMA-PESO-UTF-MAN
      *        end-if
      *        if errori
      *           move tot-peso to tot-peso-edit
      *           display message "Superamento 500 Kg. UTF."
      *               x"0d0a""Peso raggiunto: ", tot-peso-edit, " Kg."
      *               x"0d0a""Confermi?"
      *                     title tit-err
      *                      type mb-yes-no
      *                   default mb-no
      *                      icon 2
      *                    giving scelta
      *           if scelta = mb-yes
      *              set tutto-ok to true
      *           else
      *              perform CANCELLA-COLORE
      *           end-if
      *        end-if
      *     end-if.
 
      ***---
       SPOSTAMENTO.
           inquire form1-gd-1, last-row in tot-righe.
           if event-data-2 >= 2 and
              event-data-2 <= tot-righe
      
              set tutto-ok to true
      
              move event-data-2 to riga              
              perform ROW-TO-ENTRY

              read progmag no lock 
                   invalid display message "Dati errati su PROGMAG"
                                    X"0d0a""PRG-CHIAVE: " prg-chiave
                                    X"0d0a""Contattare assistenza"
                                     title tit-err
                                      icon 3
              end-read

              move art-marca-prodotto to mar-codice
              read tmarche no lock 
                 invalid 
                    continue 
              end-read
           end-if.           
                                
           perform CANCELLA-COLORE.      
           perform COLORE. 

      *****     set ArticoloSetFocus to true.
           set event-action     to event-action-terminate.
           set FromSpostamento  to true.
           set ControllaCampi   to false.

      ****---
      * SPOSTAMENTO-2.           
      *     inquire form1-gd-2 cursor-y in riga last-row in tot-righe.
      *     if event-data-2 < 2 move 2 to event-data-2 end-if.
      *
      *     evaluate event-data-1
      *     when 1
      *     when 2
      *
      *     when 3 set event-action to event-action-fail
      *     when 6 if not MousePressed
      *               add 1 to event-data-2
      *            end-if
      *            set event-action to event-action-fail
      *     end-evaluate.
      *
      *     if event-data-2 > tot-righe
      *        move 2 to event-data-2
      *        move 4 to event-data-1
      *     end-if.
      *
      *     if riga not = event-data-2
      *        move 4 to event-data-1 colonna
      *        modify form1-gd-2, cursor-x = event-data-1, 
      *                           cursor-y = event-data-2
      *        inquire form1-gd-2(event-data-2, 1), 
      *                hidden-data in hid-rof-rec
      *        move hid-giacenza  to prg-giacenza
      *        move hid-impegnato to prg-impegnato
      *        move hid-ordinato  to prg-ordinato-1
      *        perform LABEL-VALORI
      *
      *        display lab-giacenza lab-impegnato lab-ordinato
      *                lab-gia      lab-imp       lab-ord
      *
      *        move 1000 to control-id
      *        move    4 to accept-control
      *        set event-action to event-action-fail
      *     end-if.
      *
      *     move event-data-2 to riga.
      *     perform COLORE-2.
      *     set MousePressed to false.
      *     set ControllaCampi   to false.
      *****     set ArticoloSetFocus to false.

      ***---
       LABEL-VALORI.
           move prg-chiave     to old-prg-chiave.
           move art-codice     to prg-cod-articolo.
           move StoreMagazzino to prg-cod-magazzino.
           move low-value      to prg-tipo-imballo prg-peso.
           start progmag key >= prg-chiave 
                 invalid continue 
             not invalid
                 move 0 to hid-giacenza hid-impegnato hid-ordinato
                 perform until 1 = 2
                    read progmag next no lock 
                         at end exit perform 
                    end-read
                    if prg-cod-articolo  not = art-codice or
                       prg-cod-magazzino not = StoreMagazzino
                       exit perform
                    end-if
                    add prg-giacenza   to hid-giacenza
                    add prg-impegnato  to hid-impegnato
                    add prg-ordinato-1 to hid-ordinato
                 end-perform
           end-start.
           move 0 to prg-peso prg-peso-utf.

           perform DISPLAY-LABEL-VALORI.
           move old-prg-chiave to prg-chiave.
           read progmag no lock.

      ***---
       DISPLAY-LABEL-VALORI.
           if hid-giacenza >= 0 move 515 to ColorGiacenza
           else                 move 525 to ColorGiacenza
           end-if.
           move hid-giacenza  to giacenza-ed.

           if hid-impegnato >= 0 move 515 to ColorImpegnato
           else                  move 525 to ColorImpegnato
           end-if.
           move hid-impegnato to impegnato-ed.

           if hid-ordinato >= 0 move 515 to ColorOrdinato
           else                 move 525 to ColorOrdinato
           end-if.
           move hid-ordinato to ordinato-ed.

           display lab-gia lab-giacenza 
                   lab-ord lab-ordinato 
                   lab-imp lab-impegnato.

      ****---
      * ASSORCLI-IN-LINE.
      *     if link-path-tmp-assorcli not = spaces
      *        move tmp-asc-cod-articolo-per-cliente to hid-cod-art-cli
      *        move SaveGDO    to tmp-asc-cod-gruppo-gdo
      *        move ef-cli-buf to tmp-asc-cod-cliente         convert
      *        move ef-des-buf to tmp-asc-progressivo-destino convert
      *        move art-codice to tmp-asc-cod-articolo
      *        read tmp-assorcli
      *             invalid move low-value  to tmp-asc-chiave
      *                     move SaveGDO    to tmp-asc-cod-gruppo-gdo
      *
      *                     move art-codice to tmp-asc-cod-articolo
      *                     start tmp-assorcli key is >= key01
      *                           invalid set trovato to false
      *                       not invalid
      *                           read tmp-assorcli next
      *                           if art-codice = tmp-asc-cod-articolo
      *                              move art-codice to SaveArticolo
      *                              set trovato to true
      *                           else
      *                              set trovato to false
      *                           end-if
      *                     end-start
      *         not invalid set trovato to true
      *        end-read
      *        if trovato perform METTI-IN-CELLS end-if
      *     end-if.

      ****---
      * METTI-IN-CELLS.
      *     set trovato to true.
      *     move tmp-asc-prezzo-finito             to asc-prezzo-finito.
      *     move tmp-asc-perc-sconto-listino       to col-sconto
      *                                                ef-sconto-buf
      *                                           asc-perc-sconto-listino
      *     move tmp-asc-imposta-consumo           to col-consumo
      *                                                ef-cons-buf
      *                                               asc-imposta-consumo
      *     move tmp-asc-imposta-ecologica-coubat  to col-cou
      *                                                ef-cou-buf
      *                                     asc-imposta-ecologica-coubat.
      *     move tmp-asc-cod-articolo-per-cliente to hid-cod-art-cli.

      ***---
       CHANGE-TAB.
           set NonCambiareTab   to false.
      *****     set ArticoloSetFocus to false.
           set ControllaCampi   to true.

           move 0 to v-articoli v-manuale v-dati-fatt.
           if mod-k = 1
              set NonCambiareTab to true
           else
              evaluate EVENT-DATA-1
              when 1
                   move 0 to v-manuale, v-guidata
                   if StoreMagazzino = "EXD"
                      move 1 to v-dati-fatt
                   end-if
      *             if DatiBollaManuale  move 1 to v-bolla
      *             else                 move 0 to v-bolla
      *             end-if
                   move 0 to v-dett v-blister v-articoli v-manuale
      *             perform ABILITA-GEST-PLUS
              when 2
                   perform CHECK-PAGE-1
                   if tutto-ok
                      move 0 to v-gest-plus
      *                display lab-gest ef-gest
                      perform CANCELLA-COLORE
      *                evaluate form1-radio-1-buf
      *                when 1 |MANUALE
                           if FirstTime
                              if pgm-name = "gordforn"
                                |Per gordforn non è ancora stato
                                |settato il tipo di caricamento
                                |sul file di testata
                                 set tof-manuale to true
                              end-if
                              perform DISPLAY-SCREEN-MANUALE
                              |Rimane a TRUE solo se no ci sono righe,
                              |ossia in caso di transazione in corso...
                              if FirstTime
                                 set errori to true
                                 set ControllaCampi to false
                                 move store-id to CONTROL-ID
                                 move 4        to ACCEPT-CONTROL
                                 set NonCambiareTab to true
                              else
                                 move 2 to save-riga
                                 perform SETTA-RIGA
                                 set ControllaCampi to true
                              end-if
                           end-if
                           |Settato a errori da "if FirstTime" sopra
                           if tutto-ok
      *****                     set ArticoloSetFocus to true
                              move 0 to v-guidata
                              move chk-manuale-buf to v-manuale
      *****                        move 1 to v-manuale
                              if tof-da-confermare-si 
                                 move 1 to v-articoli
                              end-if
      *                        if rlis-gdo
      *                           modify ef-sconto,     read-only
      *                        else
      *                           if pgm-name = "gordforn" and 
      *                              OrdineTradizionale
      *                              modify ef-sconto, read-only
      *                           else
      *                              modify ef-sconto, not read-only
      *                           end-if
      *                        end-if
      *                        if tcl-si-recupero
      *                           move 1 to v-blister
      *                        end-if
                           end-if
      *                when 2 |GUIDATA
      *                     if FirstTime
      *                        if pgm-name = "gordforn"
      *                          |Per gordforn non è ancora stato
      *                          |settato il tipo di caricamento
      *                          |sul file di testata
      *                           set tof-guidata to true
      *                        end-if
      *                        perform DISPLAY-SCREEN-GUIDATA
      *                        modify form1-gd-2, lines 26,08
      *                     end-if
      ******                     set ArticoloSetFocus to false
      *                     move 1 to v-guidata
      *                     move 0 to v-manuale
      *                end-evaluate
                      move 0 to v-bolla
      *                if VenditaAlDettaglio
      *                   move 1 to v-dett
      *                end-if
                   else
                      set ControllaCampi to false
                      move store-id to CONTROL-ID
                      move 4        to ACCEPT-CONTROL
                      set NonCambiareTab to true
                   end-if
              end-evaluate
           end-if.

           display Screen1-Ta-1.

      ****---
      * DISPLAY-CAMPI-BOLLA.
      *     display frame-bolla ef-num-bolla ef-data-bolla lab1 lab2.

      ***---
       MOVE-DATI.
           initialize lab-iva-buf.
      *     move des-prov to SaveProvincia.

           if ef-cod-iva-buf = spaces
              move cli-iva-ese of clienti to ef-cod-iva-buf 
                                  tbliv-codice2
              move "IV"        to tbliv-codice1
              read tivaese 
                   invalid continue
               not invalid perform MOVE-DESCR-IVA
              end-read
              display lab-iva ef-cod-iva
           end-if.

      *     move des-superamento-500 to flag-superamento-500.

      *     if ef-vet-buf = 0
      *        move des-vettore         to ef-vet-buf vet-codice
      *        move spaces to lab-vet-buf
      *        read tvettori invalid continue
      *                  not invalid move vet-descrizione to lab-vet-buf
      *        end-read
      *        display lab-vet ef-vet
      *     end-if.

           move tof-chiave   to nof-chiave-ordine
           move low-value    to nof-num-nota
           start nordforn key not < nof-chiave
              invalid
                 continue
              not invalid
                 move zero   to idx
                 perform until 1 = 2
                    read nordforn next no lock
                       at end
                          exit perform
                    end-read
                    if tof-chiave not = nof-chiave-ordine
                       exit perform
                    end-if
                    add 1 to idx
                    move nof-nota  to como-note(idx)
                    if idx = 4
                       exit perform
                    end-if
                 end-perform
           end-start.

           move como-note(1) to ef-note-1-buf
           move como-note(2) to ef-note-2-buf
           move como-note(3) to ef-note-3-buf
           move como-note(4) to ef-note-4-buf

           if ef-pag-buf = spaces
              move spaces  to lab-pag-buf
              move "PA"    to tblpa-codice1
              move cli-pag of clienti to tblpa-codice2 ef-pag-buf
              read tcodpag 
                   invalid continue
               not invalid perform MOVE-DESCR-PAG
              end-read
              display lab-pag ef-pag
           end-if.


      *     initialize tcl-rec.
      *     move cli-tipo to tcl-codice.
      *     read ttipocli no lock invalid continue end-read.
      *     perform ABILITA-GEST-PLUS.

      ****---
      * CONTA-ZERI.
      *     set trovato to false.
      *     inquire form1-gd-2, last-row in tot-righe.
      *     perform varying store-riga from 2 by 1
      *               until store-riga > tot-righe
      *        inquire form1-gd-2(store-riga, 4), cell-data in col-qta
      *        move col-qta to como-qta
      *        if como-qta > 0
      *           set trovato to true
      *           exit perform
      *        end-if
      *     end-perform.
      *
      ***---
       CONTA-ZERI-MAN.
           set trovato to false.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 4), cell-data in col-qta
              move col-qta to como-qta
              if como-qta > 0
                 set trovato to true
                 exit perform
              end-if
           end-perform.

      ***---
       BEFORE-CAMPI.
LABLAB     move 0 to e-cerca.
LABLAB     modify tool-cerca, enabled e-cerca.
           evaluate control-id
           when 78-ID-ef-cau
           when 78-ID-ef-cli
      *     when 78-ID-ef-des
                continue |lascio posare il fuoco su quei campi
                         |altrimenti mi risulta impossibile cambiare
                         |la causale, il cliente o il destino se mi
                         |trovo nella seconda pagina
           when 78-ID-ef-uni
                if chk-manuale-buf = 1
                   modify ef-uni, not read-only
                else
                   if rlis-codice = 0
                      modify ef-uni, not read-only
                   else
                      modify ef-uni,     read-only
                   end-if
                end-if
                |ATTIVO IL "MANUALE"
           when 78-ID-ef-cons
                if imf-prz-reale-utf-zero or art-no-imposte
                   perform CANCELLA-COLORE
                   move 78-ID-ef-cou to control-id
                   move 4 to accept-control
                end-if
           when 78-ID-ef-cou
                if imf-prz-reale-cou-zero and imf-prz-reale-cobat-zero
                or art-no-imposte         and art-no-cobat
                   perform CANCELLA-COLORE
                   move 78-ID-ef-add to control-id
                   move 4 to accept-control
                end-if
           when 78-ID-ef-add
                if imf-prz-reale-pb-zero or art-no-cobat
                   perform CANCELLA-COLORE
                   move 78-ID-ef-costi-agg to control-id
                   move 4 to accept-control
                end-if
           when other
                if FromSpostamento
                   set FromSpostamento to false
                   move 78-ID-ef-art   to control-id
                   move 4              to accept-control
                   perform CANCELLA-COLORE
                else
                   inquire ef-art, value in art-codice
                   if art-codice = 0
                      perform CANCELLA-COLORE
                      move 78-ID-ef-art to control-id
                      move 4            to accept-control
                   end-if
                end-if
      *          if pgm-name = "gordforn" and 
      *             OrdineTradizionale
LABLAB*             modify ef-uni, read-only
      *          else
LABLAB*             modify ef-uni, not read-only
      *          end-if
LABLAB*          if control-id = 78-ID-ef-uni
LABLAB*             if tcl-si-recupero
LABLAB*                move BitmapZoomEnabled to BitmapNumZoom
LABLAB*                move 1 to e-cerca
LABLAB*                display tool-cerca
      *                |Solo in INSERIMENTO
LABLAB*                if pgm-name = "gordforn"
LABLAB*                   if ef-uni-buf not = 0
LABLAB*                      modify ef-uni, read-only
LABLAB*                   end-if
LABLAB*                end-if
LABLAB*             end-if
LABLAB*          end-if
           end-evaluate.
           set CheckAfterZoom to false.
           modify ef-imb-ord, read-only.

      ***---
       CONTROLLO.
           if mod   = 0 and
              mod-k = 0 
              exit paragraph 
           end-if.
           set tutto-ok to true.
           if not ControllaCampi
              set ControllaCampi to true
              exit paragraph
           end-if.

           perform CONTROLLO-TESTA
           perform CONTROLLO-RIGA.

           if errori
              perform CANCELLA-COLORE
              move CONTROL-ID to store-id
              move 4          to ACCEPT-CONTROL
           |SPECIFICO PER GESTIONE ORDINI
           else    
              evaluate true        
              when num-articoli = 1
                   perform UN-SOLO-ARTICOLO-SU-PROGMAG
                   move art-codice to old-art-codice
              when num-articoli > 1                
                   move 78-ID-ef-art  to control-id
                   perform ZOOM-SU-PROGMAG-ARTICOLO
                   if stato-zoom = 0
                      move art-codice to old-art-codice
                   end-if
              end-evaluate
           end-if.


      ***---
       CONTROLLO-TESTA.
      *
      * Elenco degli ID sui quali fare il CONTROLLO nel programma gclienti
      * paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-anno è l'ID del control ef-anno
           when 78-ID-ef-anno
                inquire ef-anno, value in ef-anno-buf
                if ef-anno-buf = 0
                   set errori to true
                   display message box "Inserimento anno mancante"
                           title = tit-err
                           icon mb-warning-icon
                   move 78-ID-ef-anno to control-id
                end-if
                move ef-anno-buf to como-anno

           |78-ID-ef-cau è l'ID del control ef-cau
           when 78-ID-ef-cau
                inquire ef-cau, value in ef-cau-buf
                move ef-cau-buf to tca-codice
                read tcaumag
                     invalid
                     move spaces to tca-descrizione
                     set errori to true
                     display message "Causale di magazzino NON valida"
                               title tit-err
                                icon 2
                     move 78-ID-ef-cau to control-id
                 not invalid

                     if tca-no-movim-giac and
                        tca-no-movim-imp  and
                        tca-no-movim-ord  and
                        tca-no-giac-bloc
                        set CallWProgmag to false
                     else
                        set CallWProgmag to true
                     end-if

                     if not tca-fornitore
                        move spaces to tca-descrizione
                        set errori to true
                        display message
                            "Utilizzare una causale di tipo Fornitore"
                                 title tit-err
                                  icon 2
                        move 78-ID-ef-cau to control-id
                     end-if
                     if not tca-si-ord-forn
                        move spaces to tca-descrizione
                        set errori to true
                        display message
                            "Utilizzare una causale di tipo Ordine"
                               title tit-err
                               icon 2
                        move 78-ID-ef-cau to control-id
                     end-if

                     if old-magazzino not = tca-cod-magaz and
                        pgm-name = "gordfornvar"
                        set CambiatoMagazzino to true
                     end-if

                     if tca-cod-magaz = "EXD"
                        if pgm-name = "gordforn" or CambiatoMagazzino
                           move 1 to chk-dati-fatt-buf
                        end-if
                        move 1 to v-dati-fatt
                     else
                        move 0 to v-dati-fatt
                        move 0 to chk-dati-fatt-buf
                     end-if
                     display lab-dati-fatt chk-dati-fatt

                end-read
                move tca-descrizione to lab-cau-buf
                display lab-cau
                move tca-cod-magaz   to StoreMagazzino

                if tutto-ok
LUBEXX             if not StoSalvando
                      move 78-ID-ef-cli to control-id store-id
                      move 4 to accept-control
LUBEXX             end-if
                end-if

           |78-ID-ef-cli è l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf
                if ef-cli-buf not = spaces
                   inspect ef-cli-buf replacing trailing spaces
                                                         by low-values
                   initialize CountChar
                   inspect ef-cli-buf tallying CountChar for characters
                                                      before low-value
                   inspect ef-cli-buf replacing trailing low-values
                                                         by spaces

                   | SE E' NUMERICO LEGGO IL RECORD NORMALMENTE
                   if ef-cli-buf(1:CountChar) is numeric
                      perform SELEZIONA-NUMERICO
                   else
                      | HO DIGITATO UNA SIGLA ALFANUMERICA QUINDI
                      | APRO LO ZOOM (CASE SENSITIVE)
                      perform SELEZIONA-ALFA
                   end-if
                   display lab-cli
                   display lab-ind-cli
                   display lab-loc-cli

                else
                   set errori to true
                   move 78-ID-ef-cli to control-id
                   display message 
                                "Inserimento codice fornitore mancante"
                           title = tit-err
                           icon mb-warning-icon
                end-if

      *    valorizzazione dei dati del referente dell'ordine
                if tutto-ok
                   if cli-codice of clienti
                                         not = old-tof-cod-forn |and
      *                                   ef-referente-buf = spaces   
                      move cli-pag     of clienti  to ef-pag-buf
                      move cli-iva-ese of clienti  to ef-ese-iva-buf

                      modify ef-pag value ef-pag-buf
                      modify ef-ese-iva value ef-ese-iva-buf

                      move spaces to lab-pag-buf
                      move "PA"       to tblpa-codice1
                      move ef-pag-buf to tblpa-codice2
                      read tcodpag
                         invalid
                            initialize tblpa-descrizione1
                                       tblpa-descrizione2
                         not invalid 
                            perform MOVE-DESCR-PAG
                      end-read
                      display lab-pag

                      move "IV"   to tbliv-codice1
                      move ef-ese-iva-buf   to tbliv-codice2
                      move spaces to tbliv-descrizione1
                                     tbliv-descrizione2
                      read tivaese no lock 
                       invalid 
                          continue 
                      end-read
                      perform MOVE-DESCR-ESE-IVA
                      display lab-ese-iva
                   end-if
                end-if


           when 78-ID-ef-dest
                inquire ef-dest, value in ef-dest-buf
                if ef-dest-buf not = spaces
                   inspect ef-dest-buf replacing trailing spaces
                                                         by low-values
                   initialize CountChar
                   inspect ef-dest-buf tallying CountChar for characters
                                                      before low-value
                   inspect ef-dest-buf replacing trailing low-values
                                                         by spaces

                   | SE E' NUMERICO LEGGO IL RECORD NORMALMENTE
                   if ef-dest-buf(1:CountChar) is numeric
                      perform SELEZIONA-DESTINO-NUMERICO
                   else
                      | HO DIGITATO UNA SIGLA ALFANUMERICA QUINDI
                      | APRO LO ZOOM (CASE SENSITIVE)
                      perform SELEZIONA-DESTINO-ALFA
                   end-if
                   display lab-dest
                   display lab-ind-dest
                   display lab-loc-dest

                else
                   set errori to true
                   move 78-ID-ef-dest to control-id
                   display message 
                                "Inserimento codice destino mancante"
                           title = tit-err
                           icon mb-warning-icon
                end-if

      *    valorizzazione dei dati del referente dell'ordine
                if tutto-ok
                   if desf-codice not = old-tof-cod-forn or
                      desf-prog   not = old-tof-destino
                      
                      if desf-pag not = spaces
                         move desf-pag to ef-pag-buf
                      end-if

                      if ef-referente-buf = spaces

      *    guardo se prendere i dati dal destino o dal fornitore
      *                   if desf-referente-ord not = space
                            move desf-referente-ord  to ef-referente-buf
                            move desf-tel-dir-ref-ord   to ef-tel-buf
                            move desf-mail-ref-ord      to ef-mail-buf
      *                   else
      *                      move cli-referente-ord of clienti   
      *                                               to ef-referente-buf
      *                      move cli-tel-dir-ref-ord of clienti 
      *                                                  to ef-tel-buf
      *                      move cli-mail-ref-ord of clienti  
      *                                                  to ef-mail-buf
      *                   end-if
                         modify ef-referente value ef-referente-buf
                         modify ef-tel       value ef-tel-buf
                         modify ef-mail      value ef-mail-buf
                      end-if
                   end-if
                end-if

                if tutto-ok
                   move 0 to mod-k
                   move 1 to mod mod-campi e-man |e-gui
                   move 1 to NumBitmapArticoli
                   perform DISPLAY-SCREEN               
                   move 78-ID-ef-data to control-id
                   move 4             to accept-control
                   if pgm-name = "gordfornvar"
                      move 0 to mod-cliente-destino
                      display ef-cau ef-cli |ef-des

                      move 1 to e-pb-grid
                      move 5 to BitmapNumGridNuovo
                      move 4 to BitmapNumGridElimina
                      display pb-grid-elimina 
                              pb-grid-nuovo
                   end-if
                   display pb-articoli
                end-if


           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf
                move ef-data-buf to como-data
                if como-data = 0
                   move data-oggi to como-data
                   perform DATE-TO-SCREEN
                   move como-data to ef-data-buf
                else
                   |Prima ne controllo la validita...

                   move ef-data-buf to como-data
                   perform DATE-FORMAT
                   move como-data to ef-data-buf
      *             perform DATE-TO-FILE
                end-if
                perform DATE-TO-FILE
                display ef-data
                move como-data to tof-data-ordine
                perform SELEZIONA-LISTINO
                if not listino-trovato
                   move 0 to tlis-codice
                   perform VAL-NOTE-FORN
                else
                   perform VAL-NOTE-LISTINO
      *             set errori to true        
      *             display message box "Nessun Listino valido!!!"
      *                     title = tit-err
      *                     icon mb-warning-icon
      *             move 78-ID-ef-data to control-id
                end-if

           |78-ID-ef-data-pass è l'ID del control ef-data-pass
           when 78-ID-ef-data-cons
                inquire ef-data-cons, value in ef-data-cons-buf
                move ef-data-cons-buf to como-data
                if form1-radio-1-buf = 1
                   |Solo in fase di creazione
                   if pgm-name = "gordfornvar"
                      if como-data = 0
                         move data-oggi to como-data  
                         call   "utydata" using dom-lun,
                                                1,
                                                como-data,
                                                giorno
                         cancel "utydata"
                         compute como-data =
                                 function integer-of-date(como-data)
                         evaluate giorno
                         when 1     add 7 to como-data
                         when 2     add 6 to como-data
                         when other add 5 to como-data
                         end-evaluate    
                         compute como-data =
                                 function date-of-integer(como-data)
                         perform DATE-TO-SCREEN
                         move como-data to ef-data-cons-buf
                      else
                         perform DATE-FORMAT
                         move como-data to ef-data-cons-buf
                      end-if
                   else
                      if tof-inserito       or
                         tof-in-lavorazione or
                         tof-inviato
                         move data-oggi to como-data
                         call   "utydata" using dom-lun,
                                                1,
                                                como-data,
                                                giorno
                         cancel "utydata"
                         compute como-data =
                                 function integer-of-date(como-data)
                         evaluate giorno
                         when 1     add 7 to como-data
                         when 2     add 6 to como-data
                         when other add 5 to como-data
                         end-evaluate    
                         compute como-data =
                                 function date-of-integer(como-data)
                         perform DATE-TO-SCREEN
                         move como-data to ef-data-cons-buf
                      end-if
                   end-if
                else
                   if como-data = 0
                      move data-oggi to como-data  
                      call   "utydata" using dom-lun,
                                             1,
                                             como-data,
                                             giorno
                      cancel "utydata"
                      compute como-data =
                              function integer-of-date(como-data)
                      evaluate giorno
                      when 1     add 7 to como-data
                      when 2     add 6 to como-data
                      when other add 5 to como-data
                      end-evaluate    
                      compute como-data =
                              function date-of-integer(como-data)
                      perform DATE-TO-SCREEN
                      move como-data to ef-data-cons-buf
                   else
                      perform DATE-FORMAT
                      move como-data to ef-data-cons-buf
                   end-if
                end-if
                display ef-data-cons

      *     when 78-ID-ef-data-arrivo
      *          inquire ef-data-arrivo, value in ef-data-arrivo-buf
      *          move ef-data-arrivo-buf to como-data
      *          if como-data not = 0
      **             move data-oggi to como-data
      **             perform DATE-TO-SCREEN
      **             move como-data to ef-data-arrivo-buf
      **          else
      *             perform DATE-FORMAT
      *             move como-data to ef-data-arrivo-buf
      *          end-if
      *          display ef-data-arrivo

           when 78-ID-ef-cliente   
                inquire ef-cliente value cli-codice of clienti1
                set cli-tipo-c of clienti1 to true
                if cli-codice of clienti1 = zero
                   move space   to cli-ragsoc-1 of clienti1
                   move space   to lab-des-cli-buf
                   move 0       to ef-des-cli-buf
                   display ef-des-cli lab-des-cli
                else
                   read CLIENTI1
                      invalid
                         set errori   to true
                         display message box "Cliente non valido"
                             title titolo
                         move 78-id-ef-cliente to control-id
                         move space   to cli-ragsoc-1 of clienti1
                   end-read
                end-if
                move cli-ragsoc-1 of clienti1  to lab-cliente-buf
                modify lab-cliente title lab-cliente-buf

           when 78-ID-ef-des-cli
                inquire ef-cliente value des-codice
                inquire ef-des-cli value des-prog
                if des-codice = 0 or des-prog = 0
                   move 0     to ef-des-cli-buf
                   move space to lab-des-cli-buf lab-des-loca-buf
                   display ef-des-cli lab-des-cli lab-des-loca
                else
                   read destini
                      invalid
                         set errori   to true
                         display message box "Destino non valido"
                             title titolo
                         move 78-ID-ef-des-cli to control-id
                         move spaces to lab-des-cli-buf
                         move spaces to lab-des-loca-buf
                   end-read
                end-if
                move des-ragsoc-1      to lab-des-cli-buf
                modify lab-des-cli  title lab-des-cli-buf
                move des-localita      to lab-des-loca-buf
                modify lab-des-loca title lab-des-loca-buf

           when 78-ID-ef-data-listino
                inquire ef-data-listino, value in ef-data-listino-buf
                move ef-data-listino-buf to como-data
                if como-data not = 0
                   perform DATE-FORMAT
                   move como-data to ef-data-listino-buf
                   display ef-data-listino
                end-if

           |78-ID-ef-pag è l'ID del control ef-pag
           when 78-ID-ef-pag
                inquire ef-pag, value in ef-pag-buf  
                move spaces to lab-pag-buf
                move "PA"       to tblpa-codice1
                move ef-pag-buf to tblpa-codice2
                read tcodpag
                   invalid
                      set errori to true        
                      display message "Codice pagamento NON valido"
                              title = tit-err
                              icon mb-warning-icon
                      move 78-ID-ef-pag to control-id
                   not invalid 
                       perform MOVE-DESCR-PAG
                end-read
                display lab-pag

           when 78-id-ef-ese-iva
                inquire ef-ese-iva, value in ef-ese-iva-buf  
                move spaces to lab-ese-iva-buf
                if ef-ese-iva-buf not = spaces
                   move "IV"             to tbliv-codice1
                   move ef-ese-iva-buf   to tbliv-codice2
                   read tivaese
                      invalid
                         set errori to true        
                         display message box "Codice IVA NON valido"
                                 title = tit-err
                                 icon mb-warning-icon
                         move 78-ID-ef-ese-iva to control-id
                      not invalid 
                         if tbliv-percentuale not = 0
                            set errori to true        
                            display message box "Codice IVA NON valido "
                                                "in quanto non esente"
                                    title = tit-err
                                    icon mb-warning-icon
                            move 78-ID-ef-ese-iva to control-id
                         else                   
                            perform MOVE-DESCR-ESE-IVA
                         end-if
                   end-read
                end-if
                display lab-ese-iva

           when 78-ID-ef-promo
                inquire ef-promo value tpr-codice
                if tpr-codice = zero
                   move space   to tpr-descrizione
                else
                   read tpromo
                      invalid
                         set errori   to true
                         display message "Promo non valida"
                                   title titolo
                                    icon 2
                         move space   to como-descr-promo
                                         lab-promo-buf
                       not invalid
                          perform DESCRIZIONE-PROMO
                   end-read
                end-if
                modify lab-promo title lab-promo-buf
           end-evaluate.
           if como-data-ordine = 0
              move ef-data-buf to como-data
              perform DATE-TO-FILE
              move como-data   to como-data-ordine
           end-if.

      ***---
       CONTROLLO-RIGA.
           evaluate control-id
           |78-ID-ef-anno è l'ID del control ef-anno
           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf
                move ef-art-buf to art-codice
                if art-codice not = 0
                   perform LABEL-VALORI
                   if art-codice not = old-art-codice
                      read articoli no lock
                           invalid
                           set errori to true
                           initialize art-stato
                       not invalid
                           if art-attivo |or art-disattivo
                              move tlis-chiave to rlis-codice
                              move art-codice  to rlis-articolo
                              read rlistini no lock
                                   invalid
                                   move 0 to rlis-codice
                                   move 0 to rlis-prz-acq
                                             rlis-sconto-1
                                             rlis-sconto-2
                                             rlis-sconto-3
                                             rlis-sconto-4
                                             rlis-sconto-5
                                             rlis-costi-agg-tot
                                             rlis-tipo-tratt-imposte
                               not invalid
                                    move rlis-tipo-tratt-imposte 
                                      to imf-codice ef-impforn-buf
                                    read impforn no lock
                                    move imf-descrizione 
                                      to lab-impforn-buf
                                    display ef-impforn lab-impforn
                              end-read
                              if tutto-ok
                                 perform VALORIZZA-RIGA-ARTICOLO
                              end-if
                           else
                              set errori to true
                           end-if
                      end-read
                   else
                      if CambiatoMagazzino
                         |La prima volta che riconfermo la riga
                         |l'hidden viene aggiornato col valore nuovo.
                         |Testandolo con il codice in uso, se è uguale
                         |significa che è una riga già riconfermata.
                         if art-codice        not = old-art-codice    or
                            prg-peso          not = hid-rof-peso      or
                            prg-tipo-imballo  not = hid-rof-tipo-imballo  
                                                                    or
                            prg-cod-magazzino not = 
                                         hid-rof-cod-magazzino

                            move tca-cod-magaz to prg-cod-magazzino
                            read progmag no lock
                                 invalid set errori to true
                             not invalid 
                                 if prg-attivo|E' come se avessi scelto
                                    set CheckAfterZoom to true
                                    perform VALORIZZA-RIGA-ARTICOLO
                                 else
                                    set errori to true
                                 end-if
                            end-read
                         end-if
                      end-if
                      |move 0 to num-articoli
                      if cli-codice of clienti not = old-tof-cod-forn 
                         and pgm-name = "gordfornvar"
                         perform RECUPERA-IVA
                      end-if
                   end-if
         
                   if errori
                      evaluate true
                      when art-bloccato
                           display message "Articolo BLOCCATO"
                                     title tit-err
                                      icon 2
                      when art-disattivo
                           display message "Articolo SOSPESO"
                                     title tit-err
                                      icon 2
                      when other
                           display message "Codice articolo NON valido"
                                     title tit-err
                                      icon 2
                      end-evaluate
                      move 78-ID-ef-art to control-id
                      move art-codice   to SaveArticolo
                      perform PULISCI-CAMPI-LABELS
                      move SaveArticolo to ef-art-buf
                      display ef-art
                   else
                      if CheckAfterZoom
                         perform CANCELLA-COLORE
                      end-if
                      set FromSpostamento to false
                      move 78-ID-chk-manuale to control-id
      *****                move 78-ID-ef-imb-ord to control-id
                      move 4 to accept-control
                   end-if
                else
                    set errori   to true
                    display message "Articolo obbligatorio"
                              title tit-err
                               icon 2
                end-if

           when 78-ID-ef-imb-ord
                inquire ef-imb-ord value ef-imb-ord-buf
                move ef-imb-ord-buf   to imq-codice
                read TIMBALQTA
                     invalid
                      set errori to true
                      display message "Imballo NON valido"
                                title tit-err
                                 icon 2
                end-read
                initialize imballo-descrizione
                if tutto-ok
                   perform DESCR-IMBALLO
                   set FromSpostamento to false
                   move 78-ID-ef-qta to control-id
                   move 4 to accept-control
                end-if
                move imballo-descrizione to lab-imb-buf
                modify lab-imb title lab-imb-buf

           |78-ID-ef-qta è l'ID del control ef-qta
           when 78-ID-ef-qta
                inquire ef-qta, value in ef-qta-buf

                move ef-qta-buf to como-qta rof-qta-ord
                if como-qta = 0
                   set errori to true
                   display message "Inserimento quantità mancante"
                             title tit-err
                              icon 2
                else
                   if pgm-name = "gordfornvar"
                      inquire form1-gd-1(riga, 5),
                              cell-data in como-numero
                      if como-qta < como-numero and not NewRow
                         set errori to true
                         display message "Impossibile qta < evasa"
                                   title tit-err
                                    icon 2
                      end-if
                   end-if
                   if tutto-ok
                      perform IMBALLI-QTA
                   end-if
                end-if

                if errori
                   move 78-ID-ef-qta    to control-id
                else
                   if chk-manuale-buf = 1
                      move 78-ID-ef-uni to control-id
                   else
                      if rlis-codice = 0
                         move 78-ID-ef-uni to control-id
                      else
                         inquire chk-manuale, value in chk-manuale-buf
                         if chk-manuale-buf = 0
                            move 78-ID-ef-sconto-5 to control-id
                         else
                            move 78-ID-ef-uni      to control-id
                         end-if
                      end-if
                   end-if
                end-if
                move 4 to accept-control

           |78-ID-ef-uni è l'ID del control ef-uni
           when 78-ID-ef-uni
                inquire ef-uni, value in ef-uni-buf
                move ef-uni-buf to rof-prz-unitario
                                   col-uni
                                   como-rof-prz-unitario
                if tof-in-lavorazione
                   if rof-prz-unitario > old-prezzo and old-prezzo > 0
                      set errori to true
                      display message "Ordine in lavorazione."
                 x"0d0a""Possibile solo prezzo minore all'originale: "
                 old-prezzo
                                title tit-err
                                 icon 2
                      set errori to true
                   end-if
                end-if
                if tutto-ok
                   if rof-prz-unitario = 0
                      perform PREZZO-ZERO
                   else
                      if TotaleSiZero
                         display message 
                                 "Impossibile attribuire un prezzo"
                                        " con la causale inserita."
                                   title tit-err
                                    icon 2
                         set errori to true
                      else
LUBEXX                   if ef-cod-iva-buf = tge-cod-iva-omag
                            move spaces to ef-cod-iva-buf
                            perform RECUPERA-IVA
LUBEXX                   end-if
LUBEXX                end-if
                   
                      if tutto-ok
                         if chk-manuale-buf = 1
                            move prezzo-finale to como-prz-unitario
                            if como-prz-unitario = 0 |prima volta
                               move 0 to imposta-consumo
                                         imposta-cou 
                                         imposta-cobat
                                         add-piombo
                               move ef-uni-buf to como-prz-unitario
                               perform CALCOLO-IMPOSTE-FORNITORE
                               move imposta-consumo to ef-cons-buf
                               add imposta-cou to imposta-cobat
                               giving como-imposta
                               move como-imposta to ef-cou-buf
                               move add-piombo   to ef-add-buf
                               display ef-cons ef-cou ef-add
                            end-if
                            perform CAMPI-PREZZI
                            if como-prz-unitario <= ( imposta-consumo +
                                                      imposta-cou     +
                                                      imposta-cobat   +
                                                      add-piombo )
                               set errori to true
                               display message
                                       "Prezzo inferiore alle imposte!"
                                         title tit-err
                                          icon 2
                            end-if
                         else
                            set imf-prz-reale-utf-meno    to true
                            set imf-prz-reale-cou-meno    to true
                            set imf-prz-reale-cobat-meno  to true
                            set imf-prz-reale-pb-meno     to true
                   
                            inquire ef-data value in como-data
                            perform DATE-TO-FILE
                            move como-data to como-data-ordine
                                                          
                            move ef-uni-buf to como-prz-unitario
                            move rof-prg-chiave to prg-chiave
                            read progmag no lock 
                                invalid continue 
                            end-read
                            perform CALCOLO-IMPOSTE-FORNITORE
                            if como-prz-unitario <= ( imposta-consumo +
                                                      imposta-cou     +
                                                      imposta-cobat   +
                                                      add-piombo )
                               set errori to true
                               display message
                                       "Prezzo inferiore alle imposte!"
                                         title tit-err
                                          icon 2
                            else
                               perform CALCOLA-IMPONIBILE
                               perform LABEL-PRZ-FINALE
                               move como-rof-prz-unitario  to saveprezzo
                               move imposta-consumo to ef-cons-buf
                               compute como-imposta = imposta-cobat + 
                                                      imposta-cou
                               move como-imposta to ef-cou-buf
                               move add-piombo   to ef-add-buf
                            end-if
                         end-if
                      end-if   
                   end-if
                end-if

                if errori
                   move 78-ID-ef-uni     to control-id
                else
                   if chk-manuale-buf = 1
                      inquire ef-uni, value in como-prz-unitario
                      if como-prz-unitario = 0
                         move 78-ID-ef-cod-iva to control-id
                      else   
                         move 78-ID-ef-impforn to control-id
                      end-if
                   else
                      move 78-ID-ef-cod-iva to control-id
                   end-if
                   perform DISPLAY-SCREEN
                end-if                               
                move 4 to accept-control

           when 78-ID-ef-impforn
                inquire ef-impforn, value in imf-codice
                move imf-codice to rlis-tipo-tratt-imposte
                read impforn no lock 
                     invalid 
                     move spaces to imf-descrizione
                     set errori to true
                     display message "Tipologia imposte obbligatoria"
                               title tit-err
                                icon 2
                end-read
                if imf-codice not = hid-rof-imf-codice
                   |RICALCOLO TUTTO
                   move 0 to chk-manuale-buf |TEMPORANEO PER FARE IL CALCOLO
                   move imf-codice to hid-rof-imf-codice
                end-if
                move imf-descrizione to lab-impforn-buf
                display lab-impforn
                perform CAMPI-PREZZI
                move 1 to chk-manuale-buf
                perform DISPLAY-SCREEN
                if tutto-ok 
                   move 78-ID-ef-sconto-1 to control-id
                end-if
                move 4 to accept-control
                move pgm-name to ProgrammaInUso

           when 78-ID-ef-sconto-1
                perform CAMPI-PREZZI
                perform DISPLAY-SCREEN     
                move 78-ID-ef-sconto-2 to control-id
                move 4 to accept-control

           when 78-ID-ef-sconto-2
                perform CAMPI-PREZZI
                perform DISPLAY-SCREEN
                move 78-ID-ef-sconto-3 to control-id
                move 4 to accept-control

           when 78-ID-ef-sconto-3
                perform CAMPI-PREZZI
                perform DISPLAY-SCREEN
                move 78-ID-ef-sconto-4 to control-id
                move 4 to accept-control

           when 78-ID-ef-sconto-4
                perform CAMPI-PREZZI
                perform DISPLAY-SCREEN
                move 78-ID-ef-sconto-5 to control-id
                move 4 to accept-control

           when 78-ID-ef-sconto-5
                if chk-manuale-buf = 0
                   inquire ef-sconto-5,  value rlis-sconto-5
                end-if
                perform CAMPI-PREZZI
                perform DISPLAY-SCREEN
                if chk-manuale-buf = 1
                   move 78-ID-ef-cons to control-id
                else
                   move 78-ID-ef-cod-iva to control-id
                end-if
                move 4 to accept-control

           when 78-ID-ef-cons
                perform CAMPI-PREZZI
                if como-prz-unitario <= ( imposta-consumo +
                                          imposta-cou     +
                                          imposta-cobat   +
                                          add-piombo )
                   set errori to true
                   display message "Prezzo inferiore alle imposte!"
                             title tit-err
                              icon 2
                else
                   perform DISPLAY-SCREEN
                   move 78-ID-ef-cou to control-id
                end-if
                move 4 to accept-control

           when 78-ID-ef-cou
                perform CAMPI-PREZZI
                if como-prz-unitario <= ( imposta-consumo +
                                          imposta-cou     +
                                          imposta-cobat   +
                                          add-piombo )
                   set errori to true
                   display message "Prezzo inferiore alle imposte!"
                             title tit-err
                              icon 2
                else
                   perform DISPLAY-SCREEN
                   move 78-ID-ef-add to control-id
                end-if
                move 4 to accept-control

           when 78-ID-ef-add
                perform CAMPI-PREZZI
                if como-prz-unitario <= ( imposta-consumo +
                                          imposta-cou     +
                                          imposta-cobat   +
                                          add-piombo )
                   set errori to true
                   display message "Prezzo inferiore alle imposte!"
                             title tit-err
                              icon 2
                else
                   perform DISPLAY-SCREEN
                   move 78-ID-ef-costi-agg to control-id
                end-if
                move 4 to accept-control

           when 78-ID-ef-costi-agg
                perform CAMPI-PREZZI
                perform DISPLAY-SCREEN
                move 78-ID-ef-cod-iva to control-id
                move 4 to accept-control
                

      *****     when 78-ID-ef-costi-agg
      *****          inquire ef-costi-agg, value in ef-costi-agg-buf
      *****          if errori 
      *****             continue
      *****          else
      *****             move 78-ID-ef-cod-iva to control-id
      *****             move 4 to accept-control
      *****             perform CALCOLA-IMPONIBILE
      *****             perform LABEL-PRZ-FINALE
      *****          end-if

           |78-ID-ef-imp è l'ID del control ef-imp
      *****     when 78-ID-ef-imp
      *****          inquire ef-imp, value in ef-imp-buf
      *****          |GIA' AL NETTO DEGLI SCONTI
      *****          if rof-prz-unitario  < 
      *****           ( rof-imp-consumo   + 
      *****             rof-imp-cou-cobat + 
      *****             rof-add-piombo    + 
      *****             rof-costi-aggiuntivi )
      *****             set errori to true
      *****             display message"Imposte e costi superiori al prezzo."
      *****                     x"0d0a""Rettificare i valori"
      *****                       title tit-err
      *****                        icon 2
      *****          end-if
      *****          if errori
      *****             move 78-ID-ef-uni to control-id
      *****             move 4 to accept-control
      *****          else
      *****             move 78-ID-ef-cod-iva to control-id
      *****             move 4 to accept-control
      *****          end-if

      *****     when 78-ID-ef-man
      *****          inquire ef-man, value in rof-manuale
      *****          if rof-manuale not = 0
      *****             perform AZZERA-IMPORTI
      *****             move rof-manuale to prezzo-finale 
      *****                                 ef-uni-buf
      *****                                 ef-imp-buf
      *****          end-if
      *****          perform DISPLAY-SCREEN
      *****          move 78-ID-ef-cod-iva to control-id
      *****          move 4 to accept-control

           when 78-ID-ef-cod-iva
                inquire ef-cod-iva, value in ef-cod-iva-buf  
                move spaces to lab-iva-buf
                if ef-cod-iva-buf not = spaces
                   move "IV"       to tbliv-codice1
                   move ef-cod-iva-buf to tbliv-codice2
                   read tivaese
                        invalid
                           set errori to true        
                           display message box "Codice IVA NON valido"
                                   title = tit-err
                                   icon mb-warning-icon
                           move 78-ID-ef-cod-iva to control-id
                    not invalid 
      *                  if tbliv-percentuale not = 0
      *                     set errori to true        
      *                     display message box "Codice IVA NON valido "
      *                                         "in quanto non esente"
      *                             title = tit-err
      *                             icon mb-warning-icon
      *                     move 78-ID-ef-cod-iva to control-id
      *                  else                   
                           perform MOVE-DESCR-IVA-2
      *                  end-if
                   end-read
                end-if
                display lab-iva
                if tutto-ok
                   if key-status = 13
                      perform ENTRY-TO-ROW
                      perform PULISCI-CAMPI-LABELS
                   else
                      move 78-id-ef-art  to control-id
                      move 4 to accept-control
                   end-if
                end-if
           end-evaluate.
      *
           
      ***---
       UN-SOLO-ARTICOLO-SU-PROGMAG.
           |Se c'è un solo rec. su progmag con quel codice articolo
           |simulo la scelta di progmag da zoom con il rec. in linea
           move GiacenzaKey   to prg-chiave.
           read progmag no  lock invalid continue end-read.
           move prg-cod-articolo to ef-art-buf.

           display ef-art.
           set CheckAfterZoom to true.
           move 78-ID-ef-art  to control-id.
           perform CONTROLLO.
           set CheckAfterZoom to false.

      ***---
       MOVE-DESCR-IVA.
           initialize lab-ese-iva-buf.
           inspect tbliv-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tbliv-descrizione1 delimited by low-value
                   " "                delimited by size
                   tbliv-descrizione2 delimited by size
                   into lab-ese-iva-buf
           end-string.

      ***---
       MOVE-DESCR-IVA-2.
           initialize lab-iva-buf.
           inspect tbliv-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tbliv-descrizione1 delimited by low-value
                   " "                delimited by size
                   tbliv-descrizione2 delimited by size
                   into lab-iva-buf
           end-string.

      ***---
       MOVE-DESCR-ESE-IVA.
           initialize lab-iva-buf.
           inspect tbliv-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tbliv-descrizione1 delimited by low-value
                   " "                delimited by size
                   tbliv-descrizione2 delimited by size
                   into lab-ese-iva-buf
           end-string.

      ***---
       MOVE-DESCR-PAG.
           initialize lab-pag-buf.
           inspect tblpa-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tblpa-descrizione1 delimited by low-value
                   " "                delimited by size
                   tblpa-descrizione2 delimited by size
                   into lab-pag-buf
           end-string.

      ***---
       INIT-OLD-REC.
           initialize |old-rof-rec  
                      old-tof-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           set tof-manuale      to true
           set tof-inserito     to true
           set tof-inevaso      to true
           set tof-chiusura-man to true

           move data-oggi to tof-data-ordine.
      *                       tof-data-passaggio-ordine.


      ***---
       SELEZIONA-ALFA.
           set tutto-ok to true.  
           move spaces to lab-cli-buf.
           move spaces to lab-ind-cli-buf.
           move spaces to lab-loc-cli-buf.
      * LA SECONDA CHIAVE E' ALFANUMERICA E LA VALUE VARIABLE CONTIENE
      * ANCORA GLI ZERI DAVANTI CHE ELIMINO
           inspect ef-cli-buf replacing leading ZERO by SPACES.
           move    ef-cli-buf to cli-ragsoc-1 of clienti.
      
      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           set cli-tipo-f of clienti to true.
           call "C$JUSTIFY" using cli-ragsoc-1 of clienti, "L".
      
           start clienti key >= cli-k1 of clienti
              invalid 
                 continue
              not invalid 
                 read clienti next no lock
           end-start.
      

           move "clienti-alfa-CF"    to como-file.

           call "zoom-gt"  using   como-file, cli-rec of clienti
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = 0
              if cli-disattivo of clienti or cli-bloccato of clienti
                 if not tmp
                    display message "Fornitore NON attivo"
                              title tit-err
                               icon 2
                    set errori to true
                    move spaces to lab-cli-buf
                    move spaces to lab-ind-cli-buf
                    move spaces to lab-loc-cli-buf
                 end-if   
              end-if
              if tutto-ok
                 if pgm-name = "gordfornvar"
                    perform VALUTA-CAMBIO-TIPOLOGIA
                 end-if
                 move cli-codice of clienti    to codice-ed
                 move codice-ed     to ef-cli-buf   
                 call "C$JUSTIFY" using ef-cli-buf, "L"
                 display ef-cli   
                 move cli-ragsoc-1 of clienti  to lab-cli-buf
                 move cli-indirizzo of clienti to lab-ind-cli-buf
                 move cli-localita of clienti  to lab-loc-cli-buf
              end-if
           else
              set errori         to true
              move 78-ID-ef-cli  to CONTROL-ID
           end-if.

      ***---
       SELEZIONA-NUMERICO.
           set tutto-ok to true.
           move spaces to lab-cli-buf.
           move spaces to lab-ind-cli-buf.
           move spaces to lab-loc-cli-buf.

           inquire ef-cli, value in cli-codice of clienti.
           if cli-codice of clienti not > 0
              set errori to true
              move 78-ID-ef-cli to CONTROL-ID
              display message box msg-codice-obbligatorio
                      title tit-err
                      icon  MB-WARNING-ICON
           else          
              set cli-tipo-f of clienti to true
              read clienti no lock
                 invalid
                    display message box "Codice Fornitore NON valido"
                            title = tit-err
                            icon MB-WARNING-ICON
                    set errori to true
                    move 78-ID-ef-cli to CONTROL-ID
                 not invalid
                    if cli-disattivo of clienti or 
                       cli-bloccato of clienti
                       if not tmp
                          display message "Fornitore NON attivo"
                                  title tit-err
                                   icon 2
                          set errori to true
                          move spaces to lab-cli-buf
                          move spaces to lab-ind-cli-buf
                          move spaces to lab-loc-cli-buf
                       end-if
                    end-if
                    if tutto-ok
                       if pgm-name = "gordfornvar"
                          perform VALUTA-CAMBIO-TIPOLOGIA
                       end-if
                       move cli-ragsoc-1 of clienti  to lab-cli-buf
                       move cli-indirizzo of clienti to lab-ind-cli-buf
                       move cli-localita of clienti  to lab-loc-cli-buf
                    end-if
                    if tutto-ok
                       move cli-ragsoc-1 of clienti  to lab-cli-buf
                       move cli-indirizzo of clienti to lab-ind-cli-buf
                       move cli-localita of clienti  to lab-loc-cli-buf
                    end-if
              end-read
           end-if .

      ***---
       SELEZIONA-DESTINO-NUMERICO.
      *     move spaces to lab-des-buf.
           move spaces to lab-ind-dest-buf.
           move spaces to lab-loc-dest-buf.
           inquire ef-cli, value in desf-codice.
           inquire ef-dest, value in desf-prog.
      *     move ef-dest-buf to desf-prog.
           if desf-prog not > 0
              set errori to true
              move 78-ID-ef-dest to control-id
              display message box msg-codice-obbligatorio
                      title tit-err
                      icon  mb-warning-icon
           else          
              inquire ef-cli, value in desf-codice
              read destinif no lock
                 invalid
                    display message "Progressivo destino NON valido"
                             title tit-err
                             icon mb-warning-icon
                    set errori to true
                    move 78-ID-ef-dest to control-id
                 not invalid
                    if desf-disattivo or desf-bloccato
                       set errori to true
                       display message "Destino NON attivo"
                               title tit-err
                               icon 2
                       move spaces to lab-dest-buf
                       move spaces to lab-ind-dest-buf
                       move spaces to lab-loc-dest-buf
                    end-if
                    if tutto-ok
                       move desf-ragsoc-1  to lab-dest-buf
                       move desf-indirizzo to lab-ind-dest-buf
                       move desf-localita  to lab-loc-dest-buf
                    end-if
              end-read
           end-if.
      
      ***---
       SELEZIONA-DESTINO-ALFA.
      * LA SECONDA CHIAVE E' ALFANUMERICA E LA VALUE VARIABLE CONTIENE
      * ANCORA GLI ZERI DAVANTI CHE ELIMINO
           move    ef-cli-buf to desf-codice convert.
           inspect ef-dest-buf replacing leading ZERO by SPACES.
           move    ef-dest-buf to desf-ragsoc-1.
      
      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           call "C$JUSTIFY" using desf-ragsoc-1, "L".
      
           start destinif     key >= desf-K2
              invalid     
                 continue
              not invalid 
                 read destinif next no lock
           end-start.   
      *     set cli-tipo-f to true.
           move    ef-cli-buf to desf-codice convert.
      
           move "destinif-forn" to como-file.
           call "zoom-gt" using   como-file, desf-rec
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = 0
              if desf-disattivo or desf-bloccato
                 set errori to true
                 display message "Destino NON attivo"
                         title tit-err
                         icon 2
                 move spaces to lab-dest-buf
                 move spaces to lab-ind-dest-buf
                 move spaces to lab-loc-dest-buf
              end-if
              if tutto-ok
                 move desf-prog      to codice-ed
                 move codice-ed      to ef-dest-buf   
                 call "C$JUSTIFY" using ef-dest-buf, "L"
                 display ef-dest   
                 move desf-ragsoc-1  to lab-dest-buf
                 move desf-indirizzo to lab-ind-dest-buf
                 move desf-localita  to lab-loc-dest-buf
              end-if
           else
              set errori         to true
              move 78-ID-ef-dest  to CONTROL-ID
           end-if.
      
      
      ****---
      * TROVA-DESTINO.
      *     set trovato to false.
      *     inquire ef-cli, value in ef-cli-buf.
      *     move ef-cli-buf to des-codice convert.
      *     move ef-cli-buf to cli-codice convert.
      *
      *     move low-value  to des-prog.
      *     start destini  key is >= des-chiave
      *           invalid  continue
      *       not invalid  read destini next
      *           if des-codice = cli-codice 
      *              set trovato to true 
      *           else 
      *              move ef-cli-buf to des-codice convert
      *              move ef-des-buf to des-prog   convert
      *           end-if
      *     end-start.

      ***---
       CHECK-PAGE-1.
           set tutto-ok to true.
           perform  varying control-id from 78-ID-ef-data by 1
                      until control-id    > 78-ID-ef-pag
              perform CONTROLLO
              if errori 
                 exit perform 
              end-if
           end-perform.
      *     if tutto-ok
      *        perform  varying control-id from 78-ID-ef-data-bolla by 1
      *                   until control-id    > 78-ID-ef-num-bolla
      *           perform CONTROLLO
      *           if errori 
      *              exit perform 
      *           end-if
      *        end-perform
      *     end-if.



      ***---
       VALUTA-CAMBIO-TIPOLOGIA.
      *     if e-gui = 1
      *        display message "IMPOSSIBILE" |assortimento
      *     else
      *        move cli-tipo to tcl-codice
      *        read ttipocli no lock
      *             invalid  move spaces to tcl-tipologia-tratt-imposte
      *        end-read
      *****        if rlis-tipo-tratt-imposte not = TrattamentoInUso
LUBEXX*****           display message "ATTENZIONE!!"
LUBEXX*****                    x"0d0a""E' stato cambiata la tipologia "
LUBEXX*****                           "trattamento imposte del Fornitore."
LUBEXX*****                    x"0d0a""Annullare e ricreare l'ordine"
LUBEXX*****                           " col nuovo Fornitore!!!"
LUBEXX*****                     title tit-err
LUBEXX*****                      icon 2
LUBEXX*****           set errori to true
LUBEXX*****
LUBEXX*****                 move tcl-tipologia-tratt-imposte to TrattamentoInUso
LUBEXX*****
LUBEXX*****                 if rlis-gdo modify ef-sconto,     read-only
LUBEXX*****                 else            modify ef-sconto, not read-only
LUBEXX*****                 end-if

LUBEXX*****                 display message "ATTENZIONE!!"
LUBEXX*****                          x"0d0a""E' stato cambiata la "
LUBEXX*****                                 "tipologia di Fornitore."
LUBEXX*****                          x"0d0a""Occorre ripassare TUTTE le "
LUBEXX*****                                 "righe per prezzi e imposte"
LUBEXX*****                           title tit-err
LUBEXX*****                            icon 2
LUBEXX*****
LUBEXX*****                 move tcl-tipologia-tratt-imposte to TrattamentoInUso
LUBEXX*****
LUBEXX*****                 if rlis-gdo modify ef-sconto,     read-only
LUBEXX*****                 else            modify ef-sconto, not read-only
LUBEXX*****                 end-if
LUBEXX*****
LUBEXX*****                 set CambiatoTrattamento to true

      *****        end-if.
      *     end-if.

      ****---
      * SECCA-TMP-ASSORCLI.
      *     if link-path-tmp-assorcli not = spaces
      *        close tmp-assorcli
      *        call "C$DELETE" using link-path-tmp-assorcli, "I"
      *        move spaces to link-path-tmp-assorcli
      *     end-if.

      ***---
       STATUS-BAR-MSG.
           if pgm-name = "gordforn"
              modify form1-st-1-handle, 
                     panel-index  3,
                     panel-text  "INSERIMENTO"
           else
              evaluate true
              when StatusIns
              when StatusModifica
                   modify form1-st-1-handle, 
                          panel-index  3,
                          panel-text  "MODIFICA"
              when StatusVisua
                   modify form1-st-1-handle, 
                          panel-index  3,
                          panel-text  "VISUALIZZAZIONE"
                   move 0 to StatusHelp
                   perform STATUS-HELP
              when other
                   modify form1-st-1-handle, 
                          panel-index  2,
                          panel-text   spaces
              end-evaluate
           end-if.

      ***---
       STATUS-HELP.
           if StatusHelp = 1
              modify Form1-St-1-Handle, 
                     panel-index = 2, 
                     panel-text "F8 HELP record presenti"
              move BitmapZoomEnabled to BitmapNumZoom
           else
              modify Form1-St-1-Handle, 
                     panel-index = 2, 
                     panel-text spaces
              move BitmapZoomDisabled to BitmapNumZoom
           end-if.

           move StatusHelp    to e-cerca.
           modify tool-cerca, enabled = e-cerca.
           modify tool-cerca, bitmap-number = BitmapNumZoom.

      ***---
       ORDINI-BEFORE-PROGRAM.
           set tmp                 to false.
           set CambiatoTrattamento to false.
           set CambiatoMagazzino   to false.
           set StoSalvando         to false.

LUBEXX*     initialize sw-controlla-scostamento.
LUBEXX*     accept sw-controlla-scostamento 
LUBEXX*            from environment "CONTROLLO_SCOSTAMENTO".
      *
      *     initialize FlagAssortimento.
      *     accept FlagAssortimento from environment "ASSORTIMENTO"
      *            on exception
      *               set NoAssortimento to true
      *        not on exception
      *               if FlagAssortimento = spaces
      *                  set NoAssortimento to true
      *               end-if
      *     end-accept.
      *     if SiAssortimento move 0 to e-gui
      *     else              move 1 to e-gui
      *     end-if.
           move 0       to v-bolla v-dett.
           set tutto-ok to true.
           open input tparamge.
           move spaces to tge-chiave.
           read tparamge no lock 
              invalid 
                 continue 
           end-read.
           move tge-anno to como-anno.
           close tparamge.

           move tge-cod-iva-omag   to iva-omaggio.

           open input tcaumag.
           move tge-causale-ordini-std to tca-codice.
           read  tcaumag no lock 
              invalid 
                 set errori to true 
           end-read.

           if tutto-ok
              move tca-cod-magaz to mag-codice, StoreMagazzino
              open input tmagaz
              read tmagaz no lock 
                 invalid 
                    set errori to true 
              end-read
              close tmagaz
              if errori
                 display message box 
                          "Magazzino per Bolla ordinaria mancante!!!"
                  x"0d0da""Impossibile procedere con il programma."
                         title = tit-err
                         icon MB-ERror-ICON
              end-if
           else
              display message box 
                       "Causale ordinaria di magazzino mancante!!!"
               x"0d0da""Impossibile procedere con il programma."
                      title = tit-err
                      icon MB-ERROR-ICON
           end-if.

           close tcaumag.

           if tutto-ok
              open input timposte   

              accept imp-data from century-date

              start timposte key <= imp-chiave
                    invalid continue
                not invalid
                    read timposte previous
              end-start

              if errori
                 display message box 
                          "Record imposte mancante!!!"
                  x"0d0da""Impossibile procedere con il programma."
                         title = tit-err
                         icon MB-ERROR-ICON
              end-if
              close timposte
           end-if.

           if tutto-ok
              initialize path-tmp-nordforn
              accept path-tmp-nordforn from environment "TEMP"

              inspect path-tmp-nordforn 
                                   replacing trailing space by low-value

              accept como-data  from century-date
              accept como-ora   from time

              string path-tmp-nordforn   delimited by low-value
                     "\tmp-nordforn_"    delimited by size
                     como-data           delimited by size
                     "_"                 delimited by size
                     como-ora            delimited by size
                     into path-tmp-nordforn

              inspect path-tmp-nordforn 
                                   replacing trailing low-value by space

              open output tmp-nordforn
              close tmp-nordforn
           end-if

           copy resource "conferma.bmp".

           if errori
              perform ORDINI-AFTER-PROGRAM
              SET LK-BL-CANCELLAZIONE TO TRUE 
              MOVE COMO-PROG-ID       TO LK-BL-PROG-ID
              CALL "BLOCKPGM"  USING LK-BLOCKPGM
              goback 
           else
              call "W$BITMAP" using wbitmap-load "conferma.bmp"
                             giving conferma-bmp
           end-if.

      ***---
       ORDINI-AFTER-PROGRAM.
           call "W$BITMAP" using wbitmap-destroy, conferma-bmp.
      *     perform SECCA-TMP-ASSORCLI.

           if HoSalvato
              call   "tprev-elab" using user-codi
              cancel "tprev-elab"
           end-if.

      ***---
       ORDINI-AFTER-END-ACCEPT.
           if NonCambiareTab
              set NonCambiareTab to false
              move 1 to screen1-ta-1-tab-value event-data-1
              perform SCREEN1-TA-1-TABCHANGE
              move store-id to control-id
              move 4 to accept-control
              set ControllaCampi to true
           end-if.

      *****     if ArticoloSetFocus
      *****        inquire screen1-ta-1, value in screen1-ta-1-tab-value
      *****        if screen1-ta-1-tab-value = 2
      *****           move 78-ID-ef-art    to control-id
      *****           move 4               to accept-control
      *****        end-if
      *****        if not FromSpostamento
      *****           set ControllaCampi   to true
      *****        end-if
      *****        set ArticoloSetFocus to false
      *****        initialize key-status
      *****     end-if.

      ***---
       SETTA-RIGA.
           move save-riga to event-data-2 riga.
           move 4         to event-data-1 colonna.

      *     if tof-manuale
              modify form1-gd-1, cursor-y = riga
              if riga > 1 |Succede in ingresso qundo ho solo articoli da conf
                 perform SPOSTAMENTO
              end-if.
      *     else
      *         inquire form1-gd-2(event-data-2, 1), 
      *                 hidden-data in hid-rof-rec
      *         move hid-giacenza  to prg-giacenza
      *         move hid-impegnato to prg-impegnato
      *         move hid-ordinato  to prg-ordinato-1
      *         perform LABEL-VALORI
      *
      *         move 1000 to control-id
      *         move    4 to accept-control
      *         modify form1-gd-2, cursor-y = event-data-2,
      *                            cursor-x = event-data-1
      *         perform COLORE-2
      *         set MousePressed     to false
      *         set ControllaCampi   to false
      ******         set ArticoloSetFocus to false
      *         perform DISPLAY-SCREEN
      *     end-if.

      ***---
       CERCA.
           evaluate control-id
           when 78-ID-ef-cau
                perform RIEMPI-TMP-CAUSALI
                move "zoom-tcaumag"   to como-file
                inquire ef-cau, value in zoom-tca-codice
                call "zoom-gt"  using como-file, zoom-tca-rec                                
                               giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move zoom-tca-codice      to ef-cau-buf
                   move zoom-tca-descrizione to lab-cau-buf
                   display ef-cau lab-cau
                end-if
                close       zoom-tcaumag
                delete file zoom-tcaumag

           when 78-ID-ef-cli
                set cli-tipo-f of clienti  to true
                inquire ef-cli, value in cli-codice of clienti
                set cli-tipo-f of clienti  to true
                move "clienti-all"     to como-file
                call "zoom-gt"      using como-file, cli-rec of clienti
                                   giving stato-zoom
                cancel "zoom-gt"

                if stato-zoom = 0
                   move cli-codice of clienti     to ef-cli-buf
                   inspect ef-cli-buf replacing leading x"30" by x"20"
                   call "C$JUSTIFY" using ef-cli-buf, "L"
                   move cli-ragsoc-1 of clienti   to lab-cli-buf

                   move cli-indirizzo of clienti  to lab-ind-cli-buf
                   move cli-localita of clienti   to lab-loc-cli-buf
                   display ef-cli lab-cli lab-ind-cli lab-loc-cli
                end-if

           when 78-ID-ef-dest
                inquire ef-cli, value in desf-codice
                inquire ef-dest, value in desf-prog
                move "destinif-forn"  to como-file
                call "zoom-gt"      using como-file, desf-rec
                                   giving stato-zoom
                cancel "zoom-gt"

                if stato-zoom = 0
                   move desf-prog  to ef-dest-buf
                   inspect ef-dest-buf replacing leading x"30" by x"20"
                   call "C$JUSTIFY" using ef-dest-buf, "L"
                   move desf-ragsoc-1    to lab-dest-buf
                   move desf-indirizzo   to lab-ind-dest-buf
                   move desf-localita    to lab-loc-dest-buf
                   display ef-dest lab-dest lab-ind-dest lab-loc-dest
                end-if

           when 78-ID-ef-cliente
                set cli-tipo-c of clienti1  to true
                inquire ef-cliente, value in cli-codice of clienti1
                set cli-tipo-c of clienti  to true
                move "clienti-all"  to como-file
                call "zoom-gt"      using como-file, cli-rec of clienti1
                                   giving stato-zoom
                cancel "zoom-gt"

                if stato-zoom = 0
                   move cli-codice of clienti1     to ef-cliente-buf
                   move cli-ragsoc-1 of clienti1   to lab-cliente-buf
                   display ef-cliente lab-cliente
                end-if

           when 78-ID-ef-des-cli
                inquire ef-cliente, value in des-codice
                inquire ef-des-cli, value in des-prog
                move "clienti-des"    to como-file
                call "zoom-gt"  using como-file, des-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move des-prog       to ef-des-cli-buf
                   move des-ragsoc-1   to lab-des-cli-buf
                   move des-localita   to lab-des-loca-buf
                   display ef-des-cli lab-des-cli lab-des-loca
                end-if
      *
      *     when 78-ID-ef-vet
      *          move "tvettori"  to como-file
      *          inquire ef-vet, value in vet-codice
      *          call "zoom-gt"   using como-file, vet-rec
      *                          giving stato-zoom
      *          end-call
      *          cancel "zoom-gt"
      *          if stato-zoom = 0
      *             move vet-codice      to ef-vet-buf
      *             move vet-descrizione to lab-vet-buf
      *             display ef-vet lab-vet
      *          end-if
      *
      *     when 78-ID-ef-age
      *          move "agenti"    to como-file
      *          inquire ef-age,  value in age-codice
      *          call "zoom-gt"   using como-file, age-rec
      *                          giving stato-zoom
      *          end-call
      *          cancel "zoom-gt"
      *          if stato-zoom = 0
      *             move age-codice   to ef-age-buf
      *             move age-ragsoc-1 to lab-age-buf
      *             display ef-age lab-age
      *          end-if

      *****     when 78-ID-ef-cod-iva
      *****          move "tivaese" to como-file
      *****          move "IV"          to tbliv-codice1
      *****          inquire ef-cod-iva,  value in tbliv-codice2
      *****          call "zoom-gt"   using como-file, record-tbliv
      *****                          giving stato-zoom
      *****          end-call
      *****          cancel "zoom-gt"
      *****          if stato-zoom = 0
      *****             move tbliv-codice2   to ef-cod-iva-buf
      *****             perform MOVE-DESCR-IVA
      *****             display ef-cod-iva lab-iva
      *****          end-if

           when 78-ID-ef-ESE-iva
                move "tivaese-ese" to como-file
                move "IV"          to tbliv-codice1
                inquire ef-ese-iva,  value in tbliv-codice2
                call "zoom-gt"   using como-file, record-tbliv
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move tbliv-codice2   to ef-ese-iva-buf
                   perform MOVE-DESCR-ESE-IVA
                   display ef-ese-iva lab-ese-iva
                end-if

           when 78-ID-ef-pag
                move "tcodpag"   to como-file
                move "PA"        to tblpa-codice1
                inquire ef-pag,  value in tblpa-codice2
                call "zoom-gt"   using como-file, record-tblpa
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move tblpa-codice2   to ef-pag-buf
                   perform MOVE-DESCR-PAG
                   display ef-pag lab-pag
                end-if

           when 78-ID-ef-art

                 inquire ef-art, value in art-codice
                 move "articoli"    to como-file
                 call "zoom-gt"  using como-file, art-rec
                                 giving stato-zoom
                 end-call
                 if stato-zoom = 0

                    cancel "zoom-gt"
                    move art-codice to ef-art-buf
                    modify ef-art value ef-art-buf
                    move art-descrizione to lab-art-buf
                    modify lab-art title lab-art-buf
                 end-if


           when 78-ID-ef-impforn
                inquire ef-impforn, value in imf-codice
                move "impforn"     to Como-File
                call   "zoom-gt" using  como-file, imf-rec
                                giving stato-zoom
                cancel "zoom-gt"
                if stato-zoom = 0
                   move imf-codice        to ef-impforn-buf
                   move imf-descrizione   to lab-impforn-buf
                   display ef-impforn lab-impforn
                end-if

      *****     when 78-id-ef-imb-ord
      *****          inquire ef-imb-ord value imq-codice
      *****          move "timbalqta"   to como-file
      *****          call "zoom-gt"   using como-file, imq-rec
      *****                          giving stato-zoom
      *****          end-call
      *****          cancel "zoom-gt"
      *****          if stato-zoom = 0
      *****             move tblpa-codice2   to ef-pag-buf
      *****             perform MOVE-DESCR-PAG
      *****             display ef-pag lab-pag
      *****           
      *****             move imq-codice to ef-imb-ord-buf
      *****             modify ef-imb-ord value ef-imb-ord-buf
      *****             perform DESCR-IMBALLO
      *****             move imballo-descrizione to lab-imb-buf
      *****             modify lab-imb title lab-imb-buf
      *****          end-if

           when 78-ID-ef-promo
                perform CARICA-TMP-PROMO
                move path-tmp-tpromo-zoom   to ext-file
                move "tmp-tpromo-zoom"    to como-file
                call "zoom-gt"          using como-file, 
                                              tmp-tpr-z-rec
                                       giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move tmp-tpr-z-codice       to ef-promo-buf
                                                  tpr-codice
                   read tpromo
                      invalid
                         continue
                   end-read

                   perform DESCRIZIONE-PROMO
                   display ef-promo   lab-promo
                end-if
           end-evaluate.

      ***---
       ZOOM-SU-PROGMAG.
           move 0 to num-articoli.
           inquire ef-art, value in art-codice.
           read articoli no lock
              invalid 
                 move spaces to art-descrizione
           end-read.
           move "articoli"    to como-file.
           call "zoom-gt"  using como-file, art-rec
                          giving stato-zoom
           end-call.
           cancel "zoom-gt".
           move art-codice to ef-art-buf.
           display ef-art.
           if stato-zoom = 0
              set filtro-articoli to true
              perform COMPONI-TMP
              if trovato
                 if num-articoli > 1                  
                    perform POSITION-ON-MAJOR-GIACENZA
LUBEXX*****              perform POSITION-ON-FIRST-RECORD
                    move path-tmp-progmag-zoom to ext-file
                    move "tmp-progmag-zoom"    to como-file
                    call "zoom-gt"          using como-file, 
                                                  tmp-prg-z-rec
                                           giving stato-zoom
                    end-call
                    cancel "zoom-gt"
                    if stato-zoom = 0 
                       move tmp-prg-z-chiave to prg-chiave
                       read progmag no lock invalid continue end-read
                       move prg-cod-articolo to ef-art-buf
                       display ef-art
                       set CheckAfterZoom to true
                       perform CONTROLLO
                       set CheckAfterZoom to false
                       if tutto-ok
                          move 78-ID-ef-qta  to control-id
                          move 4             to accept-control
                       end-if
                       move 0 to key-status
                    end-if
                 else
                    perform UN-SOLO-ARTICOLO-SU-PROGMAG
                 end-if
              else
                 display message "Articolo NON valido"
                           title tit-err
                            icon 2
                 set errori to true
                 move 1 to stato-zoom
              end-if
              delete file tmp-progmag-zoom
           end-if.

      ***---
       ZOOM-SU-PROGMAG-ARTICOLO.
           set filtro-articoli to true.
           perform COMPONI-TMP.
           if trovato
              perform POSITION-ON-MAJOR-GIACENZA
              move path-tmp-progmag-zoom to ext-file
              move "tmp-progmag-zoom"    to como-file
              call "zoom-gt"          using como-file, tmp-prg-z-rec
                                     giving stato-zoom
              end-call
              cancel "zoom-gt"
           else
              move 1 to stato-zoom
           end-if.
           delete file tmp-progmag-zoom.
           if stato-zoom = 0
              move tmp-prg-z-chiave to prg-chiave
              read progmag no lock invalid continue end-read
              move prg-cod-articolo to ef-art-buf
              display ef-art
              set CheckAfterZoom to true
              perform CONTROLLO
              set CheckAfterZoom to false
           else
              set errori to true
              move 4     to accept-control
           end-if.

      ***---
       COMPONI-TMP.
           move 0 to num-articoli.
           call "W$MOUSE" using set-mouse-shape, wait-pointer.
           set record-ok to true.
           inquire ef-art, value in SaveArticolo.
      *     tca-cod-magaz.
           accept  path-tmp-progmag-zoom from environment "PATH_ST".
           accept  como-data             from century-date.
           accept  como-ora              from time.
           inspect path-tmp-progmag-zoom replacing trailing
                                         spaces by low-value.
           string path-tmp-progmag-zoom  delimited by low-value
                  "tmp-progmag-zoom"     delimited by size
                  "_"                    delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  into path-tmp-progmag-zoom
           end-string.

           set trovato to false.
           open output tmp-progmag-zoom.
           if status-tmp-progmag-zoom = "00"
              initialize prg-rec
              if filtro-articoli
                 move SaveArticolo to prg-cod-articolo
              end-if
              move tca-cod-magaz to prg-cod-magazzino
              start progmag key  is >= prg-chiave
                    invalid continue 
                not invalid perform CICLO-READ
              end-start
      
              close tmp-progmag-zoom
              call "W$MOUSE" using set-mouse-shape, arrow-pointer
           end-if.

      ***---
       CICLO-READ.
           perform until 1 = 2
              read progmag next no lock at end exit perform end-read
              if filtro-articoli
                 if prg-cod-articolo not = SaveArticolo
                    exit perform
                 end-if
              end-if
              set record-ok to false
              if prg-cod-magazzino not = spaces and
                 prg-tipo-imballo  not = spaces and
                 prg-peso          not = 0      and
                 prg-cod-magazzino     = tca-cod-magaz and
                 prg-attivo
                 move prg-cod-articolo  to art-codice
                 read articoli no lock invalid continue end-read
                 if art-attivo set record-ok to true end-if
      
                 if record-ok
      
                    move prg-tipo-imballo to imb-codice
                                             imq-codice
                    read timballi  no lock invalid continue end-read
                    read timbalqta no lock invalid continue end-read
      
                    move imq-qta-imb      to imballi-ed
                    move imq-tipo         to imb-codice
                    read timballi no lock
                         invalid  initialize imb-descrizione
                    end-read
                    inspect imb-descrizione replacing trailing spaces
                                                         by low-value
                    move imq-qta-imb    to imballi-ed
                    call "C$JUSTIFY" using imballi-ed, "L"
                    initialize imballo-descrizione
                    string  imb-descrizione delimited by low-value
                            " da "          delimited by size
                            imballi-ed      delimited by spaces
                            " x "           delimited by size
                            art-udm-imballo delimited by size
                            into imballo-descrizione
                    end-string
      
                    move art-codice          to tmp-prg-z-cod-articolo
                    move art-descrizione     to tmp-prg-z-art-des
                    move tca-cod-magaz       to tmp-prg-z-cod-magazzino
                    move StoreDesMagazzino   to tmp-prg-z-mag-des
                    move prg-tipo-imballo    to tmp-prg-z-tipo-imballo
                    move imballo-descrizione to tmp-prg-z-imb-des
                    move prg-peso            to tmp-prg-z-peso
                    move prg-giacenza        to tmp-prg-z-giacenza
                    move prg-impegnato       to tmp-prg-z-impegnato
                    move prg-ordinato-1      to tmp-prg-z-ordinato
      
                    write tmp-prg-z-rec invalid continue end-write
                    set trovato to true
                    add 1 to num-articoli
                    if num-articoli = 1
                       move prg-chiave to GiacenzaKey
                    end-if
                 end-if
      
              end-if
      
           end-perform.

      ***---
       POSITION-ON-MAJOR-GIACENZA.
           open input tmp-progmag-zoom.
           move low-value to tmp-prg-z-rec.
           inquire ef-art,   value in tmp-prg-z-cod-articolo
           move tmp-prg-z-cod-articolo  to como-articolo.
           move tca-cod-magaz           to tmp-prg-z-cod-magazzino 
      
                                           como-magazzino.
           move 0      to como-giacenza.
           move spaces to como-record.
           start tmp-progmag-zoom key is >=  key-des
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-progmag-zoom next at end exit perform 
                    end-read
                    if tmp-prg-z-cod-articolo  not = como-articolo or
                       tmp-prg-z-cod-magazzino not = como-magazzino
                       exit perform
                    end-if
                    if ( tmp-prg-z-giacenza - tmp-prg-z-impegnato ) > 
                       como-giacenza or
                       como-record = spaces
                       compute como-giacenza =
                               tmp-prg-z-giacenza - tmp-prg-z-impegnato
                       move tmp-prg-z-rec      to como-record
                    end-if
                 end-perform
           end-start.
           if como-record not = spaces
              move como-record to tmp-prg-z-rec
           end-if.
           close tmp-progmag-zoom.

      ***---
       POSITION-ON-FIRST-RECORD.
           |Mi posiziono sul PRIMO record
           open input tmp-progmag-zoom.
           move low-value to tmp-prg-z-rec.
           start tmp-progmag-zoom key is >= key-des
                 invalid continue
           end-start.
           read tmp-progmag-zoom next end-read.
           close tmp-progmag-zoom.
      *
      ****---
      * ZOOM-SU-TMP-ASSORCLI.
      *     if path-tmp-assorcli not = spaces
      *        move SaveGdo    to asc-cod-gruppo-gdo
      *        move ef-cli-buf to asc-cod-cliente    with convert
      *        inquire ef-art, value in asc-cod-articolo
      *        inquire ef-des, value in ef-des-buf
      *        move path-tmp-assorcli to ext-file
      *        move "tmp-assorcli" to como-file
      *        call "zoom-gt"   using como-file, tmp-asc-rec
      *                        giving stato-zoom
      *        end-call
      *        cancel "zoom-gt"
      *        if stato-zoom = 0
      *           move tmp-asc-cod-articolo      to ef-art-buf
      *           display ef-art
      *        end-if
      *     end-if.

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
      *     if tca-si-stampa move 1 to multiplyer(2)
      *     else             move 1 to multiplyer(1)
      *     end-if.

      ********---
      ***** TESTATA-DETTAGLIO.
      *****     move tge-cliente-corrisp to cli-codice.
      *****     set cli-tipo-f to true.
      *****     read clienti no lock
      *****          invalid
      *****             set errori to true
      *****             display message "Vendita al dettaglio impossibile:"
      *****                      x"0d0a""Cliente per corrispettivi assente"
      *****                     title = tit-err
      *****                     icon mb-warning-icon
      *****             move 78-ID-ef-cli to control-id
      *****      not invalid
      *****          if pgm-name = "gordforn"
      *****             move cli-codice     to ef-cli-buf
      *****             inspect ef-cli-buf replacing leading x"30" by x"20"
      *****             call "C$JUSTIFY" using ef-cli-buf, "L"
      *****             move cli-ragsoc-1   to lab-cli-buf
      *****             move cli-indirizzo  to lab-ind-cli-buf
      *****             move cli-localita   to lab-loc-cli-buf
      *****             move spaces         to ef-des-buf
      *****             move 0              to ef-data-pass-buf
      *****             accept como-data  from century-date
      *****             perform DATE-TO-SCREEN
      *****             move como-data to ef-data-buf
      *****             display ef-data
      *****             move cli-vettore         to des-vettore
      *****             move cli-superamento-500 to des-superamento-500
      *****             move cli-prov            to des-prov
      *****             perform MOVE-DATI
      *****             move 0 to mod-k
      *****             move 1 to mod mod-campi e-man  |e-gui
LUBEXX*****             perform ABILITA-GEST-PLUS
      *****             perform DISPLAY-SCREEN
      *****             modify ef-age, read-only
      *****             modify ef-pag, read-only
      *****             modify ef-cod-iva, read-only
      *****             modify ef-vet, read-only
      *****             move 2 to event-data-1 screen1-ta-1-tab-value
      *****             perform CHANGE-TAB
      *****             perform SCREEN1-TA-1-TABCHANGE
      *****             move 78-ID-ef-art    to control-id
      *****             move 4               to accept-control
      **********             set ArticoloSetFocus to false
      *****          end-if
      *****     end-read.

      ***---
       CONTROLLA-TOTALE-MAN.
LUBEXX*****Se è attiva la causale di movimento "speciale" non
LUBEXX*****devo fare nessun controllo sul totale del documento
LUBEXX     if tca-si-speciale exit paragraph end-if.
           move 0 to Sum.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 5), cell-data in SavePrezzo
              inquire form1-gd-1(store-riga, 4), cell-data in como-qta
              compute Sum = Sum + ( SavePrezzo * como-qta )
           end-perform.
           if Sum = 0 if TotaleNoZero set errori to true end-if
           else       if TotaleSiZero set errori to true end-if
           end-if.
           if errori
              display message 
               "Salvataggio NON effettuato: Il totale del documento "
           x"0d0a""non rispetta l'indicazione della causale utilizzata "
                        title tit-err
                         icon 2
           end-if.

      ***---
       CONTA-CODICI-IVA-MAN.
           inquire form1-gd-1, last-row in tot-righe.
           move 0 to num-codici-iva.
           move 1 to idx.
           move spaces to cod-iva(1).
           move spaces to cod-iva(2).
           move spaces to cod-iva(3).
           perform varying riga from 2 by 1
                     until riga > tot-righe
              set trovato to false
              inquire form1-gd-1(riga, 11),
                      cell-data in col-iva    
              perform varying idx from 1 by 1
                        until idx > 3
                 if col-iva = cod-iva(idx)
                    set trovato to true
                    exit perform
                 end-if
              end-perform
              if not trovato
                 add 1 to num-codici-iva
                 perform varying idx from 1 by 1
                           until idx > 3
                    if cod-iva(idx) = spaces
                       move col-iva to cod-iva(idx)
                       exit perform
                    end-if
                 end-perform
              end-if
              if num-codici-iva > 3 
                 set errori to true
                 exit perform 
              end-if
           end-perform.

      ***---
       CONTROLLA-RIGHE.
           |Viene richiamato per controllare che TUTTE le righe
           |abbiano il nuovo magazzino (in caso la NUOVA causale) ne
           |apporti il cambiamento che di conseguenza influisce sui
           |progressivi di magazzino ossia sulle righe (MODIFICA)
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 1),
                                            hidden-data = hid-rof-rec-1
              inquire form1-gd-1(store-riga, 2),
                                            hidden-data = hid-rof-rec-2
              inquire form1-gd-1(store-riga, 3),
                                            hidden-data = hid-rof-rec-3
              if hid-rof-cod-magazzino not = tca-cod-magaz
                 move store-riga to save-riga
                 subtract 1 from store-riga
                 move store-riga to riga-ed
                 perform SETTA-RIGA
                 display message "Cambio di magazzino!"
                          X"0d0a""Ricaricare la riga: " riga-ed, "."
                           title tit-err
                            icon 2
                 set errori to true
                 exit perform
              end-if
           end-perform.

      ***---
       RIEMPI-TMP-CAUSALI.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-zoom-tcaumag.
           accept  path-zoom-tcaumag from environment "PATH_ST".
           inspect path-zoom-tcaumag replacing trailing spaces 
                                                     by low-value.
           string  path-zoom-tcaumag delimited low-value
                   "zoom-tcaumag"    delimited size
                   "_"               delimited size
                   como-data         delimited size
                   "_"               delimited size
                   como-ora          delimited size
                   ".tmp"            delimited size
                   into path-zoom-tcaumag
           end-string.
           open  output zoom-tcaumag.
           move  low-value to tca-rec.
           start tcaumag key is >= tca-chiave
                 invalid continue
             not invalid
                 open input tmagaz
                 perform until 1 = 2
                    read tcaumag next no lock
                         at end exit perform
                    end-read
                    if tca-si-ord-forn
                       initialize zoom-tca-rec
                       move tca-codice      to zoom-tca-codice
                       move tca-descrizione to zoom-tca-descrizione
                       move tca-cod-magaz   to zoom-tca-cod-magaz 
                       move tca-cod-magaz   to mag-codice
                       read tmagaz no lock
                            invalid continue
                        not invalid 
                            move mag-descrizione to zoom-mag-descrizione
                       end-read
                       write zoom-tca-rec invalid continue end-write
                    end-if
                 end-perform
                 close tmagaz
           end-start.
           close zoom-tcaumag.
           open input zoom-tcaumag.
           move path-zoom-tcaumag to ext-file.

      ***---
       CONTROLLA-PERCENTUALE-IVA.
           move "IV"        to tbliv-codice1.
           move rof-cod-iva to tbliv-codice2.
           read tivaese no lock 
                invalid continue
            not invalid
                if tbliv-percentuale not = 0
                   set EsisteIVA to true
                end-if
           end-read.

LABLAB****---
      * PB-FORZA-PRESSED.
      *     set trovato to false.
      *     move low-value to tpr-rec.

      *     move cli-gdo     to tpr-gdo.
      *     move ef-data-buf to como-data.
      *     perform DATE-TO-FILE.
      *     add 1 to como-data.
      *     move como-data   to tpr-ini-dpo.
      *     start tpromo key >= tpr-chiave-ricerca
      *           invalid continue
      *       not invalid
      *           perform until 1 = 2
      *              read tpromo next at end  exit perform end-read
      *              if tpr-gdo not = cli-gdo exit perform end-if
      *              if not trovato
      *                 perform APRI-TMP-PROMO-PRZ
      *                 set trovato to true
      *              end-if
      *              move tpr-codice      to tprz-codice
      *              move cli-gdo         to tprz-gdo
      *              move tpr-descrizione to tprz-descr
      *              move tpr-ini-dpo     to tprz-ini-dpo
      *              move tpr-fine-dpo    to tprz-fine-dpo
      *              write tprz-rec invalid continue end-write
      *           end-perform
      *           if tof-prg-destino not = 0
      *              move low-value       to loc-rec
      *              move cli-gdo         to loc-gdo
      *              move cli-codice      to loc-cliente
      *              move tof-prg-destino to loc-destino
      *              move como-data       to loc-ini-dpo
      *              start locali key >= loc-chiave-ricerca
      *                    invalid continue
      *                not invalid
      *                    perform until 1 = 2
      *                       read locali next 
      *                            at end exit perform 
      *                       end-read
      *                       if loc-gdo     not = cli-gdo    or
      *                          loc-cliente not = cli-codice or
      *                          loc-destino not = tof-prg-destino
      *                          exit perform
      *                       end-if
      *                       move loc-codice to tpr-codice
      *                       if not trovato
      *                          perform APRI-TMP-PROMO-PRZ
      *                          set trovato to true
      *                       end-if
      *                       move loc-codice      to tprz-codice
      *                       move cli-gdo         to tprz-gdo
      *                       move tpr-descrizione to tprz-descr
      *                       move loc-ini-dpo     to tprz-ini-dpo
      *                       move loc-fine-dpo    to tprz-fine-dpo
      *                       write tprz-rec invalid continue end-write
      *                    end-perform
      *              end-start      
      *           end-if
      *
      *           if trovato
      *              close tmp-promo-prz
      *              move path-tmp-promo-prz to ext-file
      *              move "tmp-promo-prz2"   to como-file
      *              call "zoom-gt"       using como-file, 
      *                                         tprz-rec
      *                                  giving stato-zoom
      *              end-call
      *              cancel "zoom-gt"
      *              if stato-zoom = 0
      *                 move tprz-codice to volantino-forzato
      *                 move tprz-descr  to lab-forzato-buf
      *                 display lab-forzato
      *              end-if
      *              delete file tmp-promo-prz
      *           end-if
      *     end-start.

      ****---
BLISTR* CHK-BLISTER-PRESSED.
      *     inquire chk-blister, value in chk-blister-buf.
      *     if mod-cliente-destino = 1 or bollettata
      *        if  chk-blister-buf = 1
      *           move 0 to chk-blister-buf
      *        else
      *           move 1 to chk-blister-buf
      *        end-if
      *        display chk-blister
      *     else
      *        if chk-blister-buf = 1

      *           set  link-blister   to true
      *           move "BLISTER"      to link-des
      *           move 0              to link-qta
      *           move spaces         to link-udm
      *           call   "imballo" using imballo-linkage
      *           cancel "imballo"
      *           if link-imballo-saved = 1
      *              move link-qta       to imballi-ed hid-imballi
      *              perform DESCRIZIONE-IMBALLO
      *              move 78-ID-ef-qta to control-id
      *           else
      *              move 0 to chk-blister-buf
      *              display chk-blister
      *              move 78-ID-chk-blister to control-id
      *           end-if
      *        else
      *           if rof-qta not = 0
      *              move hid-rof-tipo-imballo to imq-codice
      *              read timbalqta no lock 
      *                   invalid 
      *                   display message "Imballo non congruo"
      *                             title tit-err
      *                              icon 3
      *               not invalid
      *                   move imq-tipo to imb-codice
      *                   read timballi no lock 
      *                        invalid 
      *                        display message "Imballo non congruo"
      *                                  title tit-err
      *                                   icon 3
      *                    not invalid
      *                        move imb-descrizione to link-des 
      *                                                hid-des-imballo
      *                        move imq-qta-imb     to imballi-ed 
      *                                                hid-imballi
      *                        perform DESCRIZIONE-IMBALLO
      *                        move 78-ID-ef-qta to control-id
      *                   end-read
      *              end-read
      *           end-if
      *        end-if
      *        perform CANCELLA-COLORE
      *        move 4                to accept-control
      *        set  FromSpostamento  to false
      *        set  ArticoloSetFocus to false
      *        set  ControllaCampi   to true
      *     end-if.

LABLAB***---
      * CERCA-PROMO-LISTINO.
      *     set trovato      to false.
      *     |Inserimento ordini
      *     move ef-data-buf to como-data.
      *     perform DATE-TO-FILE.
      *     move como-data to tof-data-ordine.
      *
      *     initialize path-tmp-promo-prz.
      *     move 0 to num-promo.
      *
      *     |Provo con la promo locale
      *     if tof-prg-destino not = 0
      *        if promo-future
      *           move cli-gdo          to loc-gdo
      *           move cli-codice       to loc-cliente
      *           move tof-prg-destino  to loc-destino
      *           move tof-data-ordine  to loc-ini-dpo
      *           move low-value        to loc-fine-dpo
      *           start locali key >= loc-chiave-ricerca
      *                 invalid set record-ok to false
      *             not invalid set record-ok to true
      *           end-start
      *        else
      *           move cli-gdo          to loc-gdo
      *           move cli-codice       to loc-cliente
      *           move tof-prg-destino  to loc-destino
      *           move tof-data-ordine  to loc-fine-dpo
      *           move low-value        to loc-ini-dpo
      *           start locali key >= loc-chiave-gdo-fine
      *                 invalid set record-ok to false
      *             not invalid set record-ok to true
      *           end-start
      *        end-if
      *        if record-ok
      *           perform until 1 = 2
      *              read locali next at end exit perform end-read
      *              if loc-gdo     not = cli-gdo    or
      *                 loc-cliente not = cli-codice or
      *                 loc-destino not = tof-prg-destino
      *                 exit perform
      *              end-if
      *
      *              if loc-fine-dpo >= tof-data-ordine and
      *                 loc-ini-dpo  <= tof-data-ordine or
      *                 promo-future
      *
      *                 move loc-codice to tpr-codice
      *                 read tpromo no lock
      *                      invalid continue
      *                  not invalid
      *                      if tpr-gdo      = loc-gdo     and
      *                         tpr-ini-dpo  = loc-ini-dpo and
      *                         tpr-fine-dpo = loc-fine-dpo
      *                         move tpr-codice to rpr-codice
      *                         move art-codice to rpr-articolo
      *                         read rpromo no lock 
      *                              invalid continue
      *                          not invalid
      *                              if path-tmp-promo-prz = spaces
      *                                 perform APRI-TMP-PROMO-PRZ
      *                              end-if
      *                              set trovato to true
      *                              add 1 to num-promo
      *                              move tpr-codice to tprz-codice
      *                              move cli-gdo    to tprz-gdo
      *                              move tpr-descrizione  
      *                                to tprz-descr
      *                              move tpr-ini-dpo      
      *                                to tprz-ini-dpo
      *                              move tpr-fine-dpo     
      *                                to tprz-fine-dpo
      *                              move rpr-prz-acq      
      *                                to tprz-prz-acq
      *                              write tprz-rec 
      *                                    invalid continue 
      *                              end-write
      *                         end-read
      *                      end-if
      *                 end-read
      *              end-if
      *           end-perform
      *        end-if
      *     end-if.
      *
      *     |PROMO GDO
      *     if not trovato
      *        if promo-future
      *           move cli-gdo         to tpr-gdo
      *           move tof-data-ordine to tpr-ini-dpo
      *           move low-value       to tpr-fine-dpo
      *           start tpromo key >= tpr-chiave-ricerca
      *                 invalid set record-ok to false
      *             not invalid set record-ok to true
      *           end-start
      *        else
      *           move cli-gdo         to tpr-gdo
      *           move tof-data-ordine to tpr-fine-dpo
      *           move low-value       to tpr-ini-dpo
      *           start tpromo key >= tpr-chiave-gdo-fine
      *                 invalid set record-ok to false
      *             not invalid set record-ok to true
      *           end-start
      *        end-if
      *
      *        if record-ok
      *           perform until 1 = 2
      *              read tpromo next at end exit perform end-read
      *              if tpr-gdo not = cli-gdo
      *                 exit perform
      *              end-if
      *
      *              if tpr-fine-dpo >= tof-data-ordine and
      *                 tpr-ini-dpo  <= tof-data-ordine or promo-future
      *
      *                 if tpr-nazionale
      *                    move tpr-codice to rpr-codice
      *                    move art-codice to rpr-articolo
      *                    read rpromo no lock 
      *                         invalid continue
      *                     not invalid
      *                         set trovato to true
      *                         if path-tmp-promo-prz = spaces
      *                            perform APRI-TMP-PROMO-PRZ
      *                         end-if
      *                         add 1 to num-promo
      *                         move tpr-codice       to tprz-codice
      *                         move cli-gdo          to tprz-gdo
      *                         move tpr-descrizione  to tprz-descr
      *                         move tpr-ini-dpo      to tprz-ini-dpo
      *                         move tpr-fine-dpo     to tprz-fine-dpo
      *                         move rpr-prz-acq      to tprz-prz-acq
      *                         write tprz-rec invalid continue end-write
      *                    end-read
      *                 end-if
      *              end-if
      *           end-perform
      *        end-if
      *
      *     end-if.
      *
      *     if not trovato 
      *        if not promo-future
      *           move 0 to rpr-codice
      *           move cli-gdo          to lst-gdo
      *           move tof-data-ordine  to lst-data
      *           move art-codice       to lst-articolo
      *           start listini key <= lst-k-articolo
      *                 invalid continue
      *             not invalid
      *                 read listini previous
      *                 if lst-gdo      = cli-gdo          and
      *                    lst-data    <= tof-data-ordine  and
      *                    lst-articolo = art-codice
      *                    |In caso di "FA" non cerco promo 
      *                    |né successive né precedenti
      *                    if lst-prezzo >= 999999,99
      *                       set hid-bloccato to true
      *                       set trovato      to false
      *                    else
      *                       if lst-prezzo not = 0
      *                          set trovato to true
      *                          move lst-prezzo   to rpr-prz-acq
      *                          set  hid-bloccato to false
      *                       else
      *                          set  hid-bloccato to true
      *                          |CASO "SP"
      *                          |1. Cerco la promo immediatamente dopo
      *                          perform CERCA-PROMO-DOPO
      *                          if not trovato
      *                             |2. Cerco la promo immediatamente prima
      *                             perform CERCA-PROMO-PRIMA
      *                          end-if
      *                       end-if
      *                    end-if
      *                 end-if
      *           end-start
      *        end-if
      *     else
      *        if num-promo > 1 or promo-future
      *           move path-tmp-promo-prz to ext-file
      *           move "tmp-promo-prz"    to como-file

      *           call "zoom-gt"       using como-file, 
      *                                      tprz-rec
      *                               giving stato-zoom
      *           end-call
      *           cancel "zoom-gt"
      *           if stato-zoom = 0
      *              move tprz-codice  to rpr-codice
      *              move tprz-prz-acq to rpr-prz-acq
      *              set trovato to true
      *           else
      *              set trovato to false
      *           end-if
      *        else
      *           move tprz-codice  to rpr-codice
      *           move tprz-prz-acq to rpr-prz-acq
      *        end-if
      *        close       tmp-promo-prz
      *        delete file tmp-promo-prz
      *     end-if.

LABLAB***---
      * Usato quando è già stato scelto il volantino dalla funzione
      * apposita di forzatura. A questo punto si sa già qual è
      * FORZA-PREZZO-VOLANTINO.
      *     |Inserimento ordini
      *     move ef-data-buf to como-data.
      *     perform DATE-TO-FILE.
      *     move como-data to tof-data-ordine.
      *
      *     set promo-future to false.
      *     move volantino-forzato to rpr-codice.
      *     move art-codice        to rpr-articolo.
      *     read rpromo no lock
      *          invalid set trovato to false
      *      not invalid set trovato to true
      *     end-read.
      *
      *     if not trovato 
      *        move 0 to rpr-codice
      *        move cli-gdo          to lst-gdo
      *        move tof-data-ordine  to lst-data
      *        move art-codice       to lst-articolo
      *        start listini key <= lst-k-articolo
      *              invalid continue
      *          not invalid
      *              read listini previous
      *              if lst-gdo      = cli-gdo          and
      *                 lst-data    <= tof-data-ordine  and
      *                 lst-articolo = art-codice
      *                 if lst-prezzo not = 0
      *                    set trovato to true
      *                    move lst-prezzo  to rpr-prz-acq
      *                    set hid-bloccato to false
      *                 end-if
      *              end-if
      *        end-start
      *     end-if.

LABLAB****---
      * APRI-TMP-PROMO-PRZ.
      *     accept  como-data from century-date.
      *     accept  como-ora  from time.
      *     accept  path-tmp-promo-prz from environment "PATH_ST".
      *     inspect path-tmp-promo-prz replacing trailing 
      *                                spaces by low-value.
      *     string  path-tmp-promo-prz delimited low-value
      *             "TMP-PROMO-PRZ"    delimited size
      *             "_"                delimited size
      *             como-data          delimited size
      *             "_"                delimited size
      *             como-ora           delimited size
      *             ".tmp"             delimited size
      *             into path-tmp-promo-prz
      *     end-string.
      *     open output tmp-promo-prz.

      ***---
      * CERCA-PROMO-DOPO.
      *     if tof-prg-destino not = 0
      *        move cli-gdo         to loc-gdo
      *        move cli-codice      to loc-cliente
      *        move tof-prg-destino to loc-destino
      *        move tof-data-ordine to loc-ini-dpo
      *        move low-value       to loc-fine-dpo
      *        start locali key >= loc-chiave-ricerca
      *              invalid set record-ok to false
      *          not invalid set record-ok to true
      *        end-start
      *
      *        if record-ok
      *           perform until 1 = 2
      *              read locali next at end exit perform end-read
      *              if loc-gdo      not =  cli-gdo         or
      *                 loc-cliente  not =  cli-codice      or
      *                 loc-destino  not =  tof-prg-destino
      *                 exit perform
      *              end-if
      *
      *              if loc-ini-dpo  >= tof-data-ordine and
      *                 loc-fine-dpo >= tof-data-ordine
      *                 move loc-codice to tpr-codice
      *                 read tpromo no lock
      *                      invalid continue
      *                  not invalid
      *                      if tpr-gdo      = loc-gdo     and
      *                         tpr-ini-dpo  = loc-ini-dpo and
      *                         tpr-fine-dpo = loc-fine-dpo
      *                         move tpr-codice to rpr-codice
      *                         move art-codice to rpr-articolo
      *                         read rpromo no lock 
      *                              invalid continue
      *                          not invalid set trovato to true
      *                                      exit perform
      *                         end-read
      *                      end-if
      *                 end-read
      *              end-if

      *
      *           end-perform
      *        end-if
      *     end-if.
      *
      *     if not trovato
      *        move cli-gdo         to tpr-gdo
      *        move tof-data-ordine to tpr-ini-dpo
      *        move low-value       to tpr-fine-dpo
      *        start tpromo key >= tpr-chiave-ricerca
      *              invalid set record-ok to false
      *          not invalid set record-ok to true
      *        end-start
      *
      *        if record-ok
      *           perform until 1 = 2
      *              read tpromo next at end exit perform end-read
      *              if tpr-gdo not = cli-gdo
      *                 exit perform
      *              end-if
      *
      *
      *              if tpr-fine-dpo > tof-data-ordine and
      *                 tpr-ini-dpo  > tof-data-ordine
      *                 move tpr-codice to rpr-codice
      *                 move art-codice to rpr-articolo
      *                 read rpromo no lock 
      *                      invalid continue
      *                  not invalid set trovato to true
      *                              exit perform
      *                 end-read
      *              end-if
      *           end-perform
      *        end-if
      *     end-if.                   
      *
      ****---
      * CERCA-PROMO-PRIMA.
      *     if tof-prg-destino not = 0
      *        move cli-gdo         to loc-gdo
      *        move cli-codice      to loc-cliente
      *        move tof-prg-destino to loc-destino
      *        move tof-data-ordine to loc-fine-dpo
      *        move low-value       to loc-ini-dpo
      *        start locali key <= loc-chiave-gdo-fine
      *              invalid set record-ok to false
      *          not invalid set record-ok to true
      *        end-start
      *
      *        if record-ok
      *           perform until 1 = 2
      *              read locali previous at end exit perform end-read
      *              if loc-gdo     not =  cli-gdo      or
      *                 loc-cliente not =  cli-codice   or
      *                 loc-destino not =  tof-prg-destino
      *                 exit perform
      *              end-if
      *              if loc-ini-dpo  >= tof-data-ordine and
      *                 loc-fine-dpo >= tof-data-ordine
      *                 move loc-codice to tpr-codice
      *                 read tpromo no lock
      *                      invalid continue
      *                  not invalid
      *                      if tpr-gdo      = loc-gdo     and
      *                         tpr-ini-dpo  = loc-ini-dpo and
      *                         tpr-fine-dpo = loc-fine-dpo
      *                         move tpr-codice to rpr-codice
      *                         move art-codice to rpr-articolo
      *                         read rpromo no lock 
      *                              invalid continue
      *                          not invalid set trovato to true
      *                                      exit perform
      *                         end-read
      *                      end-if
      *                 end-read
      *              end-if
      *           end-perform
      *        end-if
      *     end-if.
      *
      *     if not trovato
      *        move cli-gdo         to tpr-gdo
      *        move tof-data-ordine to tpr-fine-dpo
      *        move low-value       to tpr-ini-dpo
      *        start tpromo key <= tpr-chiave-gdo-fine
      *              invalid set record-ok to false
      *          not invalid set record-ok to true
      *        end-start
      *
      *        if record-ok
      *           perform until 1 = 2
      *              read tpromo previous at end exit perform end-read
      *              if tpr-gdo not = cli-gdo
      *                 exit perform
      *              end-if
      *              if tpr-fine-dpo < tof-data-ordine and
      *                 tpr-ini-dpo  < tof-data-ordine
      *                 move tpr-codice to rpr-codice
      *                 move art-codice to rpr-articolo
      *                 read rpromo no lock 
      *                      invalid continue
      *                  not invalid set trovato to true
      *                              exit perform
      *                 end-read
      *              end-if
      *           end-perform
      *        end-if
      *
      *     end-if.
      *

LABLAB***---
      * VALORIZZA-MAGGIOR-GIACENZA.
      *     |Valorizzo prg-chiave con il record avente > giacenza
      *     move 0 to giacenza.
      *     set  trovato              to false.
      *     move low-value            to prg-chiave.
      *     move bli-el-articolo(idx) to prg-cod-articolo.
      *     move bli-magazzino        to prg-cod-magazzino.
      *     start progmag key >= prg-chiave
      *           invalid continue
      *       not invalid
      *           perform until 1 = 2
      *              read progmag next at end exit perform end-read
      *              if prg-cod-articolo  not = bli-el-articolo(idx) or
      *                 prg-cod-magazzino not = bli-magazzino
      *                 exit perform
      *              end-if
      *              if not trovato
      *                 set trovato to true
      *                 |Se la prima ed unica volta ho una giacenza di -1
      *                 move prg-giacenza to giacenza
      *                 move prg-giacenza to giacenza
      *                 move prg-chiave to GiacenzaKey
      *              end-if
      *              if prg-giacenza > giacenza
      *                 move prg-giacenza to giacenza
      *                 move prg-chiave to GiacenzaKey
      *              end-if
      *           end-perform
      *     end-start.

      ****---
      * CONTROLLA-SE-PESO-SUP-24000-KG.
LUBEXX******Blocco il salvataggio di un ordine con peso > 24000 Kg
      *     set tutto-ok to true.
      *     move 0 to tot-peso.
      *     inquire form1-gd-1, last-row in tot-righe.
      *     perform varying store-riga from 2 by 1 
      *               until store-riga > tot-righe
      *
      *        inquire form1-gd-1(store-riga, 1),
      *                hidden-data in hid-rof-rec
      *                
      *        move hid-peso to como-peso
      *
      *        inquire form1-gd-1(store-riga, 4), 
      *                cell-data in como-qta
      *        compute tot-peso = ( tot-peso + ( como-qta * como-peso) )
      *        if tot-peso > 24000 
      *           display message "Salvataggio impossibile!!!"
      *                    x"0d0a""Peso superiore a 24.000 Kg."
      *                     title tit-err
      *                      icon 2
      *           set errori to true
      *           exit perform
      *        end-if
      *     end-perform.

LUBEXX***---
       VALORIZZA-PROGRESSIVO-CORRETTO.
           move low-value        to prg-chiave.
           move rof-cod-articolo to prg-cod-articolo.
           move tca-cod-magaz    to prg-cod-magazzino.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 read progmag next no lock at end continue end-read
                 if prg-cod-articolo  = rof-cod-articolo and
                    prg-cod-magazzino = tca-cod-magaz
                    move prg-chiave       to hid-rof-prg-chiave
                    move prg-peso-non-utf to rof-peso-non-utf
                    move prg-peso-utf     to rof-peso-utf
                 end-if
           end-start.

LUBEXX***---
       FORZA-PESO-UGUALE.
           read progmag no lock 
                invalid 
                perform 5 times
                   display message "ARTICOLO " prg-cod-articolo
                            x"0d0a""RIGA " rof-riga
                            x"0d0a""DATI INCOERENTI!!!"
                            x"0d0a""CANCELLARE E REINSERIRE LA RIGA"
                             title tit-err
                              icon 2
                end-perform
            not invalid
                move prg-peso-utf     to rof-peso-utf
                move prg-peso-non-utf to rof-peso-non-utf
           end-read.

      ***---
      * ABILITA-GEST-PLUS.
LUBEXX*     inquire Screen1-Ta-1, value in pagina.
LUBEXX*     if pagina = 1
LUBEXX*        if tcl-si-gest-plus
LUBEXX*           move 1 to v-gest-plus
LUBEXX*        else
LUBEXX*           move 0 to v-gest-plus tof-gest-plus
LUBEXX*        end-if
LUBEXX*     else
LUBEXX*        move 0 to v-gest-plus
LUBEXX*     end-if.
LUBEXX*     display lab-gest ef-gest.

      ****---
      * ARROTONDA-PRZ-BLISTER.             
      *     add 0,005 to como-numero giving como-prezzo.
      *     add como-prezzo to Sum.
      *
      *     if idx = ins-idx|sono sull'ultimo
      *        if Sum not = TotPrzBlister
      *           if Sum > TotPrzBlister
      *              compute como-prezzo = 
      *                      como-prezzo - (Sum - TotPrzBlister)
      *           else
      *              compute como-prezzo = 
      *                      como-prezzo + (TotPrzBlister - Sum)
      *           end-if
      *        end-if
      *     end-if.
      *
      ****---
      * CONTROLLA-QTA-BLISTER.
      *     set tutto-ok to true.
      *     inquire form1-gd-1, last-row in tot-righe.
      *     perform varying store-riga from 2 by 1 
      *               until store-riga > tot-righe
      *        inquire form1-gd-1(store-riga, 1), 
      *                hidden-data in hid-rof-rec
      *        if hid-blister = 1
      *           inquire form1-gd-1(store-riga, 4), cell-data in col-qta
      *           move col-qta  to rof-qta
      *           if hid-imballi not = 0 |Sono sul primo elemento
      *              move rof-qta  to qta-blis-check
      *           else
      *              if rof-qta not = qta-blis-check
      *                 set errori to true
      *                 subtract 1 from store-riga 
      *                 move store-riga to riga-ed
      *                 exit perform
      *              end-if
      *           end-if
      *        end-if
      *     end-perform.
      *              
      *     if errori
      *        display message"Quantità blister errata. "
      *       x"0d0a""Dev'essere uguale su tutti gli articoli."
      *       x"0d0a""Controllare quantità riga ", riga-ed
      *                  title tit-err
      *                   icon 2
      *        set errori to true
      *        perform CANCELLA-COLORE
      *     end-if.
      *

      ***---
       SELEZIONA-LISTINO.
           set listino-trovato  to false
           inquire ef-cli value cli-codice of clienti
           move cli-codice of clienti   to tlis-fornitore
           inquire ef-dest value desf-prog
           move desf-prog to tlis-destino
           move low-value to tlis-ini-val
           start tlistini key not < tlis-chiave-ricerca
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tlistini next
                       at end
                          exit perform
                    end-read
                    if cli-codice of clienti not = tlis-fornitore or 
                       desf-prog not = tlis-destino
                       exit perform
                    end-if
                    if tlis-ini-val <= tof-data-ordine and 
                       tlis-fine-val >= tof-data-ordine
                       set listino-trovato  to true
                       exit perform
                    end-if
                 end-perform
           end-start.
           if not listino-trovato
              initialize tlis-chiave
           else
              if tlis-trasp-escluso and pgm-name = "gordforn"
      *****           if pgm-name = "gordfornvar"
      *****              if old-tof-franco-part-no and 
      *****                 tof-data-ordine not = old-tof-data-ordine
      *****                 move 1 to chk-franco-buf
      *****                 display chk-franco
      *****              end-if
      *****           else
                    move 1 to chk-franco-buf
                    display chk-franco
      *****           end-if
              end-if
           end-if.

      ***---
       DESCR-IMBALLO.
           move ef-imb-ord-buf  to imq-codice
           read timbalqta
              invalid
                 move space  to imq-tipo
                 move zero   to imq-qta-imb
           end-read.

           move imq-qta-imb      to imballi-ed
           move imq-tipo         to imb-codice
           read timballi no lock
              invalid  
                 initialize imb-descrizione
           end-read
           inspect imb-descrizione replacing trailing spaces
                                                         by low-value
           move imq-qta-imb    to imballi-ed
           call "C$JUSTIFY" using imballi-ed, "L"
           initialize imballo-descrizione
           string  imb-descrizione delimited by low-value
                   " da "          delimited by size
                   imballi-ed      delimited by spaces
                   " x "           delimited by size
                   art-udm-imballo delimited by size
                   into imballo-descrizione
           end-string.

           move imballo-descrizione   to lab-imb-buf.


      ***---
       SALVA-NOTE.
           move low-value to tmp-nof-chiave
           start TMP-NORDFORN key not < tmp-nof-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read TMP-NORDFORN next no lock
                       at end
                          exit perform
                    end-read
                    move tmp-nof-rec  to nof-rec
                    move tof-chiave   to nof-chiave-ordine

                    move tof-dati-comuni to nof-dati-comuni
                    write NOF-REC
                       invalid
                          continue
                    end-write
                 end-perform
           end-start.

      ***---
       CALCOLA-MESE-RIF.
           accept como-data-oggi   from century-date
           if como-data-oggi >= tof-data-consegna
              move 1 to tof-mese-rif
           else
              if tof-urgente
                 move 1 to tof-mese-rif
              else
                 move tof-data-consegna(5:2) to mese-consegna
                 move como-data-oggi(5:2)    to mese-oggi
                 |Data di consegna nell'anno successivo: il mese
                 |è di valore minore, ma la data INTERA no
                 if mese-oggi > mese-consegna
                    add 12 to mese-consegna
                 end-if
                 compute tof-mese-rif = 
                       ( mese-consegna - mese-oggi ) + 1
              end-if

      *****        move 0 to tof-mese-rif
      *****        compute como-numero = 
      *****                   (function integer-of-date(tof-data-consegna) - 
      *****                    function integer-of-date(como-data-oggi))
      *****        divide como-numero by 30 giving tof-mese-rif
      *****                              remainder resto
      *****        if resto not = zero
      *****           add 1 to tof-mese-rif
      *****        end-if
           end-if.
           
      ***---
       LABEL-PRZ-FINALE.
           compute prz-reale =
                   rof-imponib-merce  +
                   rof-imp-consumo    +
                   rof-imp-cou-cobat  +
                   rof-add-piombo.
           move  prz-reale to prezzo-finale.
      
           modify lbl-prz-finale title prezzo-finale.

      ***---
       PB-ARTICOLI-PRESSED.
LUBEXX     move "art-ordforn" to lck-nome-pgm
LUBEXX     read lockfile no lock
LUBEXX          invalid
LUBEXX          move "art-ordforn" to lck-nome-pgm
LUBEXX          move "I"           to lck-operazione
LUBEXX          move user-codi     to lck-utente-creazione
LUBEXX          accept lck-ora-creazione  from time
LUBEXX          accept lck-data-creazione from century-date
LUBEXX          write lck-rec invalid continue end-write
LUBEXX          read  lockfile record lock end-read

                initialize art-ordforn-linkage 
                           replacing numeric data by zeroes
                                alphanumeric data by spaces
        
                if pgm-name = "gordfornvar"
                   move tof-chiave to art-ordforn-chiave
                else
                   move 1 to art-ordforn-ins
                end-if
                move ef-cau-buf to art-ordforn-causale
                move ef-data-buf to como-data
                perform DATE-TO-FILE
                move como-data   to art-ordforn-data
                move user-codi   to art-ordforn-user
                move mod-campi   to art-ordforn-mod
                move cli-codice  of clienti to art-ordforn-forn
                move desf-prog   to art-ordforn-dest
                move tlis-codice to art-ordforn-listino
          
                call   "art-ordforn" using art-ordforn-linkage
                cancel "art-ordforn"
            
                if art-ordforn-status = 1
                   perform METTI-IN-GRID-ART-ORDFORN
                end-if

LUBEXX      not invalid
                if lck-utente-creazione not = user-codi
LUBEXX             display message
LUBEXX                     "Operazione impossibile!!!"
LUBEXX              x"0d0a""Funzione d'inserimento già "
LUBEXX                     "utilizzata da " lck-utente-creazione
LUBEXX                        title tit-err
LUBEXX                         icon 3
                else

                   initialize art-ordforn-linkage 
                              replacing numeric data by zeroes
                                   alphanumeric data by spaces
        
                   if pgm-name = "gordfornvar"
                      move tof-chiave to art-ordforn-chiave
                   else
                      move 1 to art-ordforn-ins
                   end-if
                   move ef-cau-buf  to art-ordforn-causale
                   move ef-data-buf to como-data
                   perform DATE-TO-FILE
                   move como-data   to art-ordforn-data
                   move user-codi   to art-ordforn-user
                   move mod-campi   to art-ordforn-mod
                   move cli-codice  of clienti to art-ordforn-forn
                   move desf-prog   to art-ordforn-dest
                   move tlis-codice to art-ordforn-listino
          
                   call   "art-ordforn" using art-ordforn-linkage
                   cancel "art-ordforn"
            
                   if art-ordforn-status = 1
                      perform METTI-IN-GRID-ART-ORDFORN
                   end-if
                end-if
LUBEXX     end-read.
           move art-ordforn-chiave to aor-chiave-testa.
           move low-value to aor-prog.
           perform ARTICOLI-DA-CONFERMARE.
           if tof-da-confermare-si move 1 to v-articoli
           else                    move 0 to v-articoli
           end-if.
           display lab-articoli.
  
      ***---
       METTI-IN-GRID-ART-ORDFORN.
           move low-value to aor-chiave.
           if pgm-name = "gordfornvar"
              move tof-chiave to aor-chiave-testa art-ordforn-chiave
           else
              move 0 to aor-anno    art-ordforn-anno
              move 0 to aor-numero  art-ordforn-numero
           end-if.
           start art-ordforn key >= aor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read art-ordforn next no lock 
                         at end exit perform 
                    end-read
                    if aor-chiave-testa not = art-ordforn-chiave
                       exit perform
                    end-if
                    if aor-articolo not = 0
                       delete art-ordforn record
                              invalid continue
                       end-delete
                       move aor-articolo to art-codice ef-art-buf
                       modify ef-art, value aor-articolo
                       set NewRow to true
                       move 0 to link-imballo-saved
                       move aor-imballo to ef-imb-ord-buf
                       move aor-qta to ef-qta-buf
                       move 0 to ef-sconto-1-BUF
                       move 0 to ef-sconto-2-BUF
                       move 0 to ef-sconto-3-BUF
                       move 0 to ef-sconto-4-BUF
                       move 0 to ef-sconto-5-BUF
                       move 0 to tlis-codice
                       move 0 to ef-costi-agg-BUF
                       move aor-descrizione to lab-art-buf col-des
                       move aor-prz-unit  to ef-uni-BUF
                       move aor-netto     to ef-imp-BUF
                       move aor-imp-cons  to ef-cons-BUF
                       move aor-coubat    to ef-cou-BUF
                       move aor-add-pb    to ef-add-BUF 
                       move aor-costi-agg to ef-costi-agg-BUF
                       if aor-netto not = 0
                          perform RECUPERA-IVA
                       else
                          move tge-cod-iva-omag to ef-cod-iva-BUF
                                                   col-iva
                                                   tbliv-codice2
                       end-if
                       move col-art to old-col-art
                       move col-des to old-col-des
                       move col-qta to old-col-qta
                       move col-uni to old-col-uni
                       move col-imp to old-col-imp
                       move col-sconto-1 to old-col-sconto-1
                       move col-sconto-2 to old-col-sconto-2
                       move col-sconto-3 to old-col-sconto-3
                       move col-sconto-4 to old-col-sconto-4
                       move col-sconto-5 to old-col-sconto-5
                       move col-cou      to old-col-cou
                       move col-iva      to old-col-iva
                       move col-add      to old-col-add
                       
                       move aor-chiave-progmag to hid-rof-prg-chiave
                       move aor-imballo        to hid-rof-imb-ordinato
                       move aor-qta            to hid-rof-qta-ord
                       move 0                  to hid-rof-qta-arrivata

                       perform ENTRY-TO-ROW
                    end-if
                 end-perform
           end-start.

      ***---
       ARTICOLI-DA-CONFERMARE.
           set tof-da-confermare-no to true.
           start art-ordforn key >= aor-chiave
                 invalid continue
             not invalid
                 read art-ordforn next no lock 
                      at end continue 
                 end-read
                 if aor-chiave-testa = art-ordforn-chiave
                    set tof-da-confermare-si to true
                 end-if
           end-start. 

      ***---
       CALCOLA-IMPONIBILE.       
           move ef-uni-buf  to imp-merce.
           compute imp-merce = imp-merce - imposta-consumo.
           compute imp-merce = imp-merce - imposta-cou.
           compute imp-merce = imp-merce - imposta-cobat.
           compute imp-merce = imp-merce - add-piombo.

           move imp-merce        to ef-imp-buf rof-imponib-merce.
           move imposta-consumo  to rof-imp-consumo.
           add imposta-cou       to 
               imposta-cobat giving rof-imp-cou-cobat.
           move add-piombo       to rof-add-piombo.

      ***---
       CALCOLA-TRASPORTO.
           move 0 to costo-trasporto.
           move spaces to tge-chiave.
           read tparamge no lock.
           if desf-nazione = "ITA"
              compute costo-trasporto = 
                      prg-peso * tge-trasp-italy
           else
              compute costo-trasporto = 
                      prg-peso * tge-trasp-estero
           end-if.     

      ***---
       CARICA-TMP-PROMO.
           call "W$MOUSE" using set-mouse-shape, wait-pointer.
           set record-ok to true.
           inquire ef-art, value in SaveArticolo.
      *     tca-cod-magaz.
           accept  path-tmp-tpromo-zoom  from environment "PATH_ST".
           accept  como-data             from century-date.
           accept  como-ora              from time.
           inspect path-tmp-tpromo-zoom  replacing trailing
                                         spaces by low-value.
           string path-tmp-tpromo-zoom   delimited by low-value
                  "tmp-tpromo-zoom"      delimited by size
                  "_"                    delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  into path-tmp-tpromo-zoom
           end-string.

           set trovato to false.
           open output tmp-tpromo-zoom.
           if status-tmp-tpromo-zoom = "00"

              move low-value to tpr-rec
              accept tpr-fine-volantino from century-date
              start tpromo key >= tpr-k-fine-vol
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read tpromo next at end exit perform end-read
                       move tpr-codice         
                         to tmp-tpr-z-codice
                       move tpr-gdo            
                         to tmp-tpr-z-gdo
                       move tpr-descrizione    
                         to tmp-tpr-z-descrizione
                       move tpr-ini-volantino  
                         to tmp-tpr-z-ini-volantino
                       move tpr-fine-volantino 
                         to tmp-tpr-z-fine-volantino
                       write tmp-tpr-z-rec
                             invalid rewrite tmp-tpr-z-rec
                       end-write                             
                    end-perform
              end-start

              close tmp-tpromo-zoom
              call "W$MOUSE" using set-mouse-shape, arrow-pointer
           end-if.

      ***---
       DESCRIZIONE-PROMO.
           initialize como-descr-promo.
           move tpr-gdo  to gdo-codice.
           read tgrupgdo invalid initialize gdo-intestazione end-read.

           inspect gdo-intestazione 
                                   replacing trailing space by low-value
           inspect tpr-descrizione 
                                   replacing trailing space by low-value

           string gdo-intestazione        delimited by low-value
                  " - "                   delimited by size
                  tpr-descrizione         delimited by low-value
                  " - dal "               delimited by size
                  tpr-ini-volantino(7:2)  delimited by size
                  "/"                     delimited by size
                  tpr-ini-volantino(5:2)  delimited by size
                  "/"                     delimited by size
                  tpr-ini-volantino(1:4)  delimited by size
                  " al "                  delimited by size
                  tpr-fine-volantino(7:2) delimited by size
                  "/"                     delimited by size
                  tpr-fine-volantino(5:2) delimited by size
                  "/"                     delimited by size
                  tpr-fine-volantino(1:4) delimited by size
                  into como-descr-promo.

           move como-descr-promo   to lab-promo-buf.

      ***---
       CHK-MANUALE-PRESSED.
           if control-id not = 78-ID-chk-manuale
              move 4 to accept-control
              if chk-manuale-buf = 1
                 move 0 to chk-manuale-buf
              else
                 move 1 to chk-manuale-buf
              end-if
              display chk-manuale
              exit paragraph
           end-if.
           add 2 to chk-manuale-buf giving ChkManualeBitmapNumber.
           move chk-manuale-buf to v-manuale.
           if chk-manuale-buf = 1
              inquire ef-impforn, value in imf-codice
              perform until 1 = 2
                 move "impforn"      to Como-File
                 call   "zoom-gt" using  como-file, imf-rec
                                 giving stato-zoom
                 cancel "zoom-gt"
                 if stato-zoom = 0
                    move imf-codice        to ef-impforn-buf
                    move imf-descrizione   to lab-impforn-buf
                    exit perform
                 end-if
              end-perform
              perform ENABLE-CAMPI-PREZZI
           else
              move 1 to ChkManualeBitmapNumber           
              move 0 to ef-cons-buf   
                        ef-add-buf
                        ef-cou-buf  
                        ef-sconto-1-buf 
                        ef-sconto-2-buf 
                        ef-sconto-3-buf 
                        ef-sconto-4-buf 
                        ef-sconto-5-buf 
                        ef-uni-buf    
                        ef-imp-buf 
                        ef-costi-agg-buf
              move spaces to lab-art-buf 
                             lab-iva-buf 
                             ef-cod-iva-buf
                             ef-imb-ord-buf
                             lab-imb-buf
              display ef-cons   
                      ef-add
                      ef-cou  
                      ef-sconto-1
                      ef-sconto-2
                      ef-sconto-3
                      ef-sconto-4
                      ef-sconto-5
                      ef-cod-iva
                      ef-uni  
                      ef-imp
                      lab-art 
                      lab-iva
                      ef-imb-ord
                      lab-imb
                      ef-costi-agg
                      chk-manuale
              move 0 to ef-impforn-buf old-art-codice
              move spaces to lab-impforn-buf   
              perform DISABLE-CAMPI-PREZZI
              move 78-ID-ef-art to control-id
              move 4            to accept-control
              move ef-qta-buf   to rof-qta-ord
              perform CONTROLLO-RIGA
              move rof-qta-ord  to ef-qta-buf
              display ef-qta
           end-if.
           display ef-impforn lab-impforn chk-manuale.

      ***---
       CHK-MANUALE-BEFOREPROC.
           if chk-manuale-buf = 0
              move 3 to ChkManualeBitmapNumber
           else
              move 4 to ChkManualeBitmapNumber
           end-if.
           display chk-manuale.

      ***---
       CHK-MANUALE-AFTERPROC.
           add 1 to chk-manuale-buf giving ChkManualeBitmapNumber.
           move 78-ID-ef-imb-ord to control-id.
           move 4 to accept-control.
           display chk-manuale.

      ***---
       AZZERA-MANUALE.
           move 0 to v-manuale,
           move 0 to chk-manuale-buf.
           move 1 to ChkManualeBitmapNumber.
           move 0 to ef-impforn-buf.
           move spaces to lab-impforn-buf.
           display chk-manuale ef-impforn lab-impforn.

      ***---
       VALORI-LISTINO-VIDEO.
           initialize rlis-rec replacing numeric data by zeros 
                                    alphanumeric data by spaces.
           inquire ef-impforn,   value rlis-tipo-tratt-imposte.
           inquire ef-uni,       value rlis-prz-acq.
           inquire ef-sconto-1,  value rlis-sconto-1.
           inquire ef-sconto-2,  value rlis-sconto-2.
           inquire ef-sconto-3,  value rlis-sconto-3.
           inquire ef-sconto-4,  value rlis-sconto-4.
           inquire ef-sconto-5,  value rlis-sconto-5.
           inquire ef-costi-agg, value rlis-costi-agg.
           inquire ef-uni,       value rlis-netto.
           move 0              to rlis-perce-pb.
           move 0              to rlis-perce-agg.
           move rlis-costi-agg to rlis-costi-agg-tot.
           move rlis-prz-acq   to rlis-scelta.
           set rlis-sconti-zero-no to true.
           move ef-art-buf to rlis-articolo.
           
           move ef-cons-buf to imposta-consumo.
           move ef-cou-buf  to imposta-cou.
           move ef-add-buf  to add-piombo.
           if add-piombo not = 0
              move imposta-cou to imposta-cobat
              move 0           to imposta-cou
           end-if.                
