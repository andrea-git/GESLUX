      ***---
       VALORIZZA-SCREEN.
           if btno-num-nc  not = 0 or
              btno-data-nc not = 0 or
              btno-anno-nc not = 0

              move btno-num-nc       to tno-numero
              move btno-data-nc(1:4) to tno-anno
              read tnotacr no lock
                   invalid
                   move btno-num-nc  to lab-num-nc-buf
                   move btno-data-nc to como-data
               not invalid
                   if tno-num-fattura not = 0
                      move tno-num-fattura  to lab-num-nc-buf
                      move tno-data-fattura to como-data
                   else
                      move btno-num-nc  to lab-num-nc-buf
                      move btno-data-nc to como-data
                   end-if
              end-read
              perform DATE-TO-SCREEN
              move como-data  to lab-data-nc-buf
           end-if.
           
           if btno-num-fm  not = 0 or
              btno-data-fm not = 0
              move btno-num-fm  to lab-num-fm-buf
              move btno-data-fm to como-data
              perform DATE-TO-SCREEN
              move como-data  to lab-data-fm-buf
           end-if.
           move btno-data   to como-data.
           perform DATE-TO-SCREEN.
           move como-data   to lab-data-buf.
           move btno-numero to numero-bozza.
           set cli-tipo-C to true.
           move btno-cod-cli to cli-codice.
           read clienti no lock invalid continue end-read.

           move btno-cod-cli     to des-codice.
           move btno-prg-destino to des-prog.
           read destini no lock invalid continue end-read.

           move btno-vettore to vet-codice.
           read tvettori no lock invalid continue end-read.

           move "PA"         to tblpa-codice1.
           move btno-cod-pag to tblpa-codice2.
           read tcodpag no lock invalid continue end-read.

           move esercizio       to lab-anno-nc-buf.

           move btno-cod-cli    to codice-ed.
           move codice-ed       to ef-cli-buf.
           call "C$JUSTIFY"  using ef-cli-buf, "L".

           move btno-prg-destino to codice-ed.
           move codice-ed        to ef-des-buf.
           call "C$JUSTIFY"   using ef-des-buf, "L".

           move btno-vettore     to ef-vet-buf.

           move cli-ragsoc-1    to lab-cli-buf.
           move cli-indirizzo   to lab-ind-buf.
           move cli-localita    to lab-loca-buf.

           move des-ragsoc-1    to lab-des-buf.
           move des-indirizzo   to lab-ind-d-buf.
           move des-localita    to lab-loca-d-buf.

           move vet-descrizione to lab-vet-buf.

           perform MOVE-DESCR-PAG.

           move btno-errore-colpa to errore-colpa.
           perform CARICA-COMBO-COLPA.

           set cli-tipo-C to true.
           move btno-cod-cli-fm to cli-codice.
           read clienti no lock 
                invalid 
                move spaces to cli-ragsoc-1 
                move spaces to cli-indirizzo
                move spaces to cli-localita 
           end-read.

           move btno-cod-cli-fm     to des-codice.
           move btno-prg-destino-fm to des-prog.
           read destini no lock 
                invalid
                move spaces to des-ragsoc-1 
                move spaces to des-indirizzo
                move spaces to des-localita 
           end-read.

           move cli-ragsoc-1    to lab-cli-fm-buf.
           move cli-indirizzo   to lab-ind-fm-buf.
           move cli-localita    to lab-loca-fm-buf.

           move des-ragsoc-1    to lab-des-fm-buf.
           move des-indirizzo   to lab-ind-d-fm-buf.
           move des-localita    to lab-loca-d-fm-buf.

           display form1.

      ***---
       CANCELLA.
           display message "Cancellare il documento corrente?"
                     title titolo
                      type mb-yes-no
                   default mb-no
                    giving scelta
                      icon 2

           if scelta = mb-yes
              move LinkChiave to btno-chiave
              delete btnotacr  record invalid continue end-delete
              perform CANCELLA-RIGHE
              perform CANCELLA-RIGHE2
              perform CANCELLA-RIGHE3
              display message "Cancellazione avenuta con successo!"
                        title titolo
              move 27 to key-status
      *****        set RigaCambiata to false
              initialize btno-rec 
                         old-btno-rec replacing numeric data by zeroes
                                          alphanumeric data by spaces
           end-if.

      ***---
       MODIFICA.
           move 5 to key-status.
           inquire tool-modifica, value in mod.
           set tutto-ok to true.
           
           evaluate pagina
           when 2 inquire form1-gd-1, cursor-y in store-riga
           when 3 inquire form1-gd-2, cursor-y in store-riga
           end-evaluate.

      *  se l'utente e' abilitato puo' modificare un record
           if mod = 1
              set YesMessage to true
              perform CURRENT-RECORD
              if tutto-ok
                 perform SETTA-RIGA
                 perform SETTA-RIGA2
                 perform SETTA-RIGA3
                 move 1 to mod
                 set StatusModifica to true
                 perform STATUS-BAR-MSG      
                 move 78-ID-ef-cli to control-id
              end-if
           else
              move 1 to mod
              perform SALV-MOD            
              move 0 to mod
              if errori
                 move 1 to mod
              else
                 unlock btnotacr all records
                 set NoMessage to true
                 perform CURRENT-RECORD
                 if tutto-ok        
                    perform SETTA-RIGA
                    perform SETTA-RIGA2
                    perform SETTA-RIGA3
                 end-if
                 set StatusVisua to true
                 perform STATUS-BAR-MSG
              end-if
           end-if.
                                
           perform DISPLAY-SCREEN.
           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.

           move 4 to accept-control.
           move 0 to StatusHelp.
           perform STATUS-HELP.

      ***---
       SALV-MOD.
           set tutto-ok to true.
           perform FORM1-CONTROLLO-OLD.

           if NoSalvato or YesDeleted or NewRow or RigaCambiata
              display message box MSG-Salvare-le-modifiche
                            title titolo
                            type mb-yes-no-cancel 
                            giving scelta       
       
              evaluate scelta
              when mb-yes 
                   perform SALVA
              when mb-no  
                   continue
              when other                
                   perform CANCELLA-COLORE
                   set errori to true
                   move store-id to CONTROL-ID       
                   move 4        to ACCEPT-CONTROL   
              end-evaluate

           end-if.

      ***---
       CURRENT-RECORD.
           move LinkAnno of LinkChiave to btno-anno.
           move LinkNumero             to btno-numero.

           set tutto-ok  to true.
           set ReadSecca to true.
           if mod = 1
              read btnotacr lock invalid
                   set errori to true
              end-read
           else
              read btnotacr no lock invalid
                   set errori to true
              end-read
           end-if.
           set ReadSecca to false.
           move btno-causale to link-TipoNota.
           
           if RecLocked
              set RecLocked to false
              set errori    to true
           end-if.

           if tutto-ok
              move "00"          to status-btnotacr
              move riga          to store-riga
              perform FORM1-FLD-TO-BUF
              perform VALORIZZA-SCREEN
              perform CARICA-GRID
              perform CARICA-GRID2
              perform CARICA-GRID3
           else
              move 0 to mod
              move 0 to mod-k
              modify tool-modifica, value = mod
           end-if.
           perform ABILITAZIONI.
           perform VALORIZZA-OLD.
           
           set YesDeleted   to false.
           set NewRow       to false.
           set RigaCambiata to false.

      ***---
       CARICA-GRID.
           modify form1-gd-1, reset-grid 1.
           perform FORM1-GD-1-CONTENT.

           modify form1-gd-1, mass-update = 1.
           move low-value   to brno-rec.
           move btno-chiave to brno-chiave.
           set  brno-nota   to true.
           start brnotacr key is >= brno-chiave
                 invalid continue
             not invalid
                 move 2 to riga
                 perform until 1 = 2
                    read brnotacr next no lock 
                         at end exit perform 
                    end-read
                    if brno-anno   not = btno-anno   or
                       brno-numero not = btno-numero or
                       brno-addebito or brno-merce-rotta
                       exit perform
                    end-if
                    perform AGGIUNGI-RIGA     
                    compute tot-doc =
                            tot-doc +
                            brno-qta * ( brno-imponib-merce +
                                         brno-imp-consumo   +
                                         brno-imp-cou-cobat +
                                         brno-add-piombo    )
                 end-perform
           end-start.
           modify form1-gd-1, mass-update = 0.

      ***---
       AGGIUNGI-RIGA.
           move brno-num-riga     to col-riga.
           move brno-cod-articolo to col-codice art-codice.
           read articoli no lock invalid continue end-read.

           move art-descrizione    to col-descr.
           move brno-qta           to col-quantita.
           move brno-prz-unitario  to col-prezzo.
           move brno-imponib-merce to col-imp-merce.
           move brno-imp-consumo   to col-consumo.
           move brno-imp-cou-cobat to col-coubat.
           move brno-add-piombo    to col-add.
           move brno-cod-iva       to col-cod-iva.

           modify form1-gd-1(riga, 1),  cell-data = col-riga.
           modify form1-gd-1(riga, 2),  cell-data = col-codice.
           modify form1-gd-1(riga, 3),  cell-data = col-descr.
           modify form1-gd-1(riga, 4),  cell-data = col-quantita.
           modify form1-gd-1(riga, 5),  cell-data = col-prezzo.  
           modify form1-gd-1(riga, 6),  cell-data = col-imp-merce.
           modify form1-gd-1(riga, 7),  cell-data = col-consumo.
           modify form1-gd-1(riga, 8),  cell-data = col-coubat.
           modify form1-gd-1(riga, 9),  cell-data = col-add.
           modify form1-gd-1(riga, 10), cell-data = col-cod-iva.

           move 0 to hidden-sel.
           move brno-prg-chiave      to HiddenKey.
           move brno-peso-utf        to HiddenUtf.
           move brno-peso-non-utf    to HiddenNonUtf.

           move col-quantita   to hid-old-qta.
           move col-prezzo     to hid-old-prz.
           move col-consumo    to hid-old-consumo.
           move col-coubat     to hid-old-coubat.
           move col-add        to hid-old-piombo.
           move col-cod-iva    to hid-old-iva.
           move HiddenUtf      to hid-peso-utf.
           move HiddenNonUtf   to hid-peso-non-utf.
           move brno-tipo-riga to hid-tipo-riga.

           modify form1-gd-1(riga, 1), hidden-data = HiddenValori.
           modify form1-gd-1(riga, 2), hidden-data = HiddenKey.
           modify form1-gd-1(riga, 3), hidden-data = HiddenPeso.
           add 1 to riga.

      ***---
       CANCELLA-RIGHE.
           move btno-chiave to brno-chiave.
           move low-value   to brno-num-riga.
           set  brno-nota   to true.

           start brnotacr key is >= brno-chiave
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read brnotacr next at end exit perform end-read
                    if btno-anno   not = brno-anno   or
                       btno-numero not = brno-numero or
                       brno-addebito or brno-merce-rotta
                       exit perform
                    end-if
                    delete brnotacr record invalid continue end-delete
                 end-perform
           end-start.

      ***---
       CANCELLA-RIGHE2.
           move btno-chiave   to brno-chiave.
           move low-value     to brno-num-riga.
           set  brno-addebito to true.

           start brnotacr key is >= brno-chiave
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read brnotacr next at end exit perform end-read
                    if btno-anno   not = brno-anno   or
                       btno-numero not = brno-numero or
                       brno-nota or brno-merce-rotta
                       exit perform
                    end-if
                    delete brnotacr record invalid continue end-delete
                 end-perform
           end-start.

      ***---
       CANCELLA-RIGHE3.
           move btno-chiave      to brno-chiave.
           move low-value        to brno-num-riga.
           set  brno-merce-rotta to true.

           start brnotacr key is >= brno-chiave
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read brnotacr next at end exit perform end-read
                    if btno-anno   not = brno-anno   or
                       btno-numero not = brno-numero or
                       brno-nota   or brno-addebito
                       exit perform
                    end-if
                    delete brnotacr record invalid continue end-delete
                 end-perform
           end-start.

      ***---
       SETTA-RIGA.
           if pagina = 2
              move store-riga to event-data-2
              move 78-ID-form1-gd-1 to control-id
              move    4 to accept-control
              modify form1-gd-1, cursor-y = event-data-2
                                 cursor-x = event-data-1
              perform SPOSTAMENTO
      *****        set MousePressed     to false
              set ControllaCampi   to false
              set ArticoloSetFocus to false
           end-if.

      ***---
       SETTA-RIGA2.
           inquire form1-gd-2, last-row in tot-righe.
           if tot-righe > 1
              if pagina = 3
                 move store-riga to event-data-2
                 move 78-ID-form1-gd-2 to control-id
                 move    4 to accept-control
                 modify form1-gd-2, cursor-y = event-data-2
                                    cursor-x = event-data-1
                 perform SPOSTAMENTO2
      *****        set MousePressed     to false
                 set ControllaCampi   to false
                 set ArticoloSetFocus to false
              end-if
           end-if.

      ***---
       SETTA-RIGA3.
           inquire form1-gd-3, last-row in tot-righe.
           if tot-righe > 1
              if pagina = 4
                 move store-riga to event-data-2
                 move 78-ID-form1-gd-3 to control-id
                 move    4 to accept-control
                 modify form1-gd-3, cursor-y = event-data-2
                                    cursor-x = event-data-1
                 perform SPOSTAMENTO3
      *****        set MousePressed     to false
                 set ControllaCampi   to false
                 set ArticoloSetFocus to false
              end-if
           end-if.

      ***---
       STATUS-BAR-MSG.
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
           end-evaluate.

      ***--
       CARICA-GRID2.
           perform RIEMPI-GRID-ADDEBITO.
           modify form1-gd-2, reset-grid 1.
           perform FORM1-GD-2-CONTENT.

           modify form1-gd-2, mass-update = 1.
           move  low-value     to brno-rec.
           move  btno-chiave   to brno-chiave.
           set   brno-addebito to true.
           start brnotacr key  is >= brno-chiave
                 invalid continue
             not invalid
                 move 2 to riga
                 perform until 1 = 2
                    read brnotacr  next no lock
                         at end exit perform 
                    end-read
                    if brno-anno   not = btno-anno   or
                       brno-numero not = btno-numero or
                       brno-nota   or brno-merce-rotta
                       exit perform
                    end-if
                    perform AGGIUNGI-RIGA2
                 end-perform
           end-start.
           modify form1-gd-2, mass-update = 0.

      ***--
       CARICA-GRID3.
           perform RIEMPI-GRID-MERCE-ROTTA.
           modify form1-gd-3, reset-grid 1.
           perform FORM1-GD-3-CONTENT.

           modify form1-gd-3, mass-update = 1.
           move  low-value        to brno-rec.
           move  btno-chiave      to brno-chiave.
           set   brno-merce-rotta to true.
           start brnotacr key  is >= brno-chiave
                 invalid continue
             not invalid
                 move 2 to riga
                 perform until 1 = 2
                    read brnotacr  next no lock
                         at end exit perform 
                    end-read
                    if brno-anno   not = btno-anno   or
                       brno-numero not = btno-numero or
                       brno-nota   or brno-addebito
                       exit perform
                    end-if
                    perform AGGIUNGI-RIGA3
                 end-perform
           end-start.
           modify form1-gd-3, mass-update = 0.

      ***---
       AGGIUNGI-RIGA2.
           move brno-num-riga     to col-riga.
           move brno-cod-articolo to col-codice art-codice.
           read articoli no lock invalid continue end-read.

           move art-descrizione    to col-descr.
           move brno-qta           to col-quantita.
           move brno-prz-unitario  to col-prezzo.
           move brno-imponib-merce to col-imp-merce.
           move brno-imp-consumo   to col-consumo.
           move brno-imp-cou-cobat to col-coubat.
           move brno-add-piombo    to col-add.
           move brno-cod-iva       to col-cod-iva.

           modify form1-gd-2(riga, 1),  cell-data = col-riga.
           modify form1-gd-2(riga, 2),  cell-data = col-codice.
           modify form1-gd-2(riga, 3),  cell-data = col-descr.

           modify form1-gd-2(riga, 4),  cell-data = col-quantita.
           modify form1-gd-2(riga, 5),  cell-data = col-prezzo.
           modify form1-gd-2(riga, 6),  cell-data = col-imp-merce.
           modify form1-gd-2(riga, 7),  cell-data = col-consumo.
           modify form1-gd-2(riga, 8),  cell-data = col-coubat.
           modify form1-gd-2(riga, 9),  cell-data = col-add.
           modify form1-gd-2(riga, 10), cell-data = col-cod-iva.

           move brno-qta      to hid-qta-nc.

           move col-codice    to hid-old-art.
           move col-quantita  to hid-old-qta.
           move col-prezzo    to hid-old-prz.
           move col-consumo   to hid-old-consumo.
           move col-coubat    to hid-old-coubat.
           move col-add       to hid-old-piombo.
           move col-cod-iva   to hid-old-iva.

           move brno-tipo-riga to hid-tipo-riga.
           move 0 to hidden-sel.

           modify form1-gd-2(riga, 1), hidden-data = HiddenValori.
           move brno-prg-chiave      to HiddenKey.
           move brno-peso-utf        to HiddenUtf.
           move brno-peso-non-utf    to HiddenNonUtf.
           modify form1-gd-2(riga, 2), hidden-data = HiddenKey.
           modify form1-gd-2(riga, 3), hidden-data = HiddenPeso.
           add 1 to riga.

      ***---
       AGGIUNGI-RIGA3.
           move brno-num-riga     to col-riga.
           move brno-cod-articolo to col-codice art-codice.
           read articoli no lock invalid continue end-read.

           move art-descrizione    to col-descr.
           move brno-qta           to col-quantita.
           move brno-prz-unitario  to col-prezzo.
           move brno-imponib-merce to col-imp-merce.
           move brno-imp-consumo   to col-consumo.
           move brno-imp-cou-cobat to col-coubat.
           move brno-add-piombo    to col-add.
           move brno-cod-iva       to col-cod-iva.

           modify form1-gd-3(riga, 1),  cell-data = col-riga.
           modify form1-gd-3(riga, 2),  cell-data = col-codice.
           modify form1-gd-3(riga, 3),  cell-data = col-descr.

           modify form1-gd-3(riga, 4),  cell-data = col-quantita.
           modify form1-gd-3(riga, 5),  cell-data = col-prezzo.
           modify form1-gd-3(riga, 6),  cell-data = col-imp-merce.
           modify form1-gd-3(riga, 7),  cell-data = col-consumo.
           modify form1-gd-3(riga, 8),  cell-data = col-coubat.
           modify form1-gd-3(riga, 9),  cell-data = col-add.
           modify form1-gd-3(riga, 10), cell-data = col-cod-iva.

           move brno-qta      to hid-qta-nc.

           move col-codice    to hid-old-art.
           move col-quantita  to hid-old-qta.
           move col-prezzo    to hid-old-prz.
           move col-consumo   to hid-old-consumo.
           move col-coubat    to hid-old-coubat.
           move col-add       to hid-old-piombo.
           move col-cod-iva   to hid-old-iva.

           move brno-tipo-riga to hid-tipo-riga.
           move 0 to hidden-sel.

           modify form1-gd-3(riga, 1), hidden-data = HiddenValori.
           move brno-prg-chiave      to HiddenKey.
           move brno-peso-utf        to HiddenUtf.
           move brno-peso-non-utf    to HiddenNonUtf.
           modify form1-gd-3(riga, 2), hidden-data = HiddenKey.
           modify form1-gd-3(riga, 3), hidden-data = HiddenPeso.
           add 1 to riga.

      ***---
       ABILITAZIONI.
           if btno-data-fm not = 0 or
              btno-num-fm  not = 0 or
              btno-data-nc not = 0 or
              btno-num-nc  not = 0 or livello-abil = 1
              move BitmapEditDisabled to BitmapNumEdit
              move 0 to e-modifica
           else
              move BitmapEditEnabled to BitmapNumEdit
              move 1 to e-modifica
           end-if.

           if livello-abil = 2 
              move 0 to e-cancella
              move BitmapDeleteDisabled to BitmapNumDelete
           end-if.

           modify tool-modifica,
                  enabled e-modifica,
                  bitmap-number = BitmapNumEdit.

           if mod = 1
              move BitmapDeleteEnabled to BitmapNumDelete
              move BitmapSaveEnabled   to BitmapNumSave
              move 1 to e-salva e-cancella

              move 1 to mod-campi
              move 5 to NumBitmapNuovoGrid
              move 4 to NumBitmapEliminaGrid

           else
              move 8 to NumBitmapNuovoGrid
              move 7 to NumBitmapEliminaGrid
              move BitmapDeleteDisabled to BitmapNumDelete
              move BitmapSaveDisabled   to BitmapNumSave
              move 0 to e-salva e-cancella
              move 0 to mod-campi
           end-if.
           move BitmapNewDisabled     to BitmapNumNew.

           modify tool-nuovo,    enabled       = 0
                                 bitmap-number = BitmapNumNew.
           modify tool-cancella, enabled       = e-cancella,
                                 bitmap-number = BitmapNumDelete.
           modify tool-salva,    enabled       = e-salva,
                                 bitmap-number = BitmapNumSave.

      ***---
       TORNA-IN-VISUA.
           move 0 to mod.
           move 78-ID-ef-cli to control-id.
           set NoMessage to true.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG.
           unlock btnotacr all records.

           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.
           move 4 to ACCEPT-CONTROL.

           perform INIT.
           perform DISPLAY-SCREEN.
           
           set YesDeleted   to false.
           set NewRow       to false.
           set RigaCambiata to false.
