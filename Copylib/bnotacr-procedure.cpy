      ***---
       LEGGI-TESTATA.
           move tor-cod-cli  to cli-codice.
           set cli-tipo-C    to true.
           read clienti  invalid continue end-read.

           if tor-prg-destino not = 0
              move tor-cod-cli     to des-codice
              move tor-prg-destino to des-prog
              read destini invalid continue end-read
           end-if.

           move tor-vettore to vet-codice.
           read tvettori invalid continue end-read.

           initialize record-tblpa.
           move "PA"              to tblpa-codice1.
           move 78-cod-pag-def    to tblpa-codice2.
           read tcodpag no lock invalid continue end-read.
           perform MOVE-DESCR-PAG.
           
      ***---
       VALORIZZA-SCREEN.
           move tor-data-fattura to como-data.
           perform DATE-TO-SCREEN.

           if LinkNumero not = 0 
              move LinkNumero to lab-num-buf
              move como-data to lab-data-buf
           end-if.

           move esercizio       to lab-anno-nc-buf.

           move data-oggi       to como-data.
           perform DATE-TO-SCREEN.
           move como-data       to lab-data-nc-buf.

           move tor-cod-cli     to codice-ed.
           move codice-ed       to ef-cli-buf.
           call "C$JUSTIFY"  using ef-cli-buf, "L".

           move tor-prg-destino to codice-ed.
           move codice-ed       to ef-des-buf.
           call "C$JUSTIFY"  using ef-des-buf, "L".

           move cli-ragsoc-1    to lab-cli-buf.
           move cli-indirizzo   to lab-ind-buf.
           move cli-localita    to lab-loca-buf.

           move des-ragsoc-1    to lab-des-buf.
           move des-indirizzo   to lab-ind-d-buf.
           move des-localita    to lab-loca-d-buf.

           move vet-codice      to ef-vet-buf.
           move vet-descrizione to lab-vet-buf.

           move 78-cod-pag-def to ef-pag-buf . 

      ***---
       CARICA-GRID.
           perform RESET-GRIGLIA.

           modify gd-fatt, mass-update = 1.
           move low-value  to ror-rec.
           move tor-chiave to ror-chiave.
           start rordini key is >= ror-chiave
                 invalid continue
             not invalid
                 move 2 to riga
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    subtract ror-qta-omaggi from ror-qta
                    perform AGGIUNGI-RIGA
OMAGGI              if ror-qta-omaggi not = 0
OMAGGI                 move ror-qta-omaggi   to ror-qta
OMAGGI                 move 0                to ror-prz-unitario
OMAGGI                 move 0                to ror-perce-sconto
OMAGGI                 move 0                to ror-imp-consumo
OMAGGI                 move 0                to ror-imp-cou-cobat
OMAGGI                 move 0                to ror-imponib-merce
OMAGGI                 move tge-cod-iva-omag to ror-cod-iva
OMAGGI                 set  ror-si-omaggio   to true
OMAGGI                 perform AGGIUNGI-RIGA
OMAGGI              end-if
                 end-perform
           end-start.
           modify gd-fatt, mass-update = 0.

      ***---
       AGGIUNGI-RIGA.
           move ror-num-riga     to col-num.
           move ror-cod-articolo to col-art art-codice.
           read articoli no lock invalid continue end-read.

           move art-descrizione   to col-des.
           move ror-qta           to col-qta.
           move ror-prz-unitario  to col-uni.
           move ror-perce-sconto  to col-sconto.
           move ror-imp-consumo   to col-cons.
           move ror-imp-cou-cobat to col-cou.
           move ror-add-piombo    to col-add-pb.
           move ror-imponib-merce to col-imp.
           move ror-cod-iva       to col-iva.

           modify gd-fatt(riga, 1),  cell-data = col-num.
           modify gd-fatt(riga, 2),  cell-data = col-art.
           modify gd-fatt(riga, 3),  cell-data = col-des.
           modify gd-fatt(riga, 4),  cell-data = col-qta.
           modify gd-fatt(riga, 5),  cell-data = col-uni.
           modify gd-fatt(riga, 6),  cell-data = col-sconto.
           modify gd-fatt(riga, 7),  cell-data = col-cons.
           modify gd-fatt(riga, 8),  cell-data = col-cou.
           modify gd-fatt(riga, 9),  cell-data = col-add-pb.
           modify gd-fatt(riga, 10), cell-data = col-imp.
           modify gd-fatt(riga, 11), cell-data = col-iva.

           if ror-si-omaggio
              modify gd-fatt(riga, 12),  
                     bitmap = conferma-bmp
                     bitmap-number = 1
                     bitmap-width  = 19
           else
              modify gd-fatt(riga, 12),  
                     bitmap = conferma-bmp
                     bitmap-number = 2
                     bitmap-width  = 19
           end-if.
           move 0 to hidden-sel.
           move ror-prg-chiave      to HiddenKey.
           move ror-peso-utf        to HiddenUtf.
           move ror-peso-non-utf    to HiddenNonUtf.
           modify gd-fatt(riga, 1), hidden-data = hidden-sel.
           modify gd-fatt(riga, 2), hidden-data = HiddenKey.
           modify gd-fatt(riga, 3), hidden-data = HiddenPeso.
           add 1 to riga. 

      ***---
       PB-GENERA-PRESSED.
           set trovato        to false.
           modify form1-gd-1, reset-grid = 1.
           perform FORM1-GD-1-CONTENT.

           modify form1-gd-1, mass-update = 1.
           inquire gd-fatt, last-row in tot-righe.
           move 0 to hid-old-prz.
           move 0 to LastPrg.
           move 2 to store-riga.
           move 0 to riga-nuova.
           move Link-TipoNota to tca-codice.
           read tcaumag no lock invalid continue end-read.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire gd-fatt(riga, 1), hidden-data in hidden-sel
              inquire gd-fatt(riga, 2), hidden-data in HiddenKey
              inquire gd-fatt(riga, 3), hidden-data in HiddenPeso
              if hidden-sel = 1
                 add 1 to LastPrg
                 move LastPrg to col-riga
                 inquire gd-fatt(riga,  2), cell-data in col-art
                 inquire gd-fatt(riga,  3), cell-data in col-des
                 inquire gd-fatt(riga,  4), cell-data in col-qta
                 inquire gd-fatt(riga,  5), cell-data in col-uni
                 inquire gd-fatt(riga,  7), cell-data in col-cons
                 inquire gd-fatt(riga,  8), cell-data in col-cou
                 inquire gd-fatt(riga,  9), cell-data in col-add-pb
                 inquire gd-fatt(riga, 10), cell-data in col-imp
                 inquire gd-fatt(riga, 11), cell-data in col-iva
                 
                 move col-art    to col-codice
                 move col-des    to col-descr
                 move col-qta    to col-quantita

                 if tca-tipo-nota-prz
                    move col-cons   to ror-imp-consumo
                    move col-cou    to ror-imp-cou-cobat
                    move col-add-pb to ror-add-piombo
                    move col-imp    to ror-imponib-merce
                    compute como-prz = ror-imp-consumo    +
                                       ror-imp-cou-cobat  +
                                       ror-add-piombo     +
                                       ror-imponib-merce
                    move como-prz to col-prezzo col-imp-merce
                    move 0 to col-consumo col-coubat col-add
                 else
                    move col-uni    to col-prezzo
                    move col-imp    to col-imp-merce
                    move col-cons   to col-consumo
                    move col-cou    to col-coubat
                    move col-add-pb to col-add
                 end-if
LUBEXX           move col-iva    to col-cod-iva

                 modify form1-gd-1(store-riga, 1), cell-data col-riga
                 modify form1-gd-1(store-riga, 2), cell-data col-codice
                 modify form1-gd-1(store-riga, 3), cell-data col-descr
                 modify form1-gd-1(store-riga, 4), 
                        cell-data col-quantita
                 modify form1-gd-1(store-riga, 5), cell-data col-prezzo
                 modify form1-gd-1(store-riga, 6), 
                        cell-data col-imp-merce
                 modify form1-gd-1(store-riga, 7), cell-data col-consumo
                 modify form1-gd-1(store-riga, 8), cell-data col-coubat 
                 modify form1-gd-1(store-riga, 9), cell-data col-add
                 modify form1-gd-1(store-riga, 10),cell-data col-cod-iva

                 move col-codice   to hid-old-art
                 move col-qta      to hid-qta-nc
                 move col-uni      to hid-old-prz
                 move col-cons     to hid-old-consumo
                 move col-cou      to hid-old-coubat
                 move col-add-pb   to hid-old-piombo
                 move col-cod-iva  to hid-old-iva
                 move HiddenUtf    to hid-peso-utf
                 move HiddenNonUtf to hid-peso-non-utf

                 if tca-tipo-nota-reso move "F" to hid-tipo-riga
                 else                  move "M" to hid-tipo-riga
                 end-if

                 modify form1-gd-1(store-riga, 1),
                        hidden-data = HiddenValori
                 modify form1-gd-1(store-riga, 2),
                        hidden-data = HiddenKey
                 set trovato to true
                 add 1 to store-riga
              end-if
           end-perform.
           modify form1-gd-1, mass-update = 0.
           perform INITIALIZE-ENTRY.

           if trovato
              perform CANCELLA-COLORE
              move 78-ID-ef-art to control-id
              move 4            to accept-control
              move 2 to event-data-2
              perform SPOSTAMENTO
           else
              perform PB-NUOVO-LINKTO
           end-if.

      ***---
       PB-SEL-TUTTO-PRESSED.
           inquire gd-fatt, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              move riga to event-data-2
              move 0 to hidden-sel
              modify gd-fatt(riga, 1), hidden-data = hidden-sel 
              perform SELEZIONA-RIGA
           end-perform.

      ***---
       RESET-GRIGLIA.
           modify gd-fatt, reset-grid = 1.
           perform INTESTAZIONE.

      ***---
       CURRENT-RECORD.
           read tordini
                invalid |Valorizzo per lo meno la data di sistema...
                        accept como-data from century-date
                        perform DATE-TO-SCREEN
                        move como-data   to lab-data-nc-buf
                        |... l'anno di esercizio, ...
                        move esercizio   to lab-anno-nc-buf
                        |... il codice pagamento standard ...
                        move "PA"            to tblpa-codice1
                        move 78-cod-pag-def  to tblpa-codice2 ef-pag-buf
                        read tcodpag no lock invalid continue end-read
                        perform MOVE-DESCR-PAG
                        |... e muovo a blank codice e destino.
                        move spaces to ef-cli-buf ef-des-buf
                        perform INTESTAZIONE
            not invalid perform LEGGI-TESTATA
                        perform VALORIZZA-SCREEN
                        perform CARICA-GRID
           end-read.
           move tor-rec to save-tor-rec.
           move 1 to form1-radio-1-buf.
           perform FORM1-BUF-TO-FLD.
           perform VALORIZZA-OLD. 

      ***---
       INTESTAZIONE.
           perform FORM1-GD-1-CONTENT.
           perform GD-FATT-CONTENT.

      ***---
       SELEZIONA-RIGA.
           inquire gd-fatt, last-row in tot-righe.
           move event-data-2 to riga.

           if riga >= 2        and 
              riga <= tot-righe                                  
              inquire gd-fatt(riga, 1), hidden-data in hidden-sel
              inquire gd-fatt(riga, 5),   cell-data in brno-prz-unitario
              if hidden-sel = 1
                 move 0 to hidden-sel
                 modify gd-fatt(riga), row-color = 516
              else                   
                 move 1 to hidden-sel
                 modify gd-fatt(riga), row-color = 176
              end-if
              modify gd-fatt(riga, 1), hidden-data = hidden-sel
           end-if . 

      ***---
       SPOSTAMENTO-GD-FATT.
           inquire gd-fatt, last-row in tot-righe.
           move event-data-2 to riga .       

      ***---
       ABILITAZIONI.
           modify tool-modifica,   value 1.
           move BitmapNewDisabled     to BitmapNumNew.
           move BitmapDeleteDisabled  to BitmapNumDelete.
           move BitmapEditDisabled    to BitmapNumEdit.
           move BitmapPrintDisabled   to BitmapNumPrint.
           move BitmapPreviewDisabled to BitmapNumPreview.
           move BitmapSelectDisabled  to BitmapNumSelect.

           if livello-abil > 1 move BitmapSaveEnabled  to BitmapNumSave
                               move 1                  to e-salva
           else                move BitmapSaveDisabled to BitmapNumSave
                               move 0                  to e-salva
           end-if.

           move 0 to e-cancella 
                     e-anteprima 
                     e-stampa 
                     e-nuovo
                     e-seleziona
                     e-modifica.

           if e-cerca = 0 move BitmapZoomDisabled to BitmapNumZoom
           else           move BitmapZoomEnabled  to BitmapNumZoom
           end-if.

           perform DISPLAY-SCREEN.
                                                      
           modify tool-nuovo,     enabled = e-nuovo,
                                  bitmap-number = BitmapNumNew.
           modify tool-cancella,  enabled = e-cancella,
                                  bitmap-number = BitmapNumDelete.
           modify tool-modifica,  enabled = e-modifica,
                                  bitmap-number = BitmapNumEdit.
           modify tool-anteprima, enabled = e-anteprima,
                                  bitmap-number = BitmapNumPreview.
           modify tool-stampa,    enabled = e-stampa,
                                  bitmap-number = BitmapNumPrint.
           modify tool-seleziona, enabled = e-seleziona,
                                  bitmap-number = BitmapNumSelect.
           modify tool-salva,     enabled = e-salva,
                                  bitmap-number = BitmapNumSave.  
           modify tool-cerca,     enabled = e-cerca,
                                  bitmap-number = BitmapNumZoom .

      * FITTIZZI --> NON TOCCARE!!!
      ***---
       CANCELLA-RIGHE .
       CANCELLA-RIGHE2.
       CANCELLA-RIGHE3.
       TORNA-IN-VISUA.
       CARICA-GRID2.
