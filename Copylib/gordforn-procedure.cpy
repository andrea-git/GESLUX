      ***---
       BEFORE-ACCEPT-PROCEDURE.
           set MousePressed to false.
           perform LEGGI-ANNO.
           perform INIT.
           perform INIT-OLD-REC.
           perform ABILITA-TOOLBAR.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG. 
           accept data-oggi from century-date. 
           accept imp-data  from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.
           move 0        to volantino-forzato.
           move spaces   to lab-forzato-buf.
           move 0        to v-gest-plus.
                                                
           move 0 to art-ordforn-anno.
           move 0 to art-ordforn-numero.
           move art-ordforn-chiave to aor-chiave-testa.
           move low-value to aor-prog.
           perform ARTICOLI-DA-CONFERMARE.
           if tof-da-confermare-si move 1 to v-articoli
           else                    move 0 to v-articoli
           end-if.
           display lab-articoli.

      ***---
       AFTER-BUF-TO-FLD-PROCEDURE.
           move ef-data-buf           to como-data.
           perform DATE-TO-FILE.
           move como-data             to tof-data-ordine.

           move ef-data-cons-buf      to como-data.
           perform DATE-TO-FILE.
           move como-data             to tof-data-consegna.

      *     move ef-data-arrivo-buf    to como-data.
      *     perform DATE-TO-FILE.
      *     move como-data             to tof-data-arr-merce.

           move ef-data-listino-buf   to como-data.
           perform DATE-TO-FILE.
           move como-data             to tof-data-listino.

      *
           move ef-cli-buf            to tof-cod-forn     convert.  
           move ef-dest-buf           to tof-destino      convert.  
           
           accept como-ora          from time.
                          
           move data-oggi             to tof-data-creazione.                  
           move como-ora              to tof-ora-creazione.
           move user-codi             to tof-utente-creazione.
                                    
           set tof-manuale   to true
           set tof-inserito to true
           set tof-inevaso to true
           move space to tof-tipo-chiusura
                         tof-tipo-invio

           initialize tof-pz-tot
                      tof-pz-arrivati
                      tof-dati-invio
                      tof-dati-chiusura replacing numeric data by zeroes
                                        alphanumeric data by spaces.

      ***---
       AFTER-FLD-TO-BUF-PROCEDURE.
      *                          
           perform VALORIZZA-OLD.
      *                            
           move tof-cod-forn to  codice-ed.
           move codice-ed    to  ef-cli-buf.   
           call "C$JUSTIFY" using ef-cli-buf, "L".
           display ef-cli.   
           
           move tof-destino  to  codice-ed.
           move codice-ed    to  ef-dest-buf.   
           call "C$JUSTIFY" using ef-dest-buf, "L".
           display ef-dest.   

           perform ABILITAZIONI.     
 
      ***----
       ABILITAZIONI.
      *     modify cbo-stato,     enabled mod.
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
                           

           if mod = 1
      *        inquire cbo-stato,   value cbo-stato-buf
      *        if cbo-stato-buf not = "Disattivo" 
                 move 1 to mod-campi
                 move 1 to NumBitmapArticoli
      *        else
      *           move 0 to mod-campi
      *        end-if
              move 0 to mod-k
           else
              move 1 to mod-k
              move 0 to mod-campi
              move 3 to NumBitmapArticoli
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

      ****----
      * CHANGE-STATUS.
      *     evaluate nome-file
      *     when "clienti"
      *          set disattivo to true
      *          perform CARICA-COMBO-STATO
      *          |specifico per i pgm. aventi Tab-Control
      *          move 1 to Screen1-Ta-1-TAB-VALUE
      *          perform SCREEN1-TA-1-TABCHANGE
      *          |******
      *          move 1 to mod
      *          move 0 to mod-campi
      *          perform DISPLAY-SCREEN
      *     end-evaluate.

      ***----
       CLEAR-SCREEN.
      *     move cli-codice to old-cli-codice.
           initialize tof-rec  
                      cli-rec of clienti
                      cli-rec of clienti1
                      des-rec
                      desf-rec
                      como-note(1)
                      como-note(2)
                      como-note(3)
                      como-note(4)
                      como-descr-promo
                      rof-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
                        
           perform FORM1-FLD-TO-BUF. 
      
           perform INIT-OLD-REC.

           move spaces to lab-iva-buf 
                          lab-ese-iva-buf 
                          lab-pag-buf 

                          
      
           perform DISPLAY-SCREEN.

      ***----
       COLORE.
           modify form1-gd-1, start-y = riga, y = riga,
                              start-x = 2,    x = 16,
                              region-color = 144.

      ***----
       DISPLAY-SCREEN.
           display form1.


      ***----
       DISPLAY-SCREEN-MANUALE.
           modify form1-gd-1, reset-grid  = 1.
           perform INTESTAZIONE.
           set FirstTime to false.
           set lab-imp-coubat to true.
           display lab-imposta.

      ***----
       ENTRY-TO-ROW.
           inquire form1-gd-1, last-row in tot-righe.
           inquire ef-art, value in art-codice.

           if NewRow
              display campi-grid|ef-cons ef-cou     ef-sconto |ef-qta-oma
                      |ef-qta  ef-cod-iva ef-uni    ef-imp lab-iva-2
              add 1 to tot-righe    giving riga
              move art-codice to old-art-codice
              modify form1-gd-1, insert-rows 1
              add 1        to LastPrg
              move LastPrg to col-num     
              perform COLORE
           else
              inquire form1-gd-1, cursor-y in riga        
           end-if. 

           if link-imballo-saved = 1
              move lab-art-buf  to col-des
              move ef-qta-buf   to col-qta
              move 0            to link-imballo-saved
           end-if. 

           move ef-imb-ord-buf  to hid-rof-imb-ordinato

           move tbliv-codice2      to hid-rof-cod-iva
                                      col-iva
           move ef-art-BUF         to hid-rof-cod-articolo
                                      col-art
           move ef-imb-ord-BUF     to hid-rof-imb-ordinato
           move ef-qta-BUF         to hid-rof-qta-ord
                                      col-qta
           move ef-uni-BUF         to hid-rof-prz-unitario
                                      col-uni
           move ef-sconto-1-BUF    to hid-rof-sconto-1
                                      col-sconto-1
           move ef-sconto-2-BUF    to hid-rof-sconto-2
                                      col-sconto-2
           move ef-sconto-3-BUF    to hid-rof-sconto-3
                                      col-sconto-3
           move ef-sconto-4-BUF    to hid-rof-sconto-4
                                      col-sconto-4
           move ef-sconto-5-BUF    to hid-rof-sconto-5
                                      col-sconto-5
           move ef-cons-BUF        to hid-rof-imp-consumo
                                      col-consumo
           move ef-cou-BUF         to hid-rof-imp-cou-cobat
                                      col-cou
           move ef-add-BUF         to hid-rof-add-piombo
                                      col-add
           move ef-costi-agg-BUF   to hid-rof-costi-aggiuntivi
                                      col-costi-agg
           move ef-imp-BUF         to hid-rof-imponib-merce
                                      col-imp
           move ef-cod-iva-BUF     to hid-rof-cod-iva
           move chk-manuale-BUF    to hid-rof-manuale.
           move ef-impforn-buf     to hid-rof-imf-codice.

           move rlis-codice        to hid-rof-cod-listino

           move lab-art-buf        to col-des

           modify form1-gd-1(riga,  1) cell-data col-num.
           modify form1-gd-1(riga,  2) cell-data col-art.
           modify form1-gd-1(riga,  3) cell-data col-des.
           modify form1-gd-1(riga,  4) cell-data col-qta.
           modify form1-gd-1(riga,  5) cell-data col-uni.
           modify form1-gd-1(riga,  6) cell-data col-sconto-1.
           modify form1-gd-1(riga,  7) cell-data col-sconto-2.
           modify form1-gd-1(riga,  8) cell-data col-sconto-3.
           modify form1-gd-1(riga,  9) cell-data col-sconto-4.
           modify form1-gd-1(riga, 10) cell-data col-sconto-5.
           modify form1-gd-1(riga, 11) cell-data col-imp.
           modify form1-gd-1(riga, 12) cell-data col-consumo.
           modify form1-gd-1(riga, 13) cell-data col-cou.
           modify form1-gd-1(riga, 14) cell-data col-add.
           modify form1-gd-1(riga, 15) cell-data col-costi-agg.
           modify form1-gd-1(riga, 16) cell-data col-iva.

           modify form1-gd-1(riga, 1), hidden-data hid-rof-rec-1.
           modify form1-gd-1(riga, 2), hidden-data hid-rof-rec-2.
           modify form1-gd-1(riga, 3), hidden-data hid-rof-rec-3.


           move 78-ID-ef-art to control-id.
           move 4            to accept-control 

           modify form1-gd-1, cursor-y = riga.
           set NewRow         to true.
           perform AZZERA-MANUALE.


      ***----
       INIT.
           set FirstTime        to true.
           set NonCambiareTab   to false.
           set ArticoloSetFocus to false.
           set ControllaCampi   to true.
           set CheckAfterZoom   to false.
           move 0 to v-manuale v-guidata form1-radio-1-buf 
                     old-art-codice.
           move 1 to event-data-1 screen1-ta-1-tab-value.
           perform SCREEN1-TA-1-TABCHANGE.
           perform CLEAR-SCREEN.
           move 0 to StatusHelp LastPrg.

           move como-anno to ef-anno-buf.
           display ef-anno.

           move data-oggi to como-data.
           perform DATE-TO-SCREEN.
           move como-data to ef-data-buf |ef-data-pass-buf.
           display ef-data |ef-data-pass.

           move spaces to lab-art-buf lab-pag-buf lab-iva-buf 

           move 0 to e-man e-gui.
           move 1 to form1-radio-1-buf.
           display rb-urgente rb-normale.

           move 0 to LastPrg.

           move 78-ID-ef-cau to control-id.
           move 4 to accept-control.
      *
           |Riempio il tabellino contenente i riferimenti agli ID
           |relativi ad ogni pagina del tab-control
           |specifico per i pgm. aventi Tab-Control
           move 78-ID-ef-data      to min-id(1).
           move 78-ID-ef-note-4    to max-id(1).
           move 78-ID-ef-art       to min-id(2).
           move 78-ID-ef-cod-iva   to max-id(2).
           |*******
      *
      *     Perform RIEMPI-COMBO-STATO.
      *
      ***     move "Attivo" to cbo-stato-buf.
      ***     Modify  cbo-stato,   value cbo-stato-buf.

           move 0 to mod.

           move tge-causale-ord-forn to ef-cau-buf tca-codice.
           read tcaumag invalid continue end-read.
           move tca-descrizione to lab-cau-buf.
           display ef-cau lab-cau.

           set NewRow to true.

      *    Luciano
           initialize tipo-note.
      *    Luciano Fine

      ***----
       LEGGI-ANNO.
           open input tparamge.
           move spaces to tge-codice.
           move 0 to tge-anno.
           read tparamge no lock invalid continue end-read.
           move tge-anno to ef-anno-buf.
           move tge-cod-iva-omag to iva-omaggio tbliv-codice2.
           move "IV"             to tbliv-codice1.
           read tivaese 
              invalid 
                 continue 
           end-read.
           perform MOVE-DESCR-IVA-2.        
           move lab-iva-buf to save-descr-iva.
           move spaces to lab-iva-buf.
           display ef-anno.
           close tparamge.

      ***----
       RESET-GRIGLIA.
           modify form1-gd-1, reset-grid = 1.
           perform FORM1-GD-1-CONTENT.

      ***----
       ROW-TO-ENTRY.
      *    Questo prf. porta i dati da grid a entry-field
      *    quando rec-grd e rec-not sono valorizzati
         
           inquire form1-gd-1(riga,  1) cell-data in col-num.
           inquire form1-gd-1(riga,  2) cell-data in col-art.
           inquire form1-gd-1(riga,  3) cell-data in col-des.
           inquire form1-gd-1(riga,  4) cell-data in col-qta.
           inquire form1-gd-1(riga,  5) cell-data in col-uni.
           inquire form1-gd-1(riga,  6) cell-data in col-sconto-1.
           inquire form1-gd-1(riga,  7) cell-data in col-sconto-2.
           inquire form1-gd-1(riga,  8) cell-data in col-sconto-3.
           inquire form1-gd-1(riga,  9) cell-data in col-sconto-4.
           inquire form1-gd-1(riga, 10) cell-data in col-sconto-5.
           inquire form1-gd-1(riga, 11) cell-data in col-imp.
           inquire form1-gd-1(riga, 12) cell-data in col-consumo.
           inquire form1-gd-1(riga, 13) cell-data in col-cou.
           inquire form1-gd-1(riga, 14) cell-data in col-add.
           inquire form1-gd-1(riga, 15) cell-data in col-costi-agg.
           inquire form1-gd-1(riga, 16) cell-data in col-iva.

           inquire form1-gd-1(riga, 1), hidden-data hid-rof-rec-1.
           inquire form1-gd-1(riga, 2), hidden-data hid-rof-rec-2.
           inquire form1-gd-1(riga, 3), hidden-data hid-rof-rec-3.

           move col-art  to art-codice.
           read articoli no lock invalid continue end-read.
           move tlis-codice to rlis-codice.
           move art-codice  to rlis-articolo.
           read rlistini no lock
                invalid
                move zero to rlis-prz-acq
                             rlis-sconto-1
                             rlis-sconto-2
                             rlis-sconto-3
                             rlis-sconto-4
                             rlis-sconto-5
                             rlis-costi-agg
                             rlis-perce-agg
                             rlis-tipo-tratt-imposte
                             rlis-codice
           end-read.

           move hid-rof-tipo-imballo to imq-codice.
           read timbalqta no lock invalid continue end-read.

           move col-art    to ef-art-buf  old-art-codice art-codice.
           set lab-imp-coubat to true.
           read articoli invalid continue end-read.
           if art-si-cobat
              set lab-imp-cobat to true
           else                        
              set lab-imp-cou   to true
           end-if.
           move col-des    to lab-art-buf.
           move col-qta    to ef-qta-buf rof-qta-ord.
OMAGGI*     move hid-qta-omaggi to ef-qta-oma-buf rof-qta-omaggi.
BLISTR*     move hid-blister to chk-blister-buf.
           move col-uni      to ef-uni-buf old-prezzo rof-prz-unitario.
           move col-sconto-1 to ef-sconto-1-buf rof-sconto-1.
           move col-sconto-2 to ef-sconto-2-buf rof-sconto-2.
           move col-sconto-3 to ef-sconto-3-buf rof-sconto-3.
           move col-sconto-4 to ef-sconto-4-buf rof-sconto-4.
           move col-sconto-5 to ef-sconto-5-buf rof-sconto-5.

           move col-consumo  to ef-cons-buf   rof-imp-consumo.
           move col-cou      to ef-cou-buf    rof-imp-cou-cobat.
           move col-add      to ef-add-buf    rof-add-piombo.
           move col-imp      to ef-imp-buf    rof-imponib-merce.
           move col-iva      to ef-cod-iva-buf tbliv-codice2 rof-cod-iva.
           move spaces       to tbliv-descrizione1 tbliv-descrizione2.
           move "IV"         to tbliv-codice1.

           move hid-rof-costi-aggiuntivi to ef-costi-agg-BUF

           move hid-rof-imb-ordinato  to ef-imb-ord-buf
                                         imq-codice
           move hid-rof-manuale    to chk-manuale-BUF.
           move hid-rof-imf-codice to ef-impforn-buf imf-codice.
           read impforn no lock 
                invalid move spaces to imf-descrizione
           end-read.
           if chk-manuale-buf = 1
              inquire screen1-ta-1, value in pagina
              if pagina = 1 move 0 to v-manuale
              else          move 1 to v-manuale
              end-if
              move 2 to ChkManualeBitmapNumber
           else
              move 0 to v-manuale
              move 1 to ChkManualeBitmapNumber
           end-if.
           move imf-descrizione to lab-impforn-buf.
           perform DESCR-IMBALLO.
           read tivaese no lock invalid continue end-read.
           perform MOVE-DESCR-IVA-2.

           perform DISPLAY-SCREEN.

           move hid-rof-prg-chiave to prg-chiave.
           move space              to prg-cod-magazzino
                                      prg-tipo-imballo.
           move zero               to prg-peso.
           read progmag no lock invalid continue end-read

           move ef-cons-buf to imposta-consumo.
           move ef-cou-buf  to imposta-cou.
           move 0           to imposta-cobat.
           move ef-add-buf  to add-piombo.

      *     move hid-giacenza  to prg-giacenza.
      *     move hid-impegnato to prg-impegnato.
      *     move hid-ordinato  to prg-ordinato-1.
           perform LABEL-PRZ-FINALE
           perform DISPLAY-SCREEN.
           perform LABEL-VALORI.
           set NewRow         to false.
      *     move HiddenKey     to prg-chiave.
           move hid-rof-sconto-1 to save-sconto-1.
           move hid-rof-sconto-2 to save-sconto-2.
           move hid-rof-sconto-3 to save-sconto-3.
           move hid-rof-sconto-4 to save-sconto-4.
           move hid-rof-sconto-5 to save-sconto-5.
           move rof-prz-unitario to savePrezzo.

           if chk-manuale-buf = 1
              perform ENABLE-CAMPI-PREZZI
           else
              if hid-rof-prz-unitario = 0
                  perform DISABLE-CAMPI-PREZZI
              else
                 perform ENABLE-CAMPI-PREZZI
              end-if
           end-if.
                                                        
           move hid-rof-prg-chiave to rof-prg-chiave prg-chiave.

      ***---
       SALVA.
           if mod-k = 1 exit paragraph end-if.

           set SaveXX to true.
           perform ACCESSOXX.

           move BitmapSaveDisabled to BitmapNumSave
           move 0 to e-salva.
           modify tool-salva, 
                  enabled = e-salva,
            bitmap-number = BitmapNumSave.

           set tutto-ok to true.

           set StoSalvando to true.

           perform CHECK-PAGE-1.

           set StoSalvando to false.

           if tutto-ok
              if tutto-ok
                  perform CONTA-ZERI-MAN
                  if not trovato
                     if tof-da-confermare-no
                       display message 
                               "Impossibile registrare l'ordine in"
                               " quanto non sono presenti righe."
                                 title tit-err
                                  icon mb-warning-icon
                       set errori to true
                    end-if
                    perform CANCELLA-COLORE
                 end-if
              end-if
           end-if.

           if errori
              inquire Screen1-Ta-1, value in pagina
              |******
              if store-id >= min-id(pagina) and
                 store-id <= max-id(pagina)
                 move store-id to CONTROL-ID
              end-if
              |******
              move 4 to accept-control
           else
              perform CONTROLLA-TOTALE-MAN
              if errori
                 perform CANCELLA-COLORE
              else
      *           perform CONTA-CODICI-IVA-MAN
                 if tutto-ok
                    perform until 1 = 2
                       set cli-tipo-F of clienti to true
                       set tutto-ok   to true
                       read clienti lock 
                            invalid set errori  to true
                       end-read
                       if recLocked
                          set errori  to true        
                          set reclocked  to false
                          initialize geslock-messaggio
                          string "Fornitore già in uso!"
                                 x"0d0a"
                                 "Impossibile procedere!" delimited size
                                 into geslock-messaggio
                          end-string
                          move 1 to geslock-v-riprova
                          move 0 to geslock-v-ignora
                          move 1 to geslock-v-termina
                          move   "Fornitori"   to geslock-nome-file
                          call   "geslock" using geslock-linkage
                          cancel "geslock"
                          evaluate true
                          when riprova
                               continue
                          when termina
      *                         set errori to true
                               display message "Operazione interrotta!"
                                       title titolo
                                       icon 2
                               exit perform
                          end-evaluate
                       end-if
                       if tutto-ok
                          exit perform
                       end-if
                    end-perform

                 end-if

      *    metto in linea il destino senza lock, perchè il lock è gestito 
      *    a livello di fornitore
                 if tutto-ok
                    read destinif no lock
                       invalid
                          continue
                    end-read
                 end-if

                 if tutto-ok
                    perform VALORIZZA-NUMERO
                    if tutto-ok
                       perform FORM1-BUF-TO-FLD
                       move zero   to tof-pz-tot
                                      tof-pz-arrivati
                       perform SCRIVI-RIGHE
                       perform CANCELLA-COLORE

PATCH                  if tof-numero = 0
PATCH                     display message "EVASIONE ZERO"
PATCH                              x"0d0a""RICARICARE L'ORDINE"
PATCH                               title tit-err
PATCH                                icon 3
PATCH                  end-if

      *                 move cli-ult-ord-forn of clienti
      *                                       to tof-ord-forn-frn
                       set tof-inserito  to true
                       set tof-inevaso   to true

                       move space  to tof-tipo-invio
                                      tof-utente-invio
                                      tof-tipo-chiusura
                                      tof-nota-chiusura
                                      tof-utente-chiusura
                       move zero   to tof-data-invio
                                      tof-ora-invio
                                      tof-utente-chiusura
                                      tof-data-chiusura
                                      tof-ora-chiusura


                       move data-oggi to tof-data-ultima-modifica
                       move como-ora  to tof-ora-ultima-modifica
                       move user-codi to tof-utente-ultima-modifica

                       perform CALCOLA-MESE-RIF
                                                
                       move 0 to art-ordforn-anno
                       move 0 to art-ordforn-numero
                       move low-value to aor-prog
                       perform ARTICOLI-DA-CONFERMARE

                       if tof-da-confermare-si
                          perform ASSEGNA-ARTICOLI-A-ORDINE
                       end-if

                       write tof-rec 
                             invalid rewrite tof-rec 
                       end-write

                       perform SALVA-NOTE
      *    Luciano
                       close tmp-nordforn
      *    Luciano fine
                       delete file tmp-nordforn

      *                 move tof-referente
      *                   to cli-referente-ord   of clienti
      *                 move tof-tel-dir  
      *                   to cli-tel-dir-ref-ord of clienti
      *                 move tof-email    
      *                    to cli-mail-ref-ord   of clienti
      *                 move tof-ord-forn-frn 
      *                    to cli-ult-ord-forn   of clienti
                       move tof-referente      to desf-referente-ord
                       move tof-tel-dir        to desf-tel-dir-ref-ord
                       move tof-email          to desf-mail-ref-ord

                       move data-oggi    
                         to cli-data-ultima-modifica of clienti
                       move como-ora     
                         to cli-ora-ultima-modifica of clienti
                       move user-codi    
                         to cli-utente-ultima-modifica of clienti

                       rewrite cli-rec of clienti
                              invalid continue
                       end-rewrite

                       move data-oggi to desf-data-ultima-modifica
                       move como-ora  to desf-ora-ultima-modifica
                       move user-codi to desf-utente-ultima-modifica

                       rewrite desf-rec
                              invalid continue
                       end-rewrite

                       unlock tordforn all record
              
PATCH                  perform SCRIVI-FILE-BACKUP

                       move tof-chiave   to stof-tof-chiave
                       set stof-normale  to true

                       set stof-scegli-stampante   to true

                       call   "ordf-sol" using tof-chiave
                       cancel "ordf-sol"

                       call   "st-ordforn" using st-ordforn-linkage
                       cancel "st-ordforn"

                       move 1 to mod-k
                       move 0 to mod mod-campi
                       move 3 to NumBitmapArticoli
                       move 0 to v-dett v-blister

                       move 0 to v-articoli
                       display lab-articoli

                       perform INIT
                       perform RESET-GRIGLIA
                       initialize hid-rof-rec old-art-codice

                       perform VALORIZZA-OLD
                       perform ABILITAZIONI
                       perform LEGGI-ANNO
                    else
                       move 27 to key-status
                    end-if
                 end-if
              end-if
           end-if.

           perform DISPLAY-SCREEN.

           set environment "KEYSTROKE" to "DATA=44   44".
           set environment "KEYSTROKE" to "DATA=46   46".

PATCH      move BitmapSaveEnabled to BitmapNumSave
PATCH      move 1 to e-salva.
PATCH      modify tool-salva, 
PATCH             enabled = e-salva,
PATCH      bitmap-number = BitmapNumSave.
           perform DESTROYXX.

      ***---
       SCRIVI-FILE-BACKUP.
      ********--- DEVE RIMANERE IDENTICO A QUELLO DI GORDFORNsVAR
      ********--- MA NON DEV'ESSERE PORTATO IN COPY!!!!
PATCH*********--- Se tutto è andatao a buon fine cancello e riscrivo
      *****      |il file di backup tale e quale a quello effettivo
      *****     move tof-chiave to btof-chiave.
      *****     delete btordforn record invalid continue end-delete.
      *****     move low-value  to brof-rec.
      *****     move tof-chiave to brof-chiave.
      *****     start brordforn key >= brof-chiave
      *****           invalid continue |nel caso in cui sia nuovo
      *****       not invalid
      *****           perform until 1 = 2
      *****              read brordforn next at end exit perform end-read
      *****              if brof-anno       not = btof-anno or
      *****                 brof-num-ordine not = btof-numero
      *****                 exit perform
      *****              end-if
      *****              delete brordforn record invalid continue end-delete
      *****           end-perform
      *****     end-start.
      *****
      *****     read tordforn no lock 
      *****          invalid continue |se annullata l'evasione
      *****      not invalid
      *****          move tof-rec to btof-rec
      *****          write btof-rec invalid continue end-write
      *****          move low-value  to rof-rec
      *****          move tof-chiave to rof-chiave
      *****          start rordforn key >= rof-chiave
      *****                invalid continue
      *****            not invalid
      *****                perform until 1 = 2
      *****                   read rordforn next at end exit perform end-read
      *****                   if rof-chiave-testa not = tof-chiave
      *****                      exit perform
      *****                   end-if
      *****                   move rof-rec to brof-rec
      *****                   write brof-rec invalid continue end-write
      *****                end-perform
      *****          end-start
      *****     end-read.

      ***---
       SCRIVI-RIGHE.
LABLAB*     set ordine-bloccato to false.
LABLAB*     set no-promo        to true.
           inquire form1-gd-1, last-row in tot-righe.
           move 0 to store-riga.
           set HoSalvato to true.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              initialize rof-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              
PATCH**              inquire form1-gd-1(riga, 1), cell-data in col-num
PATCH**              move col-num to rof-num-riga

              inquire form1-gd-1(riga, 1) hidden-data hid-rof-rec-1
              inquire form1-gd-1(riga, 2) hidden-data hid-rof-rec-2
              inquire form1-gd-1(riga, 3) hidden-data hid-rof-rec-3
              move hid-rof-rec  to rof-rec

              move tof-chiave to rof-chiave-testa
PATCH         subtract 1 from riga giving rof-riga
PATCH         modify form1-gd-1(riga, 1), cell-data rof-riga

              move tof-dati-comuni to rof-dati-comuni

LUBEXX*       Controllo che non ci sia discrepanza tra articolo
LUBEXX*       venduto (rordforn) ed articolo scaricato (progmag)
LUBEXX        if hid-rof-cod-articolo not = rof-cod-articolo
LUBEXX           perform VALORIZZA-PROGRESSIVO-CORRETTO
LUBEXX        end-if

      *        if CallWProgmag
      *           initialize link-wprogmag
      *           set link-update      to true
      *           move hid-rof-prg-chiave to link-key
      *           move ef-cau-buf         to link-causale
      *           move rof-qta-ord        to link-valore
      *           move user-codi          to link-user of link-wprogmag
      *           perform VALORIZZA-ARRAY-CAUSALI
      *
      *           call   "wprogmag" using link-wprogmag
      *           cancel "wprogmag"
      *        end-if

              move hid-rof-prg-chiave to rof-prg-chiave prg-chiave
              perform FORZA-PESO-UGUALE

              accept como-ora from time
              move data-oggi to rof-data-creazione
              move como-ora  to rof-ora-creazione
              move user-codi to rof-utente-creazione

LABLAB*        if rof-promo not = 0
LABLAB*           set  si-promo  to true
LABLAB*        end-if

PATCH         if rof-numero = 0
PATCH            display message "EVASIONE ZERO"
PATCH                     x"0d0a""RICARICARE L'ORDINE"
PATCH                      title tit-err
PATCH                       icon 3
PATCH         end-if

              write rof-rec invalid rewrite rof-rec end-write

              add rof-qta-ord   to tof-pz-tot
              add rof-qta-evasa to tof-pz-arrivati

           end-perform.
                                      
      ****---
      * TROVA-BOLLA.
      **    QUESTO PRF: VA LASCIATO QUI PER FORZA!!!
      *     set trovato to false.
      *     move tge-anno      to tof-anno-bolla.
      *     read tordforn no lock key is k-bolla
      *          invalid continue
      *      not invalid set trovato to true
      *     end-read.

      ***---
       VALORIZZA-NUMERO.
           inquire ef-anno value in ef-anno-buf.
           move ef-anno-buf   to link-anno
                                 tof-anno.

           inquire ef-anno value in ef-anno-buf.
           move ef-anno-buf   to link-anno.

           set  link-gordforn   to true.
           set  link-crea       to true.
           move ef-cau-buf      to link-nambar-causale.

           call   "nambar" using link-nambar.
           cancel "nambar".
           
           if link-status-nambar = -1 
              set errori       to true
           else                       
              move link-numero to tof-numero
           end-if.

      ***---
       VALORIZZA-OLD.
      *     move cli-rec                 to old-cli-rec.
      *     move rec-rec                 to old-rec-rec.
      *     set vecchio                  to true.
      *
      *     if old-cli-utf = space
      *        move "N" to old-cli-utf
      *     end-if.
      *
      *     if old-cli-inoltro = space
      *        move "N" to old-cli-inoltro
      *     end-if.         
      *     
      *     if old-cli-superamento-500 = spaces
      *        move "N" to old-cli-superamento-500
      *     end-if.
      *
      *     if old-cli-spost-ric-agosto = space
      *        move "N" to old-cli-spost-ric-agosto
      *     end-if.                       
      *
      *     if old-cli-spost-ric-dicembre = space
      *        move "N" to old-cli-spost-ric-dicembre
      *     end-if.           
      *     
      *     if old-cli-stato = spaces
      *        move "A" to old-cli-stato |Attivo
      *     end-if.
      *
      *     evaluate CONTROL-ID
      *     when 78-ID-ef-prov
      *     when 78-ID-ef-nazione
      *     when 78-ID-ef-tipo
      *     when 78-ID-ef-gdo
      *     when 78-ID-ef-vettore
      *     when 78-ID-ef-agente
      *     when 78-ID-ef-iva
      *     when 78-ID-ef-pag
      *     when 78-ID-ef-abi
      *     when 78-ID-ef-cab
      *     when 78-ID-ef-prov-d
      *     when 78-ID-ef-nazione-d
      *     when 78-ID-ef-vettore-d
      *     when 78-ID-ef-prov-r
      *     when 78-ID-ef-nazione-r
      *          move 1 to StatusHelp
      *     when other
      *          move 0 to StatusHelp
      *     end-evaluate.
      *     perform STATUS-HELP.
      *
      *     move 0 to riga-nuova.
      *     set DestinoCambiato to false.

      ***---
       VAL-NOTE-FORN.
           if note-forn exit paragraph end-if.
           set note-forn to true.
           initialize como-note(1) 
                      como-note(2) 
                      como-note(3) 
                      como-note(4).
           open output tmp-nordforn.
           if status-tmp-nordforn not = "00"
              close       tmp-nordforn
              open output tmp-nordforn
           end-if.
           close       tmp-nordforn.
           open i-o    tmp-nordforn.


           move desf-codice  to nfod-codice.
           move desf-prog    to nfod-dest.

           move low-value    to nfod-prog.
           move zero         to tmp-nof-num-nota.
           start nforn-dest key not < nfod-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read nforn-dest next 
                       at end
                          exit perform
                    end-read
                    if desf-codice not = nfod-codice or 
                       desf-prog   not = nfod-dest
                       exit perform
                    end-if
                    add   1           to tmp-nof-num-nota
                    move tof-chiave   to tmp-nof-chiave-ordine
                    move nfod-nota    to tmp-nof-nota
                    if tmp-nof-num-nota < 5
                       move tmp-nof-nota to como-note(tmp-nof-num-nota)
                    end-if
                    write tmp-nof-rec
                          invalid continue
                    end-write
                 end-perform
           end-start
           modify ef-note-1 value como-note(1).
           modify ef-note-2 value como-note(2).
           modify ef-note-3 value como-note(3).
           modify ef-note-4 value como-note(4).

           move como-note(1) to ef-note-1-buf.
           move como-note(2) to ef-note-2-buf.
           move como-note(3) to ef-note-3-buf.
           move como-note(4) to ef-note-4-buf.

      ***---
       VAL-NOTE-LISTINO.
           if note-list exit paragraph end-if.
           set note-list to true.
           initialize como-note(1) 
                      como-note(2) 
                      como-note(3) 
                      como-note(4).
           open output tmp-nordforn
           if status-tmp-nordforn not = "00"
              close       tmp-nordforn
              open output tmp-nordforn
           end-if.
           close       tmp-nordforn
           open i-o    tmp-nordforn

           move low-value    to nlis-rec.
           move tlis-codice  to nlis-tlis-codice.

           move zero         to tmp-nof-num-nota
           start nlistini key >= nlis-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read nlistini next at end exit perform end-read
                    if nlis-tlis-codice not = tlis-codice
                       exit perform
                    end-if
                    add   1           to tmp-nof-num-nota
                    move tof-chiave   to tmp-nof-chiave-ordine
                    move nlis-nota    to tmp-nof-nota
                    if tmp-nof-num-nota < 5
                       move tmp-nof-nota to como-note(tmp-nof-num-nota)
                    end-if
                    write tmp-nof-rec invalid continue end-write
                 end-perform
           end-start
           modify ef-note-1 value como-note(1).
           modify ef-note-2 value como-note(2).
           modify ef-note-3 value como-note(3).
           modify ef-note-4 value como-note(4).

           move como-note(1) to ef-note-1-buf.
           move como-note(2) to ef-note-2-buf.
           move como-note(3) to ef-note-3-buf.
           move como-note(4) to ef-note-4-buf.

      ***---
       PULISCI-CAMPI-LABELS.
      *     initialize HiddenKey.
           move zero   to ef-art-buf  
                          ef-cons-buf   
                          ef-add-buf
                          ef-cou-buf  
                          ef-sconto-1-buf 
                          ef-sconto-2-buf 
                          ef-sconto-3-buf 
                          ef-sconto-4-buf 
                          ef-sconto-5-buf 
      *                    ef-qta-oma-buf 
                          ef-qta-buf  
                          ef-uni-buf    
                          ef-imp-buf 
                          ef-costi-agg-buf
                          chk-manuale-buf
                          .
           move spaces to lab-art-buf 
                          lab-iva-buf 
                          ef-cod-iva-buf
                          ef-imb-ord-buf
                          lab-imb-buf.

           display ef-art  
                   ef-cons   
                   ef-add
                   ef-cou  
                   ef-sconto-1
                   ef-sconto-2
                   ef-sconto-3
                   ef-sconto-4
                   ef-sconto-5
                   ef-qta  
                   ef-cod-iva
                   ef-uni  
                   ef-imp
                   lab-art 
                   lab-iva
                   ef-imb-ord
                   lab-imb
                   ef-costi-agg
                   chk-manuale.
                   
           move 0 to old-art-codice old-prezzo.

           move 0 to hid-giacenza  hid-impegnato hid-ordinato
                     prezzo-finale hid-rof-imf-codice.
           perform DISPLAY-LABEL-VALORI.
           display lab-giacenza 
                   lab-impegnato 
                   lab-ordinato
                   lab-gia      
                   lab-imp       
                   lab-ord
                   lbl-prz-finale.
           initialize save-sconto-1
                      save-sconto-2
                      save-sconto-3
                      save-sconto-4
                      save-sconto-5
                      savePrezzo.

           perform ENABLE-CAMPI-PREZZI.

      ***---
       ASSEGNA-ARTICOLI-A-ORDINE.
           move 0 to LastPrg.
           move 0 to aor-anno.
           move 0 to aor-numero.
           move low-value to aor-prog
           start art-ordforn key >= aor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read art-ordforn next no lock
                         at end exit perform
                    end-read
                    if aor-anno   not = 0 or
                       aor-numero not = 0
                       exit perform
                    end-if
                    delete art-ordforn record
                    add 1 to LastPrg
                    move tof-chiave to aor-chiave-testa
                    write aor-rec invalid continue end-write
                    move 0 to aor-anno
                    move 0 to aor-numero
                    move low-value to aor-prog
                    start art-ordforn key >= aor-chiave
                          invalid continue
                    end-start
                 end-perform
           end-start.

           move 0 to aor-anno.
           move 0 to aor-numero.
           move low-value to aor-prog
           start art-ordforn key >= aor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read art-ordforn next no lock
                         at end exit perform
                    end-read
                    if aor-anno   not = 0 or
                       aor-numero not = 0
                       exit perform
                    end-if
                    add aor-qta to tof-pz-tot
                 end-perform
           end-start.

      ***---
       IMPORTA-EVASIONE.                 
           move tor-chiave to ror-chiave.
           move low-value  to ror-num-riga.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    initialize hid-rof-rec 
                               replacing numeric data by zeroes
                                    alphanumeric data by spaces
                    move ror-cod-articolo to ef-art-buf
                    move ror-qta          to ef-qta-buf
                    move 10               to ef-uni-buf
                    move ror-cod-iva      to tbliv-codice2
                    move ror-cod-articolo to art-codice
                    read articoli no lock
                    move art-descrizione      to lab-art-buf     
                    move ror-prg-tipo-imballo to ef-imb-ord-buf  
                                                 imq-codice
                    read timbalqta no lock
                    move 0 to ef-sconto-1-BUF  ef-sconto-2-BUF 
                              ef-sconto-3-BUF  ef-sconto-4-BUF
                              ef-sconto-5-BUF  ef-add-BUF 
                              ef-costi-agg-BUF ef-imp-BUF
                              chk-manuale-BUF  ef-impforn-buf 
                    move ror-prg-chiave   to hid-rof-prg-chiave
                    move ror-peso-utf     to hid-rof-peso-utf 
                    move ror-peso-non-utf to hid-rof-peso-non-utf
                    compute hid-rof-qta-imballi =
                            ror-qta / imq-qta-imb
                    set NewRow to true
                    perform ENTRY-TO-ROW
                 end-perform
           end-start,
           move 27 to key-status.
