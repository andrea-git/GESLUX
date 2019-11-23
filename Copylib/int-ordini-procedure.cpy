      ***---
       SPOSTAMENTO.
           inquire form1-gd-1, last-row in tot-righe.
           if event-data-2 >= 2 and
              event-data-2 <= tot-righe
      
              set tutto-ok to true                                      

              read progmag no lock 
                   invalid display message "Dati errati su PROGMAG"
                                    X"0d0a""Contattare Amministrazione"
                                     title tit-err
                                      icon 3
              end-read

              move art-marca-prodotto to mar-codice
              read tmarche no lock invalid continue end-read

           end-if.           
                     
           perform CANCELLA-COLORE.      
           perform COLORE. 

           set ControllaCampi   to false.
      *****     set ArticoloSetFocus to true.
           set event-action     to event-action-terminate.
           set FromSpostamento  to true.
           set ControllaCampi   to false.

      ***---
       LABEL-VALORI.
           if prg-giacenza >= 0 move 515 to ColorGiacenza
           else                 move 525 to ColorGiacenza
           end-if.
           move prg-giacenza  to giacenza-ed hid-giacenza.

           if prg-impegnato >= 0 move 515 to ColorImpegnato
           else                  move 525 to ColorImpegnato
           end-if.
           move prg-impegnato to impegnato-ed hid-impegnato.

           if prg-ordinato >= 0 move 515 to ColorOrdinato
           else                 move 525 to ColorOrdinato
           end-if.
           move prg-ordinato  to ordinato-ed hid-ordinato.

           |display lab-giacenza lab-impegnato lab-ordinato
           |        lab-gia      lab-imp       lab-ord.

      ***---
       CHANGE-TAB.
           set NonCambiareTab   to false.
      *****     set ArticoloSetFocus to false.
           set ControllaCampi   to true.

           if mod-k = 1
              set NonCambiareTab to true
           else
              evaluate EVENT-DATA-1
              when 1
                   move 0 to v-manuale, v-guidata
                   set ControllaCampi   to false
                   if DatiBollaManuale  move 1 to v-bolla
                   else                 move 0 to v-bolla
                   end-if
                   move 0 to v-dett
              when 2
                   perform CHECK-PAGE-1
                   if tutto-ok
                      perform CANCELLA-COLORE
                      evaluate form1-radio-1-buf
                      when 1 |MANUALE
                           if FirstTime
                              if pgm-name = "gordc"
                                |Per gordc non è ancora stato
                                |settato il tipo di caricamento
                                |sul file di testata
                                 set int-tor-manuale to true
                              end-if
                              set FirstTime to false
                              perform DISPLAY-SCREEN-MANUALE
                              move 2 to save-riga
                              perform SETTA-RIGA
                              set ControllaCampi to true
                           end-if
      *****                     set ArticoloSetFocus to true
                           move 0 to v-guidata
                           move 1 to v-manuale
                      end-evaluate
                      move 0 to v-bolla
                      if VenditaAlDettaglio
                         move 1 to v-dett
                      end-if
                   else
                      set ControllaCampi to false
                      move store-id to CONTROL-ID
                      move 4        to ACCEPT-CONTROL
                      set NonCambiareTab to true
                   end-if
              end-evaluate
           end-if.

           display Screen1-Ta-1.

      ***---
       DISPLAY-CAMPI-BOLLA.
           display frame-bolla ef-num-bolla ef-data-bolla lab1 lab2.

      ***---
       MOVE-DATI.
           initialize lab-iva-buf.
           move des-prov to SaveProvincia.
           move cli-iva  to ef-iva-buf tbliv-codice2.
           move "IV"     to tbliv-codice1.
           read tivaese invalid continue
                    not invalid perform MOVE-DESCR-IVA
           end-read.
           display lab-iva ef-iva.

           move des-superamento-500 to flag-superamento-500.
           move des-vettore         to ef-vet-buf vet-codice.
           display ef-vet.
           move space to lab-vet-buf.
           read tvettori invalid continue
                     not invalid move vet-descrizione to lab-vet-buf
           end-read.
           display lab-vet.
           move cli-gdo     to SaveGDO.
           move ef-cli-buf  to not-codice convert.
           move ef-des-buf  to not-prog   convert.
           read note invalid continue
                 not invalid move not-note-1 to ef-note-1-buf
                             move not-note-2 to ef-note-2-buf
                             move not-note-3 to ef-note-3-buf
                             move not-note-4 to ef-note-4-buf
                             move not-data   to como-data
                             perform DATE-TO-SCREEN
                             move como-data  to ef-data-cons-buf
                             display ef-note-1
                                     ef-note-2
                                     ef-note-3
                                     ef-note-4
                                     ef-data-cons
           end-read.

           move spaces  to lab-pag-buf.
           move "PA"    to tblpa-codice1.
           move cli-pag to tblpa-codice2 ef-pag-buf.
           read tcodpag invalid continue
                    not invalid perform MOVE-DESCR-PAG
           end-read.
           display lab-pag ef-pag.

           move spaces      to lab-age-buf.
           move cli-agente  to age-codice ef-age-buf.
           read agenti invalid continue
                   not invalid move age-ragsoc-1 to lab-age-buf
           end-read.
           display lab-age ef-age.
           initialize tcl-rec.
           move cli-tipo to tcl-codice.
           read ttipocli no lock invalid continue end-read.

      ***---
       MOVE-DESCR-IVA.
           initialize lab-iva-buf.
           inspect tbliv-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tbliv-descrizione1 delimited by low-value
                   " "                delimited by size
                   tbliv-descrizione2 delimited by size
                   into lab-iva-buf
           end-string.

      ***---
       MOVE-DESCR-IVA-2.
           initialize lab-iva-2-buf.
           inspect tbliv-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tbliv-descrizione1 delimited by low-value
                   " "                delimited by size
                   tbliv-descrizione2 delimited by size
                   into lab-iva-2-buf
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
           initialize |old-int-ror-rec  
                      old-int-tor-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           set attivo       to true

           move stato       to old-int-tor-stato.

           move "N" to old-int-tor-spostam-ric-ago.
           move "N" to old-int-tor-spostam-ric-dic.

           move data-oggi to int-tor-data-ordine 
                             int-tor-data-passaggio-ordine.

      ***---
       SELEZIONA-ALFA.
           set tutto-ok to true.  
           move spaces to lab-cli-buf.
           move spaces to lab-ind-cli-buf.
           move spaces to lab-loc-cli-buf.
      * LA SECONDA CHIAVE E' ALFANUMERICA E LA VALUE VARIABLE CONTIENE
      * ANCORA GLI ZERI DAVANTI CHE ELIMINO
           inspect ef-cli-buf replacing leading ZERO by SPACES.
           move    ef-cli-buf to cli-ragsoc-1.
      
      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           set cli-tipo-c to true.
           call "C$JUSTIFY" using cli-ragsoc-1, "L".
      
           start clienti key >= cli-k1
                 invalid continue
             not invalid read clienti next 
           end-start.
      
           move "clienti-alfa"    to como-file.

           call "zoom-gt"  using   como-file, cli-rec
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = 0
              if cli-disattivo or cli-bloccato
                 if not tmp
                    display message "Cliente NON attivo"
                              title tit-err
                               icon 2
                    set errori to true
                    move spaces to lab-cli-buf
                    move spaces to lab-ind-cli-buf
                    move spaces to lab-loc-cli-buf
                 end-if   
              end-if
              if tutto-ok
                 if pgm-name = "gordcvar"
                    perform VALUTA-CAMBIO-TIPOLOGIA
                 end-if
                 move cli-codice    to codice-ed
                 move codice-ed     to ef-cli-buf   
                 call "C$JUSTIFY" using ef-cli-buf, "L"
                 display ef-cli   
                 move cli-ragsoc-1  to lab-cli-buf
                 move cli-indirizzo to lab-ind-cli-buf
                 move cli-localita  to lab-loc-cli-buf
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

           inquire ef-cli, value in cli-codice.
           if cli-codice not > 0
              set errori to true
              move 78-ID-ef-cli to CONTROL-ID
              display message box msg-codice-obbligatorio
                      title tit-err
                      icon  MB-WARNING-ICON
           else          
              set cli-tipo-c to true
              read clienti no lock
                   invalid
                   display message box "Codice cliente NON valido"
                           title = tit-err
                           icon MB-WARNING-ICON
                   set errori to true
                   move 78-ID-ef-cli to CONTROL-ID
               not invalid
                   if cli-disattivo or cli-bloccato
                      if not tmp
                         display message "Cliente NON attivo"
                                   title tit-err
                                    icon 2
                         set errori to true
                         move spaces to lab-cli-buf
                         move spaces to lab-ind-cli-buf
                         move spaces to lab-loc-cli-buf
                      end-if
                   end-if
                   if tutto-ok
                      if pgm-name = "gordcvar"
                         perform VALUTA-CAMBIO-TIPOLOGIA
                      end-if
                      move cli-ragsoc-1  to lab-cli-buf
                      move cli-indirizzo to lab-ind-cli-buf
                      move cli-localita  to lab-loc-cli-buf
                   end-if
                   if tutto-ok
                      move cli-ragsoc-1  to lab-cli-buf
                      move cli-indirizzo to lab-ind-cli-buf
                      move cli-localita  to lab-loc-cli-buf
                   end-if
              end-read
           end-if .

      ***---
       SELEZIONA-DESTINO-NUMERICO.
           move spaces to lab-des-buf.
           move spaces to lab-ind-des-buf.
           move spaces to lab-loc-des-buf.
           inquire ef-cli, value in des-codice.
           inquire ef-des, value in des-prog.
           move ef-des-buf to des-codice.
           if des-codice not > 0
              set errori to true
              move 78-ID-ef-des to control-id
              display message box msg-codice-obbligatorio
                      title tit-err
                      icon  mb-warning-icon
           else          
              inquire ef-cli, value in des-codice
              read destini no lock
                   invalid
                   display message "Progressivo destino NON valido"
                             title tit-err
                             icon mb-warning-icon
                   set errori to true
                   move 78-ID-ef-des to control-id
               not invalid
                   if des-disattivo or des-bloccato
                      if not tmp
                         set errori to true
                         display message "Destino NON attivo"
                                   title tit-err
                                    icon 2
                         move spaces to lab-des-buf
                         move spaces to lab-ind-des-buf
                         move spaces to lab-loc-des-buf
                      end-if
                   end-if
                   if tutto-ok
                      move des-ragsoc-1  to lab-des-buf
                      move des-indirizzo to lab-ind-des-buf
                      move des-localita  to lab-loc-des-buf
                   end-if
              end-read
           end-if.

      ***---
       SELEZIONA-DESTINO-ALFA.
      * LA SECONDA CHIAVE E' ALFANUMERICA E LA VALUE VARIABLE CONTIENE
      * ANCORA GLI ZERI DAVANTI CHE ELIMINO
           move    ef-cli-buf to des-codice.
           inspect ef-des-buf replacing leading ZERO by SPACES.
           move    ef-des-buf to des-ragsoc-1.
      
      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           call "C$JUSTIFY" using des-ragsoc-1, "L".
      
           start destini     key >= K1
                 invalid     continue
                 not invalid read destini next 
           end-start.   
           set cli-tipo-c to true.
      
           move "clienti-des-alf" to como-file.
           call "zoom-gt" using   como-file, des-rec
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = 0
              if des-disattivo or des-bloccato
                 if not tmp
                    set errori to true
                    display message "Destino NON attivo"
                              title tit-err
                               icon 2
                    move spaces to lab-des-buf
                    move spaces to lab-ind-des-buf
                    move spaces to lab-loc-des-buf
                 end-if
              end-if
              if tutto-ok
                 move des-codice     to codice-ed
                 move codice-ed      to ef-des-buf   
                 call "C$JUSTIFY" using ef-des-buf, "L"
                 display ef-des   
                 move des-ragsoc-1  to lab-des-buf
                 move des-indirizzo to lab-ind-des-buf
                 move des-localita  to lab-loc-des-buf
              end-if
           else
              set errori         to true
              move 78-ID-ef-des  to CONTROL-ID
           end-if.


      ***---
       TROVA-DESTINO.
           set trovato to false.
           inquire ef-cli, value in ef-cli-buf.
           move ef-cli-buf to des-codice convert.
           move ef-cli-buf to cli-codice convert.

           move low-value  to des-prog.
           start destini  key is >= des-chiave
                 invalid  continue
             not invalid  read destini next
                 if des-codice = cli-codice 
                    set trovato to true 
                 else 
                    move ef-cli-buf to des-codice convert
                    move ef-des-buf to des-prog   convert
                 end-if
           end-start.

      ***---
       CHECK-PAGE-1.
           perform  varying control-id from 78-ID-ef-num-ord by 1
                      until control-id    > 78-ID-ef-data-cons
              perform CONTROLLO
              if errori 
                 exit perform 
              end-if
           end-perform.
           if tutto-ok
              perform  varying control-id from 78-ID-ef-data-bolla by 1
                         until control-id    > 78-ID-ef-num-bolla
                 perform CONTROLLO
                 if errori 
                    exit perform 
                 end-if
              end-perform
           end-if.

      ***---
       VALUTA-CAMBIO-TIPOLOGIA.
           if e-gui = 1
              display message "IMPOSSIBILE" |assortimento
           else
              move cli-tipo to tcl-codice
              read ttipocli no lock
                   invalid  move spaces to tcl-tipologia-tratt-imposte
              end-read
              if tcl-tipologia-tratt-imposte not = TrattamentoInUso
LUBEXX           display message "ATTENZIONE!!"
LUBEXX                    x"0d0a""E' stato cambiata la tipologia "
LUBEXX                           "trattamento imposte del cliente."
LUBEXX                    x"0d0a""Annullare e ricreare l'ordine"
LUBEXX                           " col nuovo Cliente!!!"
LUBEXX                     title tit-err
LUBEXX                      icon 2
LUBEXX           set errori to true
LUBEXX*****
LUBEXX*****                 move tcl-tipologia-tratt-imposte to TrattamentoInUso
LUBEXX*****
LUBEXX*****                 if ttipocli-gdo modify ef-sconto,     read-only
LUBEXX*****                 else            modify ef-sconto, not read-only
LUBEXX*****                 end-if

LUBEXX*****                 display message "ATTENZIONE!!"
LUBEXX*****                          x"0d0a""E' stato cambiata la "
LUBEXX*****                                 "tipologia di cliente."
LUBEXX*****                          x"0d0a""Occorre ripassare TUTTE le "
LUBEXX*****                                 "righe per prezzi e imposte"
LUBEXX*****                           title tit-err
LUBEXX*****                            icon 2
LUBEXX*****
LUBEXX*****                 move tcl-tipologia-tratt-imposte to TrattamentoInUso
LUBEXX*****
LUBEXX*****                 if ttipocli-gdo modify ef-sconto,     read-only
LUBEXX*****                 else            modify ef-sconto, not read-only
LUBEXX*****                 end-if
LUBEXX*****
LUBEXX*****                 set CambiatoTrattamento to true

              end-if
           end-if.

      ***---
       SECCA-TMP-ASSORCLI.
           if link-path-tmp-assorcli not = spaces
              close tmp-assorcli
              call "C$DELETE" using link-path-tmp-assorcli, "I"
              move spaces to link-path-tmp-assorcli
           end-if.

      ***---
       STATUS-BAR-MSG.
           if pgm-name = "gordc"
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

LUBEXX     initialize sw-controlla-scostamento.
LUBEXX     accept sw-controlla-scostamento 
LUBEXX            from environment "CONTROLLO_SCOSTAMENTO".

           initialize FlagAssortimento.
           accept FlagAssortimento from environment "ASSORTIMENTO"
                  on exception
                     set NoAssortimento to true
              not on exception
                     if FlagAssortimento = spaces
                        set NoAssortimento to true
                     end-if
           end-accept.
           if SiAssortimento move 0 to e-gui
           else              move 1 to e-gui
           end-if.
           move 0       to v-bolla v-dett.
           set tutto-ok to true.
           open input tparamge.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           move tge-anno to como-anno.
           close tparamge.

           open input tcaumag.
           move tge-causale-ordini-std to tca-codice.
           read  tcaumag no lock invalid set errori to true end-read.

           if tutto-ok
              move tca-cod-magaz to mag-codice, StoreMagazzino
              open input tmagaz
              read tmagaz no lock invalid set errori to true end-read
              close tmagaz
              if errori
                 display message box 
                          "Magazzino per Bolla ordinaria mancante!!!"
                  x"0d0da""Impossibile procedere con il programma."
                         title = tit-err
                         icon MB-ERROR-ICON
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
              move 0 to imp-data
              read timposte no lock invalid set errori to true end-read
           
              if errori
                 display message box 
                          "Record imposte mancante!!!"
                  x"0d0da""Impossibile procedere con il programma."
                         title = tit-err
                         icon MB-ERROR-ICON
              end-if
              close timposte
           end-if.

           if errori
              goback 
           else
              copy resource "conferma.bmp"
              call "W$BITMAP" using wbitmap-load "conferma.bmp"
                             giving conferma-bmp
           end-if.

      ***---
       ORDINI-AFTER-PROGRAM.
           call "W$BITMAP" using wbitmap-destroy, conferma-bmp.
           perform SECCA-TMP-ASSORCLI.

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

           if int-tor-manuale
              modify form1-gd-1, cursor-y = riga
              perform SPOSTAMENTO
           end-if.

      ***---
       TESTATA-DETTAGLIO.
           move tge-cliente-corrisp to cli-codice.
           set cli-tipo-C to true.
           read clienti no lock
                invalid
                set errori to true
                display message "Vendita al dettaglio impossibile:"
                         x"0d0a""Cliente per corrispettivi assente"
                        title = tit-err
                        icon mb-warning-icon
                move 78-ID-ef-cli to control-id
           end-read.

      ***---
       CONTROLLO.
           set tutto-ok to true.

      ***---
       CALCOLA-TOTALE-IVATO.
           inquire form1-gd-1, last-row in tot-righe.
           move 0 to como-tot-ivato como-imposta SavePrezzo.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire form1-gd-1(riga, 4), 
                      cell-data in int-ror-qta
              inquire form1-gd-1(riga, 7), 
                      cell-data in int-ror-imp-consumo
              inquire form1-gd-1(riga, 8), 
                      cell-data in int-ror-imp-cou-cobat
              inquire form1-gd-1(riga, 9), 
                      cell-data in int-ror-imponib-merce
              if pgm-name = "gordc"
                 inquire form1-gd-1(riga, 78-NumColMan + 17),
                         hidden-data in hid-perce-iva
              else
                 inquire form1-gd-1(riga, 78-NumColMan + 19),
                         hidden-data in hid-perce-iva
              end-if
              compute SavePrezzo = int-ror-imponib-merce +
                                   int-ror-imp-cou-cobat + 
                                   int-ror-imp-consumo
      *****        compute SavePrezzo = SavePrezzo * int-ror-qta
OMAGGI        compute SavePrezzo = 
OMAGGI                SavePrezzo * ( int-ror-qta - int-ror-qta-omaggi)
              compute como-iva =
                      SavePrezzo * hid-perce-iva / 100
              add 0,005           to como-iva
              move como-iva       to como-iva-2dec
              compute como-tot-ivato =
                      como-tot-ivato +
                      SavePrezzo     +
                      como-iva-2dec
           end-perform.
           move como-tot-ivato to lab-tot-ivato-buf.
           move 0 to lab-ivato-buf.

      ***---
       READ-TMARCHE.
           move art-marca-prodotto to mar-codice.
           read tmarche no lock invalid continue end-read.
           move mar-ven-var-listino-meno to hid-var-meno.
           move mar-ven-var-listino-piu  to hid-var-piu.

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
              add SavePrezzo to Sum
           end-perform.
           if Sum = 0 if TotaleNoZero set errori to true end-if
           else       if TotaleSiZero set errori to true end-if
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
              inquire form1-gd-1(riga, 10),
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
              inquire form1-gd-1(store-riga, 78-NumColMan + 2),
                      hidden-data = hid-cod-magazzino
              if hid-cod-magazzino not = tca-cod-magaz
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
                    if tca-si-stampa and tca-cliente or 
                       tca-codice = "AEXD"
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
           move int-ror-cod-iva to tbliv-codice2.
           read tivaese no lock 
                invalid continue
            not invalid
                if tbliv-percentuale not = 0
                   set EsisteIVA to true
                end-if
           end-read.
