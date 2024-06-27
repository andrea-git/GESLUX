      ***---
       ABILITA-DISABILITA-RIGHE.
           if mod = 0
              move 0 to e-pb-grid
                        mod-campi
                        mod-campi-righe
                        mod-campi-testa
              move 8 to BitmapNumGridNuovo
              move 7 to BitmapNumGridElimina
              move 1 to NumBitmapArticoli
           else
              move 1 to mod-campi
              move 1 to NumBitmapArticoli
              evaluate true
              when tof-inserito
      *****        when tof-accettato
              when tof-inviato
              when tof-in-lavorazione
                   move 1 to mod-campi-righe
                             mod-campi-testa
                             e-pb-grid
                   move 5 to BitmapNumGridNuovo
                   move 4 to BitmapNumGridElimina
      *****        when tof-in-lavorazione
      *****             move 0    to mod-campi-righe
      *****                          e-pb-grid
      *****             move 1    to mod-campi-testa
      *****             move 8 to BitmapNumGridNuovo
      *****             move 7 to BitmapNumGridElimina
              when tof-chiuso
                   move 0 to mod-campi-righe
                             mod-campi-testa
                             e-pb-grid
                   move 8 to BitmapNumGridNuovo
                   move 7 to BitmapNumGridElimina
              end-evaluate
           end-if.

           display pb-grid-elimina pb-grid-nuovo pb-articoli.

      ***---
       ABILITAZIONI.
           if mod = 1                  
              move BitmapDeleteEnabled to BitmapNumDelete
              move BitmapSaveEnabled   to BitmapNumSave
              move 1 to e-salva e-cancella
                                 
              move 0 to mod-campi
              move 1 to NumBitmapArticoli

      *        inquire cbo-stato,   value cbo-stato-buf
      *
      *        if cbo-stato-buf not = "Disattivo" 
      *           move 1 to mod-campi 
      *        end-if  

           else         

              move BitmapDeleteDisabled to BitmapNumDelete
              move BitmapSaveDisabled   to BitmapNumSave
              move 0 to e-salva e-cancella         
              move 0 to mod-campi e-gui e-man
              move 1 to NumBitmapArticoli
           end-if.
           display pb-articoli.
           perform ABILITA-DISABILITA-RIGHE.
           move BitmapNewDisabled     to BitmapNumNew.
                                                                 
           modify tool-nuovo,    enabled       = 0
                                 bitmap-number = BitmapNumNew.
           modify tool-cancella, enabled       = e-cancella,
                                 bitmap-number = BitmapNumDelete.
           modify tool-salva,    enabled       = e-salva,
                                 bitmap-number = BitmapNumSave.

      ***---
      * AGGIORNA-FILE-CHECK.
      *     open i-o check-rordforn
      *     |Prima li cancello tutti così resetto partendo da 0
      *     move low-value to crof-rec
      *     start check-rordforn key >= crof-chiave
      *           invalid continue
      *       not invalid
      *           perform until 1 = 2
      *              read check-rordforn next 
      *                   at end exit perform 
      *              end-read    
      *              delete check-rordforn record
      *                     invalid continue
      *                     |ATTENZIONE!!! 
      *                     |Se non riesce correttamente delete
      *                     |la stampa BOZZA MODIFICATA potrebbe 
      *                     |uscire errata. Nel caso venisse
      *                     |segnalato questo errore valutare
      *                     |l'ipotesi di gestire qui l'errore
      *              end-delete
      *           end-perform
      *     end-start.
      *
      *     move tof-chiave to rof-chiave.
      *     move low-value  to rof-riga.
      *     start rordforn key >= rof-chiave
      *           invalid continue 
      *       not invalid
      *           perform until 1 = 2
      *              read rordforn next at end exit perform end-read
      *              if rof-anno       not = tof-anno or
      *                 rof-numero not = tof-numero
      *                 exit perform
      *              end-if
      *              move rof-prg-chiave  to crof-prg-chiave
      *              move rof-des-imballo to crof-des-imballo
      *              move rof-blister     to crof-blister
      *              if rof-si-blister
      *                 move 0 to crof-qta-imballi
      *              else
      *                 move rof-qta-imballi to crof-qta-imballi
      *              end-if
      *              read check-rordforn no lock
      *                   invalid 
      *                   initialize crof-dati
      *                              replacing numeric data by zeroes
      *                                   alphanumeric data by spaces
      *                   move rof-peso-utf     to crof-peso-utf
      *                   move rof-peso-non-utf to crof-peso-non-utf
      *               not invalid
      *                   add 1 to crof-righe
      *              end-read
      *              add rof-num-colli to crof-num-colli
      *              add rof-qta       to crof-qta
      *              compute crof-importo =
      *                      crof-importo + ( rof-qta * rof-prz-unitario)
      *              write crof-rec 
      *                 invalid 
      *                    rewrite crof-rec 
      *              end-write
      *           end-perform
      *     end-start.
      *     
      *     close check-rordforn.

      ***---
       CANCELLA.                
           accept forza-canc from environment "FORZA_CANC".
           if forza-canc = "S"
              move mb-yes to scelta
           else
              evaluate true
              when tof-inserito       
                   if LinkOrdineDeleteOp = 1
                      move mb-yes to scelta
                   else
                      display message "Cancellare l'ordine corrente?"
                                title titolo
                                 type mb-yes-no
                              default mb-no
                               giving scelta
                                icon 2
                   end-if
              when tof-inviato
                   move mb-no   to scelta
                   display message "Ordine già inviato."
                                   x"0d0a"
                                   "Cancellazione impossibile."
                           title titolo 
                           icon 2
              when tof-in-lavorazione
                   move mb-no   to scelta
                   display message "Ordine in lavorazione."
                                   x"0d0a"
                                   "Cancellazione impossibile."
                           title titolo 
                           icon 2
              when tof-chiuso
                   display message "Ordine chiuso."
                                   x"0d0a"
                                   "Cancellazione impossibile."
                           title titolo 
                           icon 2
                   move mb-no   to scelta
              end-evaluate
           end-if.

      *    se è l'ultimo ordine per il fornitore devo aggiornare la 
      *    numerazione sull'anagrafica del fornitore
      *     if scelta = mb-yes
      *        move tof-cod-forn to cli-codice
      *        set cli-tipo-F to true
      *        read CLIENTI no lock
      *           invalid
      *              continue
      *        end-read
                 
      *        if cli-ult-ord-forn = tof-ord-forn
      *
      *           perform until 1 = 2
      *              set tutto-ok   to true        
      *              set reclocked  to false
      *              read CLIENTI lock
      *                 invalid
      *                    set errori  to true
      *              end-read
      *              if recLocked
      *                 initialize geslock-messaggio
      *                 string "Fornitore già in uso!"
      *                        x"0d0a"
      *                        "Impossibile procedere!" delimited size
      *                        into geslock-messaggio
      *                 end-string
      *                 move 1 to geslock-v-riprova
      *                 move 0 to geslock-v-ignora
      *                 move 1 to geslock-v-termina
      *                 move   "Fornitori"   to geslock-nome-file
      *                 call   "geslock" using geslock-linkage
      *                 cancel "geslock"
      *                 evaluate true
      *                 when riprova
      *                      continue
      *                 when termina
      *                      set errori to true
      *                      display message "Operazione interrotta!"
      *                              title titolo
      *                              icon 2
      *                      exit perform
      *                 end-evaluate
      *              end-if
      *              if tutto-ok
      *                 exit perform
      *              end-if
      *           end-perform
      *           if tutto-ok
      *              subtract 1 from cli-ult-ord
      *              accept cli-data-ultima-modifica from century-date
      *              accept cli-ora-ultima-modifica  from time
      *              move user-codi to cli-utente-ultima-modifica
      *              rewrite CLI-REC
      *                 invalid
      *                    continue
      *              end-rewrite
      *              unlock clienti all record
      *
      *
      *           else
      *              move mb-no  to scelta
      *           end-if
      *        end-if
      *
      *     end-if

           if scelta = mb-yes   
              set DeleteXX to true
              perform ACCESSOXX
      *        set CancellazioneFisica to true
              move LinkChiave to tof-chiave
PATCH         start transaction
              perform DELETE-RIGHE
              if not SystemErrorOccurred
PATCH            commit transaction
                 perform CANCELLA-NOTE
PATCH            perform SCRIVI-FILE-BACKUP
                 delete tordforn  record Invalid continue end-delete 
                 move 0 to mod
                 set SollecitiCambiati  to false
                 set SollecitiCambiatiT to false
                 initialize tof-rec 
                        old-tof-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces 
                 set RicaricaGrid   to true
                 set RigaCambiata   to false
                 set PrezzoCambiato to false
                 set HoSalvato      to true 
                 move 0 to LinkOrdineDeleteOp
              end-if
              |SCRIVO IL LOG PER DOCUMENTARE LA CANCELLAZIONE FATTA DALL'UTENTE
              accept log-data from century-date
              accept log-ora  from time
              move "GORDFORNVAR"       to log-pgm
              set log-cancellazione to true
              move LinkChiave       to log-chiave-file
              move user-codi        to log-utente
              initialize log-descrizione
              string "CANCELLATE "  delimited size
                     DeletedRows    delimited size
                     " RIGHE"       delimited size
                     into log-descrizione
              end-string
              write log-rec invalid continue end-write
              perform FORM1-EXIT
           end-if.             
           perform DESTROYXX.


      ***---
       CLEAR-SCREEN.
      *     move cli-codice to old-cli-codice.
      *    
           initialize tof-rec  
                      cli-rec of clienti
                      cli-rec of clienti1
                      des-rec
                      como-note(1)
                      como-note(2)
                      como-note(3)
                      como-note(4)
                      como-descr-promo
                      rof-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
                        
           perform FORM1-FLD-TO-BUF. 
      
           perform INIT-OLD-REC.

           move spaces to lab-ese-iva-buf 
                          lab-iva-buf 
                          lab-pag-buf.
      
           perform DISPLAY-SCREEN.

      ***---
       COLORE.
           modify form1-gd-1, start-y = riga, y = riga,
                              start-x = 2,    x = 17,
                              region-color = 144.

      ****---
      * CONTROLLA-CAUSALE.
      *     move ef-cau-buf to tof-causale tca-codice.
      *     read tcaumag no lock invalid continue end-read.
      *     if tca-no-stampa
      *        move tof-chiave to link-insdati-chiave
      **        if tof-anno-bolla = 0
      **           move tge-anno to tof-anno-bolla
      **        end-if
      *        move tof-anno-bolla to link-insdati-anno
      *        move 0 to link-data-bolla link-num-bolla
      *        move lab-data-bolla-buf to como-data
      *        move lab-num-bolla-buf  to tof-num-bolla link-num-bolla
      *        if como-data = 0 or tof-num-bolla = 0
      *           perform DATE-TO-FILE
      *           move como-data   to link-data-bolla 
      *           set obbligatorio to true
      *           call   "insdati" using insdati-linkage
      *           cancel "insdati"
      *           if link-status not = 0
      *              set errori to true
      *              display message box "Salvataggio NON effettuato: "
      *                           x"0d0a""Inserimento dati bolla "
      *                            "obbligatori per la causale inserita!"
      *                      title = tit-err
      *                      icon 2
      *           else
      *              move link-data-bolla   to como-data 
      *                                        orig-data-bolla
      *              perform DATE-TO-SCREEN
      *              move como-data         to lab-data-bolla-buf
      *              if tof-anno-bolla = 0
      *                 move tge-anno       to tof-anno-bolla
      *              end-if
      *              move link-num-bolla    to lab-num-bolla-buf 
      *                                        orig-num-bolla
      *              move "BOLLA PRENOTATA" to lab-bolla-pren-buf
      *              set  tof-bolla-si-prenotata to true
      *              display lab-data-bolla
      *                      lab-num-bolla
      *                      lab-bolla-pren
      *           end-if
      *        end-if
      *     end-if.
      *     move tca-contropartita to tof-contropartita.

      ***---
       CURRENT-RECORD.
           set tutto-ok  to true.
           set ReadSecca to true.
           if mod = 1
              read tordforn lock invalid 
                   set errori to true 
              end-read
           else       
              unlock tordforn all records
              read tordforn no lock invalid 
                   set errori to true 
              end-read
           end-if.
           set ReadSecca to false.

      *     if tof-num-bolla  not = 0 and
      *        tof-data-bolla not = 0
      *        set bollettata to true
      *     else
      *        set bollettata to false
      *     end-if.

      *     if tof-num-fattura  not = 0 and
      *        tof-data-fattura not = 0
      *        move 1 to v-fatt
      *     else
      *        move 0 to v-fatt
      *     end-if.

      *     if v-bolla = 0
      *        if bollettata and v-fatt = 0
      *           move 0 to abil-bolla
      *           move 1 to v-abil-bolla
      *           modify chk-abil-bolla, value abil-bolla, 
      *                                  bitmap-number = 1,
      *                                  visible v-abil-bolla
      *        end-if
      *     end-if.
                   
           perform SELEZIONA-LISTINO.
           if tutto-ok
LUBEXX*****   Mi salvo lo stato dell'ordine in quanto in
LUBEXX*****   fase di moficica non può essere variato
LUBEXX        move tof-stato to save-stato
              
      *        move tof-data-bolla to orig-data-bolla
      *        move tof-num-bolla  to orig-num-bolla
      *
      *        if LinkPgm(1:5) = "stdoc"
      *           if not TastiGiaAbilitati
      *              if tof-data-bolla not = 0
      *                 move 1 to e-stbolla
      *                 if LinkPgm = "stdocB"
      *                    move 777          to control-id
      *                    move 4            to accept-control
      *                 end-if
      *              else
      *                 move 0 to e-stbolla
      *              end-if
      *              if tof-data-fattura not = 0
      *                 move 1 to e-stfatt
      *                 if LinkPgm = "stdocF"
      *                    move 888          to control-id
      *                    move 4            to accept-control
      *                 end-if
      *              else
      *                 move 0 to e-stfatt
      *              end-if
      *              if LinkPgm = "stdocF" or "stdocB"
      *                 move 1 to e-stbozza
      *              end-if
      *              if tof-num-bolla not = 0
      *                 move 0 to v-stbozza
      *                 move 0 to e-stbozza
      *              end-if
      *              set TastiGiaAbilitati to true
      *           end-if
      *        end-if
                    
      *        if tof-causale = tge-causale-corrisp 
      *           set VenditaAlDettaglio to true
      *        else
      *           set VenditaAlDettaglio to false
      *        end-if

              move tof-causale     to tca-codice
              move spaces to tca-descrizione
              move 0 to v-bolla
              read tcaumag invalid continue end-read
              move tca-descrizione to lab-cau-buf
              if tca-cod-magaz = "EXD"
                 move 1 to v-dati-fatt
              else
                 move 0 to v-dati-fatt
              end-if
      *        if tca-no-stampa 
      *           move 1 to v-bolla
      *        else             
      *           move 0 to v-bolla
      *        end-if

      *        if tca-no-movim-giac and
      *           tca-no-movim-imp  and
      *           tca-no-movim-ord
      *           set CallWProgmag to false
      *        else
      *           set CallWProgmag to true
      *        end-if
      *        
      *        if tca-si-zero 
      *           set TotaleSiZero to true
      *        else           
      *           set TotaleNoZero to true
      *        end-if

              move tof-cod-forn    to cli-codice of clienti
                                         ef-cli-buf
              move spaces to cli-ragsoc-1 of clienti
                             cli-indirizzo of clienti
                             cli-localita of clienti
      *        move spaces to des-ragsoc-1 des-indirizzo des-localita
              set cli-tipo-f of clienti to true
              read clienti no lock 
                   invalid continue 
              end-read


      *        move cli-gdo to SaveGDO
       
      *        move cli-tipo to tcl-codice
      *        read ttipocli no lock
      *             invalid  move spaces to TrattamentoInUso
      *         not invalid  move tcl-tipologia-tratt-imposte 
      *                        to TrattamentoInuso
      *        end-read

      *        perform ABILITA-GEST-PLUS

      *        if tof-prg-destino not = 0
      *           move tof-cod-cli     to des-codice
      *           move tof-prg-destino to des-prog ef-des-buf
      *           read destini  no lock 
      *                invalid  move spaces to des-superamento-500
      *           end-read
      *        else
      *           initialize des-rec
      *           move cli-superamento-500 to des-superamento-500
      *        end-if
      *        move des-superamento-500 to flag-superamento-500

      *        if tof-cod-agente    not = 0
      *           move tof-cod-agente to age-codice
      *           read agenti   no lock invalid continue end-read
      *        else
      *           initialize age-rec
      *        end-if

              if tof-cod-pagamento not = spaces
                 move "PA"              to tblpa-codice1
                 move tof-cod-pagamento to tblpa-codice2
                 read tcodpag  no lock invalid continue end-read
              else
                 initialize record-tblpa
              end-if

              perform MOVE-DESCR-PAG

              if tof-cod-ese-iva not = spaces
                 move "IV"              to tbliv-codice1
                 move tof-cod-ese-iva   to tbliv-codice2
                 read tivaese  no lock invalid continue end-read
              else
                 initialize record-tbliv
              end-if

              perform MOVE-DESCR-IVA

              move tof-promo to tpr-codice
              read TPROMO
                 invalid
                    move space  to como-descr-promo
                 not invalid
                    perform DESCRIZIONE-PROMO
              end-read

      *        if tof-cod-ese-iva   not = spaces
      *           move "IV"            to tbliv-codice1
      *           move tof-cod-ese-iva to tbliv-codice2
      *           read tivaese  no lock invalid continue end-read
      *        else   
      *           initialize record-tbliv
      *        end-if
      *        perform MOVE-DESCR-IVA

      *        if tof-vettore       not = 0
      *           move tof-vettore to vet-codice
      *           read tvettori no lock invalid continue end-read
      *        else
      *           initialize vet-rec
      *        end-if          

      *        if tof-manuale  
                 move riga to save-riga
                 perform DISPLAY-SCREEN-MANUALE
      *        else
      *           perform DISPLAY-SCREEN-GUIDATA
      *        end-if
       
              perform LOAD-NOTE
              perform LOAD-SOLLECITI-T
              move "00" to status-tordforn
              perform FORM1-FLD-TO-BUF

              set vecchio to true
              if mod = 1
                 set StatusModifica to true
              else                         
                 set StatusVisua    to true
              end-if
              perform STATUS-BAR-MSG
              set YesDeleted to false
              inquire Screen1-Ta-1, value in idx
           else
              move 0 to mod
              move 0 to mod-k
              if vecchio
                 perform CLEAR-SCREEN
                 set errori to true
                 if YesMessage    
                    move 5000 to control-id |TAB-CONTROL
                    move    4 to accept-control
                    display message MSG-Record-inesistente
                              title tit-err
                              icon mb-warning-icon
                 end-if
              end-if
           end-if.
           
           if RecLocked
              set RecLocked to false
              set errori    to true
           end-if.

      ***---
       DELETE-RIGHE.
           move 0 to DeletedRows.
           move tof-chiave to rof-chiave.
           move low-value  to rof-riga.

           start rordforn key >= rof-chiave
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read rordforn next no lock 
                         at end exit perform 
                    end-read
                    if tof-anno   not = rof-anno or
                       tof-numero not = rof-numero
                       exit perform
                    end-if
PATCH               if status-rordforn not = "00"
PATCH                  move 5 to tipo-messaggio
PATCH                  perform INVIO-MAIL
PATCH                  exit perform
PATCH               end-if
                    delete rordforn record 
                           invalid
                           move 4 to tipo-messaggio
                           perform INVIO-MAIL
                           exit perform
                       not invalid
                           add 1 to DeletedRows
                    end-delete
                    initialize link-wprogmag
                    evaluate true
                    when tof-inserito
      *****              when tof-accettato
                         continue
                    when tof-inviato
                    when tof-in-lavorazione
                    when tof-chiuso
                         perform RESET-PROGMAG
                    end-evaluate

                 end-perform
           end-start.

      ***---
       RESET-PROGMAG.
           perform VALORIZZA-ARRAY-CAUSALI.
           set link-update      to true.
           move rof-prg-chiave  to link-key.
           move ef-cau-buf      to link-causale.
           move tof-mese-rif    to link-mese-rif.

           |Voglio cambiare il flag...
           if tof-aperto not = old-tof-aperto
              |...e lo voglio aprire perciò DEVO TOGLIERE e mettendo a
              |zero la chiave non c'è nessun controllo sul file
              |non ancora scritto
              if tof-aperto-si
                 initialize link-chiave-origine
              else
              |...e lo voglio chiudere perciò NON DEVO TOGLIERE e 
              |wprogmag trovaerà settato il flag per questo record
              |perciò non farà alcuna operazione
                 move tof-chiave to link-chiave-origine
              end-if
           else
           |Il flag non cambia perciò va bene 
           |quello che c'è scritto nel record
              move tof-chiave to link-chiave-origine
           end-if.

           move user-codi to link-user of link-wprogmag
      *    storno l'ordinato
           compute link-valore = rof-qta-ord - rof-qta-evasa
           if link-valore not = 0
              move "0000000000000000" to link-array
              move -1                 to multiplyer(3)
              call   "wprogmag"    using link-wprogmag
              cancel "wprogmag"
           end-if

           move tof-chiave to link-chiave-origine.
      *    storno la qta arrivata
           if rof-qta-evasa not = 0
              move rof-qta-evasa     to link-valore
              move "0000000000000000" to link-array
              move -1                 to multiplyer(1)
              move -1                 to multiplyer(15)
              call   "wprogmag"   using link-wprogmag
              cancel "wprogmag"
           end-if.

      ***---
       DISPLAY-SCREEN.
           display form1.
      *     if e-stbolla = 0
      *        modify pb-stbolla, bitmap-number = 3
      *     end-if.
      *     |per i solleciti
      *     if e-stbozza = 1
      *        modify pb-stbozza, bitmap-number = 10
      *     end-if.
      *     if e-stfatt  = 0 
      *        modify pb-stfatt,  bitmap-number = 6 
      *     end-if.

      ****---
      * DISPLAY-SCREEN-GUIDATA.
      *     modify form1-gd-2, mass-update = 1.
      *     modify form1-gd-2, reset-grid  = 1.
      *     perform INTESTAZIONE-2.
      *     perform CARICA-GRID-GUIDATA.
      *     modify form1-gd-2, mass-update = 0.


      ***---
       DISPLAY-SCREEN-MANUALE.
           modify form1-gd-1, mass-update = 1.
           modify form1-gd-1, reset-grid  = 1.
           perform INTESTAZIONE.
           perform CARICA-GRID-MANUALE.
           modify form1-gd-1, mass-update = 0.

      ***---
       CARICA-GRID-MANUALE.
           initialize tab-iva.
      *     if sw-check-rordforn = 0
      *        accept como-data from century-date
      *        accept como-ora  from time
      *        initialize path-check-rordforn
      *        accept  path-check-rordforn from environment "PATH_ST"
      *        inspect path-check-rordforn replacing trailing 
      *                                   spaces by low-value
      *        string  path-check-rordforn delimited low-value
      *                "CHECK-rordforn_"   delimited size
      *                como-data          delimited size
      *                "_"                delimited size
      *                como-ora           delimited size
      *                ".tmp"             delimited size
      *                into path-check-rordforn
      *        end-string
      *        move path-check-rordforn to link-path
      *        open output check-rordforn
      *        close       check-rordforn
      *        open i-o    check-rordforn
      *     end-if.

           move 0 to righe-iniziali.
           move 0 to como-imposta SavePrezzo.
           move 0 to LastPrg.
           move 1 to store-riga.
           set tutto-ok    to true.
           move tof-chiave to rof-chiave.
           move low-value  to rof-riga.
           start rordforn key is >= rof-chiave 
                 invalid continue
           end-start
           set FirstTime to false.
           if status-rordforn = "00"
              perform until 1 = 2
                 read rordforn next no lock at end exit perform end-read
                 if rof-anno       not = tof-anno or
                    rof-numero not = tof-numero
                    read rordforn previous
                    move rof-cod-articolo to old-art-codice
                    exit perform
                 end-if

                 add 1 to righe-iniziali
                 if tof-inviato or tof-inserito
                    add 1 to LastPrg
                 else
                    |NON VA RIASSEGNATO PERCIO' TENGO L'ULTIMO
                    move rof-riga to LastPrg SaveLastPrg
                 end-if
                 add 1 to store-riga

      *        move rof-riga     to col-num
      *        move rof-cod-articolo to col-art art-codice
      *        read articoli no lock invalid continue end-read
      *
      *        initialize col-des
      *                                by low-value
      *        if SiAssortimento |and SaveGDO not = spaces
      *           perform ASSORCLI-IN-LINE
      *        end-if
      *
      *        move rof-qta-ord      to col-qta
      *        move rof-prz-unitario to col-uni

      *        initialize prg-chiave
      *        move rof-cod-articolo to prg-cod-articolo
      *        read progmag no lock invalid continue end-read
      **        move prg-ordinato-1 to save-ordinato
      *
      *        move rof-prg-chiave to prg-chiave
      *        read progmag no lock
      *           invalid
      *              initialize prg-rec replacing numeric data by zeroes
      *                                     alphanumeric data by spaces
      *        end-read
      *
      *        perform FIND-PROGMAG
      *        move rof-des-imballo   to hid-des-imballo
      *        move rof-qta-imballi   to hid-imballi

      *        move rof-perce-sconto  to col-sconto
      *        move rof-imp-consumo   to col-cons
      *        move rof-imp-cou-cobat to col-cou
      *        move rof-add-piombo    to col-add
      *        move rof-imponib-merce to col-imp
      *        move rof-cod-iva       to col-iva
      *        move rof-cod-art-cli   to hid-cod-art-cli
      *        move rof-omaggio       to hid-omaggio
      *        move prg-peso-utf      to hid-utf
      *        move prg-peso-non-utf  to hid-non-utf
       
                 move rof-riga  to col-num
                 move rof-cod-articolo   to art-codice
                                            col-art
                 read articoli
                    invalid
                       initialize art-descrizione
                 end-read
                 move art-descrizione to col-des

                 move rof-qta-ord           to col-qta
                 move rof-qta-evasa         to col-qta-arr
                 move rof-prz-unitario      to col-uni
                 move rof-sconto-1          to col-sconto-1
                 move rof-sconto-2          to col-sconto-2
                 move rof-sconto-3          to col-sconto-3
                 move rof-sconto-4          to col-sconto-4
                 move rof-sconto-5          to col-sconto-5
                 move rof-imponib-merce     to col-imp

                 move rof-imp-consumo       to col-consumo
                 move rof-imp-cou-cobat     to col-cou
                 move rof-add-piombo        to col-add
                 move rof-costi-aggiuntivi  to col-costi-agg
                 move rof-cod-iva           to col-iva

                 move rof-rec to hid-rof-rec
                 
                 modify form1-gd-1(store-riga,  1)
                        cell-data col-num
                 modify form1-gd-1(store-riga,  2)
                        cell-data col-art
                 modify form1-gd-1(store-riga,  3)
                        cell-data col-des
                 modify form1-gd-1(store-riga,  4)
                        cell-data col-qta
                 modify form1-gd-1(store-riga,  5)
                        cell-data col-qta-arr
                 modify form1-gd-1(store-riga,  6)
                        cell-data col-uni
                 modify form1-gd-1(store-riga,  7)
                        cell-data col-sconto-1
                 modify form1-gd-1(store-riga,  8)
                        cell-data col-sconto-2
                 modify form1-gd-1(store-riga,  9)
                        cell-data col-sconto-3
                 modify form1-gd-1(store-riga, 10)
                        cell-data col-sconto-4
                 modify form1-gd-1(store-riga, 11)
                        cell-data col-sconto-5
                 modify form1-gd-1(store-riga, 12)
                        cell-data col-imp
                 modify form1-gd-1(store-riga, 13)
                        cell-data col-consumo
                 modify form1-gd-1(store-riga, 14)
                        cell-data col-cou
                 modify form1-gd-1(store-riga, 15)
                        cell-data col-add
                 modify form1-gd-1(store-riga, 16)
                        cell-data col-costi-agg
                 modify form1-gd-1(store-riga, 17)
                        cell-data col-iva

                 modify form1-gd-1(store-riga, 1) 
                        hidden-data hid-rof-rec-1
                 modify form1-gd-1(store-riga, 2) 
                        hidden-data hid-rof-rec-2
                 modify form1-gd-1(store-riga, 3) 
                        hidden-data hid-rof-rec-3

      *          aggiungo i dati dei solleciti
                 move rof-chiave   to sof-chiave
                 read sordforn no lock
                      invalid
                      initialize sof-note
                                 sof-data-arr
                                 sof-qta
                 end-read
                 move sof-rec      to hid-sof-rec
                 move sof-note     to hid-sof-note    
                 move sof-data-arr to hid-sof-data-arr
                 move sof-qta      to hid-sof-qta

                 modify form1-gd-1(store-riga, 18) 
                        hidden-data hid-sof-rec-1
                 modify form1-gd-1(store-riga, 19) 
                        hidden-data hid-sof-rec-2
                 modify form1-gd-1(store-riga, 20) 
                        hidden-data hid-sof-rec-3
                 modify form1-gd-1(store-riga, 21) 
                        hidden-data hid-sof-rec-4

      *        if rof-si-omaggio
      *           modify form1-gd-1(store-riga, 78-NumColMan),  
      *                  bitmap = conferma-bmp
      *                  bitmap-number = 1
      *                  bitmap-width  = 19
      *        else
      *           modify form1-gd-1(store-riga, 78-NumColMan),
      *                  bitmap = conferma-bmp
      *                  bitmap-number = 2
      *                  bitmap-width  = 19
      *        end-if
      *
      *        if SaveGDO = spaces
      *           move art-prezzo-vendita    to hid-prezzo
      *        else
      *           if SiAssortimento
      *              move asc-prezzo-finito  to hid-prezzo
      *           end-if
      *        end-if
      *
      *        perform READ-TMARCHE
      *
      *        move rof-qta        to hid-old-qta
      *
OMAGGI*        move rof-qta-omaggi to hid-qta-omaggi
BLISTR*        move rof-blister    to hid-blister
      *
LABLAB*        move rof-promo      to hid-promo
LABLAB*        if rof-bloccato
LABLAB*           set hid-bloccato to true
LABLAB*        else
LABLAB*           set hid-bloccato to false
LABLAB*        end-if
LABLAB*        move rof-prz-commle to hid-prz-commle
      *
      *        move rof-prg-chiave to hid-rof-prg-chiave
      *
      *        move "IV"        to tbliv-codice1
      *        move rof-cod-iva to tbliv-codice2 hid-cod-iva
      *        read tivaese invalid continue end-read
      *        move tbliv-percentuale to hid-perce-iva
      *
      *        modify form1-gd-1(store-riga, 1), 
      *               hidden-data gruppo-hidden
      *
      *        compute SavePrezzo = rof-imponib-merce +
      *                             rof-imp-cou-cobat +
      *                             rof-imp-consumo   +
      *                             rof-add-piombo
OMAGGI*        compute SavePrezzo = 
OMAGGI*                SavePrezzo * ( rof-qta - rof-qta-omaggi )
      *
      *        perform varying idx from 1 by 1
      *                  until idx > 3
      *           if el-perce-iva(idx) = hid-perce-iva or
      *              el-imponib(idx)   = 0
      *              exit perform
      *           end-if
      *        end-perform
      *
      *        move    hid-perce-iva to el-perce-iva(idx)
      *        compute el-imponib(idx) = el-imponib(idx) + SavePrezzo
      *        
      *        if sw-check-rordforn = 0
      *           move rof-prg-chiave  to crof-prg-chiave
      *           move rof-des-imballo to crof-des-imballo
      *           move rof-blister     to crof-blister
      *           if rof-si-blister
      *              move 0 to crof-qta-imballi
      *           else
      *              move rof-qta-imballi to crof-qta-imballi
      *           end-if
      *           read check-rordforn no lock
      *                invalid 
      *                initialize crof-dati
      *                           replacing numeric data by zeroes
      *                                alphanumeric data by spaces
      *                move rof-peso-utf     to crof-peso-utf
      *                move rof-peso-non-utf to crof-peso-non-utf
      *            not invalid
      *                add 1 to crof-righe
      *           end-read
      *           add rof-num-colli to crof-num-colli
      *           add rof-qta       to crof-qta
      *           compute crof-importo =
      *                   crof-importo + ( rof-qta * rof-prz-unitario )
      *           write crof-rec invalid rewrite crof-rec end-write
      *        end-if

              end-perform
           end-if. 

      *****     move 0 to como-tot-ivato.
      *****     perform varying idx from 1 by 1 
      *****               until idx > 3
      *****
      *****        if el-imponib(idx) = 0
      *****           exit perform
      *****        end-if
      *****
      *****        compute como-iva = 
      *****              ( el-imponib(idx) * 
      *****                el-perce-iva(idx) ) / 100
      *****        
      *****        add 0,005     to como-iva
      *****        move como-iva to como-iva-2dec
      *****        
      *****        compute como-tot-ivato = 
      *****                como-tot-ivato +
      *****                como-iva-2dec  +
      *****                el-imponib(idx)
      *****     end-perform.
      *****
      *****     move como-tot-ivato to lab-tot-ivato-buf.

      *     if v-dett = 1
      *        display lab-tot-ivato
      *     end-if.

      *     if v-fatt = 1
      *        display lab-tot-fatt lab-totale-fatt
      *     end-if.
           
      *     if sw-check-rordforn = 0
      *        close check-rordforn
      *        move 1 to sw-check-rordforn
      *     end-if.

      ***---
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
              move 0 to hid-rof-num-riga
           else
              inquire form1-gd-1, cursor-y in riga        
           end-if. 
           if col-art    not = old-col-art
              set RigaCambiata to true
              if not NewRow
                 set YesDeleted   to true
              end-if
           end-if.

           if col-des    not = old-col-des
              set RigaCambiata to true
           end-if.


           if col-qta    not = old-col-qta
              set RigaCambiata to true
           end-if.

           if col-uni    not = old-col-uni
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if col-imp    not = old-col-imp
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if col-sconto-1 not = old-col-sconto-1 
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if col-sconto-2 not = old-col-sconto-2 
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if col-sconto-3 not = old-col-sconto-3 
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if col-sconto-4 not = old-col-sconto-4
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if col-sconto-5 not = old-col-sconto-5
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

      *     if col-cons   not = old-col-cons
      *        if not NewRow
      *           set PrezzoCambiato to true
      *        end-if
      *     end-if.

           if col-cou    not = old-col-cou
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if col-add    not = old-col-add
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if col-iva    not = old-col-iva
              continue
           end-if.

           if link-imballo-saved = 1
              move lab-art-buf  to col-des
      *        move link-qta    to hid-imballi 
              move ef-qta-buf   to col-qta
      *        move link-des    to hid-des-imballo
              move 0            to link-imballo-saved
           end-if. 

           move ef-imb-ord-buf  to hid-rof-imb-ordinato

      *     move ef-qta-oma-buf    to hid-qta-omaggi.
      *     move chk-blister-buf   to hid-blister.

      *     move tbliv-percentuale to hid-rof-cod-iva.
           move tbliv-codice2     to hid-rof-cod-iva.


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
                                      col-iva
           move chk-manuale-BUF    to hid-rof-manuale.
           move ef-impforn-buf     to hid-rof-imf-codice.
           move tlis-codice        to hid-rof-cod-listino

           move hid-rof-qta-arrivata  to col-qta-arr

           modify form1-gd-1(riga,  1) cell-data col-num.
           modify form1-gd-1(riga,  2) cell-data col-art.
           modify form1-gd-1(riga,  3) cell-data col-des.
           modify form1-gd-1(riga,  4) cell-data col-qta.
           modify form1-gd-1(riga,  5) cell-data col-qta-arr.
           modify form1-gd-1(riga,  6) cell-data col-uni.
           modify form1-gd-1(riga,  7) cell-data col-sconto-1.
           modify form1-gd-1(riga,  8) cell-data col-sconto-2.
           modify form1-gd-1(riga,  9) cell-data col-sconto-3.
           modify form1-gd-1(riga, 10) cell-data col-sconto-4.
           modify form1-gd-1(riga, 11) cell-data col-sconto-5.
           modify form1-gd-1(riga, 12) cell-data col-imp.
           modify form1-gd-1(riga, 13) cell-data col-consumo.
           modify form1-gd-1(riga, 14) cell-data col-cou.
           modify form1-gd-1(riga, 15) cell-data col-add.
           modify form1-gd-1(riga, 16) cell-data col-costi-agg.
           modify form1-gd-1(riga, 17) cell-data col-iva.

      *     move 78-ID-ef-art to control-id.
      *     move 4            to accept-control 

           modify form1-gd-1, cursor-y = riga.
           set NewRow         to true.
           perform AZZERA-MANUALE.

           modify form1-gd-1(riga, 1), hidden-data hid-rof-rec-1.
           modify form1-gd-1(riga, 2), hidden-data hid-rof-rec-2.
           modify form1-gd-1(riga, 3), hidden-data hid-rof-rec-3.


      ***---
       INIT.
           set TastiGiaAbilitati to false.
           set NonCambiareTab    to false.
           set ControllaCampi    to true. 
           set FirstTime         to true.
           set CheckAfterZoom    to false.
           move 0 to v-manuale v-guidata old-art-codice.
           move 1 to event-data-1 screen1-ta-1-tab-value 
           form1-radio-1-buf.
           perform SCREEN1-TA-1-TABCHANGE.
           move 0 to StatusHelp LastPrg e-man e-gui.
           initialize link-imballo-saved.

           move como-anno to ef-anno-buf.
           display ef-anno.

           set RigaCambiata        to false.
           set PrezzoCambiato      to false.
           set SollecitiCambiati   to false.
           set SollecitiCambiatiT  to false.

           move data-oggi to como-data.
           perform DATE-TO-SCREEN.
           move como-data to ef-data-buf |ef-data-pass-buf.
           display ef-data |ef-data-pass.

           move spaces to lab-art-buf lab-iva-buf.
                                
           move 0 to LastPrg.

           move 78-ID-ef-cli to control-id.
           move 4 to accept-control.
      *
           |Riempio il tabellino contenente i riferimenti agli ID
           |relativi ad ogni pagina del tab-control
           |specifico per i pgm. aventi Tab-Control
           move 78-ID-ef-data   to min-id(1).
           move 78-ID-ef-note-4 to max-id(1).
           |*******
      *
      *     Perform RIEMPI-COMBO-STATO.
      *
      *     move "Attivo" to cbo-stato-buf.
      *     Modify  cbo-stato,   value cbo-stato-buf.

           move 0 to mod.

           move 0 to mod-k.

      ***---
       INVIO-MAIL.
           accept como-data from century-date.
           accept como-ora  from time.
           accept system-information from system-info.

           initialize LinkSubject.
           string "ATTENZIONE: ERRORE RIGHE GESLUX " delimited size
                  " - VERIFICATOSI IN DATA: "        delimited size
                  como-data(7:2)                     delimited size
                  "/"                                delimited size
                  como-data(5:2)                     delimited size
                  "/"                                delimited size
                  como-data(1:4)                     delimited size
                  " - ALLE ORE: "                    delimited size
                  como-ora(1:2)                      delimited size
                  ":"                                delimited size
                  como-ora(3:2)                      delimited size
                  ". Programma: GORDCVAR"            delimited size
                  " - UTENTE: "                      delimited size
                  user-codi                          delimited size
                  " - DALLA POSTAZIONE: "            delimited size
                  user-id                            delimited size
                  into LinkSubject
           end-string.

           initialize LinkBody.
           evaluate tipo-messaggio
           when 1
                string "Numero: "                delimited size
                       tof-numero                delimited size
                       " GRIGLIA: "              delimited size
                       tot-righe                 delimited size
                       " REALI: "                delimited size
                       righe-finali              delimited size
                       " WRITE EFFETUATE: "      delimited size
                       write-effettuate          delimited size
                       ". FARE COPIA ARCHIVI!!!" delimited size
                       ". Programma: GORDCVAR"   delimited size
                       " - UTENTE: "             delimited size
                       user-codi                 delimited size
                       " - DALLA POSTAZIONE: "   delimited size
                       user-id                   delimited size
                       into LinkBody
                end-string
           when 2
                call "C$RERR" using extend-stat,
                                    text-message
                string "CANCELLAZIONE PREVENTIVA" delimited size
                       "(DELETE INVALID...)"      delimited size
                       " - TESTATA: "             delimited size
                       tof-chiave                 delimited size
                       " - RIGA: "                delimited size
                       rof-chiave                 delimited size
                       " - CODICE D'ERRORE: "     delimited size
                       extend-stat                delimited size
                       ". OPERAZIONE FALLITA!"    delimited size
                       ". Programma: GORDCVAR"    delimited size
                       " - UTENTE: "              delimited size
                       user-codi                  delimited size
                       " - DALLA POSTAZIONE: "    delimited size
                       user-id                    delimited size
                       into LinkBody
                end-string
           when 3
                call "C$RERR" using extend-stat, 
                                    text-message
                string "SCRITTURA DA GRIGLIA "    delimited size
                       "(WRITE INVALID...)"       delimited size
                       " - TESTATA: "             delimited size
                       tof-chiave                 delimited size
                       " - RIGA: "                delimited size
                       rof-chiave                 delimited size
                       " - CODICE D'ERRORE: "     delimited size
                       extend-stat                delimited size
                       ". OPERAZIONE FALLITA!"    delimited size
                       ". Programma: GORDCVAR"    delimited size
                       " - UTENTE: "              delimited size
                       user-codi                  delimited size
                       " - DALLA POSTAZIONE: "    delimited size
                       user-id                    delimited size
                       into LinkBody
                end-string
           when 4
                call "C$RERR" using extend-stat, 
                                    text-message
                if CancellazioneFisica
                   string "CANCELLAZIONE DELETE-RIGHE" delimited size
                          "(DELETE INVALID...)"        delimited size
                          " - TESTATA: "               delimited size
                          tof-chiave                   delimited size
                          " - RIGA: "                  delimited size
                          rof-chiave                   delimited size
                          " - CODICE D'ERRORE: "       delimited size
                          extend-stat                  delimited size
                          ". OPERAZIONE FALLITA!"      delimited size
                          ". Programma: GORDCVAR"      delimited size
                          " - UTENTE: "                delimited size
                           user-codi                   delimited size
                          " - DALLA POSTAZIONE: "      delimited size
                          user-id                      delimited size
                          into LinkBody
                   end-string
                else
                   string "REWRITE DELETE-RIGHE"   delimited size
                          "(CANCELLAZIONE LOGICA)" delimited size
                          " - TESTATA: "           delimited size
                          tof-chiave               delimited size
                          " - RIGA: "              delimited size
                          rof-chiave               delimited size
                          " - CODICE D'ERRORE: "   delimited size
                          extend-stat              delimited size
                          ". OPERAZIONE FALLITA!"  delimited size
                          ". Programma: GORDCVAR"  delimited size
                          " - UTENTE: "            delimited size
                          user-codi                delimited size
                          " - DALLA POSTAZIONE: "  delimited size
                          user-id                  delimited size
                          into LinkBody
                    end-string
                end-if
           when 5
                call "C$RERR" using extend-stat, 
                                    text-message
                string "CANCELLAZIONE DELETE-RIGHE"      delimited size
                       "(IF STATUS-rordforn NOT = 00...)" delimited size
                       " - TESTATA: "                    delimited size
                       tof-chiave                        delimited size
                       " - RIGA: "                       delimited size
                       rof-chiave                        delimited size
                       " - CODICE D'ERRORE: "            delimited size
                       extend-stat                       delimited size
                       ". OPERAZIONE FALLITA!"           delimited size
                       ". Programma: GORDCVAR"           delimited size
                       " - UTENTE: "                     delimited size
                       user-codi                         delimited size
                       " - DALLA POSTAZIONE: "           delimited size
                       user-id                           delimited size
                       into LinkBody
                end-string
           when 6
                call "C$RERR" using extend-stat, 
                                    text-message
                string "CANCELLAZIONE RIGHE CON FLAG"    delimited size
                       "(IF STATUS-rordforn NOT = 00...)" delimited size
                       " - TESTATA: "                    delimited size
                       tof-chiave                        delimited size
                       " - RIGA: "                       delimited size
                       rof-chiave                        delimited size
                       " - CODICE D'ERRORE: "            delimited size
                       extend-stat                       delimited size
                       ". OPERAZIONE FALLITA!"           delimited size
                       ". Programma: GORDFORNVAR"        delimited size
                       " - UTENTE: "                     delimited size
                       user-codi                         delimited size
                       " - DALLA POSTAZIONE: "           delimited size
                       user-id                           delimited size
                       into LinkBody
                end-string
           end-evaluate.
           accept prova-mail from environment "PROVA_MAIL".
           if MailInProva
              move "a.eventi@goodworks.it" to LinkAddress
           else
              initialize LinkAddress
              string "a.eventi@goodworks.it;" delimited size
                     into LinkAddress
              end-string
           end-if.             
           move "gordfornvar" to NomeProgramma.
           move 5 to tentativi-mail.
           perform CICLO-SEND-MAIL.
           initialize wk-path copy-status.
           accept  wk-date from century-date.
           accept  wk-hour from time.
           accept  wk-path from environment "PATH_BACKUP".
           inspect wk-path replacing trailing spaces by low-value.
           string  wk-path     delimited low-value
                   "InvioMail" delimited size
                   "_"         delimited size
                   wk-date     delimited size
                   "_"         delimited size
                   wk-hour     delimited size
                   ".ini"      delimited size
                   into wk-path
           end-string.
           call "C$COPY" using path-lineseq-mail, wk-path, "S"
                        giving copy-status.
           delete file lineseq-mail.

           perform 3 times
              set SystemErrorOccurred to true
              display message "ERRORE DI SISTEMA!!!"
                       x"0d0a""ORDINE NON AGGIORNATO!!!"
                       x"0d0a""CONTROLLARE!!!"
                        title titolo
                         icon 3
           end-perform.
           perform RECUPERA-RECORD.
           rollback transaction.

      ***---
       AFTER-SEND-MAIL.

      ***---
       LEGGI-ANNO.
           open input tparamge.
           move spaces to tge-codice.
           move 0 to tge-anno.
           read tparamge no lock invalid continue end-read.
           move tge-anno to ef-anno-buf.
           move tge-cod-iva-omag to iva-omaggio tbliv-codice2.
           move "IV"             to tbliv-codice1.
           read tivaese invalid continue end-read.
           close tparamge.

      ***---
       MODIFICA.
           move 5 to key-status.
           inquire tool-modifica, value in mod.
           set tutto-ok to true.
           
      *  se l'utente e' abilitato puo' modificare un record
           if mod = 1
              set YesMessage to true
              perform CURRENT-RECORD
              if tutto-ok
                 if save-riga > tot-righe
                    move tot-righe to save-riga
                 end-if
                 perform SETTA-RIGA
      *           if tof-chiuso
      *              display message "ATTENZIONE!!!"
      *                       x"0d0a""ORDINE CHIUSO!!!"
      *                       x"0d0a""PASSARE COMUNQUE ALLA MODIFICA?"
      *                        title titolo
      *                         icon 2
      *                         type mb-yes-no
      *                      default mb-no
      *                       giving scelta
      *           else
                    move mb-yes to scelta
      *           end-if
                 if scelta = mb-yes 
                    move 1 to mod
                 else
                    move 1 to NumBitmapArticoli 
                    move 0 to mod 
                              mod-campi
                              mod-cliente-destino
                 end-if
                 set StatusModifica to true
                 perform STATUS-BAR-MSG      
                 move 78-ID-ef-cau to control-id
                 set ControllaCampi   to true
              end-if                         
           else
              move 1 to mod
              perform SALV-MOD
              move 0 to mod
              if errori
                 move 1 to mod
              else
                 move 5000 to control-id
                 set NoMessage to true
                 perform CURRENT-RECORD
                 if tutto-ok
                    perform SETTA-RIGA
                 end-if
                 set StatusVisua to true
                 perform STATUS-BAR-MSG 
                 unlock articoli all records
                 move 5000 to control-id
              end-if
           end-if.

           if mod = 1 
              if control-id = 2000 |PB-DATI
                 move 2 to NumBitmapDatiBolla
              else                               
                 move 1 to NumBitmapDatiBolla
              end-if
              if control-id = 3000 |PB-COLLEG
                 move 2 to NumBitmapDocColl
              else                               
                 move 1 to NumBitmapDocColl
              end-if
           else                               
              move 3 to NumBitmapDatiBolla
              move 1 to NumBitmapDocColl
           end-if.
                                
           perform DISPLAY-SCREEN.
           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.

           move 4 to accept-control.
           move 0 to StatusHelp.
           perform STATUS-HELP.

      ****---
      * READ-PROGMAG-LOCK.
      *     set RecLocked to false.
      *     set trovato to true.
      *     read progmag lock invalid set trovato to false end-read.
      *     if RecLocked
      *        subtract 1 from riga giving idx
      *        move idx to riga-ed
      *        move prg-cod-articolo to codice-ed
      *        move prg-peso         to peso-ed
      *        display message box 
      *               "Impossibile procedere con la scrittura su file dei
      *-    " "
      *        x"0d0a""Progressivi di magazzino [PROG] relativo alla riga
      *-    " " riga-ed, ". "
      *        x"0d0a""====== KeyFile ======"
      *        x"0d0a""Articolo:       ", codice-ed
      *        x"0d0a""Magazzino:  ", prg-cod-magazzino
      *        x"0d0a""Imballo:       ", prg-tipo-imballo
      *        x"0d0a""Peso:           ", peso-ed 
      *        x"0d0a""================="
      *        x"0d0a""Record già in uso su altro terminale. Ritentare la
      *-    " connessione?"
      *                type mb-yes-no
      *                giving scelta
      *                title = tit-err
      *                icon mb-errof-icon
      *        if scelta = mb-yes perform READ-PROGMAG-LOCK end-if
      *     end-if.

      ***---
       RECUPERA-RECORD.

      ***---
       ROW-TO-ENTRY.
      *    Questo prf. porta i dati da grid a entry-field
      *    quando rec-grd e rec-not sono valorizzati

           inquire form1-gd-1(riga,  1) cell-data in col-num.
           inquire form1-gd-1(riga,  2) cell-data in col-art.
           inquire form1-gd-1(riga,  3) cell-data in col-des.
           inquire form1-gd-1(riga,  4) cell-data in col-qta.
           inquire form1-gd-1(riga,  5) cell-data in col-qta-arr.
           inquire form1-gd-1(riga,  6) cell-data in col-uni.
           inquire form1-gd-1(riga,  7) cell-data in col-sconto-1.
           inquire form1-gd-1(riga,  8) cell-data in col-sconto-2.
           inquire form1-gd-1(riga,  9) cell-data in col-sconto-3.
           inquire form1-gd-1(riga, 10) cell-data in col-sconto-4.
           inquire form1-gd-1(riga, 11) cell-data in col-sconto-5.
           inquire form1-gd-1(riga, 12) cell-data in col-imp.
           inquire form1-gd-1(riga, 13) cell-data in col-consumo.
           inquire form1-gd-1(riga, 14) cell-data in col-cou.
           inquire form1-gd-1(riga, 15) cell-data in col-add.
           inquire form1-gd-1(riga, 16) cell-data in col-costi-agg.
           inquire form1-gd-1(riga, 17) cell-data in col-iva.

           inquire form1-gd-1(riga, 1), hidden-data hid-rof-rec-1.
           inquire form1-gd-1(riga, 2), hidden-data hid-rof-rec-2.
           inquire form1-gd-1(riga, 3), hidden-data hid-rof-rec-3.

           inquire form1-gd-1(riga, 18) hidden-data hid-sof-rec-1
           inquire form1-gd-1(riga, 19) hidden-data hid-sof-rec-2
           inquire form1-gd-1(riga, 20) hidden-data hid-sof-rec-3
           inquire form1-gd-1(riga, 21) hidden-data hid-sof-rec-4

           if not StoSalvando
              move hid-rof-chiave-testa to rof-chiave
              read rordforn no lock 
                   invalid
                   if rof-riga   = 0 |Nuova riga
                      move 0 to rof-prz-unitario
                   else
                      display message "Chiave: " rof-chiave
                               x"0d0a""NON TROVATA SU ORDINI F"
                               x"0d0a""CONTATTARE ASSISTENZA!!!!!"
                               x"0d0a""SU RIGA: " riga
                                title tit-err
                                 icon 2
                  end-if
              end-read
              move rof-prz-unitario to old-prezzo
           end-if.

           move col-art  to art-codice.
           read articoli no lock invalid continue end-read.

           set costi-percentuale     to false.
           move hid-rof-cod-listino  to rlis-codice
           move art-codice   to rlis-articolo
           read rlistini no lock
                invalid
                move 0 to rlis-prz-acq
                          rlis-sconto-1
                          rlis-sconto-2
                          rlis-sconto-3
                          rlis-sconto-4
                          rlis-sconto-5
                          rlis-costi-agg
                          rlis-tipo-tratt-imposte
                          rlis-codice
           end-read.

           move hid-rof-tipo-imballo  to imq-codice.
           read timbalqta no lock invalid continue end-read.

           move col-art    to ef-art-buf  
                              old-art-codice 
                              art-codice.
           set lab-imp-coubat to true.
           read articoli no lock invalid continue end-read.
           if art-si-cobat
              set lab-imp-cobat to true
           else                        
              set lab-imp-cou   to true
           end-if.
           move col-des      to lab-art-buf.
           move col-qta      to ef-qta-buf rof-qta-ord.
           move col-uni      to ef-uni-buf rof-prz-unitario.
           move col-sconto-1 to ef-sconto-1-buf rof-sconto-1.
           move col-sconto-2 to ef-sconto-2-buf rof-sconto-2.
           move col-sconto-3 to ef-sconto-3-buf rof-sconto-3.
           move col-sconto-4 to ef-sconto-4-buf rof-sconto-4.
           move col-sconto-5 to ef-sconto-5-buf rof-sconto-5.

           move col-consumo  to ef-cons-buf   rof-imp-consumo.
           move col-cou      to ef-cou-buf    rof-imp-cou-cobat.
           move col-add      to ef-add-buf    rof-add-piombo.
           move col-imp      to ef-imp-buf    rof-imponib-merce.
           move col-iva      to ef-cod-iva-buf tbliv-codice2 rof-cod-iva
           move spaces       to tbliv-descrizione1 tbliv-descrizione2.
           move "IV"         to tbliv-codice1.

           move hid-rof-costi-aggiuntivi to ef-costi-agg-BUF
                                                        
           move hid-rof-prg-chiave to rof-prg-chiave prg-chiave.
           read progmag no lock
                invalid 
                display message "ARTICOLO " prg-cod-articolo
                         x"0d0a""RIGA SELEZIONATA"
                         x"0d0a""PROGRESSIVO INESISTENTE!!!"
                         x"0d0a""CANCELLARE E REINSERIRE LA RIGA"
                          title tit-err
                           icon 2
           end-read.
      *****     |IN OGNI CASO AGGIORNO I VALORI DAL LISTINO
      *****     if rlis-codice not = 0 and mod-campi = 1
      *****        move rlis-tipo-tratt-imposte to imf-codice
      *****        read impforn no lock invalid continue end-read
      *****        
      *****        inquire ef-data value in como-data
      *****        perform DATE-TO-FILE
      *****        move como-data to como-data-ordine
      *****
      *****        perform RECUPERA-PREZZO
      *****        perform RECUPERA-SCONTO
      *****        move rlis-scelta to como-prz-unitario
      *****        perform CALCOLO-IMPOSTE-FORNITORE
      *****        perform CALCOLA-IMPONIBILE
      *****        move imposta-consumo to ef-cons-buf
      *****        compute como-imposta = imposta-cobat + imposta-cou
      *****        move como-imposta to ef-cou-buf
      *****        move add-piombo   to ef-add-buf
      *****     end-if.

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

           move hid-rof-anno-ddt   to lbl-anno-ddt-BUF.
           move hid-rof-numero-ddt to lbl-num-ddt-BUF.
           if hid-rof-data-carico = 0
              move space  to lbl-dt-carico-BUF
           else
              move hid-rof-data-carico(7:2) to lbl-dt-carico-BUF(1:2)
              move "/"                      to lbl-dt-carico-BUF(3:1)
              move hid-rof-data-carico(5:2) to lbl-dt-carico-BUF(4:2)
              move "/"                      to lbl-dt-carico-BUF(6:1)
              move hid-rof-data-carico(1:4) to lbl-dt-carico-BUF(7:4)
           end-if
           if hid-rof-ora-carico = 0
              move space                    to lbl-ora-carico-BUF
           else
              move hid-rof-ora-carico(1:2)  to lbl-ora-carico-BUF(1:2)
              move ":"                      to lbl-ora-carico-BUF(3:1)
              move hid-rof-ora-carico(3:2)  to lbl-ora-carico-BUF(4:2)
           end-if.
           move hid-rof-utente-carico       to lbl-utente-carico-BUF.


           perform DATI-SOLLECITI.

           if hid-rof-qta-arrivata not = 0
              move 1 to e-pb-elmovcd
           else
              move 0 to e-pb-elmovcd
           end-if.               

           move hid-rof-prg-chiave to prg-chiave.
           move space              to prg-cod-magazzino
                                      prg-tipo-imballo.
           move 0                  to prg-peso.

           read progmag no lock invalid continue end-read.

           move ef-cons-buf to imposta-consumo.
           move ef-cou-buf  to imposta-cou.
           move 0           to imposta-cobat.
           move ef-add-buf  to add-piombo. 
           perform LABEL-PRZ-FINALE
           perform LABEL-VALORI.
           perform DISPLAY-SCREEN.
           set NewRow         to false.

           move hid-rof-sconto-1 to save-sconto-1.
           move hid-rof-sconto-2 to save-sconto-2.
           move hid-rof-sconto-3 to save-sconto-3.
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
           if mod = 0 
              exit paragraph 
           end-if.                            
                  
           set SaveXX to true.
           perform ACCESSOXX.
                  
PATCH      move BitmapSaveDisabled to BitmapNumSave
PATCH      move 0 to e-salva.
PATCH      modify tool-salva, 
PATCH             enabled = e-salva,
PATCH       bitmap-number = BitmapNumSave.

           set tutto-ok       to true.

           set StoSalvando to true.
           perform  varying CONTROL-ID from 78-ID-ef-anno by 1
                      until CONTROL-ID > 78-ID-ef-note-4
              perform CONTROLLO
              if errori 
                 exit perform 
              end-if
           end-perform.
           set StoSalvando to false.
                             
           if tutto-ok
              if CambiatoMagazzino
                 perform CONTROLLA-RIGHE
              end-if
           end-if.

      *     if tutto-ok
      *        perform SUPERAMENTO-500-UTF
      *     end-if.
      *
      *     if tutto-ok
      *        perform CONTROLLA-SE-PESO-SUP-24000-KG
      *     end-if.
      *
      *     if tutto-ok
      *        perform CONTROLLA-QTA-BLISTER
      *     end-if.

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
                 end-if
              end-if
           end-if.
      
           if errori
              |Specifico per il pgm. GORDCVAR
              if store-id < min-id(1)
                 move store-id to control-id
              else              

              |controllo che l'errore sia sulla pagina corrente
              |specifico per i pgm. aventi Tab-Control
                 inquire Screen1-Ta-1, value in pagina
                 if store-id >= min-id(pagina) and
                    store-id <= max-id(pagina)
                    move store-id to control-id
                 end-if
                 |******
              end-if
              move 4 to accept-control
           else
              perform CONTROLLA-TOTALE-MAN
              if errori
                 perform CANCELLA-COLORE
              else
                 perform SALVATAGGIO-EFFETTIVO
              end-if
           end-if.

           if not SystemErrorOccurred

              perform DISPLAY-SCREEN

              set environment "KEYSTROKE" to "DATA=44   44"
              set environment "KEYSTROKE" to "DATA=46   46"

              if mod = 0
PATCH            move BitmapSaveDisabled to BitmapNumSave
PATCH            move 0 to e-salva
              else                
PATCH            move BitmapSaveEnabled to BitmapNumSave
PATCH            move 1 to e-salva
              end-if
PATCH         modify tool-salva, 
PATCH                enabled = e-salva,
PATCH          bitmap-number = BitmapNumSave
              if tutto-ok
                 perform CURRENT-RECORD
                 move 2 to riga event-data-2
                 perform SPOSTAMENTO
              end-if
PATCH      else
PATCH         perform FORM1-EXIT
PATCH      end-if.                   
           perform DESTROYXX.

      ***---
       SALV-MOD.
           set tutto-ok to true.
           perform FORM1-CONTROLLO-OLD.  

           if RigaCambiata and mod = 1
              set NoSalvato to true
           end-if.

           if PrezzoCambiato and mod = 1
              set NoSalvato to true
           end-if.

           if SollecitiCambiati
              set NoSalvato to true
           end-if.
              

      *     if SiSalvato
      *        if tof-data-bolla not = orig-data-bolla or
      *           tof-num-bolla  not = orig-num-bolla
      *           set NoSalvato to true
      *        end-if
      *     end-if.
                     
           if NoSalvato
              display message MSG-Salvare-le-modifiche
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
                   |Specifico per il pgm. GORDCVAR
                   if store-id < min-id(1)
                      move store-id to control-id
                   else              
                   |controllo che l'errore sia sulla pagina corrente
                   |specifico per i pgm. aventi Tab-Control
                      inquire Screen1-Ta-1, value in pagina
                      if store-id >= min-id(pagina) and
                         store-id <= max-id(pagina)
                         move store-id to control-id
                      end-if
                      |******
                   end-if
                   move 4        to accept-control
              end-evaluate
           end-if.

      ***---
       SCRIVI-FILE-BACKUP.
      ********--- DEVE RIMANERE IDENTICO A QUELLO DI GORDC
      ********--- MA NON DEV'ESSERE PORTATO IN COPY!!!!
PATCH*********--- Se tutto è andatao a buon fine cancello e riscrivo
      *****      |il file di backup tale e quale a quello effettivo
      *****     move tof-chiave to btof-chiave.
      *****     delete btordforn record invalid continue end-delete.
      *****     move low-value  to brof-rec.
      *****     move tof-chiave to brof-chiave.
      *****     start brordforn key >= brof-chiave
      *****        invalid 
      *****           continue |nel caso in cui sia nuovo
      *****        not invalid
      *****           perform until 1 = 2
      *****              read brordforn next 
      *****                 at end 
      *****                    exit perform 
      *****              end-read
      *****              if brof-anno       not = btof-anno or
      *****                 brof-num-ordine not = btof-numero
      *****                 exit perform
      *****              end-if
      *****              delete brordforn record 
      *****                 invalid 
      *****                    continue 
      *****              end-delete
      *****           end-perform
      *****     end-start.
      *****
      *****     read tordforn no lock
      *****        invalid 
      *****           continue |se annullata l'evasione
      *****        not invalid
      *****           move tof-rec to btof-rec
      *****           write btof-rec invalid continue end-write
      *****           move low-value  to rof-rec
      *****           move tof-chiave to rof-chiave
      *****           start rordforn key >= rof-chiave
      *****              invalid 
      *****                 continue
      *****              not invalid
      *****                 perform until 1 = 2
      *****                    read rordforn next 
      *****                       at end 
      *****                          exit perform 
      *****                    end-read
      *****                    if rof-anno       not = tof-anno or
      *****                       rof-numero not = tof-numero
      *****                      exit perform
      *****                    end-if
      *****                    move rof-rec to brof-rec
      *****                    write brof-rec 
      *****                       invalid 
      *****                          continue 
      *****                    end-write
      *****                 end-perform
      *****          end-start
      *****     end-read.

      ***---
       SCRIVI-RIGHE-MAN.
PATCH      move 0   to righe-finali 
                       progressivo
                       write-effettuate
                       tof-pz-tot
                       tof-pz-arrivati.
                        
           set CancellazioneLogica to true.
           set HoSalvato           to true.
           |Azzero prima TUTTE le quantità sui progressivi
           perform DELETE-RIGHE.     
           add 1 to SaveLastPrg giving LastPrg.

           if not SystemErrorOccurred
              perform CALCOLA-MESE-RIF
              set tutto-ok        to true
LABLAB        set ordine-bloccato to false
LABLAB        set no-promo        to true
LUBEXX        set EsisteIVA to false
              inquire form1-gd-1, last-row in tot-righe
              perform varying riga from 2 by 1
                        until riga > tot-righe
                 add 1 to progressivo

                 inquire form1-gd-1(riga, 1) hidden-data hid-rof-rec-1
                 inquire form1-gd-1(riga, 2) hidden-data hid-rof-rec-2
                 inquire form1-gd-1(riga, 3) hidden-data hid-rof-rec-3
                 move hid-rof-rec  to rof-rec

                 move tof-chiave   to rof-chiave-testa

                 |Se sto già lavorando l'ordine NON devo  riassegnare
                 |la riga altrimenti se ci sono evasioni perde 
                 |l'associazione fatta con questo numero
                 if tof-inserito or tof-inviato
                    move progressivo      to rof-riga
                 else
                    if hid-rof-num-riga not = 0
                       move hid-rof-num-riga to rof-riga
                    else
                       move LastPrg to rof-riga
                       add 1 to LastPrg
                    end-if
                 end-if
                 modify form1-gd-1(riga, 1), cell-data rof-riga
              
                 move tof-dati-comuni to rof-dati-comuni
              
                 evaluate true
                 when tof-inserito
      *****           when tof-accettato
                      continue
                 when tof-inviato
                 when tof-in-lavorazione
                 when tof-chiuso
                      perform CHIAMA-PROGMAG
                 end-evaluate

                 move hid-rof-prg-chiave to rof-prg-chiave prg-chiave
               
LUBEXX           perform FORZA-PESO-UGUALE
               
LABLAB           if rof-promo not = 0
LABLAB              set  si-promo  to true
LABLAB           end-if

PATCH            if rof-numero = 0
PATCH               display message "EVASIONE ZERO"
PATCH                        x"0d0a""RICARICARE L'ORDINE"
PATCH                         title tit-err
PATCH                          icon 3
PATCH            end-if

                 accept como-ora from time
                 move data-oggi  to rof-data-ultima-modifica
                 move como-ora   to rof-ora-ultima-modifica
                 move user-codi  to rof-utente-ultima-modifica
                 add rof-qta-ord to tof-pz-tot
                 add rof-qta-evasa to tof-pz-arrivati
              
      *           set rof-cancellato to false
                 write rof-rec
                       invalid 
                       rewrite rof-rec
                               invalid continue
PATCH                      not invalid add 1 to write-effettuate
                       end-rewrite
PATCH              not invalid 
                       add 1 to write-effettuate
                 end-write

                 evaluate status-rordforn 
                 when "00"
                 when "02" continue
                 when other
PATCH                 move 3 to tipo-messaggio
PATCH                 perform INVIO-MAIL
PATCH                 exit perform
                 end-evaluate        

              end-perform 
                                      
LABLAB*        if ordine-bloccato
LABLAB*           set tof-bloccato to true
LABLAB*        end-if
              
PATCH *       Per monitorare l'errore di cancellazione/inserimento altre righe
              if righe-iniziali not = write-effettuate
                 move righe-iniziali   to righe-iniziali-x
                 move write-effettuate to righe-finali-x
                 inspect righe-iniziali-x 
                         replacing leading x"30" by x"20"
                 inspect righe-iniziali-x 
                         replacing leading x"30" by x"20"
                 call "C$JUSTIFY" using righe-iniziali-x, "L"
                 call "C$JUSTIFY" using righe-finali-x,   "L"
                 initialize geslock-linkage 
                            replacing numeric data by zeroes
                                 alphanumeric data by spaces
                 move 1 to geslock-v-salva
                 set geslock-righe to true
                 string "Da "            delimited size
                     righe-iniziali-x delimited spaces
                     " a "            delimited size
                     righe-finali-x   delimited spaces
                     " righe."        delimited size
                     into geslock-messaggio
                 end-string
                 perform 5 times
                    call   "geslock" using geslock-linkage
                    cancel "geslock"
                 end-perform
              end-if
              
PATCH**       Invio mail nel caso in cui le righe presenti su
PATCH**       griglia non siano le stesse scritte su file 
PATCH         inquire form1-gd-1, last-row in save-tot-righe
PATCH         subtract 1 from save-tot-righe
              
PATCH         move 0 to righe-finali
PATCH         move low-value  to rof-chiave
PATCH         move tof-anno   to rof-anno
PATCH         move tof-numero to rof-numero
PATCH         start rordforn key >= rof-chiave
PATCH               invalid  continue
PATCH           not invalid
PATCH               perform until 1 = 2
PATCH                  read rordforn next no lock 
PATCH                       at end exit perform 
PATCH                  end-read
PATCH                  if rof-anno       not = tof-anno or
PATCH                     rof-numero not = tof-numero
PATCH                     exit perform
PATCH                  end-if
PATCH                  add 1 to righe-finali
PATCH               end-perform
PATCH         end-start
PATCH      end-if.

      ***---
      * Se inserisco una riga devo creare il sollecito
       SCRIVI-SOLLECITI-R.
           move tof-chiave      to sof-chiave-testa.
           move 0               to sof-prog
           read sordforn.
           move sof-dati-salvati to t-dati-salvati.

           move low-value  to rof-chiave.
           move tof-chiave to rof-chiave.
           start rordforn key >= rof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordforn next at end exit perform end-read
                    if rof-chiave-testa not = tof-chiave
                       exit perform
                    end-if
                    initialize sof-rec replacing numeric data by zeroes
                                            alphanumeric data by spaces
                    move rof-chiave      to sof-chiave
                    read sordforn no lock
                         invalid
                         move spaces          to sof-note    
                         move t-sof-data-arr  to sof-data-arr
                         move 0               to sof-qta            
                         move t-dati-salvati  to sof-dati-salvati
                         accept sof-data-creazione from century-date
                         accept sof-ora-creazione  from time
                         move rof-utente-creazione 
                           to sof-utente-creazione
                         if gsordforn-data-conf
                            accept sof-data-conf from century-date
                         end-if
                         write sof-rec end-write
                     not invalid
                         if t-dati-salvati not = sof-dati-salvati
                            move t-dati-salvati to sof-dati-salvati       
                            if gsordforn-data-conf
                               accept sof-data-conf from century-date
                            end-if
                            rewrite sof-rec
                         end-if
                    end-read
                 end-perform
           end-start.

      ***---
       CHIAMA-PROGMAG.
           perform VALORIZZA-ARRAY-CAUSALI
           set link-update     to true.
           move rof-prg-chiave to link-key.
           move ef-cau-buf     to link-causale.
           move tof-mese-rif   to link-mese-rif.

           |Voglio cambiare il flag...
           if tof-aperto not = old-tof-aperto
              |...la quantità è GIA' STATA TOLTA perciò DEVO NEUTRALIZZARE
              if tof-aperto-si
                 move 7 to link-mese-rif
              else
              |...DEVO AGGIUNGERE e non far 
              |fare nessun controllo al wprogmag
                 initialize link-chiave-origine
              end-if
           else
           |Il flag non cambia perciò va bene 
           |quello che c'è scritto nel record
              move tof-chiave to link-chiave-origine
           end-if.

           move user-codi to link-user of link-wprogmag.
      *    storno l'ordinato
           compute link-valore = rof-qta-ord - rof-qta-evasa.
           if link-valore not = 0
              move "0010000000000000" to link-array
              call   "wprogmag" using link-wprogmag
              cancel "wprogmag"
           end-if.
                                                
           move tof-chiave     to link-chiave-origine.
      *    storno la qta arrivata
           if rof-qta-evasa not = 0
              move rof-qta-evasa      to link-valore
              move "1000000000000010" to link-array
              call   "wprogmag"   using link-wprogmag
              cancel "wprogmag"
           end-if.


      ***---
       TORNA-IN-VISUA.
           move 0 to mod.
           move 0 to mod-k.
           move 78-ID-ef-anno to control-id.
           set NoMessage to true.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG.
           unlock tordforn all records.
                        
           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.
           move 4 to ACCEPT-CONTROL.

      *     move 3 to NumBitmapDatiBolla.
           initialize hid-rof-rec old-art-codice.

           move 0      to volantino-forzato.
      *     move spaces to lab-forzato-buf.
      *     display lab-forzato.

PATCH      |Usato come flag (se 0 non ho richiamato la mail)
PATCH      move 0 to tipo-messaggio save-tot-righe.

      ***---
       STAMPA.
           move tof-chiave   to stof-tof-chiave.
           set stof-normale  to true.

           if anteprima
              set stof-anteprima   to true
           else
              set stof-scegli-stampante   to true
           end-if.


           call   "st-ordforn" using st-ordforn-linkage.
           cancel "st-ordforn".

      ****---
      * TROVA-BOLLA.
      **    QUESTO PRF. VA LASCIATO QUI PER FORZA!!!
      *     set trovato to false.
      *     move tge-anno      to tof-anno-bolla.
      *     read tordforn no lock key is k-bolla
      *          invalid continue
      *      not invalid set trovato to true
      *     end-read.

      ***---
       VALORIZZA-OLD.
           move tof-rec      to old-tof-rec.
           move como-note(1) to old-como-note(1)
           move como-note(2) to old-como-note(2)
           move como-note(3) to old-como-note(3)
           move como-note(4) to old-como-note(4)
           set vecchio                  to true.
      
      *     if old-tof-spostam-ric-ago = space
      *        move "N" to old-tof-spostam-ric-ago
      *     end-if.
      
      *     if old-tof-spostam-ric-dic = space
      *        move "N" to old-tof-spostam-ric-dic
      *     end-if.
      
           evaluate CONTROL-ID
      *     when 78-ID-ef-age
           when 78-ID-ef-pag
      *     when 78-ID-ef-iva
      *     when 78-ID-ef-vet
           when 78-ID-ef-cod-iva
                move 1 to StatusHelp
           when other
                move 0 to StatusHelp
           end-evaluate.
           perform STATUS-HELP.
      
      *     move 0 to riga-nuova.
           set RigaCambiata   to false.
           set PrezzoCambiato to false.
           set SollecitiCambiati   to false.
           set SollecitiCambiatiT  to false.

LUBEXX*     move tof-data-bolla to orig-data-bolla.
LUBEXX*     move tof-num-bolla  to orig-num-bolla.

      ***---
       BEFORE-ACCEPT-CODE.
PATCH      call "c$calledby" using PgmChiamante.

           set RicaricaGrid        to false.
           set MousePressed        to false.
           set SystemErrorOccurred to false.

           accept  data-oggi from century-date.
           perform INIT.   
           perform LEGGI-ANNO.
                         
           move LinkChiave   to tof-chiave.
           read tordforn  no lock invalid  continue end-read.
           move tof-cod-forn to cli-codice of clienti
                                desf-codice.
           set cli-tipo-f    of clienti    to true.
           read clienti no lock 
              invalid 
                 continue 
           end-read.

           move tof-destino  to desf-prog
           read destinif
              invalid
                 continue
           end-read
           move tof-cliente to cli-codice of clienti1.
           set cli-tipo-c   of clienti1   to true.
           read clienti1 no lock invalid  continue end-read.

           move tof-cliente   to des-codice.
           move tof-destino-c to des-prog.
           read destini no lock invalid  continue end-read.

           perform INIT-OLD-REC.
           perform ABILITA-TOOLBAR.
           move 1 to e-stampa.
           modify tool-stampa, enabled e-stampa, bitmap-number = 7.
      *    da richieste del Sig. Trivella 07/12/2004
      *    se l'ordine è fatturato disabilito la modifica
      *     if tof-data-fattura not = 0 or tof-anno-fattura not = 0
      *        move 0 to e-modifica
      *        move BitmapEditDisabled to BitmapNumEdit
      *        modify tool-modifica, enabled = e-modifica
      *                              bitmap-number = BitmapNumEdit
      *     end-if.
      *
           move 0 to e-nuovo.
           modify tool-nuovo, enabled = e-nuovo.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG.

           move LinkChiave to tof-chiave.
      *     perform CANCELLA-FLAG.
           perform CURRENT-RECORD.
           perform DISPLAY-SCREEN.
           move 2 to save-riga event-data-2 riga.
           move 4 to event-data-1 colonna.
           perform SETTA-RIGA.      

      *     if PgmChiamante = "del-ordinif"
           if LinkOrdineDeleteOp = 1
              move -1 to LinkOrdineDeleteOp
              perform MODIFICA
              if tutto-ok
                 perform CANCELLA
              end-if
              move 27 to key-status
           end-if.

           move LinkNumero to numero-edit.
           display lab-numero.
           if livello-abil < 2 
      *****        set ArticoloSetFocus to false
              set NonCambiareTab   to false
           end-if.
                                     
           accept imp-data from century-date.
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.
                                         
           move tof-chiave to aor-chiave-testa art-ordforn-chiave.
           move low-value  to aor-prog
           perform ARTICOLI-DA-CONFERMARE.

      *     evaluate PgmChiamante 
      *     when "selnota"
      *          modify pb-notacr, bitmap-number = 1, 
      *                            title "Creazione &Nota Credito"
      *          move 1 to NumBitmapNotaCr
      *     when "selbozza"
      *          modify pb-notacr, bitmap-number = 3, 
      *                            title "Creazione &Bozza"
      *          move 3 to NumBitmapNotaCr
      *     when "selcont"
      *          modify pb-notacr, bitmap-number = 7, 
      *                            title "Creazione Contestazioni"
      *          move 7 to NumBitmapNotaCr
      *     end-evaluate.

      *     perform ABILITA-COLLEGATI.

      ***---

       AFTER-BUF-TO-FLD-CODE.
      *     move "IV" to tbliv-codice1.
      *     move tof-cod-ese-iva to tbliv-codice2.
      *     read tivaese invalid continue end-read.
      *                                              
      *     move lab-num-bolla-buf  to tof-num-bolla.


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

           if form1-radio-1-buf = 1
              set tof-urgente to true
           end-if.

      *
           move ef-cli-buf to tof-cod-forn  convert.  
           move ef-dest-buf to tof-destino  convert.  
                                                    
           accept como-ora from time.
           move data-oggi  to tof-data-ultima-modifica.
           move como-ora   to tof-ora-ultima-modifica.
           move user-codi  to tof-utente-ultima-modifica.

      ***---
       AFTER-FLD-TO-BUF-CODE.
           move tof-data-ordine    to como-data.
           perform DATE-TO-SCREEN.
           move como-data          to ef-data-buf.

           move tof-data-consegna  to como-data.
           perform DATE-TO-SCREEN.
           move como-data          to ef-data-cons-buf
      
      *     move tof-data-arr-merce to como-data.
      *     perform DATE-TO-SCREEN.
      *     move como-data          to ef-data-arrivo-buf.

           move tof-data-listino   to como-data.
           perform DATE-TO-SCREEN.
           move como-data          to ef-data-listino-buf.

           move tof-data-invio     to como-data.
           perform DATE-TO-SCREEN.
           move como-data          to lbl-data-invio-buf.

           move tof-ora-invio(1:4) to lbl-ora-invio-buf

           move tof-data-chiusura     to como-data.
           perform DATE-TO-SCREEN.
           move como-data          to lbl-data-chiusura-buf.

           move tof-ora-chiusura(1:4) to lbl-ora-chiusura-buf

           if tof-urgente move 1 to form1-radio-1-buf
           else           move 2 to form1-radio-1-buf
           end-if.

           if tof-destino-c = 0
              move spaces to lab-des-cli-buf lab-des-loca-buf
           end-if.   

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

      *     evaluate true
      *     when tof-fatt-si-prenotata 
      *          move "FATTURA PRENOTATA"     to lab-fatt-pren-buf
      *     when tof-fatt-no-prenotata                        
      *          move "FATTURA NON PRENOTATA" to lab-fatt-pren-buf
      *     end-evaluate.                       
      *
      *     evaluate true
      *     when tof-bolla-si-prenotata 
      *          move "BOLLA PRENOTATA"       to lab-bolla-pren-buf
      *     when tof-bolla-no-prenotata                        
      *          move "BOLLA NON PRENOTATA"   to lab-bolla-pren-buf
      *     end-evaluate.

      *     evaluate true
      *     when tof-invio-manuale
      *          move "INVIO MANUALE" to lab-invio-buf
      *     when tof-invio-postel
      *          move "INVIO POSTEL"  to lab-invio-buf
      *     end-evaluate.

      *     initialize lab-lotto-buf.
      *     if tof-num-prenot not = 0
      *        move tof-num-prenot to lotto-x
      *        inspect lotto-x replacing leading x"30" by x"20"
      *        call "C$JUSTIFY" using lotto-x, "L"
      *        inspect lotto-x replacing trailing spaces by low-value
      *        string "LOTTO N. " delimited by size
      *               lotto-x     delimited by low-value
      *               into lab-lotto-buf
      *        end-string
      *     end-if.

           move spaces      to old-magazzino.
           move tof-causale to tca-codice.
           read tcaumag     no  lock
                invalid     continue
            not invalid     
                 move tca-cod-magaz to old-magazzino
           end-read.

      ***---
       VAL-NOTE-FORN.

      ***---
       VAL-NOTE-LISTINO.

      ***---
       LOAD-NOTE.
           initialize como-note(1)
                      como-note(2)
                      como-note(3)
                      como-note(4)
           move tof-chiave   to nof-chiave-ordine
           move low-value    to nof-num-nota

           start NORDFORN key not < nof-chiave
              invalid
                 continue
              not invalid
                 move 0 to cont
                 perform until 1 = 2
                    read NORDFORN next no lock
                       at end
                          exit perform
                    end-read
                    if tof-chiave not = nof-chiave-ordine
                       exit perform
                    end-if
                    add 1    to cont
                    move nof-rec   to tmp-nof-rec
                    write TMP-NOF-REC
                       invalid
                          continue
                    end-write
                    if cont < 5
                       move nof-nota  to como-note(cont)
                    end-if
                 end-perform
           end-start.

      ***---  
       SALVATAGGIO-EFFETTIVO.
           perform FORM1-BUF-TO-FLD.
           inquire form1-gd-1, cursor-y in riga.
           move riga           to event-data-2.
           perform SPOSTAMENTO.

           start transaction.

      *     if RigaCambiata or PrezzoCambiato
              perform SCRIVI-RIGHE-MAN

              inquire form1-gd-1, cursor-y in riga

              if save-tot-righe not = righe-finali
                 move 1 to tipo-messaggio
                 perform INVIO-MAIL
             end-if
      *     end-if
           perform CANCELLA-NOTE
           perform SALVA-NOTE.      

           if not SystemErrorOccurred
              perform CANCELLA-COLORE
              move LinkChiave to tof-chiave
              perform BEFORE-FLDTOBUF-CODE
              move como-stato to lab-stato-buf
              display lab-stato

LUBEXX*****Risetto lo stato dell'ordine cone quello iniziale
LUBEXX*        move save-stato to tof-stato

PATCH         if tof-numero = 0
PATCH            display message "EVASIONE ZERO"
PATCH                  x"0d0a""RICARICARE L'ORDINE"
PATCH                   title tit-err
PATCH                    icon 3
PATCH         end-if

PATCH         commit transaction

              if tof-data-consegna not = old-tof-data-consegna
                 display message "Aggiornare i solleciti "
                                 "con la data di consegna?"
                           title titolo
                            icon 2
                            type mb-yes-no
                         default mb-no
                         giving scelta
                 if scelta = mb-yes
                    move tof-data-consegna to t-sof-data-arr
                    move 1 to forza-testata-solleciti
                 end-if
              end-if
                                          
              perform SALVA-SOLLECITI-T
              perform CANCELLA-SOLLECITI  
              perform SCRIVI-SOLLECITI

              move tof-chiave to aor-chiave-testa
              move low-value to aor-prog
              start art-ordforn key >= aor-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read art-ordforn next no lock
                          at end 
                             exit perform
                       end-read
                       if aor-chiave-testa not = tof-chiave
                          exit perform
                       end-if
                       add aor-qta to tof-pz-tot
                    end-perform
              end-start

              perform SCRIVI-SOLLECITI-R

              rewrite tof-rec invalid continue end-rewrite
              unlock  tordforn all records

              perform AGGIORNA-STATO-ORDF

              if tof-automatico
                 call   "ordf-ord" using tof-anno tof-numero tof-numero
                 cancel "ordf-ord"
              end-if

              move tof-chiave   to stof-tof-chiave
              if PrezzoCambiato   
                 set stof-normale  to true
                 call   "st-ordforn" using st-ordforn-linkage
                 cancel "st-ordforn"
              end-if

              if RigaCambiata       
                 set stof-normale  to true
                 call   "st-ordforn" using st-ordforn-linkage, 
                 cancel "st-ordforn"
              end-if
           
      **     if tof-causale = "OMOM"   
      **        initialize geslock-linkage 
      **                   replacing numeric data by zeroes
      **                        alphanumeric data by spaces
      **        move 1 to geslock-v-salva
      **        set geslock-omom to true
      **        move 
      **        "RICORDARSI D'INSERIRE DETTAGLI PROMOZIONE!!!"
      **          to geslock-messaggio
      **        perform 5 times
      **           call   "geslock" using geslock-linkage
      **           cancel "geslock"
      **        end-perform
      **     end-if

LUBEXX*     if sw-check-rordforn = 1
LUBEXX*        perform AGGIORNA-FILE-CHECK
LUBEXX*     end-if

PATCH      |E' già arrivata la mail che il file non
PATCH      |è stato scritto correttamente ed evito
PATCH      |di danneggiare anche il file di backup
PATCH         if tipo-messaggio = 0
PATCH            perform SCRIVI-FILE-BACKUP
PATCH         end-if

              move 0 to v-dett
              set vecchio to true
LUBEXX        perform VALORIZZA-OLD
              perform TORNA-IN-VISUA
              set RicaricaGrid to true
           end-if.
           set RigaCambiata        to false.
           set PrezzoCambiato      to false.
           set SollecitiCambiati   to false.
           set SollecitiCambiatiT  to false.

      ***---
       CANCELLA-NOTE.
           move tof-chiave   to nof-chiave-ordine
           move low-value    to nof-num-nota
           start nordforn key >= nof-chiave
                 invalid continue
              not invalid
                 perform until 1 = 2
                    read nordforn next no lock
                      at end exit perform
                    end-read
                    if tof-chiave not = nof-chiave-ordine
                       exit perform
                    end-if
                    delete nordforn record
                 end-perform
           end-start.

      ***---
       CANCELLA-SOLLECITI.
           move tof-chiave to sof-chiave-testa
           move 1          to sof-prog
           start sordforn key >= sof-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read sordforn next no lock
                       at end exit perform
                    end-read
                    if tof-chiave not = sof-chiave-testa
                       exit perform
                    end-if
                    delete sordforn record
                 end-perform
           end-start.

      ***---
       SCRIVI-SOLLECITI.
           inquire form1-gd-1, last-row in tot-righe
           perform varying riga from 2 by 1
                     until riga > tot-righe
              perform SCRIVI-SOL-R
           end-perform. 
           if SollecitiCambiati or SollecitiCambiatiT
              |Aggiorno i dati salvati nella testata
              move rof-chiave      to sof-chiave
              move 0               to sof-prog
              read sordforn
              set sof-dati-salvati-si to true
              rewrite sof-rec
           end-if.

      ***---
       SCRIVI-SOL-R.                                           
           inquire form1-gd-1(riga, 1), cell-data in rof-riga.
           move tof-chiave to rof-chiave-testa.
           read rordforn no lock.
           
           inquire form1-gd-1(riga, 18) hidden-data hid-sof-rec-1.
           inquire form1-gd-1(riga, 19) hidden-data hid-sof-rec-2.
           inquire form1-gd-1(riga, 20) hidden-data hid-sof-rec-3.
           inquire form1-gd-1(riga, 21) hidden-data hid-sof-rec-4.

           move hid-sof-rec        to sof-rec.

           move hid-sof-note       to sof-note.
      *     move tof-data-consegna  to sof-data-arr.
           move hid-sof-data-arr   to sof-data-arr.
           move hid-sof-qta        to sof-qta.

      *    scrivo solo se c'è qualcosa
           if sof-note     not = space or
              sof-data-arr not = 0     or
              sof-qta      not = 0 

              if forza-testata-solleciti = 1  
                 compute sof-qta = rof-qta-ord - rof-qta-evasa

                 move t-sof-data-arr  to sof-data-arr
                 accept sof-data-conf from century-date
              end-if

              move rof-chiave      to sof-chiave
              move rof-dati-comuni to sof-dati-comuni   
              if SollecitiCambiati
                 set sof-dati-salvati-si to true
              end-if       

              if gsordforn-data-conf
                 accept sof-data-conf from century-date
              end-if

              write sof-rec 
                    invalid rewrite sof-rec invalid continue end-rewrite
              end-write
           else
              if SollecitiCambiatiT
      *    altrimenti metto la data di testata

                 move rof-chiave      to sof-chiave
                 move t-sof-data-arr  to sof-data-arr
                 move tof-dati-comuni to sof-dati-comuni
                 set sof-dati-salvati-si to true  

                 if gsordforn-data-conf
                    accept sof-data-conf from century-date
                 end-if

                 write sof-rec 
                       invalid 
                       rewrite sof-rec invalid continue end-rewrite
                 end-write
              end-if
           end-if.

      
      ***---
       CAMBIA-STATO.
           accept como-data from century-date
           accept como-ora from time
           evaluate true
           when n-inserito
                evaluate true
                when tof-inserito
      *****          when tof-accettato
                     continue
                when tof-inviato
                     perform STORNA-ORDINATO 

                     initialize tof-tipo-invio
                                tof-data-invio
                                tof-ora-invio
                                tof-utente-invio
                end-evaluate 
                set tof-inserito   to true
           when n-accettato
                evaluate true
                when tof-inserito
      *****          when tof-accettato
                     continue
                when tof-inviato
                     perform STORNA-ORDINATO 
              
                     initialize tof-tipo-invio
                                tof-data-invio
                                tof-ora-invio
                                tof-utente-invio
                end-evaluate
      *****          set tof-accettato  to true

           when n-aperto
                if tof-pz-arrivati = 0
                   set tof-inserito   to true
                   initialize tof-tipo-invio
                              tof-data-invio
                              tof-ora-invio
                              tof-utente-invio
                else
                   set tof-in-lavorazione   to true
                   perform RIPRISTINA-ORDINATO
                end-if
                initialize tof-tipo-chiusura
                           tof-nota-chiusura
                           tof-data-chiusura
                           tof-ora-chiusura
                           tof-utente-chiusura

           when n-chiuso
                evaluate true
                when tof-inserito
      *****          when tof-accettato
                     continue
                when tof-inviato
                when tof-in-lavorazione
                     perform STORNA-ORDINATO
                end-evaluate
                set tof-chiuso  to true
                set tof-chiusura-man  to true
                inquire ef-nota-chiusura value tof-nota-chiusura
                move como-data        to tof-data-chiusura
                move como-ora         to tof-ora-chiusura
                move user-codi        to tof-utente-chiusura
           end-evaluate.

           move como-data to tof-data-ultima-modifica  
           move como-ora  to tof-ora-ultima-modifica   
           move user-codi to tof-utente-ultima-modifica

           rewrite tof-rec invalid continue end-rewrite.
           set RicaricaGrid  to true
           unlock tordforn all record.

      ***---
       STORNA-ORDINATO.
           move tof-chiave to rof-chiave.
           move low-value  to rof-riga.

           start rordforn key >= rof-chiave
                 invalid  continue
             not invalid
                 perform until 1 = 2
                    read rordforn next no lock
                         at end exit perform
                    end-read
                    if tof-anno   not = rof-anno or
                       tof-numero not = rof-numero
                       exit perform
                    end-if

                    move ef-cau-buf      to link-causale
                    perform VALORIZZA-ARRAY-CAUSALI
                    set link-update      to true
                    move rof-prg-chiave  to link-key
                    move tof-mese-rif    to link-mese-rif
                    move tof-chiave      to link-chiave-origine

                    move user-codi to link-user of link-wprogmag
      *    storno l'ordinato
                    if rof-qta-ord > rof-qta-evasa
                       compute link-valore = rof-qta-ord - rof-qta-evasa
                       if link-valore not = 0
                          move "0000000000000000" to link-array
                          move -1                 to multiplyer(3)
                          call   "wprogmag"    using link-wprogmag
                          cancel "wprogmag"
                       end-if
                    end-if

                 end-perform
           end-start.

      ***---
       RIPRISTINA-ORDINATO.        
           move tof-chiave to rof-chiave.
           move low-value  to rof-riga.

           start rordforn key >= rof-chiave
                 invalid  continue
             not invalid 
                 perform until 1 = 2
                    read rordforn next no lock 
                       at end 
                          exit perform 
                    end-read
                    if tof-anno   not = rof-anno or
                       tof-numero not = rof-numero
                       exit perform
                    end-if

                    move ef-cau-buf      to link-causale
                    perform VALORIZZA-ARRAY-CAUSALI
                    set link-update      to true
                    move rof-prg-chiave  to link-key
                    move tof-mese-rif    to link-mese-rif
                    move tof-chiave      to link-chiave-origine

                    move user-codi to link-user of link-wprogmag
      *    ripristino l'ordinato
                    if rof-qta-ord > rof-qta-evasa
                       compute link-valore = rof-qta-ord - rof-qta-evasa
                       if link-valore not = 0
                          move "0010000000000000" to link-array
                          call   "wprogmag" using link-wprogmag
                          cancel "wprogmag"
                       end-if
                    end-if

                 end-perform
           end-start.

      ***---
       PULISCI-CAMPI-LABELS.
      *     initialize HiddenKey.
           move 0      to ef-art-buf  
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
                          lab-imb-buf
                          lbl-anno-ddt-BUF
                          lbl-num-ddt-BUF
                          lbl-dt-carico-BUF
                          lbl-ora-carico-BUF 
                          lbl-utente-carico-BUF

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
                   lbl-anno-ddt
                   lbl-num-ddt
                   lbl-dt-carico
                   lbl-ora-carico
                   lbl-utente-carico
                   chk-manuale.


           move 0 to old-art-codice old-prezzo.

           move 0 to hid-giacenza  hid-ordinato hid-impegnato 
                     prezzo-finale hid-rof-imf-codice 
                     hid-rof-qta-arrivata.
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
       BEFORE-FLDTOBUF-CODE.
           evaluate true
           when tof-inserito
                move "INSERITO"       to como-stato
      *****     when tof-accettato
      *****          move "ACCETTATO"      to como-stato
           when tof-inviato
                move "INVIATO"        to como-stato
           when tof-in-lavorazione
                move "IN LAVORAZIONE" to como-stato
           when tof-chiuso
                move "CHIUSO"         to como-stato
           end-evaluate.

           evaluate true
           when tof-manuale
                move "MANUALE"     to como-inserimento
           when tof-automatico
                if tof-programmazione-si
                   move "AUTOMATICA PROGRAMMAZIONE"  to como-inserimento
                else
                   move "AUTOMATICA"  to como-inserimento
                end-if
           end-evaluate

           evaluate true
           when tof-invio-man
                move "MANUALE"     to como-tipo-inv
           when tof-invio-fax
                move "FAX"         to como-tipo-inv
           when tof-invio-mail  
                move "E-MAIL"      to como-tipo-inv
           when other
                move space         to como-tipo-inv
           end-evaluate

           evaluate true
           when tof-chiusura-man
                move "MANUALE"     to como-tipo-chiusura
           when tof-chiusura-auto
                move "AUTOMATICA"  to como-tipo-chiusura
           when other
                move space         to como-tipo-chiusura
           end-evaluate

           if tof-inevaso
              move 0 to e-pb-elmovc
           else
              move 1 to e-pb-elmovc
           end-if.

      ***---
       LOAD-SOLLECITI-T.
           move tof-chiave  to sof-chiave-testa
           move 0           to sof-prog
      
           read sordforn no lock
                invalid initialize sof-dati
           end-read.

           move sof-note     to t-sof-note.
           move sof-data-arr to t-sof-data-arr.
           move sof-qta      to t-sof-qta.

      ***---
       SALVA-SOLLECITI-T.
           move tof-chiave      to sof-chiave-testa.
           move 0               to sof-prog

           move t-sof-note      to sof-note    

           move t-sof-data-arr  to sof-data-arr
           move t-sof-qta       to sof-qta     

           move tof-dati-comuni to sof-dati-comuni

           write sof-rec invalid rewrite sof-rec end-write.                             

      ***---
       PB-SOLLECITI-CODE.
           move tof-chiave      to gsordforn-sof-chiave-testa.
           move 0               to gsordforn-sof-prog
                                   gsordforn-articolo
                                   gsordforn-qta
                                   gsordforn-qta-eva.
           move mod             to gsordforn-mod.

           move t-sof-note      to gsordforn-sof-note.
           move t-sof-data-arr  to gsordforn-sof-data-arr.
      *     move tof-data-consegna to gsordforn-sof-data-arr
           move t-sof-qta       to gsordforn-sof-qta.

           call   "gsordforn" using gsordforn-linkage
                                    LK-BLOCKPGM
                                    USER-CODI
                                    LIVELLO-ABIL
           cancel "gsordforn".

           move 0 to forza-testata-solleciti.
           if gsordforn-forza-testata
              move 1 to forza-testata-solleciti
           end-if.

           if gsordforn-cambiati 
              move gsordforn-sof-note       to t-sof-note
              move gsordforn-sof-data-arr   to t-sof-data-arr
              move gsordforn-sof-qta        to t-sof-qta   
              set SollecitiCambiati   to true
              set SollecitiCambiatiT  to true
           end-if.

      ***---
       PB-SOL-R-CODE.
           inquire form1-gd-1(riga, 18) hidden-data hid-sof-rec-1
           inquire form1-gd-1(riga, 19) hidden-data hid-sof-rec-2
           inquire form1-gd-1(riga, 20) hidden-data hid-sof-rec-3
           inquire form1-gd-1(riga, 21) hidden-data hid-sof-rec-4
           inquire form1-gd-1(riga, 1)  cell-data gsordforn-sof-prog

           move tof-chiave            to gsordforn-sof-chiave-testa
      *     move hid-rof-num-riga      to gsordforn-sof-prog

           move hid-rof-cod-articolo  to gsordforn-articolo
           move hid-rof-qta-ord       to gsordforn-qta
           move hid-rof-qta-arrivata  to gsordforn-qta-eva
           move mod                   to gsordforn-mod

           move hid-sof-note          to gsordforn-sof-note    
           move hid-sof-data-arr      to gsordforn-sof-data-arr
           move hid-sof-qta           to gsordforn-sof-qta     

           call   "gsordforn" using gsordforn-linkage
                                    lk-blockpgm
                                    user-codi
                                    livello-abil
           cancel "gsordforn".

           if gsordforn-cambiati
              move gsordforn-sof-note       to hid-sof-note
              move gsordforn-sof-data-arr   to hid-sof-data-arr
              move gsordforn-sof-qta        to hid-sof-qta 

              modify form1-gd-1(riga, 18), hidden-data hid-sof-rec-1
              modify form1-gd-1(riga, 19), hidden-data hid-sof-rec-2
              modify form1-gd-1(riga, 20), hidden-data hid-sof-rec-3
              modify form1-gd-1(riga, 21), hidden-data hid-sof-rec-4
              set SollecitiCambiati   to true
              perform DATI-SOLLECITI
              modify lbl-dt-arrivo title lbl-dt-arrivo-buf
              modify lbl-qta-arr   title lbl-qta-arr-buf
           end-if. 

      ***---
       DATI-SOLLECITI.
           if hid-sof-data-arr = 0
              move space to data-arrivo
                            lbl-dt-arrivo-buf
           else
              move hid-sof-data-arr(7:2) to lbl-dt-arrivo-buf(1:2)
              move "/"                   to lbl-dt-arrivo-buf(3:1)
              move hid-sof-data-arr(5:2) to lbl-dt-arrivo-buf(4:2)
              move "/"                   to lbl-dt-arrivo-buf(6:1)
              move hid-sof-data-arr(1:4) to lbl-dt-arrivo-buf(7:4)
              move hid-sof-data-arr      to data-arrivo
              
           end-if

           move hid-sof-qta   to lbl-qta-arr-buf
                                 qta-arrivo.
