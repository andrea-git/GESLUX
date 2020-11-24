      ***---
       ABILITA-DISABILITA-CLIENTE-DESTINO.
           if mod = 0
              move 0 to mod-cliente-destino
              move 0 to e-pb-grid
              move 8 to BitmapNumGridNuovo
              move 7 to BitmapNumGridElimina
           else
              move 1 to mod-campi
              move 1 to mod-dati-bolla
              move 1 to NumBitmapDatiBolla
              display pb-dati
      *****        if SaveGdo = spaces move 1 to mod-cliente-destino
      *****        else                move 0 to mod-cliente-destino
      *****        end-if
              if NoAssortimento 
                 move 1 to mod-cliente-destino
                 move 0 to e-pb-grid
                 move 8 to BitmapNumGridNuovo
                 move 7 to BitmapNumGridElimina
              else              
                 move 0 to mod-cliente-destino
                 move 1 to e-pb-grid
                 move 5 to BitmapNumGridNuovo
                 move 4 to BitmapNumGridElimina
              end-if
           end-if.

           display pb-grid-elimina pb-grid-nuovo.

      ***---
       ABILITAZIONI.
           if mod = 1
              move BitmapDeleteEnabled to BitmapNumDelete
              move BitmapSaveEnabled   to BitmapNumSave
              move 1 to e-salva 
              if v-abil-bolla = 1
                 if abil-bolla = 1
                    move 1 to e-cancella
                 else
                    move BitmapDeleteDisabled to BitmapNumDelete
                    move 0 to e-cancella
                 end-if
              else
                 move 1 to e-cancella
              end-if
                                 
              move 0 to mod-campi
              move 1 to mod-dati-bolla
              move 0 to mod-dati-bolla
              move 2 to NumBitmapDatiBolla

              inquire cbo-stato,   value cbo-stato-buf

              if cbo-stato-buf not = "Disattivo" 
                 move 1 to mod-campi 
                 move 0 to mod-dati-bolla
                 move 2 to NumBitmapDatiBolla
              end-if  

           else         

              move BitmapDeleteDisabled to BitmapNumDelete
              move BitmapSaveDisabled   to BitmapNumSave
              move 0 to e-salva e-cancella         
              move 0 to mod-campi e-gui e-man
              move 1 to mod-dati-bolla
              move 1 to NumBitmapDatiBolla
           end-if.
           display pb-dati.
           perform ABILITA-DISABILITA-CLIENTE-DESTINO.
           move BitmapNewDisabled     to BitmapNumNew.
                                                                 
           modify tool-nuovo,    enabled       = 0
                                 bitmap-number = BitmapNumNew.
           modify tool-cancella, enabled       = e-cancella,
                                 bitmap-number = BitmapNumDelete.
           modify tool-salva,    enabled       = e-salva,
                                 bitmap-number = BitmapNumSave.

      ***---
       AGGIORNA-FILE-CHECK.
           open i-o check-rordini
           |Prima li cancello tutti così resetto partendo da 0
           move low-value to cror-rec
           start check-rordini key >= cror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read check-rordini next 
                         at end exit perform 
                    end-read
                    delete check-rordini record
                           invalid continue
                           |ATTENZIONE!!! 
                           |Se non riesce correttamente delete
                           |la stampa BOZZA MODIFICATA potrebbe 
                           |uscire errata. Nel caso venisse
                           |segnalato questo errore valutare
                           |l'ipotesi di gestire qui l'errore
                    end-delete
                 end-perform
           end-start.

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
                    move ror-prg-chiave  to cror-prg-chiave
                    move ror-des-imballo to cror-des-imballo
                    move ror-blister     to cror-blister
                    if ror-si-blister
                       move 0 to cror-qta-imballi
                    else
                       move ror-qta-imballi to cror-qta-imballi
                    end-if
                    read check-rordini no lock
                         invalid 
                         initialize cror-dati
                                    replacing numeric data by zeroes
                                         alphanumeric data by spaces
                         move ror-peso-utf     to cror-peso-utf
                         move ror-peso-non-utf to cror-peso-non-utf
                     not invalid
                         add 1 to cror-righe
                    end-read
                    add ror-num-colli to cror-num-colli
                    add ror-qta       to cror-qta
                    compute cror-importo =
                            cror-importo + (ror-qta * ror-prz-unitario)
                    write cror-rec invalid rewrite cror-rec end-write
                 end-perform
           end-start.
           
           close check-rordini.

      ***---
       CANCELLA.
           if PgmChiamante not = "del-evasioni"
              display message "Cancellare l'ordine corrente?"
                        title titolo
                        type mb-yes-no
                      default mb-no
                      giving scelta
                        icon 2
           else
              move mb-yes to scelta
           end-if.

           if scelta = mb-yes
              
              set DeleteXX to true
              perform ACCESSOXX

              set CancellazioneFisica   to true
              move LinkChiave to tor-chiave
PATCH         start transaction

              move "C" to tipoLogProgmag
              perform INI-LOG-PROGMAG

              perform DELETE-RIGHE
              if not SystemErrorOccurred
                 perform DELETE-ESITI
PATCH            commit transaction
PATCH            perform SCRIVI-FILE-BACKUP
                 delete tordini  record Invalid continue end-delete
                 if PgmChiamante not = "del-evasioni"
                    display message "Cancellazione avenuta con successo"
                              title titolo
                 end-if              
                 set RicaricaGrid   to true
                 set RigaCambiata   to false
                 set PrezzoCambiato to false
                 move 0 to mod 
                 move 0 to orig-data-bolla
                 move 0 to orig-num-bolla
      *    Luciano
                 evaluate true
                 when mail-modifica-GET
                      move tor-chiave         to mmb-tor-chiave
                      set mmb-cancellazione   to true
                      close tmp-mod-rordini
                      call   "GET-mail-mod-bozze" 
                             using mail-mod-bozze-linkage
                      cancel "GET-mail-mod-bozze"
                      delete file tmp-mod-rordini
                 when mail-modifica-SHI
                      move tor-chiave         to mmb-tor-chiave
                      set mmb-cancellazione   to true
                      close tmp-mod-rordini
                      call   "SHI-mail-mod-bozze" 
                             using mail-mod-bozze-linkage
                      cancel "SHI-mail-mod-bozze"
                      delete file tmp-mod-rordini
                 end-evaluate
      *    Luciano
                 initialize tor-rec 
                        old-tor-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
              end-if
              |SCRIVO IL LOG PER DOCUMENTARE LA CANCELLAZIONE FATTA DALL'UTENTE
              accept log-data from century-date
              accept log-ora  from time
              move "GORDCVAR"       to log-pgm
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
 
              perform varying idx-master from 1 by 1 
                        until idx-master > tot-master
                 move el-ordine-m(idx-master) to mto-chiave
                 perform AGGIORNA-STATO-MASTER
              end-perform
              move 0 to orig-data-bolla tor-data-bolla
              move 0 to orig-num-bolla  tor-num-bolla

              perform FINE-LOG-PROGMAG
                
              |ATTIVANDO QUESTO FLAG RICHIAMO IL RICALCOLO 
              |DELLE QTA PRENOTATE.
              |LO FACCIO ALLA FINE DIRETTAMENTE NEL PROGRAMMA
              |CHIAMANTE UNA VOLTA SOLA
              if PgmChiamante not = "del-evasioni"
                 set HoSalvato to true
              end-if
              perform FORM1-EXIT
              perform DESTROYXX
           end-if.

      ***---
       INI-LOG-PROGMAG.
           if mag-codice = "LBX" exit paragraph end-if.
           move 0 to idx.
           initialize progmag-tab replacing numeric data by zeroes
                                       alphanumeric data by spaces.
           move tor-chiave to ror-chiave.
           move low-value  to ror-num-riga.

           start rordini key is >= ror-chiave
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read rordini next no lock at 
                         end exit perform 
                    end-read
                    if tor-anno   not = ror-anno or
                       tor-numero not = ror-num-ordine
                       exit perform
                    end-if
                    add 1 to idx
                    move ror-prg-chiave to el-prg-chiave(idx)
                 end-perform
           end-start.
           accept como-data from century-date.
           accept como-ora  from time.
           accept  path-log-progmag from environment "PROGMAG_LOG_PATH"
           inspect path-log-progmag 
                   replacing trailing spaces by low-value.
           inspect user-codi
                   replacing trailing spaces by low-value.
           string  path-log-progmag delimited low-value
                   tor-anno         delimited size
                   "-"              delimited size
                   tor-numero       delimited size
                   "_"              delimited size
                   como-data        delimited size
                   "_"              delimited size
                   como-ora         delimited size
                   "_"              delimited size
                   user-codi        delimited low-value
                   "_"              delimited size
                   tipoLogProgmag   delimited size
                   ".log"           delimited size
              into path-log-progmag
           end-string.
           open output log-progmag.

           inspect user-codi
                   replacing trailing low-value by spaces.

           move "** SITUAZIONE PRIMA **" to riga-log-progmag.
           write riga-log-progmag.
           perform SCRIVI-PROGMAG-LOG.

      ***---
       SCRIVI-PROGMAG-LOG.
           perform varying idx from 1 by 1 
                     until idx > 999
              if el-prg-cod-articolo(idx) = 0
                 exit perform
              end-if
              move el-prg-cod-articolo(idx) to prg-cod-articolo
              move spaces                   to prg-cod-magazzino
              move spaces                   to prg-tipo-imballo
              move 0                        to prg-peso
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read progmag next at end exit perform end-read
                       if prg-cod-articolo not = 
                          el-prg-cod-articolo(idx)
                          write riga-log-progmag from spaces
                          exit perform
                       end-if
                       move prg-cod-articolo  to rl-prg-cod-articolo
                       move prg-cod-magazzino to rl-prg-cod-magazzino
                       move prg-tipo-imballo  to rl-prg-tipo-imballo
                       move prg-peso          to rl-prg-peso     
                       move prg-impegnato     to rl-prg-impegnato
                       move prg-imp-master    to rl-prg-imp-master
                       move prg-imp-GDO       to rl-prg-imp-GDO
                       move prg-imp-TRAD      to rl-prg-imp-TRAD
                       write riga-log-progmag from rl-progmag
                    end-perform
              end-start
           end-perform.               

           close log-progmag.

      ***---
       FINE-LOG-PROGMAG.  
           if mag-codice = "LBX" exit paragraph end-if. 
           open extend log-progmag.
           move "** SITUAZIONE DOPO **" to riga-log-progmag.
           write riga-log-progmag. 
           perform SCRIVI-PROGMAG-LOG.

      ***---
       CANCELLA-FLAG.
PATCH      move low-value  to ror-chiave
PATCH      move tor-anno   to ror-anno
PATCH      move tor-numero to ror-num-ordine
PATCH      start rordini key >= ror-chiave
PATCH            invalid continue
PATCH        not invalid
PATCH            perform until 1 = 2
PATCH               read rordini next no lock 
PATCH                    at end exit perform 
PATCH               end-read
PATCH               if ror-anno       not = tor-anno or
PATCH                  ror-num-ordine not = tor-numero
PATCH                  exit perform
PATCH               end-if
PATCH               if ror-cancellato
PATCH                  delete rordini record
PATCH                         invalid continue
PATCH                  end-delete
PATCH               end-if
PATCH            end-perform
PATCH      end-start.

      ***---
       CHANGE-STATUS.
           evaluate nome-file
           when "clienti"
                set disattivo to true
                perform CARICA-COMBO-STATO
                |specifico per i pgm. aventi Tab-Control
                move 1 to Screen1-Ta-1-TAB-VALUE
                perform SCREEN1-TA-1-TABCHANGE
                |******
                move 1 to mod
                move 0 to mod-campi
                move 1 to mod-dati-bolla
                move 1 to NumBitmapDatiBolla
                display pb-dati

                perform DISPLAY-SCREEN
           end-evaluate.
           perform ABILITA-DISABILITA-CLIENTE-DESTINO.

      ***---
       CLEAR-SCREEN.
      *     move cli-codice to old-cli-codice.
      *    

           initialize tor-rec  
                      cli-rec
                      des-rec
                      ror-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
                        
           perform FORM1-FLD-TO-BUF. 
      
           perform INIT-OLD-REC.

           move spaces to lab-vet-buf 
                          lab-iva-buf 
                          lab-pag-buf 
                          lab-age-buf.
      
           perform DISPLAY-SCREEN.
      *
      *     perform RESET-GRIGLIA 
           .

      ***---
       COLORE.
           modify form1-gd-1, start-y = riga, y = riga,
                              start-x = 2,    x = 78-NumColMan,
                              region-color = 144.

      ***---
       CONTROLLA-CAUSALE.
           move ef-cau-buf to tor-causale tca-codice.
           read tcaumag no lock invalid continue end-read.
           if tca-no-stampa
              move tor-chiave to link-insdati-chiave
              if tor-anno-bolla = 0
                 move tge-anno to tor-anno-bolla
              end-if
              move tor-anno-bolla to link-insdati-anno
              move 0 to link-data-bolla link-num-bolla
              move lab-data-bolla-buf to como-data
              move lab-num-bolla-buf  to tor-num-bolla link-num-bolla
              if como-data = 0 or tor-num-bolla = 0
                 perform DATE-TO-FILE
                 move como-data   to link-data-bolla
                 set obbligatorio to true
                 move StoreMagazzino to link-mag
      *****           call   "insdati" using insdati-linkage
      *****           cancel "insdati"
      *****           if link-status not = 0
      *****              set errori to true
      *****              display message "Salvataggio NON effettuato: "
      *****                       x"0d0a""Inserimento dati bolla "
      *****                      "obbligatori per la causale inserita!"
      *****                      title = tit-err
      *****                      icon 2
      *****           else
                    move link-data-bolla   to como-data 
                                              orig-data-bolla
                    perform DATE-TO-SCREEN
                    move como-data         to lab-data-bolla-buf
                    if tor-anno-bolla = 0
                       move tge-anno       to tor-anno-bolla
                    end-if
                    move link-num-bolla    to lab-num-bolla-buf 
                                              orig-num-bolla
                    move "BOLLA PRENOTATA" to lab-bolla-pren-buf
                    set  tor-bolla-si-prenotata to true
                    display lab-data-bolla
                            lab-num-bolla
                            lab-bolla-pren
      *****           end-if
              end-if
           end-if.
LUBEXX     move tca-contropartita to tor-contropartita.

      ***---
       CURRENT-RECORD.
      *     perform RIEMPI-CHIAVE.
           set tutto-ok  to true.
           set ReadSecca to true.
           if mod = 1
              read tordini lock invalid 
                   set errori to true 
              end-read
           else       
              unlock tordini all records
              read tordini no lock invalid 
                   set errori to true 
              end-read
           end-if.
           set ReadSecca to false.

           if tor-num-bolla  not = 0 and
              tor-data-bolla not = 0
              set bollettata to true
           else
              set bollettata to false
           end-if.

           if tor-num-fattura  not = 0 and
              tor-data-fattura not = 0
              move 1 to v-fatt
           else
              move 0 to v-fatt
           end-if.

           if v-bolla = 0
              if bollettata and v-fatt = 0
                 move 0 to abil-bolla
                 move 1 to v-abil-bolla
                 modify chk-abil-bolla, value abil-bolla, 
                                        bitmap-number = 1,
                                        visible v-abil-bolla
              end-if
           end-if.

           if tutto-ok
LUBEXX*****   Mi salvo lo stato dell'ordine in quanto in
LUBEXX*****   fase di moficica non può essere variato
LUBEXX        move tor-stato to save-stato
              
              move tor-data-bolla to orig-data-bolla
              move tor-num-bolla  to orig-num-bolla

              perform AGGIORNA-LAB-INVIATO
                                       
              if LinkPgm(1:5) = "stdoc"
                 if not TastiGiaAbilitati
                    if tor-data-bolla not = 0
                       move 1 to e-stbolla
                       if LinkPgm = "stdocB"
                          move 777          to control-id
                          move 4            to accept-control
                       end-if
                    else
                       move 0 to e-stbolla
                    end-if
                    if tor-data-fattura not = 0
                       move 1 to e-stfatt
                       if LinkPgm = "stdocF"
                          move 888          to control-id
                          move 4            to accept-control
                       end-if
                    else
                       move 0 to e-stfatt
                    end-if
                    if LinkPgm = "stdocF" or "stdocB"
                       move 1 to e-stbozza
                    end-if
                    if tor-num-bolla not = 0
                       move 0 to v-stbozza
                       move 0 to e-stbozza
                    end-if
                    set TastiGiaAbilitati to true
                 end-if
              end-if

              if LinkPgm = "selordc"
                 if tor-data-bolla not = 0
                    move 1 to v-stbolla
                 else
                    move 0 to v-stbolla
                 end-if
              end-if
                    
              if tor-causale = tge-causale-corrisp 
                 set VenditaAlDettaglio to true
              else
                 set VenditaAlDettaglio to false
              end-if

              move tor-causale     to tca-codice
              move spaces to tca-descrizione
              move 0 to v-bolla
              read tcaumag invalid continue end-read
              move tca-descrizione to lab-cau-buf
              if tca-no-stampa move 1 to v-bolla
              else             move 0 to v-bolla
              end-if

              if tca-no-movim-giac and
                 tca-no-movim-imp  and
                 tca-no-movim-ord  and
                 tca-no-giac-bloc
                 set CallWProgmag to false
              else
                 set CallWProgmag to true
              end-if
              
              if tca-si-zero set TotaleSiZero to true
              else           set TotaleNoZero to true
              end-if

              move tor-cod-cli     to cli-codice ef-cli-buf 
           save-cli-codice
              move spaces to cli-ragsoc-1 cli-indirizzo cli-localita
              move spaces to des-ragsoc-1 des-indirizzo des-localita
              set cli-tipo-C to true
              read clienti no lock invalid continue end-read
      *****        move cli-gdo to SaveGDO                                            
              move cli-tipo to tcl-codice
              read ttipocli no lock
                   invalid  
                   move spaces to TrattamentoInUso SaveGDO
               not invalid  
                   move tcl-tipologia-tratt-imposte to TrattamentoInuso
                   evaluate true
                   when tcl-gdo-no  move spaces  to SaveGDO
                   when other       move cli-gdo to SaveGDO
                   end-evaluate
              end-read

              perform ABILITA-GEST-PLUS

              if tor-prg-destino not = 0
                 move tor-cod-cli     to des-codice
                 move tor-prg-destino to des-prog ef-des-buf
                 read destini  no lock 
                      invalid  move spaces to des-deposito-utf
                 end-read
              else
                 initialize des-rec
                 move cli-utf to des-deposito-utf
              end-if
              move des-deposito-utf to flag-deposito-utf

              if tor-cod-agente    not = 0
                 move tor-cod-agente to age-codice
                 read agenti   no lock invalid continue end-read
              else
                 initialize age-rec
              end-if

              if tor-cod-pagamento not = spaces
                 move "PA"              to tblpa-codice1
                 move tor-cod-pagamento to tblpa-codice2
                 read tcodpag  no lock invalid continue end-read
              else
                 initialize record-tblpa
              end-if

              perform MOVE-DESCR-PAG

              if tor-cod-ese-iva   not = spaces
                 move "IV"            to tbliv-codice1
                 move tor-cod-ese-iva to tbliv-codice2
                 read tivaese  no lock invalid continue end-read
              else   
                 initialize record-tbliv
              end-if
              perform MOVE-DESCR-IVA

              if tor-vettore       not = 0
                 move tor-vettore to vet-codice
                 read tvettori no lock invalid continue end-read
              else
                 initialize vet-rec
              end-if          

              move riga to save-riga
              perform DISPLAY-SCREEN-MANUALE

              move "00" to status-tordini
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
                                  
              move tca-cod-magaz to mag-codice
              read tmagaz no lock
                   invalid set tor-da-inviare-no to true
               not invalid 
                   if mag-da-inviare-si
                      set tor-da-inviare-si to true
                   else                            
                      set tor-da-inviare-no to true
                   end-if
              end-read
      *****        move spaces to shi-codice
      *****        read paramSHI no lock
      *****        if tca-cod-magaz = shi-mag-shi-1 or
      *****           tca-cod-magaz = shi-mag-shi-2
      *****           set tor-da-inviare-si to true
      *****        else                            
      *****           set tor-da-inviare-no to true
      *****        end-if 

      *****        if tca-cod-magaz = "SHI"
      *****           if tor-da-inviare-si
      *****              set mail-modifica-SHI to false
      *****           else
      *****              if shi-mail-mod-bozze    = space and 
      *****                 shi-mail-cc-mod-bozze = space
      *****                 set mail-modifica-SHI to false
      *****              else
      *****                 set mail-modifica-SHI to true
      *****              end-if
      *****           end-if
      *****        else
      *****           set mail-modifica-SHI to false
      *****        end-if
      *****        if tca-cod-magaz = "GET"
      *****           if tor-da-inviare-si
      *****              set mail-modifica-GET to false
      *****           else
      *****              if get-mail-mod-bozze    = space and 
      *****                 get-mail-cc-mod-bozze = space
      *****                 set mail-modifica-GET to false
      *****              else
      *****                 set mail-modifica-GET to true
      *****              end-if
      *****           end-if
      *****        else
      *****           set mail-modifica-GET to false
      *****        end-if
      *****        if mail-modifica-SHI and mod = 1
      *****           perform OPEN-TMP-MOD
      *****        end-if
      *****        if mail-modifica-GET and mod = 1
      *****           perform OPEN-TMP-MOD
      *****        end-if
      ******    Luciano

           else
              move 0 to mod
              move 0 to mod-k
              if vecchio
                 perform CLEAR-SCREEN
                 set errori to true
                 if YesMessage    
                    move 5000 to control-id |TAB-CONTROL
                    move    4 to accept-control
                    display message box MSG-Record-inesistente
                            title = tit-err
                            icon mb-warning-icon
                 end-if
              end-if
           end-if.
           
           if RecLocked
              set RecLocked to false
              set errori    to true
           end-if.

           move tor-cod-pagamento to SavePag.
           move tor-cod-ese-iva   to SaveIva.
           move tor-vettore       to SaveVet.

      ***---
       DELETE-RIGHE.
           move 0 to DeletedRows 
           move tor-chiave to ror-chiave.
           move low-value  to ror-num-riga.

           start rordini key is >= ror-chiave
                 invalid continue
             not invalid 
                 perform until 1 = 2
                    read rordini next no lock at 
                         end exit perform 
                    end-read
                    if tor-anno   not = ror-anno or
                       tor-numero not = ror-num-ordine
                       exit perform
                    end-if
PATCH               if status-rordini not = "00"
PATCH                  move 5 to tipo-messaggio
PATCH                  perform INVIO-MAIL
PATCH                  exit perform
PATCH               end-if
                    if CancellazioneFisica
                       delete rordini record
                              invalid
PATCH                         move 4 to tipo-messaggio
PATCH                         perform INVIO-MAIL
PATCH                         exit perform
                          not invalid
                              add 1 to DeletedRows
                       end-delete
                    else
                       set ror-cancellato to true
                       rewrite ror-rec
                               invalid
PATCH                          move 4 to tipo-messaggio
PATCH                          perform INVIO-MAIL
PATCH                          exit perform
                       end-rewrite
                    end-if
                    if tor-da-ordine-si
      *****                 if CancellazioneFisica
                          set cancella to true
                          perform PROGMAG-MASTER
      *****                 end-if
                       perform QTA-EVASIONE-ORDINE
                    else
                       initialize link-wprogmag
                       perform VALORIZZA-ARRAY-CAUSALI
                       set link-update      to true
                       move ror-prg-chiave  to link-key
                       move ef-cau-buf      to link-causale
                       move ror-qta         to link-valore
                       |lo devo lasciare qui perchè l'unico
                       |caso in cui assegno -1 a questa causale
                       if tca-si-stampa
                          move "0000000000000000" to link-array
                          |-1 perchè diminuendola di default
                          |gli cambio il segno e aumenta
                          move -1              to multiplyer(2)
                          if tor-anno-bolla not = 0
                             |L'impegnato non va proprio considerato
                             move  0            to multiplyer(2)
                             move -1            to multiplyer(1)
                             move -1            to multiplyer(15)
                          end-if
                       else
                          move "0000000000000000" to link-array
                          move -1              to multiplyer(1)
                          move -1              to multiplyer(15)
                       end-if
                       move user-codi to link-user of link-wprogmag
                       call   "wprogmag" using link-wprogmag
                       cancel "wprogmag"
                    end-if
                 end-perform
           end-start.

      ***---
       DISPLAY-SCREEN.
           display form1.
           if e-stbolla = 0
              modify pb-stbolla, bitmap-number = 3
           end-if.
           |per i solleciti
           if e-stbozza = 1
              modify pb-stbozza, bitmap-number = 10
           end-if.
           if e-stfatt  = 0 modify pb-stfatt,  bitmap-number = 6 end-if.

      ***---
       DISPLAY-SCREEN-MANUALE.
           modify form1-gd-1, mass-update = 1.
           modify form1-gd-1, reset-grid  = 1.
           perform INTESTAZIONE.
           perform CARICA-GRID-MANUALE.
           modify form1-gd-1, mass-update = 0.

      ***---
       CARICA-GRID-MANUALE.
           accept como-data from century-date.
           accept como-ora  from time.

           move zero   to num-colli

           initialize tab-iva.
           if sw-check-rordini = 0
              accept como-data from century-date
              accept como-ora  from time
              |Capita che non riesca ad apriore il file tmp
              |allora riprovo finchè va a b.f. cambiando il nome
              perform until 1 = 2
                 initialize path-check-rordini
                 accept  path-check-rordini from environment "PATH_ST"
                 inspect path-check-rordini replacing trailing 
                                            spaces by low-value
                 string  path-check-rordini delimited low-value
                         "CHECK-RORDINI_"   delimited size
                         como-data          delimited size
                         "_"                delimited size
                         como-ora           delimited size
                         ".tmp"             delimited size
                         into path-check-rordini
                 end-string
                 move path-check-rordini to link-path
                 open output check-rordini
                 if status-check-rordini = "00"
                    exit perform
                 end-if
              end-perform
              close       check-rordini
              open i-o    check-rordini

              perform until 1 = 2
                 initialize path-zoom-tor-master
                 accept  path-zoom-tor-master from environment "PATH_ST"
                 inspect path-zoom-tor-master replacing trailing 
                                               spaces by low-value
                 string  path-zoom-tor-master delimited low-value
                         "zoom-tor-master"    delimited size
                         como-data            delimited size
                         "_"                  delimited size
                         como-ora             delimited size
                         ".tmp"               delimited size
                         into path-zoom-tor-master
                 end-string
                 open output zoom-tor-master
                 if status-zoom-tor-master = "00"
                    exit perform
                 end-if
              end-perform                   
              close       zoom-tor-master
              open i-o    zoom-tor-master
           end-if.
                 
           move 0 to righe-iniziali.
           move 0 to como-imposta SavePrezzo como-tot-ivato.
           move 0 to LastPrg.
           move 1 to store-riga.
           set tutto-ok    to true.
           move tor-chiave to ror-chiave.
           move low-value  to ror-num-riga.
           start rordini key is >= ror-chiave invalid continue end-start
           perform until 1 = 2
              read rordini next no lock at end exit perform end-read
              if ror-anno       not = tor-anno or
                 ror-num-ordine not = tor-numero
                 read rordini previous
                 move ror-cod-articolo to old-art-codice
                 exit perform
              end-if
              set FirstTime to false

              add 1 to righe-iniziali
              add 1 to LastPrg
              add 1 to store-riga

              move ror-num-riga     to col-num
              move ror-cod-articolo to col-art art-codice
              read articoli no lock invalid continue end-read

              initialize col-des
              inspect art-descrizione replacing trailing spaces 
                                      by low-value
              inspect ror-des-imballo replacing trailing spaces 
                                      by low-value
              move  ror-qta-imballi to imballi-ed
              call "C$JUSTIFY" using imballi-ed, "L"
BLISTR        if ror-si-blister
BLISTR           string  art-descrizione delimited by low-value
BLISTR                   " - "           delimited by size
BLISTR                   ror-des-imballo delimited by low-value
BLISTR                   " ("            delimited size
BLISTR                   imballi-ed      delimited spaces
BLISTR                   ")"             delimited size
BLISTR                   into col-des
BLISTR           end-string
              else
                 string  art-descrizione delimited by low-value
                         " - "           delimited by size
                         ror-des-imballo delimited by low-value
                         " da "          delimited by size
                         imballi-ed      delimited by spaces
                         " x "           delimited by size
                         art-udm-imballo delimited by size
                         into col-des
                 end-string                          
              end-if
              inspect ror-des-imballo replacing trailing low-value
                                      by spaces                
              inspect art-descrizione replacing trailing low-value
                                      by spaces

              if SiAssortimento |and SaveGDO not = spaces
                 perform ASSORCLI-IN-LINE
              end-if

              move ror-qta          to col-qta
              move ror-prz-unitario to col-uni

              initialize prg-chiave
              move ror-cod-articolo to prg-cod-articolo
              read progmag no lock invalid continue end-read
              move prg-ordinato-1 to save-ordinato

              move ror-prg-chiave to prg-chiave
              read progmag no lock
                   invalid
                   initialize prg-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces
              end-read

              perform FIND-PROGMAG

              |RILETTURA DI SICUREZZA: LA CHIAVE E' SICURAMENTE
              |NELLA RIGA CHE STO LEGGENDO
              move ror-prg-chiave to prg-chiave
              read progmag no lock 
                   invalid
                   initialize prg-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces
              end-read

              move ror-des-imballo   to hid-des-imballo
              move ror-qta-imballi   to hid-imballi

              move ror-perce-sconto  to col-sconto
              move ror-imp-consumo   to col-cons
              move ror-imp-cou-cobat to col-cou
              move ror-add-piombo    to col-add
              move ror-imponib-merce to col-imp
              move ror-cod-iva       to col-iva
              move ror-cod-art-cli   to hid-cod-art-cli
              move ror-omaggio       to hid-omaggio
              move prg-peso-utf      to hid-utf
              move prg-peso-non-utf  to hid-non-utf

              modify form1-gd-1(store-riga, 1),  cell-data = col-num
              modify form1-gd-1(store-riga, 2),  cell-data = col-art
              modify form1-gd-1(store-riga, 3),  cell-data = col-des
              modify form1-gd-1(store-riga, 4),  cell-data = col-qta
              modify form1-gd-1(store-riga, 5),  cell-data = col-uni
              modify form1-gd-1(store-riga, 6),  cell-data = col-sconto
              modify form1-gd-1(store-riga, 7),  cell-data = col-cons
              modify form1-gd-1(store-riga, 8),  cell-data = col-cou
              modify form1-gd-1(store-riga, 9),  cell-data = col-add
              modify form1-gd-1(store-riga, 10), cell-data = col-imp
              modify form1-gd-1(store-riga, 11), cell-data = col-iva 

              if OrdineAnno not = 0 or OrdineNumero not = 0
                 if LinkOrdine = ror-chiave-ordine-testa
                    modify form1-gd-1(store-riga), row-color 449
                 end-if
              end-if

              if ror-si-omaggio
                 modify form1-gd-1(store-riga, 78-NumColMan),  
                        bitmap = conferma-bmp
                        bitmap-number = 1
                        bitmap-width  = 19
              else
                 modify form1-gd-1(store-riga, 78-NumColMan),
                        bitmap = conferma-bmp
                        bitmap-number = 2
                        bitmap-width  = 19
              end-if

              if SaveGDO = spaces
                 move art-prezzo-vendita    to hid-prezzo
              else
                 if SiAssortimento
                    move asc-prezzo-finito  to hid-prezzo
                 end-if
              end-if

              perform READ-TMARCHE

              move ror-qta        to hid-old-qta

OMAGGI        move ror-qta-omaggi to hid-qta-omaggi
BLISTR        move ror-blister    to hid-blister
BLISTR        move ror-bli-codice to hid-bli-codice
                                     cli-codice
              move zero to hid-bli-qta
                           hid-bli-perce


              if ror-si-blister
                 perform VAL-HID-BLISTER
              end-if

LABLAB        move ror-promo      to hid-promo
LABLAB        if ror-bloccato
LABLAB           set hid-bloccato to true
LABLAB        else
LABLAB           set hid-bloccato to false
LABLAB        end-if
LABLAB        move ror-prz-commle to hid-prz-commle

              move ror-prg-chiave to HiddenKey

              move "IV"        to tbliv-codice1
              move ror-cod-iva to tbliv-codice2 hid-cod-iva
              read tivaese invalid continue end-read
              move tbliv-percentuale to hid-perce-iva


      *    Luciano
      *    se il flag è a spazi vuol dire che non era inizializzato e lo
      *    inizializzo qui
              if ror-prz-manuale = space
                 move zero   to ror-prz-manuale
              end-if
              move ror-prz-manuale to hid-prz-manuale
      *    Luciano fine


      *****        modify form1-gd-1(store-riga, 78-NumColMan + 19),
      *****               hidden-data = hid-perce-iva

              modify form1-gd-1(store-riga, 1), 
                     hidden-data gruppo-hidden

              modify form1-gd-1(store-riga, 2), 
                     hidden-data ror-chiave-ordine

      *    Luciano
              move ror-cod-articolo      to hid-art-orig
              move ror-qta               to hid-qta-orig
              move ror-prg-tipo-imballo  to hid-imb-orig
              move store-riga            to hid-riga
              modify form1-gd-1(store-riga, 3), 
                     hidden-data hidden-modifiche
      *    Luciano fine

              compute SavePrezzo = ror-imponib-merce +
                                   ror-imp-cou-cobat +
                                   ror-imp-consumo   +
                                   ror-add-piombo
OMAGGI        compute SavePrezzo = 
OMAGGI                SavePrezzo * ( ror-qta - ror-qta-omaggi )

              perform varying idx from 1 by 1
                        until idx > 3
                 if el-perce-iva(idx) = hid-perce-iva or
                    el-imponib(idx)   = 0
                    exit perform
                 end-if
              end-perform

              move    hid-perce-iva to el-perce-iva(idx)
              compute el-imponib(idx) = el-imponib(idx) + SavePrezzo
              
              if sw-check-rordini = 0
                 move ror-prg-chiave  to cror-prg-chiave
                 move ror-des-imballo to cror-des-imballo
                 move ror-blister     to cror-blister
                 if ror-si-blister
                    move 0 to cror-qta-imballi
                 else
                    move ror-qta-imballi to cror-qta-imballi
                 end-if
                 read check-rordini no lock
                      invalid 
                      initialize cror-dati
                                 replacing numeric data by zeroes
                                      alphanumeric data by spaces
                      move ror-peso-utf     to cror-peso-utf
                      move ror-peso-non-utf to cror-peso-non-utf
                  not invalid
                      add 1 to cror-righe
                 end-read
                 add ror-num-colli to cror-num-colli
                 add ror-qta       to cror-qta
                 compute cror-importo =
                         cror-importo + ( ror-qta * ror-prz-unitario )
                 write cror-rec invalid rewrite cror-rec end-write

                 if tor-da-ordine-si
                    move ror-chiave-ordine-testa to mto-chiave
                    read mtordini no lock
                         invalid
                         display message 
                                 "ORDINE MASTER"
                          x"0d0a""ANNO " mto-anno,
                          x"0d0a""NUMERO " mto-numero
                        x"0d0a""NON TROVATO!!! CONTATTARE ASSISTENZA!!!"
                                 title tit-err
                                  icon 2
                     not invalid
                          move mto-chiave to ztm-chiave

                          move mto-data-ordine(7:2) to ztm-data(1:2)
                          move "/"                  to ztm-data(3:1)
                          move mto-data-ordine(5:2) to ztm-data(4:2)
                          move "/"                  to ztm-data(6:1)
                          move mto-data-ordine(1:4) to ztm-data(7:4)

                          move mto-num-ord-cli to ztm-vettore
                          evaluate true
                          when mto-registrato     
                               move "REGISTRATO"        to ztm-esito
                          when mto-in-lavorazione 
                               move "IN LAVORAZIONE"    to ztm-esito
                          when mto-sped-parz      
                               move "SPEDITO PARZIALE"  to ztm-esito
                          when mto-sped-tot       
                               move "SPEDITO TOTALE"    to ztm-esito
      *****                    when mto-fatt-parz      move "FATTURATO PARZIALE" to ztm-esito
      *****                    when mto-fatt-tot       move "FATTURATO TOTALE"   to ztm-esito
                          when mto-chiuso         
                               move "CHIUSO"            to ztm-esito
                          end-evaluate
                          write ztm-rec invalid continue end-write
                    end-read
                 end-if
              end-if

              move store-riga   to riga
                                                 
              perform ROW-TO-ENTRY               
              |Ripetuto ma da lasciare qua perchè sostituisco
              |la qta recuperata dall'imballo con quella della 
              |riga che può essere forzata
              move ror-qta-imballi to imq-qta-imb
              perform CALC-COLLI-RIGA
              if hid-blister = 1 and hid-bli-codice not = zero
                 move hid-imballi  to num-colli-riga
              end-if
              add num-colli-riga   to num-colli

           end-perform. 

           move num-colli to lab-colli-buf
           modify lab-colli title lab-colli-buf.


           move 0 to como-tot-ivato.
           perform varying idx from 1 by 1 
                     until idx > 3

              if el-imponib(idx) = 0
                 exit perform
              end-if

              compute como-iva = 
                    ( el-imponib(idx) * 
                      el-perce-iva(idx) ) / 100
              
              add 0,005     to como-iva
              move como-iva to como-iva-2dec
              
              compute como-tot-ivato = 
                      como-tot-ivato +
                      como-iva-2dec  +
                      el-imponib(idx)
           end-perform.

           move como-tot-ivato to lab-tot-ivato-buf.

           if v-dett = 1
              display lab-tot-ivato
           end-if.

           if v-fatt = 1
              display lab-tot-fatt lab-totale-fatt
           end-if.
           
           if sw-check-rordini = 0
              close check-rordini
              close zoom-tor-master
              move 1 to sw-check-rordini

              |Riposiziono clienti e destini
              set cli-tipo-C to true
              move tor-cod-cli to cli-codice
              read clienti no lock

              if tor-prg-destino not = 0
                 move cli-codice      to des-codice
                 move tor-prg-destino to des-prog
                 read destini no lock
              end-if
           end-if.

      ***---
       ENTRY-TO-ROW.
           inquire form1-gd-1, last-row in tot-righe.
           inquire ef-art, value in art-codice.

           if NewRow
              add 1 to tot-righe    giving riga
              move art-codice to old-art-codice
              modify form1-gd-1, insert-rows 1
              add 1        to LastPrg
              move LastPrg to col-num
              perform COLORE
              move 0 to hid-old-qta
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

BLISTR     if hid-blister not = old-hid-blister
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

           if col-sconto not = old-col-sconto 
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if col-cons   not = old-col-cons
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

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
              if not NewRow
                 set PrezzoCambiato to true
              end-if
           end-if.

           if link-imballo-saved = 1
              set RigaCambiata to true
              move lab-art-buf to col-des
              move link-qta    to hid-imballi
              move ef-qta-buf  to col-qta
              move link-des    to hid-des-imballo
              move 0           to link-imballo-saved
           end-if.

           move tbliv-codice2     to hid-cod-iva.

           move tbliv-percentuale to hid-perce-iva.
           move ef-qta-oma-buf    to hid-qta-omaggi.
BLISTR     move chk-blister-buf   to hid-blister.

           modify form1-gd-1(riga, 1),  cell-data = col-num.
           modify form1-gd-1(riga, 2),  cell-data = col-art.
           modify form1-gd-1(riga, 3),  cell-data = col-des.
           modify form1-gd-1(riga, 4),  cell-data = col-qta.
           modify form1-gd-1(riga, 5),  cell-data = col-uni.
           modify form1-gd-1(riga, 6),  cell-data = col-sconto.
           modify form1-gd-1(riga, 7),  cell-data = col-cons.
           modify form1-gd-1(riga, 8),  cell-data = col-cou.
           modify form1-gd-1(riga, 9),  cell-data = col-add.
           modify form1-gd-1(riga, 10), cell-data = col-imp.
           modify form1-gd-1(riga, 11), cell-data = col-iva.

LABLAB     |Ho cambiato il prezzo confermato in precedenza dall'ufficio comm.le
LABLAB     if hid-prz-commle not = 0
LABLAB        if hid-prz-commle not = ror-prz-unitario
LABLAB           set hid-bloccato to true
LABLAB        else
LABLAB           set hid-bloccato to false
LABLAB        end-if
LABLAB     end-if.


      *    Luciano
           if hid-no-prz-manuale
              if newRow
                 move ef-uni-buf      to mro-prz-unitario
                 move ef-sconto-buf   to mro-perce-sconto
                 if mro-prz-unitario  not = como-prezzo-proposto or
                    mro-perce-sconto  not = como-sconto-proposto
                    set hid-si-prz-manuale  to true
                 end-if
              else
                 if PrezzoCambiato
                    set hid-si-prz-manuale  to true
                 end-if
              end-if
           end-if
      *    Luciano fine


      *****     modify form1-gd-1(riga, 78-NumColMan + 1),  hidden-data = 
      *****     hid-cod-articolo.
      *****     modify form1-gd-1(riga, 78-NumColMan + 2),  hidden-data = 
      *****     hid-cod-magazzino.
      *****     modify form1-gd-1(riga, 78-NumColMan + 3),  hidden-data = 
      *****     hid-tipo-imballo.
      *****     modify form1-gd-1(riga, 78-NumColMan + 4),  hidden-data = 
      *****     hid-peso.
      *****     modify form1-gd-1(riga, 78-NumColMan + 5),  hidden-data = 
      *****     hid-imballi.
      *****     modify form1-gd-1(riga, 78-NumColMan + 6),  hidden-data = 
      *****     hid-listino.
      *****     modify form1-gd-1(riga, 78-NumColMan + 7),  hidden-data = 
      *****     hid-des-imballo.
      *****     modify form1-gd-1(riga, 78-NumColMan + 8),  hidden-data = 
      *****     hid-cod-art-cli.
      *****     modify form1-gd-1(riga, 78-NumColMan + 9),  hidden-data = 
      *****     hid-var-piu.
      *****     modify form1-gd-1(riga, 78-NumColMan + 10), hidden-data = 
      *****     hid-var-meno.
      *****     modify form1-gd-1(riga, 78-NumColMan + 11), hidden-data = 
      *****     hid-prezzo.
      *****     modify form1-gd-1(riga, 78-NumColMan + 12), hidden-data = 
      *****     hid-utf.
      *****     modify form1-gd-1(riga, 78-NumColMan + 13), hidden-data = 
      *****     hid-non-utf.                   
      *****     modify form1-gd-1(riga, 78-NumColMan + 16), hidden-data = 
      *****     hid-giacenza.
      *****     modify form1-gd-1(riga, 78-NumColMan + 17), hidden-data = 
      *****     hid-impegnato.
      *****     modify form1-gd-1(riga, 78-NumColMan + 18), hidden-data = 
      *****     hid-ordinato.
      *****     modify form1-gd-1(riga, 78-NumColMan + 19), hidden-data = 
      *****     hid-perce-iva.
      *****     modify form1-gd-1(riga, 78-NumColMan + 20), hidden-data = 
      *****     hid-qta-omaggi.
BLISTR*****     modify form1-gd-1(riga, 78-NumColMan + 21), hidden-data = 
      *****     hid-blister.   
LABLAB*****     modify form1-gd-1(riga, 78-NumColMan + 22), hidden-data = 
      *****     hid-flag-bloccato.
LABLAB*****     modify form1-gd-1(riga, 78-NumColMan + 23), hidden-data = 
      *****     hid-prz-commle.
LABLAB*****     modify form1-gd-1(riga, 78-NumColMan + 24), hidden-data = 
      *****     hid-promo.

           modify form1-gd-1(riga, 1), hidden-data gruppo-hidden.
           modify form1-gd-1(riga, 2), hidden-data ror-chiave-ordine.
               

      ******    Luciano
      *****     if mail-modifica-SHI or mail-modifica-GET
      *****        perform MOD-MAIL
      *****     end-if.
      ******    Luciano fine


      *****     if NewRow
      *****        modify form1-gd-1(riga, 78-NumColMan + 15), 
      *****               hidden-data = hid-old-qta
      *****     else
              if old-cod-articolo   not = hid-cod-articolo  or
                 old-cod-magazzino  not = hid-cod-magazzino or
                 old-tipo-imballo   not = hid-tipo-imballo  or
                 old-peso           not = hid-peso
                 set YesDeleted to true
              end-if
      *****     end-if.

           move col-uni to SavePrezzo.
           if SavePrezzo = 0
LUBEXX        if tca-si-speciale
LUBEXX           modify form1-gd-1(riga, 78-NumColMan),
LUBEXX           bitmap        = conferma-bmp
LUBEXX           bitmap-number = 2
LUBEXX           bitmap-width  = 19
LUBEXX        else
LUBEXX           modify form1-gd-1(riga, 78-NumColMan),
LUBEXX           bitmap        = conferma-bmp
LUBEXX           bitmap-number = 1
LUBEXX           bitmap-width  = 19
LUBEXX        end-if
           else
              modify form1-gd-1(riga, 78-NumColMan),
              bitmap        = conferma-bmp
              bitmap-number = 2
              bitmap-width  = 19
           end-if.

           move 78-ID-ef-art to control-id.
           move 4            to accept-control.

           modify form1-gd-1, cursor-y = riga.
           set    NewRow      to true.


           perform AGGIORNA-NUM-COLLI.

      ***---
       INIT.
           set TastiGiaAbilitati to false.
           set NonCambiareTab    to false.
      *****     set ArticoloSetFocus  to false.
           set ControllaCampi    to true. 
           set FirstTime         to true.
           set CheckAfterZoom    to false.
           move 0 to v-guidata old-art-codice.
           move 1 to event-data-1 screen1-ta-1-tab-value 
           form1-radio-1-buf.
           perform SCREEN1-TA-1-TABCHANGE.
           move 0 to StatusHelp LastPrg e-man e-gui.
           initialize link-imballo-saved.

           move como-anno to ef-anno-buf.
           display ef-anno.

           set RigaCambiata   to false.
           set PrezzoCambiato to false.

           move data-oggi to como-data.
           perform DATE-TO-SCREEN.
           move como-data to ef-data-buf ef-data-pass-buf.
           display ef-data ef-data-pass.

           move spaces to lab-art-buf lab-iva-2-buf.
                                
           move 0 to LastPrg.

           move 78-ID-ef-cli to control-id.
           move 4 to accept-control.
      *
           |Riempio il tabellino contenente i riferimenti agli ID
           |relativi ad ogni pagina del tab-control
           |specifico per i pgm. aventi Tab-Control
           move 78-ID-ef-num-ord       to min-id(1).
           move 78-ID-rb-gui           to max-id(1).
           |*******
      *
           Perform RIEMPI-COMBO-STATO.
      *
           move "Attivo" to cbo-stato-buf.
           Modify  cbo-stato,   value cbo-stato-buf.

           move 0 to mod.

           move 0 to mod-k.
           move 0 to como-tot-ivato.

      *    Luciano
           move 1000   to como-riga.
      *    Luciano fine

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
                       tor-numero                delimited size
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
                       tor-chiave                 delimited size
                       " - RIGA: "                delimited size
                       ror-chiave                 delimited size
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
                       tor-chiave                 delimited size
                       " - RIGA: "                delimited size
                       ror-chiave                 delimited size
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
                          tor-chiave                   delimited size
                          " - RIGA: "                  delimited size
                          ror-chiave                   delimited size
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
                          tor-chiave               delimited size
                          " - RIGA: "              delimited size
                          ror-chiave               delimited size
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
                       "(IF STATUS-RORDINI NOT = 00...)" delimited size
                       " - TESTATA: "                    delimited size
                       tor-chiave                        delimited size
                       " - RIGA: "                       delimited size
                       ror-chiave                        delimited size
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
                       "(IF STATUS-RORDINI NOT = 00...)" delimited size
                       " - TESTATA: "                    delimited size
                       tor-chiave                        delimited size
                       " - RIGA: "                       delimited size
                       ror-chiave                        delimited size
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
           end-evaluate.
           accept prova-mail from environment "PROVA_MAIL".
           if MailInProva
              move "a.eventi@goodworks.it" to LinkAddress
           else
              initialize LinkAddress
              string "a.eventi@goodworks.it" delimited size
                     into LinkAddress
              end-string
           end-if.
           move "gordcvar" to NomeProgramma.
           perform 10 times
              perform SEND-MAIL
              open input lineseq1
              read lineseq1 next
              if line-riga of lineseq1 = "True"
                 close lineseq1
                 exit perform
              end-if
              close lineseq1
           end-perform.
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
           call "C$COPY" using wstampa, wk-path, "S"
                        giving copy-status.
           delete file lineseq.

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
       LEGGI-ANNO.
           move spaces to tge-codice.
           move 0 to tge-anno.
           read tparamge no lock invalid continue end-read.
           move tge-anno to ef-anno-buf.
           move tge-cod-iva-omag to iva-omaggio tbliv-codice2.
           move "IV"             to tbliv-codice1.

           read tivaese invalid continue end-read.

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
                 if tor-bloccato
                    display message "ATTENZIONE!!!"
                             x"0d0a""ORDINE BLOCCATO!!!"
                             x"0d0a""PASSARE COMUNQUE ALLA MODIFICA?"
                              title titolo
                               icon 2
                               type mb-yes-no
                            default mb-no
                             giving scelta
                 else
                    move mb-yes to scelta
                 end-if
                 if scelta = mb-yes 
                    move 1 to mod
                 else               
                    move 0 to mod 
                              mod-campi 
                              mod-cliente-destino
                    move 1 to mod-dati-bolla
                    move 1 to NumBitmapDatiBolla
                    display pb-dati
                    read tordini no lock
                 end-if
                 set StatusModifica to true
                 perform STATUS-BAR-MSG      
                 move 78-ID-ef-cau to control-id
      *****           set ArticoloSetFocus to false
                 set ControllaCampi   to true
              end-if                         
           else

              move 1 to mod
              perform SALV-MOD
              move 0 to mod
              if errori
                 move 1 to mod
              else
      *    Luciano
                 if mail-modifica-SHI or mail-modifica-GET
                    close       tmp-mod-rordini
                    delete file tmp-mod-rordini
                 end-if
      *    Luciano fine

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
              move 3 to NumBitmapDatiBolla
              if control-id = 3000 |PB-COLLEG
                 move 2 to NumBitmapDocColl
              else                               
                 move 1 to NumBitmapDocColl
              end-if
              move 0 to mod-dati-bolla
           else                               
              if control-id = 2000 |PB-DATI
                 move 2 to NumBitmapDatiBolla
              else                               
                 move 1 to NumBitmapDatiBolla
              end-if
              move 1 to mod-dati-bolla
              move 1 to NumBitmapDocColl
           end-if.
                                
           perform DISPLAY-SCREEN.
           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.

           move 4 to accept-control.
           move 0 to StatusHelp.
           perform STATUS-HELP. 

      ***---
       QTA-EVASIONE-ORDINE.
           if ror-anno-master   of rordini not = 0 and
              ror-numero-master of rordini not = 0
              set trovato-master to false
              set idx-master to 1
              search el-ordine-m
              when el-ordine-m(idx-master) = 
                   ror-chiave-ordine-testa of rordini
                   set trovato-master to true
              end-search
              if not trovato-master
                 add 1 to tot-master
                 move ror-chiave-ordine-testa of rordini
                   to el-ordine-m(tot-master)
              end-if
           end-if.

      ***---
       PROGMAG-MASTER.
           if ror-anno-master   of rordini not = 0 and
              ror-numero-master of rordini not = 0 and
              ror-progr-master  of rordini not = 0
              move ror-anno-master   of rordini
                to mro-anno
              move ror-numero-master of rordini
                to mro-numero
              move ror-progr-master  of rordini
                to mro-progr
              read mrordini no lock key mro-k-progr
                   invalid
                   display message
                   "GRAVE ERRORE"
            x"0d0a""RIGA O" ror-chiave of rordini
            x"0d0a""RIGA M" mro-anno, "-", mro-numero, "-", mro-progr
            x"0d0a""NON TROVATA!!!"
            x"0d0a""CONTATTARE ASSISTENZA!!!"
                             title tit-err
                              icon 2
               not invalid
                   perform READ-MRORDINI-LOCK
                   set link-update-um     to true
                   set link-update-peso   to false
                   set link-update-valore to false
                   set link-update        to true
                   move mro-prg-chiave    to link-key
                   move ef-cau-buf        to link-causale

                   move mro-chiave-testa to mto-chiave
                   read mtordini no lock
                        invalid display message
                              "GRAVE ERRORE"
                       x"0d0a""TESTA" mto-chiave
                       x"0d0a""NON TROVATA!!!"
                       x"0d0a""CONTATTARE ASSISTENZA!!!"
                                          title tit-err
                                           icon 2
                        exit paragraph
                   end-read

                   if cancella

                      if mro-prg-chiave not = ror-prg-chiave
      *****                   if not ( mto-chiuso     or 
      *****                            mto-chiuso-man or 
      *****                            mro-chiuso )
                            add 1 to ra-idx
                            move mro-prg-cod-articolo 
                              to ra-articolo(ra-idx)
      *****                   end-if
                      end-if

                      |SE E' BOLLETTATA TUTTA LA GIACENZA VA STORNATA
                      |EVENTUALMENTE SARA' DIMINUITA DOPO DELLA NUOVA
                      if tor-anno-bolla not = 0
                         move 0              to link-impegnato
                         move ror-qta        to link-valore
                         move ror-prg-chiave to link-key
                         move "0000000000000000"   to link-array
                         |Devo fare il contrario di quanto dice la
                         |causale perciò per aumentare metto il -1
                         move -1                to multiplyer(1)
                         move -1                to multiplyer(15)
                         move user-codi to link-user of link-wprogmag
                         call   "wprogmag" using link-wprogmag
                         cancel "wprogmag"
                      else

                         if mto-chiuso or mto-chiuso-man or mro-chiuso
                           |30/10/2020 utilizzo sempre il magazzino dell'evasione
                            move mag-codice         to link-magazzino
                            move 0                  to link-impegnato
                            move ror-qta            to link-valore
                            move "0000000000000000" to link-array
                            move -1                 to multiplyer(2)
                            move user-codi to link-user of link-wprogmag
                            call   "wprogmag" using link-wprogmag
                            cancel "wprogmag"
                          |10/11/2020 In questo caso non devo eseguire nessun ricalcolo
      *                      set master-chiuso to true
                         else

                            if mro-qta > mro-qta-e
                               move mro-qta   to link-valore
                            else
                               move mro-qta-e to link-valore
                            end-if
                            move "0000000000000000" to link-array
                            move -1                 to multiplyer(2)
                            move user-codi to link-user of link-wprogmag
                            call   "wprogmag" using link-wprogmag
                            cancel "wprogmag"
      
                            compute eva-new = mro-qta-e - ror-qta
                            if mro-qta > eva-new
                               move mro-qta  to link-valore
                            else
                               move eva-new  to link-valore
                            end-if
                            move "0000000000000000"     to link-array
                            move 1                      to multiplyer(2)
                            move user-codi to link-user of link-wprogmag
                            call   "wprogmag" using link-wprogmag
                            cancel "wprogmag"
      
      *****                      if mro-qta > mro-qta-e
      *****                         move mro-qta   to link-valore-meno
      *****                      else
      *****                         move mro-qta-e to link-valore-meno
      *****                      end-if
      *****
      *****                      compute eva-new = mro-qta-e - ror-qta
      *****                      if mro-qta > eva-new
      *****                         move mro-qta  to link-valore-piu
      *****                      else
      *****                         move eva-new  to link-valore-piu
      *****                      end-if
      *****
      *****                      compute link-valore = 
      *****                              link-valore-piu - link-valore-meno
      *****                      move "0000000000000000"     to link-array
      *****                      move 1                      to multiplyer(2)
      *****                      move user-codi to link-user of link-wprogmag
      *****                      call   "wprogmag" using link-wprogmag
      *****                      cancel "wprogmag"

                            perform DIREZIONA-IMPEGNATO
                            if mro-qta > mro-qta-e
                               compute link-impegnato = mro-qta - 
                                                        mro-qta-e
                               if tor-anno-bolla = 0
                                  move 0              to link-valore
                               else
                                  move link-impegnato to link-valore
                               end-if
                               move "0000000000000000" to link-array
                               move -1                 to multiplyer(2)
                               move user-codi 
                                       to link-user of link-wprogmag
                               call   "wprogmag" using link-wprogmag
                               cancel "wprogmag"
                            end-if
                         
                            compute eva-new = mro-qta-e - ror-qta
                            if mro-qta > eva-new
                               compute link-impegnato = 
                                       mro-qta - eva-new
                               if tor-anno-bolla = 0
                                  move 0              to link-valore
                               else
                                  move link-impegnato to link-valore
                               end-if
                               move "0000000000000000" to link-array
                               move 1                  to multiplyer(2)
                               move user-codi          to link-user 
                                 of link-wprogmag
                               call   "wprogmag"    using link-wprogmag
                               cancel "wprogmag"
                            end-if
                         end-if
                      end-if

                      if CancellazioneLogica
                         |SE NON DIMINUISCO SUBITO POI NON MI RITROVO CON LO STORNO
                         if tor-anno-bolla not = 0
                            subtract ror-qta from mro-qta-b
                         end-if
                         subtract ror-qta from mro-qta-e
                         rewrite mro-rec invalid continue end-rewrite
                      end-if

                   else
                      if tor-anno-bolla not = 0
                         move 0                      to link-impegnato
                         move ror-qta                to link-valore       
                         move ror-prg-chiave         to link-key
                         move "0000000000000000"     to link-array
                         move 1                      to multiplyer(1)
                         move 1                      to multiplyer(15)
                         move user-codi to link-user of link-wprogmag
                         call   "wprogmag" using link-wprogmag
                         cancel "wprogmag"
                      else
                         if mto-chiuso or mto-chiuso-man
                            move 0                  to link-impegnato
                            move ror-qta            to link-valore
                            move "0000000000000000" to link-array
                            move 1                  to multiplyer(2)
                            move user-codi to link-user of link-wprogmag
                            call   "wprogmag" using link-wprogmag
                            cancel "wprogmag"
                         else
                            if mro-qta > mro-qta-e
                               move mro-qta   to link-valore
                            else
                               move mro-qta-e to link-valore
                            end-if
                            move "0000000000000000"     to link-array
                            move -1                     to multiplyer(2)
                            move user-codi to link-user of link-wprogmag
                            call   "wprogmag" using link-wprogmag
                            cancel "wprogmag"
                         
                            compute eva-new = mro-qta-e + ror-qta
                            if mro-qta > eva-new
                               move mro-qta  to link-valore
                            else
                               move eva-new  to link-valore
                            end-if
                            move "0000000000000000" to link-array
                            move 1                  to multiplyer(2)
                            move user-codi to link-user of link-wprogmag
                            call   "wprogmag" using link-wprogmag
                            cancel "wprogmag"
      
      *****                      if mro-qta > mro-qta-e
      *****                         move mro-qta   to link-valore-meno
      *****                      else
      *****                         move mro-qta-e to link-valore-meno
      *****                      end-if
      *****                   
      *****                      compute eva-new = mro-qta-e + ror-qta
      *****                      if mro-qta > eva-new
      *****                         move mro-qta  to link-valore-piu
      *****                      else
      *****                         move eva-new  to link-valore-piu
      *****                      end-if
      *****
      *****                      compute link-valore = 
      *****                              link-valore-piu - link-valore-meno
      *****
      *****                      move "0000000000000000" to link-array
      *****                      move 1                  to multiplyer(2)
      *****                      move user-codi to link-user of link-wprogmag
      *****                      call   "wprogmag" using link-wprogmag
      *****                      cancel "wprogmag"
                            
                            perform DIREZIONA-IMPEGNATO
                            if mro-qta > mro-qta-e
                               compute link-impegnato = 
                                       mro-qta - mro-qta-e
                               if tor-anno-bolla = 0
                                  move 0              to link-valore
                               else
                                  move link-impegnato to link-valore
                               end-if
                               move "0000000000000000" to link-array
                               move -1                 to multiplyer(2)
                               move user-codi to link-user 
                                 of link-wprogmag
                               call   "wprogmag" using link-wprogmag
                               cancel "wprogmag"
                            end-if
                              
                            compute eva-new = mro-qta-e - ror-qta
                            if mro-qta > eva-new
                               compute link-impegnato = 
                                       mro-qta - eva-new
                               if tor-anno-bolla = 0
                                  move 0               to link-valore
                               else
                                  move link-impegnato  to link-valore
                               end-if
                               move "0000000000000000" to link-array
                               move 1                  to multiplyer(2)
                               move user-codi to link-user 
                                 of link-wprogmag
                               call   "wprogmag" using link-wprogmag
                               cancel "wprogmag"
                            end-if
                         end-if
                      end-if
                   
                      |SE NON DIMINUISCO SUBITO POI NON MI RITROVO CON LO STORNO
                      if tor-anno-bolla not = 0
                         add ror-qta to mro-qta-b
                      end-if
                      add ror-qta to mro-qta-e
                      rewrite mro-rec invalid continue end-rewrite
                   end-if

              end-read
           end-if.

      ***---
       READ-MRORDINI-LOCK.
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "mrordini" to geslock-nome-file.
      
           set tutto-ok to true.
           read mrordini lock key mro-k-progr invalid continue end-read
      
           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              set RecLocked to false
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 0 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   read mrordini lock key mro-k-progr 
                        invalid continue 
                   end-read
              end-evaluate
           end-perform.

      ***---
       READ-PROGMAG-LOCK.
           set RecLocked to false.
           set trovato to true.
           read progmag lock invalid set trovato to false end-read.
           if RecLocked
              subtract 1 from riga giving idx
              move idx to riga-ed
              move prg-cod-articolo to codice-ed
              move prg-peso         to peso-ed
              display message box 
                   "Impossibile procedere con la scrittura su file dei "
                   x"0d0a"
                   "Progressivi di magazzino [PROG] relativo alla riga "
                    riga-ed, ". "
              x"0d0a""====== KeyFile ======"
              x"0d0a""Articolo:       ", codice-ed
              x"0d0a""Magazzino:  ", prg-cod-magazzino
              x"0d0a""Imballo:       ", prg-tipo-imballo
              x"0d0a""Peso:           ", peso-ed 
              x"0d0a""================="
              x"0d0a""Record già in uso su altro terminale. Ritentare la
      -    " connessione?"
                      type mb-yes-no
                      giving scelta
                      title = tit-err
                      icon mb-error-icon
              if scelta = mb-yes perform READ-PROGMAG-LOCK end-if
           end-if.

      ***---
       RECUPERA-RECORD.

      ***---
       ROW-TO-ENTRY.
      *    Questo prf. porta i dati da grid a entry-field
      *    quando rec-grd e rec-not sono valorizzati
         
           inquire form1-gd-1(riga, 1),  cell-data in col-num.
           inquire form1-gd-1(riga, 2),  cell-data in col-art.
           inquire form1-gd-1(riga, 3),  cell-data in col-des.
           inquire form1-gd-1(riga, 4),  cell-data in col-qta.
           inquire form1-gd-1(riga, 5),  cell-data in col-uni.
           inquire form1-gd-1(riga, 6),  cell-data in col-sconto.
           inquire form1-gd-1(riga, 7),  cell-data in col-cons.
           inquire form1-gd-1(riga, 8),  cell-data in col-cou. 
           inquire form1-gd-1(riga, 9),  cell-data in col-add.
           inquire form1-gd-1(riga, 10), cell-data in col-imp.
           inquire form1-gd-1(riga, 11), cell-data in col-iva.

           inquire form1-gd-1(riga, 1), hidden-data gruppo-hidden.
           inquire form1-gd-1(riga, 2), hidden-data ror-chiave-ordine.

      *    Luciano
           inquire form1-gd-1(riga, 3), hidden-data hidden-modifiche.
           move hid-imballi  to hid-imballi-old
      *    Luciano fine


      *****     inquire form1-gd-1(riga, 78-NumColMan + 1),  hidden-data in 
      *****             hid-cod-articolo.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 2),  hidden-data in 
      *****             hid-cod-magazzino.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 3),  hidden-data in 
      *****             hid-tipo-imballo.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 4),  hidden-data in 
      *****             hid-peso.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 5),  hidden-data in 
      *****             hid-imballi.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 6),  hidden-data in 
      *****             hid-listino.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 7),  hidden-data in 
      *****             hid-des-imballo.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 8),  hidden-data in 
      *****             hid-cod-art-cli.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 9),  hidden-data in 
      *****             hid-var-piu.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 10), hidden-data in 
      *****             hid-var-meno.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 11), hidden-data in 
      *****             hid-prezzo.  
      *****     inquire form1-gd-1(riga, 78-NumColMan + 12), hidden-data in 
      *****             hid-utf.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 13), hidden-data in 
      *****             hid-non-utf. 
      *****     inquire form1-gd-1(riga, 78-NumColMan + 14), hidden-data in 
      *****             hid-omaggio.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 15), hidden-data in 
      *****             hid-old-qta.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 16), hidden-data in 
      *****             hid-giacenza.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 17), hidden-data in 
      *****             hid-impegnato.
      *****     inquire form1-gd-1(riga, 78-NumColMan + 18), hidden-data in 
      *****             hid-ordinato. 
      *****     inquire form1-gd-1(riga, 78-NumColMan + 20), hidden-data in 
      *****             hid-qta-omaggi.
BLISTR*****     inquire form1-gd-1(riga, 78-NumColMan + 21), hidden-data in 
      *****             hid-blister.   
LABLAB*****     inquire form1-gd-1(riga, 78-NumColMan + 22), hidden-data in 
      *****             hid-flag-bloccato.
LABLAB*****     inquire form1-gd-1(riga, 78-NumColMan + 23), hidden-data in 
      *****             hid-prz-commle.
LABLAB*****     inquire form1-gd-1(riga, 78-NumColMan + 24), hidden-data in 
      *****             hid-promo.

LUBEXX     move col-art  to art-codice.
LUBEXX     read articoli no lock invalid continue end-read.
LUBEXX*****     move art-imballo-standard to imq-codice.
LUBEXX     move hid-tipo-imballo to imq-codice.
LUBEXX     read timbalqta no lock invalid continue end-read.

           move col-art    to old-col-art    
                              ef-art-buf  
                              old-art-codice art-codice.
           set lab-imp-coubat to true.
           read articoli invalid continue end-read.
           if art-si-cobat
              set lab-imp-cobat to true
           else                        
              set lab-imp-cou   to true
           end-if.
           move col-des    to old-col-des    lab-art-buf.
           move col-qta    to old-col-qta    ef-qta-buf ror-qta.
OMAGGI     move hid-qta-omaggi to ef-qta-oma-buf ror-qta-omaggi.
BLISTR     move hid-blister to chk-blister-buf ror-blister 
           old-hid-blister.

LABLAB     if hid-bloccato set ror-bloccato to true
LABLAB     else            set ror-attivo   to true
LABLAB     end-if

LABLAB     move hid-prz-commle to ror-prz-commle.
LABLAB     move hid-promo      to ror-promo.
           move hid-bli-codice to ror-bli-codice.

           move col-uni    to old-col-uni    ef-uni-buf old-prezzo
                              ror-prz-unitario.
           move col-sconto to old-col-sconto ef-sconto-buf 
                              ror-perce-sconto.
           move col-cons   to old-col-cons   ef-cons-buf
                              ror-imp-consumo.
           move col-cou    to old-col-cou    ef-cou-buf
                              ror-imp-cou-cobat.
           move col-add    to old-col-add    ef-add-buf
                              ror-add-piombo.
           move col-imp    to old-col-imp    ef-imp-buf
                              ror-imponib-merce.
           move col-iva    to ef-cod-iva-buf old-col-iva
                              tbliv-codice2  ror-cod-iva.
           move spaces     to tbliv-descrizione1 tbliv-descrizione2.
           move "IV"       to tbliv-codice1.
           read tivaese no lock invalid continue end-read.
           perform MOVE-DESCR-IVA-2.
           
           move hid-giacenza  to prg-giacenza.
           move hid-impegnato to prg-impegnato.
           move hid-ordinato  to save-ordinato.
           perform LABEL-VALORI.
           perform DISPLAY-SCREEN.
           set    NewRow      to false.
           move HiddenKey     to old-prg-chiave prg-chiave.


           perform CALC-COLLI-RIGA.
           move num-colli-riga  to num-colli-riga-old.

      ***---
       SALVA.
           if mod = 0 exit paragraph end-if.
           
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
                      until CONTROL-ID > 78-ID-ef-data-cons
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
              if tutto-ok
                 perform CONTROLLA-CAUSALE
              end-if
           end-if.

           if tutto-ok
              perform SUPERAMENTO-500-UTF
           end-if.

           if tutto-ok
              perform CONTROLLA-SE-PESO-SUP-24000-KG
           end-if.

           if tutto-ok
              perform CONTROLLA-QTA-BLISTER
           end-if.

           if tutto-ok
              if tutto-ok
                 perform CONTA-ZERI-MAN
                 if not trovato
                    display message "Impossibile registrare l'ordine in"
                                    " quanto non sono presenti righe."
                            title = tit-err
                            icon mb-warning-icon
                    set errori to true
                    perform CANCELLA-COLORE
                 else
                    move 78-ID-cbo-stato to CONTROL-ID
                    perform CONTROLLO
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

              if tor-cod-cli not = old-tor-cod-cli
                 if tutto-ok 
                    if tcl-si-gestione-fido or cli-gestione-fido-si
                       perform CONTROLLA-FUORI-FIDO
                    end-if
                 end-if
              end-if
      *****           if tutto-ok and cli-gestione-fido-si
      *****              initialize calfido-linkage 
      *****                          sitfin-linkage
      *****                         replacing numeric data by zeroes
      *****                              alphanumeric data by spaces
      *****              move ef-cli-buf   to link-cli-codice
      *****              call "C$JUSTIFY"  using link-cli-codice, "R"
      *****              inspect link-cli-codice 
      *****                      replacing leading x"20" by x"30"
      *****              move Sum to sitfin-importo
      *****              call   "sitfin"  using sitfin-linkage
      *****                                    calfido-linkage
      *****              cancel "sitfin"
      *****              display message "Confermi l'ordine?"
      *****                         title titolo
      *****                          type mb-yes-no
      *****                          icon 2
      *****                        giving scelta
      *****              if scelta = mb-no
      *****                 set errori to true
      *****              end-if
      *****           end-if

              if errori
                 perform CANCELLA-COLORE
              else
                 perform CONTA-CODICI-IVA-MAN
                 if errori
                    display message
                    "Impossibile registrare il documento in quanto"
                    " sono presenti più di tre codici IVA"
                              title tit-err
                               icon 2
                    set errori to true
                    perform CANCELLA-COLORE
                 else
                    perform FORM1-BUF-TO-FLD
                    inquire form1-gd-1, cursor-y in riga
                    move riga           to event-data-2
                    perform SPOSTAMENTO

PATCH               start transaction
                       
                    if tor-da-ordine-si
                       move tor-cod-cli     to como-prm-cliente
                       move tor-prg-destino to como-prm-destino
                       perform TROVA-PARAMETRO
                    else
                       set prm-imb-minimo-no to true
                    end-if

                    if RigaCambiata or PrezzoCambiato
              
                       move "M" to tipoLogProgmag
                       perform INI-LOG-PROGMAG

                       perform SCRIVI-RIGHE-MAN
PATCH**                Ne ho ritrovato meno di quelle che ho in grid
PATCH                  if save-tot-righe not = righe-finali
PATCH                     move 1 to tipo-messaggio
PATCH                     perform INVIO-MAIL
PATCH                  end-if
                       
LUBEXX                 if not EsisteIVA
LUBEXX                    set tor-invio-manuale to true
LUBEXX                 end-if

                       set tor-ordine to true
                    end-if
   
                    if not SystemErrorOccurred
                       perform CANCELLA-COLORE

LABLAB                 if si-promo 
                          set tor-si-promo to true
LABLAB                 else        
                          set tor-no-promo to true
LABLAB                 end-if

                       move LinkChiave to tor-chiave

LUBEXX*****            Risetto lo stato dell'ordine cone quello iniziale
LUBEXX                 move save-stato to tor-stato

PATCH                  if tor-numero = 0
PATCH                     display message "EVASIONE ZERO"
PATCH                              x"0d0a""RICARICARE L'ORDINE"
PATCH                               title tit-err
PATCH                                icon 3
PATCH                  end-if

                       perform RECUPERA-ULTIMO-ESITO
PATCH                  commit transaction
                              
                       if tor-num-bolla  not = 0 or
                          tor-data-bolla not = 0
                          set tor-da-inviare-no to true
                       end-if

                       rewrite tor-rec invalid continue end-rewrite
 
                       perform varying idx-master from 1 by 1 
                                 until idx-master > tot-master
                          move el-ordine-m(idx-master) to mto-chiave
                          perform AGGIORNA-STATO-MASTER
                       end-perform

                       unlock tordini all records
                       move 0 to mod       
                                                    
                       if RigaCambiata or PrezzoCambiato
                          perform FINE-LOG-PROGMAG
                       end-if
                       
                       move LinkChiave to tor-chiave
                       perform CURRENT-RECORD

                       if PrezzoCambiato
                          call   "st-ordine" using tor-chiave, 
                                                   link-path, "I"
                          cancel "st-ordine"
                       end-if

                       if RigaCambiata
                          call   "st-ordine" using tor-chiave, 
                                                   link-path, "M"
                          cancel "st-ordine"
      *****                    call   "st-ordine-PDF" using tor-chiave, 
      *****                                                 link-path, "M"
      *****                    cancel "st-ordine-PDF"
                       end-if
              
                       if tor-causale = tge-causale-omag   
                          initialize geslock-linkage 
                                     replacing numeric data by zeroes
                                          alphanumeric data by spaces
                          move 1 to geslock-v-salva
                          set geslock-omom to true
                          move 
                          "RICORDARSI D'INSERIRE DETTAGLI PROMOZIONE!!!"
                            to geslock-messaggio
                          perform 5 times
                             call   "geslock" using geslock-linkage
                             cancel "geslock"
                          end-perform
                       end-if

LUBEXX                 if sw-check-rordini = 1
LUBEXX                    perform AGGIORNA-FILE-CHECK
LUBEXX                 end-if

PATCH                  |E' già arrivata la mail che il file non
PATCH                  |è stato scritto correttamente ed evito
PATCH                  |di danneggiare anche il file di backup
PATCH                  if tipo-messaggio = 0
PATCH                     perform SCRIVI-FILE-BACKUP
PATCH                  end-if

                       if VenditaAlDettaglio
                          display message box "Effettuare la stampa"
                                              " del buono di carico?"
                                    title titolo
                                     type mb-yes-no
                                   giving scelta
                          if scelta = mb-yes
                             move tor-chiave to buono-chiave
                             call   "stbuono" using stbuono-linkage
                             cancel "stbuono"
                          end-if
                       end-if
                       move 0 to v-dett
                       set vecchio to true
      ******                Luciano
      *****                 evaluate true
      *****                 when mail-modifica-GET
      *****                      close tmp-mod-rordini
      *****                      move tor-chiave   to mmb-tor-chiave
      *****                      set mmb-modifica  to true
      *****                      move path-tmp-mod-rordini  to mmb-path
      *****                      call   "GET-mail-mod-bozze" 
      *****                                     using mail-mod-bozze-linkage
      *****                      cancel "GET-mail-mod-bozze"
      *****                      delete file tmp-mod-rordini
      *****                 when mail-modifica-SHI
      *****                      close tmp-mod-rordini
      *****                      move tor-chiave   to mmb-tor-chiave
      *****                      set mmb-modifica  to true
      *****                      move path-tmp-mod-rordini  to mmb-path
      *****                      call   "SHI-mail-mod-bozze" 
      *****                                     using mail-mod-bozze-linkage
      *****                      cancel "SHI-mail-mod-bozze"
      *****                      delete file tmp-mod-rordini
      *****                 end-evaluate

      *                Luciano fine
LUBEXX                 perform VALORIZZA-OLD
                       perform TORNA-IN-VISUA
                       set RicaricaGrid to true
                    end-if
                    set RigaCambiata   to false
                    set PrezzoCambiato to false
                 end-if
              end-if
           end-if.

           if not SystemErrorOccurred

              perform DISPLAY-SCREEN

              set environment "KEYSTROKE" to "DATA=44   44"
              set environment "KEYSTROKE" to "DATA=46   46"

              if mod = 1
PATCH            move BitmapSaveEnabled to BitmapNumSave
PATCH            move 1 to e-salva
PATCH            modify tool-salva,
PATCH                   enabled = e-salva,
PATCH             bitmap-number = BitmapNumSave
              else
                 move 1 to mod-dati-bolla
                 move 1 to NumBitmapDatiBolla
                 display pb-dati
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

           if SiSalvato and mod = 1
              
              if tor-data-bolla not = orig-data-bolla or
                 tor-num-bolla  not = orig-num-bolla
                 set NoSalvato to true
              end-if

           end-if.
                     
           if NoSalvato
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
      *****     move tor-chiave to btor-chiave.
      *****     delete btordini record invalid continue end-delete.
      *****     move low-value  to bror-rec.
      *****     move tor-chiave to bror-chiave.
      *****     start brordini key >= bror-chiave
      *****           invalid continue |nel caso in cui sia nuovo
      *****       not invalid
      *****           perform until 1 = 2
      *****              read brordini next at end exit perform end-read
      *****              if bror-anno       not = btor-anno or
      *****                 bror-num-ordine not = btor-numero
      *****                 exit perform
      *****              end-if
      *****              delete brordini record invalid continue end-delete
      *****           end-perform
      *****     end-start.
      *****
      *****     read tordini no lock 
      *****          invalid continue |se annullata l'evasione
      *****      not invalid
      *****          move tor-rec to btor-rec
      *****          write btor-rec invalid continue end-write
      *****          move low-value  to ror-rec
      *****          move tor-chiave to ror-chiave
      *****          start rordini key >= ror-chiave
      *****                invalid continue
      *****            not invalid
      *****                perform until 1 = 2
      *****                   read rordini next at end exit perform end-read
      *****                   if ror-anno       not = tor-anno or
      *****                      ror-num-ordine not = tor-numero
      *****                      exit perform
      *****                   end-if
      *****                   move ror-rec to bror-rec
      *****                   write bror-rec invalid continue end-write
      *****                end-perform
      *****          end-start
      *****     end-read.

      ***---
       SCRIVI-RIGHE-MAN.
PATCH*******    Prima le cancello tutte per sicurezza
PATCH *****     move low-value  to ror-chiave.
PATCH *****     move tor-anno   to ror-anno.
PATCH *****     move tor-numero to ror-num-ordine.
PATCH *****     start rordini key >= ror-chiave
PATCH *****           invalid continue
PATCH *****       not invalid
PATCH *****           perform until 1 = 2
PATCH *****              read rordini next no lock 
PATCH *****                   at end exit perform 
PATCH *****              end-read
PATCH *****              if ror-anno       not = tor-anno or
PATCH *****                 ror-num-ordine not = tor-numero
PATCH *****                 exit perform
PATCH *****              end-if
PATCH *****              delete rordini record 
PATCH *****                     invalid
PATCH *****                     move 2 to tipo-messaggio
PATCH *****                     perform INVIO-MAIL
PATCH *****              end-delete
PATCH *****           end-perform
PATCH *****     end-start.
PATCH      move 0 to righe-finali progressivo write-effettuate.
                        
           set CancellazioneLogica to true.
           |Azzero prima TUTTE le quantità sui progressivi
           perform DELETE-RIGHE.
           set HoSalvato to true.

           if not SystemErrorOccurred
              set tutto-ok        to true
LABLAB        set ordine-bloccato to false
LABLAB        set no-promo        to true
LUBEXX        set EsisteIVA to false
              inquire form1-gd-1, last-row in tot-righe
              perform varying riga from 2 by 1
                        until riga > tot-righe
PATCH            add 1 to progressivo

                 initialize ror-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move tor-anno   to ror-anno
              
                 move tor-numero to ror-num-ordine

PATCH            move progressivo to ror-num-riga
PATCH            modify form1-gd-1(riga, 1), cell-data ror-num-riga

                 inquire form1-gd-1(riga, 2), cell-data in col-art
                 move col-art to ror-cod-articolo
              
                 inquire form1-gd-1(riga, 4), cell-data in col-qta
                 move col-qta  to ror-qta
              
                 inquire form1-gd-1(riga, 5), cell-data in col-uni
                 move col-uni to ror-prz-unitario
              
                 inquire form1-gd-1(riga, 6), cell-data in col-sconto
                 move col-sconto to ror-perce-sconto
              
                 inquire form1-gd-1(riga, 7), cell-data in col-cons
                 move col-cons to ror-imp-consumo
              
                 inquire form1-gd-1(riga, 8), cell-data in col-cou
                 move col-cou to ror-imp-cou-cobat
              
                 inquire form1-gd-1(riga, 9), cell-data in col-add
                 move col-add to ror-add-piombo
              
                 inquire form1-gd-1(riga, 10), cell-data in col-imp
                 move col-imp to ror-imponib-merce

LUBEXX           if ef-iva-buf = spaces
                    inquire form1-gd-1(riga, 11), cell-data in col-iva
                    move col-iva to ror-cod-iva
LUBEXX           else
LUBEXX              if ror-prz-unitario = 0
LUBEXX                 inquire form1-gd-1(riga, 11) cell-data in col-iva
LUBEXX                 move col-iva to ror-cod-iva
LUBEXX              else
LUBEXX                 move ef-iva-buf to ror-cod-iva
LUBEXX              end-if
LUBEXX           end-if               
              
LUBEXX           if not EsisteIVA
LUBEXX              perform CONTROLLA-PERCENTUALE-IVA
LUBEXX           end-if
              
                 if ror-prz-unitario = 0
LUBEXX              if tca-no-speciale
LUBEXX                 set ror-si-omaggio to true
LUBEXX              else
LUBEXX                 set ror-no-omaggio to true
LUBEXX              end-if
                 else
                    set ror-no-omaggio to true
                 end-if  

                 inquire form1-gd-1(riga, 1), hidden-data in 
           gruppo-hidden
              
      *****           inquire form1-gd-1(riga, 78-NumColMan + 5),  
      *****                   hidden-data in hid-imballi
                 move hid-imballi to ror-qta-imballi
              
      *****           inquire form1-gd-1(riga, 78-NumColMan + 7),  
      *****                   hidden-data in hid-des-imballo
                 move hid-des-imballo to ror-des-imballo
              
      *****           inquire form1-gd-1(riga, 78-NumColMan + 8),  
      *****                   hidden-data in hid-cod-art-cli
                 move hid-cod-art-cli to ror-cod-art-cli
              
      *****           inquire form1-gd-1(riga, 78-NumColMan + 12), 
      *****                   hidden-data in hid-utf
              
      *****           inquire form1-gd-1(riga, 78-NumColMan + 13), 
      *****                   hidden-data in hid-non-utf
      *****           inquire form1-gd-1(riga, 78-NumColMan + 15), 
      *****                hidden-data in hid-old-qta  

      *****           inquire form1-gd-1(riga, 78-NumColMan + 20), 
      *****                   hidden-data in hid-qta-omaggi
OMAGGI           move hid-qta-omaggi to ror-qta-omaggi
              
BLISTR*****           inquire form1-gd-1(riga, 78-NumColMan + 21), 
      *****                   hidden-data in hid-blister
BLISTR           move hid-blister to ror-blister
              
BLISTR           if hid-blister = 0
                    compute ror-num-colli = ror-qta / hid-imballi
BLISTR           else
BLISTR              move hid-imballi to ror-num-colli
BLISTR           end-if
              
LABLAB*****           inquire form1-gd-1(riga, 78-NumColMan + 23),
LABLAB*****                   hidden-data in hid-prz-commle
LABLAB           move hid-prz-commle to ror-prz-commle
              
LABLAB           if ror-prz-commle not = ror-prz-unitario and
LABLAB              ror-prz-commle not = 0
LABLAB              |Blocco di nuovo l'ordine
LABLAB              set hid-bloccato to true
LABLAB              move 0 to hid-prz-commle ror-prz-commle
                    modify form1-gd-1(riga, 1), hidden-data 
           gruppo-hidden
LABLAB*****           modify form1-gd-1(riga, 78-NumColMan + 22),
LABLAB*****                  hidden-data hid-flag-bloccato
LABLAB*****           modify form1-gd-1(riga, 78-NumColMan + 23),
LABLAB*****                   hidden-data hid-prz-commle
LABLAB           end-if
              
LABLAB*****           inquire form1-gd-1(riga, 78-NumColMan + 22),
      *****                   hidden-data in hid-flag-bloccato
LABLAB           if hid-bloccato
                    set ror-bloccato-prezzo-si to true
LABLAB              set ror-bloccato    to true
LABLAB              set ordine-bloccato to true
LABLAB           else
LABLAB              set ror-attivo   to true          
                    set ror-bloccato-prezzo-no to true
LABLAB           end-if

                 if ror-prz-unitario >= 999999 and tcl-gdo-si
                    set  ordine-bloccato to true
                    set  ror-bloccato    to true
                    set  tor-bloccato    to true
                    move tor-stato       to save-stato
                 end-if
              
LABLAB*****           inquire form1-gd-1(riga, 78-NumColMan + 24),
LABLAB*****                   hidden-data in hid-promo
LABLAB           move hid-promo      to ror-promo
                 move hid-bli-codice to ror-bli-codice
              
                 move tor-dati-comuni to ror-dati-comuni
              
      *****           inquire form1-gd-1(riga, 78-NumColMan + 1),  
      *****                   hidden-data in hid-cod-articolo
      *****           inquire form1-gd-1(riga, 78-NumColMan + 2),  
      *****                hidden-data in hid-cod-magazzino
      *****        inquire form1-gd-1(riga, 78-NumColMan + 3),  
      *****                hidden-data in hid-tipo-imballo
      *****        inquire form1-gd-1(riga, 78-NumColMan + 4),  
      *****                hidden-data in hid-peso

LUBEXX*          Controllo che non ci sia discrepanza tra articolo
LUBEXX*          venduto (rordini) ed articolo scaricato (progmag)
LUBEXX           if hid-cod-articolo not = ror-cod-articolo
LUBEXX              perform VALORIZZA-PROGRESSIVO-CORRETTO
LUBEXX           end-if
              
      *****           if YesDeleted
      *****              if CallWProgmag
                       if tor-da-ordine-no
                          |Aggiorno i progressivi con la quantità 
                          |totale dato che l'ho storanta in precedenza
                          initialize link-wprogmag
                          set link-update      to true
                          move HiddenKey       to link-key
                          move ef-cau-buf      to link-causale
                          move ror-qta         to link-valore  
                          perform VALORIZZA-ARRAY-CAUSALI
                          if tor-anno-bolla not = 0
                             |L'impegnato non va proprio considerato
                             move  0            to multiplyer(2)
                             |1 perchè diminuisce di default
                             move 1            to multiplyer(1)
                             move 1            to multiplyer(15)
                          end-if
                          move user-codi       to link-user of 
           link-wprogmag
                          call   "wprogmag" using link-wprogmag
                          cancel "wprogmag"
                       end-if
      *****              end-if
      *****           else
      *****              if ror-qta not = hid-old-qta
      *****                 initialize link-wprogmag
      *****                 move user-codi       to link-user of link-wprogmag
      *****                 set link-update      to true
      *****                 move HiddenKey       to link-key
      *****                 move ef-cau-buf      to link-causale
      *****                 compute link-valore = ( ror-qta - hid-old-qta )
      *****                 perform VALORIZZA-ARRAY-CAUSALI
      *****                 move user-codi       to link-user of link-wprogmag
      *****                 call   "wprogmag" using link-wprogmag
      *****                 cancel "wprogmag"
      *****              end-if
      *****           end-if
               
                 move HiddenKey to ror-prg-chiave prg-chiave

                 move ror-prg-cod-articolo to ror-cod-articolo
               
LUBEXX           perform FORZA-PESO-UGUALE
               
LABLAB           if ror-promo not = 0
LABLAB              set  si-promo  to true
LABLAB           end-if
              
PATCH            if ror-num-ordine = 0
PATCH               display message "EVASIONE ZERO"
PATCH                        x"0d0a""RICARICARE L'ORDINE"
PATCH                         title tit-err
PATCH                          icon 3
PATCH            end-if
              
                 accept como-ora from time
                 move data-oggi  to ror-data-ultima-modifica
                 move como-ora   to ror-ora-ultima-modifica
                 move user-codi  to ror-utente-ultima-modifica

                 if tor-da-ordine-si
                    inquire form1-gd-1(riga, 2), 
                           hidden-data in ror-chiave-ordine
                    perform QTA-EVASIONE-ORDINE
                    
                    set scrivi to true
                    perform PROGMAG-MASTER
                 end-if

      *    Luciano
                 move hid-prz-manuale  to ror-prz-manuale
      *    Luciano fine
              
                 set ror-cancellato to false
                 write ror-rec
                       invalid 
                       rewrite ror-rec
                               invalid continue
PATCH                      not invalid add 1 to write-effettuate
                       end-rewrite
PATCH              not invalid add 1 to write-effettuate
                 end-write

                 if prm-imb-minimo-si
                    perform QTA-MINIMA-IMBALLO
                 end-if 

      *****           ||||||||||||||||
      *****           if tor-anno-bolla = 0
      *****              move ror-prg-chiave to prg-chiave
      *****              read progmag no lock
      *****              if prg-impegnato < 0
      *****                 perform 5 times
      *****                    display message 
      *****                      "EVASIONE: " tor-numero
      *****               x"0d0a""ART : " prg-cod-articolo
      *****               x"0d0a""MAG : " prg-cod-magazzino
      *****               x"0d0a""IMB : " prg-tipo-imballo
      *****               x"0d0a""PESO: " prg-peso
      *****               x"0d0a""IMP : " prg-impegnato
      *****               x"0d0a""FROM: GORDCVAR"
      *****               x"0d0a"
      *****               x"0d0a""CONTATTARE ASSISTENZA!!!"
      *****                                title titolo
      *****                                 icon 3
      *****                end-perform
      *****             end-if
      *****             if prg-impegnato < ( prg-imp-GDO + prg-imp-TRAD )
      *****                perform 5 times
      *****                   display message 
      *****                      "EVASIONE: " tor-numero
      *****               x"0d0a""ART :  " prg-cod-articolo
      *****               x"0d0a""MAG :  " prg-cod-magazzino
      *****               x"0d0a""IMB :  " prg-tipo-imballo
      *****               x"0d0a""PESO:  " prg-peso
      *****               x"0d0a""IMP :  " prg-impegnato
      *****               x"0d0a""IMP G: " prg-imp-GDO
      *****               x"0d0a""IMP T: " prg-imp-TRAD
      *****               x"0d0a""FROM:  GORDCVAR"
      *****               x"0d0a"
      *****               x"0d0a""CONTATTARE ASSISTENZA!!!"
      *****                             title titolo
      *****                              icon 3
      *****                end-perform
      *****             end-if
      *****          end-if
      *****           |||||||||||||

                 evaluate status-rordini 
                 when "00"
                 when "02" continue
                 when other
PATCH                 move 3 to tipo-messaggio
PATCH                 perform INVIO-MAIL
PATCH                 exit perform
                 end-evaluate

              end-perform 
                                      
LABLAB        if ordine-bloccato
LABLAB           set tor-bloccato to true
LABLAB        end-if
              
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
      *****           perform 5 times
      *****              call   "geslock" using geslock-linkage
      *****              cancel "geslock"
      *****           end-perform
              end-if
              
PATCH**       Invio mail nel caso in cui le righe presenti su
PATCH**       griglia non siano le stesse scritte su file 
PATCH         inquire form1-gd-1, last-row in save-tot-righe
PATCH         subtract 1 from save-tot-righe
              
PATCH         move low-value  to ror-chiave
PATCH         move tor-anno   to ror-anno
PATCH         move tor-numero to ror-num-ordine
PATCH         start rordini key >= ror-chiave
PATCH               invalid continue
PATCH           not invalid
PATCH               perform until 1 = 2
PATCH                  read rordini next no lock 
PATCH                       at end exit perform 
PATCH                  end-read
PATCH                  if ror-anno       not = tor-anno or
PATCH                     ror-num-ordine not = tor-numero
PATCH                     exit perform
PATCH                  end-if
PATCH                  if ror-cancellato
PATCH                     delete rordini record
PATCH                            invalid continue
PATCH                     end-delete
PATCH                     if status-rordini not = "00"
PATCH                        move 6 to tipo-messaggio
PATCH                        perform INVIO-MAIL
PATCH                        exit perform
PATCH                     end-if
PATCH                  end-if
PATCH               end-perform
PATCH         end-start
              
PATCH         move 0 to righe-finali
PATCH         move low-value  to ror-chiave
PATCH         move tor-anno   to ror-anno
PATCH         move tor-numero to ror-num-ordine
PATCH         start rordini key >= ror-chiave
PATCH               invalid continue
PATCH           not invalid
PATCH               perform until 1 = 2
PATCH                  read rordini next no lock 
PATCH                       at end exit perform 
PATCH                  end-read
PATCH                  if ror-anno       not = tor-anno or
PATCH                     ror-num-ordine not = tor-numero
PATCH                     exit perform
PATCH                  end-if
PATCH                  add 1 to righe-finali
PATCH               end-perform
PATCH         end-start
PATCH      end-if.

      ***---
       STAMPA.
           call   "st-ordine" using tor-chiave,
                                    link-path, "N".
           cancel "st-ordine".

      ***---
       TORNA-IN-VISUA.
           move 0 to mod.
           move 0 to mod-k.
           move 78-ID-ef-anno to control-id.
           set NoMessage to true.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG.
           unlock tordini all records.
                        
           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.
           move 4 to ACCEPT-CONTROL.

           move 3 to NumBitmapDatiBolla.
           initialize gruppo-hidden old-art-codice.

           move 0      to volantino-forzato.
           move spaces to lab-forzato-buf.
           display lab-forzato.

PATCH      |Usato come flag (se 0 non ho richiamato la mail)
PATCH      move 0 to tipo-messaggio save-tot-righe.

      ***---
       TROVA-BOLLA.
      *    QUESTO PRF. VA LASCIATO QUI PER FORZA!!!
           set trovato to false.
           move tge-anno      to tor-anno-bolla.
           read tordini no lock key is k-bolla
                invalid continue
            not invalid set trovato to true
           end-read.

      ***---
       VALORIZZA-OLD.
           move tor-rec                 to old-tor-rec.
           set vecchio                  to true.
      
           if old-tor-spostam-ric-ago = space
              move "N" to old-tor-spostam-ric-ago
           end-if.
      
           if old-tor-spostam-ric-dic = space
              move "N" to old-tor-spostam-ric-dic
           end-if.
      
           evaluate CONTROL-ID
           when 78-ID-ef-age
           when 78-ID-ef-pag
           when 78-ID-ef-iva
           when 78-ID-ef-vet
           when 78-ID-ef-cod-iva
                move 1 to StatusHelp
           when other
                move 0 to StatusHelp
           end-evaluate.
           perform STATUS-HELP.
      
      *     move 0 to riga-nuova.
           set RigaCambiata   to false.
           set PrezzoCambiato to false.

LUBEXX     move tor-data-bolla to orig-data-bolla.
LUBEXX     move tor-num-bolla  to orig-num-bolla.

      ***---
       DELETE-ESITI.
           move tor-chiave to eor-tor-chiave.
           move low-value  to eor-num-riga.

           start eordini key is >= eor-chiave
              invalid 
                 continue
              not invalid 
                 perform until 1 = 2
                    read eordini next no lock 
                       at end 
                          exit perform 
                    end-read
                    if tor-chiave not = eor-tor-chiave
                       exit perform
                    end-if
                    delete eordini record 
                       invalid
                          continue
                    end-delete
                 end-perform
           end-start.

      ***---
       RECUPERA-ULTIMO-ESITO.
           move tor-chiave   to eor-tor-chiave
           move high-value   to eor-num-riga
           start eordini key not > eor-chiave
              invalid
                 continue
              not invalid
                 read eordini previous
                    at end
                       continue
                    not at end
                       if tor-chiave = eor-tor-chiave
                          move eor-esito to tor-esito-consegna
                       end-if
                 end-read
           end-start.

      ***---
      ***** OPEN-TMP-MOD.
      *****     accept como-ora   from time
      *****     accept como-data  from century-date
      *****     accept path-tmp-mod-rordini from environment "PATH_ST"
      *****     inspect path-tmp-mod-rordini 
      *****                       replacing trailing space by low-value
      *****     string path-tmp-mod-rordini   delimited by low-value
      *****            "tmp-mod-rordini_"     delimited by size
      *****            como-data              delimited by size
      *****            "_"                    delimited by size
      *****            como-ora               delimited by size
      *****            into path-tmp-mod-rordini
      *****
      *****     inspect path-tmp-mod-rordini 
      *****                       replacing trailing low-value by space 
      *****
      *****     open output tmp-mod-rordini
      *****     close tmp-mod-rordini
      *****     open i-o tmp-mod-rordini.
      *****
      ********---
      ******    Luciano
      ***** MOD-MAIL.
      *****     if NewRow
      *****        initialize hidden-modifiche
      *****        add 1          to como-riga
      *****        move como-riga to hid-riga
      *****                          tmp-mror-num-riga
      *****        modify form1-gd-1(riga, 3), 
      *****               hidden-data hidden-modifiche
      *****
      *****        initialize tmp-mor-articolo-mod
      *****                   tmp-mror-imb-mod
      *****                   tmp-qta-mod
      *****
      *****        set tmp-mror-inserita   to true
      *****        write tmp-mror-rec
      *****           invalid
      *****              display message box "controllare"
      *****        end-write
      *****     else
      *****        move hid-riga           to tmp-mror-num-riga
      *****        perform LEGGI-TMP-MOD-RORDINI
      *****        set tmp-mror-modifica   to true
      *****     end-if.
      *****
      *****
      *****     move hid-cod-articolo   to tmp-mor-articolo-mod
      *****     move hid-tipo-imballo   to tmp-mror-imb-mod
      *****     move col-qta            to tmp-qta-mod
      *****
      *****     rewrite tmp-mror-rec
      *****        invalid
      *****           continue
      *****     end-rewrite.
      *****
      ********---
      ***** LEGGI-TMP-MOD-RORDINI.
      *****     read tmp-mod-rordini
      *****        invalid
      *****           move hid-art-orig to tmp-mor-articolo-orig
      *****           move hid-imb-orig to tmp-mror-imb-orig
      *****           move hid-qta-orig to tmp-qta-orig
      *****           move space        to tmp-mror-tipo-modifica
      *****           write tmp-mror-rec
      *****              invalid
      *****                 continue
      *****           end-write
      *****     end-read.
      *****
      *****
      ********---
      ***** CANC-MAIL.
      ******    se il numero d'ordine è zero vuol dire che è una riga che ho 
      ******    inserito io e quindi cancello il record del temporaneo senza
      ******    fare altro
      *****     move hid-riga      to tmp-mror-num-riga
      *****     perform LEGGI-TMP-MOD-RORDINI
      *****
      *****     if tmp-mror-inserita
      *****        delete tmp-mod-rordini record
      *****           invalid
      *****              continue
      *****        end-delete
      *****     else
      *****        set tmp-mror-cancellata to true
      *****        rewrite tmp-mror-rec
      *****           invalid
      *****              continue
      *****        end-rewrite
      *****     end-if.

      ***---
       PB-GRID-ELIMINA-LINKTO-CODE.
           if mod-campi           = 0 or
              mod-cliente-destino = 1 or
              bollettata
              exit paragraph
           end-if.

           inquire form1-gd-1, last-row in tot-righe, cursor-y in riga.
           if tot-righe > 1
              if riga <= tot-righe or riga > 2
      *    Luciano
                 inquire form1-gd-1(riga, 1), 
                          hidden-data in gruppo-hidden
                 inquire form1-gd-1(riga, 3), 
                          hidden-data hidden-modifiche
                 if hid-blister = 0
                    display message "Cancellare la riga selezionata?"
                              title titolo
                               type mb-yes-no
                             giving scelta
                 else
                    display message "Cancellare il blister selezionato?"
                              title titolo
                               type mb-yes-no
                             giving scelta
                 end-if
      *    Luciano
                 if scelta = mb-yes
                            
                    move col-qta to como-qta
                    if prg-giacenza >= como-qta and not NewRow
                       display message "Si sta tagliando "
                                       "merce con giacenza."
                                x"0d0a""Confermare?"
                                 title titolo
                                  icon 2
                                  type mb-yes-no
                                giving scelta
                    end-if

                    if scelta = mb-yes
      *    luciano
                       if hid-blister = 0
                          move riga   to store-riga
                          perform CANC-RIGA
                       else
                          perform CANC-BLISTER
                       end-if

                       inquire form1-gd-1, last-row in tot-righe
                       if tot-righe = 1                         
                          perform CANCELLA-COLORE
                          perform PULISCI-CAMPI-LABELS
                       else
LUBEXX                    move riga to store-riga
                          perform CALCOLA-TOTALE-IVATO
LUBEXX                    move store-riga to riga
                          if riga > tot-righe
                             move tot-righe to riga
                          end-if
                          modify form1-gd-1, cursor-y = riga
                          move riga to event-data-2
                          perform SPOSTAMENTO   
                          set ControllaCampi to true
                       end-if
                       set YesDeleted to true
                    end-if
                 end-if
              else
                 display message "Occorre selezionare una riga"
                           title titolo
              end-if
           end-if.

      ***---
       CANC-RIGA.
      *    Luciano
      *****     if mail-modifica-GET or mail-modifica-SHI
      *****        perform CANC-MAIL
      *****     end-if
      *    Luciano fine
           set RigaCambiata   to true
           set PrezzoCambiato to false
           perform AGGIORNA-NUM-COLLI-DEL
           modify  form1-gd-1, record-to-delete = store-riga.

      ***---
       CANC-BLISTER.
           move riga to store-riga
           perform until 1 = 2
              inquire form1-gd-1(store-riga, 1),
                       hidden-data in gruppo-hidden
              inquire form1-gd-1(store-riga, 3), 
                       hidden-data hidden-modifiche
              if hid-imballi not = 0
                 perform CANC-RIGA
                 exit perform
              end-if
              subtract 1 from store-riga
           end-perform
           perform until 1 = 2
              inquire form1-gd-1, last-row in tot-righe
              inquire form1-gd-1(store-riga, 1),
                       hidden-data in gruppo-hidden
              inquire form1-gd-1(store-riga, 3), 
                       hidden-data hidden-modifiche
              if hid-imballi not = 0 or 
                 store-riga > tot-righe
                 exit perform
              end-if
              perform CANC-RIGA
           end-perform.

      ***---
       VAL-HID-BLISTER.
           if ror-bli-codice = zero
              exit paragraph
           end-if
              

           if hid-imballi not = zero
              move zero   to idx-blister
              move ror-bli-codice  to bli-codice
              read blister no lock
                 invalid
                    initialize bli-dati
              end-read
           end-if
           add 1 to idx-blister.
           move bli-el-qta(idx-blister)     to hid-bli-qta
           move bli-el-perce(idx-blister)   to hid-bli-perce.

      ***---
       QTA-MINIMA-IMBALLO.
           move 0 to link-wprogmag-status.
           move ror-chiave-ordine to mro-chiave.
           perform READ-MRORDINI-LOCK.
           move 0 to qta-evadere.
           if not mro-si-blister
              compute como-imballi = mro-qta-e / mro-qta-imballi
              compute qta-evadere  = mro-qta-e - 
                                     como-imballi * mro-qta-imballi
           end-if.
           |TORNO PARI ED HO ANCORA QTA DA EVADERE:
           |LA QTA ORDINATA SARA' STATA PRECEDENTEMENTE ADATTATA
           |RISULTANDO DIVERSA DAGLI IMBALLI NECESSARI
           if qta-evadere = 0 and mro-qta > mro-qta-e
              compute como-imballi = mro-qta / mro-qta-imballi
              compute qta-evadere  = mro-qta - 
                                     como-imballi * mro-qta-imballi

           end-if.

           if qta-evadere > 0
                                           
              compute qta-evadere  = mro-qta - mro-qta-e             
              move mro-qta-e to mro-qta
              rewrite mro-rec
              unlock mrordini record

              initialize fp-linkage
              move mro-chiave-testa  to fp-chiave
              call   "find-progr" using fp-linkage
              cancel "find-progr"
              move fp-riga     to mro-riga
              move fp-progr    to mro-progr
              if qta-evadere < mro-qta-imballi
                 move qta-evadere to mro-qta
                 move mro-qta     to mro-num-colli 
                 move 1 to mro-qta-imballi
                 move "A VISTA"   to mro-des-imballo
              else
                 compute como-imballi = qta-evadere /  mro-qta-imballi
                 add 1 to como-imballi
                 compute mro-qta      = como-imballi * mro-qta-imballi
              end-if
              move 0 to mro-qta-e
              move 0 to mro-qta-b
              move 0 to mro-qta-f
              move 0 to mro-qta-omaggi

              accept mro-data-creazione from century-date
              accept mro-ora-creazione  from time
              move user-codi to mro-utente-creazione
              move 0 to mro-data-ultima-modifica
              move 0 to mro-ora-ultima-modifica
              move spaces to mro-utente-ultima-modifica
              set mro-registrato to true
              write mro-rec

           end-if.
           unlock mrordini record.

      ***---
       AGGIORNA-LAB-INVIATO.
           if tca-blocco-modeva-si
              if tor-da-inviare-si
                 move "NON INVIATO" to lab-inviato-buf
              else
                 move "INVIATO"     to lab-inviato-buf
              end-if
           else
              move spaces to lab-inviato-buf
           end-if.
           display lab-inviato.
