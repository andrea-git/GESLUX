      *******************************************************************
      *                  SEZIONE PARAGRFI (User Dephined)               *
      *******************************************************************

      ***---
       ABILITAZIONI.           
           if mod = 1                  
              move BitmapDeleteEnabled to BitmapNumDelete
              move BitmapSaveEnabled   to BitmapNumSave
              move 1 to e-salva e-cancella

              move 0 to mod-destini
              move 0 to mod-campi

              inquire cbo-stato,   value cbo-stato-buf
              inquire cbo-stato-d, value cbo-stato-d-buf

              if cbo-stato-buf not = "Disattivo" 
                 move 1 to mod-campi
                 if cbo-stato-d-buf not = "Disattivo"
                    move 1 to mod-destini  
                 end-if
              end-if

           else         

              move BitmapDeleteDisabled to BitmapNumDelete
              move BitmapSaveDisabled   to BitmapNumSave
              move 0 to e-salva e-cancella
              move 0 to mod-campi
              move 0 to mod-destini
           end-if.            



      * ISACCO (CORREZIONE SU LIVELLO ABILITAZIONE 2) - 10/11/2003
      * (CANCELLAZIONE NON PERMESSA)
           if livello-abil = 2 
              move ZERO to e-cancella
              move BitmapDeleteDisabled to BitmapNumDelete
           end-if.
      * FINE

           modify tool-cancella, enabled = e-cancella.
           modify tool-salva,    enabled = e-salva.
       
      * ISACCO (SERVE SEMPRE PER LA MODIFICA PRCEDENTE E PER RIPRISTINARE
      * LE BITMAP DISABILITATE. - 10/11/2003
      *     perform FORM1-DISPLAY.
           display tool-nuovo, tool-salva, tool-cancella, tool-modifica.
      * FINE.

      ***---
       ANTEPRIMA.
           SET ANTEPRIMA     TO TRUE.

      ***---
       CANCELLA.
           if mod = ZERO  
              exit paragraph  
           end-if.

           inquire ef-codice, value in ef-codice-buf.

           move ef-codice-buf to codice-x.

           call "C$JUSTIFY" using codice-x, "R".
           inspect codice-x replacing all X"20" by x"30".

           if codice-x is numeric
              call   "del-cli" using codice-x, SceltaDelete
              cancel "del-cli"
              evaluate true
              when annulla   
                   continue
              when elimina   
                   initialize G2Agg-linkage
                   move cli-codice to G2Agg-codice
                   move cli-chiave to save-chiave                                
                   perform FORM1-DELETE
                   if totem-msg-return-value = 1
                      move codice-x to notf-codice of notef
                      move 0        to notf-prog   of notef
                      delete notef record 
                         invalid 
                            continue
                      end-delete
                      move save-chiave to cli-chiave
                      perform LOOP-DELETE-NOTE
                      initialize WrkCampi old-notf-rec
                                replacing numeric data by zeroes
                                     alphanumeric data by spaces

                      set G2Agg-for    to true
                      set G2Agg-delete to true
                      call   "G2Agg" using G2Agg-linkage
                      cancel "G2Agg"
                      perform PRIMO
                   end-if
              when disattiva
                   move "clienti" to nome-file
                   perform CHANGE-STATUS
                   move "D" to old-cli-stato |Disattivo
                   perform SALVA
              end-evaluate
           else
              display message box "Impossibile procedere "
                                  "con la cancellazione."
                                  "Digitare un valore numerico."
                      title = tit-err
                      icon mb-warning-icon
           end-if.


      ***---
       CERCA.
           evaluate control-id 
           when 78-ID-ef-cap               
                move "anacap"        to como-file         
                inquire ef-cap,   value in anc-cap
                call "zoom-gt"   using como-file, anc-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move anc-cap to ef-cap-buf
                   display ef-cap 
                end-if      
  
           when 78-ID-ef-gdo
                move "tgrupgdo"      to como-file         
                inquire ef-gdo,   value in gdo-codice
                call "zoom-gt"   using como-file, gdo-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move gdo-codice to ef-gdo-buf
                   display ef-gdo
                   move gdo-intestazione to lab-gdo-buf
                   display lab-gdo
                end-if        

           when 78-ID-ef-cap-d
                move "anacap"        to como-file         
                inquire ef-cap-d,   value in anc-cap
                call "zoom-gt"   using como-file, anc-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move anc-cap to ef-cap-d-buf
                   display ef-cap-d
                end-if

           when 78-ID-ef-prov               
                move "tprov"     to como-file         
                inquire ef-prov,    value in prv-codice
                call "zoom-gt"   using como-file, prv-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move prv-codice to ef-prov-buf
                   display ef-prov
                   move "tprov" to nome-file
                   perform RELAZIONI-CLIENTI
                end-if

           when 78-ID-ef-nazione            
                move "tnazioni"  to como-file         
                inquire ef-nazione, value in naz-codice
                call "zoom-gt"   using como-file, naz-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move naz-codice      to ef-nazione-buf
                   move naz-descrizione to lab-nazione-buf
                   display ef-nazione lab-nazione
                end-if

           when 78-ID-ef-vettore            
                move "tvettori"  to como-file         
                inquire ef-vettore, value in vet-codice
                call "zoom-gt"   using como-file, vet-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move vet-codice      to ef-vettore-buf
                   move vet-descrizione to lab-vettore-buf
                   display ef-vettore lab-vettore
                end-if
      
           when 78-ID-ef-iva                
                move "tivaese-ese"   to como-file
                move "IV"        to tbliv-codice1
                inquire ef-iva,  value in tbliv-codice2
                call "zoom-gt"   using como-file, record-tbliv
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move tbliv-codice2   to ef-iva-buf
                   initialize lab-iva-buf
                   inspect tbliv-descrizione1 replacing trailing 
                                                 spaces by low-value
                   string  tbliv-descrizione1 delimited by low-value
                           " "                delimited by size
                           tbliv-descrizione2 delimited by size
                           into lab-iva-buf
                   end-string
                   display ef-iva lab-iva
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
                   initialize lab-pag-buf
                   inspect tblpa-descrizione1 replacing trailing 
                                                 spaces by low-value
                   string  tblpa-descrizione1 delimited by low-value
                           " "                delimited by size
                           tblpa-descrizione2 delimited by size
                           into lab-pag-buf
                   end-string
                   display ef-pag lab-pag
                end-if

           when 78-ID-ef-abi
           when 78-ID-ef-cab                          
                move "ABI"                to como-file              
                inquire ef-abi,     value in abi-codice-abi-x
                inquire ef-cab,     value in abi-codice-cab-x
                call "zoom-gt"   using como-file, record-abi
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move abi-codice-abi-x to ef-abi-buf
                   move abi-codice-cab-x to ef-cab-buf
                   display ef-abi ef-cab
                   move "ABI" to nome-file
                   perform RELAZIONI-CLIENTI
                end-if

           when 78-ID-ef-prov-d             
                move "tprov"     to como-file         
                inquire ef-prov-d,  value in prv-codice
                call "zoom-gt"   using como-file, prv-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move "tprov" to nome-file
                   move prv-codice to ef-prov-d-buf
                   display ef-prov-d
                   perform RELAZIONI-DESTINI
                end-if

           when 78-ID-ef-nazione-d          
                move "tnazioni"  to como-file         
                inquire ef-nazione-d, value in naz-codice
                call "zoom-gt"   using como-file, naz-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move naz-codice      to ef-nazione-d-buf
                   move naz-descrizione to lab-nazione-d-buf
                   display ef-nazione-d lab-nazione-d
                end-if            

           when 78-ID-ef-vettore-d                    
                move "tvettori"  to como-file         
                inquire ef-vettore-d, value in vet-codice
                call "zoom-gt"   using como-file, vet-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move vet-codice      to ef-vettore-d-buf
                   move vet-descrizione to lab-vettore-d-buf
                   display ef-vettore-d lab-vettore-d
                end-if

           when 78-ID-ef-pag-d                    
                move "tcodpag"    to como-file         
                move "PA"         to tblpa-codice1
                inquire ef-pag-d, value in tblpa-codice2
                call "zoom-gt"    using como-file, record-tblpa
                                 giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move tblpa-codice2   to ef-pag-d-buf
                   initialize lab-pag-d-buf
                   inspect tblpa-descrizione1 replacing trailing 
                                                 spaces by low-value
                   string  tblpa-descrizione1 delimited by low-value
                           " "                delimited by size
                           tblpa-descrizione2 delimited by size
                           into lab-pag-d-buf
                   end-string
                   display ef-pag-d lab-pag-d
                end-if                  

           when 78-ID-ef-st-naz
                move "tnazioni"  to como-file         
                inquire ef-st-naz, value in naz-codice
                call "zoom-gt"   using como-file, naz-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move naz-codice      to ef-st-naz-buf
                   move naz-descrizione to lab-st-naz-buf
                   display ef-st-naz lab-st-naz
                end-if
                                    
           when 78-ID-ef-cod-da
                set cli-tipo-F to true
                move   "clienti-all"  to como-file
                call   "zoom-gt"   using como-file, cli-rec
                                  giving stato-zoom
                cancel "zoom-gt"
      
                if stato-zoom = 0
                   move cli-codice to ef-cod-da-buf
                   display ef-cod-da
                end-if

           when 78-ID-ef-cod-a
                set cli-tipo-F to true
                move   "clienti-all"  to como-file
                call   "zoom-gt"   using como-file, cli-rec
                                  giving stato-zoom
                cancel "zoom-gt"
      
                if stato-zoom = 0
                   move cli-codice to ef-cod-a-buf
                   display ef-cod-a
                end-if

           when 78-ID-ef-st-naz
                move "tnazioni"  to como-file         
                inquire ef-st-naz, value in naz-codice
                call "zoom-gt"   using como-file, naz-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move naz-codice      to ef-st-naz-buf
                   move naz-descrizione to lab-st-naz-buf
                   display ef-st-naz lab-st-naz
                end-if

           when 78-ID-ef-st-prov
                move "tprov"     to como-file         
                inquire ef-st-prov,    value in prv-codice
                call "zoom-gt"   using como-file, prv-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move prv-codice      to ef-st-prov-buf
                   move prv-descrizione to lab-st-prov-buf
                   display ef-st-prov lab-st-prov
                end-if

           end-evaluate. 


      ***---
       CLEAR-SCREEN.
           move cli-codice to old-cli-codice.

           initialize cli-dati  replacing numeric data by zeroes
                                     alphanumeric data by spaces.
                        
           perform FORM1-FLD-TO-BUF. 

           perform INIT-OLD-REC.

           perform DISPLAY-SCREEN.
  
           unlock clienti all records.

           perform RESET-GRIGLIA.


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
                move 0 to mod-destini
                perform DISPLAY-SCREEN
           when "destini"
                set disattivo-d to true
                perform CARICA-COMBO-DESTINI
                |specifico per i pgm. aventi Tab-Control
                move 2 to Screen1-Ta-1-TAB-VALUE
                perform SCREEN1-TA-1-TABCHANGE
                |******
                move 0 to mod-destini
                perform DISPLAY-CAMPI-GRID
                perform DISPLAY-CAMPI-NOTE
           end-evaluate.


      ***---
       CHECK-CODFIS-PIVA.
LUBEXX*****           inquire ef-codfis   value cf-codice-fiscale.
LUBEXX*****           inquire ef-piva     value cf-piva.

           call   "codfis"     using link-codfis.
           cancel "codfis".

      * FLAG CHE INDICA SE IL CF O PIVA E' CORRETTO
           if cf-risultato not = 0
              set errori to true
           end-if.


      ***---
       CHECK-INSERIMENTO.
      * modifica la status-bar
      * controllo se da inserimento scorro. allora devo segnalare che
      * sono tornato in modifica.
           inquire tool-modifica, value in mod.

           evaluate mod
           when 1
                set StatusModifica to true
           when 0
                set StatusVisua to true   
           end-evaluate.
      
           perform STATUS-BAR-MSG.


      ***---
       COLORE.
           modify form1-gd-1, start-y = riga, y = riga,
                              start-x = 1,    x = 6,
                              region-color = 257.

                              
      ***---
       CONTROLLO.
           set tutto-ok to true. 

      * Elenco degli Id sui quali fare il CONTROLLO nel programma gclienti
      * paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del campo ef-codice
           when 78-ID-ef-codice                
                perform RIEMPI-CHIAVE
                if ef-codice-buf not = spaces                                               
                   inspect ef-codice-buf replacing trailing 
                                         spaces by low-values
                   initialize CountChar
                   inspect ef-codice-buf tallying CountChar 
                           for characters before low-value
                   inspect ef-codice-buf replacing trailing 
                                         low-values by spaces

                   | SE E' NUMERICO LEGGO IL RECORD NORMALMENTE
                   if ef-codice-buf(1:CountChar) is numeric
                      perform SELEZIONA-NUMERICO
                   else                     
                      | HO DIGITATO UNA SIGLA ALFANUMERICA QUINDI 
                      | APRO LO ZOOM (CASE SENSITIVE)
                      perform SELEZIONA-ALFA
                   end-if

                else
                   set errori to true  
                   move 78-ID-ef-codice to control-id
                   display message box "Inserimento codice mancante"
                           title = tit-err
                           icon mb-warning-icon
                end-if

           |78-ID-ef-ragsoc-1 è l'ID del campo ef-ragsoc-1
           when 78-ID-ef-ragsoc-1
                inquire ef-ragsoc-1, value in ef-ragsoc-1-buf
                if ef-ragsoc-1-buf = spaces 
                   set errori to true  
                   move 78-ID-ef-ragsoc-1 to control-id
                   display message"Inserimento Ragione Sociale mancante"
                           title = tit-err
                           icon mb-warning-icon
                end-if
                move ef-ragsoc-1-buf to lab-des-buf
                display lab-des

           |78-ID-ef-cap è l'ID del campo ef-cap
           when 78-ID-ef-cap
                inquire ef-cap, value in ef-cap-buf
                if ef-cap-buf = spaces 
                   set errori to true  
                   move 78-ID-ef-cap to control-id
                   display message box "Inserimento CAP mancante"
                           title = tit-err
                           icon mb-warning-icon 
                else
                   if ef-prov-buf = "EE"
                      set trovato to true
                   else
                      move "anacap" to nome-file
                      perform RELAZIONI-CLIENTI
                      if not trovato
                         set errori to true
                         move 78-ID-ef-cap to control-id
                         display message box "CAP NON valido"
                                 title = tit-err
                                 icon mb-warning-icon
                      end-if
                   end-if
                end-if 
           |78-ID-ef-indirizzo è l'ID del campo ef-indirizzo
           when 78-ID-ef-indirizzo
                inquire ef-indirizzo, value in ef-indirizzo-buf
                if ef-indirizzo-buf = spaces 
                   set errori to true  
                   move 78-ID-ef-indirizzo to control-id
                   display message box "Inserimento indirizzo mancante"
                           title = tit-err
                           icon mb-warning-icon
                end-if
           |78-ID-ef-localita è l'ID del campo ef-localita
           when 78-ID-ef-localita
                inquire ef-localita, value in ef-localita-buf
                if ef-localita-buf = spaces 
                   set errori to true  
                   move 78-ID-ef-localita to control-id
                   display message box "Inserimento località mancante"
                           title = tit-err
                           icon mb-warning-icon
                end-if
           |78-ID-ef-prov è l'ID del campo ef-prov
           when 78-ID-ef-prov
                move "tprov" to nome-file
                perform RELAZIONI-CLIENTI
                if not trovato
                   set errori to true
                   move 78-ID-ef-prov to control-id
                   display message box "Provincia NON valida"
                           title = tit-err
                           icon mb-warning-icon
                end-if 

           |78-ID-ef-nazione è l'ID del campo ef-nazione
           when 78-ID-ef-nazione
                move "tnazioni" to nome-file
                perform RELAZIONI-CLIENTI
                if not trovato
                   set errori to true
                   move 78-ID-ef-nazione to control-id
                   display message box "Nazione NON valida"
                           title = tit-err
                           icon mb-warning-icon
                end-if 

           |78-ID-ef-GDO è l'ID del campo ef-gdo
           when 78-ID-ef-gdo
                move "tgrupgdo" to nome-file
                perform RELAZIONI-CLIENTI

                if gdo-codice not = spaces
                   if not trovato
                      set errori to true
                      move 78-ID-ef-gdo to control-id
                      display message box "Gruppo GDO NON valido"
                              title = tit-err
                              icon mb-warning-icon
                   end-if        
                end-if 

           |78-ID-ef-vettore è l'ID del campo ef-vettore
           when 78-ID-ef-vettore
                move spaces to lab-vettore-buf
                move "tvettori" to nome-file 
                perform RELAZIONI-CLIENTI

                if vet-codice not = ZERO
                   if not trovato
                      set errori to true  
                      move 78-ID-ef-vettore to control-id
                      display message box "Vettore NON valido"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if
                                                      
           |78-ID-cbo-stato è l'ID del campo cbo-stato
           when 78-ID-cbo-stato
                perform SCARICA-COMBO-STATO
                move stato   to  cli-stato
                if cli-stato not = old-cli-stato
                   move 1 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"
         
                   if not Passwd-StatusOk
                      move old-cli-stato to stato
                      perform CARICA-COMBO-STATO
                   else
                      if disattivo
                         move control-id to store-id
                         move "clienti" to nome-file
                         perform CHANGE-STATUS
                         move store-id to control-id
                      end-if
                   end-if
               end-if

           |78-ID-ef-codfis è l'ID del campo ef-codfis
           when 78-ID-ef-codfis          
                inquire ef-nazione, value in ef-nazione-buf
                if ef-nazione-buf = "ITA"               
                   inquire ef-codfis, value  in ef-codfis-buf
                   if ef-codfis-buf not = spaces

LUBEXX                if form1-radio-1-buf = 1 |PERSONA FISICA
LUBEXX                   set cf-contr-gen to true
LUBEXX                   inquire ef-codfis value cf-codice-fiscale
LUBEXX                else
LUBEXX                   inquire ef-codfis value cf-piva
LUBEXX                   set cf-partita-iva to true
LUBEXX                end-if
LUBEXX*****                      set cf-contr-gen to true
                      perform CHECK-CODFIS-PIVA
                      if errori
                         move 78-ID-ef-codfis to control-id 
                         display message box MSG-Codifce-Fiscale-errato
                                 title = tit-err
                                 icon mb-warning-icon
                      end-if
                   end-if
                end-if

           |78-ID-ef-piva è l'ID del campo ef-piva
           when 78-ID-ef-piva
                inquire ef-piva,   value  in ef-piva-buf
                inquire ef-codfis, value  in ef-codfis-buf
                if ef-piva-buf   not = spaces
                   inquire ef-nazione, value in ef-nazione-buf
                   if ef-nazione-buf = "ITA"
                      set cf-partita-iva to true
LUBEXX                inquire ef-piva, value in cf-piva
                      perform CHECK-CODFIS-PIVA
                      if errori
                         move 78-ID-ef-piva to control-id 
                         display message box MSG-Partita-IVA-errata
                                 title = tit-err
                                 icon mb-warning-icon
                      end-if
                   end-if
                else
                   if ef-codfis-buf = spaces
                      set errori to true
                      move 78-ID-ef-codfis to control-id
                      display message box MSG-codfis-piva-mancanti
                                title = tit-err
                                 icon mb-warning-icon
                   end-if
                end-if

           |78-ID-ef-iva è l'ID del campo ef-iva
           when 78-ID-ef-iva         
                move "tivaese" to nome-file
                perform RELAZIONI-CLIENTI

                if tbliv-codice2 not = spaces
                   if not trovato
                      if ef-iva-buf not = spaces 
                         set errori to true
                         move 78-ID-ef-iva to control-id
                         display message "Codice ESENZIONE NON valido"
                                 title = tit-err
                                 icon mb-warning-icon
                      end-if
                   else
                      if tbliv-percentuale not = 0
                         set errori to true
                         move 78-ID-ef-iva to control-id
                         display message box "Codice IVA NON valido "
                                             "in quanto NON ESENTE"
                                 title = tit-err
                                 icon mb-warning-icon
                      else
                         display message "Confermi il Codice Esenzione "
                                         ef-iva-buf " ?"
                                 title = tit-err
                                 type mb-yes-no
                                 icon mb-warning-icon
                                 giving scelta
                         if scelta = mb-no
                            set errori to true
                            move 78-ID-ef-iva to control-id
                         end-if
                      end-if
                   end-if
                end-if

           |78-ID-ef-pag è l'ID del campo ef-pag
           when 78-ID-ef-pag
                inquire ef-pag, value in ef-pag-buf
                move "tcodpag" to nome-file
                perform RELAZIONI-CLIENTI
                if not trovato
                   set errori to true
                   move 78-ID-ef-pag to control-id
                   display message box "Codice pagamento NON valido"
                           title = tit-err
                           icon mb-warning-icon
                end-if   

           when 78-ID-ef-cab
                inquire ef-abi, value ef-abi-buf
                if ef-abi-buf = spaces
                   perform VERIFICA-PAGAMENTO-BANCARIO
                   if PagamentoBancario
                      set errori to true
                      move 78-ID-ef-abi to control-id
                      display message box "Codice ABI obbligatorio per "
                                          "pagamento di tipo bancario"
                              title = tit-err
                              icon mb-warning-icon
                      move spaces to lab-banca-buf
                   else
                      if ef-cab-buf not = spaces
                         set errori to true
                         move 78-ID-ef-abi to control-id
                         display message"Coordinate bancarie incomplete"
                                 title = tit-err
                                 icon mb-warning-icon
                      else
                         move spaces to lab-banca-buf
                      end-if
                   end-if
                else
                   if ef-cab-buf = spaces
                      perform VERIFICA-PAGAMENTO-BANCARIO
                      if PagamentoBancario
                         set errori to true
                         move 78-ID-ef-abi to control-id
                         display message "Codice CAB obbligatorio per "
                                         "pagamento di tipo bancario"
                                 title = tit-err
                                 icon mb-warning-icon
                         move spaces to lab-banca-buf
                      else
                         set errori to true
                         move 78-ID-ef-abi to control-id
                         display message"Coordinate bancarie incomplete"
                                 title = tit-err
                                 icon mb-warning-icon
                         move spaces to lab-banca-buf
                      end-if
                   else
                      move "ABI" to nome-file
                      perform RELAZIONI-CLIENTI
                      if not trovato
                         set errori to true
                         move 78-ID-ef-abi to control-id
                         display message"Coordinate bancarie NON valide"
                                 title = tit-err
                                 icon mb-warning-icon
                         move spaces to lab-banca-buf
                      end-if
                   end-if  
                end-if
                display lab-banca

           |---> Inizio controlli sul file dei DESTINI
           |78-ID-ef-ragsoc-1-d è l'ID del campo ef-ragsoc-1-d
           when 78-ID-ef-ragsoc-1-d
LUBEXX          if tot-righe > 1 or riga-nuova = 1
                   inquire ef-ragsoc-1-d, value in ef-ragsoc-1-d-buf
                   if ef-ragsoc-1-d-buf = spaces 
                      move ef-ragsoc-1-buf  to ef-ragsoc-1-d-buf
                      modify ef-ragsoc-1-d value ef-ragsoc-1-d-buf
      *                set errori to true  
      *                move 78-ID-ef-ragsoc-1-d to control-id
      *                display message box "Inserimento Ragione Sociale destinatario mancante"
      *                        title = tit-err
      *                        icon mb-warning-icon
                   end-if
LUBEXX          end-if

           |78-ID-ef-indirizzo-d è l'ID del campo ef-indirizzo-d
           when 78-ID-ef-indirizzo-d
                if tot-righe > 1 or riga-nuova = 1
                   inquire ef-indirizzo-d, value in ef-indirizzo-d-buf
                   if ef-indirizzo-d-buf = spaces 
                      set errori to true  
                      move 78-ID-ef-indirizzo-d to control-id
                      display message "Inserimento Indirizzo "
                                      "destino mancante"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if  

           |78-ID-ef-indirizzo-d è l'ID del campo ef-indirizzo-d
           when 78-ID-ef-indirizzo-d
                if tot-righe > 1 or riga-nuova = 1
                   inquire ef-indirizzo-d, value in ef-indirizzo-d-buf
                   if ef-indirizzo-d-buf = spaces 
                      set errori to true  
                      move 78-ID-ef-indirizzo-d to control-id
                      display message box "Inserimento indirizzo "
                                          "destinatario mancante"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if

           |78-ID-ef-cap-d è l'ID del campo ef-cap-d
           when 78-ID-ef-cap-d
                if tot-righe > 1 or riga-nuova = 1
                   inquire ef-cap-d, value in ef-cap-d-buf
                   if ef-cap-d-buf = spaces 
                      set errori to true  
                      move 78-ID-ef-cap-d to control-id
                      display message box "Inserimento CAP "
                                          "destinatario mancante"
                              title = tit-err
                              icon mb-warning-icon   
                   else   
                      if ef-prov-d-buf = "EE"
                         set trovato to true
                      else
                         move "anacap" to nome-file
                         perform RELAZIONI-DESTINI
                         if not trovato
                            set errori to true
                            move 78-ID-ef-cap-d to control-id
                            display message"CAP destinatario NON valido"
                                      title tit-err
                                       icon mb-warning-icon
                         end-if
                      end-if
                   end-if
                end-if  

           |78-ID-ef-localita-d è l'ID del campo ef-localita-d
           when 78-ID-ef-localita-d
                if tot-righe > 1 or riga-nuova = 1
                   inquire ef-localita-d, value in ef-localita-d-buf
                   if ef-localita-d-buf = spaces 
                      set errori to true  
                      move 78-ID-ef-localita-d to control-id
                      display message box "Inserimento località "
                                          "destinatario mancante"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if

           |78-ID-ef-prov-d è l'ID del campo ef-prov-d
           when 78-ID-ef-prov-d                
                if tot-righe > 1 or riga-nuova = 1
                   move "tprov" to nome-file
                   perform RELAZIONI-DESTINI
                   if not trovato
                      set errori to true
                      move 78-ID-ef-prov-d to control-id
                      display message box "Provincia destino NON valida"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if

           |78-ID-ef-nazione-d è l'ID del campo ef-nazione-d
           when 78-ID-ef-nazione-d
                if tot-righe > 1 or riga-nuova = 1
                   move "tnazioni" to nome-file
                   perform RELAZIONI-DESTINI
                   if not trovato
                      set errori to true
                      move 78-ID-ef-nazione-d to control-id
                      display message box "Nazione destino NON valida"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if        
           |78-ID-ef-vettore-d è l'ID del campo ef-vettore-d
           when 78-ID-ef-vettore-d
                if tot-righe > 1 or riga-nuova = 1
                   move "tvettori" to nome-file
                   perform RELAZIONI-DESTINI
                   if vet-codice not = ZERO
                      if not trovato
                         set errori to true  
                         move 78-ID-ef-vettore-d to control-id
                        display message box "Vettore destino NON valido"
                                 title = tit-err
                                 icon mb-warning-icon
                      end-if
                   end-if
                end-if    

           when 78-ID-ef-pag-d
                if tot-righe > 1 or riga-nuova = 1
                   move "tcodpag" to nome-file
                   perform RELAZIONI-DESTINI
                   if tblpa-codice2 not = spaces
                      if not trovato
                         set errori to true  
                         move 78-ID-ef-pag-d to control-id
                         display message "Pagamento destino NON valido"
                                 title = tit-err
                                 icon mb-warning-icon
                      end-if
                   end-if
                end-if    
                   
           |78-ID-cbo-stato-d è l'ID del campo cbo-stato-d
           when 78-ID-cbo-stato-d
                perform SCARICA-COMBO-DESTINI
                move stato-destini   to  desf-stato
                if desf-stato not = old-desf-stato
                   move 1 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"

                   if not Passwd-StatusOk

                      move old-desf-stato to stato-destini
                      perform CARICA-COMBO-DESTINI
                   else
                      if disattivo-d
                         move control-id to store-id
                         move "destini" to nome-file
                         perform CHANGE-STATUS
                         move store-id to control-id
                      end-if
                   end-if
               end-if

           |78-ID-ef-note-data è l'ID del control ef-note-data
           when 78-ID-ef-note-data                             
                inquire ef-note-1,    value in ef-note-1-buf
                inquire ef-note-data, value in ef-note-data-buf
                move ef-note-data-buf to como-data
                if como-data = 0
                   if ef-note-1-buf not = spaces
                      perform DATE-FORMAT
                      move como-data to ef-note-data-buf
                   end-if
                else
                   perform DATE-FORMAT
                   move como-data to ef-note-data-buf
                end-if
                display ef-note-data
           when 78-ID-ef-perce-premi-d
                inquire ef-perce-premi-d, 
                                value in desf-perce-premi-fine-anno
                if desf-perce-premi-fine-anno > 100
                   set errori to true  
                   move 78-ID-ef-perce-premi-d to control-id
                   display message box "Percentuale errata!"
                           title = tit-err
                           icon mb-warning-icon
                end-if
           end-evaluate.

           if errori
              perform CANCELLA-COLORE
              move CONTROL-ID to store-id
              move 4          to ACCEPT-CONTROL
           end-if.


      ***---
       CONTROLLO-OLD-RIGA.
           set SiSalvato to true.
           if mod-destini = 0 exit paragraph end-if.
           perform PAGE-2-BUF-TO-FLD.
           move 0 to scelta.

           if desf-ragsoc-1 not = old-desf-ragsoc-1 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-ragsoc-1-d è l'ID del campo ef-ragsoc-1-d
              move 78-ID-ef-ragsoc-1-d to store-id 
           end-if.

           if desf-ragsoc-2 not = old-desf-ragsoc-2 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-ragsoc-2-d è l'ID del campo ef-ragsoc-2-d
              move 78-ID-ef-ragsoc-2-d to store-id 
           end-if.

           if desf-indirizzo not = old-desf-indirizzo and SiSalvato
              set NoSalvato to true
              |78-ID-ef-indirizzo-d è l'ID del campo ef-indirizzo-d
              move 78-ID-ef-indirizzo-d to store-id 
           end-if.

           if desf-cap not = old-desf-cap and SiSalvato
              set NoSalvato to true
              |78-ID-ef-cap-d è l'ID del campo ef-cap-d
              move 78-ID-ef-cap-d to store-id 
           end-if.

           if desf-localita not = old-desf-localita and SiSalvato
              set NoSalvato to true
              |78-ID-ef-localita-d è l'ID del campo ef-localita-d
              move 78-ID-ef-localita-d to store-id 
           end-if.

           if desf-prov not = old-desf-prov and SiSalvato
              set NoSalvato to true
              |78-ID-ef-prov-d è l'ID del campo ef-prov-d
              move 78-ID-ef-prov-d to store-id 
           end-if.

           if desf-nazione not = old-desf-nazione and SiSalvato
              set NoSalvato to true
              |78-ID-ef-nazione-d è l'ID del campo ef-nazione-d
              move 78-ID-ef-nazione-d to store-id 
           end-if.

           if desf-telef-1 not = old-desf-telef-1 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-telef-1-d è l'ID del campo ef-telef-1-d
              move 78-ID-ef-telef-1-d to store-id 
           end-if.

           if desf-telef-2 not = old-desf-telef-2 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-telef-2-d è l'ID del campo ef-telef-2-d
              move 78-ID-ef-telef-2-d to store-id 
           end-if.

           if desf-fax not = old-desf-fax and SiSalvato
              set NoSalvato to true
              |78-ID-ef-fax-d è l'ID del campo ef-fax-d
              move 78-ID-ef-fax-d to store-id 
           end-if.

           if desf-ufficio not = old-desf-ufficio and SiSalvato
              set NoSalvato to true
              move 78-ID-cbo-ufficio-d to store-id 
           end-if.

           if desf-mail not = old-desf-mail and SiSalvato
              set NoSalvato to true
              |78-ID-ef-mail-d è l'ID del campo ef-mail-d
              move 78-ID-ef-mail-d to store-id 
           end-if.

           if desf-referente not = old-desf-referente and SiSalvato
              set NoSalvato to true
              |78-ID-ef-referente-d è l'ID del campo ef-referente-d
              move 78-ID-ef-referente-d to store-id 
           end-if.

           if desf-vettore not = old-desf-vettore and SiSalvato
              set NoSalvato to true
              |78-ID-ef-vettore-d è l'ID del campo ef-vettore-d
              move 78-ID-ef-vettore-d to store-id 
           end-if.

           if desf-pag not = old-desf-pag and SiSalvato
              set NoSalvato to true
              |78-ID-ef-pag-d è l'ID del campo ef-pag-d
              move 78-ID-ef-pag-d to store-id 
           end-if.  
           
           if desf-referente-ord not = old-desf-referente-ord 
                                                        and SiSalvato
              set NoSalvato to true
              move 78-ID-ef-ref-ord-d to store-id 
           end-if.  
           
           if desf-tel-dir-ref-ord not = old-desf-tel-dir-ref-ord 
                                                        and SiSalvato
              set NoSalvato to true
              move 78-ID-ef-tel-ord-d to store-id 
           end-if.  
           
           if desf-mail-ref-ord not = old-desf-mail-ref-ord 
                                                        and SiSalvato
              set NoSalvato to true
              move 78-ID-ef-mail-ord-d to store-id 
           end-if.  

           if desf-mail-ref-ord-cc not = old-desf-mail-ref-ord-cc 
                                                        and SiSalvato
              set NoSalvato to true
              move 78-ID-ef-mail-ord-cc to store-id 
           end-if.  

           if desf-perce-premi-fine-anno 
                    not = old-desf-perce-premi-fine-anno and SiSalvato
              set NoSalvato to true
              move 78-ID-ef-perce-premi-d to store-id 
           end-if.  

           if notf-note-1 of notef1 not = old-notf-note-1 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-1 è l'ID del campo 78-ID-ef-note-1
              move 78-ID-ef-note-1 to store-id 
           end-if.      

           if notf-data of notef1 not = old-notf-data and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-data è l'ID del campo 78-ID-ef-note-data
              move 78-ID-ef-note-data to store-id 
           end-if.

           if notf-note-2 of notef1 not = old-notf-note-2 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-2 è l'ID del campo 78-ID-ef-note-2
              move 78-ID-ef-note-2 to store-id 
           end-if.

           if notf-note-3 of notef1 not = old-notf-note-3 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-3 è l'ID del campo 78-ID-ef-note-3
              move 78-ID-ef-note-3 to store-id 
           end-if.

           if notf-note-4 of notef1 not = old-notf-note-4 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-4 è l'ID del campo 78-ID-ef-note-4
              move 78-ID-ef-note-4 to store-id 
           end-if.


      ***---
       CURRENT-RECORD.
           perform RIEMPI-CHIAVE.
           set tutto-ok   to true.
           set ReadSecca  to true.
           set cli-tipo-F to true.
           if mod = 1     
              read clienti    lock invalid set errori to true end-read
           else
              read clienti no lock invalid set errori to true end-read
           end-if.                    
           set ReadSecca to false.

           if RecLocked
              set RecLocked to false
              set errori    to true
           else
              if tutto-ok
                 if nuovo
                    move 0 to mod-campi
                    move 0 to mod           
                    move 1 to mod-k         
                    move 78-ID-ef-codice to control-id    
                    move 4 to accept-control
                 end-if   
                 perform FORM1-CURR
                 set vecchio to true
                 if mod = 1
                    set StatusModifica to true
                 else                         
                    set StatusVisua    to true
                 end-if
                 perform STATUS-BAR-MSG
              else
                 move 0 to mod
                 move 1 to mod-k
                 if vecchio
                    perform CLEAR-SCREEN
                    set errori to true
                    if YesMessage    
                       move 78-ID-ef-codice to control-id    
                       move 4 to accept-control
                       display message box MSG-Record-inesistente
                               title = tit-err
                               icon mb-warning-icon
                    end-if
                 end-if
              end-if
           end-if.


      ***---
       DISPLAY-CAMPI-GRID.
           display ef-ragsoc-1-d  ef-ragsoc-2-d
                   ef-indirizzo-d ef-cap-d
                   ef-localita-d  ef-prov-d
                   ef-nazione-d   ef-telef-1-d
                   ef-telef-2-d   ef-fax-d cbo-ufficio-d
                   ef-mail-d      ef-mail-ord-cc
                   ef-referente-d ef-pag-d
                   ef-vettore-d   ef-ref-ord-d
                   ef-tel-ord-d   ef-mail-ord-d
                   ef-perce-premi-d chk-netto-PFA-d
                   ef-gg chk-saldi-d chk-immediata-d chk-invio-sol.


      ***---
       DISPLAY-CAMPI-NOTE.
           display ef-note-1 
                   ef-note-data 
                   ef-note-2 
                   ef-note-3 
                   ef-note-4.

                  
      ***---
       DISPLAY-SCREEN.
           display ef-codice, ef-ragsoc-1, ef-ragsoc-2, 
                   ef-indirizzo, ef-cap, ef-localita, 
                   ef-prov, ef-nazione, ef-tel-1, 
                   ef-tel-2, ef-fax, ef-mail, ef-url, 
                   ef-referente, ef-vettore,
                   cbo-stato, ef-note, ef-note-agg, 
                   pb-note, ef-codfis, ef-piva, ef-iva, 
                   ef-pag, ef-cab, ef-abi, pb-nota-pie,
                   lab-vettore, lab-nazione, lab-citta, lab-gdo,
                   lab-regione, TOOL-ESCI, TOOL-NUOVO, TOOL-CANCELLA, 
                   TOOL-SALVA, TOOL-ANTEPRIMA, TOOL-MODIFICA,
                   TOOL-STAMPA, TOOL-CERCA, TOOL-SELEZIONA, Form1-Pb-1a, 
                   Form1-Pb-1b, Form1-Pb-1c, Form1-Pb-1d, TOOL-ORD.
           perform DISPLAY-CAMPI-GRID.
           perform DISPLAY-CAMPI-NOTE.


      ***---
       INIT.
           move 0 to StatusHelp.
           move 0 to LastPrg.
           move 0 to WrkData.
           move 1 to mod-k.
           set StatusVisua      to true.  
           set DestinoCambiato  to false.
           set NoteCambiate     to false.

           |Riempio il tabellino contenente i riferimenti agli ID
           |relativi ad ogni pagina del tab-control
           |specifico per i pgm. aventi Tab-Control
           move 78-ID-ef-ragsoc-1       to min-id(1).
           move 78-ID-ef-cab            to max-id(1).
           move 78-ID-form1-gd-1        to min-id(2).
           move 78-ID-ef-note-4         to max-id(2).
           |*******

           perform RIEMPI-COMBO-STATO.
           perform RIEMPI-COMBO-DESTINI.
           perform RIEMPI-COMBO-UFFICIO-D.

           move "Attivo"      to cbo-stato-buf.
           move "Attivo"      to cbo-stato-d-buf.
           move "Non Gestito" to cbo-ufficio-d-buf.
           Modify  cbo-stato,     value cbo-stato-buf.
           Modify  cbo-stato-d,   value cbo-stato-d-buf.
           Modify  cbo-ufficio-d, value cbo-ufficio-d-buf.

           move      0 to old-notf-data.
           move spaces to old-notf-note-1
                          old-notf-note-2
                          old-notf-note-3
                          old-notf-note-4.


      ***---
       INIT-OLD-REC.
           initialize old-cli-rec  
                      old-desf-rec
                      old-notf-rec
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           Set Attivo       to true

           move stato       to old-cli-stato.
                               
           move "N" to old-cli-utf.   
           move "N" to old-cli-spost-ric-agosto.
           move "N" to old-cli-spost-ric-dicembre.
           move "A" to old-desf-stato.


      ***---
       INTESTAZIONE.
           modify form1-gd-1(1, 1), cell-data = "Numero".
           modify form1-gd-1(1, 2), cell-data = "Ragione Sociale".
           modify form1-gd-1(1, 3), cell-data = "Indirizzo".
           modify form1-gd-1(1, 4), cell-data = "CAP".
           modify form1-gd-1(1, 5), cell-data = "Località".
           modify form1-gd-1(1, 6), cell-data = "Provincia".


      ***---
       LOAD-RECORD.
           move cli-codice to desf-codice.
           move low-value  to desf-prog.
           perform RESET-GRIGLIA.

           set tutto-ok to true.
           start destinif key is >= desf-chiave 
              invalid 
                 continue
              not invalid
                 modify form1-gd-1, mass-update = 1
                 perform varying riga from 2 by 1 until 1 = 2
                    read destinif next no lock 
                       at end 
                          exit perform 
                    end-read
                    if desf-codice not = cli-codice exit perform end-if
                    perform MOVE-NOTE
                    move desf-prog      to col-prog LastPrg
                    move desf-ragsoc-1  to col-ragsoc
                    move desf-indirizzo to col-indirizzo
                    move desf-localita  to col-localita
                    move desf-cap       to col-cap
                    move desf-prov      to col-prov                     
                    modify form1-gd-1(riga, 1), cell-data col-prog
                    modify form1-gd-1(riga, 2), cell-data col-ragsoc   
                    modify form1-gd-1(riga, 3), cell-data col-indirizzo
                    modify form1-gd-1(riga, 4), cell-data col-cap
                    modify form1-gd-1(riga, 5), cell-data col-localita      
                    modify form1-gd-1(riga, 6), cell-data col-prov
                                                    
                    move desf-ragsoc-2         to hidden-ragsoc-2 
                    move desf-nazione          to hidden-nazione  
                    move desf-telef-1          to hidden-telef-1  
                    move desf-telef-2          to hidden-telef-2  
                    move desf-fax              to hidden-fax      
                    move desf-ufficio          to hidden-ufficio-d
                    move desf-mail             to hidden-mail     
                    move desf-referente        to hidden-referente
                    move desf-vettore          to hidden-vettore
                    move desf-pag              to hidden-pag
                    move desf-gg-consegna      to hidden-gg-consegna

                    move desf-referente-ord    to hidden-referente-ord        
                    move desf-tel-dir-ref-ord  to hidden-tel-dir-ref-ord      
                    move desf-mail-ref-ord     to hidden-mail-ref-ord         
                    move desf-mail-ref-ord-cc  to hidden-mail-ref-ord-cc
                    move desf-perce-premi-fine-anno      
                                         to hidden-perce-premi-fine-anno
                    if desf-premio-netto-si
                       move 1 to hidden-netto-PFA
                    else
                       move 0 to hidden-netto-PFA
                    end-if

                    if desf-saldi-si
                       move 1 to hidden-saldi-d
                    else
                       move 0 to hidden-saldi-d
                    end-if              

                    if desf-ev-immediata-si
                       move 1 to hidden-ei-d
                    else
                       move 0 to hidden-ei-d
                    end-if

                    if desf-invio-sol-si
                       move 1 to hidden-invio-sol-d
                    else
                       move 0 to hidden-invio-sol-d
                    end-if

                    move desf-stato     to hidden-stato
                 
                    modify form1-gd-1(riga, 7)  
                           hidden-data hidden-ragsoc-2
                    modify form1-gd-1(riga, 8)  
                           hidden-data hidden-nazione
                    modify form1-gd-1(riga, 9)  
                           hidden-data hidden-telef-1
                    modify form1-gd-1(riga, 10) 
                           hidden-data hidden-telef-2
                    modify form1-gd-1(riga, 11) 
                           hidden-data hidden-fax
                    modify form1-gd-1(riga, 12) 
                           hidden-data hidden-mail
                    modify form1-gd-1(riga, 13) 
                           hidden-data hidden-referente
                    modify form1-gd-1(riga, 14) 
                           hidden-data hidden-vettore
                    modify form1-gd-1(riga, 15) 
                           hidden-data hidden-pag
                    modify form1-gd-1(riga, 17) 
                           hidden-data hidden-stato
                    modify form1-gd-1(riga, 18) 
                           hidden-data hidden-note-1   
                    modify form1-gd-1(riga, 19) 
                           hidden-data hidden-data-note
                    modify form1-gd-1(riga, 20) 
                           hidden-data hidden-note-2   
                    modify form1-gd-1(riga, 21) 
                           hidden-data hidden-note-3   
                    modify form1-gd-1(riga, 22) 
                           hidden-data hidden-note-4
                    modify form1-gd-1(riga, 23) 
                           hidden-data hidden-referente-ord
                    modify form1-gd-1(riga, 24) 
                           hidden-data hidden-tel-dir-ref-ord
                    modify form1-gd-1(riga, 25) 
                           hidden-data hidden-mail-ref-ord
                    modify form1-gd-1(riga, 26) 
                           hidden-data hidden-perce-premi-fine-anno
                    modify form1-gd-1(riga, 27) 
                           hidden-data hidden-gg-consegna
      *              modify form1-gd-1(riga, 28) 
      *                     hidden-data hidden-ult-ord
                    modify form1-gd-1(riga, 28) 
                           hidden-data hidden-mail-ref-ord-cc
                    modify form1-gd-1(riga, 29) 
                           hidden-data hidden-netto-PFA
                    modify form1-gd-1(riga, 30) 
                           hidden-data hidden-saldi-d
                    modify form1-gd-1(riga, 31) 
                           hidden-data hidden-ufficio-d
                    modify form1-gd-1(riga, 32) 
                           hidden-data hidden-ei-d
                    modify form1-gd-1(riga, 33) 
                           hidden-data hidden-invio-sol-d

              end-perform   
             
              modify form1-gd-1, mass-update = 0 

              move 2 to riga
              perform VALORE-RIGA
              perform ROW-TO-ENTRY
              perform COLORE
           end-start. 


      ***---
       LOOP-DELETE-DESTINI.
           move cli-codice to desf-codice.
           move low-value  to desf-prog.

           start destinif key is >= desf-chiave
                 invalid  continue
             not invalid
                 initialize tab-dati-creazione
                 perform varying idx from 1 by 1
                           until 1 = 2
                    read destinif next no lock 
                         at end exit perform 
                    end-read
                    move desf-data-creazione   to el-tab-data(idx)             
                    move desf-ora-creazione    to el-tab-ora(idx)
                    move desf-utente-creazione to el-tab-utente(idx)
                    if desf-codice not = cli-codice 
                       exit perform 
                    end-if
                    delete destinif record invalid continue end-delete
                    move desf-codice to notf-codice of notef1
                    move desf-prog   to notf-prog   of notef1
                    delete notef1 record 
                       invalid 
                          continue 
                    end-delete
                 end-perform
           end-start.

      ***---
       LOOP-SCRIVI-DESTINI.
           move cli-codice to desf-codice.
           perform X-Y.

           move 0 to desf-prog.
           move riga to store-riga.
           perform varying riga from 2 by 1
                     until riga > tot-righe
              perform VALORE-RIGA
              accept como-ora from time
              if nuovo
                 move data-oggi to desf-data-creazione
                 move como-ora  to desf-ora-creazione 
                 move user-codi to desf-utente-creazione
                 move data-oggi to notf-data-creazione    of notef1
                 move como-ora  to notf-ora-creazione     of notef1
                 move user-codi to notf-utente-creazione  of notef1
              else
                 subtract 1 from riga giving idx
                 move el-tab-data(idx)   to desf-data-creazione  
                 move el-tab-ora(idx)    to desf-ora-creazione   
                 move el-tab-utente(idx) to desf-utente-creazione
                    
                 move data-oggi to desf-data-ultima-modifica
                 move como-ora  to desf-ora-ultima-modifica
                 move user-codi to desf-utente-ultima-modifica
                 move data-oggi to notf-data-ultima-modifica    of notef1
                 move como-ora  to notf-ora-ultima-modifica     of notef1
                 move user-codi to notf-utente-ultima-modifica  of notef1

              end-if
              perform MOVE-DATI       
              perform PAGE-2-BUF-TO-FLD
              write desf-rec invalid rewrite desf-rec end-write
              perform CHECK-ESISTENZA-NOTE
              if ExistRecord        
                 move desf-codice to notf-codice of notef1
                 write notf-rec of notef1 
                       invalid rewrite notf-rec of notef1
                 end-write
              end-if
           end-perform.
           move store-riga to riga.

           |rifaccio il posizionamento della riga
           |su cui mi trovo dei campi a video
           if tot-righe > 2
              perform VALORE-RIGA
              perform ROW-TO-ENTRY
           end-if.

      ***---
       CHECK-ESISTENZA-NOTE.
           set ExistRecord to false.
           if hidden-note-1    not = spaces or
              hidden-data-note not =      0 or
              hidden-note-2    not = spaces or
              hidden-note-3    not = spaces or
              hidden-note-4    not = spaces
              set ExistRecord to true
           end-if.


      ***---
       MODIFICA.
           move 5 to key-status.
           inquire tool-modifica, value in mod.
           set tutto-ok to true..
           inquire form1-gd-1, cursor-y in store-riga.

           if nuovo
              move 1 to mod                  
              modify tool-modifica, value mod
           else                         
      *  se l'utente e' abilitato puo' modificare un record
              if mod = 1
                 set YesMessage to true
                 perform CURRENT-RECORD
                 if tutto-ok
                    inquire Screen1-Ta-1, value in idx
                    if control-id < min-id(idx) or
                       control-id > max-id(idx)
                       move min-id(idx) to control-id
                    end-if
                    move 0 to mod-k
                    move 1 to mod
                    set StatusModifica to true
                    perform STATUS-BAR-MSG
                 end-if
              else
                 move 1 to mod
                 perform SALV-MOD            
                 move 0 to mod
                 move 1 to mod-k
                 if errori
                    move 1 to mod
                    move 0 to mod-k
                 else
                    move 78-ID-ef-codice to control-id
                    set NoMessage to true
                    perform CURRENT-RECORD
                    set StatusVisua to true
                    perform STATUS-BAR-MSG 
                    unlock clienti all records
                    if KeyboardSaved
                       set KeyboardReleased to true
                       set environment "KEYSTROKE" to "DATA=44 44"
                       set environment "KEYSTROKE" to "DATA=46 46"
                    end-if
                    move 78-ID-ef-codice to control-id
                 end-if
              end-if
                        
              perform DISPLAY-SCREEN
              modify tool-modifica,  value mod
              perform CANCELLA-COLORE

              move 4 to accept-control
              move 0 to StatusHelp
              perform STATUS-HELP
           end-if.
           move store-riga to riga.
           modify form1-gd-1, cursor-y = riga.
           perform VALORE-RIGA.
           perform ROW-TO-ENTRY.
           perform COLORE.


      ***---
       MOVE-CAMPI.
           move ef-ragsoc-1-d-buf  to col-ragsoc.
           move ef-ragsoc-2-d-buf  to hidden-ragsoc-2.
           move ef-indirizzo-d-buf to col-indirizzo.
           move ef-cap-d-buf       to col-cap.
           move ef-localita-d-buf  to col-localita.
           move ef-prov-d-buf      to col-prov.
           move ef-nazione-d-buf   to hidden-nazione.
           move ef-telef-1-d-buf   to hidden-telef-1.
           move ef-telef-2-d-buf   to hidden-telef-2.
           move ef-fax-d-buf       to hidden-fax.
           move cbo-ufficio-d-buf  to hidden-ufficio-d.
           move ef-mail-d-buf      to hidden-mail.
           move ef-referente-d-buf to hidden-referente.
           move ef-vettore-d-buf   to hidden-vettore.
           move ef-pag-d-buf       to hidden-pag.
           move ef-gg-buf          to hidden-gg-consegna

           if riga-nuova = 1
              add 1         to LastPrg
              move LastPrg  to col-prog desf-prog
           else
              move desf-prog to col-prog
           end-if.

           perform SCARICA-COMBO-DESTINI.
           move stato-destini      to hidden-stato.

           perform SCARICA-COMBO-UFFICIO-D.
           move ufficio-d  to hidden-ufficio-d.

           if hidden-stato = "D" |Disattivo
              move 0 to mod-destini
           else
              move 1 to mod-destini
           end-if.

           move ef-note-1-buf      to hidden-note-1.

           move ef-note-data-buf   to como-data.
           perform DATE-TO-FILE.
           move como-data          to hidden-data-note.

           move ef-note-2-buf        to hidden-note-2.
           move ef-note-3-buf        to hidden-note-3.
           move ef-note-4-buf        to hidden-note-4.
                                                           
           move ef-ref-ord-d-BUF     to hidden-referente-ord        
           move ef-tel-ord-d-BUF     to hidden-tel-dir-ref-ord      
           move ef-mail-ord-d-BUF    to hidden-mail-ref-ord         
           move ef-mail-ord-cc-BUF   to hidden-mail-ref-ord-cc
           move ef-perce-premi-d-BUF to hidden-perce-premi-fine-anno
           move chk-netto-PFA-d-buf  to hidden-netto-PFA.
           move chk-saldi-d-buf      to hidden-saldi-d.
           move chk-immediata-d-buf  to hidden-ei-d.
           move chk-invio-sol-buf    to hidden-invio-sol-d.

           modify form1-gd-1(riga, 1)  cell-data col-prog.
           modify form1-gd-1(riga, 2)  cell-data col-ragsoc.
           modify form1-gd-1(riga, 3)  cell-data col-indirizzo.
           modify form1-gd-1(riga, 4)  cell-data col-cap.   
           modify form1-gd-1(riga, 5)  cell-data col-localita.
           modify form1-gd-1(riga, 6)  cell-data col-prov.  
           modify form1-gd-1(riga, 7)  hidden-data hidden-ragsoc-2.
           modify form1-gd-1(riga, 8)  hidden-data hidden-nazione.
           modify form1-gd-1(riga, 9)  hidden-data hidden-telef-1.
           modify form1-gd-1(riga, 10) hidden-data hidden-telef-2.
           modify form1-gd-1(riga, 11) hidden-data hidden-fax.
           modify form1-gd-1(riga, 12) hidden-data hidden-mail.
           modify form1-gd-1(riga, 13) hidden-data hidden-referente.
           modify form1-gd-1(riga, 14) hidden-data hidden-vettore.
           modify form1-gd-1(riga, 15) hidden-data hidden-pag.
           modify form1-gd-1(riga, 17) hidden-data hidden-stato.
           modify form1-gd-1(riga, 18) hidden-data hidden-note-1.
           modify form1-gd-1(riga, 19) hidden-data hidden-data-note.
           modify form1-gd-1(riga, 20) hidden-data hidden-note-2.
           modify form1-gd-1(riga, 21) hidden-data hidden-note-3.
           modify form1-gd-1(riga, 22) hidden-data hidden-note-4.

           modify form1-gd-1(riga, 23) hidden-data hidden-referente-ord.
           modify form1-gd-1(riga, 24) 
                                      hidden-data hidden-tel-dir-ref-ord.
           modify form1-gd-1(riga, 25) hidden-data hidden-mail-ref-ord.
           modify form1-gd-1(riga, 26) 
                                hidden-data hidden-perce-premi-fine-anno.
           modify form1-gd-1(riga, 27) hidden-data hidden-gg-consegna.
      *     modify form1-gd-1(riga, 28) hidden-data hidden-ult-ord.
           modify form1-gd-1(riga, 28) 
                                   hidden-data hidden-mail-ref-ord-cc. 
           modify form1-gd-1(riga, 29) hidden-data hidden-netto-PFA.
           modify form1-gd-1(riga, 30) hidden-data hidden-saldi-d.
           modify form1-gd-1(riga, 31) hidden-data hidden-ufficio-d.
           modify form1-gd-1(riga, 32) hidden-data hidden-ei-d.
           modify form1-gd-1(riga, 33) hidden-data hidden-invio-sol-d.

           move 0 to riga-nuova.


      ***---
       MOVE-DATI.
           move col-ragsoc         to ef-ragsoc-1-d-buf.
           move hidden-ragsoc-2    to ef-ragsoc-2-d-buf.
           move col-indirizzo      to ef-indirizzo-d-buf.
           move col-cap            to ef-cap-d-buf.
           move col-localita       to ef-localita-d-buf.
           move col-prov           to ef-prov-d-buf.
           move hidden-nazione     to ef-nazione-d-buf.
           move hidden-telef-1     to ef-telef-1-d-buf.
           move hidden-telef-2     to ef-telef-2-d-buf.
           move hidden-fax         to ef-fax-d-buf.
           move hidden-ufficio-d   to cbo-ufficio-d-buf.
           move hidden-mail        to ef-mail-d-buf.
           move hidden-referente   to ef-referente-d-buf.

           move hidden-vettore     to ef-vettore-d-buf.
           move hidden-pag         to ef-pag-d-buf.
           move hidden-gg-consegna to ef-gg-buf
           move col-prog           to desf-prog.

           move hidden-referente-ord         to ef-ref-ord-d-BUF.
           move hidden-tel-dir-ref-ord       to ef-tel-ord-d-BUF.
           move hidden-mail-ref-ord          to ef-mail-ord-d-BUF.
           move hidden-mail-ref-ord-cc       to ef-mail-ord-cc-BUF.
           move hidden-perce-premi-fine-anno to ef-perce-premi-d-BUF.
           move hidden-netto-PFA             to chk-netto-PFA-d-buf.
           move hidden-saldi-d               to chk-saldi-d-buf.
           move hidden-ei-d                  to chk-immediata-d-buf.
           move hidden-invio-sol-d           to chk-invio-sol-buf.

           move hidden-stato to stato-destini.
           perform CARICA-COMBO-DESTINI.
           if mod-campi = 1
              evaluate true
              when bloccato-d  move 1 to mod-destini
              when attivo-d    move 1 to mod-destini
              when disattivo-d move 0 to mod-destini
              end-evaluate
           end-if.
           
           move hidden-ufficio-d to ufficio-d.
           perform CARICA-COMBO-UFFICIO-D.
                  
           move hidden-data-note to como-data.
           perform DATE-TO-SCREEN.
           move como-data      to  ef-note-data-buf.

           move hidden-note-1  to  ef-note-1-buf.
           move hidden-note-2  to  ef-note-2-buf.
           move hidden-note-3  to  ef-note-3-buf.
           move hidden-note-4  to  ef-note-4-buf.
                              
           move 0 to riga-nuova.

           move "tprov"    to nome-file.
           perform RELAZIONI-DESTINI.
           move "tnazioni" to nome-file.
           perform RELAZIONI-DESTINI.
           move "tvettori" to nome-file.
           perform RELAZIONI-DESTINI.
           move "tcodpag" to nome-file.
           perform RELAZIONI-DESTINI.


      ***---
       MOVE-NOTE.
           move desf-codice    to notf-codice of notef1
           move desf-prog      to notf-prog   of notef1
           read notef1 
                invalid move spaces   to hidden-note-1 
                        move 0        to hidden-data-note
                        move spaces   to hidden-note-2
                        move spaces   to hidden-note-3
                        move spaces   to hidden-note-4
            not invalid move notf-note-1 of notef1 to hidden-note-1
                        move notf-data   of notef1 to hidden-data-note
                        move notf-note-2 of notef1 to hidden-note-2
                        move notf-note-3 of notef1 to hidden-note-3
                        move notf-note-4 of notef1 to hidden-note-4
           end-read.


      ***---
       NUOVO.
           perform SALV-MOD.

           if tutto-ok           
              |specifico per i pgm. aventi Tab-Control
              move 1 to Screen1-Ta-1-TAB-VALUE
              perform SCREEN1-TA-1-TABCHANGE
              |******          
              move 78-ID-ef-codice to control-id
              move 4 to accept-control 
              move 1 to mod 
              move 0 to mod-k
              modify tool-modifica,  value = mod
              perform CANCELLA-COLORE
              perform FORM1-CLEAR

              Perform VALORIZZA-NUOVO
              perform INIT-OLD-REC
              
              |Specifico del pgm. gclienti
              move "Attivo"      to cbo-stato-buf
              Modify  cbo-stato, value cbo-stato-buf

              move 1 to mod-campi mod-destini
              perform DISPLAY-SCREEN

              set StatusIns to true
              perform STATUS-BAR-MSG 
              unlock clienti all records
           end-if.


      ***---
       ORDINAMENTO.
           inquire tool-ord, value in OrderBy.

           if OrderBy = zero
              perform DataSet1-CHANGETO-KEY1
           else
              perform DataSet1-CHANGETO-KEY2
           end-if.
           perform CURRENT-RECORD.


      ***---
       PAGE-2-BUF-TO-FLD.
           move ef-ragsoc-1-d-buf   to desf-ragsoc-1.
           move ef-ragsoc-2-d-buf   to desf-ragsoc-2.
           move ef-indirizzo-d-buf  to desf-indirizzo.
           move ef-cap-d-buf        to desf-cap.
           move ef-localita-d-buf   to desf-localita.
           move ef-prov-d-buf       to desf-prov.
           move ef-nazione-d-buf    to desf-nazione.
           move ef-telef-1-d-buf    to desf-telef-1.
           move ef-telef-2-d-buf    to desf-telef-2.
           move ef-fax-d-buf        to desf-fax.
           move cbo-ufficio-d-buf   to desf-ufficio.
           move ef-mail-d-buf       to desf-mail.
           move ef-referente-d-buf  to desf-referente.
           move ef-vettore-d-buf    to desf-vettore.
           move ef-pag-d-buf        to desf-pag.

           move ef-ref-ord-d-BUF      to desf-referente-ord        
           move ef-tel-ord-d-BUF      to desf-tel-dir-ref-ord      
           move ef-mail-ord-d-BUF     to desf-mail-ref-ord         
           move ef-mail-ord-cc-BUF    to desf-mail-ref-ord-cc

           move ef-perce-premi-d-BUF  to desf-perce-premi-fine-anno
           if chk-netto-PFA-d-buf = 1
              set desf-premio-netto-si to true
           else
              set desf-premio-netto-no to true
           end-if.
           if chk-saldi-d-buf = 1
              set desf-saldi-si to true
           else
              set desf-saldi-no to true
           end-if.                 
           if chk-immediata-d-buf = 1
              set desf-ev-immediata-si to true
           else
              set desf-ev-immediata-no to true
           end-if.
           if chk-invio-sol-buf = 1
              set desf-invio-sol-si to true
           else
              set desf-invio-sol-no to true
           end-if.
           move ef-gg-buf             to desf-gg-consegna

           perform SCARICA-COMBO-DESTINI.
           move stato-destini       to desf-stato.

           perform SCARICA-COMBO-UFFICIO-D.
           move ufficio-d           to desf-ufficio.

           move desf-prog           to notf-prog of notef1.

           move ef-note-data-buf to como-data.
           perform DATE-TO-FILE.
           move como-data        to notf-data of notef1.

           move ef-note-1-buf    to notf-note-1 of notef1.
           move ef-note-2-buf    to notf-note-2 of notef1.
           move ef-note-3-buf    to notf-note-3 of notef1.
           move ef-note-4-buf    to notf-note-4 of notef1.


      ***---
       PAGE-2-CLEAR.
           move spaces to lab-citta-d-buf   lab-regione-d-buf
                          lab-nazione-d-buf lab-vettore-d-buf .
           display lab-citta-d   lab-regione-d
                   lab-nazione-d lab-vettore-d.
           move 1 to riga-nuova.
           move 0 to ef-note-data-buf.
           initialize ef-ragsoc-1-d-buf  ef-ragsoc-2-d-buf
                      ef-indirizzo-d-buf ef-cap-d-buf
                      ef-localita-d-buf  ef-prov-d-buf
                      ef-nazione-d-buf   ef-telef-1-d-buf
                      ef-telef-2-d-buf   ef-fax-d-buf
                      ef-mail-d-buf      ef-referente-d-buf
                      ef-vettore-d-buf   cbo-ufficio-d-buf
                      ef-note-1-buf      ef-note-2-buf
                      ef-note-3-buf      ef-note-4-buf
                      old-desf-rec       ef-pag-d-buf
                      old-notf-rec
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.

           |Specifico del pgm. gclienti
           move "Attivo" to cbo-stato-d-buf.
           move "A"      to old-desf-stato.
      *     move "N"      to old-desf-inoltro.
           move "N"      to old-desf-superamento-500.
           Modify  cbo-stato-d,    value cbo-stato-d-buf.
           move 1 to mod-destini.
           perform DISPLAY-CAMPI-GRID.
           perform DISPLAY-CAMPI-NOTE.


      ***---
       PRECEDENTE.
           perform SALV-MOD.           
           if tutto-ok
              perform CHECK-INSERIMENTO
              unlock clienti all records

              if mod = 1
                 move 0 to mod-k
                 set dataset1-clienti-lock to true
              else              
                 move 1 to mod-k     
                 initialize dataset1-clienti-lock-flag
              end-if

              perform CANCELLA-COLORE
              perform FORM1-PREVIOUS

              if RecLocked
                 set RecLocked to false             
                 initialize dataset1-clienti-lock-flag
                 move 1 to mod-k
                 perform FORM1-PREVIOUS
              end-if 
           end-if.


      ***---
       PRIMO.
           perform SALV-MOD.           
           if tutto-ok     
              perform CHECK-INSERIMENTO
              unlock clienti all records

              if mod = 1
                 move ZERO to mod-k
                 set dataset1-clienti-lock to true
              else              
                 move 1 to mod-k     
                 initialize dataset1-clienti-lock-flag
              end-if

              perform CANCELLA-COLORE
              perform FORM1-FIRST

              if RecLocked
                 set RecLocked to false             
                 initialize dataset1-clienti-lock-flag
                 move 1 to mod-k
                 perform FORM1-FIRST
              end-if 
           end-if.


      ***---
       RELATIONS.
           set trovato to true.

           evaluate nome-file    
           when "anacap"     perform READ-ANACAP
           when "tvettori"   perform READ-TVETTORI
           when "tprov"      perform READ-TPROV
           when "tnazioni"   perform READ-TNAZIONI
           when "tivaese"    perform READ-TIVAESE
           when "tcodpag"    perform READ-TCODPAG
           when "ABI"        perform READ-ABI
           when "tgrupgdo"   perform READ-TGRUPGDO
           end-evaluate.    

      ***---
       READ-ANACAP.
           read anacap no lock
                invalid set trovato to false
           end-read. 

      ***---
       READ-TGRUPGDO.
           read tgrupgdo no lock
                invalid set trovato to false
           end-read. 

      ***---
       READ-TVETTORI.
           read tvettori no lock
                invalid  move spaces to vet-descrizione
                         set trovato to false
           end-read. 

      ***---
       READ-TPROV.         
           read tprov no lock
                invalid  move spaces to prv-descrizione
                                        reg-descrizione
                         set trovato to false 
            not invalid 
                move prv-regione     to reg-codice
                read tregioni no lock
                     invalid  move spaces to reg-descrizione
                end-read
           end-read.

      ***---
       READ-TNAZIONI.
           read tnazioni no lock
                invalid  move spaces to naz-descrizione
                         set trovato to false
           end-read.

      ***---
       READ-TIVAESE.
           read tivaese no lock
                invalid 
                move spaces to tbliv-descrizione1
                move spaces to tbliv-descrizione2
                set trovato to false                
            not invalid
                if tbliv-percentuale not = 0
                   move spaces to tbliv-descrizione1
                   move spaces to tbliv-descrizione2
                   set trovato to true
                end-if
           end-read.

      ***---
       READ-TCODPAG.
           read tcodpag no lock
                invalid move spaces to tblpa-descrizione1
                                       tblpa-descrizione2
                        set trovato to false
           end-read.

      ***---
       READ-ABI.
           read abi no lock
                invalid move spaces to abi-banca1
                                       abi-banca2
                        set trovato to false
           end-read.


      ***---
       RELAZIONI-CLIENTI.
           set trovato to false.

           evaluate nome-file
           when "anacap"  
                move ef-cap-buf to anc-cap
                perform RELATIONS
           when "tgrupgdo"  
                move spaces to lab-gdo-buf
                move ef-gdo-buf to gdo-codice
                if gdo-codice not = spaces
                   perform RELATIONS
                   move gdo-intestazione to lab-gdo-buf
                end-if    
                display lab-gdo
           when "tvettori"  
                move spaces to lab-vettore-buf
                move ef-vettore-buf to vet-codice
                if vet-codice not = 0
                   perform RELATIONS
                   move vet-descrizione to lab-vettore-buf
                end-if    
                display lab-vettore
           when "tprov"
                move spaces to lab-citta-buf
                move spaces to lab-regione-buf
                move ef-prov-buf to prv-codice
                if prv-codice not = space
                   perform RELATIONS
                   move prv-descrizione to lab-citta-buf
                   move reg-descrizione to lab-regione-buf
                end-if
                display lab-citta lab-regione
           when "tnazioni"                
                move spaces to lab-nazione-buf
                move ef-nazione-buf to naz-codice
                if naz-codice not = space
                   perform RELATIONS
                   move naz-descrizione to lab-nazione-buf
                end-if
                display lab-nazione
           when "tivaese"                
                move spaces     to lab-iva-buf      
                move "IV"       to tbliv-codice1
                move ef-iva-buf to tbliv-codice2
                if tbliv-codice2 not = spaces
                   perform RELATIONS
                   initialize lab-iva-buf
                   inspect tbliv-descrizione1 replacing trailing spaces 
                                                        by low-value
                   string  tbliv-descrizione1 delimited by low-value
                           " "                delimited by size
                           tbliv-descrizione2 delimited by size
                           into lab-iva-buf
                   end-string
                end-if
                display lab-iva
           when "tcodpag"             
                move spaces to lab-pag-buf
                if ef-pag-buf not = spaces
                   move "PA"       to tblpa-codice1
                   move ef-pag-buf to tblpa-codice2
                   perform RELATIONS
                   initialize lab-pag-buf
                   string tblpa-descrizione1 delimited by size
                          tblpa-descrizione2 delimited by size
                          into lab-pag-buf
                   end-string
                end-if
                display lab-pag
           when "ABI"
                move spaces to lab-banca-buf
                move ef-abi-buf to abi-codice-abi-x
                move ef-cab-buf to abi-codice-cab-x
                if abi-codice not = spaces   
                   perform RELATIONS
                   initialize lab-banca-buf
                   inspect abi-banca1 replacing trailing 
                                      spaces by low-value
                   string abi-banca1  delimited low-value
                          " "         delimited size
                          abi-banca2  delimited size
                          into lab-banca-buf
                   end-string
                end-if
                display lab-banca
           end-evaluate.

         
      ***---
       RELAZIONI-DESTINI.
           set trovato to false.

           evaluate nome-file   
           when "anacap" 
                move ef-cap-d-buf to anc-cap
                perform RELATIONS

           when "tvettori" 
                move spaces to lab-vettore-d-buf 
                move ef-vettore-d-buf to vet-codice
                if vet-codice not = 0
                   perform RELATIONS
                   move vet-descrizione to lab-vettore-d-buf
                end-if    
                display lab-vettore-d
           when "tprov"                     
                move spaces to lab-citta-d-buf
                move spaces to lab-regione-d-buf
                move ef-prov-d-buf to prv-codice
                if prv-codice not = space
                   perform RELATIONS
                   move prv-descrizione to lab-citta-d-buf
                   move reg-descrizione to lab-regione-d-buf
                end-if
                display lab-citta-d lab-regione-d
           when "tnazioni"        
                move spaces to lab-nazione-d-buf
                move ef-nazione-d-buf to naz-codice
                if naz-codice not = space
                   perform RELATIONS
                   move naz-descrizione to lab-nazione-d-buf
                end-if
                display lab-nazione-d
           when "tcodpag"        
                move spaces       to lab-pag-d-buf
                move "PA"         to tblpa-codice1
                move ef-pag-d-buf to tblpa-codice2
                if tblpa-codice2 not = space
                   perform RELATIONS
                   inspect tblpa-descrizione1 replacing trailing 
                                                 spaces by low-value
                   string  tblpa-descrizione1 delimited by low-value
                           " "                delimited by size
                           tblpa-descrizione2 delimited by size
                           into lab-pag-d-buf
                   end-string
                end-if
                display lab-pag-d
           end-evaluate.


      ***---
       RESET-GRIGLIA.
           modify form1-gd-1, reset-grid = 1.
           perform INTESTAZIONE.


      ***---
       RIEMPI-CHIAVE.
           move ef-codice-buf to cli-codice with convert.

      ***---
       ROW-TO-ENTRY.
      *    Questo prf. porta i dati da grid a entry-field
      *    quando rec-grd e rec-not sono valorizzati
           perform MOVE-DATI.
           perform DISPLAY-CAMPI-GRID.
           perform DISPLAY-CAMPI-NOTE.
           perform PAGE-2-BUF-TO-FLD.
           perform VALORIZZA-OLD-PAGE-2.


      ***---
       SALVA.
           if MOD = zero 
              exit paragraph 
           end-if.

           set tutto-ok to true.
           
           |Setto a zero queste due variabili così il controllo
           |su alcuni campi della griglia non vengono RIFATTI
           |perchè già presi in considerazione precedentemente
           |con la pressione del tasto "Salva" relativo alla grid...
           move 0 to tot-righe.
           move riga-nuova to store-riga.
           move 0 to riga-nuova.

           perform  varying control-id from 78-id-ef-codice by 1
                      until control-id > 78-ID-ef-cab
              |non faccio i controlli sui destini
              |perchè ci pensa il salva della grid
              perform CONTROLLO
              if errori 
                 exit perform 
              end-if
           end-perform.

LUBEXX*****           if tutto-ok perform SALVA-RIGA end-if.

           |...dopodiché riassegno a riga-nuova il suo valore originale,
           |mentre tot-righe può essre lasciato a 0 perchè
           |ricalcolato ogni volta che dev'essre testato
           move store-riga to riga-nuova.

LUBEXX     if tutto-ok 
              perform SALVA-RIGA 
           end-if.

           if errori
      *        perform ABILITAZIONI
              |controllo che l'errore sia sulla pagina corrente
              |specifico per i pgm. aventi Tab-Control
              inquire Screen1-Ta-1, value in pagina
              if store-id >= min-id(pagina) and
                 store-id <= max-id(pagina)
                 move store-id to CONTROL-ID
              end-if
              |******
              move 4 to accept-control
           else     
              |Specifico del pgm. GClienti: faccio il controllo dello
              |stato solamente alla fine, cioè quando gali altri sono
              |già andati TUTTI a buon fine
              move 78-ID-cbo-stato to control-id
              perform CONTROLLO
              ||||
              perform FORM1-BUF-TO-FLD
              perform CANCELLA-COLORE
                                                             
              move  cli-codice   to notf-codice of notef
              move  0            to notf-prog   of notef
              move WrkCampi      to notf-dati   of notef
                                           
              accept como-ora from time
              if nuovo
                 move data-oggi to cli-data-creazione
                                   notf-data-creazione of notef

                 |Così appena dopo aver creato un record NUOVO
                 |metto a video la sua data di creazione
                 move cli-data-creazione to como-data
                 perform DATE-TO-SCREEN
                 move como-data          to lab-data-buf
                 display lab-data

                 move como-ora  to cli-ora-creazione
                                   notf-ora-creazione    of notef
                 move user-codi to cli-utente-creazione
                                   notf-utente-creazione of notef
              else
                 move data-oggi to cli-data-ultima-modifica
                                   notf-data-ultima-modifica   of notef
                 move como-ora  to cli-ora-ultima-modifica 
                                   notf-ora-ultima-modifica    of notef
                 move user-codi to cli-utente-ultima-modifica
                                   notf-utente-ultima-modifica of notef
              end-if  

              write cli-rec 
                 invalid 
                    rewrite cli-rec 
              end-write
              |Specifico per GClienti mi riposiziono altrimenti
              |fallisce la read previous x' non fa la start
              set cli-tipo-F to true
              read clienti  no lock 
                 invalid 
                    continue 
              end-read

              move WrkNote-1 to notf-note-1 of notef
              move WrkData   to como-data
              perform DATE-TO-FILE
              move como-data to notf-data   of notef
              move WrkNote-2 to notf-note-2 of notef
              move WrkNote-3 to notf-note-3 of notef
              move WrkNote-4 to notf-note-4 of notef

              if notf-note-1 of notef not = spaces or
                 notf-data   of notef not =      0 or
                 notf-note-2 of notef not = spaces or
                 notf-note-3 of notef not = spaces or
                 notf-note-4 of notef not = spaces
                 write notf-rec of notef 
                    invalid 
                       rewrite notf-rec of notef
                 end-write
              else
                 delete notef record 
                    invalid 
                       continue 
                 end-delete
              end-if

              if DestinoCambiato
                 perform LOOP-DELETE-DESTINI
                 perform LOOP-SCRIVI-DESTINI
              else
                 perform X-Y
                 perform VALORE-RIGA
                 perform ROW-TO-ENTRY
              end-if

              if NoteCambiate
                 perform LOOP-DELETE-NOTE
                 perform LOOP-SCRIVI-NOTE
              end-if

              perform AGGIUNGI-DEST-1
              initialize G2Agg-linkage
              set G2Agg-for   to true
              move cli-codice to G2Agg-codice
              if nuovo set G2Agg-insert to true
              else     set G2Agg-update to true
              end-if
              call   "G2Agg" using G2Agg-linkage
              cancel "G2Agg"

              set vecchio to true       
              perform TORNA-IN-VISUA
           end-if.
                    
           perform DISPLAY-SCREEN.


      ***---
       SALVA-RIGA.
           if mod-campi = 0 
              exit paragraph 
           end-if.

           perform X-Y.

           if riga-nuova = 1
              add 1 to tot-righe giving riga
LUBEXX     |else
LUBEXX     |   if tot-righe = 1
LUBEXX     |      move 1 to riga-nuova
LUBEXX     |   end-if
           end-if.

           perform varying control-id from 78-ID-ef-ragsoc-1-d by 1
                     until control-id > 78-ID-ef-note-4
              perform CONTROLLO
              if errori exit perform end-if
           end-perform.
                       
           if tutto-ok
              perform MOVE-CAMPI
              perform DISPLAY-CAMPI-GRID
              perform DISPLAY-CAMPI-NOTE

              set DestinoCambiato to true
              perform PAGE-2-BUF-TO-FLD
              perform VALORIZZA-OLD-PAGE-2
              perform CANCELLA-COLORE
                      
              move 78-ID-form1-gd-1 to control-id
              move 4 to accept-control
              modify form1-gd-1, cursor-y = riga, cursor-x = 1
              perform COLORE
           end-if.


      ***---
       SALV-MOD.
           set tutto-ok to true.
           perform FORM1-CONTROLLO-OLD.
           
           if mod = 1
              if DestinoCambiato
                 set NoSalvato to true
              end-if

              if NoteCambiate
                 set NoSalvato to true
              end-if

              move WrkData to como-data
              perform DATE-TO-FILE
              move como-data to WrkData
                             
              if WrkNote-1  not = notf-note-1 of notef or
                 WrkData    not = notf-data   of notef or
                 WrkNote-2  not = notf-note-2 of notef or
                 WrkNote-3  not = notf-note-3 of notef or
                 WrkNote-4  not = notf-note-4 of notef
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

                   set ERRORI to true
                   |controllo che l'errore sia sulla pagina corrente
                   |specifico per i pgm. aventi Tab-Control
                   inquire Screen1-Ta-1, value in pagina
                   if store-id >= min-id(pagina) and
                      store-id <= max-id(pagina)
                      move store-id to CONTROL-ID
                   end-if
                   |******
                   move 4 to accept-control
              end-evaluate
           end-if.


      ***---
       SALV-MOD-RIGA.
           if mod-destini = 0 
              exit paragraph 
           end-if.

           set tutto-ok to true.
           |Specifico del pgm. GClienti
           perform CONTROLLO-OLD-RIGA.

           if NoSalvato
              display message "Confermare la modifica dei destinatari?"
                            title titolo
                            type mb-yes-no-cancel 
                            giving scelta       
       
              evaluate scelta
              when mb-yes 
                   perform SALVA-RIGA
              when mb-no  
                   continue
              when other  
                   perform CANCELLA-COLORE
                   set errori to true 
                   move 4 to accept-control
              end-evaluate

           end-if.


      ***---
       SELEZIONA.
           set cli-tipo-F to true.
           move   "clienti-all"  to como-file.
           call   "zoom-gt"   using como-file, cli-rec
                             giving stato-zoom.
           cancel "zoom-gt".
      
           if stato-zoom = 0
              if old-cli-chiave not = cli-chiave
                 move cli-chiave to save-chiave
                 perform SALV-MOD
                 if tutto-ok
                    move save-chiave        to cli-chiave
                    modify ef-codice,       VALUE = cli-codice
                    move cli-codice to ef-codice-buf
                    set ReadSecca to true 
                    perform CANCELLA-COLORE
                    perform CURRENT-RECORD
                    move 78-ID-ef-codice to control-id
                    move 4 to accept-control
                 end-if
              end-if
           end-if.



      ***---
       SELEZIONA-ALFA.
      * LA SECONDA CHIAVE E' ALFANUMERICA E LA VALUE VARIABLE CONTIENE
      * ANCORA GLI ZERI DAVANTI CHE ELIMINO
           inspect ef-codice-buf replacing leading ZERO by SPACES.
           move ef-codice-buf to cli-ragsoc-1.

      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           call "C$JUSTIFY" using cli-ragsoc-1, "L".
                        
           set cli-tipo-F to true.
           start clienti key >= cli-k1
                 invalid continue
             not invalid read clienti next 
                         if cli-tipo-C
                            initialize cli-rec
                         end-if
           end-start.
           set cli-tipo-F to true.

           move "clienti-alfa-all" to como-file.
           call "zoom-gt"  using  como-file, cli-rec
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = 0
              move cli-codice    to codice-ed
              move codice-ed     to ef-codice-buf   
              call "C$JUSTIFY" using ef-codice-buf, "L"
              display ef-codice

              if old-cli-ragsoc-1 not = cli-ragsoc-1
                 move cli-ragsoc-1 to save-ragsoc-k1
                 move cli-codice   to save-codice-k1
                 perform SALV-MOD
                 if tutto-ok
                    move save-cli-K1 to cli-ragsoc-1
                    set ReadSecca  to true
                    modify ef-codice, value cli-codice
                    perform CURRENT-RECORD
                    perform CANCELLA-COLORE
                    move 78-ID-ef-codice to CONTROL-ID       
                    move 4               to ACCEPT-CONTROL   
                 end-if                                  
              end-if
           else
              set errori           to true
              move 78-ID-ef-codice to CONTROL-ID
           end-if.


      ***---
       SELEZIONA-NUMERICO.
           if cli-codice not > 0
              set errori to true
              move 78-ID-ef-codice to control-id
              display message box msg-codice-obbligatorio
                      title tit-err
                      icon  mb-warning-icon
           else           
              set cli-tipo-F to true
              read clienti no lock
                   invalid 
                       if vecchio
                          display message box MSG-record-inesistente
                                  title = tit-err
                                  icon mb-warning-icon
                          perform CLEAR-SCREEN
                          set errori to true
                          move 78-ID-ef-codice to control-id
                       end-if
               not invalid      
                       if old-cli-chiave not = cli-chiave
                          perform CURRENT-RECORD 
                          set errori to true  
                          move 78-ID-ef-codice to control-id
                       end-if
              end-read
           end-if.


      ***---
       SPOSTAMENTO.
           inquire form1-gd-1, last-row in tot-righe
           if event-data-2 >= 2 and
              event-data-2 <= tot-righe
              if riga not = event-data-2 
                 if riga-nuova = 1
                    perform X-Y
                    modify form1-gd-1, record-to-delete = tot-righe + 1
                    move 0 to riga-nuova
                 end-if
      
                 set tutto-ok to true

                 move event-data-2 to riga
                 perform VALORE-RIGA
                 perform ROW-TO-ENTRY
              end-if
              perform COLORE
           end-if.

      ***---
       STAMPA.
           SET STAMPA     TO TRUE.


      ***---
       SUCCESSIVO.
           perform SALV-MOD.           
           if tutto-ok          
              perform CHECK-INSERIMENTO
              unlock clienti all records

              if mod = 1
                 move 0 to mod-k
                 set dataset1-clienti-lock to true
              else              
                 move 1 to mod-k     
                 initialize dataset1-clienti-lock-flag
              end-if

              perform CANCELLA-COLORE
              perform FORM1-NEXT

              if RecLocked
                 set RecLocked to false             
                 initialize dataset1-clienti-lock-flag
                 move 1 to mod-k
                 perform FORM1-NEXT
              end-if
           end-if.


      ***---
       TORNA-IN-VISUA.
           move 0 to mod.
           move 1 to mod-k.
           move 78-ID-ef-codice to CONTROL-ID.
           set NoMessage to true.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG.
           unlock clienti all records.
                        
           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.
           move 4 to ACCEPT-CONTROL.

           if KeyboardSaved
              set KeyboardReleased to true
              set environment "KEYSTROKE" to "DATA=44 44"
              set environment "KEYSTROKE" to "DATA=46 46"
           end-if.

           display Form1.


      ***---
       ULTIMO.
           perform SALV-MOD.
           if tutto-ok
              perform CHECK-INSERIMENTO
              unlock clienti all records 

              if mod = 1
                 move 0 to mod-k
                 set dataset1-clienti-lock to true
              else              
                 move 1 to mod-k     
                 initialize dataset1-clienti-lock-flag
              end-if

              perform CANCELLA-COLORE
              perform FORM1-LAST 

              if RecLocked
                 set RecLocked to false             
                 initialize dataset1-clienti-lock-flag
                 move 1 to mod-k
                 perform FORM1-LAST
              end-if
           end-if.


      ***---
       VALORE-RIGA.
           inquire form1-gd-1(riga, 1), cell-data   col-prog.
           inquire form1-gd-1(riga, 2), cell-data   col-ragsoc.
           inquire form1-gd-1(riga, 3), cell-data   col-indirizzo.
           inquire form1-gd-1(riga, 4), cell-data   col-cap.
           inquire form1-gd-1(riga, 5), cell-data   col-localita.
           inquire form1-gd-1(riga, 6), cell-data   col-prov.
           inquire form1-gd-1(riga, 7)  hidden-data hidden-ragsoc-2.
           inquire form1-gd-1(riga, 8)  hidden-data hidden-nazione.
           inquire form1-gd-1(riga, 9)  hidden-data hidden-telef-1.
           inquire form1-gd-1(riga, 10) hidden-data hidden-telef-2.
           inquire form1-gd-1(riga, 11) hidden-data hidden-fax.
           inquire form1-gd-1(riga, 12) hidden-data hidden-mail.
           inquire form1-gd-1(riga, 13) hidden-data hidden-referente.
           inquire form1-gd-1(riga, 14) hidden-data hidden-vettore.
           inquire form1-gd-1(riga, 15) hidden-data hidden-pag.
           inquire form1-gd-1(riga, 17) hidden-data hidden-stato. 
           inquire form1-gd-1(riga, 18) hidden-data hidden-note-1.
           inquire form1-gd-1(riga, 19) hidden-data hidden-data-note.
           inquire form1-gd-1(riga, 20) hidden-data hidden-note-2.
           inquire form1-gd-1(riga, 21) hidden-data hidden-note-3.
           inquire form1-gd-1(riga, 22) hidden-data hidden-note-4.

           inquire form1-gd-1(riga, 23) hidden-data hidden-referente-ord.
           inquire form1-gd-1(riga, 24) 
                                      hidden-data hidden-tel-dir-ref-ord.
           inquire form1-gd-1(riga, 25) hidden-data hidden-mail-ref-ord.
           inquire form1-gd-1(riga, 26) 
                               hidden-data hidden-perce-premi-fine-anno.
           inquire form1-gd-1(riga, 27) hidden-data hidden-gg-consegna.
      *     inquire form1-gd-1(riga, 28) hidden-data hidden-ult-ord.
           inquire form1-gd-1(riga, 28) 
                 hidden-data hidden-mail-ref-ord-cc.
           inquire form1-gd-1(riga, 29) 
                               hidden-data hidden-netto-PFA.
           inquire form1-gd-1(riga, 30) 
                               hidden-data hidden-saldi-d.
           inquire form1-gd-1(riga, 31) hidden-data hidden-ufficio-d.
           inquire form1-gd-1(riga, 32) hidden-data hidden-ei-d.
           inquire form1-gd-1(riga, 33) hidden-data hidden-invio-sol-d.

      ***---
       VALORIZZA-NUOVO.
           move high-value to cli-codice.
           set cli-tipo-F to true.
           start clienti key <= cli-chiave
                 invalid
                 move 1 to cli-codice
             not invalid
                 read clienti previous with no lock
                      at end  move 1 to cli-codice
                  not at end
                      if cli-tipo-c
                         move 1 to cli-codice
                      else
                         add  1 to cli-codice
                      end-if
                 end-read
           end-start.
           move "00" to status-clienti.  

           initialize cli-dati 
                      desf-dati
                      notf-dati of notef1 
                                replacing numeric data by zeroes
                                     alphanumeric data by spaces
                                 
           perform FORM1-IUD-DISPLAY.

           set nuovo to true.    

           initialize notf-dati of notef1
                      notf-dati of notef 
                      replacing numeric data by zeroes
                                            alphanumeric data by spaces.
                                            
           move spaces to WrkNote-1.
           move spaces to WrkNote-2.
           move spaces to WrkNote-3.
           move spaces to WrkNote-4.
           move      0 to WrkData.
                        
           move 0 to LastPrg.
      *     move 1   to riga-nuova.


      ***---
       VALORIZZA-OLD.
           move cli-rec to old-cli-rec.
           set vecchio                  to true.

           if old-cli-utf = space
              move "N" to old-cli-utf
           end-if.           

           if old-cli-spost-ric-agosto = space
              move "N" to old-cli-spost-ric-agosto
           end-if.                       

           if old-cli-spost-ric-dicembre = space
              move "N" to old-cli-spost-ric-dicembre
           end-if.
           
           if old-cli-stato = spaces
              move "A" to old-cli-stato |Attivo
           end-if.

           evaluate CONTROL-ID
           when 78-ID-ef-prov
           when 78-ID-ef-nazione
           when 78-ID-ef-vettore
           when 78-ID-ef-iva
           when 78-ID-ef-pag
           when 78-ID-ef-abi
           when 78-ID-ef-cab
           when 78-ID-ef-prov-d
           when 78-ID-ef-nazione-d
           when 78-ID-ef-vettore-d
           when 78-ID-ef-pag-d
                move 1 to StatusHelp
           when other
                move 0 to StatusHelp
           end-evaluate.
           perform STATUS-HELP.

           move 0 to riga-nuova.
           set DestinoCambiato  to false.
           set NoteCambiate     to false.


      ***--- 
       VALORIZZA-OLD-PAGE-2.
           move desf-rec           to old-desf-rec.
           move notf-rec of notef1 to old-notf-rec.

           if old-desf-stato = space
              move "A" to old-desf-stato |Attivo (Default)
           end-if.


      ***---
       VERIFICA-PAGAMENTO-BANCARIO.
           set PagamentoBancario to false.
LUBEXX*****     move "tcodpag" to nome-file.
LUBEXX*****     perform RELAZIONI-CLIENTI.
LUBEXX*****     if trovato
LUBEXX*****        perform varying idx from 1 by 1 until idx > 36
LUBEXX*****           if tblpa-codice-tr(idx) = "W"
LUBEXX*****              set PagamentoBancario to true
LUBEXX*****              exit perform
LUBEXX*****           end-if
LUBEXX*****        end-perform
LUBEXX*****     end-if.


      ***---
       X-Y.
           inquire form1-gd-1, last-row in tot-righe, 
                               cursor-y in riga,
                               cursor-x in colonna.



      *******************************************************************
      *                  SEZIONE ENTRY_POINT & EVENTS                   *
      *******************************************************************

      ***---
       GFORN-BEFORE-ACCEPT.
           perform INIT.
           perform ABILITA-TOOLBAR.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG.
                         
           accept data-oggi from century-date.


      ***---
       GFORN-AFTER-BUF-TO-FLD.
           perform SCARICA-COMBO-STATO.
           move stato          to cli-stato.

           move cli-codice to notf-codice of notef.
           move 0          to notf-prog   of notef.

           move ef-codice-buf to cli-codice convert.  


      ***---
       GFORN-AFTER-FLD-TO-BUF.
           move cli-stato        to stato.
           perform CARICA-COMBO-STATO.
           evaluate true
           when bloccato  move 1 to mod-campi
           when attivo    move 1 to mod-campi
           when disattivo move 0 to mod-campi
           end-evaluate.

           perform CARICA-TMP-NOTE.
           perform LOAD-RECORD.  
  
           perform X-Y.
           if tot-righe = 1 
              modify form1-gd-1, insert-rows = 1 
              move "Attivo" to cbo-stato-d-buf old-desf-stato
              modify cbo-stato-d, value = cbo-stato-d-buf
           else
              perform CARICA-COMBO-DESTINI
              evaluate true
              when bloccato  move 1 to mod-destini
              when attivo    move 1 to mod-destini
              when disattivo move 0 to mod-destini
              end-evaluate
           end-if.

           move cli-data-creazione to como-data.
           perform DATE-TO-SCREEN
           move como-data          to lab-data-buf.

           perform VALORIZZA-OLD.
                                  
           move cli-codice    to  codice-ed.
           move codice-ed     to  ef-codice-buf.   
           call "C$JUSTIFY" using ef-codice-buf, "L".
           display ef-codice.   

      * Relazioni per il file dei CLIENTI
      * CLIENTI-NOTE
           initialize notf-rec of notef replacing numeric data by zeroes
                                             alphanumeric data by spaces
           move cli-codice to notf-codice of notef.
           move 0          to notf-prog   of notef.
           read notef no lock 
              invalid 
                 move spaces to WrkNote-1
                 move spaces to WrkNote-2
                 move spaces to WrkNote-3
                 move spaces to WrkNote-4
                 move      0 to WrkData
              not invalid 
                 move notf-note-1 of notef to WrkNote-1
                 move notf-data   of notef to como-data
                 perform DATE-TO-SCREEN
                 move como-data            to WrkData  
                 move notf-note-2 of notef to WrkNote-2
                 move notf-note-3 of notef to WrkNote-3
                 move notf-note-4 of notef to WrkNote-4
           end-read.

      * CLIENTI-PROVINCIE
           move "tprov" to nome-file.
           perform RELAZIONI-CLIENTI.

      * CLIENTI-NAZIONI
           move "tnazioni" to nome-file.
           perform RELAZIONI-CLIENTI.

      * CLIENTI-VETTORI
           move "tvettori" to nome-file.
           perform RELAZIONI-CLIENTI.

      * CLIENTI-TIVAESE     
           move "tivaese" to nome-file.
           perform RELAZIONI-CLIENTI.

      * CLIENTI-CODICI DI PAGAMENTO
           move "tcodpag" to nome-file.
           perform RELAZIONI-CLIENTI.  

      * CLIENTI-BANCHE
           move "ABI" to nome-file.
           perform RELAZIONI-CLIENTI. 

      * CLIENTI-GDO
           move "tgrupgdo" to nome-file.
           perform RELAZIONI-CLIENTI. 
                          
      * Relazioni per il file dei DESTINI
      * DESTINI-PROVINCIE
           move "tprov" to nome-file.
           perform RELAZIONI-DESTINI.

      * DESTINI-NAZIONI
           move "tnazioni" to nome-file.
           perform RELAZIONI-DESTINI.

      * CLIENTI-VETTORI
           move "tvettori" to nome-file.
           perform RELAZIONI-DESTINI.

           perform ABILITAZIONI.

LUBEXX     if control-id = 78-ID-rb-pers or
              control-id = 78-ID-rb-soc
              perform CANCELLA-COLORE
              move 78-ID-ef-codfis to control-id
              move 4               to accept-control
           end-if.


      ***---
       PUSH-NOTE-PRESSED.
           move mod-campi to e-campo.
           perform APRI-NOTE.


      ***---
       PUSH-NOTA-PIE-PRESSED.
           inquire ef-codice, value in ef-codice-buf.
           move ef-codice-buf to notf-codice of notef with convert.

           if notf-codice of notef > 0
              call   "note-pie-f" using livello-abil
                                        StatoRec
                                        mod
                                        notf-codice of notef
                                        WrkCampi
              cancel "note-pie-f"
           end-if.


      ***---
       PUSH-GRID-ELIMINA-PRESSED.
           if mod-campi = 0 exit paragraph end-if.

           perform X-Y.

           if tot-righe > 1 or
              ( riga > 1 and riga < tot-righe )

              display message box "Eliminare la riga selezionata?"
                      title = titolo
                      type mb-yes-no
                      giving scelta
                      default mb-no

              if scelta = mb-yes
                 perform X-Y
                 modify form1-gd-1, record-to-delete = riga
                 set DestinoCambiato to true
                 set NoteCambiate  to true
                 if tot-righe = 2
                    perform PAGE-2-CLEAR
                    modify form1-gd-1, insert-rows = 1
                    move 2 to riga
                 else
                    move 2 to riga
                    perform VALORE-RIGA
                    perform ROW-TO-ENTRY
                 end-if
                 modify form1-gd-1, cursor-y = riga, cursor-x = 1
                 perform COLORE
              end-if
           end-if.



      ***---
       PUSH-GRID-NUOVO-PRESSED.
           if mod-campi = 0 exit paragraph end-if.

           perform SALV-MOD-RIGA.

           if tutto-ok
              perform X-Y
              |per sicurezza cancello un'eventuale riga vuota già inserita
              modify form1-gd-1, record-to-delete = tot-righe + 1
              modify form1-gd-1, insert-rows = 1
              perform VALORIZZA-DA-FORN
              add 1 to tot-righe
              move tot-righe to riga
              modify form1-gd-1, cursor-y = riga, cursor-x = 1
              perform PAGE-2-CLEAR
              perform COLORE
              move 78-ID-ef-ragsoc-1-d to control-id
              move 4 to accept-control
              move "Consegna" to ef-note-1-buf
              display ef-note-1
              set non-gestito-d to true
              perform CARICA-COMBO-UFFICIO-D
           end-if.

      ***---
       VALORIZZA-DA-FORN.
           move chk-saldi-buf to chk-saldi-d-buf.
           display chk-saldi-d.

      ***---
       PB-STAMPA-PRESSED.
           perform CANCELLA-COLORE.
           if nuovo
              display message "Occorre confermare il fornitore"
                        title titolo
                         icon 2
           else
              call "W$MOUSE" using set-mouse-shape, wait-pointer
              call   "st-frn-det" using cli-codice
              cancel "st-frn-det"
              call "W$MOUSE" using set-mouse-shape, arrow-pointer
           end-if.
           move 760 to control-id.
           move 4   to accept-control.
           modify pb-stampa, bitmap-number = 2.

      ***---
       CARICA-TMP-NOTE.
           close       TMP-NFORN.
           open output TMP-NFORN.
           close       TMP-NFORN.
           open i-o    TMP-NFORN.

           move cli-chiave   to nfor-chiave-forn
           move low-value    to nfor-prog

           start NFORN key not < nfor-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read NFORN next no lock
                       at end
                          exit perform
                    end-read
                    if cli-chiave not = nfor-chiave-forn
                       exit perform
                    end-if
                    move nfor-rec  to tmp-nfor-rec
                    write TMP-NFOR-REC
                       invalid
                          continue
                    end-write
                 end-perform
           end-start.

           close       tmp-nforn-dest.
           open output tmp-nforn-dest
           close       tmp-nforn-dest
           open i-o    tmp-nforn-dest

           move cli-codice   to nfod-codice
           move low-value    to nfod-dest
                                nfod-prog

           start NFORN-dest key not < nfod-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read nforn-dest next no lock
                       at end
                          exit perform
                    end-read
                    if cli-codice not = nfod-codice
                       exit perform
                    end-if
                    move nfod-rec  to tmp-nfod-rec
                    write tmp-nfod-rec
                       invalid
                          continue
                    end-write
                 end-perform
           end-start.

      ***---
       LOOP-DELETE-NOTE.
           move cli-chiave   to nfor-chiave-forn
           move low-value    to nfor-prog

           start NFORN key not < nfor-chiave
                 invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read NFORN next no lock
                       at end
                          exit perform
                    end-read
                    if cli-chiave not = nfor-chiave-forn
                       exit perform
                    end-if
                    delete NFORN record
                       invalid
                          continue
                    end-delete
                 end-perform
           end-start.

           move cli-codice   to nfod-codice
           move low-value    to nfod-dest
                                nfod-prog

           start nforn-dest key not < nfod-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read nforn-dest next no lock
                       at end
                          exit perform
                    end-read
                    if cli-codice not = nfod-codice
                       exit perform
                    end-if
                    delete nforn-dest record
                       invalid
                          continue
                    end-delete
                 end-perform
           end-start.

      ***---
       LOOP-SCRIVI-NOTE.
           move low-value    to tmp-nfor-chiave

           start TMP-NFORN key not < tmp-nfor-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read TMP-NFORN next no lock
                       at end
                          exit perform
                    end-read
                    
                    move tmp-nfor-rec  to nfor-rec
                    write NFOR-REC
                       invalid
                          rewrite NFOR-REC
                    end-write

                 end-perform
           end-start.

           move low-value    to tmp-nfod-chiave

           start tmp-nforn-dest key not < tmp-nfod-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tmp-nforn-dest next no lock
                       at end
                          exit perform
                    end-read
                    
      *    prima di scrivere controllo che esista ancora il destino
                    move tmp-nfod-chiave-dest  to desf-chiave
                    read DESTINIF no lock
                       invalid
                          continue
                       not invalid
                          move tmp-nfod-rec  to nfod-rec
                          write nfod-rec
                             invalid
                                rewrite nfod-rec
                          end-write
                    end-read

                 end-perform
           end-start.

      ***---
       CARICA-GRID-NOTE.
           modify gd-note, MASS-UPDATE = 1 
                           reset-grid = 1
           perform GD-NOTE-CONTENT

           move 1   to riga-note

           move low-value    to tmp-nfor-chiave

           start tmp-nforn key not less tmp-nfor-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tmp-nforn next no lock
                       at end
                          exit perform
                    end-read
                    add 1 to riga-note
                    initialize rec-note
                    move tmp-nfor-prog   to col-prog-note
                    move tmp-nfor-nota   to col-note

                    modify gd-note, RECORD-TO-ADD = rec-note

                    modify gd-note(riga-note, 1), 
                                   HIDDEN-DATA = tmp-nfor-dati-comuni
                    
                 end-perform
           end-start
           
           move 2   to event-data-2

           perform SPOSTAMENTO-NOTE


           modify gd-note, MASS-UPDATE = zero.

      ***---
       SALVA-NOTE.
           accept como-data  from century-date
           accept como-ora   from time

      *    cancello tutte le righe delle note
           move low-value    to tmp-nfor-chiave

           start tmp-nforn key not < tmp-nfor-chiave
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read tmp-nforn next no lock
                       at end
                          exit perform
                    end-read
                    delete tmp-nforn record
                 end-perform
           end-start

           inquire gd-note, LAST-ROW = tot-righe-note
           move zero   to cont
           perform varying riga-note from 2 by 1 
                          until riga-note > tot-righe-note

              add 1 to cont
              inquire gd-note(riga-note), record-data = rec-note
              inquire gd-note, (riga-note, 1) 
                                      hidden-data = tmp-nfor-dati-comuni

              move cli-chiave   to tmp-nfor-chiave-forn
              move cont         to tmp-nfor-prog
              move col-note     to tmp-nfor-nota
              move como-data    to nfor-data-ultima-modifica
              move como-ora     to nfor-ora-ultima-modifica
              move user-codi    to nfor-utente-ultima-modifica

              write tmp-nfor-rec
           end-perform.

      ***---
       SPOSTAMENTO-NOTE.
           inquire gd-note, last-row in tot-righe-note.
           if event-data-2 >= 2 and
              event-data-2 <= tot-righe-note
              move riga-note   to riga-old-note
              if riga-note not = event-data-2 
                 if riga-nuova-note = 1
                    perform X-Y-NOTE
                    modify gd-note, record-to-delete = 
                                                     tot-righe-note + 1
                    move 0 to riga-nuova-note
                 end-if
      
                 set tutto-ok to true

                 move event-data-2 to riga-note
              end-if
            
              perform CAMBIA-FONT-RIGA-NOTE
           end-if.      

      ***---
       X-Y-NOTE.
           inquire gd-note, last-row in tot-righe-note, 
                            cursor-y in riga-note,
                            cursor-x in colonna-note.

      ***---
       CAMBIA-FONT-RIGA-NOTE.
           modify gd-note(riga-old-note), row-font = small-font.

           modify gd-note(riga-old-note, 1),
                  bitmap        = elemento-bmp,
                  bitmap-number = 1,
                  bitmap-width  = 16.

           move event-data-2 to riga-note.
           modify gd-note(riga-note), row-font = font-evidenzia-griglia..

           modify gd-note, (riga-note, 1),
                  bitmap        = elemento-bmp,
                  bitmap-number = 2,
                  bitmap-width  = 16.

      ***---
       AGGIUNGI-DEST-1.
           move cli-codice   to desf-codice
           move 1            to desf-prog
           read destinif
              invalid
                 move cli-ragsoc-1          to desf-ragsoc-1 
                 move cli-ragsoc-2          to desf-ragsoc-2 
                 move cli-indirizzo         to desf-indirizzo
                 move cli-cap               to desf-cap      
                 move cli-localita          to desf-localita 
                 move cli-prov              to desf-prov     
                 move cli-nazione           to desf-nazione  
                 move cli-tel-1             to desf-telef-1  
                 move cli-tel-2             to desf-telef-2  
                 move cli-fax               to desf-fax    
                 move cli-email             to desf-mail   
                 move cli-superamento-500   to desf-superamento-500
                 move cli-stato             to desf-stato
                 move cli-referente         to desf-referente
                 move cli-vettore           to desf-vettore
                 move cli-utf               to desf-depostio-UTF
                 move cli-referente-ord     to desf-referente-ord        
                 move cli-tel-dir-ref-ord   to desf-tel-dir-ref-ord      
                 move cli-mail-ref-ord      to desf-mail-ref-ord         
                 move cli-dati-comuni       to desf-dati-comuni
                 write desf-rec invalid continue end-write
           end-read.

      ***---
       SCR-STAMPA-BOTTON-SI-PRESSED.
           inquire chk-st-cli,  value in e-st-cli.
           inquire chk-st-des,  value in e-st-des.
           inquire chk-st-note, value in e-st-note.

           perform varying control-id from 78-ID-ef-cod-da by 1
                     until control-id > 78-ID-ef-des-a
              perform CHECK-INTERVALLO
              if errori exit perform end-if
           end-perform.

           if tutto-ok
              if e-st-cli  = 0 and 
                 e-st-des  = 0 and
                 e-st-note = 0
                 display message "Selezionare un tipo di stampa"
                           title tit-err
                            icon 2
              else
                 if e-st-cli  = 1 perform STAMPA-FORNITORI end-if
                 if e-st-des  = 1 perform STAMPA-DESTINI-F end-if
                 if e-st-note = 1 perform STAMPA-NOTE-F    end-if

                 perform CANCELLA-COLORE
                 move 100 to control-id
                 move   4 to accept-control
                 modify pb-sib, bitmap-number = 2
              end-if
           end-if.

      ***---
       CHECK-INTERVALLO.
           set tutto-ok to true.

           evaluate control-id
           when 78-ID-ef-cod-da
                inquire ef-cod-da, value in ef-cod-da-buf
                if ef-cod-da-buf not = 0
                   move ef-cod-da-buf to cli-codice
                   set cli-tipo-C to true
                   read clienti no lock 
                        invalid 
                        set errori to true
                        display message "Cliente NON valido"
                                  title tit-err
                                   icon 2
                   end-read
                end-if

           when 78-ID-ef-cod-a
                inquire ef-cod-da, value in ef-cod-da-buf
                inquire ef-cod-a,  value in ef-cod-a-buf
                evaluate ef-cod-a-buf
                when 0
                     move 99999 to ef-cod-a-buf
                     display ef-cod-a
                when 99999
                     continue
                when other
                     move ef-cod-a-buf to cli-codice
                     set cli-tipo-C to true
                     read clienti no lock 
                          invalid
                          set errori to true
                          display message "Cliente NON valido"
                                    title tit-err
                                     icon 2
                      not invalid
                          if ef-cod-a-buf < ef-cod-da-buf
                             display message "Intervallo Cliente errato"
                                       title tit-err
                                        icon 2
                             set errori to true
                             move 78-ID-ef-cod-da to control-id
                          end-if
                     end-read
                end-evaluate   
   
           when 78-ID-ef-st-naz
                inquire ef-st-naz, value in naz-codice
                if naz-codice = spaces
                   move "Tutte le nazioni" to lab-st-naz-buf
                else
                   read tnazioni no lock
                        invalid
                        display message "Nazione NON valida"
                                  title tit-err
                                   icon 2
                        set errori to true
                        move spaces to naz-descrizione
                   end-read
                   move naz-descrizione to lab-st-naz-buf
                end-if     
                display lab-st-naz
   
           when 78-ID-ef-st-prov
                inquire ef-st-prov, value in prv-codice
                if prv-codice = spaces
                   move "Tutte le provincie" to lab-st-prov-buf
                else
                   read tprov no lock
                        invalid
                        display message "Provincia NON valida"
                                  title tit-err
                                   icon 2
                        set errori to true
                        move spaces to prv-descrizione
                   end-read
                   move prv-descrizione to lab-st-prov-buf
                end-if
                display lab-st-prov
   
           when 78-ID-ef-des-a
                if e-st-des = 1
                   inquire ef-des-da, value in ef-des-da-buf
                   inquire ef-des-a,  value in ef-des-a-buf
                   evaluate ef-des-a-buf
                   when 0
                        move 99999 to ef-des-a-buf
                        display ef-des-a
                   when 99999
                        continue
                   when other
                        if ef-des-a-buf < ef-des-da-buf
                           display message "Intervallo Destini errato"
                                     title tit-err
                                      icon 2
                           set errori to true
                           move 78-ID-ef-des-da to control-id
                        end-if
                   end-evaluate
                end-if      

           end-evaluate.

           if errori
              perform CANCELLA-COLORE
              move 4 to accept-control
           end-if.
