      *******************************************************************
      *                  SEZIONE PARAGRAFI (User Dephined)              *
      *******************************************************************

      ***---
       ABILITAZIONI.
           if mod = 1                  
              move BitmapDeleteEnabled to BitmapNumDelete
              move BitmapSaveEnabled   to BitmapNumSave
              move 1 to e-salva e-cancella

              move 0 to mod-destini
              move 0 to mod-campi
      *****        move 0 to mod-gdo
              move 0 to mod-agente
              move 0 to mod-blocco

              inquire cbo-stato,   value cbo-stato-buf
              inquire cbo-stato-d, value cbo-stato-d-buf

              if cbo-stato-buf not = "Disattivo" 
                 move 1 to mod-campi
                 perform VALORIZZA-ENABLE-VARIABLE
                 if cbo-stato-d-buf not = "Disattivo"
                    move 1 to mod-destini  
                 end-if
              end-if

              if cbo-stato-buf = "Bloccato" 
                 move 1 to mod-blocco
              end-if
           else         

              move BitmapDeleteDisabled to BitmapNumDelete
              move BitmapSaveDisabled   to BitmapNumSave
              move 0 to e-salva e-cancella
              move 0 to mod-campi
      *****        move 0 to mod-gdo
              move 0 to mod-destini
              move 0 to mod-agente
              move 0 to mod-blocco
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
      * FINE        

      ***---
       ANTEPRIMA.
           SET ANTEPRIMA     TO TRUE.

      ***---
       CANCELLA.
           if mod = ZERO  exit paragraph  end-if.

           inquire ef-codice, value in ef-codice-buf.

           move ef-codice-buf to codice-x.

           call "C$JUSTIFY" using codice-x, "R".
           inspect codice-x replacing all X"20" by x"30".

           if codice-x is numeric
              call   "del-cli" using codice-x, SceltaDelete
              cancel "del-cli"
              evaluate true
              when annulla   continue
              when elimina   initialize G2Agg-linkage
                             move cli-codice to G2Agg-codice
                             perform FORM1-DELETE
                             if totem-msg-return-value = 1
                                perform CANCELLA-EVACLIDES
                                perform CANCELLA-CLI-PRG
                                move codice-x to not-codice of note
                                move 0        to not-prog   of note
                                delete note record invalid continue 
                                end-delete
                                initialize WrkCampi old-not-rec
                                replacing numeric data by zeroes
                                     alphanumeric data by spaces

                                set G2Agg-cli    to true
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
              display message
                      "Impossibile procedere con la cancellazione."
                                  "Digitare un valore numerico."
                      title = tit-err
                      icon mb-warning-icon
           end-if .

      ***---
       CANCELLA-EVACLIDES.            
           move low-value  to ecd-rec-OLD.
           move cli-codice to ecd-cliente-OLD.
           start evaclides key >= ecd-chiave-OLD
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read evaclides next no lock 
                         at end exit perform
                    end-read
                    if ecd-cliente-OLD not = cli-codice
                       exit perform
                    end-if
                    delete evaclides record
                 end-perform
           end-start.

      ***---
       CANCELLA-CLI-PRG.
           move low-value  to cp-rec.
           set  cp-tipo-C  to true.
           move cli-codice to cp-clifor.

           start cli-prg key >= cp-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read cli-prg next no lock 
                         at end exit perform
                    end-read
                    if cp-tipo-F
                       exit perform
                    end-if
                    if cp-clifor not = cli-codice
                       exit perform
                    end-if
                    delete cli-prg record
                 end-perform
           end-start.

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

           when 78-ID-ef-tipo
                move "ttipocli"  to como-file         
                inquire ef-tipo,    value in tcl-codice
                call "zoom-gt"   using como-file, tcl-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move tcl-codice      to ef-tipo-buf
                   move tcl-descrizione to lab-tipo-buf
                   display ef-tipo lab-tipo
                end-if

           when 78-ID-ef-gdo                
                move "tgrupgdo"  to como-file         
                inquire ef-gdo,     value in gdo-codice
                call "zoom-gt"   using como-file, gdo-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move gdo-codice       to ef-gdo-buf
                   move gdo-intestazione to lab-gdo-buf
                   display ef-gdo lab-gdo
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

           when 78-ID-ef-agente             
                move "agenti"    to como-file         
                inquire ef-agente,  value in age-codice
                call "zoom-gt"   using como-file, age-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move age-codice   to ef-agente-buf
                   move age-ragsoc-1 to lab-agente-buf
                   display ef-agente lab-agente
                end-if

           when 78-ID-ef-agente2             
                move "agenti"    to como-file         
                inquire ef-agente2,  value in age-codice
                call "zoom-gt"   using como-file, age-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move age-codice   to ef-agente2-buf
                   move age-ragsoc-1 to lab-agente2-buf
                   display ef-agente2 lab-agente2
                end-if
      
           when 78-ID-ef-iva-ese
                move "tivaese-ese"   to como-file
                move "IV"            to tbliv-codice1
                inquire ef-iva-ese,  value in tbliv-codice2
                call "zoom-gt"   using como-file, record-tbliv
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move tbliv-codice2   to ef-iva-ese-buf
                   initialize lab-iva-ese-buf
                   inspect tbliv-descrizione1 replacing trailing 
                                                 spaces by low-value
                   string  tbliv-descrizione1 delimited by low-value
                           " "                delimited by size
                           tbliv-descrizione2 delimited by size
                           into lab-iva-ese-buf
                   end-string
                   display ef-iva-ese lab-iva-ese
                end-if
      
           when 78-ID-ef-cod-iva                
                move "tivaese"   to como-file
                move "IV"        to tbliv-codice1
                inquire ef-cod-iva,  value in tbliv-codice2
                call "zoom-gt"   using como-file, record-tbliv
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move tbliv-codice2   to ef-cod-iva-buf
                   initialize lab-cod-iva-buf
                   inspect tbliv-descrizione1 replacing trailing 
                                                 spaces by low-value
                   string  tbliv-descrizione1 delimited by low-value
                           " "                delimited by size
                           tbliv-descrizione2 delimited by size
                           into lab-cod-iva-buf
                   end-string
                   display ef-cod-iva lab-cod-iva
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
                   string  tblpa-descrizione1 delimited low-value
                           " "                delimited size
                           tblpa-descrizione2 delimited size
                           into lab-pag-buf
                   end-string
                   display ef-pag lab-pag
                end-if

           when 78-ID-ef-abi
           when 78-ID-ef-cab                          
                move "ABI"   to como-file              
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

           when 78-ID-ef-prov-r
                move "tprov"     to como-file
                inquire ef-prov-r,  value in prv-codice
                call "zoom-gt"   using como-file, prv-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move prv-codice to ef-prov-r-buf
                   display ef-prov-r
                   move "tprov" to nome-file
                   perform RELAZIONI-RECAPITI
                end-if

           when 78-ID-ef-nazione-r
                move "tnazioni"  to como-file            
                inquire ef-nazione-r, value in naz-codice
                call "zoom-gt"   using como-file, naz-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move naz-codice      to ef-nazione-r-buf
                   move naz-descrizione to lab-nazione-r-buf
                   display ef-nazione-r lab-nazione-r
                end-if

           when 78-ID-ef-cod-da
                set cli-tipo-c to true
                inquire ef-cod-da, value in cli-codice
                move   "clienti-CF-all"  to     como-file
                call   "zoom-gt"  using  como-file, cli-rec
                                 giving stato-zoom
                cancel "zoom-gt"
                if stato-zoom = 0
                   move cli-codice to ef-cod-da-buf
                   display ef-cod-da
                 end-if

           when 78-ID-ef-cod-a
                set cli-tipo-c to true
                inquire ef-cod-a, value in cli-codice
                move   "clienti-CF-all"  to     como-file
                call   "zoom-gt"  using  como-file, cli-rec
                                 giving stato-zoom
                cancel "zoom-gt"
                if stato-zoom = 0
                   move cli-codice to ef-cod-a-buf
                   display ef-cod-a
                 end-if

           when 78-ID-ef-st-tipo
                move "ttipocli"  to como-file         
                inquire ef-st-tipo,    value in tcl-codice
                call "zoom-gt"   using como-file, tcl-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move tcl-codice      to ef-st-tipo-buf
                   move tcl-descrizione to lab-st-tipo-buf
                   display ef-st-tipo lab-st-tipo
                end-if

           when 78-ID-ef-st-gdo
                move "tgrupgdo"  to como-file         
                inquire ef-st-gdo,     value in gdo-codice
                call "zoom-gt"   using como-file, gdo-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move gdo-codice       to ef-st-gdo-buf
                   move gdo-intestazione to lab-st-gdo-buf
                   display ef-st-gdo lab-st-gdo
                end-if

           when 78-ID-ef-st-age
                move "agenti"       to como-file         
                inquire ef-st-age,  value in age-codice
                call "zoom-gt"      using como-file, age-rec
                                   giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move age-codice   to ef-st-age-buf
                   move age-ragsoc-1 to lab-st-age-buf
                   display ef-st-age lab-st-age
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
      *****          move 0 to mod-gdo
                move 0 to mod-agente
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
LUBEXX     inquire ef-tipo,  value tcl-codice.
LUBEXX     if tcl-codice not = save-ttipocli-privato

LUBEXX*****              inquire ef-codfis value cf-codice-fiscale
LUBEXX*****              inquire ef-piva   value cf-piva

              call   "codfis"   using link-codfis
              cancel "codfis"

      *       FLAG CHE INDICA SE IL CF O PIVA E' CORRETTO
              if cf-risultato not = 0
                 set errori to true 
              end-if
           end-if.      

      ***---
       CHECK-CODFIS-PIVA-D.
LUBEXX     inquire ef-tipo,  value tcl-codice.
LUBEXX     if tcl-codice not = save-ttipocli-privato
              inquire ef-piva-d   value cf-piva

              call   "codfis"   using link-codfis
              cancel "codfis"

      *       FLAG CHE INDICA SE IL CF O PIVA E' CORRETTO
              if cf-risultato not = 0
                 set errori to true 
              end-if
           end-if.

      ***---
       CHECK-EXISTENCE-RECAPITI.
           set ExistRecord to false.
           inquire ef-ragsoc-2-r,  value in ef-ragsoc-2-r-buf.
           inquire ef-indirizzo-r, value in ef-indirizzo-r-buf.
           inquire ef-localita-r,  value in ef-localita-r-buf.
           inquire ef-cap-r,       value in ef-cap-r-buf.
           inquire ef-prov-r,      value in ef-prov-r-buf.
           inquire ef-nazione-r,   value in ef-nazione-r-buf.

           if ef-ragsoc-2-r-buf    not = spaces or
              ef-indirizzo-r-buf   not = spaces or
              ef-localita-r-buf    not = spaces or
              ef-cap-r-buf         not = spaces or
              ef-prov-r-buf        not = spaces or
              ef-nazione-r-buf     not = spaces
              set ExistRecord to true
           end-if.

      ***---
       CHECK-INSERIMENTO.
      * modifica la status-bar
      * controllo se da inserimento scorro. allora devo segnalare che
      * sono tornato in modifica.
           inquire tool-modifica, value in mod.

           evaluate mod
           when 1   set StatusModifica to true
           when 0   set StatusVisua to true   
           end-evaluate.
      
           perform STATUS-BAR-MSG.

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

           when 78-ID-ef-st-tipo
                inquire ef-st-tipo, value in tcl-codice
                if tcl-codice = spaces
                   move "Tutte le tipologie" to lab-st-tipo-buf
                else
                   read ttipocli no lock
                        invalid
                        display message"Tipologia Cliente NON valida"
                                  title tit-err
                                   icon 2
                        set errori to true
                        move spaces to tcl-descrizione
                   end-read
                   move tcl-descrizione to lab-st-tipo-buf
                end-if
                display lab-st-tipo
   
           when 78-ID-ef-st-gdo
                inquire ef-st-gdo, value in gdo-codice
                if gdo-codice = spaces
                   move "Tutti i codici GDO" to lab-st-gdo-buf
                else
                   read tgrupgdo no lock
                        invalid
                        display message "Gruppo GDO NON valido"
                                  title tit-err
                                   icon 2
                        set errori to true
                        move spaces to tcl-descrizione
                   end-read
                   move gdo-intestazione to lab-st-gdo-buf
                end-if     
                display lab-st-gdo
   
           when 78-ID-ef-st-age
                inquire ef-st-age, value in age-codice
                if age-codice = 0
                   move "Tutti gli agenti" to lab-st-age-buf
                else
                   read agenti no lock
                        invalid
                        display message "Agente NON valido"
                                  title tit-err
                                   icon 2
                        set errori to true
                        move spaces to age-ragsoc-1
                   end-read
                   move age-ragsoc-1 to lab-st-age-buf
                end-if
                display lab-st-age
   
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
                        if ef-cod-a-buf < ef-cod-da-buf
                           display message "Intervallo Cliente errato"
                                     title tit-err
                                      icon 2
                           set errori to true
                           move 78-ID-ef-cod-da to control-id
                        end-if
                   end-evaluate
                end-if

           end-evaluate.

           if errori
              perform CANCELLA-COLORE
              move 4 to accept-control
           end-if.
             
      ***---
       COLORE.
           modify form1-gd-1, start-y = riga, y = riga,
                              start-x = 1,    x = 6,
                              region-color = 257.

      ***---
       COLORE-EDI.
           modify form1-gd-1-edi, start-y = riga, y = riga,
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
                             title tit-err
                              icon 2
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

           |78-ID-ef-mail è l'ID del campo ef-mail
           when 78-ID-ef-mail
                if ef-mail-buf = spaces 
                   perform SCARICA-COMBO-INVIO
                   if invio-manuale
                      set errori to true
                      move 78-ID-ef-mail to control-id
                      display message "E-mail obbligatoria"
                                title tit-err
                                 icon mb-warning-icon
                   end-if
                end-if

           |78-ID-ef-tipo è l'ID del campo ef-tipo
           when 78-ID-ef-tipo
                inquire ef-tipo, value in cli-tipo
                move "ttipocli" to nome-file
                perform RELAZIONI-CLIENTI
      *****          move 0 to mod-gdo
                move 0 to mod-agente
                if not trovato
                   set errori to true
                   move 78-ID-ef-tipo to control-id
                   display message box "Tipo cliente NON valido"
                           title = tit-err
                           icon mb-warning-icon
                else                                            
                   perform VALORIZZA-ENABLE-VARIABLE
                end-if     
      *****          if mod-gdo = 0
      *****             initialize ef-gdo-buf
      *****                        lab-gdo-buf
      *****             modify ef-gdo,  value = ef-gdo-buf
      *****             modify lab-gdo, title = lab-gdo-buf
      *****          end-if
      *****          modify ef-gdo, enabled = mod-gdo
                if mod-agente = 0
                   initialize ef-agente-buf
                              lab-agente-buf
                   modify ef-agente,   value = ef-agente-buf
                   modify lab-agente,  title = lab-agente-buf
                   initialize ef-agente2-buf
                              lab-agente2-buf
                   modify ef-agente2,   value = ef-agente2-buf
                   modify lab-agente2,  title = lab-agente2-buf
                end-if
                modify ef-agente,  enabled = mod-agente
                modify ef-agente2, enabled = mod-agente

           |78-ID-ef-gdo è l'ID del campo ef-gdo
           when 78-ID-ef-gdo
                if ef-gdo-buf not = spaces
                   move "tgrupgdo" to nome-file
                   perform RELAZIONI-CLIENTI
                   if not trovato
                      set errori to true  
                   end-if
                else
                   inquire ef-tipo, value in ef-tipo-buf
                   move "ttipocli" to nome-file
                   perform RELAZIONI-CLIENTI
                   if trovato and tcl-gdo-si
                      set errori to true
                   end-if
                   move spaces to lab-gdo-buf
                   display lab-gdo
                end-if

                if errori
                   move 78-ID-ef-gdo to control-id
                   display message box "Gruppo GDO NON valido"
                           title = tit-err
                           icon mb-warning-icon
                end-if

           |78-ID-ef-vettore è l'ID del campo ef-vettore
           when 78-ID-ef-vettore
                move spaces to lab-vettore-buf
                move "tvettori" to nome-file 
                perform RELAZIONI-CLIENTI

                if vet-codice not = 0
                   if not trovato
                      set errori to true  
                      move 78-ID-ef-vettore to control-id
                      display message box "Vettore NON valido"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if

           |78-ID-ef-agente è l'ID del campo ef-agente
           when 78-ID-ef-agente
                if ef-agente-buf not = 0
                   move "agenti" to nome-file
                   perform RELAZIONI-CLIENTI
                   if not trovato
                      set errori to true  
                   end-if
                else
                   inquire ef-tipo, value in ef-tipo-buf
                   move "ttipocli" to nome-file
                   perform RELAZIONI-CLIENTI
                   if trovato and tcl-agente-si
                      set errori to true
                   end-if
                   move spaces to lab-agente-buf
                   display lab-agente
                end-if

                if errori
                   move 78-ID-ef-agente to control-id
                   display message box "Agente NON valido"
                           title = tit-err
                           icon mb-warning-icon
                end-if

           |78-ID-ef-agente è l'ID del campo ef-agente
           when 78-ID-ef-agente2
                if ef-agente2-buf not = 0
                   move "agenti2" to nome-file
                   perform RELAZIONI-CLIENTI
                   if not trovato
                      set errori to true  
                   end-if
                else
                   inquire ef-tipo, value in ef-tipo-buf
                   move "ttipocli" to nome-file
                   perform RELAZIONI-CLIENTI
                   if trovato and tcl-agente-si
                      set errori to true
                   end-if
                   move spaces to lab-agente2-buf
                   display lab-agente2
                end-if

                if errori
                   move 78-ID-ef-agente2 to control-id
                   display message box "Agente NON valido"
                           title = tit-err
                           icon mb-warning-icon
                end-if

           when 78-ID-cbo-sost-art
                perform SCARICA-COMBO-SOST
                move sost to cli-sost
                if cli-sost not = old-cli-sost and vecchio
                   move 9 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"

                   if not Passwd-StatusOk
                      move old-cli-sost to sost
                      perform CARICA-COMBO-SOST
                   end-if
               end-if
                                                      
           |78-ID-cbo-stato è l'ID del campo cbo-stato
           when 78-ID-cbo-stato
                perform SCARICA-COMBO-STATO
                move stato   to  cli-stato
                if cli-stato not = old-cli-stato
                   move 7 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"

                   if not Passwd-StatusOk
                      if nuovo
                         set old-cli-bloccato to true
                      end-if
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

           |78-ID-cbo-stato è l'ID del campo cbo-stato
           when 78-ID-cbo-stato
                perform SCARICA-COMBO-CAUSALE
                move cau-blocco to cli-cau-blocco

           |78-ID-ef-codfis è l'ID del campo ef-codfis
           when 78-ID-ef-codfis
                inquire ef-nazione, value in ef-nazione-buf
                if ef-nazione-buf = "ITA"
                   inquire ef-codfis, value  in ef-codfis-buf
                   if ef-codfis-buf not = spaces
                      if form1-radio-1-buf = 1
LUBEXX                   inquire ef-codfis value cf-codice-fiscale
LUBEXX                   set cf-contr-gen to true
                         perform CHECK-CODFIS-PIVA
                         if errori
                            move 78-ID-ef-codfis to control-id
                            display message MSG-Codifce-Fiscale-errato
                                      title tit-err
                                       icon 2
                         end-if
                      end-if
                   end-if
                end-if   

           when 78-ID-ef-esi
                inquire ef-esi,   value in ef-esi-buf
                if ef-esi-buf not = "D" and not = "I" and not = "S"
                   display message "Valori ammessi: "
                            x"0d0a""D = differita"
                            x"0d0a""I = Immediata"
                            x"0d0a""S = Scissione di pagamenti"
                             title tit-err
                              icon 2
                   set errori to true
                end-if

           |78-ID-ef-piva è l'ID del campo ef-piva
           when 78-ID-ef-piva
                inquire ef-piva,   value in ef-piva-buf
                inquire ef-codfis, value in ef-codfis-buf
                if ef-piva-buf   not = spaces
                   inquire ef-nazione, value in ef-nazione-buf
                   if ef-nazione-buf = "ITA"
                      set  cf-partita-iva to true
LUBEXX                move ef-piva-buf    to cf-piva
                      perform CHECK-CODFIS-PIVA
                      if errori
                         move 78-ID-ef-piva to control-id 
                         display message MSG-Partita-IVA-errata
                                   title tit-err
                                    icon 2
                      end-if
                   end-if
                else
                   if ef-codfis-buf = spaces
                      set errori to true
                      move 78-ID-ef-codfis to control-id
                      display message MSG-codfis-piva-mancanti
                                title tit-err
                                 icon 2
                   end-if
                end-if

           |78-ID-ef-gg è l'ID del campo ef-gg
           when 78-ID-ef-gg
                inquire ef-gg,    value in cli-gg-dilazione
                inquire chk-fido, value in chk-fido-buf
                if chk-fido-buf = 1 and cli-gg-dilazione = 0
                   set errori to true
                   display message "Giorni di dilazione obbligatori"
                            x"0d0a""in quanto gestisce il fido"
                             title tit-err
                              icon 2
                end-if

           |78-ID-ef-iva-ese è l'ID del campo ef-iva-ese
           when 78-ID-ef-iva-ese
                move "tivaese-ese" to nome-file
                perform RELAZIONI-CLIENTI

                if tbliv-codice2 not = spaces
                   if not trovato
                      if ef-iva-ese-buf not = spaces 
                         set errori to true
                         move 78-ID-ef-iva-ese to control-id
                         display message "Codice ESENZIONE NON valido"
                                 title = tit-err
                                 icon mb-warning-icon
                      end-if
                   else
                      if tbliv-percentuale not = 0
                         set errori to true
                         move 78-ID-ef-iva-ese to control-id
                         display message box "Codice IVA NON valido "
                                             "in quanto NON ESENTE"
                                 title = tit-err
                                 icon mb-warning-icon
                      else
                         display message "Confermi il Codice Esenzione "
                                         ef-iva-ese-buf " ?"
                                 title = tit-err
                                 type mb-yes-no
                                 icon mb-warning-icon
                                 giving scelta
                         if scelta = mb-no
                            set errori to true
                            move 78-ID-ef-iva-ese to control-id
                         end-if
                      end-if
                   end-if
LUBEXX          end-if

           |78-ID-ef-cod-iva è l'ID del campo ef-cod-iva
           when 78-ID-ef-cod-iva
                inquire ef-iva-ese value in ef-iva-ese-buf
                inquire ef-cod-iva value in ef-cod-iva-buf
                if ef-iva-ese-buf not = spaces and
                   ef-cod-iva-buf not = spaces
                   display message 
                           "Possibile solo Esenzione o Codice IVA"
                             title tit-err
                              icon 2
                   set errori to true
                   move 78-ID-ef-iva-ese to control-id
                else
                   move "tivaese" to nome-file
                   perform RELAZIONI-CLIENTI
                   
                   if tbliv-codice2 not = spaces
                      if not trovato
                         if ef-cod-iva-buf not = spaces 
                            set errori to true
                            move 78-ID-ef-cod-iva to control-id
                            display message"Codice ESENZIONE NON valido"
                                    title = tit-err
                                    icon mb-warning-icon
                         end-if
                      else
                         display message "Confermi il Codice "
                                            ef-cod-iva-buf " ?"
                                   title tit-err
                                    type mb-yes-no
                                    icon mb-warning-icon
                                    giving scelta
                            if scelta = mb-no
                               set errori to true
                               move 78-ID-ef-cod-iva to control-id
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
                else
                   move ef-pag-buf to pgb-codice
                   read pagbloc no lock
                        invalid continue
                    not invalid
                        set errori to true
                        move 78-ID-ef-pag to control-id
                        display message "Codice pagamento BLOCCATO"
                                  title tit-err
                                   icon mb-warning-icon
                   end-read
                end-if  

           |78-ID-ef-fido-data è l'ID del campo ef-fido-data
           when 78-ID-ef-fido-data
                inquire ef-fido-data,  value in como-data
                inquire ef-fido,       value in cli-fido
                inquire ef-fido-extra, value in cli-fido-extra
                inquire ef-fide,       value in cli-fidejussione
                inquire ef-PFA,        value in cli-PFA
      *****          if como-data = 0
      *****             if cli-fido > 0
      *****                perform DATE-FORMAT
      *****                move como-data to ef-fido-data-buf
      *****             end-if
      *****          else
                if como-data not = 0
                   perform DATE-FORMAT
                   move como-data to ef-fido-data-buf
                end-if
                display ef-fido-data
                                    
           when 78-ID-ef-data-fido-extra
                inquire ef-data-fido-extra,  value in como-data
                if como-data not = 0
                   perform DATE-FORMAT
                   move como-data to ef-data-fido-extra-buf
                end-if
                display ef-data-fido-extra
           
           when 78-ID-ef-grade
                inquire ef-grade, value in ef-grade-buf
                move ef-grade-buf to cli-grade
                if cli-grade > 10
                   set errori to true
                   display message "Valore GRADE massimo: 10"
                             title tit-err
                              icon 2
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
                      display message box 
                              "Inserimento Indirizzo destino mancante"
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
                      display message box 
                           "Inserimento indirizzo destinatario mancante"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if

           |78-ID-ef-cap-d è l'ID del campo ef-cap-d
           when 78-ID-ef-cap-d
                perform SCARICA-COMBO-DESTINI
                if ( tot-righe > 1 or riga-nuova = 1 ) and attivo-d
                   inquire ef-cap-d, value in ef-cap-d-buf
                   if ef-cap-d-buf = spaces 
                      set errori to true  
                      move 78-ID-ef-cap-d to control-id
                      display message box 
                              "Inserimento CAP destinatario " des-prog 
                              " mancante"
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
                            display message "CAP destinatario " des-prog
                                            " NON valido"
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
                      display message box 
                            "Inserimento località destinatario mancante"
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
                   if not trovato
                      set errori to true  
                      move 78-ID-ef-vettore-d to control-id
                      display message box "Vettore destino NON valido"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if    
                   
           |78-ID-cbo-stato-d è l'ID del campo cbo-stato-d
           when 78-ID-cbo-stato-d
                perform SCARICA-COMBO-DESTINI
                move stato-destini   to  des-stato
                if des-stato not = old-des-stato
                   move 1 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"

                   if not Passwd-StatusOk
                      move old-des-stato to stato-destini
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

           |78-ID-ef-piva è l'ID del campo ef-piva
           when 78-ID-ef-piva-d
                inquire ef-piva-d,   value in ef-piva-d-buf
                if ef-piva-d-buf   not = "00000000000"
                   inquire ef-nazione-d, value in ef-nazione-d-buf
                   if ef-nazione-d-buf = "ITA"
                      set cf-partita-iva to true
                      perform CHECK-CODFIS-PIVA-D
                      if errori
                         move 78-ID-ef-piva-d to control-id 
                         display message box MSG-Partita-IVA-errata
                                 title = tit-err
                                 icon mb-warning-icon
                      end-if
                   end-if
                end-if

           |---> Inizio controlli sul file dei RECAPITI
           |78-ID-ef-ragsoc-1-r è l'ID del campo ef-ragsoc-1-r
           when 78-ID-ef-ragsoc-1-r
                perform CHECK-EXISTENCE-RECAPITI
                inquire ef-ragsoc-1-r, value in ef-ragsoc-1-r-buf
                if ExistRecord and ef-ragsoc-1-r-buf = spaces
                   set errori to true  
                   move 78-ID-ef-ragsoc-1-r to control-id
                   display message box 
                           "Inserimento Ragione Sociale "
                           "per recapito fatture mancante"              
                           title = tit-err
                           icon mb-warning-icon
                end-if

           |78-ID-ef-prov-r è l'ID del campo ef-prov-r
           when 78-ID-ef-prov-r                 
                inquire ef-ragsoc-1-r, value in ef-ragsoc-1-r-buf
                if ef-ragsoc-1-r-buf = spaces
                   set ExistRecord to false
                else                         
                   set ExistRecord to true
                end-if
                move "tprov" to nome-file
                perform RELAZIONI-RECAPITI
                if not trovato
                   if   ef-prov-r-buf not = spaces or
                      ( ef-prov-r-buf     = spaces and ExistRecord )
                      set errori to true
                      move 78-ID-ef-prov-r to control-id
                      display message box 
                      "Provincia per recapito fatture NON valida"
                              title = tit-err
                              icon mb-warning-icon
                      
                   end-if
                end-if   

           |78-ID-ef-nazione-r è l'ID del campo ef-nazione-r
           when 78-ID-ef-nazione-r              
                inquire ef-ragsoc-1-r, value in ef-ragsoc-1-r-buf
                move "tnazioni" to nome-file
                perform RELAZIONI-RECAPITI
                if not trovato
                   if   ef-nazione-r-buf not = spaces or
                      ( ef-nazione-r-buf     = spaces and ExistRecord )
                      set errori to true
                      move 78-ID-ef-nazione-r to control-id
                      display message box 
                              "Nazione per recapito fatture NON valida"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if         
                                                      
           |78-ID-cbo-invio è l'ID del campo cbo-invio
           when 78-ID-cbo-invio
                perform SCARICA-COMBO-INVIO
                inquire ef-iva-ese, value in ef-iva-ese-buf
      *****          if ef-iva-ese-buf not = spaces and invio-postel
      *****             set errori to true
      *****             move 78-ID-cbo-invio to control-id
      *****             display message "Invio POSTEL impossibile in quanto "
      *****                             "presente codice esenzione IVA"
      *****                     title = tit-err
      *****                     icon mb-warning-icon
      *****          end-if


           |78-ID-ef-data-dich è l'ID del control ef-data-dich
           when 78-ID-ef-data-dich
                inquire ef-data-dich, value in ef-data-dich-buf
                move ef-data-dich-buf to como-data
                if como-data not = 0
                   perform DATE-FORMAT
                   move como-data to ef-data-dich-buf
                end-if
                display ef-data-dich    

           |ef-data-reg è l'ID del control ef-data-reg
           when 78-ID-ef-data-reg
                inquire ef-data-reg, value in ef-data-reg-buf
                move ef-data-reg-buf to como-data
                if como-data not = 0
                   perform DATE-FORMAT
                   move como-data to ef-data-reg-buf
                end-if
                display ef-data-reg  

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

           if des-ragsoc-1 not = old-des-ragsoc-1 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-ragsoc-1-d è l'ID del campo ef-ragsoc-1-d
              move 78-ID-ef-ragsoc-1-d to store-id 
           end-if.

           if des-ragsoc-2 not = old-des-ragsoc-2 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-ragsoc-2-d è l'ID del campo ef-ragsoc-2-d
              move 78-ID-ef-ragsoc-2-d to store-id 
           end-if.

           if des-indirizzo not = old-des-indirizzo and SiSalvato
              set NoSalvato to true
              |78-ID-ef-indirizzo-d è l'ID del campo ef-indirizzo-d
              move 78-ID-ef-indirizzo-d to store-id 
           end-if.

           if des-cap not = old-des-cap and SiSalvato
              set NoSalvato to true

              |78-ID-ef-cap-d è l'ID del campo ef-cap-d
              move 78-ID-ef-cap-d to store-id 
           end-if.

           if des-localita not = old-des-localita and SiSalvato
              set NoSalvato to true
              |78-ID-ef-localita-d è l'ID del campo ef-localita-d
              move 78-ID-ef-localita-d to store-id 
           end-if.

           if des-prov not = old-des-prov and SiSalvato
              set NoSalvato to true
              |78-ID-ef-prov-d è l'ID del campo ef-prov-d
              move 78-ID-ef-prov-d to store-id 
           end-if.

           if des-nazione not = old-des-nazione and SiSalvato
              set NoSalvato to true
              |78-ID-ef-nazione-d è l'ID del campo ef-nazione-d
              move 78-ID-ef-nazione-d to store-id 
           end-if.

           if des-telef-1 not = old-des-telef-1 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-telef-1-d è l'ID del campo ef-telef-1-d
              move 78-ID-ef-telef-1-d to store-id 
           end-if.

           if des-telef-2 not = old-des-telef-2 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-telef-2-d è l'ID del campo ef-telef-2-d
              move 78-ID-ef-telef-2-d to store-id 
           end-if.

           if des-fax not = old-des-fax and SiSalvato
              set NoSalvato to true
              |78-ID-ef-fax-d è l'ID del campo ef-fax-d
              move 78-ID-ef-fax-d to store-id 
           end-if.

           if des-mail not = old-des-mail and SiSalvato
              set NoSalvato to true
              |78-ID-ef-mail-d è l'ID del campo ef-mail-d
              move 78-ID-ef-mail-d to store-id 
           end-if.

           if des-cod-ditta not = old-des-cod-ditta and SiSalvato
              set NoSalvato to true
              |78-ID-ef-cod-ditta-d è l'ID del campo ef-cod-ditta-d
              move 78-ID-ef-cod-ditta-d to store-id 
           end-if. 

           if old-des-tipo-art = 0
              move des-tipo-art to old-des-tipo-art
           end-if.
           if des-tipo-art not = old-des-tipo-art and SiSalvato
              set NoSalvato to true
              move 78-ID-cbo-tipo-art to store-id 
           end-if.         

           if des-referente not = old-des-referente and SiSalvato
              set NoSalvato to true
              |78-ID-ef-referente-d è l'ID del campo ef-referente-d
              move 78-ID-ef-referente-d to store-id 
           end-if.

           if des-cig not = old-des-cig and SiSalvato
              set NoSalvato to true
              |78-ID-ef-cig-d è l'ID del campo ef-cig-d
              move 78-ID-ef-cig-d to store-id 
           end-if.

           if des-piva not = old-des-piva and SiSalvato
              set NoSalvato to true
              |78-ID-ef-piva-d è l'ID del campo ef-piva-d
              move 78-ID-ef-piva-d to store-id 
           end-if.

           if des-vettore not = old-des-vettore and SiSalvato
              set NoSalvato to true
              |78-ID-ef-vettore-d è l'ID del campo ef-vettore-d
              move 78-ID-ef-vettore-d to store-id 
           end-if.

           if des-deposito-utf not = old-des-deposito-utf and SiSalvato
              set NoSalvato to true
              |78-ID-chk-deposito-utf è l'ID del campo chk-deposito-utf
              move 78-ID-chk-deposito-utf to store-id 
           end-if.                

           if des-superamento-500 not = old-des-superamento-500 and 
           SiSalvato
              set NoSalvato to true
              |78-ID-chk-superamento-d è l'ID del campo chk-superamento-d
              move 78-ID-chk-superamento-d to store-id 
           end-if.  

           if des-invio-fatt not = old-des-invio-fatt and SiSalvato
              set NoSalvato to true
              |78-ID-chk-invio-d è l'ID del campo chk-invio-d
              move 78-ID-chk-invio-d to store-id
           end-if.
           
           if not-note-1 of note1 not = old-not-note-1 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-1 è l'ID del campo 78-ID-ef-note-1
              move 78-ID-ef-note-1 to store-id 
           end-if.      

           if not-data of note1 not = old-not-data and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-data è l'ID del campo 78-ID-ef-note-data
              move 78-ID-ef-note-data to store-id 
           end-if.

           if not-note-2 of note1 not = old-not-note-2 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-2 è l'ID del campo 78-ID-ef-note-2
              move 78-ID-ef-note-2 to store-id 
           end-if.

           if not-note-3 of note1 not = old-not-note-3 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-3 è l'ID del campo 78-ID-ef-note-3
              move 78-ID-ef-note-3 to store-id 
           end-if.

           if not-note-4 of note1 not = old-not-note-4 and SiSalvato
              set NoSalvato to true
              |78-ID-ef-note-4 è l'ID del campo 78-ID-ef-note-4
              move 78-ID-ef-note-4 to store-id 
           end-if.

      ***---
       CURRENT-RECORD.
           perform RIEMPI-CHIAVE.
           set tutto-ok   to true.
           set ReadSecca  to true.
           set cli-tipo-c to true.
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
      *****              move 0 to mod-gdo
                    move 0 to mod           
                    move 1 to mod-k         
                    move 78-ID-ef-codice to control-id    
                    move 4 to accept-control
                 end-if            
                 perform LEGGI-EDI

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
       LEGGI-EDI.                                  
           move cli-codice to ecd-cli-codice.
           move 0          to ecd-prg-destino.
           read EDI-clides no lock
                invalid 
                initialize ecd-cliente
           end-read.
           move ecd-id-edi       to ef-id-edi-buf.
           move ecd-q-id-edi     to ef-q-id-edi-buf.
           move ecd-codforn      to ef-codforn-edi-buf.
           move ecd-q-codforn    to ef-q-codforn-edi-buf.
           move ecd-piva         to ef-piva-c-edi-buf.
           move ecd-ragsoc-c     to ef-ragsoc-c-edi-buf.
           move ecd-indirizzo-c  to ef-ind-c-edi-buf.
           move ecd-citta-c      to ef-citta-c-edi-buf.
           move ecd-prov-c       to ef-prov-c-edi-buf.
           move ecd-cap-c        to ef-cap-c-edi-buf.
           move ecd-cod-dest     to ef-codcli-edi-buf.
           move ecd-q-cod-dest   to ef-q-codcli-edi-buf.
                                      
           move ecd-import-importi  to chk-imp-i-buf.
           move ecd-import-articoli to chk-imp-a-buf.
           move ecd-export-imposte  to chk-exp-i-buf.

      ***---
       DISPLAY-CAMPI-GRID.
           display ef-ragsoc-1-d  ef-ragsoc-2-d
                   ef-indirizzo-d ef-cap-d
                   ef-localita-d  ef-prov-d 
                   ef-nazione-d   ef-telef-1-d
                   ef-telef-2-d   ef-fax-d ef-cod-ditta-d
                   ef-mail-d      ef-referente-d  ef-piva-d ef-cig-d
                   ef-vettore-d   chk-deposito-utf
                   chk-superamento-d chk-invio-d
                   cbo-tipo-art-d.

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
                   ef-tel-2, ef-fax, ef-mail, ef-url, ef-cod-ditta
                   ef-tipo, ef-gdo, chk-utf, ef-referente, ef-piva-d,
                   ef-vettore, chk-inoltro, chk-superamento, ef-agente, 
                   ef-agente2, cbo-stato, ef-note, ef-note-agg,
                   pb-note, ef-codfis, ef-piva, ef-iva-ese, ef-cod-iva,
                   ef-pag, chk-spost-ago, chk-spost-dic, 
                   ef-fido, ef-fido-data, ef-data-fido-extra, ef-cab, 
                   ef-fido-extra, ef-fide, ef-pfa, ef-pfa-perce
                   ef-abi, pb-nota-pie, lab-agente, lab-vettore, 
                   lab-tipo, lab-gdo, lab-nazione, lab-citta, 
                   lab-regione, ef-ragsoc-1-r, ef-ragsoc-2-r, 
                   ef-indirizzo-r, ef-localita-r, ef-cap-r, 
                   chk-fido, ef-gg, rb-pers, rb-soc, ef-cig-d,
                   ef-prov-r, ef-nazione-r, lab-citta-r, lab-regione-r, 
                   lab-nazione-r, ef-dich-n, ef-data-dich, ef-data-reg,
                   ef-reg-n, TOOL-ESCI, TOOL-NUOVO, TOOL-CANCELLA, 
                   TOOL-SALVA, TOOL-ANTEPRIMA, TOOL-MODIFICA, cbo-invio
                   TOOL-STAMPA, TOOL-CERCA, TOOL-SELEZIONA, Form1-Pb-1a, 
                   Form1-Pb-1b, Form1-Pb-1c, Form1-Pb-1d, TOOL-ORD
                   cbo-cau-blocco cbo-sost-art cbo-tipo-art
                   chk-escludi-e
                   chk-intera-e
                   chk-accorpa-e
                   chk-saldo-banco-e
                   chk-saldo-promo-e
                   ef-gg-e pb-copia
                   chk-invio-bolle-EDI
                   chk-destino-auto
           display ef-id-edi ef-q-id-edi ef-codforn-edi ef-q-codforn-edi
                   ef-piva-c-edi ef-ragsoc-c-edi ef-ind-c-edi
                   ef-citta-c-edi ef-prov-c-edi ef-cap-c-edi
                   ef-codcli-edi ef-q-codcli-edi ef-esi.
           display ef-dest-edi ef-q-dest-edi ef-ragsoc-d-edi
                   ef-ind-d-edi ef-citta-d-edi ef-prov-d-edi
                   ef-cap-d-edi.
           display chk-imp-i chk-imp-a chk-exp-i.
           perform DISPLAY-CAMPI-GRID.
           perform DISPLAY-CAMPI-NOTE.
           if mod = 0
              modify ef-note, read-only
           else      
              modify ef-note, not read-only
           end-if.

      ***---
       INIT.
           move 0 to StatusHelp.
           move 0 to LastPrg.
           move 0 to WrkData.
           move 1 to mod-k.
           set StatusVisua to true.  
           set DestinoCambiato to false.
           set PrgCambiato to false.

           |Riempio il tabellino contenente i riferimenti agli ID
           |relativi ad ogni pagina del tab-control
           |specifico per i pgm. aventi Tab-Control
           move 78-ID-ef-ragsoc-1       to min-id(1).
           move 78-ID-ef-abi            to max-id(1).
           move 78-ID-form1-gd-1        to min-id(2).
           move 78-ID-ef-note-4         to max-id(2).
           move 78-ID-ef-ragsoc-1-r     to min-id(3).
           move 78-ID-ef-reg-n          to max-id(3).
           move 78-ID-gd-destini-e      to min-id(4).
           move 78-ID-gd-evasioni       to max-id(4).
           move 78-ID-gd-prg            to min-id(5).
           move 78-ID-gd-prg            to max-id(5).
           move 78-ID-ef-id-edi         to min-id(6).
           move 78-ID-ef-cap-d-edi      to max-id(6).
           |*******

           perform RIEMPI-COMBO-STATO.

           perform RIEMPI-COMBO-DESTINI.
           perform RIEMPI-COMBO-INVIO.
           perform RIEMPI-COMBO-CAUSALE.
           perform RIEMPI-COMBO-SOST.
           perform RIEMPI-COMBO-TIPO-ART.
           perform RIEMPI-COMBO-TIPO-ART-D.

           move "Attivo"                 to cbo-stato-buf.
           move "Attivo"                 to cbo-stato-d-buf.
           move "Manuale"                to cbo-invio-buf.
           move space                    to cbo-cau-blocco-buf
           move "Nessuna"                to cbo-sost-art-buf
           move "Gruppi"                 to cbo-tipo-art-buf
           move "Gruppi"                 to cbo-tipo-art-d-buf
           modify cbo-stato,       value cbo-stato-buf.
           modify cbo-stato-d,     value cbo-stato-d-buf.
           modify cbo-invio,       value cbo-invio-buf.
           modify cbo-cau-blocco,  value cbo-cau-blocco-buf.
           modify cbo-sost-art,    value cbo-sost-art-buf.
           modify cbo-tipo-art,    value cbo-tipo-art-buf.
           modify cbo-tipo-art-d,  value cbo-tipo-art-d-buf.


           move      0 to old-not-data.
           move spaces to old-not-note-1
                          old-not-note-2
                          old-not-note-3
                          old-not-note-4.

      ***---
       INIT-OLD-REC.
           initialize old-cli-rec  
                      old-des-rec
                      old-rec-rec
                      old-not-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           set Attivo             to true.
           move "N"               to cli-utf.

           move stato           to old-cli-stato.
                               
           move "N" to old-cli-utf.
           move "N" to old-cli-inoltro.        
           move "N" to old-cli-superamento-500.
           move "N" to old-cli-spost-ric-agosto.
           move "N" to old-cli-spost-ric-dicembre.
           move "N" to old-des-deposito-utf.
           move "N" to old-des-superamento-500.
           move "N" to old-des-invio-fatt.
           move "A" to old-des-stato.
           move "M" to old-rec-invio.

           set tipo-art-gruppi to true.
           perform SCARICA-COMBO-TIPO-ART-D.
           move des-tipo-art to old-des-tipo-art.


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
           modify form1-gd-1-edi, reset-grid = 1.
           perform FORM1-GD-1-EDI-CONTENT.      
                                  
           move cli-codice to des-codice.
           move low-value  to des-prog.
           perform RESET-GRIGLIA.

           modify gd-destini-e, reset-grid = 1.
           perform GD-DESTINI-E-CONTENT.
                    
           move 0             to col-codice-e.
           move cli-ragsoc-1  to col-destino-e.
           move cli-indirizzo to col-indirizzo-e.
           move cli-localita  to col-localita-e.

           modify gd-destini-e(2, 1), cell-data col-codice-e
           modify gd-destini-e(2, 2), cell-data col-destino-e   
           modify gd-destini-e(2, 3), cell-data col-indirizzo-e
           modify gd-destini-e(2, 4), cell-data col-localita-e
           modify gd-destini-e(2), 
                  row-font  = Large-Font,
                  row-color = 5.

           move 0 to LastPrg.
           set tutto-ok to true.
           start destini key is >= des-chiave 
                 invalid continue
             not invalid
                 modify form1-gd-1,   mass-update = 1
                 modify gd-destini-e, mass-update = 1
                 perform varying riga from 2 by 1 until 1 = 2
                    read destini next no lock 
                         at end exit perform 
                    end-read
                    if des-codice not = cli-codice exit perform end-if

                    if LastPrg = 0
                       move cli-codice to ecd-cli-codice
                       move des-prog   to ecd-prg-destino
                       read EDI-clides no lock
                            invalid 
                            initialize ecd-destino
                       end-read
                       move ecd-cod-consegna   to ef-dest-edi-buf
                       move ecd-q-cod-consegna to ef-q-dest-edi-buf
                       move ecd-ragsoc-d       to ef-ragsoc-d-edi-buf
                       move ecd-indirizzo-d    to ef-ind-d-edi-buf
                       move ecd-citta-d        to ef-citta-d-edi-buf
                       move ecd-prov-d         to ef-prov-d-edi-buf
                       move ecd-cap-d          to ef-cap-d-edi-buf
                       display ef-dest-edi ef-q-dest-edi ef-ragsoc-d-edi
                               ef-ind-d-edi ef-citta-d-edi ef-prov-d-edi
                               ef-cap-d-edi
                    end-if

                    perform MOVE-NOTE
                    move des-prog      to col-prog LastPrg  
                                          col-codice-e
                    move des-ragsoc-1  to col-ragsoc       
                                          col-destino-e
                    move des-indirizzo to col-indirizzo
                                          col-indirizzo-e
                    move des-localita  to col-localita    
                                          col-localita-e
                    move des-cap       to col-cap
                    move des-prov      to col-prov
                    modify form1-gd-1(riga, 1), cell-data col-prog
                    modify form1-gd-1(riga, 2), cell-data col-ragsoc   
                    modify form1-gd-1(riga, 3), cell-data col-indirizzo
                    modify form1-gd-1(riga, 4), cell-data col-cap
                    modify form1-gd-1(riga, 5), cell-data col-localita  
               
                    modify form1-gd-1(riga, 6), cell-data col-prov  

                    modify form1-gd-1-edi(riga, 1), 
                           cell-data col-prog
                    modify form1-gd-1-edi(riga, 2), 
                           cell-data col-ragsoc   
                    modify form1-gd-1-edi(riga, 3), 
                           cell-data col-indirizzo
                    modify form1-gd-1-edi(riga, 4), 
                           cell-data col-cap
                    modify form1-gd-1-edi(riga, 5), 
                           cell-data col-localita  
                    modify form1-gd-1-edi(riga, 6), 
                           cell-data col-prov
                    
                    modify gd-destini-e(riga + 1, 1), 
                           cell-data col-codice-e
                    modify gd-destini-e(riga + 1, 2), 
                           cell-data col-destino-e   
                    modify gd-destini-e(riga + 1, 3), 
                           cell-data col-indirizzo-e
                    modify gd-destini-e(riga + 1, 4), 
                           cell-data col-localita-e
                                                    
                    move des-ragsoc-2        to hidden-ragsoc-2 
                    move des-nazione         to hidden-nazione  
                    move des-telef-1         to hidden-telef-1  
                    move des-telef-2         to hidden-telef-2  
                    move des-fax             to hidden-fax      
                    move des-mail            to hidden-mail     
                    move des-cod-ditta       to hidden-cod-ditta
                    move des-referente       to hidden-referente
                    move des-cig             to hidden-cig
                    move des-piva            to hidden-piva
                    move des-vettore         to hidden-vettore  
                    move des-deposito-utf    to hidden-deposito-utf
                    move des-superamento-500 to hidden-superamento
                    move des-invio-fatt      to hidden-invio
                    move des-saldi-banco     to hidden-saldi-banco
                    move des-saldi-promo     to hidden-saldi-promo
                    move des-note-bolla-1    to hidden-note-bolla-1
                    move des-note-bolla-2    to hidden-note-bolla-2

                    move des-stato     to hidden-stato
                    move des-tipo-art  to hidden-tipo-art
                 
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
                           hidden-data hidden-deposito-utf
                    modify form1-gd-1(riga, 16) 
                           hidden-data hidden-superamento
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
                           hidden-data hidden-invio    
                    modify form1-gd-1(riga, 24) 
                           hidden-data hidden-piva
                    modify form1-gd-1(riga, 25) 
                           hidden-data hidden-cod-ditta
                    modify form1-gd-1(riga, 26) 
                           hidden-data hidden-saldi-banco
                    modify form1-gd-1(riga, 27) 
                           hidden-data hidden-saldi-promo
                    modify form1-gd-1(riga, 28)  
                           hidden-data hidden-tipo-art
                    modify form1-gd-1(riga, 29)  
                           hidden-data hidden-note-bolla-1
                    modify form1-gd-1(riga, 30)  
                           hidden-data hidden-note-bolla-2
                    modify form1-gd-1(riga, 31) 
                           hidden-data hidden-cig
          
              end-perform   
             
              modify form1-gd-1,   mass-update = 0   
              modify gd-destini-e, mass-update = 0   

              if cli-codice not = 0
                 inquire gd-destini-e, cursor-y in riga-d
                 move riga-d to event-data-2
                 perform SPOSTAMENTO-DESTINI-E
                 inquire gd-destini-e(riga-d, 1), 
                         cell-data in col-codice-e
                 move col-codice-e to ecd-destino-OLD
                 perform EVA-TO-SCREEN
                 perform CLI-PRG-TO-SCREEN
              end-if
              
              move 2 to riga
              perform VALORE-RIGA
              perform ROW-TO-ENTRY
              perform COLORE
           end-start.

      ***---
       LOOP-DELETE-DESTINI.
           move cli-codice to des-codice.
           move low-value  to des-prog.

           start destini key is >= des-chiave
                 invalid continue
             not invalid
                 initialize tab-dati-creazione
                 perform varying idx from 1 by 1
                           until 1 = 2
                    read destini next at end exit perform end-read
                    move des-data-creazione   to el-tab-data(idx)       
                 
                    move des-ora-creazione    to el-tab-ora(idx)
                    move des-utente-creazione to el-tab-utente(idx)
                    if des-codice not = cli-codice exit perform end-if
                    delete destini record invalid continue end-delete
                    move des-codice to not-codice of note1
                    move des-prog   to not-prog   of note1
                    delete note1 record invalid continue end-delete
                 end-perform
           end-start.

      ***---
       LOOP-SCRIVI-DESTINI.
           move cli-codice to des-codice.
           perform X-Y.

           move 0 to des-prog.
           move riga to store-riga.
           perform varying riga from 2 by 1
                     until riga > tot-righe
              perform VALORE-RIGA
              accept como-ora from time
              if nuovo
                 move data-oggi to des-data-creazione
                 move como-ora  to des-ora-creazione 
                 move user-codi to des-utente-creazione
                 move data-oggi to not-data-creazione    of note1
                 move como-ora  to not-ora-creazione     of note1
                 move user-codi to not-utente-creazione  of note1
              else
                 subtract 1 from riga giving idx
                 move el-tab-data(idx)   to des-data-creazione  
                 move el-tab-ora(idx)    to des-ora-creazione   
                 move el-tab-utente(idx) to des-utente-creazione
                    
                 move data-oggi to des-data-ultima-modifica
                 move como-ora  to des-ora-ultima-modifica
                 move user-codi to des-utente-ultima-modifica
                 move data-oggi to not-data-ultima-modifica    of note1
                 move como-ora  to not-ora-ultima-modifica     of note1
                 move user-codi to not-utente-ultima-modifica  of note1

              end-if
              perform MOVE-DATI       
              perform PAGE-2-BUF-TO-FLD

              write des-rec invalid rewrite des-rec end-write
              perform CHECK-ESISTENZA-NOTE
              if ExistRecord        
                 move des-codice to not-codice of note1
                 write not-rec of note1 invalid rewrite not-rec of note1
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
       LOOP-DELETE-EVACLIDES.
           move low-value  to ecd-rec-OLD.
           move cli-codice to ecd-cliente-OLD.
           start evaclides key >= ecd-chiave-OLD
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read evaclides next at end exit perform end-read
                    if ecd-cliente-OLD not = cli-codice
                       exit perform
                    end-if
                    if ecd-destino-OLD not = 0
                       move ecd-cliente-OLD to des-codice
                       move ecd-destino-OLD to des-prog
                       read destini no lock
                            invalid delete evaclides record
                       end-read
                    end-if
                 end-perform
           end-start.

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
      *              if cli-bloccato
      *                 move 1   to mod-blocco
      *              else
      *                 move zero   to mod-blocco
      *              end-if
                    set StatusModifica to true
                    perform STATUS-BAR-MSG
                 end-if
              else
                 move 1 to mod
                 perform SALV-MOD            
                 move 0 to mod
                 move 1 to mod-k
      *           move zero   to mod-blocco
                 if errori
                    move 1 to mod
                    move 0 to mod-k
      *              if cli-bloccato
      *                 move 1   to mod-blocco
      *              else
      *                 move zero   to mod-blocco
      *              end-if
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
           move ef-mail-d-buf      to hidden-mail.
           move ef-cod-ditta-d-buf to hidden-cod-ditta.
           move ef-referente-d-buf to hidden-referente.
           move ef-cig-d-buf       to hidden-cig.
           move ef-piva-d-buf      to hidden-piva.
           move ef-vettore-d-buf   to hidden-vettore.

           if riga-nuova = 1
              add 1         to LastPrg
              move LastPrg  to col-prog des-prog
           else

              move des-prog to col-prog
           end-if.

           perform SCARICA-COMBO-DESTINI.
           move stato-destini      to hidden-stato.

           perform SCARICA-COMBO-TIPO-ART-D.
           move tipo-art to hidden-tipo-art. 

           if hidden-stato = "D" |Disattivo
              move 0 to mod-destini
           else
              move 1 to mod-destini
           end-if.
                                
           move ef-note-1-buf      to hidden-note-1.

           move ef-note-data-buf   to como-data.
           perform DATE-TO-FILE.
           move como-data          to hidden-data-note.

           move ef-note-2-buf      to hidden-note-2.
           move ef-note-3-buf      to hidden-note-3.
           move ef-note-4-buf      to hidden-note-4.

           if chk-deposito-utf-buf  = 1
              move "S" to hidden-deposito-utf
           else                          
              move "N" to hidden-deposito-utf
           end-if.

           if chk-superamento-d-buf  = 1
              move "S" to hidden-superamento
           else                          
              move "N" to hidden-superamento
           end-if.

           if chk-invio-d-buf  = 1
              move "S" to hidden-invio
           else
              move "N" to hidden-invio
           end-if.

           modify form1-gd-1(riga, 1)    cell-data col-prog.
           modify form1-gd-1(riga, 2)    cell-data col-ragsoc.
           modify form1-gd-1(riga, 3)    cell-data col-indirizzo.
           modify form1-gd-1(riga, 4)    cell-data col-cap.
           modify form1-gd-1(riga, 5)    cell-data col-localita.
           modify form1-gd-1(riga, 6)    cell-data col-prov.
           modify form1-gd-1(riga, 7)  hidden-data hidden-ragsoc-2.
           modify form1-gd-1(riga, 8)  hidden-data hidden-nazione.
           modify form1-gd-1(riga, 9)  hidden-data hidden-telef-1.
           modify form1-gd-1(riga, 10) hidden-data hidden-telef-2.
           modify form1-gd-1(riga, 11) hidden-data hidden-fax.
           modify form1-gd-1(riga, 12) hidden-data hidden-mail.
           modify form1-gd-1(riga, 13) hidden-data hidden-referente.
           modify form1-gd-1(riga, 14) hidden-data hidden-vettore.
           modify form1-gd-1(riga, 15) hidden-data hidden-deposito-utf.
           modify form1-gd-1(riga, 16) hidden-data hidden-superamento.
           modify form1-gd-1(riga, 17) hidden-data hidden-stato.
           modify form1-gd-1(riga, 18) hidden-data hidden-note-1.
           modify form1-gd-1(riga, 19) hidden-data hidden-data-note.
           modify form1-gd-1(riga, 20) hidden-data hidden-note-2.
           modify form1-gd-1(riga, 21) hidden-data hidden-note-3.
           modify form1-gd-1(riga, 22) hidden-data hidden-note-4.
           modify form1-gd-1(riga, 23) hidden-data hidden-invio. 
           modify form1-gd-1(riga, 24) hidden-data hidden-piva.
           modify form1-gd-1(riga, 25) hidden-data hidden-cod-ditta.
           modify form1-gd-1(riga, 26) hidden-data hidden-saldi-banco.
           modify form1-gd-1(riga, 27) hidden-data hidden-saldi-promo.
           modify form1-gd-1(riga, 28) hidden-data hidden-tipo-art.   
           modify form1-gd-1(riga, 29) hidden-data hidden-note-bolla-1.
           modify form1-gd-1(riga, 30) hidden-data hidden-note-bolla-2.
           modify form1-gd-1(riga, 31) hidden-data hidden-cig.

           move 0 to riga-nuova.

      ***---
       MOVE-DATI.
           move col-ragsoc       to ef-ragsoc-1-d-buf.
           move hidden-ragsoc-2  to ef-ragsoc-2-d-buf.
           move col-indirizzo    to ef-indirizzo-d-buf.
           move col-cap          to ef-cap-d-buf.
           move col-localita     to ef-localita-d-buf.
           move col-prov         to ef-prov-d-buf.
           move hidden-nazione   to ef-nazione-d-buf.
           move hidden-telef-1   to ef-telef-1-d-buf.
           move hidden-telef-2   to ef-telef-2-d-buf.
           move hidden-fax       to ef-fax-d-buf.
           move hidden-mail      to ef-mail-d-buf.
           move hidden-cod-ditta to ef-cod-ditta-d-buf.
           move hidden-referente to ef-referente-d-buf.
           move hidden-cig       to ef-cig-d-buf.
           move hidden-piva      to ef-piva-d-buf.
           move hidden-vettore   to ef-vettore-d-buf.

           move col-prog         to des-prog.
           
           if hidden-deposito-utf = "S"
              move 1 to chk-deposito-utf-buf
           else
              move 0 to chk-deposito-utf-buf
           end-if.

           if hidden-superamento = "S"
              move 1 to chk-superamento-d-buf
           else                          
              move 0 to chk-superamento-d-buf
           end-if.

           if hidden-invio = "S"
              move 1 to chk-invio-d-buf
           else
              move 0 to chk-invio-d-buf
           end-if.

           move hidden-stato to stato-destini.
           perform CARICA-COMBO-DESTINI.
           if mod-campi = 1
              evaluate true
              when bloccato-d  move 1 to mod-destini
              when attivo-d    move 1 to mod-destini
              when disattivo-d move 0 to mod-destini
              end-evaluate
           end-if.
           
           move hidden-tipo-art to tipo-ART.
           perform CARICA-COMBO-TIPO-ART-D.
                  
           move hidden-data-note to como-data.
           perform DATE-TO-SCREEN.
           move como-data      to  ef-note-data-buf.

           move hidden-note-1  to  ef-note-1-buf.
           move hidden-note-2  to  ef-note-2-buf.
           move hidden-note-3  to  ef-note-3-buf.
           move hidden-note-4  to  ef-note-4-buf.

           if old-des-deposito-utf = spaces
              move "N" to old-des-deposito-utf 
           end-if.

           if old-des-superamento-500 = spaces
              move "N" to old-des-superamento-500
           end-if.

           if old-des-invio-fatt = spaces
              move "N" to old-des-invio-fatt
           end-if.
                              
           move 0 to riga-nuova.

           move "tprov"    to nome-file.
           perform RELAZIONI-DESTINI.
           move "tnazioni" to nome-file.
           perform RELAZIONI-DESTINI.
           move "tvettori" to nome-file.
           perform RELAZIONI-DESTINI.

      ***---
       MOVE-NOTE.
           move des-codice    to not-codice of note1
           move des-prog      to not-prog   of note1
           read note1 
                invalid move spaces   to hidden-note-1 
                        move 0        to hidden-data-note
                        move spaces   to hidden-note-2
                        move spaces   to hidden-note-3
                        move spaces   to hidden-note-4
            not invalid move not-note-1 of note1 to hidden-note-1
                        move not-data   of note1 to hidden-data-note
                        move not-note-2 of note1 to hidden-note-2
                        move not-note-3 of note1 to hidden-note-3
                        move not-note-4 of note1 to hidden-note-4
           end-read.

      ***---
       ORDINAMENTO.
           inquire tool-ord, value in OrderBy.

           if OrderBy = 0
              perform DATASET1-CHANGETO-KEY1
           else
              perform DATASET1-CHANGETO-KEY2
           end-if.
           perform CURRENT-RECORD.

      ***---
       NUOVO.
           perform SALV-MOD.

           if tutto-ok
LUBEXX        |Solo un utente alla volta può inserire
LUBEXX        move 78-gclienti to lck-nome-pgm
LUBEXX        read lockfile no lock
LUBEXX             invalid
LUBEXX             move 78-gclienti to lck-nome-pgm
LUBEXX             move "I"         to lck-operazione
LUBEXX             move user-codi   to lck-utente-creazione
LUBEXX             accept lck-ora-creazione  from time
LUBEXX             accept lck-data-creazione from century-date
LUBEXX             write lck-rec invalid continue end-write
LUBEXX             read  lockfile record lock end-read
LUBEXX         not invalid
LUBEXX             display message
LUBEXX                     "Operazione impossibile!!!"
LUBEXX              x"0d0a""Funzione d'inserimento già "
LUBEXX                     "utilizzata da " lck-utente-creazione
LUBEXX                        title tit-err
LUBEXX                         icon 3
LUBEXX             set errori to true
LUBEXX        end-read
LUBEXX        if tutto-ok
                 |Specifico per i pgm. aventi Tab-Control
                 move 1 to Screen1-Ta-1-TAB-VALUE
                 perform SCREEN1-TA-1-TABCHANGE
                 |******

                 move 78-ID-ef-ragsoc-1 to control-id
                 move 4 to accept-control
                 move 1 to mod 
                 move 0 to mod-k
                 modify tool-modifica,  value = mod
                 perform CANCELLA-COLORE
                 perform FORM1-CLEAR

                 perform VALORIZZA-NUOVO
                 perform INIT-OLD-REC

                 |Specifico del pgm. gclienti
                 move "Attivo"            to cbo-stato-buf
                 modify  cbo-stato,    value cbo-stato-buf

      *****           move "Postel"            to cbo-invio-buf
      *****           modify  cbo-invio,    value cbo-invio-buf

                 move "Automatica"        to cbo-sost-art-buf
                 modify  cbo-sost-art, value cbo-sost-art-buf

                 move 1 to mod-campi mod-destini
                 move "I" to ef-esi-buf
      *****           move 0 to mod-gdo
                 perform DISPLAY-SCREEN

                 set StatusIns to true
                 perform STATUS-BAR-MSG
                 unlock clienti all records

LUBEXX        end-if
           end-if.

      ***---
       PAGE-2-BUF-TO-FLD.
           move ef-ragsoc-1-d-buf   to des-ragsoc-1.
           move ef-ragsoc-2-d-buf   to des-ragsoc-2.
           move ef-indirizzo-d-buf  to des-indirizzo.
           move ef-cap-d-buf        to des-cap.
           move ef-localita-d-buf   to des-localita.
           move ef-prov-d-buf       to des-prov.
           move ef-nazione-d-buf    to des-nazione.
           move ef-telef-1-d-buf    to des-telef-1.
           move ef-telef-2-d-buf    to des-telef-2.
           move ef-fax-d-buf        to des-fax.
           move ef-mail-d-buf       to des-mail.
           move ef-referente-d-buf  to des-referente.
           move ef-cig-d-buf        to des-cig.
           move ef-piva-d-buf       to des-piva.
           move ef-vettore-d-buf    to des-vettore.
           move ef-cod-ditta-d-buf  to des-cod-ditta.
           
           if chk-deposito-utf-buf = 1
              move "S" to des-deposito-utf
           else
              move "N" to des-deposito-utf
           end-if.

           if chk-superamento-d-buf = 1
              move "S" to des-superamento-500
           else
              move "N" to des-superamento-500
           end-if.

           if chk-invio-d-buf = 1
              move "S" to des-invio-fatt
           else
              move "N" to des-invio-fatt
           end-if.

           perform SCARICA-COMBO-DESTINI.
           move stato-destini       to des-stato.

           perform SCARICA-COMBO-TIPO-ART-D.
           move tipo-art to des-tipo-art.
                                                        
           move hidden-note-bolla-1 to des-note-bolla-1.
           move hidden-note-bolla-2 to des-note-bolla-2.

           move des-prog            to not-prog of note1.

           move ef-note-data-buf to como-data.
           perform DATE-TO-FILE.
           move como-data        to not-data of note1.

           move ef-note-1-buf       to not-note-1 of note1.
           move ef-note-2-buf       to not-note-2 of note1.
           move ef-note-3-buf       to not-note-3 of note1.
           move ef-note-4-buf       to not-note-4 of note1.

      ***---
       PAGE-2-CLEAR.
           move spaces to lab-citta-d-buf   lab-regione-d-buf
                          lab-nazione-d-buf lab-vettore-d-buf.
           display lab-citta-d   lab-regione-d
                   lab-nazione-d lab-vettore-d.
           move 0 to riga-nuova.
           move 0 to ef-note-data-buf.
           initialize ef-ragsoc-1-d-buf  ef-ragsoc-2-d-buf
                      ef-indirizzo-d-buf ef-cap-d-buf
                      ef-localita-d-buf  ef-prov-d-buf
                      ef-nazione-d-buf   ef-telef-1-d-buf
                      ef-telef-2-d-buf   ef-fax-d-buf
                      ef-mail-d-buf      ef-referente-d-buf,
                      ef-piva-d-buf,     ef-vettore-d-buf,
                      ef-cod-ditta-d-buf, ef-cig-d-buf,
                      chk-deposito-utf-buf 
                      chk-superamento-d-buf, chk-invio-d-buf,
                      ef-note-1-buf      ef-note-2-buf
                      ef-note-3-buf      ef-note-4-buf
                      old-des-rec
                      old-not-rec
                      replacing numeric data by zeroes
                           alphanumeric data by spaces.
           move "00000000000" to ef-piva-d-buf.

           |Specifico del pgm. gclienti
           move "Attivo" to cbo-stato-d-buf.
           move "A"      to old-des-stato.
           move "N"      to old-des-deposito-utf.
           move "N"      to old-des-superamento-500.
           move "N"      to old-des-invio-fatt.
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
           when "anacap"      perform READ-ANACAP
           when "tvettori"    perform READ-TVETTORI
           when "tprov"       perform READ-TPROV
           when "tnazioni"    perform READ-TNAZIONI
           when "ttipocli"    perform READ-TTIPOCLI
           when "tgrupgdo"    perform READ-TGRUPGDO
           when "agenti"      perform READ-AGENTI
           when "agenti2"     perform READ-AGENTI
           when "tivaese-ese" perform READ-TIVAESE-ESE
           when "tivaese"     perform READ-TIVAESE
           when "tcodpag"     perform READ-TCODPAG
           when "ABI"         perform READ-ABI
           end-evaluate.

      ***---
       READ-ANACAP.   
           read anacap no lock
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
       READ-TTIPOCLI.
           read ttipocli no lock
                invalid  move spaces to tcl-descrizione
                         set trovato to false
           end-read.

      ***---
       READ-TGRUPGDO.
           read tgrupgdo no lock
                invalid  move spaces to gdo-intestazione
                         set trovato to false
           end-read.

      ***---
       READ-AGENTI.
           read agenti no lock
                invalid move spaces to age-ragsoc-1
                        set trovato to false
           end-read.

      ***---
       READ-TIVAESE-ESE.
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
       READ-TIVAESE.
           read tivaese no lock
                invalid 
                move spaces to tbliv-descrizione1
                move spaces to tbliv-descrizione2
                set trovato to false
            not invalid
                set trovato to true
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
           when "ttipocli"                
                move spaces to lab-tipo-buf
                move ef-tipo-buf to tcl-codice
                if tcl-codice not = space
                   perform RELATIONS
                   move tcl-descrizione to lab-tipo-buf
                end-if
                display lab-tipo
           when "tgrupgdo"             
                move spaces to lab-gdo-buf
                move ef-gdo-buf to gdo-codice
                if gdo-codice not = space
                   perform RELATIONS
                   move gdo-intestazione to lab-gdo-buf
                end-if
                display lab-gdo
           when "agenti"              
                move spaces to lab-agente-buf
                move ef-agente-buf to age-codice
                if age-codice not = 0
                   perform RELATIONS
                   move age-ragsoc-1 to lab-agente-buf
                else
                   set trovato to true
                end-if
                display lab-agente
           when "agenti2"              
                move spaces to lab-agente2-buf
                move ef-agente2-buf to age-codice
                if age-codice not = 0
                   perform RELATIONS
                   move age-ragsoc-1 to lab-agente2-buf
                else
                   set trovato to true
                end-if
                display lab-agente2
           when "tivaese-ese"                
                move spaces         to lab-iva-ese-buf
                move "IV"           to tbliv-codice1
                move ef-iva-ese-buf to tbliv-codice2
                if tbliv-codice2 not = spaces
                   perform RELATIONS
                   initialize lab-iva-ese-buf
                   inspect tbliv-descrizione1 replacing trailing spaces 
                                                        by low-value
                   string  tbliv-descrizione1 delimited by low-value
                           " "                delimited by size
                           tbliv-descrizione2 delimited by size
                           into lab-iva-ese-buf
                   end-string
                end-if
                display lab-iva-ese
           when "tivaese"
                move spaces         to lab-cod-iva-buf
                move "IV"           to tbliv-codice1
                move ef-cod-iva-buf to tbliv-codice2
                if tbliv-codice2 not = spaces
                   perform RELATIONS
                   initialize lab-iva-ese-buf
                   inspect tbliv-descrizione1 replacing trailing spaces 
                                                        by low-value
                   string  tbliv-descrizione1 delimited by low-value
                           " "                delimited by size
                           tbliv-descrizione2 delimited by size
                           into lab-cod-iva-buf
                   end-string
                end-if
                display lab-cod-iva
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
                else
                   |Il vettore non è più obbligatorio!
                   set trovato to true
                   move spaces to lab-vettore-d-buf
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
           end-evaluate.

      ***---
       RELAZIONI-RECAPITI.
           set trovato to false.

           evaluate nome-file
           when "tprov"         
                move spaces to lab-citta-r-buf
                move spaces to lab-regione-r-buf
                move ef-prov-r-buf to prv-codice
                if prv-codice not = space
                   perform RELATIONS
                   move prv-descrizione to lab-citta-r-buf
                   move reg-descrizione to lab-regione-r-buf
                end-if
                display lab-citta-r lab-regione-r
           when "tnazioni"              
                move spaces to lab-nazione-r-buf
                move ef-nazione-r-buf to naz-codice
                if naz-codice not = space
                   perform RELATIONS
                   move naz-descrizione to lab-nazione-r-buf
                end-if
                display lab-nazione-r
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
           if mod = 0 
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
                      until control-id > 78-id-ef-reg-n
              |non faccio i controlli sui destini
              |perchè ci pensa il salva della grid
              if control-id = 78-ID-ef-ragsoc-1-d
                 move 78-ID-ef-ragsoc-1-r to control-id
              end-if
              |||
              |Lascio alla fine il controllo sul cambio di stato
              if control-id = 78-ID-cbo-stato
                 add 1 to control-id
              end-if
              |||
              perform CONTROLLO
              if errori 
                 exit perform 
              end-if
           end-perform.
           
           if tutto-ok
              perform SCARICA-COMBO-STATO
              move stato   to  cli-stato
              if cli-attivo
                 |Se voglio attivare il cliente devo controllare che
                 |1. Se persona fisica abbia codice fiscale
                 |2. Se persona giuridica abbia codice fiscale e p.iva
                 |controllo solamente clienti italiani
                 if ef-nazione-buf not = "ITA" or
                    ef-tipo-buf = save-ttipocli-privato
                    continue
                 else
                    if Form1-RADIO-1-BUF = 1 |Persona fisica
                       if ef-codfis-buf = spaces
                          set errori to true
                          display message 
                          "Per salvare un cliente ATTIVO occorre "
                          "valorizzare il codice fiscale"
                                    title tit-err
                                     icon 2
                          perform CANCELLA-COLORE
                          move 78-ID-ef-codfis to control-id
                       end-if
                    else
                       if ef-codfis-buf = spaces or 
                          ef-piva-buf   = spaces
                          set errori to true
                          display message
                          "Per attivare il cliente occorre "
                          "valorizzare il codice fiscale e la P.IVA"
                                    title tit-err
                                     icon 2
                          perform CANCELLA-COLORE
                          move 78-ID-ef-codfis to control-id
                       end-if
                    end-if
                 end-if
              end-if
           end-if.

           if tutto-ok
              if cli-tipo not = old-cli-tipo and vecchio
                 move 17 to Passwd-password
                 call   "passwd" using Passwd-linkage
                 cancel "passwd"

                 if not Passwd-StatusOk
                    set errori to true
                    move old-cli-tipo to ef-tipo-buf
                    display ef-tipo                     
                    move "ttipocli" to nome-file
                    perform RELAZIONI-CLIENTI
                end-if                       

                perform VALORIZZA-ENABLE-VARIABLE
      *****          if mod-gdo = 0
      *****             initialize ef-gdo-buf
      *****                        lab-gdo-buf
      *****             modify ef-gdo,  value = ef-gdo-buf
      *****             modify lab-gdo, title = lab-gdo-buf
      *****          end-if
      *****          modify ef-gdo, enabled = mod-gdo
                if mod-agente = 0
                   initialize ef-agente-buf
                              lab-agente-buf
                   modify ef-agente,   value = ef-agente-buf
                   modify lab-agente,  title = lab-agente-buf
                   initialize ef-agente2-buf
                              lab-agente2-buf
                   modify ef-agente2,   value = ef-agente2-buf
                   modify lab-agente2,  title = lab-agente2-buf
                end-if
                modify ef-agente,  enabled = mod-agente
                modify ef-agente2, enabled = mod-agente
              end-if
           end-if.

           if tutto-ok
              if cli-fido-extra   not = old-cli-fido-extra   or
                 cli-fidejussione not = old-cli-fidejussione or
                 cli-PFA          not = old-cli-PFA          or
                 cli-fido         not = old-cli-fido
                 move 3 to Passwd-password
                 call   "passwd" using Passwd-linkage
                 cancel "passwd"

                 if not Passwd-StatusOk
                    set errori to true
                    display message 
                            "Occorre conoscere la password per variare"
                 "i valori del fido (Fido/Fido Extra/Fidejussione/PFA)"
                              title tit-err
                               icon 2
                    move 78-ID-ef-fido to store-id control-id 
                    perform CANCELLA-COLORE
                 end-if
              end-if
           end-if.

           |23/05/2012
           if tutto-ok
              perform SCARICA-COMBO-CAUSALE
              move cau-blocco to cli-cau-blocco
              perform SCARICA-COMBO-STATO
              if cli-stato = old-cli-stato
                 if cli-cau-blocco not = old-cli-cau-blocco
                    move 7 to Passwd-password
                    call   "passwd" using Passwd-linkage
                    cancel "passwd"

                    if not Passwd-StatusOk
                       set errori to true
                       display message "Occorre conoscere la password "
                                     "per variare la causale di blocco"
                                 title tit-err
                                  icon 2
                       move 78-ID-cbo-cau-blocco to store-id control-id 
                       perform CANCELLA-COLORE
                    end-if
                 end-if
              end-if
           end-if.

           if tutto-ok and nuovo and ef-piva-buf not = spaces
              call   "check-cli-piva" using ef-piva-buf codice-ed
              cancel "check-cli-piva"
              if codice-ed not = 0 and not = spaces
                 display message "E' già presente il cliente " codice-ed
                                 " con la stessa partita IVA."
                          X"0D0A""Confermi?"
                            title titolo
                             type mb-yes-no
                             icon 2
                          default mb-no
                           giving scelta
                 if scelta = mb-no
                    perform CANCELLA-COLORE
                    move 78-ID-ef-piva to store-id control-id
                    set errori to true
                 end-if
              end-if
           end-if.

           if tutto-ok
LUBEXX        if ef-nazione-buf not = "ITA" and 
                 ef-iva-ese-buf     = spaces
                 if old-cli-nazione not = ef-nazione-buf or
                    old-cli-iva-ese not = ef-iva-ese-buf
                    perform CANCELLA-COLORE
                    move 78-ID-ef-iva-ese to control-id
                    move 20 to Passwd-password
                    call   "passwd" using Passwd-linkage
                    cancel "passwd"
              
                    if not Passwd-StatusOk
LUBEXX                 set errori to true
LUBEXX                 display message "Codice Esenzione obbligatorio"
LUBEXX                                 " per clienti esteri"
LUBEXX                           title tit-err
LUBEXX                            icon 2
                    end-if
                 end-if
LUBEXX        end-if
           end-if
           |23/05/2012


LUBEXX*****           if tutto-ok perform SALVA-RIGA end-if.

           |...dopodiché riassegno a riga-nuova il suo valore originale,
           |mentre tot-righe può essre lasciato a 0 perchè
           |ricalcolato ogni volta che dev'essre testato
           move store-riga to riga-nuova.

LUBEXX     if tutto-ok perform SALVA-RIGA end-if.

           if tutto-ok
              move riga to store-riga
              perform X-Y
              perform varying riga from 2 by 1
                        until riga > tot-righe                      
                 inquire form1-gd-1(riga, 6), cell-data in col-prov
                 inquire form1-gd-1(riga, 4), cell-data in col-cap
                 if col-prov not = "EE"
                    move col-cap to anc-cap
                    read anacap no lock
                         invalid
                         inquire form1-gd-1(riga, 1), 
                                 cell-data in col-prog
                         if riga = store-riga
                            perform SCARICA-COMBO-DESTINI
                            if attivo-d
                               move "A" to hidden-stato
                            else
                               move "B" to hidden-stato
                            end-if
                         else
                            inquire form1-gd-1(riga, 17),
                                    hidden-data in hidden-stato
                         end-if
                         if hidden-stato = "A" 
                            set errori to true
                            move 78-ID-ef-cap-d to control-id
                            display message "CAP destinatario " col-prog
                                            " NON valido"
                                      title tit-err
                                       icon mb-warning-icon
                            exit perform
                         end-if
                    end-read
                 end-if
              end-perform                                            
              move store-riga to riga

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
                   
              initialize ecd-rec         
              move cli-codice to ecd-cli-codice
              move 0          to ecd-prg-destino
              move ef-id-edi-buf         to ecd-id-edi
              move ef-q-id-edi-buf       to ecd-q-id-edi
              move ef-codforn-edi-buf    to ecd-codforn
              move ef-q-codforn-edi-buf  to ecd-q-codforn
              move ef-piva-c-edi-buf     to ecd-piva        
              move ef-ragsoc-c-edi-buf   to ecd-ragsoc-c  
              move ef-ind-c-edi-buf      to ecd-indirizzo-c  
              move ef-citta-c-edi-buf    to ecd-citta-c    
              move ef-prov-c-edi-buf     to ecd-prov-c      
              move ef-cap-c-edi-buf      to ecd-cap-c        
              move ef-codcli-edi-buf     to ecd-cod-dest    
              move ef-q-codcli-edi-buf   to ecd-q-cod-dest
                                      
              move chk-imp-i-buf to ecd-import-importi
              move chk-imp-a-buf to ecd-import-articoli
              move chk-exp-i-buf to ecd-export-imposte

              write ecd-rec invalid rewrite ecd-rec end-write

              |Aggiorno i destini EDI con i dati del cliente
              start edi-clides key >= ecd-chiave
                    invalid continue
                not invalid 
                    perform until 1 = 2
                       read edi-clides next no lock 
                            at end exit perform 
                       end-read
                       |GESTIRE IL LOCK
                       if ecd-cli-codice not = cli-codice
                          exit perform
                       end-if
                       move ef-codcli-edi-buf to ecd-cod-dest
                       rewrite ecd-rec
                    end-perform
              end-start

              move  cli-codice   to not-codice of note
              move  0            to not-prog   of note
              move WrkCampi      to not-dati   of note
                                           
              accept como-ora from time
              if nuovo
                 move data-oggi to cli-data-creazione
                                   rec-data-creazione
                                   not-data-creazione of note

                 |Così appena dopo aver creato un record NUOVO
                 |metto a video la sua data di creazione
                 move cli-data-creazione to como-data
                 perform DATE-TO-SCREEN
                 move como-data          to lab-data-buf
                 display lab-data

                 move como-ora  to cli-ora-creazione
                                   rec-ora-creazione
                                   not-ora-creazione  of note
                 move user-codi to cli-utente-creazione
                                   rec-utente-creazione
                                   not-utente-creazione of note
              else
                 move data-oggi to cli-data-ultima-modifica
                                   rec-data-ultima-modifica
                                   not-data-ultima-modifica of note
                 move como-ora  to cli-ora-ultima-modifica
                                   rec-ora-ultima-modifica
                                   not-ora-ultima-modifica of note
                 move user-codi to cli-utente-ultima-modifica
                                   rec-utente-ultima-modifica
                                   not-utente-ultima-modifica of note
              end-if
              
              move cli-codice to rec-codice
              |19/05/2015: attivo su tutti
              set cli-gestione-fido-si to true
              |||||||
              write cli-rec invalid rewrite cli-rec end-write
              write rec-rec invalid rewrite rec-rec end-write
              perform PB-SALVA-EVA-PRESSED
              if PrgCambiato
                 perform CANCELLA-CLI-PRG
                 inquire gd-prg, last-row in tot-righe
                 perform varying riga from 2 by 1 
                           until riga > tot-righe
                    initialize cp-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces
                    perform VALORE-RIGA-PRG
                    set cp-tipo-C   to true
                    move cli-codice to cp-clifor
                    move col-art    to cp-articolo
                    move col-art    to cp-cod-articolo
                    move col-mag    to cp-cod-magazzino
                    move col-imb    to cp-tipo-imballo
                    move col-peso   to cp-peso
                    write cp-rec
                 end-perform
                 set PrgCambiato to false
              end-if

              |Specifico per GClienti mi riposiziono altrimenti
              |fallisce la read previous x' non fa la start
              set cli-tipo-c to true
              read clienti  no lock invalid continue end-read

              move WrkNote-1 to not-note-1 of note
              move WrkData   to como-data
              perform DATE-TO-FILE
              move como-data to not-data   of note
              move WrkNote-2 to not-note-2 of note
              move WrkNote-3 to not-note-3 of note
              move WrkNote-4 to not-note-4 of note

              if not-note-1 of note not = spaces or
                 not-data   of note not =      0 or
                 not-note-2 of note not = spaces or
                 not-note-3 of note not = spaces or
                 not-note-4 of note not = spaces
                 write not-rec of note 
                       invalid rewrite not-rec of note 
                 end-write
              else
                 delete note record invalid continue end-delete
              end-if

              if DestinoCambiato
                 |perform LOOP-DELETE-DESTINI
                 perform LOOP-SCRIVI-DESTINI
                 perform LOOP-DELETE-EVACLIDES
              else
                 perform X-Y
                 perform VALORE-RIGA
                 perform ROW-TO-ENTRY
              end-if

              initialize G2Agg-linkage
              set G2Agg-cli   to true
              move cli-codice to G2Agg-codice
              if nuovo set G2Agg-insert to true
              else     set G2Agg-update to true
              end-if
              call   "G2Agg" using G2Agg-linkage
              cancel "G2Agg"

              set vecchio to true
              perform TORNA-IN-VISUA
LUBEXX        perform DELETE-LOCKFILE

           end-if.
           perform DISPLAY-SCREEN.

       
      ***---
       SALVA-RIGA.
      * <TOTEM:PARA. SALVA-RIGA>
           if mod-campi = 0 exit paragraph end-if.
           
           perform X-Y.

LUBEXX     if tot-righe = 1 and riga-nuova = 0
LUBEXX        exit paragraph
LUBEXX     end-if.

           if riga-nuova = 1
              add 1 to tot-righe giving riga
LUBEXX     |else
LUBEXX     |   if tot-righe = 1
LUBEXX     |      move 1 to riga-nuova
LUBEXX     |   end-if
           end-if.

           perform varying control-id from 78-ID-ef-ragsoc-1-d by 1
                     until control-id > 78-ID-ef-cap-d-edi
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
           |Se ho cancellato vado direttamente su 
           |PRIMO senza controllare nessun OLD
           if key-status = 4
              exit paragraph
           end-if.
           perform FORM1-CONTROLLO-OLD.
           
           if mod = 1
              if DestinoCambiato or PrgCambiato
                 set NoSalvato to true
              end-if

              move WrkData to como-data
              perform DATE-TO-FILE
              move como-data to WrkData
                             
              if WrkNote-1  not = not-note-1 of note or
                 WrkData    not = not-data   of note or
                 WrkNote-2  not = not-note-2 of note or
                 WrkNote-3  not = not-note-3 of note or
                 WrkNote-4  not = not-note-4 of note
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
LUBEXX             if tutto-ok
LUBEXX                perform DELETE-LOCKFILE
LUBEXX             end-if
              when mb-no
                   continue
LUBEXX             perform DELETE-LOCKFILE
              when other
                   perform CANCELLA-COLORE

                   set errori to true
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
LUBEXX     else
LUBEXX        perform DELETE-LOCKFILE
           end-if.

      ***---
       SALV-MOD-RIGA.
           if mod-destini = 0 exit paragraph end-if.

           set tutto-ok to true.
           |Specifico del pgm. GClienti
           perform CONTROLLO-OLD-RIGA.

           if NoSalvato
              display message "Confermare la modifica dei destinatari?"
                            title titolo
                            type mb-yes-no-cancel 
                            giving scelta       
       
              evaluate scelta
              when mb-yes perform SALVA-RIGA
              when mb-no  perform CANCELLA-COLORE
              when other  perform CANCELLA-COLORE
                          set errori to true 
                          move 4 to accept-control
              end-evaluate

           end-if.

      ***---
       SELEZIONA.
           set cli-tipo-c to true.
           move   "clienti-all"  to como-file.
           call   "zoom-gt"   using como-file, cli-rec
                             giving stato-zoom.
           cancel "zoom-gt".
      
           if stato-zoom = 0
              if old-cli-chiave  not =  cli-chiave
                 move cli-chiave   to  save-chiave
                 move cli-ragsoc-1 to  save-ragsoc-K1
                 perform SALV-MOD
                 if tutto-ok
                    move save-ragsoc-K1 to cli-ragsoc-1
                    move save-chiave    to cli-chiave
                    move cli-codice     to codice-ed
                    move codice-ed      to ef-codice-buf   
                    call "C$JUSTIFY" using ef-codice-buf, "L"
                    move cli-ragsoc-1   to ef-ragsoc-1-buf 
                                             lab-des-buf
                    display ef-codice lab-des ef-ragsoc-1
                    set     ReadSecca    to true 
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
           inspect ef-codice-buf replacing leading zero by spaces.
           move ef-codice-buf to cli-ragsoc-1.

      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           call "C$JUSTIFY" using cli-ragsoc-1, "L".
                        
           set cli-tipo-c to true.
           start clienti  key >= cli-k1
                 invalid  continue
             not invalid  
                 read clienti next  
                 if cli-tipo-F
                    initialize cli-rec
                 end-if
           end-start.
           set cli-tipo-C to true.

           move "clienti-alfa-all" to como-file.
           call "zoom-gt"  using  como-file, cli-rec
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = ZERO
              move cli-codice     to codice-ed
              move codice-ed      to ef-codice-buf   
              call "C$JUSTIFY" using ef-codice-buf, "L"
              display ef-codice

              if cli-codice not = old-cli-codice
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
              set cli-tipo-c to true
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
           inquire form1-gd-1, last-row in tot-righe.
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
       SPOSTAMENTO-EDI.
           inquire form1-gd-1-edi, last-row in tot-righe.
           if event-data-2 >= 2 and
              event-data-2 <= tot-righe
              if riga not = event-data-2 or ForzaRiga
                 set ForzaRiga to false
                 if riga-nuova = 1
                    perform X-Y
                    modify form1-gd-1, record-to-delete = tot-righe + 1
                    move 0 to riga-nuova
                 end-if
      
                 set tutto-ok to true

                 move event-data-2 to riga
                                                                     
                 inquire form1-gd-1-edi (riga, 1), cell-data col-prog
                 move cli-codice to ecd-cli-codice
                 move col-prog   to ecd-prg-destino
                 read EDI-clides no lock
                      invalid         
                      inquire form1-gd-1-edi (riga, 2), 
                              cell-data col-ragsoc
                      inquire form1-gd-1-edi (riga, 3), 
                              cell-data col-indirizzo
                      inquire form1-gd-1-edi (riga, 4), 
                              cell-data col-cap
                      inquire form1-gd-1-edi (riga, 5), 
                              cell-data col-localita
                      inquire form1-gd-1-edi (riga, 6), 
                              cell-data col-prov
                      initialize ecd-destino
                      move "92"          to ecd-q-cod-consegna
                      move col-ragsoc    to ecd-ragsoc-d
                      move col-indirizzo to ecd-indirizzo-d
                      move col-localita  to ecd-citta-d
                      move col-prov      to ecd-prov-d
                      move col-cap       to ecd-cap-d
                 end-read
                 move ecd-cod-consegna   to ef-dest-edi-buf
                 move ecd-q-cod-consegna to ef-q-dest-edi-buf
                 move ecd-ragsoc-d       to ef-ragsoc-d-edi-buf
                 move ecd-indirizzo-d    to ef-ind-d-edi-buf
                 move ecd-citta-d        to ef-citta-d-edi-buf
                 move ecd-prov-d         to ef-prov-d-edi-buf
                 move ecd-cap-d          to ef-cap-d-edi-buf
                 display ef-dest-edi ef-q-dest-edi ef-ragsoc-d-edi
                         ef-ind-d-edi ef-citta-d-edi ef-prov-d-edi
                         ef-cap-d-edi

              end-if
            
              perform COLORE-EDI
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
           inquire form1-gd-1(riga, 1), cell-data   in col-prog.
           inquire form1-gd-1(riga, 2), cell-data   in col-ragsoc.
           inquire form1-gd-1(riga, 3), cell-data   in col-indirizzo.
           inquire form1-gd-1(riga, 4), cell-data   in col-cap.
           inquire form1-gd-1(riga, 5), cell-data   in col-localita.
           inquire form1-gd-1(riga, 6), cell-data   in col-prov.
           inquire form1-gd-1(riga, 7)  hidden-data in hidden-ragsoc-2.
           inquire form1-gd-1(riga, 8)  hidden-data in hidden-nazione.
           inquire form1-gd-1(riga, 9)  hidden-data in hidden-telef-1.
           inquire form1-gd-1(riga, 10) hidden-data in hidden-telef-2.
           inquire form1-gd-1(riga, 11) hidden-data in hidden-fax.
           inquire form1-gd-1(riga, 12) hidden-data in hidden-mail.
           inquire form1-gd-1(riga, 13) hidden-data in hidden-referente.
           inquire form1-gd-1(riga, 14) hidden-data in hidden-vettore.
           inquire form1-gd-1(riga, 15) 
                   hidden-data in hidden-deposito-utf.
           inquire form1-gd-1(riga, 16) 
                   hidden-data in hidden-superamento.
           inquire form1-gd-1(riga, 17) hidden-data in hidden-stato. 
           inquire form1-gd-1(riga, 18) hidden-data in hidden-note-1.
           inquire form1-gd-1(riga, 19) hidden-data in hidden-data-note.
           inquire form1-gd-1(riga, 20) hidden-data in hidden-note-2.
           inquire form1-gd-1(riga, 21) hidden-data in hidden-note-3.
           inquire form1-gd-1(riga, 22) hidden-data in hidden-note-4.
           inquire form1-gd-1(riga, 23) hidden-data in hidden-invio.
           inquire form1-gd-1(riga, 24) hidden-data in hidden-piva.
           inquire form1-gd-1(riga, 25) hidden-data in hidden-cod-ditta.
           inquire form1-gd-1(riga, 26) hidden-data hidden-saldi-banco.
           inquire form1-gd-1(riga, 27) hidden-data hidden-saldi-promo.
           inquire form1-gd-1(riga, 28) hidden-data in hidden-tipo-art.
           inquire form1-gd-1(riga, 29) hidden-data hidden-note-bolla-1.
           inquire form1-gd-1(riga, 30) hidden-data hidden-note-bolla-2.
           inquire form1-gd-1(riga, 31) hidden-data hidden-cig.

      ***---
       VALORIZZA-ENABLE-VARIABLE.
      *****     evaluate true
      *****
      *****     when tcl-gdo-si       move 1 to mod-gdo
      *****     when tcl-gdo-opz      move 1 to mod-gdo
      *****     when tcl-gdo-no       move 0 to mod-gdo
      *****
      *****     end-evaluate.
      *****
           evaluate true

           when tcl-agente-si    move 1 to mod-agente
           when tcl-agente-opz   move 1 to mod-agente
           when tcl-agente-no    move 0 to mod-agente

           end-evaluate.


      ***---
       VALORIZZA-NUOVO.
           move high-value to cli-codice.
           set cli-tipo-c to true.
           start clienti key <= cli-chiave
                 invalid move 1 to cli-codice
             not invalid
                 read clienti previous with no lock
                      at end  move 1 to cli-codice
                  not at end  add  1 to cli-codice
                 end-read
           end-start.
           move "00" to status-clienti.  

           modify gd-prg, reset-grid = 1.
           perform GD-PRG-CONTENT.

           initialize cli-dati 
                      des-dati
                      rec-dati
                      not-dati of note1 replacing numeric data by zeroes
                                             alphanumeric data by spaces
                                 
           perform FORM1-IUD-DISPLAY.
           
           set nuovo to true.    

           initialize not-dati of note1
                      not-dati of note replacing numeric data by zeroes
                                            alphanumeric data by spaces.
                                            
           move spaces to WrkNote-1.
           move spaces to WrkNote-2.
           move spaces to WrkNote-3.
           move spaces to WrkNote-4.
           move      0 to WrkData.
                        
           move 0 to LastPrg.


      ***---
       VALORIZZA-OLD.
           move cli-rec                 to old-cli-rec.
           move rec-rec                 to old-rec-rec.

           set vecchio                  to true.

           if old-cli-utf = space
              move "N" to old-cli-utf
           end-if.

           if old-cli-inoltro = space
              move "N" to old-cli-inoltro
           end-if.         
           
           if old-cli-superamento-500 = spaces
              move "N" to old-cli-superamento-500
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
           
           if old-rec-invio = spaces
              move "M" to old-rec-invio |MANUALE
           end-if.       
           
           if old-cli-invio-bolle-EDI = spaces
              move "N" to old-cli-invio-bolle-EDI 
           end-if.     
           
           if old-cli-destino-auto-EDI = spaces
              move "N" to old-cli-destino-auto-EDI
           end-if.

           evaluate CONTROL-ID
           when 78-ID-ef-prov
           when 78-ID-ef-nazione
           when 78-ID-ef-tipo
           when 78-ID-ef-gdo
           when 78-ID-ef-vettore
           when 78-ID-ef-agente
           when 78-ID-ef-agente2
           when 78-ID-ef-iva-ese
           when 78-ID-ef-cod-iva
           when 78-ID-ef-pag
           when 78-ID-ef-abi
           when 78-ID-ef-cab
           when 78-ID-ef-prov-d
           when 78-ID-ef-nazione-d
           when 78-ID-ef-vettore-d
           when 78-ID-ef-prov-r
           when 78-ID-ef-nazione-r
                move 1 to StatusHelp
           when other
                move 0 to StatusHelp
           end-evaluate.
           perform STATUS-HELP.

           move 0 to riga-nuova.
           set DestinoCambiato to false .
           set PrgCambiato to false.

            
      ***---
       VALORIZZA-OLD-PAGE-2.
           move des-rec             to old-des-rec.
           move not-rec of note1    to old-not-rec.

           move hidden-deposito-utf to old-des-deposito-utf.

           move hidden-superamento  to old-des-superamento-500.

           move hidden-invio        to old-des-invio-fatt.

           if old-des-deposito-utf = spaces 
              move "N" to old-des-deposito-utf
           end-if.

           if old-des-superamento-500 = spaces
              move "N" to old-des-superamento-500
           end-if.

           if old-des-invio-fatt = spaces
              move "N" to old-des-invio-fatt
           end-if.

           if old-des-stato = space
              move "A" to old-des-stato |Attivo (Default)
           end-if.

      ***---
       VERIFICA-PAGAMENTO-BANCARIO.
           set PagamentoBancario to false.
           move "tcodpag" to nome-file.
           perform RELAZIONI-CLIENTI.
           if trovato
              perform varying idx from 1 by 1 until idx > 36
                 if tblpa-codice-tr(idx) = "W"
                    set PagamentoBancario to true
                    exit perform
                 end-if
              end-perform
           end-if.

      ***---
       X-Y.
           inquire form1-gd-1, last-row in tot-righe, 
                               cursor-y in riga,
                               cursor-x in colonna.

      *******************************************************************
      *                  SEZIONE ENTRY_POINT & EVENTS                   *
      *******************************************************************

      ***---
       GCLIENTI-BEFORE-PROGRAM.
           perform CALCOLA-COLORE-TRASPARENTE.
           open  input tparamge.
           move  spaces   to tge-chiave.
           read  tparamge no lock invalid continue end-read.
           close tparamge.
           move tge-ttipocli-privato to save-ttipocli-privato.

      ***---
       SCR-STAMPA-BEFORE-ACCEPT.
           move 0               to ef-cod-da-buf.
           move 99999           to ef-cod-a-buf.
           move spaces          to ef-st-tipo-buf.
           move space           to ef-st-gdo-buf.
           move 0               to ef-st-age-buf.
           move "Blank = TUTTI" to lab-st-tipo-buf.
           move "Blank = TUTTI" to lab-st-gdo-buf.
           move "0 = TUTTI"     to lab-st-age-buf.
           move 0               to ef-des-da-buf.
           move 99999           to ef-des-a-buf.
           move 0               to e-st-des.
           move 0               to e-st-note.
           move 0               to stampa-tipo-des.
           move 0               to stampa-tipo-note.
           move 1               to e-st-cli.
           display scr-stampa.

           modify chk-st-cli,  bitmap-number = 2.
           modify chk-st-des,  bitmap-number = 1.
           modify chk-st-note, bitmap-number = 1.

           modify chk-excel,   bitmap-number = 1.
           modify chk-excel-2, bitmap-number = 3.
           modify chk-excel-3, bitmap-number = 3.

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
                 if e-st-cli  = 1 perform STAMPA-CLIENTI end-if
                 if e-st-des  = 1 perform STAMPA-DESTINI end-if
                 if e-st-note = 1 perform STAMPA-NOTE    end-if

                 perform CANCELLA-COLORE
                 move 100 to control-id
                 move   4 to accept-control
                 modify pb-sib, bitmap-number = 2
              end-if
           end-if.

      ***---
       PB-STAMPA-PRESSED.
           perform CANCELLA-COLORE.
           if nuovo
              display message "Occorre confermare il Cliente"
                        title titolo
                         icon 2
           else
              call "W$MOUSE" using set-mouse-shape, wait-pointer
              call   "st-cli-det" using cli-codice
              cancel "st-cli-det"
              call "W$MOUSE" using set-mouse-shape, arrow-pointer
           end-if.
           move 760 to control-id.
           move 4   to accept-control.
           modify pb-stampa, bitmap-number = 2.

      ***---
       GCLIENTI-BEFORE-ACCEPT.
           perform INIT.
           move 1 to e-stampa pagina.
           perform ABILITA-TOOLBAR.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG.

           accept data-oggi from century-date.

           modify gd-evasioni, reset-grid = 1.
           perform GD-EVASIONI-CONTENT.
           move low-value to tpa-rec.
           start tparameva key >= tpa-chiave
                 invalid continue
             not invalid
                 move 2 to riga
                 perform until 1 = 2
                    read tparameva next at end exit perform end-read
                    move tpa-codice      to col-eva
                    move tpa-descrizione to col-des-eva
                    move "N"             to col-scelta
                    modify gd-evasioni(riga, 1), cell-data col-eva
                    modify gd-evasioni(riga, 2), cell-data col-des-eva
                    modify gd-evasioni(riga, 3), cell-data col-scelta
                    add 1 to riga
                 end-perform
           end-start.

LUBEXX     perform DELETE-LOCKFILE.

      ***---
       GCLIENTI-AFTER-BUF-TO-FLD.
           perform SCARICA-COMBO-STATO.
           move stato          to cli-stato.

           perform SCARICA-COMBO-INVIO.
           move invio          to rec-invio.

           move ef-fido-data-buf     to como-data.
           perform DATE-TO-FILE.
           move como-data            to cli-fido-data.

           move ef-data-fido-extra-buf     to como-data.
           perform DATE-TO-FILE.
           move como-data            to cli-data-fido-extra.

           if cli-bloccato
              perform SCARICA-COMBO-CAUSALE
              move cau-blocco         to cli-cau-blocco
           else
              move space              to cli-cau-blocco
                                         cau-blocco
              perform CARICA-COMBO-CAUSALE
           end-if

           perform SCARICA-COMBO-SOST.
           move sost to cli-sost.
           if old-cli-sost = spaces
              move cli-sost to old-cli-sost
           end-if.

           perform SCARICA-COMBO-TIPO-ART.
           move tipo-art to cli-tipo-art.
           if old-cli-tipo-art = 0
              move cli-tipo-art to old-cli-tipo-art
           end-if.

           move cli-codice to not-codice of note.
           move 0          to not-prog   of note.

           move ef-codice-buf to cli-codice convert.  

           move ef-data-dich-buf     to como-data.
           perform DATE-TO-FILE.
           move como-data            to cli-data-dich.

           move ef-data-reg-buf     to como-data.
           perform DATE-TO-FILE.
           move como-data            to cli-data-reg.

           |levo l'eventuale acapo finale
           inspect ef-mail-buf replacing trailing spaces by low-value.
           move 0 to CountChar.
           inspect ef-mail-buf
                   tallying CountChar for characters before low-value.
           subtract 1 from CountChar.
           move ef-mail-buf(CountChar:2) to cli-email.
           if ef-mail-buf(CountChar:2) = X"0D0A"
              move X"2020" to ef-mail-buf(CountChar:2)
           end-if.
           inspect ef-mail-buf replacing trailing low-value by spaces.
           move ef-mail-buf to cli-email.


      ***---
       GCLIENTI-AFTER-FLD-TO-BUF.
           perform LEGGI-EDI.
           modify gd-destini-e, visible false.
           move cli-stato        to stato.
           perform CARICA-COMBO-STATO.
           evaluate true
           when bloccato  
                move 1 to mod-campi
                move 1 to mod-blocco
           when attivo    
                move 1 to mod-campi
                move 0 to mod-blocco
           when disattivo 
                move 0 to mod-campi
                move 0 to mod-blocco
           end-evaluate.

           move cli-cau-blocco          to cau-blocco.
           perform CARICA-COMBO-CAUSALE.

           move cli-sost to sost.
           perform CARICA-COMBO-SOST.

           move cli-tipo-art to tipo-art.
           perform CARICA-COMBO-TIPO-ART.

           perform LOAD-RECORD.
  
           perform X-Y.
           if tot-righe = 1 
              modify form1-gd-1, insert-rows = 1 
              move "Attivo" to cbo-stato-d-buf old-des-stato
              modify cbo-stato-d, value = cbo-stato-d-buf
           else
              perform CARICA-COMBO-DESTINI
              evaluate true
              when bloccato  move 1 to mod-destini
              when attivo    move 1 to mod-destini
              when disattivo move 0 to mod-destini
              end-evaluate
           end-if.

           move rec-invio        to invio.
           perform CARICA-COMBO-INVIO.

           move cli-data-creazione to como-data.
           perform DATE-TO-SCREEN
           move como-data          to lab-data-buf.

           move cli-fido-data      to como-data
           perform DATE-TO-SCREEN
           move como-data          to ef-fido-data-buf. 

           move cli-data-fido-extra to como-data
           perform DATE-TO-SCREEN
           move como-data           to ef-data-fido-extra-buf.

           move cli-data-dich      to como-data.
           perform DATE-TO-SCREEN.
           move como-data          to ef-data-dich-buf.

           move cli-data-reg       to como-data.
           perform DATE-TO-SCREEN.
           move como-data          to ef-data-reg-buf.
                                
           perform VALORIZZA-OLD.
                                  
           move cli-codice    to  codice-ed.
           move codice-ed     to  ef-codice-buf.   
           call "C$JUSTIFY" using ef-codice-buf, "L".
           display ef-codice.   

      * Relazioni per il file dei CLIENTI
      * CLIENTI-NOTE
           initialize not-rec of note replacing numeric data by zeroes
                                           alphanumeric data by spaces
           move cli-codice to not-codice of note.
           move 0          to not-prog of note.
           read note no lock 
                invalid move spaces to WrkNote-1
                        move spaces to WrkNote-2
                        move spaces to WrkNote-3
                        move spaces to WrkNote-4
                        move      0 to WrkData
            not invalid move not-note-1 of note to WrkNote-1
                        move not-data   of note to como-data
                        perform DATE-TO-SCREEN
                        move como-data          to WrkData  
                        move not-note-2 of note to WrkNote-2
                        move not-note-3 of note to WrkNote-3
                        move not-note-4 of note to WrkNote-4
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

      * CLIENTI-TIPOCLI         
           move "ttipocli" to nome-file.
           perform RELAZIONI-CLIENTI.
      *    luciano
           if mod-campi = 1 
              perform VALORIZZA-ENABLE-VARIABLE
           else
      *****        move 0  to mod-gdo
              move 0  to mod-agente
           end-if
      *    luciano

      * CLIENTI-GRUPPOGDO
           move "tgrupgdo" to nome-file.
           perform RELAZIONI-CLIENTI.
      * CLIENTI-AGENTI      
           move "agenti" to nome-file.
           perform RELAZIONI-CLIENTI.
      * CLIENTI-AGENTI      
           move "agenti2" to nome-file.
           perform RELAZIONI-CLIENTI.

      * CLIENTI-TIVAESE     
           move "tivaese-ese" to nome-file.
           perform RELAZIONI-CLIENTI.

           move "tivaese" to nome-file.
           perform RELAZIONI-CLIENTI.

      * CLIENTI-CODICI DI PAGAMENTO
           move "tcodpag" to nome-file.
           perform RELAZIONI-CLIENTI. 

      * CLIENTI-BANCHE
           move "ABI" to nome-file.
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

      * Relazioni per il file dei RECAPITI
      * RECAPITI-PROVINCIE           
           move "tprov" to nome-file.
           perform RELAZIONI-RECAPITI.

      * RECAPITI-NAZIONI
           move "tnazioni" to nome-file.
           perform RELAZIONI-RECAPITI.  

           perform ABILITAZIONI.

LUBEXX     if control-id = 78-ID-rb-pers or
              control-id = 78-ID-rb-soc
              perform CANCELLA-COLORE
              move 78-ID-ef-codfis to control-id
              move 4               to accept-control
           end-if.
           modify gd-destini-e, visible true.
           set ForzaRiga to true.

      ***---
       PUSH-NOTE-PRESSED.
           move mod-campi to e-campo.
           perform APRI-NOTE.

      ***---
       PUSH-NOTA-PIE-PRESSED.
           inquire ef-codice, value in ef-codice-buf.
           move ef-codice-buf to not-codice of note with convert.

           if not-codice of note > 0
              call   "note-pie" using livello-abil
                                      StatoRec
                                      mod
                                      not-codice of note
                                      WrkCampi
              cancel "note-pie"
           end-if.
                      
      ***---
       PUSH-GRID-SALVA-PRESSED.           
           perform SALVA-RIGA.

      ***---
       PUSH-GRID-SALVA-EDI-PRESSED.       
           if mod-campi = 0 exit paragraph end-if.
           inquire form1-gd-1-edi, cursor-y in riga.
           inquire form1-gd-1-edi(riga, 1), cell-data in col-prog.
           initialize ecd-rec.
           move cli-codice          to ecd-cli-codice.
           move col-prog            to ecd-prg-destino.
           move ef-codcli-edi-buf   to ecd-cod-dest.
           move ef-dest-edi-buf     to ecd-cod-consegna.
           move ef-q-dest-edi-buf   to ecd-q-cod-consegna.
           move ef-ragsoc-d-edi-buf to ecd-ragsoc-d.
           move ef-ind-d-edi-buf    to ecd-indirizzo-d.
           move ef-citta-d-edi-buf  to ecd-citta-d.
           move ef-prov-d-edi-buf   to ecd-prov-d.
           move ef-cap-d-edi-buf    to ecd-cap-d.
           write ecd-rec invalid rewrite ecd-rec end-write.
   
      ***---
       PUSH-GRID-NUOVO-PRESSED.
           if mod-campi = 0 exit paragraph end-if.

           perform SALV-MOD-RIGA.

           if tutto-ok
              perform X-Y
              |per sicurezza cancello un'eventuale riga vuota già inserita
              modify form1-gd-1, record-to-delete = tot-righe + 1

              modify form1-gd-1, insert-rows = 1
              add 1 to tot-righe
              move tot-righe to riga                      
              modify form1-gd-1, cursor-y = riga, cursor-x = 1
              perform PAGE-2-CLEAR
              move 1 to riga-nuova             
              perform COLORE                                 
              move 78-ID-ef-ragsoc-1-d to control-id
              move 4 to accept-control
              move "Consegna" to ef-note-1-buf
              display ef-note-1
              move chk-utf-buf to chk-deposito-utf-buf
              display chk-deposito-utf
              perform SCARICA-COMBO-TIPO-ART
              perform CARICA-COMBO-TIPO-ART-D
           end-if.

      ***---
       PUSH-GRID-ELIMINA-PRESSED.
           if mod-campi = 0 exit paragraph end-if.

           perform X-Y.

           if tot-righe > 1 or
              ( riga > 1 and riga < tot-righe )

              display message "Eliminare la riga selezionata?"
                        title titolo
                         type mb-yes-no
                      default mb-no
                       giving scelta

              if scelta = mb-yes
                 perform X-Y
LUBEXX           initialize rec-grid replacing numeric data by zeroes
LUBEXX                                    alphanumeric data by spaces
                 modify form1-gd-1, record-to-delete = riga
                 set DestinoCambiato to true
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
LUBEXX     else
LUBEXX        perform PAGE-2-CLEAR
           end-if.

LUBEXX***---
LUBEXX DELETE-LOCKFILE.
LUBEXX     move 78-gclienti to lck-nome-pgm.
LUBEXX     delete lockfile record invalid continue end-delete.

      ***---
       RIEMPI-COMBO-CAUSALE.
           modify cbo-cau-blocco,  
                  item-to-add "Anagrafica errata - mancante".
           modify cbo-cau-blocco,  item-to-add "Problematiche pagamento"
           modify cbo-cau-blocco,  item-to-add "Nuova ragione sociale".
           modify cbo-cau-blocco,  item-to-add "Cliente fuori fido".
           modify cbo-cau-blocco,  item-to-add "Blocco amministrativo".


      ***---
       CARICA-COMBO-CAUSALE.
           evaluate true
           when no-angraf     
                move "Anagrafica errata - mancante" 
                                                  to cbo-cau-blocco-buf
           when prob-pag        
                move "Problematiche pagamento"    to cbo-cau-blocco-buf
           when nuovo-ragsoc      
                move "Nuova ragione sociale"      to cbo-cau-blocco-buf
           when fuori-fido
                move "Cliente fuori fido"         to cbo-cau-blocco-buf
           when blocco-amministrativo
                move "Blocco amministrativo"      to cbo-cau-blocco-buf
           when other 
                if cbo-stato-buf = "Bloccato"
                   move "Anagrafica errata - mancante" 
                                                  to cbo-cau-blocco-buf
                else
                   move space                     to cbo-cau-blocco-buf
                end-if
           end-evaluate.
           modify cbo-cau-blocco,  value cbo-cau-blocco-buf.

      ***---
       SCARICA-COMBO-CAUSALE.     
           inquire cbo-cau-blocco, value cbo-cau-blocco-buf.
           evaluate cbo-cau-blocco-buf
           when "Anagrafica errata - mancante"
                 set no-angraf     to true
           when "Problematiche pagamento"
                set prob-pag       to true              
           when "Nuova ragione sociale"
                set nuovo-ragsoc   to true
           when "Cliente fuori fido"
                set fuori-fido     to true
           when "Blocco amministrativo"
                set blocco-amministrativo to true
           end-evaluate.

      ***---
       RIEMPI-COMBO-SOST.
           Modify cbo-sost-art, item-to-add "Automatica".
           Modify cbo-sost-art, item-to-add "Su richiesta".
           Modify cbo-sost-art, item-to-add "Nessuna".

      ***---
       CARICA-COMBO-SOST.
           evaluate true
           when sost-auto      move "Automatica"   to cbo-sost-art-buf
           when sost-richiesta move "Su richiesta" to cbo-sost-art-buf
           when sost-no        move "Nessuna"      to cbo-sost-art-buf
           end-evaluate.
           modify cbo-sost-art,  value cbo-sost-art-buf.

      ***---
       SCARICA-COMBO-SOST.
           inquire cbo-sost-art, value cbo-sost-art-buf.
           evaluate cbo-sost-art-buf
           when "Automatica"   set sost-auto      to true
           when "Su richiesta" set sost-richiesta to true
           when "Nessuna"      set sost-no        to true
           end-evaluate.

      ***---
       RIEMPI-COMBO-TIPO-ART.
           Modify cbo-tipo-art, item-to-add "Diretti".
           Modify cbo-tipo-art, item-to-add "Gruppi".
           Modify cbo-tipo-art, item-to-add "Specialist".
           Modify cbo-tipo-art, item-to-add "D.O.".
           Modify cbo-tipo-art, item-to-add "G.D.A.".
           Modify cbo-tipo-art, item-to-add "G.D.S.".
           Modify cbo-tipo-art, item-to-add "Estero".

      ***---
       CARICA-COMBO-TIPO-ART.
           evaluate true
           when tipo-art-diretti    move "Diretti"   to cbo-tipo-art-buf
           when tipo-art-gruppi     move "Gruppi"    to cbo-tipo-art-buf
           when tipo-art-specialist move "Specialist"to cbo-tipo-art-buf
           when tipo-art-do         move "D.O."      to cbo-tipo-art-buf
           when tipo-art-gda        move "G.D.A."    to cbo-tipo-art-buf
           when tipo-art-gds        move "G.D.S."    to cbo-tipo-art-buf
           when tipo-art-estero     move "Estero"    to cbo-tipo-art-buf
           end-evaluate.
           modify cbo-tipo-art,  value cbo-tipo-art-buf.

      ***---
       SCARICA-COMBO-TIPO-ART.
           inquire cbo-tipo-art, value cbo-tipo-art-buf.
           evaluate cbo-tipo-art-buf
           when "Diretti"    set tipo-art-diretti    to true
           when "Gruppi"     set tipo-art-gruppi     to true
           when "Specialist" set tipo-art-specialist to true
           when "D.O."       set tipo-art-do         to true
           when "G.D.A."     set tipo-art-gda        to true
           when "G.D.S."     set tipo-art-gds        to true
           when "Estero"     set tipo-art-estero     to true
           end-evaluate.

      ***---
       RIEMPI-COMBO-TIPO-ART-D.
           Modify cbo-tipo-art-d, item-to-add "Diretti".
           Modify cbo-tipo-art-d, item-to-add "Gruppi".
           Modify cbo-tipo-art-d, item-to-add "Specialist".
           Modify cbo-tipo-art-d, item-to-add "D.O.".
           Modify cbo-tipo-art-d, item-to-add "G.D.A.".
           Modify cbo-tipo-art-d, item-to-add "G.D.S.".
           Modify cbo-tipo-art-d, item-to-add "Estero".

      ***---
       CARICA-COMBO-TIPO-ART-D.
           evaluate true
           when tipo-art-diretti    
                move "Diretti"   to cbo-tipo-art-d-buf
           when tipo-art-gruppi     
                move "Gruppi"    to cbo-tipo-art-d-buf
           when tipo-art-specialist 
                move "Specialist"to cbo-tipo-art-d-buf
           when tipo-art-do         
                move "D.O."      to cbo-tipo-art-d-buf
           when tipo-art-gda        
                move "G.D.A."    to cbo-tipo-art-d-buf
           when tipo-art-gds        
                move "G.D.S."    to cbo-tipo-art-d-buf
           when tipo-art-estero     
                move "Estero"    to cbo-tipo-art-d-buf
           end-evaluate.
           modify cbo-tipo-art-d,  value cbo-tipo-art-d-buf.

      ***---
       SCARICA-COMBO-TIPO-ART-D.
           inquire cbo-tipo-art-d, value cbo-tipo-art-d-buf.
           evaluate cbo-tipo-art-d-buf
           when "Diretti"    set tipo-art-diretti    to true
           when "Gruppi"     set tipo-art-gruppi     to true                              
           when "Specialist" set tipo-art-specialist to true
           when "D.O."       set tipo-art-do         to true
           when "G.D.A."     set tipo-art-gda        to true
           when "G.D.S."     set tipo-art-gds        to true
           when "Estero"     set tipo-art-estero     to true
           end-evaluate.

      ***---
       CHANGE-STATO.
           perform SCARICA-COMBO-STATO
           if bloccato
              move 1   to mod-blocco
              inquire cbo-cau-blocco, VALUE cbo-cau-blocco-buf
              if cbo-cau-blocco-buf = space
                 set fuori-fido  to true
                 perform CARICA-COMBO-CAUSALE
              end-if
           else
              move zero   to mod-blocco
           end-if
           modify cbo-cau-blocco, enabled mod-blocco.

      ***---
       PB-COPIA-PRESSED.
           display message "Riportare i dati dell'evasione "
                    X"0D0A""relativi al cliente su tutti i "
                    X"0D0A""destini in modo definitivo?"
                     title titolo
                      type mb-yes-no
                      icon 2
                    giving scelta
                   default mb-no
           if scelta = mb-yes
              inquire gd-destini-e, cursor-y in riga
              if riga not = 2
                 move 2 to riga event-data-2
                 perform SPOSTAMENTO-DESTINI-E
                 perform EVA-TO-SCREEN
              end-if
              inquire gd-destini-e, last-row in tot-righe-d
              perform varying riga-d from 3 by 1 
                        until riga-d > tot-righe-d
                 inquire gd-destini-e(riga-d , 1), 
                         cell-data in ecd-destino-OLD
                 perform SCREEN-TO-EVA
              end-perform
           end-if.

      ***---
       PB-SALVA-EVA-PRESSED.
           if mod = 0 exit paragraph end-if.
           inquire gd-destini-e, cursor-y in riga-d.
           inquire gd-destini-e(riga-d , 1), 
                   cell-data in ecd-destino-OLD.
           perform SCREEN-TO-EVA.

      ***---
       SCREEN-TO-EVA.
           move cli-codice               to ecd-cliente-OLD.
           move chk-escludi-e-buf        to ecd-escludi-OLD.
           move chk-intera-e-buf         to ecd-ev-intera-OLD.
           move chk-accorpa-e-buf        to ecd-accorpa-OLD.
           move chk-saldo-banco-e-buf    to ecd-saldi-banco-OLD.
           move chk-saldo-promo-e-buf    to ecd-saldi-promo-OLD.
           move ef-gg-e-buf              to ecd-gg-scadenza-vol-OLD.
           inquire gd-evasioni, last-row in tot-righe-e.
           initialize ecd-tab-evasione-OLD.
           perform varying riga-e from 2 by 1 
                     until riga-e > tot-righe-e
              inquire gd-evasioni(riga-e, 1), cell-data col-eva
              inquire gd-evasioni(riga-e, 3), cell-data col-scelta
              subtract 1 from riga-e giving idx
              if col-scelta = "S"
                 move col-eva to tpa-codice
                 read tparameva no lock
                      invalid continue
                  not invalid move col-eva to ecd-el-evasione-OLD(idx)
                 end-read
              end-if
           end-perform.
           if ecd-data-creazione-OLD = 0
              accept ecd-data-creazione-OLD from century-date
              accept ecd-ora-creazione-OLD  from time
              move user-codi to ecd-utente-creazione-OLD
           else
              accept ecd-data-modifica-OLD from century-date
              accept ecd-ora-modifica-OLD  from time
              move user-codi to ecd-utente-modifica-OLD
           end-if.
           write ecd-rec-OLD invalid rewrite ecd-rec-OLD end-write.      
      
      ***---
       SPOSTAMENTO-DESTINI-E.
           inquire gd-destini-e, last-row in tot-righe-d.
           if event-data-2 >= 2 and
              event-data-2 <= tot-righe-d
              
              modify gd-destini-e, start-y = event-data-2, 
                                         y = event-data-2,
                                   start-x = 1,    
                                         x = 4,
                              region-color = 480
              inquire gd-destini-e, cursor-y in riga-d
              inquire gd-destini-e(event-data-2, 1), 
                      cell-data in ecd-destino-OLD
              perform EVA-TO-SCREEN
           end-if.

      ***---
       EVA-TO-SCREEN.
           inquire gd-evasioni, last-row in tot-righe-e.
           perform varying riga-e from 2 by 1 
                     until riga-e > tot-righe-e
              move "N" to col-scelta
              modify gd-evasioni(riga-e, 3), cell-data col-scelta
           end-perform.

           move cli-codice to ecd-cliente-OLD.
           initialize ecd-dati-OLD replacing numeric data by zeroes
                                        alphanumeric data by spaces.             
           read evaclides no lock invalid continue end-read.
           move ecd-escludi-OLD         to chk-escludi-e-buf.
           move ecd-ev-intera-OLD       to chk-intera-e-buf.
           move ecd-accorpa-OLD         to chk-accorpa-e-buf.
           move ecd-saldi-banco-OLD     to chk-saldo-banco-e-buf.
           move ecd-saldi-promo-OLD     to chk-saldo-promo-e-buf.
           move ecd-gg-scadenza-vol-OLD to ef-gg-e-buf.
           inquire gd-evasioni, last-row in tot-righe-e.
           perform varying idx from 1 by 1
                     until idx > 50
              set trovato to false
              perform varying riga-e from 2 by 1
                        until riga-e > tot-righe-e
                 inquire gd-evasioni(riga-e, 1), cell-data in col-eva
                 move col-eva to tpa-codice
                 if tpa-codice = ecd-el-evasione-OLD(idx)
                    set trovato to true
                    exit perform
                 end-if
              end-perform
              if not trovato
                 move 0 to ecd-el-evasione-OLD(idx)
              else
                 move "S" to col-scelta
                 modify gd-evasioni(riga-e, 3), cell-data col-scelta
              end-if
           end-perform.
           display chk-escludi-e
                   chk-intera-e
                   chk-accorpa-e
                   chk-saldo-banco-e
                   chk-saldo-promo-e
                   ef-gg-e.

      ***---
       CLI-PRG-TO-SCREEN.
           modify gd-prg, reset-grid = 1.
           perform GD-PRG-CONTENT.
           move low-value  to cp-rec.
           set  cp-tipo-C  to true.
           move cli-codice to cp-clifor.

           move 1 to riga.
           start cli-prg key >= cp-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read cli-prg next no lock 
                         at end exit perform
                    end-read
                    if cp-tipo-F
                       exit perform
                    end-if
                    if cp-clifor not = cli-codice
                       exit perform
                    end-if
                    move cp-prg-chiave to prg-chiave
                    add 1 to riga
                    perform METTI-IN-GRID-CLI-PRG
                 end-perform
           end-start.

      ***---
       SPOSTAMENTO-GD-EVASIONI.
           inquire gd-evasioni, last-row in tot-righe-e.
           if event-data-2 >= 2 and
              event-data-2 <= tot-righe-e
              
              modify gd-evasioni, start-y = event-data-2, 
                                        y = event-data-2,
                                  start-x = 1,    
                                        x = 3,
                             region-color = 480
           end-if.

      ***---
       CHANGE-TAB.
           if event-data-1 = 4 and nuovo
              set NonCambiareTab to true
           else
              set NonCambiareTab to false
              move event-data-1 to pagina
           end-if.

      ***---
       GCLIENTI-AFTER-ENDACCEPT.
           if NonCambiareTab
              set NonCambiareTab to false
              move pagina to screen1-ta-1-tab-value event-data-1
              perform SCREEN1-TA-1-TABCHANGE
              move 4 to accept-control
           end-if. 
           if ScegliCliPrg
              perform VALORE-RIGA
              move col-art  to prg-cod-articolo
              move col-mag  to prg-cod-magazzino
              move col-imb  to prg-tipo-imballo
              move col-peso to prg-peso
              perform PUSH-GRID-NUOVO-PRG-PRESSED
              set ScegliCliPrg to false
           end-if.

      ***---
       PUSH-GRID-NUOVO-PRG-PRESSED.
           if mod-campi = 0 exit paragraph end-if.

           if tutto-ok
              initialize prg-rec
              move "progmag-sons"   to como-file
              call "zoom-gt"     using como-file, prg-rec
                                giving stato-zoom
              end-call
              cancel "zoom-gt"
              if stato-zoom = 0
                 inquire gd-prg, last-row in tot-righe
                 perform CONTROLLO-ESISTENZA
                 if not ScegliCliPrg and not trovato
                    add 1 to tot-righe giving riga
                    modify gd-prg, insert-rows = 1
                 end-if
                 perform METTI-IN-GRID-CLI-PRG
              end-if
           end-if.

      ***---
       CONTROLLO-ESISTENZA.
           set trovato to false.
           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe
              inquire gd-prg(store-riga, 1), cell-data in col-art
              move col-art to art-codice
              if art-codice = prg-cod-articolo
                 set trovato to true
                 move store-riga to riga
                 exit perform
              end-if
           end-perform.

      ***---
       METTI-IN-GRID-CLI-PRG.
           move prg-cod-articolo  to art-codice  col-art.
           read articoli  no lock.
           move prg-cod-magazzino to mag-codice  col-mag.
           read tmagaz    no lock.
           move prg-tipo-imballo  to imq-codice  col-imb.
           read timbalqta with no lock.
           move imq-tipo  to imb-codice.
           read timballi  with no lock.
           move prg-peso  to col-peso.
           move art-descrizione to col-art-des.
           move mag-descrizione to col-mag-des.
           initialize col-imb-des.
           move imq-qta-imb to imb-qta-edit.
           inspect imb-descrizione replacing trailing spaces 
                                   by low-value.
           call "C$JUSTIFY" using imb-qta-edit, "L".
           string  imb-descrizione delimited by low-value
                   " da "          delimited by size
                   imb-qta-edit    delimited by spaces
                   " x "           delimited by size
                   art-udm-imballo delimited by size
              into col-imb-des
           end-string.
           modify gd-prg(riga, 1), cell-data col-art.
           modify gd-prg(riga, 2), cell-data col-art-des.
           modify gd-prg(riga, 3), cell-data col-mag.
           modify gd-prg(riga, 4), cell-data col-mag-des.
           modify gd-prg(riga, 5), cell-data col-imb.
           modify gd-prg(riga, 6), cell-data col-imb-des.
           modify gd-prg(riga, 7)  cell-data col-peso.
           set PrgCambiato to true.

      ***---
       PUSH-GRID-ELIMINA-PRG-PRESSED.
           if mod-campi = 0 exit paragraph end-if.

           
           inquire gd-prg, last-row in tot-righe, 
                           cursor-y in riga,
                           cursor-x in colonna.

           if tot-righe > 1 or
              ( riga > 1 and riga < tot-righe )

              display message "Eliminare la riga selezionata?"
                        title titolo
                         type mb-yes-no
                      default mb-no
                       giving scelta

              if scelta = mb-yes
LUBEXX           initialize rec-prg replacing numeric data by zeroes
LUBEXX                                   alphanumeric data by spaces
                 modify gd-prg, record-to-delete = riga
                 set PrgCambiato to true
                 modify gd-prg, cursor-y = riga, cursor-x = 1
              end-if
           end-if.

      ***---
       VALORE-RIGA-PRG.
           inquire gd-prg(riga, 1), cell-data in col-art.
           inquire gd-prg(riga, 3), cell-data in col-mag.
           inquire gd-prg(riga, 5), cell-data in col-imb.
           inquire gd-prg(riga, 7)  cell-data in col-peso.


      ***---
       SPOSTAMENTO-PRG.
           inquire gd-prg, last-row in tot-righe.
           if event-data-2 >= 2 and
              event-data-2 <= tot-righe
              modify gd-prg, start-y = event-data-2, y = event-data-2,
                             start-x = 1,            x = 7,
                             region-color = 257
           end-if.

      ***---
       GD-PRG-ENTRY.
           if mod = 0
              set event-action to event-action-fail
           else
              inquire gd-prg, entry-reason in como-x
              set event-action to event-action-fail-terminate
              evaluate como-x
              when X"00"|doppio click
              when X"0D"|invio
                   inquire gd-prg, CURSOR-Y = riga
                   set ScegliCliPrg to true
              end-evaluate
           end-if.
