      *******************************************************************
      *                  SEZIONE PARAGRAFI (User Dephined)              *
      *******************************************************************

      ***---
       ANTEPRIMA.
           set anteprima     to true.
           call   "starticoli" using lk-blockpgm, 
                                     user-codi, 
                                     livello-abil.
           cancel "starticoli".

      **---
       ABILITAZIONI.
           if mod = 1                  
              move BitmapDeleteEnabled to BitmapNumDelete
              move BitmapSaveEnabled   to BitmapNumSave
              move 1 to e-salva e-cancella
                                 
              move 0 to mod-campi 
                        mod-imposte 
                        mod-cobat 
                        mod-peso-utf
                        mod-peso-non-utf
              move 3 to NumBitmapCodici
      
              inquire cbo-stato,   value cbo-stato-buf
      
              if cbo-stato-buf not = "Disattivo" 
                 move 1 to mod-campi
                 move 1 to NumBitmapCodici
      
                 evaluate cbo-utf-buf
                 when "Soggetto"       move 1 to mod-peso-utf
                 when "Misto"          move 1 to mod-peso-utf 
                                                 mod-peso-non-utf
                 when "Non soggetto"   move 1 to mod-peso-non-utf
                 end-evaluate
      
                 if chk-imposte-buf = 1
                    move 1 to mod-imposte
                 end-if
      
                 if chk-cobat-buf = 1
                    move 1 to mod-cobat
                 end-if
      
              end-if  
      
           else         

              move BitmapDeleteDisabled to BitmapNumDelete
              move BitmapSaveDisabled   to BitmapNumSave
              move 0 to e-salva e-cancella         
              move 0 to mod-campi 
                        mod-imposte 
                        mod-cobat 
                        mod-peso-utf
                        mod-peso-non-utf
              move 3 to NumBitmapCodici
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
       CANCELLA.
           if mod = 0  exit paragraph  end-if.
      *              
           |Se l'articolo è movimentato o venduto durante l'anno
           |non ancora chiuso o quello futuro non posso cancellarlo
           set trovato to false.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           perform RIEMPI-CHIAVE.
           move art-codice of articoli to rmo-articolo.
           move low-value  to rmo-data-movim.
           start rmovmag key >= k-art-data
                 invalid continue
             not invalid
                 read rmovmag next
                 if rmo-articolo = art-codice of articoli
                    set trovato to true
                    display message 
                    "Impossibile cancellare: " x"0d0a"
                    "articolo movimentato nell'anno "rmo-data-movim(1:4)
                              title tit-err
                               icon 2
                 end-if
           end-start.

           if not trovato
              move low-value  to rno-rec
              move art-codice of articoli to rno-cod-articolo
              start rnotacr key >= rno-k-articolo
                 invalid 
                    continue
                 not invalid
                    read rnotacr next
                    if rno-cod-articolo = art-codice of articoli
                       set trovato to true
                       display message 
                       "Impossibile cancellare: " x"0d0a"
                       "articolo in nota credito nell'anno " rno-anno
                                 title tit-err
                                  icon 2
                    end-if
              end-start
           end-if.   

           if not trovato
              move low-value  to ror-rec
              move art-codice of articoli to ror-cod-articolo
              start rordini key >= ror-k-articolo
                    invalid continue
                not invalid
                    read rordini next
                    if ror-cod-articolo = art-codice of articoli
                       set trovato to true
                       display message 
                       "Impossibile cancellare: " x"0d0a"
                       "articolo in ordine nell'anno " ror-anno
                                 title tit-err
                                  icon 2
                    end-if
              end-start
           end-if.

           if not trovato
              move low-value  to brno-rec
              move art-codice of articoli to brno-cod-articolo
              start brnotacr key >= brno-k-articolo
                    invalid continue
                not invalid
                    read brnotacr next
                    if brno-cod-articolo = art-codice of articoli
                       set trovato to true
                       display message 
                       "Impossibile cancellare: " x"0d0a"
                       "articolo in bozza NC nell'anno " brno-anno
                                 title tit-err
                                  icon 2
                    end-if
              end-start
           end-if.

           if not trovato
      *    ordini master
              move low-value  to brno-rec
              move art-codice of articoli to mro-cod-articolo
              start mrordini key >= mro-k-articolo
                 invalid 
                    continue
                 not invalid
                    read mrordini next
                       at end
                          continue
                    end-read
                    if mro-cod-articolo = art-codice of articoli
                       set trovato to true
                       display message 
                       "Impossibile cancellare: " x"0d0a"
                       "articolo in ordine master nell'anno " mro-anno
                                 title tit-err
                                  icon 2
                    end-if
              end-start
           end-if

           if not trovato
      *    bozze evasione
              move low-value  to reva-rec
              move art-codice of articoli to reva-articolo
              start reva key >= reva-k-articolo
                 invalid 
                    continue
                 not invalid
                    read reva next
                       at end
                          continue
                    end-read
                    if reva-articolo = art-codice of articoli
                       set trovato to true
                       display message 
                       "Impossibile cancellare: " x"0d0a"
                       "articolo in bozze d'evasione nell'anno " 
                                                              mro-anno
                                 title tit-err
                                  icon 2
                    end-if
              end-start
           end-if

           if not trovato
      *    ordini fornitori
              move low-value  to rof-rec
              move art-codice of articoli to rof-cod-articolo
              start rordforn key >= rof-k-articolo
                 invalid 
                    continue
                 not invalid
                    read reva next
                       at end
                          continue
                    end-read
                    if rof-cod-articolo = art-codice of articoli
                       set trovato to true
                       display message 
                       "Impossibile cancellare: " x"0d0a"
                       "articolo in ordini fornitori nell'anno " 
                                                              mro-anno
                                 title tit-err
                                  icon 2
                    end-if
              end-start
           end-if

           if not trovato

              display message MSG-Cancellare-il-record-corrente
                        title titolo
                         type mb-yes-no 
                      default mb-no
                       giving scelta

              if scelta = mb-yes
                 set DeleteXX to true
                 perform ACCESSOXX

                 initialize link-wprogmag
                 set link-delete      to true
                 move ef-codice-buf   to link-articolo convert
                 move user-codi       to link-user

                 perform RIEMPI-CHIAVE
                 delete articoli record invalid continue end-delete
   
                 move 0  to mod
                 move 1  to mod-k
                 perform FORM1-CLEAR

                 set vecchio to true
                 perform CANCELLA-COLORE
                 perform INIT-OLD-REC
                            
                 call   "wprogmag"  using link-wprogmag
                 cancel "wprogmag"  

                 perform AGGIORNA-CATENA
                 display message MSG-Cancellazione-avvenuta-con-successo
                           title titolo                         

                 unlock articoli all records     

                 initialize G2Agg-linkage
                 move art-codice of articoli to G2Agg-articolo
                 set  G2Agg-art    to true
                 set  G2Agg-delete to true
                 call   "G2Agg" using G2Agg-linkage
                 cancel "G2Agg"

                 perform PRIMO
                 move 1 to control-id
                 move 4 to accept-control
                 set Statusvisua to true
                 perform STATUS-BAR-MSG
                 modify TOOL-MODIFICA value = mod
                 perform DESTROYXX
              end-if
           end-if.

      ***---
       CERCA.
           evaluate control-id
      
           when 78-ID-ef-coll
                move "articoli-alfa" to como-file         
                inquire ef-coll,    value in art-codice of articoli1
                call "zoom-gt"   using como-file, art-rec of articoli1
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move art-codice of articoli1   to ef-coll-buf
                   move art-descrizione of articoli1 to lab-des-coll-buf
                   display ef-coll lab-des-coll
                else
                   move "blister" to como-file         
                   inquire ef-coll,    value in bli-codice
                   call "zoom-gt"   using como-file, bli-rec
                                   giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      move bli-codice      to ef-coll-buf
                      move bli-descrizione to lab-des-coll-buf
                      display ef-coll lab-des-coll
                   end-if
                end-if          
      
           when 78-ID-ef-marca
                move "tmarche"     to como-file         
                inquire ef-marca,    value in mar-codice
                call "zoom-gt"   using como-file, mar-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move mar-codice to ef-marca-buf
                   move mar-descrizione to lab-marca-buf
                   display ef-marca lab-marca
                end-if          
      
           when 78-ID-ef-mag
                move "tmagaz"     to como-file         
                inquire ef-mag,    value in mag-codice
                call "zoom-gt"   using como-file, mag-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move mag-codice      to ef-mag-buf
                   move mag-descrizione to lab-mag-buf
                   display ef-mag lab-mag
                end-if        
      
           when 78-ID-ef-iva                
                move "tivaese-non"  to como-file
                move "IV"           to tbliv-codice1
                inquire ef-iva,  value tbliv-codice2
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
      
           when 78-ID-ef-classe-1
                move "tcla1art"  to como-file         
                inquire ef-classe-1, value in cl1-codice
                call "zoom-gt"   using como-file, cl1-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move cl1-codice      to ef-classe-1-buf
                   move cl1-descrizione to lab-classe-buf
                   display ef-classe-1 lab-classe
                end-if
      
           when 78-ID-ef-udm
                move "tudm"  to como-file         
                inquire ef-udm,    value in udm-codice
                call "zoom-gt"   using como-file, udm-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move udm-codice      to ef-udm-buf
                   move udm-descrizione to lab-udm-buf
                   display ef-udm lab-udm
                end-if
      
           when 78-ID-ef-setmerc
                move "tsetmerc"  to como-file         
                inquire ef-setmerc, value in sme-codice
                call "zoom-gt"   using como-file, sme-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move sme-codice      to ef-setmerc-buf
                   move sme-descrizione to lab-setmerc-buf
                   display ef-setmerc lab-setmerc
                end-if  
      
           when 78-ID-ef-imballo
                move "timbalqta"  to como-file         
                inquire ef-imballo, value in imq-codice
                call "zoom-gt"   using como-file, imq-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move imq-codice       to ef-imballo-buf
                   move imq-tipo         to imb-codice
                   read timballi no lock invalid continue end-read
                   perform DESCRIZIONE-IMBALLO
                   display ef-imballo lab-imballo
                end-if                           
      
           when 78-ID-ef-forn
                set cli-tipo-F to true
                inquire ef-forn, value in cli-codice
                move "clienti-CF"   to como-file
                call "zoom-gt"   using como-file, cli-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move cli-codice   to ef-forn-buf
                   move cli-ragsoc-1 to lab-forn-buf
                   display ef-forn lab-forn
                end-if
      
           when 78-ID-ef-destino
                inquire ef-forn,      value in desf-codice
                inquire ef-destino    value desf-prog
                move "destinif-forn"  to como-file
                call "zoom-gt"   using como-file, desf-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move desf-prog    to ef-destino-buf
                   move desf-ragsoc-1 to lab-dest-buf
                   display ef-destino lab-dest
                end-if
      
           when 78-ID-ef-dogana
                move "tnomen"  to como-file         
                inquire ef-dogana, value in nom-codice
                call "zoom-gt"   using como-file, nom-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move nom-codice      to ef-dogana-buf
                   move nom-descrizione to lab-dogana-buf
                   display ef-dogana lab-dogana
                end-if
           when 78-ID-ef-scorta
                move ef-coll-buf to como-articolo
                if como-articolo = 0
                   move "tscorte"  to como-file         
                   inquire ef-scorta, value in sco-codice
                   call "zoom-gt"   using como-file, sco-rec
                                   giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      move sco-codice      to ef-scorta-buf
                      display ef-scorta 
                   end-if
                end-if
      
           when 78-ID-ef-prodener
                move "prodener"     to como-file         
                inquire ef-prodener,    value in pen-codice
                call "zoom-gt"   using como-file, pen-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move pen-codice to ef-prodener-buf
                   initialize lab-prodener-buf
                   string "CPA: "      delimited size
                          pen-cpa      delimited size
                          " - NC: "    delimited size
                          pen-nc       delimited size
                          " - TARIC: " delimited size
                          pen-taric    delimited size
                          " - DAC:"    delimited size
                          pen-dac      delimited size
                          into lab-prodener-buf
                   end-string
                   display ef-prodener lab-prodener
                end-if          
           end-evaluate. 

      ***---
       CLEAR-SCREEN.
           move art-codice of articoli to old-art-codice.

           initialize art-dati of articoli
                                   replacing numeric data by zeroes
                                     alphanumeric data by spaces.
                        
           perform FORM1-FLD-TO-BUF. 

           perform INIT-OLD-REC.

           display form1.
  
           unlock articoli all records.

      ***---
       CHANGE-STATUS.
           set disattivo to true.
           perform CARICA-COMBO-STATO.

           |specifico per i pgm. aventi Tab-Control
           move 1 to Screen1-Ta-1-TAB-VALUE.
           perform SCREEN1-TA-1-TABCHANGE.
           |******
           move 1 to mod.
           move 0 to mod-campi.
           move 3 to NumBitmapCodici.
           display form1.

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
       CONTROLLO.
           set tutto-ok to true.

      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate CONTROL-ID
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                perform RIEMPI-CHIAVE
                if ef-codice-buf not = spaces                                               
                   inspect ef-codice-buf replacing trailing 
                                         spaces by low-values
                   initialize CountChar
                   inspect ef-codice-buf tallying CountChar 
                                         for characters before low-value
                   inspect ef-codice-buf replacing trailing 
                                         low-values by SPACES

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
                   move 78-ID-ef-codice to CONTROL-ID
                   display message box "Inserimento codice mancante"
                           title = tit-err
                           icon MB-WARNING-ICON
                end-if

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des1
                inquire ef-des1, value in ef-des1-buf
                if ef-des1-buf = spaces
                   set errori to true
                   move 78-ID-ef-des1 to CONTROL-ID
                   display message "Inserimento descrizione mancante"
                             title tit-err
                              icon MB-WARNING-ICON
                end-if
                perform DESCRIZIONE-ARTICOLO

           when 78-ID-ef-des2
                perform DESCRIZIONE-ARTICOLO
      *****          move ef-des-buf to lab-des-buf
      *****          display lab-des


           when 78-ID-ef-coll
                inquire ef-coll, value in ef-coll-buf
                move ef-coll-buf   to art-collegato of articoli 
                perform CAMBIO-CODICE-COLLEGATO
                if art-collegato of articoli = 0
                   move spaces to lab-des-coll-buf
                   display lab-des-coll
                else
                   perform IMPOSTA-SCORTA
                end-if                   

           |78-ID-ef-setmerc è l'ID del control ef-setmerc
           when 78-ID-ef-setmerc
                inquire ef-setmerc, value in ef-setmerc-buf        
                move ef-setmerc-buf 
                                to art-settore-merceologico of articoli 
                move "tsetmerc" to nome-file
                perform RELAZIONI-ARTICOLI
                if not trovato
      *             if art-settore-merceologico not = 0
                      set errori to true
                      move 78-ID-ef-setmerc to control-id
                      display message "Settore merceologico NON valido"
                                title tit-err
                                 icon MB-WARNING-ICON
      *             end-if
                end-if

           |78-ID-ef-marca è l'ID del control ef-marca
           when 78-ID-ef-marca
                inquire ef-marca, value in ef-marca-buf        
                move "tmarche" to nome-file
                perform RELAZIONI-ARTICOLI
                if not trovato
                   set errori to true
                   move 78-ID-ef-marca to CONTROL-ID
                   display message box "Codice marca NON valido"
                           title = tit-err
                           icon MB-WARNING-ICON
                end-if

           |78-ID-ef-mag è l'ID del control ef-mag
           when 78-ID-ef-mag
                inquire ef-mag, value in ef-mag-buf       
                move ef-mag-buf to art-mag-std of articoli 
                move "tmagaz" to nome-file
                perform RELAZIONI-ARTICOLI
                if not trovato
                   set errori to true
                   move 78-ID-ef-mag to CONTROL-ID
                   display message box "Codice magazzino NON valido"
                           title = tit-err
                           icon MB-WARNING-ICON
                else
                   if vecchio  
                      set tutto-ok to true
                      initialize prg-chiave 
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                      move art-codice of articoli to prg-cod-articolo
                      move mag-codice to prg-cod-magazzino
                      start progmag key >= prg-chiave
                            invalid set errori to true
                        not invalid
                            read progmag next
                            if prg-cod-articolo  not = 
                               art-codice of articoli or
                               prg-cod-magazzino not = mag-codice
                               set errori to true
                            end-if
                      end-start
                      if errori
                         move 78-ID-ef-mag to CONTROL-ID
                         display message 
                                "Nessun progressivo su questo magazzino"
                                   title tit-err
                                    icon 2
                      end-if
                   end-if
                end-if

           |78-ID-ef-iva è l'ID del control ef-iva
           when 78-ID-ef-iva
                inquire ef-iva, value in ef-iva-buf
                move "tivaese" to nome-file
                perform RELAZIONI-ARTICOLI
                if not trovato
                   set errori to true
                   move 78-ID-ef-iva to CONTROL-ID
                   display message box "Codice IVA NON valido"
                           title = tit-err
                           icon MB-WARNING-ICON
                else
      *****             if tbliv-percentuale = 0    
      *****                set errori to true
      *****                move 78-ID-ef-iva to CONTROL-ID
      *****                display message box "Codice IVA NON valido in"
      *****                                    " quanto senza aliquota"
      *****                        title = tit-err
      *****                        icon MB-WARNING-ICON
      *****             else   
                      if ef-iva-buf not = CodiceIvaStd
                         display message 
                               "Confermi il codice IVA inserito?"
                        x"0d0a""Codice Standard: ", CodiceIvaStd
                                 title titolo
                                  type mb-yes-no
                                giving scelta
                         if scelta = mb-no
                            set errori to true
                            move 78-ID-ef-iva to control-id
                         end-if
                      end-if
      *****             end-if
                end-if

           |78-ID-ef-classe-1 è l'ID del control ef-classe-1
           when 78-ID-ef-classe-1
                inquire ef-classe-1, value in ef-classe-1-buf        
                move "tcla1art" to nome-file
                perform RELAZIONI-ARTICOLI
                if not trovato
                   set errori to true
                   move 78-ID-ef-classe-1 to CONTROL-ID
                   display message box "Classe NON valida"
                           title = tit-err
                           icon MB-WARNING-ICON
                end-if

           |78-ID-ef-udm è l'ID del control ef-udm
           when 78-ID-ef-udm
                inquire ef-udm, value in ef-udm-buf        
                move "tudm" to nome-file
                perform RELAZIONI-ARTICOLI
                if not trovato
                   set errori to true
                   move 78-ID-ef-udm to CONTROL-ID
                   display message box "Unità di misura NON valida"
                           title = tit-err
                           icon MB-WARNING-ICON
                end-if  

           |78-ID-ef-imballo è l'ID del control ef-imballo
           when 78-ID-ef-imballo
                inquire ef-imballo, value in ef-imballo-buf
                move ef-imballo-buf  to art-imballo-standard of articoli
                move "timbalqta"          to nome-file
                perform RELAZIONI-ARTICOLI
                if not trovato
                   set errori to true
                   move 78-ID-ef-imballo to control-id
                   display message box "Codice d'imballo NON valido"
                           title = tit-err
                           icon mb-warning-icon
                end-if 

           |78-ID-ef-udm-imballo è l'ID del control ef-udm-imballo
           when 78-ID-ef-udm-imballo
                inquire ef-udm-imballo, value in ef-udm-imballo-buf
                if ef-udm-imballo-buf = spaces
                   set errori to true
                   move 78-ID-ef-udm-imballo to CONTROL-ID
                   display message "U.d.m. per imballo obbligatoria"
                           title = tit-err
                           icon MB-WARNING-ICON
                end-if

           |78-ID-ef-dogana è l'ID del control ef-dogana
           when 78-ID-ef-dogana
                inquire ef-dogana, value in ef-dogana-buf        
                move ef-dogana-buf to art-cod-doganale of articoli
                move "tnomen" to nome-file
                perform RELAZIONI-ARTICOLI
                if not trovato
                   set errori to true
                   move 78-ID-ef-dogana to CONTROL-ID
                   display message box "Codice doganale NON valido"
                           title = tit-err
                           icon MB-WARNING-ICON
                else
                   evaluate true
                   when nom-si-utf 
                        if mod-peso-utf = 0 |ho cambiato lo stato
                           modify cbo-utf, reset-list = 1
                           perform RIEMPI-COMBO-UTF
                           set soggetto to true
                           perform CARICA-COMBO-UTF 
                           move 0 to mod-peso-non-utf
                        end-if
                        move 1 to mod-peso-utf
                   when nom-no-utf 
                        if mod-peso-utf = 1 |ho cambiato lo stato
                           modify cbo-utf, reset-list = 1
                           perform RIEMPI-COMBO-NON-UTF
                           set non-soggetto to true
                           perform CARICA-COMBO-UTF
                        end-if
                        move 0 to mod-peso-utf
                        move 1 to mod-peso-non-utf
                   end-evaluate

                   evaluate true
                   when nom-si-cobat move 1 to mod-cobat
                   when nom-no-cobat move 0 to mod-cobat
                                     move 0 to ef-amperaggio-buf
                   end-evaluate 







                   evaluate true
                   when nom-si-ic-cou move 1 to mod-imposte
                   when nom-no-ic-cou move 0 to mod-imposte
                   end-evaluate

                   if mod-peso-utf = 0
                      move 0 to ef-peso-utf-buf
                   end-if
                   if mod-peso-non-utf = 0
                      move 0 to ef-peso-non-utf-buf
                   end-if


                   if mod-imposte = 1
                      inquire ef-perce-cou,
                                         value art-perce-cou of articoli
                   else
                      move 0 to ef-perce-imposte-buf
                                ef-perce-cou-buf
                   end-if

                   display ef-peso-utf      ef-peso-non-utf
                           ef-perce-imposte ef-perce-cou
                           ef-amperaggio    cbo-cobat

                   move ef-perce-imposte-buf 
                                      to art-perce-imposte of articoli
                   perform SET-ASSOGGETTAMENTO-DEFAULT
                end-if

           |78-ID-cbo-utf è l'ID del control ef-peso-utf
           when 78-ID-cbo-utf
                perform SCARICA-COMBO-UTF
                evaluate true
                when soggetto     move 1 to mod-peso-utf
                                  move 0 to mod-peso-non-utf
                                  move 0 to ef-peso-non-utf-buf
                when misto        move 1 to mod-peso-utf
                                  move 1 to mod-peso-non-utf
                when non-soggetto move 0 to mod-peso-utf
                                  move 1 to mod-peso-non-utf   
                                  move 0 to ef-peso-utf-buf
                end-evaluate
                display ef-peso-utf ef-peso-non-utf

           |78-ID-ef-peso-utf è l'ID del control ef-peso-utf
           when 78-ID-ef-peso-utf
                inquire ef-peso-utf, value in ef-peso-utf-buf
                move ef-peso-utf-buf to art-peso-utf of articoli
                if art-peso-utf of articoli = 0 and mod-peso-utf = 1
                   set errori to true
                   move 78-ID-ef-peso-utf to CONTROL-ID
                   display message box "Inserimento peso UTF mancante"
                           title = tit-err
                           icon MB-WARNING-ICON
                end-if

           |78-ID-ef-peso-non-utf è l'ID del control ef-peso-non-utf
           when 78-ID-ef-peso-non-utf
                inquire ef-peso-non-utf, value in ef-peso-non-utf-buf
                move ef-peso-non-utf-buf to art-peso-non-utf of articoli
                if art-peso-non-utf of articoli = 0 
                                            and mod-peso-non-utf = 1
                   set errori to true
                   move 78-ID-ef-peso-non-utf to CONTROL-ID
                   display message "Inserimento peso non UTF mancante"
                           title = tit-err
                           icon MB-WARNING-ICON
                end-if

           |78-ID-ef-perce-imposte è l'ID del control ef-perce-imposte
           when 78-ID-ef-perce-imposte
                inquire ef-perce-imposte, value in ef-perce-imposte-buf
                move ef-perce-imposte-buf 
                                      to art-perce-imposte of articoli
                if mod-imposte = 1
      *****             perform SET-ASSOGGETTAMENTO-DEFAULT

                   set errori to true
                   evaluate true 
                   when nom-uguale
                        if art-perce-imposte of articoli = nom-perce
                           set tutto-ok to true
                        end-if
                   when nom-maggiore
                        if art-perce-imposte of articoli > nom-perce
                           set tutto-ok to true
                        end-if
                   when nom-minore
                        if art-perce-imposte of articoli < nom-perce
                           set tutto-ok to true
                        end-if
                   when nom-mag-u
                        if art-perce-imposte of articoli >= nom-perce
                           set tutto-ok to true
                        end-if
                   when nom-min-u
                        if art-perce-imposte of articoli <= nom-perce
                           set tutto-ok to true
                        end-if
                   end-evaluate
                   if errori
                      move 78-ID-ef-perce-imposte to CONTROL-ID
                      display message "Assoggettamento imposte errato."
                               x"0d0a""Confermi?"
                                title titolo
                                 type mb-yes-no
                                 icon 2
                               giving scelta
                              default mb-no
                      if scelta = mb-yes set tutto-ok to true end-if
                   end-if
                end-if 
                                  
           when 78-ID-ef-perce-cou
                inquire ef-perce-cou, value in ef-perce-cou-buf
                move ef-perce-cou-buf to art-perce-cou of articoli
                if art-perce-cou of articoli = 0 and mod-imposte = 1
      *****             set errori to true
      *****             move 78-ID-ef-perce-cou to control-id
      *****             display message
      *****                     "Inserimento percentuale COU mancante"
      *****                       title tit-err
      *****                        icon 2
                   move 78-ID-ef-perce-cou to control-id
                   display message "Assoggettamento COU errato."
                            x"0d0a""Confermi?"
                             title titolo
                              type mb-yes-no
                              icon 2
                            giving scelta
                           default mb-no
                   if scelta = mb-yes 
                      set tutto-ok to true 
                   end-if
                end-if

           |78-ID-ef-amperaggio è l'ID del control ef-amperaggio
           when 78-ID-ef-amperaggio
                inquire ef-amperaggio, value in ef-amperaggio-buf
                move ef-amperaggio-buf to art-amperaggio of articoli
                if art-amperaggio of articoli = 0 and mod-cobat = 1       
                   set errori to true
                   move 78-ID-ef-amperaggio to CONTROL-ID
                   display message "Inserimento amperaggio mancante"
                           title = tit-err
                           icon MB-WARNING-ICON
                end-if

           |78-ID-ef-amperaggio è l'ID del control ef-amperaggio
           when 78-ID-ef-scorta
                inquire ef-scorta, value in ef-scorta-buf
                move ef-scorta-buf to art-scorta of articoli
                                      sco-codice
                read tscorte
                     invalid
                     set errori to true
                     move 78-ID-ef-scorta  to CONTROL-ID
                     display message "Tipologia Scorta non valido"
                               title tit-err
                               icon 2
                end-read
                if sco-moq-si
                   move 1 to v-moq
                else
                   move 0 to v-moq
                   move 0 to ef-moq-buf
                end-if
                display ef-moq
                display lab-moq
                if art-scorta of articoli = 9
                   move 1 to v-reale
                else
                   move 0 to v-reale
                   move 0 to ef-reale-buf
                end-if
                display ef-reale
                display lab-reale

           |78-ID-ef-reale è l'ID del control ef-reale
           when 78-ID-ef-reale
                if v-reale = 1
                   inquire ef-reale, value in ef-reale-buf
                   move ef-reale-buf to art-peso-reale of articoli
                   if art-peso-reale of articoli = 0
                      set errori to true
                      display message "Peso reale obbligatorio"
                                title tit-err
                                 icon 2
                   end-if
                end-if

           |78-ID-ef-forn è l'ID del control ef-forn
           when 78-ID-ef-forn
                inquire ef-forn, value in ef-forn-buf
                move ef-forn-buf to cli-codice
                move spaces      to lab-forn-buf
                if cli-codice not = 0
                   move "fornitori" to nome-file
                   perform RELAZIONI-ARTICOLI
                   if not trovato
                      set errori to true
                      move 78-ID-ef-forn to CONTROL-ID
                      display message box "Fornitore NON valido"
                              title = tit-err
                              icon MB-WARNING-ICON
                   end-if
                end-if
                modify lab-forn title lab-forn-buf

           |78-ID-ef-destino è l'ID del control ef-destino
           when 78-ID-ef-destino
                inquire ef-forn, value in ef-forn-buf
                move ef-forn-buf to desf-codice

                inquire ef-destino, value in ef-destino-buf
                move ef-destino-buf to desf-prog

                move spaces      to lab-dest-buf
                if desf-codice not = 0
                   move "destinif" to nome-file
                   perform RELAZIONI-ARTICOLI
                   if not trovato
                      set errori to true
                      move 78-ID-ef-destino to CONTROL-ID
                      display message box "Destini NON valido"
                              title = tit-err
                              icon MB-WARNING-ICON
                   end-if
                else
                   if desf-prog not = zero
                      set errori to true
                      move 78-ID-ef-destino to CONTROL-ID
                      display message box "Destino NON valido"
                              title = tit-err
                              icon MB-WARNING-ICON
                   end-if
                end-if
                modify lab-dest title lab-dest-buf

           |78-ID-cbo-stato è l'ID del campo cbo-stato
           when 78-ID-cbo-stato
                perform NO-SOSPESO
                perform SCARICA-COMBO-STATO
                move stato   to  art-stato of articoli
                if art-stato of articoli not = old-art-stato
                   move 16 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"            
                   if bloccato
                      initialize prg-chiave 
                                 replacing numeric data by zeroes
                                      alphanumeric data by spaces
                      move art-codice of articoli to prg-cod-articolo
                      read progmag no lock
                      if prg-giacenza not = 0
                         display message
                  "Impossibile bloccare: giacenza movimentata"
                                   title tit-err
                                    icon 2 
                         set Passwd-StatusOk to false
                      end-if
                   end-if

                   if not Passwd-StatusOk
                      move old-art-stato to stato
                      perform CARICA-COMBO-STATO
                   else
                      if disattivo
                         move control-id to store-id
                         perform CHANGE-STATUS
                         move store-id to CONTROL-ID
                      end-if
                   end-if
               end-if

           when 78-id-ef-dt-val-da
                move ef-dt-val-da-buf to como-data
                perform DATE-FORMAT
                move como-data to ef-dt-val-da-buf
                modify ef-dt-val-da value ef-dt-val-da-buf

                perform DATE-TO-FILE
                move como-data to data-val-da
      *          perform CARICA-LISTINI

           when 78-id-ef-dt-val-a
                inquire ef-dt-val-da value como-data
                perform DATE-TO-FILE
                move como-data to data-val-da

                move ef-dt-val-a-buf to como-data
                perform DATE-FORMAT
                move como-data to ef-dt-val-a-buf
                modify ef-dt-val-a value ef-dt-val-a-buf

                perform DATE-TO-FILE
                move como-data to data-val-a

                if data-val-da > data-val-a
                   set errori to true
                   move 78-id-ef-dt-val-da to CONTROL-ID
                   display message box "Intervallo date non valido"
                           title = tit-err
                           icon MB-WARNING-ICON
                end-if
                if tutto-ok
                   perform CARICA-LISTINI
                end-if

           when 78-ID-ef-litri
                if nuovo
                   inquire ef-litri value in art-litri of articoli
                   if art-litri of articoli = 0
                      display message "Confermi valore 0 litri?"
                                title titolo
                                 type mb-yes-no
                                 icon mb-warning-icon
                               giving scelta
                               default mb-no
                      if scelta = mb-no
                         set errori to true
                      end-if
                   end-if
                end-if
                           

           when 78-ID-ef-prodener
                inquire ef-dogana, value in nom-codice
                read tnomen no lock 
                     invalid set nom-no-ic-cou to true
                end-read
                inquire ef-prodener, value in ef-prodener-buf
                if ef-prodener-buf = spaces
                   move spaces to pen-descrizione lab-prodener-buf
                   display lab-prodener
                   if nom-si-ic-cou
                      set errori to true
                      move 78-ID-ef-prodener to control-id
                      display message "Prodotto energetico NON valido"
                                title tit-err
                                 icon 2
                   end-if
                else
                   move ef-prodener-buf  to pen-codice
                   move "prodener"          to nome-file
                   perform RELAZIONI-ARTICOLI
                   if not trovato
                      set errori to true
                      move 78-ID-ef-prodener to control-id
                      display message "Prodotto energetico NON valido"
                                title tit-err
                                 icon 2
                   else
      *       controllo in base al trattamento UTF
                      perform SCARICA-COMBO-UTF
                      evaluate true 
                      when soggetto
                      when misto
                           if pen-si-sdoppia-riga
                              set errori to true
                              move 78-ID-ef-prodener to control-id
                              display message box   
                                  "Prodotto energetico NON compatibile"
                                   title = tit-err
                                   icon mb-warning-icon
                           end-if
                      when non-soggetto
                           if not pen-si-sdoppia-riga
                              set errori to true
                              move 78-ID-ef-prodener to control-id
                              display message box   
                                  "Prodotto energetico NON compatibile"
                                   title = tit-err
                                   icon mb-warning-icon
                           end-if
                      end-evaluate                   
                   end-if
                end-if 

           when 78-ID-ef-altezza
                move ef-scorta-buf to como-scorta
                if como-scorta = 5 and nuovo
                   move ef-altezza-buf to como-altezza
                   if como-altezza = 0
                      set errori to true
                      display message 
                    "Altezza bancale obbligatoria per articoli scorta 5"
                                title tit-err
                                 icon 2
                   end-if
                end-if 

           when 78-ID-ef-larghezza
                move ef-scorta-buf to como-scorta
                if como-scorta = 5 and nuovo
                   move ef-larghezza-buf to como-larghezza
                   if como-larghezza = 0
                      set errori to true
                      display message 
                  "Larghezza bancale obbligatoria per articoli scorta 5"
                                title tit-err
                                 icon 2
                   end-if
                end-if 

           when 78-ID-ef-profondita
                move ef-scorta-buf to como-scorta
                if como-scorta = 5 and nuovo
                   move ef-profondita-buf to como-profondita
                   if como-profondita = 0
                      set errori to true
                      display message 
                 "Profondita bancale obbligatoria per articoli scorta 5"
                                title tit-err
                                 icon 2
                   end-if
                end-if 

           when 78-ID-ef-qta-EPAL
                inquire ef-qta-EPAL, value in ef-qta-EPAL-buf
                move ef-qta-EPAL-buf to como-qta-EPAL
                if como-qta-EPAL > 0
                   inquire ef-imballo, value in imq-codice
                   read timbalqta no lock
                        invalid continue
                    not invalid
                        if como-qta-EPAL >= imq-qta-imb
                           move 0 to resto
                           divide como-qta-EPAL by imq-qta-imb
                                  giving cont
                                  remainder resto
                        else
                           move 1 to resto
                        end-if
                        if resto not = 0 
                           display message "ATTENZIONE!"
                                x"0d0a""La quantità bancale EPAL non è"
                                x"0d0a""multipla dell'imballo standard"
                                     title titolo
                                      icon 2
                       end-if
                   end-read

           when 78-ID-ef-qta-STD 
                inquire ef-qta-STD, value in ef-qta-STD-buf
                move ef-qta-STD-buf to como-qta-STD
                if como-qta-STD > 0
                   inquire ef-imballo, value in imq-codice
                   read timbalqta no lock
                        invalid continue
                    not invalid
                        if como-qta-STD >= imq-qta-imb
                           move 0 to resto
                           divide como-qta-STD by imq-qta-imb
                                  giving cont
                                  remainder resto
                        else
                           move 1 to resto
                        end-if
                        if resto not = 0 
                           display message "ATTENZIONE!"               
                                x"0d0a""La quantità bancale STD non è"
                                x"0d0a""multipla dell'imballo standard"
                                     title titolo
                                      icon 2
                       end-if
                   end-read
                end-if
                move ef-scorta-buf to como-scorta
                if como-scorta = 5 and nuovo
                   move ef-qta-EPAL-buf to como-qta-EPAL
                   move ef-qta-STD-buf  to como-qta-STD
                   if como-qta-EPAl = 0 and como-qta-STD = 0
                      set errori to true
                      move 78-ID-ef-qta-EPAL to control-id
                      display message 
           "QTa bancale (EPAL o STD) obbligatoria per articoli scorta 5"
                                title tit-err
                                 icon 2
                   end-if
                end-if

           end-evaluate.


           if errori
              perform CANCELLA-COLORE
              move CONTROL-ID to store-id
              move 4          to ACCEPT-CONTROL
           end-if.

      ***---
       CURRENT-RECORD.
           perform RIEMPI-CHIAVE.
           set tutto-ok  to true.
           set ReadSecca to true.
           if nuovo move 0 to mod end-if.

           if mod = 1
              read articoli lock invalid 
                 set errori to true 
              end-read
           else
              read articoli no lock invalid 
                 set errori to true 
              end-read
           end-if.                    
           set ReadSecca to false.

           if RecLocked
              set RecLocked to false
              set errori    to true
           else
              if tutto-ok
                 if nuovo
                    move 3 to NumBitmapCodici
                    move 0 to mod-campi
                    move 0 to mod
                    move 1 to mod-k
                    move 78-ID-ef-codice to control-id    
                    move 4 to accept-control
                 end-if   
                 perform FORM1-IUD-DISPLAY
                 set vecchio to true
                 if mod = 1                        
                    move 1 to NumBitmapCodici
                    set StatusModifica to true
                 else                              
                    move 3 to NumBitmapCodici
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
       DESCRIZIONE-ARTICOLO.
           initialize lab-des-buf.
           inspect ef-des1-buf replacing trailing spaces by low-value.
           string  ef-des1-buf delimited by low-value
                   ef-des2-buf delimited by size
                  into lab-des-buf
           end-string.
           display lab-des.
           inspect ef-des1-buf replacing trailing low-value by spaces.
      *****          move ef-des-buf to lab-des-buf
      *****          display lab-des

      ***---
       DESCRIZIONE-IMBALLO.
           initialize lab-imballo-buf.
           inspect imb-descrizione replacing trailing spaces 
                                   by low-value.
           move imq-qta-imb to qta-x.
           inspect qta-x replacing leading x"30" by x"20".
           call "C$JUSTIFY" using qta-x, "L".
           inspect qta-x replacing trailing spaces by low-value.
           string imb-descrizione delimited by low-value
                  " da "          delimited by size
                  qta-x           delimited by low-value
                  into lab-imballo-buf
           end-string.

      ***---
       INIT.
           move 0 to StatusHelp. 
           move 1 to mod-k.
           set StatusVisua to true.

           |Riempio il tabellino contenente i riferimenti agli ID
           |relativi ad ogni pagina del tab-control
           |specifico per i pgm. aventi Tab-Control
           move 78-ID-ef-des1    to min-id(1).
           move 78-ID-cbo-stato  to max-id(1).

           move 78-ID-ef-note-agg     to min-id(2).
           move 78-ID-ef-qta-std      to max-id(2).
           |*******
           perform RIEMPI-COMBO-STATO.
           perform RIEMPI-COMBO-UTF.
           perform RIEMPI-COMBO-COBAT.

           move "Attivo" to cbo-stato-buf.
           Modify  cbo-stato,   value cbo-stato-buf.

           move "Soggetto" to cbo-utf-buf.
           Modify  cbo-utf,   value cbo-utf-buf.

           move "Auto" to cbo-cobat-buf.
           Modify  cbo-cobat,   value cbo-cobat-buf.

      ***---
       INIT-OLD-REC.
           initialize old-art-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           set  attivo            to true
           move stato             to old-art-stato.
           set  non-soggetto      to true.
           move gestione-utf      to old-art-gestione-utf.
           set  auto-w            to true.
           move cobat             to old-art-auto-moto-per-cobat.
           move CodiceIvaStd      to old-art-codice-iva.
                                                            
           set old-art-no-imposte   to true.
           set old-art-no-cobat     to true.
           set old-art-confezionato to true.

      ***---
       MODIFICA.
           move 5 to key-status.
           inquire tool-modifica, value in mod.
           set tutto-ok to true.

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
                    if control-id <= min-id(idx) or
                       control-id >= max-id(idx)
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
                    unlock articoli all records
                    if KeyboardSaved
                       set KeyboardReleased to true
                       set environment "KEYSTROKE" to "DATA=44 44"
                       set environment "KEYSTROKE" to "DATA=46 46"
                    end-if
                    move 78-ID-ef-codice to control-id
                 end-if
              end-if
                                   
              display form1
              modify tool-modifica,  value mod
              perform CANCELLA-COLORE

              move 4 to accept-control
              move 0 to StatusHelp
              perform STATUS-HELP
           end-if.

      ***---
       NUOVO.
           perform SALV-MOD.

           if tutto-ok           
              |specifico per i pgm. aventi Tab-Control
              move 1 to Screen1-Ta-1-TAB-VALUE
              perform SCREEN1-TA-1-TABCHANGE
              |******          
              move 78-ID-ef-des1 to control-id
              move 4 to accept-control
              move 1 to mod
              move 0 to mod-k
              modify tool-modifica,  value = mod
              perform CANCELLA-COLORE
              perform FORM1-CLEAR

              perform VALORIZZA-NUOVO
              perform INIT-OLD-REC
              
              |Specifico del pgm. garticoli
              set disattivo to true |Sospeso in attesa di conferma
              perform CARICA-COMBO-STATO

              move stato             to old-art-stato

              set non-soggetto to true
              perform CARICA-COMBO-UTF

              set auto-w to true
              perform CARICA-COMBO-COBAT

              move 1 to mod-campi,   mod-peso-non-utf
              move 0 to mod-imposte, mod-cobat, mod-peso-utf 
              move 1 to NumBitmapCodici

              move 0 to v-moq
              move 0 to v-reale
                        
              display form1

              set StatusIns to true
              perform STATUS-BAR-MSG 
              unlock articoli all records

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
       PRECEDENTE.
           perform SALV-MOD.           
           if tutto-ok
              perform CHECK-INSERIMENTO
              unlock articoli all records

              if mod = 1
                 move 0 to mod-k
                 set dataset1-articoli-lock to true
              else              
                 move 1 to mod-k     
                 initialize dataset1-articoli-lock-flag
              end-if

              perform CANCELLA-COLORE
              perform FORM1-PREVIOUS

              if RecLocked
                 set RecLocked to false             
                 initialize dataset1-articoli-lock-flag
                 move 1 to mod-k
                 perform FORM1-PREVIOUS
              end-if 
           end-if.

      ***---
       PRIMO.
           perform SALV-MOD.           
           if tutto-ok     
              perform CHECK-INSERIMENTO
              unlock articoli all records

              if mod = 1
                 move ZERO to mod-k
                 set dataset1-articoli-lock to true
              else              
                 move 1 to mod-k     
                 initialize dataset1-articoli-lock-flag
              end-if

              perform CANCELLA-COLORE
              perform FORM1-FIRST

              if RecLocked
                 set RecLocked to false             
                 initialize dataset1-articoli-lock-flag
                 move 1 to mod-k
                 perform FORM1-FIRST
              end-if 
           end-if.

      ***---
       RELATIONS.
           set trovato to true.

           evaluate nome-file
           when "articoli1"   
                perform READ-ARTICOLI1
           when "tsetmerc"   
                perform READ-TSETMERC
           when "tmarche"    
                perform READ-TMARCHE
           when "tmagaz"     
                perform READ-TMAGAZ
           when "tcla1art"   
                perform READ-TCLA1ART
           when "tivaese"    
                perform READ-TIVAESE
           when "tnomen"     
                perform READ-TNOMEN
           when "tudm"       
                perform READ-TUDM  
           when "timbalqta"  
                perform READ-TIMBALQTA
           when "fornitori"  
                perform READ-FORNITORI
           when "destinif"  
                perform READ-DESTINIF
           when "prodener"
                perform READ-PRODENER
           when "blister"
                perform READ-BLISTER
           when "tscorte"
                perform READ-TSCORTE
           end-evaluate.

      ***---
       READ-TSCORTE.
           read tscorte no lock
              invalid 
                 set trovato to false
           end-read.

      ***---
       READ-ARTICOLI1.
           read articoli1 no lock
              invalid 
                 move spaces to art-descrizione of articoli1
                 set trovato to false
           end-read.
       
      ***---
       READ-TSETMERC.
           read tsetmerc no lock
                invalid move spaces to sme-descrizione
                        set trovato to false
           end-read.

      ***---
       READ-TMARCHE. 
           read tmarche no lock
                invalid move spaces to mar-descrizione
                        set trovato to false
           end-read.
       
      ***---
       READ-TMAGAZ. 
           read tmagaz no lock
                invalid move spaces to mag-descrizione
                        set trovato to false
           end-read.
       
      ***---
       READ-PRODENER.  
           read prodener no lock
                invalid move spaces to pen-descrizione
                        set trovato to false
           end-read.
       
      ***---
       READ-BLISTER.  
           read blister no lock
                invalid move spaces to bli-descrizione
                        set trovato to false
           end-read.

      ***---
       READ-TCLA1ART.
           read tcla1art no lock
                invalid move spaces to cl1-descrizione
                        set trovato to false
           end-read.
       
      ***---
       READ-TIVAESE.
           read tivaese no lock
                invalid move spaces to tbliv-descrizione1
                        set trovato to false
           end-read.
       
      ***---
       READ-TNOMEN.  
           read tnomen no lock
                invalid move spaces to nom-descrizione
                        set trovato to false
           end-read.
       
      ***---
       READ-TUDM.    
           read tudm no lock
                invalid move spaces to udm-descrizione
                        set trovato to false
           end-read.
       
      ***---
       READ-FORNITORI.    
           read clienti no lock
                invalid move spaces to cli-ragsoc-1
                        set trovato to false
           end-read.

      ***---
       READ-DESTINIF.
           read destinif no lock
              invalid 
                 move spaces to desf-ragsoc-1
                 set trovato to false
           end-read.

      ***---
       READ-TIMBALQTA.    
           read timbalqta no lock
                invalid 
                initialize imb-descrizione
                set trovato to false
            not invalid
                move imq-tipo  to imb-codice
                read timballi
                   invalid 
                      initialize imb-descrizione
                   not invalid 
                      perform DESCRIZIONE-IMBALLO
                end-read
           end-read.

      ***---
       RELAZIONI-ARTICOLI.
           set trovato to false.
      
           evaluate nome-file
           when "articoli1"  
                move spaces to lab-des-coll-buf
                move ef-coll-buf to art-codice of articoli1
                if art-codice of articoli1 = zero
                   set trovato  to true
                else
                   perform RELATIONS
                   move art-descrizione of articoli1 to lab-des-coll-buf
                end-if
                display lab-des-coll
           when "tsetmerc"  
                move spaces to lab-setmerc-buf
                move ef-setmerc-buf to sme-codice
                perform RELATIONS
                move sme-descrizione to lab-setmerc-buf
                display lab-setmerc
           when "tmarche"
                move spaces to lab-marca-buf
                move ef-marca-buf to mar-codice
                if mar-codice not = 0
                   perform RELATIONS
                   move mar-descrizione to lab-marca-buf
                end-if
                display lab-marca
           when "tmagaz"
                move spaces to lab-mag-buf
                move ef-mag-buf to mag-codice
                if mag-codice not = 0
                   perform RELATIONS
                   move mag-descrizione to lab-mag-buf
                end-if
                display lab-mag
           when "tcla1art"                
                move spaces to lab-classe-buf
                move ef-classe-1-buf to cl1-codice
                if cl1-codice not = 0
                   perform RELATIONS
                   move cl1-descrizione to lab-classe-buf
                end-if
                display lab-classe
           when "tivaese"
                initialize lab-iva-buf
                move "IV"       to tbliv-codice1
                move ef-iva-buf to tbliv-codice2
                perform RELATIONS
                if trovato
                   inspect tbliv-descrizione1 replacing   trailing
                                                spaces by low-value
                   string tbliv-descrizione1 delimited by low-value
                          " "                delimited by size
                          tbliv-descrizione2 delimited by size
                          into lab-iva-buf
                   end-string
                end-if
                display lab-iva
           when "tnomen"
                move spaces to lab-dogana-buf
                move ef-dogana-buf to nom-codice
                perform RELATIONS
                move nom-descrizione to lab-dogana-buf

                evaluate true
                when nom-si-ic-cou 
                     set art-si-imposte of articoli to true
                     move 1    to chk-imposte-buf
                when nom-no-ic-cou 
                     set art-no-imposte of articoli to true
                     move 0 to chk-imposte-buf
                end-evaluate

                evaluate true
                when nom-si-cobat 
                     set art-si-cobat of articoli   to true
                     move 1    to chk-cobat-buf
                when nom-no-cobat 
                     set art-no-cobat of articoli   to true
                     move 0 to chk-cobat-buf
                end-evaluate

                if mod = 1
                   if chk-imposte-buf = 1
                      move 1 to mod-imposte
                   else
                      move 0 to mod-imposte                    
                   end-if
                   if chk-cobat-buf = 1
                      move 1 to mod-cobat
                   else
                      move 0 to mod-cobat
                   end-if
                end-if
                display lab-dogana, chk-imposte, chk-cobat, ef-perce-cou
                        ef-amperaggio, ef-perce-imposte, cbo-cobat

           when "tudm"
                move spaces to lab-udm-buf
                move ef-udm-buf to udm-codice
                if udm-codice not = space
                   perform RELATIONS
                   move udm-descrizione to lab-udm-buf
                end-if
                display lab-udm                                   

           when "fornitori"
                move spaces      to lab-forn-buf
                set cli-tipo-F   to true
                move ef-forn-buf to cli-codice
                if cli-codice not = 0
                   perform RELATIONS
                   move cli-ragsoc-1 to lab-forn-buf
                end-if
                display lab-forn
   
           when "destinif"
                move spaces      to lab-dest-buf
                move ef-forn-buf to desf-codice
                move ef-destino-buf to desf-prog
                if desf-prog not = 0
                   perform RELATIONS
                   move desf-ragsoc-1 to lab-dest-buf
                end-if
                display lab-dest
   
           when "timbalqta"
                move ef-imballo-buf to imq-codice
                if imq-codice not = space
                   perform RELATIONS
                   if trovato perform DESCRIZIONE-IMBALLO
                   else       initialize lab-imballo-buf
                   end-if
                end-if
                display lab-imballo
           when "tparamge"
                move spaces to tge-codice
                read tparamge no lock  
                     invalid continue
                 not invalid move tge-cod-iva-std to CodiceIvaStd
                end-read
           when "prodener"
                move spaces to lab-prodener-buf
                move ef-prodener-buf to pen-codice
                perform RELATIONS
                initialize lab-prodener-buf
                if pen-descrizione not = spaces
                   string "CPA: "      delimited size
                          pen-cpa      delimited size
                          " - NC: "    delimited size
                          pen-nc       delimited size
                          " - TARIC: " delimited size
                          pen-taric    delimited size
                          " - DAC:"    delimited size
                          pen-dac      delimited size
                          into lab-prodener-buf
                   end-string
                else
                   move spaces to lab-prodener-buf
                end-if
                display lab-prodener 
           when "blister"  
                move spaces to lab-des-coll-buf
                move ef-coll-buf to bli-codice
                perform RELATIONS
                move bli-descrizione to lab-des-coll-buf
                display lab-des-coll
           when "tscorte"  
                move ef-scorta-buf to sco-codice
                perform RELATIONS
           end-evaluate.

      ***---
       RIEMPI-CHIAVE.
           move ef-codice-buf to art-codice of articoli with convert.

      ***---
       SALV-MOD.
           set tutto-ok to true.
           perform FORM1-CONTROLLO-OLD.
                     
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
       SALVA.
           if MOD = zero exit paragraph end-if.

           set tutto-ok    to true.

           perform varying control-id from 78-ID-ef-codice by 1
                     until control-id > 78-ID-ef-qta-std
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
              move ef-qta-std-buf  to art-qta-std  of articoli
              move ef-qta-epal-buf to art-qta-epal of articoli
              if art-qta-std  of articoli = 0 and
                 art-qta-epal of articoli = 0
                 set errori to true
                 move 78-ID-ef-qta-epal to store-id
                 display message 
            "Indicare il numero di pezzi per pallet (o epal o standard)"
                           title tit-err
                            icon 2
              end-if
           end-if.

           if tutto-ok
              if art-collegato of articoli not = old-art-collegato
                 move 10 to Passwd-password
                 call   "passwd" using Passwd-linkage
                 cancel "passwd"

                 if not Passwd-StatusOk
                    if old-art-collegato = 0
                       move 0 to ef-coll-buf
                       move spaces to lab-des-coll-buf
                    else
                       move old-art-collegato to ef-coll-buf 
                                   art-codice of articoli1
                       read articoli1 no lock
                       move art-descrizione of articoli1 
                         to lab-des-coll-buf
                    end-if
                    display ef-coll lab-des-coll
                 end-if
              end-if
           end-if.

           if tutto-ok
              if art-scorta of articoli not = old-art-scorta
                 move 15 to Passwd-password
                 call   "passwd" using Passwd-linkage
                 cancel "passwd"

                 if not Passwd-StatusOk
                    move old-art-scorta to ef-scorta-buf 
                    display ef-scorta
                 end-if
              end-if
           end-if.

           if tutto-ok  
              if art-mag-std of articoli not = old-art-mag-std and 
                 vecchio
                 move 19 to Passwd-password
                 call   "passwd" using Passwd-linkage
                 cancel "passwd"

                 if not Passwd-StatusOk
                    move old-art-mag-std to ef-mag-buf mag-codice
                    display ef-mag

                    read tmagaz no lock
                    move mag-descrizione to lab-mag-buf
                    display lab-mag
                 end-if
              end-if
           end-if.

           set SaveXX to true.
           perform ACCESSOXX.

           if tutto-ok
      *****     if art-attivo of articoli or art-bloccato of articoli
              |Devo farlo sempre x' anche i sospesi hanno progressivi
              perform VALUTA-CAMBIO-VALORI-PROGMAG
      *****     end-if.
           end-if.

           if errori
              perform CANCELLA-COLORE
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
              |stato solamente alla fine, cioè quando gli altri sono
              |già andati TUTTI a buon fine
              move 78-ID-cbo-stato to control-id
              perform CONTROLLO     

LUBEXX        if tutto-ok
                 if nuovo or art-disattivo of articoli
                    if art-scorta of articoli = 5
                       perform CONTROLLA-PESO
                    end-if
                 end-if
                 perform FORM1-BUF-TO-FLD
                 perform CANCELLA-COLORE
                                       
                 accept como-ora from time
                 if nuovo
                    move data-oggi to art-data-creazione of articoli
                    move como-ora  to art-ora-creazione of articoli
                    move user-codi to art-utente-creazione of articoli   
                 else
                    move data-oggi 
                                to art-data-ultima-modifica of articoli
                    move como-ora  
                                to art-ora-ultima-modifica of articoli
                    move user-codi 
                               to art-utente-ultima-modifica of articoli
                 end-if          
                                      
                 evaluate true
                 when soggetto     
                      move 0 to art-peso-non-utf of articoli
                                ef-peso-non-utf-buf
                 when non-soggetto 
                      move 0 to art-peso-utf of articoli
                                ef-peso-utf-buf
                 end-evaluate            
                 add art-peso-utf of articoli 
                  to art-peso-non-utf of articoli 
                              giving art-peso-standard of articoli

                 if nuovo or art-disattivo of articoli
                    |Articolo nuovo è per forza SOSPESO
                    move 1 to mod-k
                    move 0 to mod
                    modify tool-modifica, value = mod
                    set link-batch to true
                    perform SCRIVI-PROGMAG
                 else
      *              if art-attivo   of articoli or 
      *                 art-bloccato of articoli
                       |Scrivo subito il progressivo per poter permettere
                       |al pgm di ordini f di inserire un articolo sospeso
                       initialize prg-chiave 
                                  replacing numeric data by zeroes
                                       alphanumeric data by spaces
                       move art-codice of articoli to prg-cod-articolo
                       read progmag no lock
                            invalid 
                            set link-batch to true
                            perform SCRIVI-PROGMAG
                       end-read
      *              end-if
                 end-if

                 if art-no-imposte of articoli 
                    move 0 to art-perce-imposte of articoli
                              ef-perce-imposte-buf
                 end-if

                 if art-no-cobat of articoli
                    move 0   to art-amperaggio of articoli
                                ef-amperaggio-buf
                    if art-moto-cobat of articoli
                       set art-auto-cobat of articoli to true
                       set auto-w         to true
                       perform CARICA-COMBO-COBAT
                    end-if
                 end-if

                 if art-bloccato  of articoli or 
                    art-disattivo of articoli
                    move 0 to art-scorta of articoli ef-scorta-buf
                    display ef-scorta
                 end-if

                 write art-rec of articoli
                       invalid rewrite art-rec of articoli
                 end-write             

                 |Allineare anche i progressivi
                 close      progmag
                 open i-o   progmag
                 initialize prg-chiave replacing numeric data by zeroes
                                            alphanumeric data by spaces
                 move art-codice of articoli to prg-cod-articolo
                 start progmag key >= prg-chiave
                       invalid continue 
                   not invalid
                       perform until 1 = 2
                          read progmag next at end exit perform end-read
                          if prg-cod-articolo not = 
                             art-codice of articoli
                             exit perform
                          end-if
      
                          if RecLocked |IGNORA
                             exit perform
                          end-if

                          if prg-disattivo
                             if art-bloccato of articoli
                                set prg-attivo to true |--> lo creo cmq attivo
                             else
                                move art-stato of articoli to prg-stato
                             end-if
                             rewrite prg-rec 
                                     invalid continue 
                             end-rewrite
                          end-if

                          |26/11/2013: lo stato già impostato 
                          |dei progressivi non deve cambiare
      *****                    if prg-bloccato and art-attivo of articoli
      *****                       set prg-attivo to true
      *****                       rewrite prg-rec 
      *****                               invalid continue 
      *****                       end-rewrite
      *****                    end-if
      
      *****                    evaluate true
      *****                    |Lo allineo in ogni caso
      *****                    when prg-disattivo
      *****                         move art-stato of articoli to prg-stato
      *****                    |Non lo tocco: dev'essere fatto manualmente
      *****                    when prg-bloccato
      *****                         continue
      *****                    |Lo blocco se blocco articolo
      *****                    when prg-attivo
      *****                         if art-bloccato of articoli
      *****                            set prg-bloccato to true
      *****                         end-if
      *****                    end-evaluate
      *****                    rewrite prg-rec invalid continue end-rewrite
                       end-perform
                 end-start
                 close      progmag
                 open input progmag

                 if sincro-progmag
                    |Come prima cosa scrivo il progressivo coi nuovi valori
                    set link-batch to true
                    perform SCRIVI-PROGMAG

                    close      progmag
                    open i-o   progmag
                    |Dopodiché blocco gli altri
                    move low-value  to prg-rec
                    move art-codice of articoli to prg-cod-articolo
                    start progmag key >= prg-chiave
                          invalid continue 
                      not invalid
                       perform until 1 = 2
                          read progmag next at end exit perform end-read
                          if prg-cod-articolo not = 
                             art-codice of articoli
                             exit perform
                          end-if
                          |Aggiorno il padre
                          if prg-peso = 0
                             move link-utf     to prg-peso-utf 
                             move link-non-utf to prg-peso-non-utf 
                             move art-stato of articoli to prg-stato
                             rewrite prg-rec
                          else
                             |E blocco gli altri
                             if prg-tipo-imballo  = link-imballo and
                                prg-peso          = link-peso
                                move art-stato of articoli to prg-stato
                                rewrite prg-rec
                             else
                                set prg-bloccato to true
                                rewrite prg-rec 
                                        invalid continue 
                                end-rewrite
                             end-if
                          end-if
                       end-perform
                    end-start
                    close      progmag
                    open input progmag
                 end-if

                 if nuovo
                    call "W$MOUSE" using set-mouse-shape, wait-pointer
                    call   "st-art-det" using art-codice of articoli
                    cancel "st-art-det"
                    call "W$MOUSE" using set-mouse-shape, arrow-pointer
                 end-if

                 if art-collegato of articoli not = old-art-collegato
                    perform AGGIORNA-CATENA
                 end-if
         
                 initialize G2Agg-linkage
                 set G2Agg-art   to true
                 move art-codice of articoli to G2Agg-articolo
                 if nuovo set G2Agg-insert to true
                 else     set G2Agg-update to true
                 end-if
                 call   "G2Agg" using G2Agg-linkage
                 cancel "G2Agg"

                 set vecchio to true       
                 perform TORNA-IN-VISUA
LUBEXX        end-if
           end-if.

           perform DESTROYXX.
                    
           display form1.

      ***---
       SCRIVI-PROGMAG.
           move user-codi   to link-user.
           inquire ef-codice,      value in link-articolo.
           move lab-des-buf to link-des-articolo.
           inquire ef-imballo,     value in link-imballo
           inquire ef-mag,         value in link-magazzino.
           inquire ef-peso-utf     value in link-utf.
           inquire ef-peso-non-utf value in link-non-utf.
           add link-utf to link-non-utf giving link-peso.
           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".

      ***---
       SELEZIONA.
           move   "articoli-all" to     como-file.
           call   "zoom-gt"  using  como-file, art-rec of articoli
                             giving stato-zoom.
           cancel "zoom-gt".
      
           if stato-zoom = 0
              if old-art-chiave not = art-chiave of articoli
                 move art-chiave of articoli to save-chiave
                 perform SALV-MOD
                 if tutto-ok
                    move save-chiave  to art-chiave of articoli
                    modify ef-codice, VALUE = art-codice of articoli
                    move art-codice of articoli to ef-codice-buf
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
           move ef-codice-buf to art-descrizione of articoli.

      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           call "C$JUSTIFY" using art-descrizione of articoli, "L".

           start articoli key >= art-k1 of articoli
              invalid 
                 continue
              not invalid 
                 read articoli next 
           end-start.

           move "articoli-alfa-all" to como-file.
           call "zoom-gt" using   como-file, art-rec of articoli
                          giving  stato-zoom.
           cancel "zoom-gt".

           if stato-zoom = ZERO
              move art-codice of articoli    to codice-ed
              move codice-ed     to ef-codice-buf   
              call "C$JUSTIFY" using ef-codice-buf, "L"
              display ef-codice

              if old-art-descrizione not = art-descrizione of articoli
                 move art-descrizione of articoli to save-descrizione-k1
                 perform SALV-MOD
                 if tutto-ok
                    move save-art-K1 to art-descrizione of articoli
                    set ReadSecca  to true
                    modify ef-codice, value art-codice of articoli
                    perform CURRENT-RECORD
                    perform CANCELLA-COLORE
                    move 78-ID-ef-codice to CONTROL-ID       
                    move 4               to ACCEPT-CONTROL   
                 end-if                                  
              end-if
           end-if.

      ***---
       SELEZIONA-NUMERICO.
           if art-codice of articoli not > 0
              set errori to true
              move 78-ID-ef-codice to control-id
              display message box msg-codice-obbligatorio
                      title tit-err
                      icon  mb-warning-icon
           else          
              read articoli no lock
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
                       if old-art-chiave not = art-chiave of articoli
                          perform CURRENT-RECORD 
                          set errori to true  
                          move 78-ID-ef-codice to control-id
                       end-if
              end-read
           end-if.

      ***---
       STAMPA.
           set stampa     to true.
           call   "starticoli" using lk-blockpgm, 
                                     user-codi, 
                                     livello-abil.
           cancel "starticoli".

      ***---
       SUCCESSIVO.
           perform SALV-MOD.           
           if tutto-ok          
              perform CHECK-INSERIMENTO
              unlock articoli all records

              if mod = 1
                 move 0 to mod-k
                 set dataset1-articoli-lock to true
              else              
                 move 1 to mod-k     
                 initialize dataset1-articoli-lock-flag
              end-if

              perform CANCELLA-COLORE
              perform FORM1-NEXT

              if RecLocked
                 set RecLocked to false             
                 initialize dataset1-articoli-lock-flag
                 move 1 to mod-k
                 perform FORM1-NEXT
              end-if
           end-if.

      ***---
       TORNA-IN-VISUA.
           move 0 to mod.
           move 1 to mod-k.
           move 78-ID-ef-codice to CONTROL-ID.
           set NoMessage        to true.
           perform ABILITAZIONI.
           set StatusVisua      to true.
           perform STATUS-BAR-MSG.
           unlock articoli all records.
                        
           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.
           move 4 to ACCEPT-CONTROL.

           if KeyboardSaved
              set KeyboardReleased to true
              set environment "KEYSTROKE" to "DATA=44 44"
              set environment "KEYSTROKE" to "DATA=46 46"
           end-if.

           move 3 to BitmapNumDistinta.
           move 0 to e-distinta.

           move art-codice of articoli   to dis-articolo-finale.
           move 0                        to dis-codice.
           start distinteb key is >= k-articolo
              invalid 
                 continue
              not invalid
                 read distinteb next 
                    at end 
                       continue
                    not at end
                      if dis-articolo-finale = art-codice of articoli
                         move 1 to BitmapNumDistinta
                         move 1 to e-distinta
                      end-if
                  end-read
           end-start.
           modify pb-distinta, enabled = e-distinta,
                         bitmap-number = BitmapNumDistinta.

      ***---
       ULTIMO.
           perform SALV-MOD.
           if tutto-ok
              perform CHECK-INSERIMENTO
              unlock articoli all records 

              if mod = 1
                 move 0 to mod-k
                 set dataset1-articoli-lock to true
              else              
                 move 1 to mod-k     
                 initialize dataset1-articoli-lock-flag
              end-if

              perform CANCELLA-COLORE
              perform FORM1-LAST 

              if RecLocked
                 set RecLocked to false             
                 initialize dataset1-articoli-lock-flag
                 move 1 to mod-k
                 perform FORM1-LAST
              end-if
           end-if.

      ***---
       VALORIZZA-NUOVO.
           move high-value to art-codice of articoli.
           start articoli key <= art-codice of articoli
              invalid     
                 move 1 to art-codice of articoli
              not invalid
                 read articoli previous with no lock
                    at end   
                       move 1 to art-codice of articoli
                    not at end   
                       add  1 to art-codice of articoli
                 end-read
           end-start.
           move "00" to status-articoli.  

           initialize art-dati of articoli
                                   replacing numeric data by zeroes
                                    alphanumeric data by spaces.
                                    
           move CodiceIvaStd to art-codice-iva of articoli
           set art-confezionato of articoli to true.

           perform FORM1-FLD-TO-BUF.

           set nuovo to true.  

           move 3 to BitmapNumDistinta.
           move 0 to e-distinta.

           modify pb-distinta, enabled = e-distinta,
                         bitmap-number = BitmapNumDistinta.

           modify cbo-utf, reset-list = 1.
           perform RIEMPI-COMBO-NON-UTF.

           initialize lab-imballo-buf.

           perform FORM1-DISPLAY.

      ***---
       VALORIZZA-OLD.
           move art-rec of articoli   to old-art-rec.
           set vecchio                to true.

           if old-art-soggetto-imposte = spaces             
              set old-art-no-imposte to true
           end-if.

           if old-art-soggetto-cobat = spaces
              set old-art-no-cobat   to true
           end-if.
      *
           evaluate CONTROL-ID
           when 78-ID-ef-setmerc
           when 78-ID-ef-marca
           when 78-ID-ef-classe-1
           when 78-ID-ef-udm
           when 78-ID-ef-dogana
                move 1 to StatusHelp
           when other
                move 0 to StatusHelp
           end-evaluate.
           perform STATUS-HELP.

      ***---
       VALUTA-CAMBIO-VALORI-PROGMAG.
           move "000" to FlagsChanged.
           set sincro-progmag to false.

           |Fatto solo su vecchio perchè in caso di 
           |nuovo ovviamente non ho valori di confronto
           if vecchio
              add art-peso-utf     of articoli     
               to art-peso-non-utf of articoli
                               giving peso

              add old-art-peso-utf 
               to old-art-peso-non-utf giving old-peso

              if art-peso-utf of articoli     not = old-art-peso-utf  or
                 art-peso-non-utf of articoli not = old-art-peso-non-utf
                 set peso-changed to true
              end-if

              if art-imballo-standard of articoli not = 
                 old-art-imballo-standard
                 set imballo-changed to true
              end-if
           end-if.

           |Valuto lo stato del momento
           perform SCARICA-COMBO-STATO.
           if attivo or bloccato
              if attivo
                 perform CONTROLLA-PRESENZA-LISTINI-F
              end-if
              if tutto-ok
                 if art-disattivo of articoli |STO ATTIVANDO DA SOSPESO
                    perform CAMBIO-VALORI-PESO-IMBALLO
                 else
                    evaluate FlagsChanged
      *****              when "10"
      *****                   display message
      *****                   "Impossibile cambiare il valore del PESO."
      *****         x"0d0a""Occorre registrare un nuovo progressivo a zero."
      *****                             title titolo
      *****                              icon 2
      *****                   move 78-ID-ef-peso-utf to store-id
      *****                   set errori to true
                    when "11"
                         display message
                      "Impossibile cambiare i valori di IMBALLO e PESO."
               x"0d0a""Occorre registrare un nuovo progressivo a zero."
                                   title titolo
                                    icon 2
                         move 78-ID-ef-imballo to store-id
                         set errori to true
                    when "01"
                         display message
                         "Impossibile cambiare il valore dell'IMBALLO."
                x"0d0a""Occorre registrare un nuovo progressivo a zero."
                                   title titolo
                                    icon 2
                         move 78-ID-ef-imballo to store-id
                         set errori to true
                    end-evaluate
                 end-if
              end-if
           else
              perform CAMBIO-VALORI-PESO-IMBALLO
           end-if.

      ***---
       CAMBIO-VALORI-PESO-IMBALLO.
           evaluate FlagsChanged
           when "10"
                display message
                   "Confermi il cambio del PESO?"
                          title titolo
                           icon 2
                           type mb-yes-no
                         giving scelta
                if scelta = mb-no
                   move 78-ID-ef-peso-utf to store-id
                   set errori to true
                else
                   set sincro-progmag to true
                end-if
           when "11"
                display message
                   "Confermi il cambio di PESO/IMBALLO?"
                          title titolo
                           icon 2
                           type mb-yes-no
                         giving scelta

                if scelta = mb-no
                   move 78-ID-ef-imballo to store-id
                   set errori to true
                else
                   set sincro-progmag to true
                end-if
           when "01"
                display message
                   "Confermi il cambio dell'IMBALLO?"
                          title titolo
                           icon 2
                           type mb-yes-no
                         giving scelta
                if scelta = mb-no
                   move 78-ID-ef-imballo to store-id
                   set errori to true
                else
                   set sincro-progmag to true
                end-if
           
           end-evaluate.


      *******************************************************************
      *                  SEZIONE ENTRY_POINT & EVENTS                   *
      *******************************************************************
      ***---
       GARTICOLI-BEFORE-ACCEPT.
           move 1 to e-stampa.
           perform INIT.

           perform ABILITA-TOOLBAR.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG.

           move "tparamge" to nome-file.
           perform RELAZIONI-ARTICOLI.
                         
           accept data-oggi from century-date.

           move data-oggi to data-val-da
                             data-val-a

           move "0101"    to data-val-da(5:4).
           move "1231"    to data-val-a(5:4).

           move data-val-da  to como-data.
           perform DATE-TO-SCREEN.
           move como-data    to ef-dt-val-da-buf.
           modify ef-dt-val-da value ef-dt-val-da-buf

           move data-val-a  to como-data.
           perform DATE-TO-SCREEN.
           move como-data    to ef-dt-val-a-buf.
           modify ef-dt-val-a value ef-dt-val-a-buf.


      ***--
       GARTICOLI-AFTER-ACCEPT.
           evaluate key-status
           when 5 |modifica
                inquire tool-modifica, enabled in e-modifica
                if e-modifica = 1
                   if mod = zero move 1 to mod
                   else          move zero to mod
                   end-if
                   modify tool-modifica, value = mod
                   perform MODIFICA
                end-if
           when 10 |ordinamento alfabetico 
                if OrderBy = 0 move 1 to OrderBy
                else           move 0 to OrderBy
                end-if
                modify TOOL-ORD, value OrderBy
                perform ORDINAMENTO

           end-evaluate.

      ***---
       GARTICOLI-AFTER-BUF-TO-FLD.
           perform SCARICA-COMBO-STATO.
           move stato          to art-stato of articoli. 
                                       
           perform SCARICA-COMBO-UTF.  
           move gestione-utf        to art-gestione-utf of articoli.
                    
           perform SCARICA-COMBO-COBAT.
           move cobat to art-auto-moto-per-cobat of articoli.

           move ef-codice-buf to art-codice of articoli convert.

           move ef-dt-val-da-BUF to como-data.
           perform DATE-TO-FILE.
           move como-data to data-val-da.

           move ef-dt-val-a-BUF to como-data.
           perform DATE-TO-FILE.
           move como-data to data-val-a.


      ***---
       GARTICOLI-BEFORE-FLD-TO-BUF.
      * CLIENTI-PROGRESSIVI
           initialize prg-rec replacing alphanumeric data by spaces
                                             numeric data by zeroes.
           
           move art-codice of articoli to prg-cod-articolo.
           read progmag no lock invalid continue end-read.
           perform CALCOLA-COSTO-MP.
           add 0,005 to costo-mp giving costo-mp-2dec.

      ***---
       GARTICOLI-AFTER-FLD-TO-BUF.  
           display lab-moq ef-moq.
           if art-scorta of articoli = 9
              move 1 to v-reale
           else
              move 0 to v-reale
           end-if.
           display lab-reale ef-reale.
              
           move art-stato of articoli        to stato.
           perform CARICA-COMBO-STATO.
           evaluate true
           when bloccato  move 1 to mod-campi
                          move 1 to NumBitmapCodici
           when attivo    move 1 to mod-campi
                          move 1 to NumBitmapCodici
           when disattivo move 0 to mod-campi
                          move 3 to NumBitmapCodici
           end-evaluate.   
           
           perform DESCRIZIONE-ARTICOLO.
           
           move art-gestione-utf of articoli        to gestione-utf.
           perform CARICA-COMBO-UTF.
           
           move art-auto-moto-per-cobat of articoli        to cobat.
           perform CARICA-COMBO-COBAT.
                                  
           move art-codice of articoli    to  codice-ed.
           move codice-ed     to  ef-codice-buf.   
           call "C$JUSTIFY" using ef-codice-buf, "L".
           display ef-codice.          
           
      * ARTICOLI-SCORTE
           move "tscorte" to nome-file.
           perform RELAZIONI-ARTICOLI.   
           if sco-moq-si
              move 1 to v-moq
           else
              move 0 to v-moq
           end-if.
           
      * ARTICOLI-SETTORE MERCEOLOGICO
           move "tsetmerc" to nome-file.
           perform RELAZIONI-ARTICOLI.   
           
      * ARTICOLI-artcoli1
           move "articoli1" to nome-file.
           perform RELAZIONI-ARTICOLI.   
           if not trovato and art-codice of articoli1 not = 0
              move "blister" to nome-file
              perform RELAZIONI-ARTICOLI
           end-if.
           
      * CLIENTI-CODICE IVA
           move "tivaese" to nome-file.
           perform RELAZIONI-ARTICOLI.
           
      * ARTICOLI-MARCHE
           move "tmarche" to nome-file.
           perform RELAZIONI-ARTICOLI.
           
      * ARTICOLI-MAGAZZINO
           move "tmagaz" to nome-file.
           perform RELAZIONI-ARTICOLI.
           
      * ARTICOLI-CLASSE
           move "tcla1art" to nome-file.
           perform RELAZIONI-ARTICOLI.
           
      * ARTICOLI-DOGANA
           move "tnomen" to nome-file.
           perform RELAZIONI-ARTICOLI.  
           if trovato                       
              modify cbo-utf, reset-list = 1
              evaluate true
              when nom-si-utf               
                   perform RIEMPI-COMBO-UTF
                   perform CARICA-COMBO-UTF
              when nom-no-utf
                   perform RIEMPI-COMBO-NON-UTF
              end-evaluate
           end-if.
           
      * ARTICOLI-UNITA DI MISURA
           move "tudm" to nome-file.
           perform RELAZIONI-ARTICOLI.

      * ARTICOLI-IMBALLI
           move "timbalqta" to nome-file.
           perform RELAZIONI-ARTICOLI.

      * ARTICOLI-FORNITORI
           move "fornitori" to nome-file.
           perform RELAZIONI-ARTICOLI.
                                
      * ARTICOLI-DESTINIF
           move "destinif" to nome-file.
           perform RELAZIONI-ARTICOLI.

      * ARTICOLI-PRODENER
           move "prodener" to nome-file.
           perform RELAZIONI-ARTICOLI.

           perform VALORIZZA-OLD.

           perform ABILITAZIONI.

           move 3 to BitmapNumDistinta.
           move 0 to e-distinta.

           if vecchio
              move art-codice of articoli to dis-articolo-finale
              move 0          to dis-codice
              start distinteb key is >= k-articolo
                    invalid continue
                not invalid
                    read distinteb next 
                         at end continue
                     not at end
                         if dis-articolo-finale = art-codice of articoli
                            move 1 to BitmapNumDistinta
                            move 1 to e-distinta
                         end-if
                     end-read
              end-start
           end-if.
           modify pb-distinta, enabled = e-distinta,
                         bitmap-number = BitmapNumDistinta.

           move data-val-da  to como-data.
           perform DATE-TO-SCREEN.
           move como-data    to ef-dt-val-da-buf.

           move data-val-a  to como-data.
           perform DATE-TO-SCREEN.
           move como-data    to ef-dt-val-a-buf.


           perform CARICA-LISTINI.

      ***---
       PUSH-NOTE-PRESSED.
           move 0 to num-campo.
           move mod-campi to e-campo.
           perform APRI-NOTE.

      ***---
       PB-FOTO-PRESSED.
           inquire ef-foto, value in LinkImage.
           move mod to LinkAbil.
           call   "browser" using LinkBrowser.
           cancel "browser".
           move LinkImage to ef-foto-buf.
           display ef-foto.

      ***---
       PB-BRAND-PRESSED.
           inquire ef-brand, value in LinkImage.
           move mod to LinkAbil.
           call   "browser" using LinkBrowser.
           cancel "browser".
           move LinkImage to ef-brand-buf.
           display ef-brand.

      ***---
       PB-SCHEDA-PRESSED.
           move 1 to num-campo.
           move mod-campi to e-campo.
           inquire ef-scheda, value in path-note.
           perform APRI-NOTE.
           move opnsav-filename to ef-scheda-buf.
           display ef-scheda.

      ***---
       PB-TOSS-PRESSED.
           move 1 to num-campo.
           move mod-campi to e-campo.
           inquire ef-toss, value in path-note.
           perform APRI-NOTE.
           move opnsav-filename to ef-toss-buf.
           display ef-toss.

      ***---
       PB-DISTINTA-PRESSED.
           set tutto-ok    to true.
           move 0          to num-articoli.
           move 0          to dis-codice.
           move art-codice of articoli to dis-articolo-finale.

           start distinteb key is >= k-articolo
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read distinteb next at end exit perform end-read
                    if dis-articolo-finale not = art-codice of articoli
                       exit perform
                    end-if
                    add 1 to num-articoli
                    evaluate true
                    when num-articoli = 1
                         move dis-codice to save-codice
                    when num-articoli > 1
                         exit perform
                    end-evaluate
                 end-perform
           end-start.

      *****     if num-articoli > 1
      *****        Move Art-Codice To Dis-Articolo-Finale
      *****        Move "distinteb-Art" To Como-File
      *****        Call "zoom-Gt" Using Como-File, Dis-Rec
      *****                      Giving Stato-Zoom
      *****        Cancel "zoom-Gt"
      *****        If Stato-Zoom = 0
      *****           Move Dis-Codice To Save-Codice
      *****        Else
      *****           Set Errori To True
      *****        End-If
      *****     End-If.
      *****
           if tutto-ok
              move save-codice to dis-codice
              call   "vdistinta" using dis-codice
              cancel "vdistinta"
           end-if.

      ***---
       PB-CODICI-PRESSED.
           set link-accept to true.
           perform SCRIVI-PROGMAG.

      ***---
       PB-DETTAGLI-PRESSED.
           move "prg-artico-sons" to como-file.
           initialize prg-chiave.
           inquire ef-codice, value in prg-cod-articolo
           call "zoom-gt"   using como-file, prg-rec
                           giving stato-zoom
           end-call.
           cancel "zoom-gt".
           if stato-zoom = 0
              call   "vprogmag" using prg-chiave
              cancel "vprogmag"
           end-if.

      ***---
       CHK-IMPOSTE-PRESSED.
           inquire chk-imposte, value chk-imposte-buf.

           if chk-imposte-buf = 1
              move 0 to chk-cobat-buf, mod-cobat
              perform CANCELLA-COLORE
              display chk-cobat, ef-amperaggio, cbo-cobat
              move 1    to mod-imposte
           else
              move 0 to mod-imposte
              perform CANCELLA-COLORE
              display ef-perce-imposte
           end-if.

      ***---
       CHK-COBAT-PRESSED.
      *     move chk-cobat-buf to mod-cobat.
      *     display ef-amperaggio
      *             cbo-cobat.

           inquire chk-cobat, value chk-cobat-buf.

           if chk-cobat-buf = 1
              move ZERO to chk-imposte-buf, mod-imposte
              perform CANCELLA-COLORE
              display chk-imposte, ef-perce-imposte
              move 1    to mod-cobat
              display ef-amperaggio, cbo-cobat
           else
              move 0 to mod-cobat
              perform CANCELLA-COLORE
              display ef-amperaggio, cbo-cobat
           end-if.

      ***---
       PB-STAMPA-PRESSED.
           perform CANCELLA-COLORE.
           if nuovo
              display message "Occorre confermare l'articolo"
                        title titolo
                         icon 2
           else
              call "W$MOUSE" using set-mouse-shape, wait-pointer
              call   "st-art-det" using art-codice of articoli
              cancel "st-art-det"
              call "W$MOUSE" using set-mouse-shape, arrow-pointer
           end-if.
           move 760 to control-id.
           move 4   to accept-control.
           modify pb-stampa, bitmap-number = 2.

      ***---
       PB-LISTA-PRESSED.
           perform CANCELLA-COLORE.
           call "W$MOUSE" using set-mouse-shape, wait-pointer
           call   "starticoli-all" using user-codi
           cancel "starticoli-all"
           call "W$MOUSE" using set-mouse-shape, arrow-pointer.
           move 696 to control-id.
           move 4   to accept-control.
           modify pb-lista, bitmap-number = 2.

      ***---
       SELEZIONE-COMBO-UTF.
           perform SCARICA-COMBO-UTF.
           evaluate true
           when soggetto     move 1 to mod-peso-utf
                             move 0 to mod-peso-non-utf
                             move 0 to ef-peso-non-utf-buf
           when misto        move 1 to mod-peso-utf
                             move 1 to mod-peso-non-utf
           when non-soggetto move 0 to mod-peso-utf
                             move 1 to mod-peso-non-utf   
                             move 0 to ef-peso-utf-buf
           end-evaluate.

           display ef-peso-utf ef-peso-non-utf.

      ***---
       PB-OK-EAN-PRESSED.
           move 0 to art-codice of articoli.
           inquire ef-ean, value in ef-ean-buf.
           if ef-ean-buf not = 0
              call   "cerca-ean" using art-codice of articoli, 
                                       ef-ean-buf
              cancel "cerca-ean"
              if art-codice of articoli = 0
                 display message "Nessun articolo trovato"
                           title titolo
                            icon 2
                 modify pb-ok, bitmap-number = 1
                 move 78-ID-ef-ean to control-id
                 move 4            to accept-control
              else
                 move 27 to key-status
              end-if
           else
              move 4 to accept-control
           end-if.

      ***---
       PB-EAN-PRESSED.
           perform  CANCELLA-COLORE.
           perform FORM-EAN-OPEN-ROUTINE.
           if art-codice of articoli not = 0
              if old-art-chiave not = art-chiave of articoli
                 move art-chiave of articoli to  save-chiave
                 perform SALV-MOD
                 if tutto-ok
                    move save-chiave  to art-chiave of articoli
                    modify ef-codice, value = art-codice of articoli
                    move  art-codice of articoli to ef-codice-buf
                    set   ReadSecca  to true
                    perform CANCELLA-COLORE
                    perform CURRENT-RECORD
                    modify pb-ean, bitmap-number = 1
                    move 78-ID-ef-codice to control-id
                    move 4 to accept-control
                 end-if
              else
                 move 1761 to control-id
                 move 4    to accept-control
              end-if
           else
              move 1761 to control-id
              move 4    to accept-control
           end-if.    

      ***---
       PB-OK-FRN-PRESSED.
           move 0 to art-codice of articoli.
           inquire ef-frn, value in ef-frn-buf.
           if ef-frn-buf not = spaces
              call   "cerca-frn" using art-codice of articoli, 
                                       ef-frn-buf
              cancel "cerca-frn"
              if art-codice of articoli = 0
                 display message "Nessun articolo trovato"
                           title titolo
                            icon 2
                 modify pb-ok, bitmap-number = 1
                 move 78-ID-ef-frn to control-id
                 move 4            to accept-control
              else
                 move 27 to key-status
              end-if
           else
              move 4 to accept-control
           end-if.

      ***---
       PB-FRN-PRESSED.
           perform CANCELLA-COLORE.
           perform FORM-FRN-OPEN-ROUTINE.
           if art-codice of articoli not = 0
              if old-art-chiave not = art-chiave of articoli
                 move art-chiave of articoli to  save-chiave
                 perform SALV-MOD
                 if tutto-ok
                    move save-chiave  to art-chiave of articoli
                    modify ef-codice, value = art-codice of articoli
                    move  art-codice of articoli to ef-codice-buf
                    set   ReadSecca  to true
                    perform CANCELLA-COLORE
                    perform CURRENT-RECORD
                    modify pb-ean, bitmap-number = 1
                    move 78-ID-ef-codice to control-id
                    move 4 to accept-control
                 end-if
              else
                 move 1761 to control-id
                 move 4    to accept-control
              end-if
           else
              move 1761 to control-id
              move 4    to accept-control
           end-if.
           move spaces to ef-frn-buf.

      ***---
       RIEMPI-COMBO-STATO.
           modify cbo-stato,  item-to-add "Sospeso".
           modify cbo-stato,  item-to-add "Attivo".
           modify cbo-stato,  item-to-add "Bloccato".

      ***---
       CARICA-COMBO-STATO.
           evaluate true
           when disattivo     move "Sospeso"  to cbo-stato-buf
           when attivo        move "Attivo"   to cbo-stato-buf
           when bloccato      move "Bloccato" to cbo-stato-buf
           end-evaluate.
           modify cbo-stato,  value cbo-stato-buf.

      ***---
       SCARICA-COMBO-STATO.
           inquire  cbo-stato value cbo-stato-buf.
           evaluate cbo-stato-buf
           when "Sospeso"     set disattivo       to true
           when "Attivo"      set attivo          to true
           when "Bloccato"    set bloccato        to true
           end-evaluate.

      ***---
       CBO-STATO-SELCHANGE.
           perform NO-SOSPESO.

      ***---
       NO-SOSPESO.
           |Una volta tolto lo stato di sospeso
           |non può più essere impostato!!!
           if vecchio
              if old-art-bloccato or old-art-attivo
                 inquire cbo-stato value cbo-stato-buf
                 if cbo-stato-buf = "Sospeso"
                    move old-art-stato to stato
                    perform CARICA-COMBO-STATO
                 end-if
              end-if
           end-if.

      ***---
       SET-ASSOGGETTAMENTO-DEFAULT.
           if art-no-imposte of articoli
              move 0 to art-perce-imposte of articoli
              move 0 to art-perce-cou     of articoli
           else
              if art-perce-imposte of articoli = 0 or
                 art-perce-cou     of articoli = 0
                 display message "Percentuali imposte zero."
                 x"0d0a""Impostare quanto indicato dal codice doganale?"
                              title titolo
                               type mb-yes-no
                               icon 2
                             giving scelta
                            default mb-no
                 if scelta = mb-yes
                    if art-perce-cou of articoli = 0
                       move 100 to ef-perce-cou-buf
                       display ef-perce-cou
                    end-if
                    if art-perce-imposte of articoli = 0
                       evaluate true 
                       when nom-uguale
                       when nom-mag-u
                       when nom-min-u
                            move nom-perce 
                              to art-perce-imposte of articoli
                       when nom-maggiore
                            add 0,001 to nom-perce 
                                  giving art-perce-imposte of articoli
                       when nom-minore
                            subtract 0,001 from nom-perce 
                              giving art-perce-imposte of articoli
                       end-evaluate
                       move art-perce-imposte of articoli 
                         to  ef-perce-imposte-buf
                       display ef-perce-imposte
                    end-if
                 end-if
              end-if
           end-if.

      ***---
       CARICA-LISTINI.
           accept data-oggi from century-date
           modify gd-list mass-update 1
           modify gd-list reset-grid  1
           perform GD-LIST-CONTENT
           
           sort sort-art-list
              on descending key sart-prz-conf
              on ascending key  sart-forn
              with duplicates
              input  procedure is SCORRI-LISTINI
              output procedure is SCORRI-SORT.

           modify gd-list mass-update 0.
           move 2 to event-data-2.
           perform SPOSTAMENTO.

      ***---
       SCORRI-LISTINI.
           move low-value    to rlis-chiave-ricerca
           move art-codice of articoli   to rlis-articolo

           initialize rec-grid
           move 1   to riga
           move zero   to old-forn
           start rlistini key >= rlis-k-art 
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rlistini next at end exit perform end-read
                    if art-codice of articoli not = rlis-articolo
                       exit perform
                    end-if
                    set rec-ok  to false

      *              if rlis-fine-val >= data-oggi and 
      *                 rlis-ini-val <= data-oggi
      *                 set rec-ok  to true
      *              end-if

                    if data-val-da >= rlis-ini-val and 
                       data-val-da <= rlis-fine-val 
                       set rec-ok  to true
                    end-if

                    if data-val-a >= rlis-ini-val and 
                       data-val-a <= rlis-fine-val 
                       set rec-ok  to true
                    end-if

      *    controllo che sia compreso  nelle date che sto testando 
                    if data-val-da <= rlis-ini-val and 
                       data-val-a >= rlis-fine-val 
                       set rec-ok  to true
                    end-if

                    if rec-ok
                       initialize sart-descr-forn
                       move rlis-fornitore     to sart-forn
                                                  cli-codice
                       set cli-tipo-F to true
                       read clienti no lock
                            invalid continue
                        not invalid
                            move rlis-fornitore to desf-codice
                            move rlis-destino   to desf-prog
                            read destinif no lock
                                invalid 
                                inspect cli-ragsoc-1 replacing trailing 
                                                    spaces by low-value
                                inspect cli-ragsoc-2 replacing trailing 
                                                    spaces by low-value
                                inspect cli-localita replacing trailing 
                                                    spaces by low-value
                                string cli-ragsoc-1 delimited low-value
                                       cli-ragsoc-2 delimited low-value
                                       " - "        delimited size
                                       cli-localita delimited low-value
                                  into sart-descr-forn 
                                end-string
                            not invalid
                                inspect desf-ragsoc-1 replacing trailing 
                                                     spaces by low-value
                                inspect desf-ragsoc-2 replacing trailing 
                                                     spaces by low-value
                                inspect desf-localita replacing trailing 
                                                     spaces by low-value
                                string desf-ragsoc-1 delimited low-value
                                       desf-ragsoc-2 delimited low-value
                                       " - "         delimited size
                                       desf-localita delimited low-value
                                  into sart-descr-forn 
                                end-string
                            end-read
                       end-read
                       move 0                 to prg-peso-utf
                       move 0                 to prg-peso-non-utf
                       move rlis-ini-val      to sart-data-da
                       move rlis-fine-val     to sartl-data-a
                       move rlis-art-forn     to sart-art-forn

                       move rlis-codice       to tlis-codice
                       read tlistini no lock invalid continue end-read 
                       move tlis-fornitore    to desf-codice
                       move rlis-destino      to desf-prog
                       read destinif no lock invalid continue end-read

                       move tlis-trasp       to como-trasporto
                       |13012010
                       move art-peso-utf     of articoli 
                                             to prg-peso-utf
                       move art-peso-non-utf of articoli
                                             to prg-peso-non-utf
                       perform CALCOLA-PRZ-FINITO
                       add 0,0005             to prz-confronto
                       add 0,005              to prz-confronto

                       add 0,0005             to prz-reale
                       add 0,005              to prz-reale
           
                       move prz-reale         to sart-prz-fint
                       move prz-confronto     to sart-prz-conf 
                       |13012010

                       release sart-rec
                    end-if
                 end-perform
           end-start.

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
       SCORRI-SORT.
           perform until 1 = 2
              return sort-art-list
                 at end exit perform
              end-return
              perform METTI-IN-GRIGLIA-LIST
           end-perform.

      ***---
       METTI-IN-GRIGLIA-LIST.
           move sart-forn  to col-forn
           move sart-descr-forn to col-descr-forn
                                     
           move sart-data-da(7:2)        to como-data(1:2).
           move sart-data-da(5:2)        to como-data(3:2).
           move sart-data-da(1:4)        to como-data(5:4).
           move como-data                to col-data-da.
           move sartl-data-a(7:2)        to como-data(1:2).
           move sartl-data-a(5:2)        to como-data(3:2).
           move sartl-data-a(1:4)        to como-data(5:4).
           move como-data                to col-data-a.
           move sart-art-forn            to col-art-forn.
           
           move sart-prz-fint            to col-prz-fint
           move sart-prz-conf            to col-prz-conf 

           add 1 to riga.
           modify gd-list(riga), record-data rec-grid.
           accept como-data from century-date.
           if como-data >= sart-data-da and
              como-data <= sartl-data-a
              modify gd-list(riga), row-color 112
           else
              modify gd-list(riga), row-color 176
           end-if.


      ***---
       SPOSTAMENTO.
           inquire gd-list, last-row in tot-righe.
           if event-data-2 >= 2 and
              event-data-2 <= tot-righe
              move riga   to riga-old
              if riga not = event-data-2 
                 move event-data-2 to riga
              end-if
            
              perform CAMBIA-FONT-RIGA
           end-if.

      ***---
       CAMBIA-FONT-RIGA.
           modify gd-list(riga-old), row-font = small-font.

           modify gd-list(riga-old, 1),
                  bitmap        = elemento-bmp,
                  bitmap-number = 1,
                  bitmap-width  = 16.

           move event-data-2 to riga.
           modify gd-list(riga), row-font = font-evidenzia-griglia.

           modify gd-list, (riga, 1),
                  bitmap        = elemento-bmp,
                  bitmap-number = 2,
                  bitmap-width  = 16.

      ***---
       CONTROLLA-PESO.
           inquire ef-altezza,    value in como-altezza.
           inquire ef-larghezza,  value in como-larghezza.
           inquire ef-profondita, value in como-profondita.
           inquire ef-qta-EPAL,   value in como-qta-EPAL.
           inquire ef-qta-STD,    value in como-qta-STD.
           if como-qta-EPAL = 0
              move como-qta-STD to como-qta-EPAL
           end-if.

           inquire ef-peso-utf,     
                   value in art-peso-utf     of articoli.
           inquire ef-peso-non-utf, 
                   value in art-peso-non-utf of articoli.
           compute peso-inserito =  art-peso-utf     of articoli + 
                                    art-peso-non-utf of articoli.

           compute como-peso =
               ( ( como-altezza * como-larghezza * como-profondita ) / 
                   1000000 * 150 ) / como-qta-EPAL.
           if como-peso > peso-inserito
              move 9 to ef-scorta-buf
              display ef-scorta
              move peso-inserito to ef-reale-buf
              move 1 to v-reale
              display ef-reale
              if art-peso-utf of articoli > 0
                 move como-peso to ef-peso-utf-buf
                 display ef-peso-utf
              end-if
              if art-peso-non-utf of articoli > 0
                 move como-peso to ef-peso-non-utf-buf
                 display ef-peso-non-utf
              end-if
              move 0 to ef-moq-buf
              move 0 to v-moq
              display ef-moq
           end-if.

      ***---
       CONTROLLA-PRESENZA-LISTINI-F.
           move low-value to rlis-rec.
           move art-codice of articoli to rlis-articolo.
           start rlistini key >= rlis-k-art
                 invalid 
                 set errori to true
                 display message 
                  "Nessun listino fornitore relativo presente."
           x"0d0a""Impossibile attivare l'articolo"
                           title tit-err
                            icon 2
             not invalid
                 read rlistini next
                 if rlis-articolo not = art-codice of articoli
                    set errori to true
                    display message 
                  "Nessun listino fornitore relativo presente."
           x"0d0a""Impossibile attivare l'articolo"
                              title tit-err
                               icon 2
                 end-if
           end-start.

      ***---
       AGGIORNA-CATENA.
           perform FORM3-OPEN-ROUTINE.

      ***---
       LAST-EL-CATENA.
           move zero   to last-codice
           open input catart
           move art-codice of articoli   to cat-codice
           move low-value    to cat-princ
           start catart key not < cat-chiave
              invalid
                 move zero   to ultimo-elemento
              not invalid
                 read catart next no lock
                    at end
                       move zero   to ultimo-elemento
                    not at end
                       if art-codice of articoli not = cat-codice
                          move zero   to ultimo-elemento
                       else
                          if cat-princ not = zero
                             move cat-princ to cat-codice
                             move zero      to cat-princ
                             read catart
                                invalid
                                   move zero   to cat-num-el-catena
                             end-read
                          end-if
                          move cat-num-el-catena  to ultimo-elemento
                          if cat-num-el-catena not = zero
                             move cat-collegato(cat-num-el-catena)  
                                                  to last-codice
                          end-if
                       end-if
                 end-read
           end-start.
           close catart.

      ***---
       CAMBIO-CODICE-COLLEGATO. 
           move "articoli1" to nome-file.
           perform RELAZIONI-ARTICOLI.
           if not trovato
              inquire ef-coll, value in ef-coll-buf
              move ef-coll-buf   to bli-codice
              move "blister" to nome-file
              perform RELAZIONI-ARTICOLI
              if not trovato
                 set errori to true
                 move 78-ID-ef-coll to control-id
                 display message "Articolo collegato NON valido"
                           title tit-err
                            icon 2
              end-if               
           end-if.
           if tutto-ok
              inquire ef-codice value art-codice of articoli
              if art-codice of articoli1 <= art-codice of articoli and
                 art-codice of articoli1 not = 0
                 if sco-permetti-sost-no
                    set errori to true
                    move 78-ID-ef-coll to control-id
                    display message "Articolo collegato NON valido"
                              title tit-err
                               icon MB-WARNING-ICON
                 end-if
              end-if
           end-if.

           if tutto-ok and art-codice of articoli1 not = zero
      *    Luciano 02/09/2010
      *             move art-codice of articoli1 
      *                                   to art-collegato of articoli1
      *                                      art-collegato of articoli
      *             start ARTICOLI1 key not < art-collegato of articoli1
      *                invalid
      *                   continue
      *                not invalid
      *                   perform until 1 = 2
      *                      read articoli1 next no lock
      *                         at end
      *                            exit perform
      *                      end-read
      *                      if art-collegato of articoli1 not = 
      *                         art-collegato of articoli
      *                         exit perform
      *                      end-if
      *                      if art-codice of articoli not =
      *                         art-codice of articoli1
      *                         set errori to true
      *                         move 78-ID-ef-coll to control-id
      *                         display message 
      *                    "Articolo "art-collegato of articoli
      *                    " già collegato all'articolo "
      *                     art-codice of articoli1
      *                                 title tit-err
      *                               icon MB-WARNING-ICON
      *                         exit perform
      *                      end-if
      *                   end-perform
      *             end-start
              move art-codice of articoli  to chk-ca-art
              move art-codice of articoli1 to chk-ca-collegato
              call "check-catart" using check-catart-linkage
              cancel "check-catart"
              if chk-ca-errore
                 move 78-ID-ef-coll to control-id
                 set errori to true
              end-if
      *    Luciano 02/09/2010 fine
           end-if.                      

      ***---
       IMPOSTA-SCORTA.
           |10-04-2015: non deve più farlo, lo farà in notturna.
      *****     |19-09-2013: se questo codice ha un collegato la scorta 
      *****     |dev'essere 2 se giacenza > 0 altrimenti scorta 0
      *****     initialize prg-chiave replacing numeric data by zeroes
      *****                                alphanumeric data by spaces.
      *****     move art-codice of articoli to prg-cod-articolo
      *****     read progmag no lock
      *****          invalid move 0 to ef-scorta-buf old-art-scorta
      *****      not invalid
      *****          if prg-giacenza <= 0
      *****             move 0 to ef-scorta-buf old-art-scorta
      *****          else
      *****             move 2 to ef-scorta-buf old-art-scorta
      *****          end-if                    
      *****     end-read.
      *****     display ef-scorta.
           |||

