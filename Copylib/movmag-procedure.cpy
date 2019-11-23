      ***---
       AFTER-BUF-TO-FLD.
           perform SCARICA-COMBO-STATO.
           move stato           to tmo-stato.
      *
           move ef-data-buf     to como-data.
           perform DATE-TO-FILE.
           move como-data       to  tmo-data-movim.

           move ef-data-doc-buf to como-data.
           perform DATE-TO-FILE.
           move como-data       to tmo-data-doc.

           move ef-data-via-buf to como-data.
           perform DATE-TO-FILE.
           move como-data       to tmo-dt-via.

      *
           move ef-clifor-buf   to tmo-cod-clifor convert.
           move ef-destino-buf  to tmo-destino    convert.

           if v-destino = 0 move 0 to tmo-destino end-if.

           accept como-ora from time.

           move SaveCliFor to tmo-tipo.

      ***---
       AFTER-PROGRAM.
           SET LK-BL-CANCELLAZIONE TO TRUE.
           MOVE COMO-PROG-ID       TO LK-BL-PROG-ID.
           CALL "BLOCKPGM"  USING LK-BLOCKPGM.
      *     call "W$BITMAP" using wbitmap-destroy, conferma-bmp

           if HoSalvato
              call   "tprev-elab" using user-codi
              cancel "tprev-elab"
           end-if.

      ***---
       AFTER-END-ACCEPT.
           if NonCambiareTab
              set NonCambiareTab to false
              move 1 to screen1-ta-1-tab-value event-data-1
              perform SCREEN1-TA-1-TABCHANGE
              move store-id to control-id
              move 4 to accept-control
              set ControllaCampi to true
           end-if
      *
           if ArticoloSetFocus
              move 78-ID-ef-art    to control-id
              move 4               to accept-control
              set ArticoloSetFocus to false  
      *        set ControllaCampi   to true
              accept omitted before time zero
           end-if.

      ***---
       BEFORE-PROGRAM.
           move LK-BL-PROG-ID    TO COMO-PROG-ID.
           set MovimentoConsolidato to false.
           call "C$CALLEDBY" using pgm-chiamante.
           
      ***---
       CALCOLA-IMPOSTE.             
           if SaveCli
              move esercizio to con-anno
              open input tcontat
              read tcontat no lock
              close tcontat
               
              evaluate tcl-serie-bolle
              when 1 move con-ult-stampa-bolle-gdo 
                       to imp-data
              when 2 move con-ult-stampa-bolle-mv  
                       to imp-data
              when 3 move con-ult-stampa-bolle-at  
                       to imp-data
              end-evaluate
           else
              accept imp-data from century-date
           end-if
           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move 0 to ef-cons-buf col-cons.
           set lab-imp-coubat to true.
           if des-prog = 0
              move cli-nazione to naz-codice
           else
              move des-nazione to naz-codice
           end-if.
           read tnazioni no lock.

           if naz-imp-esenti-si
              perform CALCOLO-IMPOSTE-ESTERO
           else
              evaluate true
              when ttipocli-standard perform CALCOLO-IMPOSTE-STANDARD
              when ttipocli-gdo      perform CALCOLO-IMPOSTE-GDO
              end-evaluate
           end-if.

      ***---
       CALCOLO-IMPOSTE-ESTERO.
           move 0 to rmo-imp-cons rmo-coubat.


      ***---
       CALCOLO-IMPOSTE-STANDARD.
           move 0 to col-coubat ef-coubat-buf imposta-cou.
           if art-si-imposte
              if mar-si-imposta-consumo
                 evaluate true
                 when art-misto
                 when art-si-utf
                      compute como-imposta =
                    (( prg-peso-utf * imp-imposta-consumo ) 
                                    * art-perce-imposte   ) / 100
                 when art-no-utf
                      compute como-imposta =
                    (( prg-peso-non-utf * imp-imposta-consumo ) 
                                        * art-perce-imposte) / 100
                 end-evaluate
                 add 0,005              to como-imposta
                 move como-imposta      to rmo-imp-cons
                 move rmo-imp-cons      to ef-cons-buf col-cons
              end-if
      
              if mar-si-cou
                 evaluate true
                 when art-misto
                 when art-si-utf
                      compute como-imposta =
                    (( prg-peso-utf * imp-cou ) 
                                    * art-perce-cou   ) / 100
                 when art-no-utf
                      compute como-imposta =
                    (( prg-peso-non-utf * imp-cou )
                                        * art-perce-cou   ) / 100
                 end-evaluate
                 add 0,005              to como-imposta
                 move como-imposta      to rmo-coubat imposta-cou
                 move rmo-coubat        to ef-coubat-buf col-coubat
                 set lab-imp-cou to true
              else
                 move 0 to imposta-cou
              end-if
      
           end-if.
      *     
           if art-si-cobat
              set lab-imp-cobat to true
              perform CALCOLA-COBAT
           end-if.

      ***---
       CALCOLO-IMPOSTE-GDO.
           move 0 to col-coubat ef-coubat-buf imposta-cou.
           if art-si-imposte
              evaluate true
              when art-misto
              when art-si-utf
                   compute como-imposta =
                  (( prg-peso-utf * imp-imposta-consumo ) 
                                  * art-perce-imposte   ) / 100
              when art-no-utf
                   compute como-imposta =
                 (( prg-peso-non-utf * imp-imposta-consumo ) 
                                     * art-perce-imposte) / 100
              end-evaluate
              add 0,005              to como-imposta
              move como-imposta      to rmo-imp-cons
              move rmo-imp-cons      to ef-cons-buf col-cons
           end-if.
      
           move 0 to imposta-cou.
           evaluate true
           when art-misto
           when art-si-utf
                compute como-imposta = 
                     (( prg-peso-utf * imp-cou ) 
                                     * art-perce-cou   ) / 100
           when art-no-utf
                compute como-imposta =
                 (( prg-peso-non-utf * imp-cou )
                                     * art-perce-cou   ) / 100
           end-evaluate.
           add 0,005              to como-imposta.
           move como-imposta      to rmo-coubat imposta-cou.
           move rmo-coubat        to ef-coubat-buf col-coubat.

           set lab-imp-cou to true.
      *     
           if art-si-cobat
              set lab-imp-cobat to true
              perform CALCOLA-COBAT
           end-if.

      ***---
       CALCOLA-COBAT.
           if ttipocli-gdo or mar-si-cobat
              evaluate true
              when art-auto-cobat
                   evaluate true
                   when art-amperaggio >= imp-cb-auto-sca-1-da and
                        art-amperaggio <= imp-cb-auto-sca-1-a
                        move imp-cb-auto-sca-1-euro 
                          to Imposta-Cobat
                   when art-amperaggio >= imp-cb-auto-sca-2-da and
                        art-amperaggio <= imp-cb-auto-sca-2-a
                        move imp-cb-auto-sca-2-euro 
                          to Imposta-Cobat
                   when art-amperaggio >= imp-cb-auto-sca-3-da and
                        art-amperaggio <= imp-cb-auto-sca-3-a
                        move imp-cb-auto-sca-3-euro 
                          to Imposta-Cobat
                   when art-amperaggio >= imp-cb-auto-sca-4-da and
                        art-amperaggio <= imp-cb-auto-sca-4-a
                        move imp-cb-auto-sca-4-euro 
                          to Imposta-Cobat
                   when art-amperaggio >= imp-cb-auto-sca-5-da and
                        art-amperaggio <= imp-cb-auto-sca-5-a
                        move imp-cb-auto-sca-5-euro 
                          to Imposta-Cobat
                   end-evaluate
         
              when art-moto-cobat
                   evaluate true
                   when art-amperaggio >= imp-cb-scooter-sca-1-da 
                    and art-amperaggio <= imp-cb-scooter-sca-1-a
                        move imp-cb-scooter-sca-1-euro 
                          to Imposta-Cobat
                   when art-amperaggio >= imp-cb-scooter-sca-2-da 
                    and art-amperaggio <= imp-cb-scooter-sca-2-a
                        move imp-cb-scooter-sca-2-euro 
                          to Imposta-Cobat
                   when art-amperaggio >= imp-cb-scooter-sca-3-da 
                    and art-amperaggio <= imp-cb-scooter-sca-3-a
                        move imp-cb-scooter-sca-3-euro 
                          to Imposta-Cobat
                   end-evaluate
              end-evaluate
           
              add 0,005              to imposta-cobat
              add  imposta-cou       to imposta-cobat
              move imposta-cobat     to ef-coubat-buf col-coubat
              if imposta-cou = 0
                 set lab-imp-cobat  to true
              else
                 set lab-imp-coubat to true
              end-if
           end-if.

      ***---
       CALCOLA-IMPONIBILE-MERCE.
           move ef-listino-buf  to rmo-listino.
           move ef-sconto-buf   to rmo-sconto.
           move ef-listino-buf  to rmo-listino.
           move ef-cons-buf     to rmo-imp-cons.
           move ef-coubat-buf   to rmo-coubat.

           compute numero = rmo-listino - 
                        ( ( rmo-listino * rmo-sconto ) / 100 )

           if ttipocli-gdo
              compute numero = numero - rmo-imp-cons - rmo-coubat
           end-if.

           move numero to rmo-netto col-netto ef-imp-merce-buf.

      ***---
       CALCOLA-TOTALE.
           inquire ef-listino, value in rmo-listino.
           inquire ef-cons,    value in rmo-imp-cons.
           inquire ef-coubat,  value in rmo-coubat.
           inquire ef-qta,     value in rmo-qta.
           if rmo-qta = 0
              move 1 to como-qta
           else
              move rmo-qta to como-qta
           end-if.
           inquire ef-sconto,  value in ef-sconto-buf.
           perform CALCOLA-IMPONIBILE-MERCE.
           compute numero = 
           ( rmo-netto + rmo-imp-cons + rmo-coubat ) * como-qta.
           move numero to lab-tot-buf.
           display lab-tot ef-imp-merce.

      ***---
       CALCOLA-UTF-TOTALE.
           move 0 to numero.
           perform varying riga from 2 by 1 until riga > tot-righe
              perform INQUIRE-HIDDEN
      *****        inquire form1-gd-1(riga, 78-NumCol + 6),
      *****                hidden-data in hid-utf
              inquire form1-gd-1(riga, 4), cell-data in rmo-qta
              compute numero = numero + ( hid-utf * rmo-qta)
           end-perform.

           move numero to link-peso-tot.

      ***---
       CANCELLA-RIGA.
           if mod-campi = 0 exit paragraph end-if.
           inquire form1-gd-1, last-row in tot-righe, cursor-y in riga.
           if tot-righe > 1
              if riga <= tot-righe or riga > 2
                 display message box "Cancellare la riga selezionta?"
                           title titolo
                            type mb-yes-no
                          giving scelta
                 if scelta = mb-yes
                    set RigaCambiata to true
                    modify  form1-gd-1, record-to-delete = riga
                    inquire form1-gd-1, last-row in tot-righe
                    if tot-righe = 1
                       perform CANCELLA-COLORE
                       perform PULISCI-CAMPI
                       set NewRow to true
                    else
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
              else
                 display message box "Occorre selezionare una riga"
                         title = titolo
              end-if
           end-if.

      ***---
       CERCA.
           evaluate control-id

           when 78-ID-ef-causale
                if rotta or bnota
                   continue
                else
                   evaluate true
                   when evasione
                        move "tcaumag-forn"       to como-file
                   when movimento
                        move "tcaumag-si-mov"     to como-file
                   end-evaluate
                   inquire ef-causale, value in tca-codice
                   call "zoom-gt"   using como-file, tca-rec
                                   giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      move tca-codice      to ef-causale-buf
                      move tca-descrizione to lab-causale-buf
                      display ef-causale lab-causale
                   end-if
                end-if
      
           when 78-ID-ef-clifor
                if rotta or bnota
                   continue
                else
                   move SaveCliFor    to cli-tipo-CF
                   if cli-tipo-C
                      move "clienti-alfa-all"  to como-file         
                   else
                      move "clienti-alfa-CF"   to como-file
                   end-if
                   inquire ef-clifor, value in cli-codice
                   call "zoom-gt"  using como-file, cli-rec
                                   giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      move cli-codice     to ef-clifor-buf
                      inspect ef-clifor-buf replacing 
                                            leading x"30" by x"20"
                      call "C$JUSTIFY" using ef-clifor-buf, "L"
                      move cli-ragsoc-1   to lab-cli-buf
                      display ef-clifor lab-cli
                   end-if        
                end-if
      
           when 78-ID-ef-destino
                if rotta or bnota
                   continue
                else
                   if v-destino = 1
                      inquire ef-clifor   value in des-codice
                      move "clienti-des"  to como-file         
                      inquire ef-destino, value in des-prog
                      call "zoom-gt"  using como-file, des-rec
                                      giving stato-zoom
                      end-call
                      cancel "zoom-gt"
                      if stato-zoom = 0
                         move des-prog     to ef-destino-buf
                         inspect ef-destino-buf replacing 
                                               leading x"30" by x"20"
                         call "C$JUSTIFY" using ef-destino-buf, "L"
                         move des-ragsoc-1   to lab-destino-buf
                         display ef-destino lab-destino
                      end-if
                   end-if
                end-if
      
           when 78-ID-ef-vettore
                move "tvettori"     to como-file         
                inquire ef-vettore, value in vet-codice
                call "zoom-gt"  using como-file, vet-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move vet-codice     to ef-vettore-buf
                   inspect ef-vettore-buf replacing 
                                         leading x"30" by x"20"
                   call "C$JUSTIFY" using ef-vettore-buf, "L"
                   move vet-descrizione to lab-vettore-buf
                   display ef-vettore lab-vettore
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
                   move tblpa-codice2 to ef-pag-buf
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
      
           when 78-ID-ef-mag
                if rotta or bnota
                   continue
                else
                   move "tmagaz"    to como-file         
                   inquire ef-mag,  value in mag-codice
                   call "zoom-gt"   using como-file, mag-rec
                                   giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      move mag-codice      to ef-mag-buf
                      move mag-descrizione to lab-mag-buf
                      display ef-mag lab-mag
                   end-if
                end-if

           when 78-ID-ef-art
                if movimento or rotta or bnota
                   perform ZOOM-SU-PROGMAG
                   move 4 to ACCEPT-CONTROL
                end-if

           end-evaluate.  

      ***---
       CHANGE-TAB.
           set NonCambiareTab   to false.
           set ArticoloSetFocus to false.
           set ControllaCampi   to true.
                       
           evaluate event-data-1
           when 1                  
                set ControllaCampi   to true
                if pgm-chiamante not = "selmovcons"
                   if MovimentoConsolidato
                      move 1 to v-cons
                   end-if
                else
                   move 0 to v-cons
                end-if
           when 2
                perform CHECK-PAGE-1
                if tutto-ok
                   move 0 to v-cons
                   perform CANCELLA-COLORE
                   set ArticoloSetFocus to true
                else    
                   set ControllaCampi to false
                   move store-id to control-id
                   move 4        to accept-control
                   set NonCambiareTab to true
                end-if
           end-evaluate.
           display Screen1-Ta-1.

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
                if nome-pgm = "gmovcvar" move 3 to NumBitmapCodici
                else                     move 1 to NumBitmapCodici
                end-if
                display pb-codici
                perform DISPLAY-SCREEN
           end-evaluate.

      ***---
       CHECK-PAGE-1.
           set tutto-ok to true.
           perform  varying control-id from 78-ID-ef-data by 1
                      until control-id > 78-ID-ef-mag
              perform CONTROLLO
              if errori 
                 accept omitted before time zero
                 exit perform 
              end-if
           end-perform.

      ***---
       CHECK-RIGHE.
           set tutto-ok to true.
           inquire Form1-Gd-1, last-row in tot-righe.
           move 0 to idx.
           initialize tabella-righe.
           perform varying riga from 2 by 1
                     until riga > tot-righe
              inquire form1-gd-1(riga, 2), cell-data in col-art
              move col-art    to art-codice
              perform INQUIRE-HIDDEN
              perform FIND-PROGMAG-CHANGE-CAUSALE
              if idx = 20 exit perform end-if
           end-perform.

           if idx > 0 set errori to true end-if.

      ***---
       CLEAR-SCREEN.
           initialize tmo-rec  
                      rmo-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
                        
           perform FORM1-FLD-TO-BUF. 
      
           perform INIT-OLD-REC.

           move spaces to lab-causale-buf 
                          lab-cli-buf 
                          lab-pag-buf 
                          lab-mag-buf
                          lab-destino-buf
                          lab-vettore-buf
                          ef-mag-buf.

           perform PULISCI-CAMPI.
      
           perform DISPLAY-SCREEN.

      ***---
       COLORE.
           modify form1-gd-1, start-y = riga, y = riga,
                              start-x = 2,    x = 78-NumCol,
                              region-color = 144.

      ***---
       CONTROLLA-TOTALE.
           move 0 to Sum.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 12) cell-data in SavePrezzo
              add SavePrezzo to Sum
           end-perform.
           if Sum = 0
              if TotaleNoZero set errori to true end-if
           else
              if TotaleSiZero set errori to true end-if
           end-if.

      ***---
       CONTROLLO.
           set tutto-ok to true. 
           set ChangeTabToPageTwo to false. 
           if not ControllaCampi
              set ControllaCampi to true
              exit paragraph
           end-if.
           if mod = 0 exit paragraph end-if.
      **
      * Elenco degli Id sui quali fare il CONTROLLO nel programma gclienti
      * paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           when 78-ID-ef-data
                inquire ef-data, value in como-data
                if como-data = 0
LUBEXX*****                   move data-oggi to como-data
LUBEXX             move data-ult-movim to como-data
                   perform DATE-TO-SCREEN
                   move como-data to ef-data-buf
                else
                   move ef-data-buf to como-data
                   perform DATE-FORMAT
                   move como-data to ef-data-buf
                end-if
                display ef-data
                move ef-data-buf to como-data
                if como-data(5:4) not = esercizio
                   display message "L'anno inserito non "
                                   "corrisponde a quello in uso"
                             title tit-err
                              icon 2
                   set errori to true
                   move 78-ID-ef-data to control-id
                else
                   perform DATE-TO-FILE
                   if pgm-chiamante not = "selmovcons"
                      if como-data <= tge-data-consolid-progmag
                         move tge-data-consolid-progmag to como-data
                         perform DATE-TO-SCREEN
                         display message box 
                             "Data inferiore a quella di consolidamento"
                      x"0d0a""Data di consolidamento: "
                              como-data(1:2)
                              "/"
                              como-data(3:2)
                              "/"
                              como-data(5:4)
                              title tit-err
                              icon  2
                         set errori to true
                         move 78-ID-ef-data to control-id
                      end-if
                   else
                      if como-data > tge-data-consolid-progmag
                         move tge-data-consolid-progmag to como-data
                         perform DATE-TO-SCREEN
                         display message box 
                             "Data magiore a quella di consolidamento"
                      x"0d0a""Data di consolidamento: "
                              como-data(1:2)
                              "/"
                              como-data(3:2)
                              "/"
                              como-data(5:4)
                              title tit-err
                              icon  2
                         set errori to true
                         move 78-ID-ef-data to control-id
                      end-if
                   end-if
                end-if
                move ef-data-buf to lab-data-buf
                display lab-data
                move ef-data-buf to como-data
                perform DATE-TO-FILE
                move como-data to tmo-data-movim
      *
           when 78-ID-ef-causale
                inquire ef-causale value in tca-codice
                move spaces to lab-causale-buf
                if tca-codice not = spaces
                   read tcaumag
                        invalid
                        set errori to true
                        display message "Codice causale NON valido"
                                title = tit-err
                                icon mb-warning-icon
                        move 78-ID-ef-causale     to control-id
                    not invalid   
                        if tca-no-movim
                           display message 
                           "Codice causale NON valido in quanto"
                           " non movimenta il magazzino"
                                   title = tit-err
                                   icon  2
                           set errori to true
                        else
                           if evasione
                              if tca-cod-magaz not = EvaMag
                                 display message 
                                 "Causale NON valida."
                          x"0d0a""Magazzino evasione " EvaMag
                          x"0d0a""Magazzino causale  " tca-cod-magaz
                                           title = tit-err
                                            icon  2
                                 set errori to true
                              else
                                 if tca-cliente
                                    display message 
                                    "Causale NON valida"
                             x"0d0a""Occorre di tipo fornitore"
                                              title = tit-err
                                               icon  2
                                    set errori to true
                                 end-if
                              end-if 
                           end-if
                           if tutto-ok   
                              if tca-tipo not = SaveCliFor
                                 set CliForChanged to true
                              else
                                 set CliForChanged to false
                              end-if
                              move tca-descrizione   to lab-causale-buf

                              if tca-no-movim-giac-periodo
                                 set VariazioneDiValore   to true
                                 modify ef-qta, read-only
                              else
                                 set VariazioneDiQuantita to true
                                 modify ef-qta, not read-only
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

                              if VariazioneInUso = spaces
                                move TipoDiVariazione to VariazioneInUso
                              else
                               if VariazioneInUso not = TipoDiVariazione
                                    display message 
                                    "La causale inserita applica una "
                                 "variazione non congruente con quella "
                                    "già utilizzata."
                             x"0d0a""Impossibile procedere "
                                    "col cambio di causale "
                                            title tit-err
                                             icon 2
                                    set errori to true
                                 end-if
                              end-if
                           end-if

                        end-if
                   end-read
                   if tutto-ok
                      inquire ef-pag, value in ef-pag-buf
                      if ef-pag-buf            = spaces and 
                         tca-cod-pagamento not = spaces
                         move "PA"              to tblpa-codice1
                         move tca-cod-pagamento to tblpa-codice2
                         move spaces     to tblpa-descrizione1
                                            tblpa-descrizione2
                         read tcodpag invalid continue end-read
                         initialize lab-pag-buf
                         inspect tblpa-descrizione1 replacing trailing 
                                                    spaces by low-value
                         string  tblpa-descrizione1 delimited low-value
                                 " "                delimited size
                                 tblpa-descrizione2 delimited size
                                 into lab-pag-buf
                         end-string 
                         move tca-cod-pagamento to ef-pag-buf
                         display ef-pag lab-pag
                      end-if
                      move tca-cod-magaz to ef-mag-buf mag-codice
                                            lab-codmag-buf
                      read tmagaz no lock invalid continue end-read
                      move mag-descrizione to lab-mag-buf lab-desmag-buf
                      display lab-mag ef-mag lab-codmag lab-desmag

                      inquire ef-clifor, value in cli-codice
                      if CliForChanged
                         perform RESETTA-CLIFOR
      *****                   move SaveCliFor to cli-tipo-CF
      *****                   read clienti
      *****                        invalid
      *****                           if SaveCli
      *****                              display message box
      *****                              "Codice cliente NON valido"
      *****                                      title = tit-err
      *****                                      icon mb-warning-icon
      *****                           else
      *****                              display message box 
      *****                              "Codice fornitore NON valido"
      *****                                      title = tit-err
      *****                                      icon mb-warning-icon
      *****                           end-if 
      *****                           set  errori to true
      *****                           move spaces to lab-cli-buf
      *****                           move 78-ID-ef-clifor to control-id
      *****                           move 4               to accept-control
      *****                    not invalid
      *****                        move cli-ragsoc-1    to lab-cli-buf
      *****                   end-read
                      end-if
                      if MagazzinoInUso not = spaces
                         if ef-mag-buf not = MagazzinoInUso
                            perform MAGAZZINO-CHANGED
                         end-if
                      end-if
                   end-if
                else
                   set errori to true
                   display message "Inserimento codice causale mancante"
                           title = tit-err
                           icon mb-warning-icon
                   move 78-ID-ef-causale to control-id
                end-if
                display lab-causale
                display lab-clifor
                move ef-causale-buf  to lab-cau-buf
                move lab-causale-buf to lab-des-cau-buf
                display lab-cau lab-des-cau lab-cli
                if tutto-ok 
                   if SaveCli
                      if v-destino = 0
                         move spaces to ef-destino-buf
                      end-if
                      move 1 to v-destino
                   else                   
                      move 0 to v-destino
                   end-if
                end-if
                display lab-prg-des ef-destino lab-destino
      *               
           when 78-ID-ef-clifor
                inquire ef-clifor, value in ef-clifor-buf
                move spaces to lab-cli-buf
                if ef-clifor-buf not = spaces
                   move ef-clifor-buf to cli-codice convert
                   inspect ef-clifor-buf replacing trailing spaces 
                                                         by low-values
                   initialize CountChar
                   inspect ef-clifor-buf tallying CountChar 
                           for characters before low-value
                   inspect ef-clifor-buf replacing trailing low-values 
                                                         by spaces

                   | SE E' NUMERICO LEGGO IL RECORD NORMALMENTE
                   if ef-clifor-buf(1:CountChar) is numeric
                      perform SELEZIONA-NUMERICO
                   else                     
                      | HO DIGITATO UNA SIGLA ALFANUMERICA QUINDI 
                      | APRO LO ZOOM (CASE SENSITIVE)
                      perform SELEZIONA-ALFA
                   end-if

                   if tutto-ok
                      set ttipocli-standard to true
                      if cli-tipo-C
                         move cli-tipo to tcl-codice
                         read ttipocli no lock
                              invalid continue
                         end-read
                      end-if
                      inquire ef-pag, value in tblpa-codice2
                      if tblpa-codice2 = spaces
                         move cli-pag to ef-pag-buf
                         move "PA"    to tblpa-codice1
                         move cli-pag to tblpa-codice2
                         move spaces  to tblpa-descrizione1 
                                         tblpa-descrizione2
                         read tcodpag invalid continue end-read
                         initialize lab-pag-buf
                         inspect tblpa-descrizione1 replacing trailing 
                                                    spaces by low-value
                         string  tblpa-descrizione1 delimited low-value
                                 " "                delimited size
                                 tblpa-descrizione2 delimited size
                                 into lab-pag-buf
                         end-string
                      end-if
                      inquire ef-vettore, value in vet-codice
                      if vet-codice = 0
                         move cli-vettore to vet-codice codice-ed
                         read tvettori no lock
                              invalid  
                              move spaces          to lab-vettore-buf
                          not invalid  
                              move vet-descrizione to lab-vettore-buf
                         end-read
                         move codice-ed to ef-vettore-buf
                      end-if
                   end-if
                else   
                   set errori to true        
                   if SaveCli
                      display message box
                              "Inserimento codice cliente mancante"
                              title = tit-err
                              icon mb-warning-icon
                   else            
                      display message box 
                              "Inserimento codice fornitore mancante"
                              title = tit-err
                              icon mb-warning-icon
                   end-if 
                   move 78-ID-ef-clifor to control-id
                end-if
                display lab-cli
                display lab-pag
                display ef-pag
                display ef-vettore
                display lab-vettore
      *               
           when 78-ID-ef-destino
                if v-destino = 1
                   inquire ef-clifor,  value in ef-clifor-buf
                   move ef-clifor-buf to des-codice convert
                   inquire ef-destino, value in ef-destino-buf
                   move spaces to lab-destino-buf
                   if ef-destino-buf not = spaces
                      move ef-destino-buf to des-prog convert
                      inspect ef-destino-buf replacing trailing spaces 
                                                         by low-values
                      initialize CountChar
                      inspect ef-destino-buf tallying CountChar 
                              for characters before low-value
                      inspect ef-destino-buf replacing trailing 
                                             low-values by spaces

                      | SE E' NUMERICO LEGGO IL RECORD NORMALMENTE
                      if ef-destino-buf(1:CountChar) is numeric
                         perform SELEZIONA-NUMERICO-DESTINO
                      else                     
                         | HO DIGITATO UNA SIGLA ALFANUMERICA QUINDI 
                         | APRO LO ZOOM (CASE SENSITIVE)
                         perform SELEZIONA-ALFA-DESTINO
                      end-if
                   else
                      if cli-codice not = old-cliente
                         perform TROVA-DESTINO
                         if trovato
                            display message 
                            "Esiste uno o più destini per"
                            " il cliente specificato."
                            x"0d0a""Procedere comunque con "
                                   "progressivo non valorizzato?"
                                      title titolo
                                       type mb-yes-no
                                     giving scelta
                            if scelta = mb-no
                               set  errori          to true
                               move 78-ID-ef-clifor to control-id
                            else
                               move cli-codice to old-cliente
                            end-if
                         else
                            move cli-codice to old-cliente
                         end-if
                      end-if
                   end-if
                end-if
                display lab-destino 
      *               
           when 78-ID-ef-vettore
                inquire ef-vettore, value in vet-codice
                move spaces to lab-vettore-buf
                if vet-codice not = 0
                   read tvettori no lock
                        invalid
                        set errori to true
                        display message "Vettore NON valido"
                                  title tit-err
                                   icon 2
                    not invalid
                        move vet-descrizione to lab-vettore-buf
                   end-read
                end-if
                display lab-vettore
      *                                          
           when 78-ID-ef-num
                inquire ef-num value in tmo-numdoc-clifor
                if tmo-numdoc-clifor = spaces
                   set errori to true
                   display message box 
                           "Inserimento numero documento mancante"
                           title = tit-err
                           icon mb-warning-icon
                   move 78-ID-ef-num to control-id
                end-if         
      *                                          
           when 78-ID-ef-data-doc
                inquire ef-data-doc value in ef-data-doc-buf
                move ef-data-doc-buf to como-data
                if como-data = 0
                   move data-ult-movim to como-data
                   perform DATE-TO-SCREEN
                else
                   perform DATE-FORMAT
                end-if
                move como-data to ef-data-doc-buf
                display ef-data-doc

           when 78-ID-ef-data-via
                inquire ef-data-via value in ef-data-via-buf
                move ef-data-via-buf to como-data
                if como-data = 0
      *             move data-ult-movim to como-data
      *             perform DATE-TO-SCREEN
                   continue
                else
                   perform DATE-FORMAT
                end-if
                move como-data to ef-data-via-buf
                display ef-data-via
      *          
           when 78-ID-ef-pag
                inquire ef-pag, value in tblpa-codice2
                move spaces to lab-pag-buf
                move "PA" to tblpa-codice1
                if tblpa-codice2 not = spaces
                   read tcodpag 
                        invalid 
                           set errori to true
                           display message "Codice pagamento NON valido"
                                   title = tit-err
                                   icon mb-warning-icon
                    not invalid 
                        initialize lab-pag-buf
                        inspect tblpa-descrizione1 replacing trailing 
                                                   spaces by low-value
                        string  tblpa-descrizione1 delimited low-value
                                " "                delimited size
                                tblpa-descrizione2 delimited size
                                into lab-pag-buf
                        end-string
                   end-read
                else
                   set errori to true
                   display message box 
                           "Inserimento codice pagamento mancante"
                           title = tit-err             
                           icon mb-warning-icon
                end-if
                if errori move 78-ID-ef-pag to control-id end-if
                display lab-pag
      *
      *****     when 78-ID-ef-mag
      *****          inquire ef-mag, value in mag-codice
      *****          move spaces to lab-mag-buf
      *****          if mag-codice not = spaces
      *****             read tmagaz
      *****                  invalid
      *****                     set errori to true        
      *****                     display message "Codice magazzino NON valido"
      *****                             title = tit-err
      *****                             icon mb-warning-icon
      *****                     move 78-ID-ef-mag to control-id


      *****              not invalid move mag-descrizione to lab-mag-buf
      *****             end-read
      *****          else
      *****             set errori to true
      *****             display message box
      *****                     "Inserimento codice magazzino mancante"
      *****                     title = tit-err
      *****                     icon mb-warning-icon
      *****          end-if
      *****          if errori move 78-ID-ef-mag to control-id end-if
      *****          display lab-mag
      *****          move ef-mag-buf  to lab-codmag-buf
      *****          move lab-mag-buf to lab-desmag-buf
      *****          display lab-codmag lab-desmag
      *****          if tutto-ok
      *****             if MagazzinoInUso = spaces
      *****                move old-tmo-codmag to MagazzinoInUso
      *****             end-if
      *****             if MagazzinoInUso not = mag-codice
      *****                perform CHECK-RIGHE
      *****                if errori
      *****                   display message "Codice magazzino impossibile "
      *****                                  "per gli articoli già inseriti."
      *****                         x"0d0a""Reinserire il vecchio magazzino?"
      *****                           title = tit-err
      *****                           type mb-yes-no
      *****                           giving scelta
      *****                           icon 2
      *****                   move 78-ID-ef-mag to control-id
      *****                   if scelta = 1
      *****                      move MagazzinoInUso to ef-mag-buf
      *****                      move spaces         to lab-mag-buf
      *****                      display ef-mag lab-mag
      *****                   end-if
      *****                end-if
      *****             end-if
      *****          end-if
      *****          if tutto-ok and key-status = 13 ||CAMBIO DEL TAB
      *****             set ChangeTabToPageTwo to true
      *****          end-if
      *
           when 78-ID-cbo-stato
                inquire cbo-stato, value in cbo-stato-buf
                perform SCARICA-COMBO-STATO
                move stato   to  tmo-stato
                if tmo-stato not = old-tmo-stato
                   move 1 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"
         
                   if not Passwd-StatusOk
                      move old-tmo-stato to stato
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
      *
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf
                move ef-art-buf to art-codice
                if art-codice not = 0
                   if art-codice        not = old-art-codice    or
                      prg-cod-magazzino not = hid-cod-magazzino or
                      prg-peso          not = hid-peso          or
                      prg-tipo-imballo  not = hid-tipo-imballo  
                      read articoli no lock
                           invalid  set errori to true
                                    initialize art-stato
                       not invalid
                           if art-attivo
                              perform VALORIZZA-RIGA-ARTICOLO
                           else
                              set errori to true
                           end-if
                      end-read
                   else
                      move 0 to num-articoli
                   end-if
                else
                   set errori to true
                   initialize art-stato
                end-if

                if errori                                         
                   evaluate true
                   when art-bloccato
                        display message "Articolo BLOCCATO"
                                  title tit-err
                                   icon mb-warning-icon
                   when art-disattivo
                        display message "Articolo SOSPESO"
                                  title tit-err
                                   icon mb-warning-icon
                   when other
                        display message "Codice articolo NON valido"
                                  title tit-err
                                   icon mb-warning-icon
                   end-evaluate
                   move 78-ID-ef-art to control-id
                   move art-codice   to SaveArticolo
                   perform PULISCI-CAMPI
                   move SaveArticolo to ef-art-buf
                   display ef-art
                else
                   if CheckAfterZoom
                      perform CANCELLA-COLORE
                   end-if
                   if VariazioneDiValore
                      move 0 to ef-qta-buf
                      display ef-qta
                   end-if
                   move 78-ID-ef-qta to control-id
                   move 4 to accept-control
                end-if   
                perform DISPLAY-SCREEN
      *****          inquire ef-art, value in art-codice
      *****          if art-codice not = 0
      *****             move spaces to lab-desart-buf lab-udm-buf 
      *****                            lab-imb-buf    lab-desimb-buf
      *****             move      0 to ef-listino-buf  ef-coubat-buf 
      *****                            lab-tot-buf
      *****                            ef-sconto-buf   lab-peso-udm-buf
      *****                            lab-peso-kg-buf ef-cons-buf ef-qta-buf
      *****             set lab-imp-coubat to true
      *****             move 0 to lab-peso-buf
      *****             read articoli no lock 
      *****                  invalid 
      *****                  set errori to true
      *****                  display message "Codice articolo NON valido"
      *****                          title = tit-err
      *****                          icon mb-warning-icon
      *****              not invalid    
      *****                  perform FIND-PROGMAG
      *****                  if errori
      *****                     display message box 
      *****                     "Non esistono articoli avente "
      *****                     "il codice magazzino digitato"
      *****                             title = tit-err
      *****                             icon mb-warning-icon
      *****                  else
      *****                     if not trovato
      *****                        move mag-codice   to prg-cod-magazzino
      *****                        move "prg-tmagaz" to como-file         
      *****                        call "zoom-gt" using como-file, prg-rec
      *****                                      giving stato-zoom
      *****                        end-call
      *****                        cancel "zoom-gt"
      *****                        if stato-zoom = 0
      *****                           move prg-cod-articolo to ef-art-buf
      *****                           perform FIND-PROGMAG
      *****                        end-if
      *****                        |Setto errore in ogni caso così il cursore
      *****                        |si ferma sul campo e la lettura di progmag
      *****                        |col codice scelto viene fatta da sola
      *****                        set errori to true
      *****                     end-if
      *****                  end-if
      *****
      *****             end-read
      *****          else
      *****             set errori to true
      *****             display message box 
      *****                     "Inserimento codice articolo mancante"                              
      *****                     title = tit-err
      *****                     icon mb-warning-icon
      *****          end-if
      *****          if errori 
      *****             move 78-ID-ef-art to control-id
      *****          else
      *****             move 78-ID-ef-qta to control-id
      *****             move 4 to accept-control
      *****          end-if
      *****
      *****          display ef-art ef-listino ef-sconto ef-coubat
      *****                  lab-desart lab-udm lab-imb lab-peso lab-coubat 
      *****                  lab-tot lab-desimb lab-peso-udm
      *****                  lab-peso-kg ef-cons ef-qta
      * 
           when 78-ID-ef-qta
                inquire ef-qta, value in rmo-qta
                if VariazioneDiValore
                   perform CALCOLA-TOTALE
                   compute numero = hid-peso * rmo-qta
                   move numero to lab-peso-kg-buf
                   display lab-peso-kg
                   move 78-ID-ef-listino to control-id
                   move 4 to accept-control
                else
                   if rmo-qta = 0
                      |Secondo richiesta di Lubex per cui hanno la
                      |necessità di poter registrare per il magazzino
                      |movimentazioni senza quantità ad esempio per
                      |gil arrotondamenti. Diventano variazioni di valore
                      continue
      *****                display message "Inserimento quantità mancante"
      *****                        title = tit-err
      *****                        icon mb-warning-icon
      *****                set errori to true
      *****                move 78-ID-ef-qta to control-id
                   else
                      perform CALCOLA-TOTALE
                      compute numero = hid-peso * rmo-qta
                      move numero to lab-peso-kg-buf
                      display lab-peso-kg
                      move 78-ID-ef-listino to control-id
                      move 4 to accept-control
                   end-if
                end-if
      *
           when 78-ID-ef-listino
                inquire ef-listino, value in rmo-listino
                if rmo-listino = 0
      *****             display message box "Inserimento prezzo mancante"
      *****                     title = tit-err
      *****                     icon mb-warning-icon
      *****             set errori to true
      *****             move 78-ID-ef-listino to control-id
                   modify ef-sconto,  read-only
                   modify ef-cons,    read-only  
                   modify ef-coubat,  read-only
                   move 0 to rmo-imp-cons rmo-coubat  rmo-netto 
                             rmo-sconto   rmo-listino
                             ef-sconto-buf ef-cons-buf 
                             ef-coubat-buf ef-imp-merce-buf
                   display ef-listino 
                           ef-cons 
                           ef-coubat 
                           ef-sconto 
                           ef-imp-merce
                   perform CALCOLA-TOTALE
                else
                   modify ef-sconto, not read-only
                   modify ef-cons,   not read-only
                   modify ef-coubat, not read-only
                   perform CALCOLA-TOTALE
                   move 78-ID-ef-sconto to control-id
                   move 4 to accept-control
                end-if
      *
           when 78-ID-ef-sconto
                inquire ef-listino, value in rmo-listino
                perform CALCOLA-TOTALE
                move 78-ID-ef-cons to control-id
                move 4 to accept-control
      *      
           when 78-ID-ef-cons
                perform CALCOLA-TOTALE
                move 78-ID-ef-coubat to control-id
                move 4 to accept-control
      *      
           when 78-ID-ef-coubat
                perform CALCOLA-TOTALE
                move 78-ID-ef-art to control-id
                move 4 to accept-control
                if key-status = 13
                   perform ENTRY-TO-ROW
                end-if                 

           end-evaluate.
      *
           if errori
              perform CANCELLA-COLORE
              move CONTROL-ID to store-id
              move 4          to ACCEPT-CONTROL
           |SPECIFICO PER GESTIONE MOVIMENTI
           else    
              evaluate true        
              when num-articoli = 1
                  |Se c'è un solo rec. su progmag con quel codice articolo
                  |simulo la scelta di progmag da zoom con il rec. in linea
                   move GiacenzaKey   to prg-chiave
                   read progmag no  lock invalid continue end-read
                   move prg-cod-articolo to ef-art-buf
LUBEXX             move 0 to old-art-codice
                   display ef-art
                   set CheckAfterZoom to true                
                   move 78-ID-ef-art  to control-id
                   perform CONTROLLO         
                   set CheckAfterZoom to false
              when num-articoli > 1                
                   move 78-ID-ef-art  to control-id
                   perform ZOOM-SU-PROGMAG-ARTICOLO
              end-evaluate
           end-if.

      ***---
      * Principlamente per variazione, ma usato anche per riempire
      * la griglia iniziale con movimento o bozza evasione
       CURRENT-RECORD.
           set tutto-ok  to true.
           set ReadSecca to true.

           evaluate true
           when evasione
                initialize tmo-rec replacing numeric data by zeroes
                                        alphanumeric data by spaces

                move LinkChiave to teva-chiave
                read teva no lock invalid continue end-read
PATCH           |Usato come flag (se 0 non ho richiamato la mail)
PATCH           move 0 to tipo-messaggio
                move "F" to  SaveCliFor tmo-tipo
                move teva-cod-forn   to tmo-cod-clifor 
                move teva-mag        to EvaMag
                move low-value   to reva-chiave
                move teva-chiave to reva-chiave-testa
                start reva key >= reva-chiave
                      invalid
                      set  cli-tipo-F      to true
                      move teva-cod-forn   to cli-codice
                      read clienti no lock invalid continue end-read
                      move cli-pag         to tmo-codpag 
                  not invalid
                      read reva next
                      if reva-chiave-testa = teva-chiave
                         move reva-chiave-testa-ordf to tof-chiave
                         read tordforn no lock
                              invalid
                              set  cli-tipo-F      to true
                              move teva-cod-forn   to cli-codice
                              read clienti no lock 
                                   invalid continue 
                              end-read
                              move cli-pag         to tmo-codpag 
                          not invalid
                              move tof-cod-pagamento to tmo-codpag
                         end-read     
                      else
                         set  cli-tipo-F      to true
                         move teva-cod-forn   to cli-codice
                         read clienti no lock invalid continue end-read
                         move cli-pag         to tmo-codpag 
                      end-if
                end-start   

           when rotta
                initialize tmo-rec replacing numeric data by zeroes
                                        alphanumeric data by spaces

                move LinkChiave to btno-chiave
                read btnotacr no lock invalid continue end-read
PATCH           |Usato come flag (se 0 non ho richiamato la mail)
PATCH           move 0 to tipo-messaggio
                move "F" to  SaveCliFor tmo-tipo
                move tge-forn-corrisp to tmo-cod-clifor cli-codice
                move CausaleRotta-s   to tca-codice tmo-causale 
                                         lab-cau-buf
                move MagazRotta-s     to EvaMag lab-codmag-buf 
                                         mag-codice
                read tmagaz no lock
                move mag-descrizione  to lab-mag-buf

                read clienti no lock  invalid continue end-read
                move cli-pag          to tmo-codpag

           when bnota
                initialize tmo-rec replacing numeric data by zeroes
                                        alphanumeric data by spaces

                move LinkChiave to btno-chiave
                read btnotacr no lock invalid continue end-read
PATCH           |Usato come flag (se 0 non ho richiamato la mail)
PATCH           move 0 to tipo-messaggio
                move "C" to  SaveCliFor tmo-tipo
                move btno-cod-cli     to tmo-cod-clifor  cli-codice
                                                         des-codice
                move btno-prg-destino to tmo-destino     cli-codice
                                                         des-prog
                move btno-causale     to tmo-causale 
                                         lab-cau-buf tca-codice

                move EvaMag           to EvaMag lab-codmag-buf 
                                         mag-codice
                read tmagaz no lock
                move mag-descrizione  to lab-mag-buf
                                                     
                read clienti no lock  invalid continue end-read
                move cli-pag          to tmo-codpag
              
           when movimento
                if mod = 1
                   read tmovmag lock 
                        invalid set errori to true 
                   end-read
                 else
                    read tmovmag no lock 
                        invalid set errori to true 
                   end-read
                end-if
           end-evaluate.
           set ReadSecca to false.

           if tutto-ok

PATCH         |Usato come flag (se 0 non ho richiamato la mail)
PATCH         move 0 to tipo-messaggio
              move tmo-tipo to SaveCliFor

              if tmo-causale not = spaces |PER INSERIMENTO
                 move tmo-causale to tca-codice 
                 read tcaumag invalid continue end-read
                 move tca-descrizione to lab-des-cau-buf lab-causale-buf
                 move tca-tipo        to SaveCliFor cli-tipo-CF

                 if tca-si-zero set TotaleSiZero to true
                 else           set TotaleNoZero to true
                 end-if
              
                 if tca-no-movim-giac-periodo
                    set VariazioneDiValore   to true
                    modify ef-qta, read-only
                 else
                    set VariazioneDiQuantita to true
                    modify ef-qta, not read-only
                 end-if

                 move TipoDiVariazione to VariazioneInUso

                 move tmo-vettore     to vet-codice
                 read tvettori no lock invalid continue end-read
              end-if
              
              if SaveCli 
                 set cod-cli to true
                 move 1 to v-destino
                 move tmo-cod-clifor to des-codice
                 move tmo-destino    to des-prog
                 move spaces to des-ragsoc-1
                 read destini no lock invalid continue end-read
                 move des-ragsoc-1 to lab-destino-buf
                 display lab-destino
              else
                 set cod-for to true
                 move 0 to v-destino
              end-if

              move tmo-cod-clifor    to old-cliente

              move tmo-cod-clifor to cli-codice
              read clienti no lock invalid continue end-read
              move cli-ragsoc-1 to lab-cli-buf
              set  ttipocli-standard to true
              move cli-tipo to tcl-codice
              read ttipocli no lock invalid continue end-read

              move tca-cod-magaz to tmo-codmag 
                                    mag-codice 
                                    tmo-codmag 
                                    lab-codmag-buf
                                    MagazzinoInUso
                                    old-tmo-codmag
              read tmagaz invalid continue end-read
              move mag-descrizione to lab-desmag-buf lab-mag-buf

              move tmo-codpag to tblpa-codice2
              move "PA"       to tblpa-codice1
              move spaces     to tblpa-descrizione1 tblpa-descrizione2
              read tcodpag invalid continue end-read
              initialize lab-pag-buf
              inspect tblpa-descrizione1 replacing trailing 
                                            spaces by low-value
              string  tblpa-descrizione1 delimited by low-value
                      " "                delimited by size
                      tblpa-descrizione2 delimited by size
                      into lab-pag-buf
              end-string 

              perform FORM1-FLD-TO-BUF

              evaluate true
              when evasione 
                   perform RIEMPI-GRIGLIA-FROM-EVA
              when rotta
              when bnota
                   perform RIEMPI-GRIGLIA-FROM-BNOTA-ROTTA
              when movimento
                   perform RIEMPI-GRIGLIA
              end-evaluate

              if tmo-teva-anno   not = 0 and
                 tmo-teva-numero not = 0
PATCH            |Usato come flag (se 0 non ho richiamato la mail)
PATCH            move 0 to tipo-messaggio
                 if evasione
                    move tmo-teva-chiave to teva-chiave
                    read teva no lock invalid continue end-read
                    move teva-mag to EvaMag
                 else
                    move tmo-teva-chiave to btno-chiave
                    read btnotacr no lock invalid continue end-read
                    move MagazRotta-s to EvaMag
                 end-if
              end-if

              set YesDeleted to false
           end-if.

           if RecLocked
              set RecLocked to false
              set errori    to true
           end-if.

      ***---
       EF-CLIFOR-BEFORE-PROCEDURE.
           inquire ef-causale, value in ef-causale-buf.
           if ef-causale-buf = spaces
              perform CANCELLA-COLORE
              move 78-ID-ef-causale to control-id
              move 4 to accept-control
           end-if.

      ***---
       EF-DESTINO-BEFORE-PROCEDURE.
           inquire ef-clifor, value in ef-clifor-buf.
           if ef-clifor-buf = spaces
              perform CANCELLA-COLORE
              move 78-ID-ef-clifor to control-id
              move               4 to accept-control
           end-if.

      ***---
       FIND-MORE-ARTICOLI-ON-PROGMAG.
           move 0 to num-articoli.
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

      ***---
       FIND-PROGMAG.
           move 0 to imq-qta-imb| hid-imballi.
           
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
           if prg-bloccato or prg-disattivo
              move spaces  to hid-tipo-imballo
           end-if.
              
           if hid-tipo-imballo not = spaces
              move hid-tipo-imballo     to imq-codice
           else
              move art-imballo-standard to imq-codice
           end-if.
            
           read timbalqta no lock invalid continue end-read.
      *****     move imq-qta-imb     to hid-imballi imballi-ed.
           move imq-qta-imb     to imballi-ed.
           move imq-tipo        to imb-codice.
           read timballi no lock 
                invalid  initialize imb-descrizione
           end-read.               
           inspect imb-descrizione replacing trailing spaces 
                                                   by low-value.
           move imb-descrizione to hid-desimb.

      *****     if pgm-name = "gordcvar"
      *****        move ror-qta-imballi to hid-imballi
      *****        move ror-des-imballo to hid-des-imballo
      *****     end-if.
                                
           move art-codice to prg-cod-articolo.
      *****     move 0          to prg-peso. 
      *****     move spaces     to prg-cod-magazzino.
      *****     move spaces     to prg-tipo-imballo.
      *****     read progmag    no lock invalid continue end-read.
      *****     perform LABEL-VALORI.

      *    luciano 
      *****     move hid-imballi  to ef-qta-buf.

      ***---
       FIND-PROGMAG-CHANGE-CAUSALE.
      *****     move art-marca-prodotto to mar-codice.
      *****     read tmarche no lock invalid continue end-read.
           set tutto-ok to true.                                           
           move HiddenKey  to       prg-chiave.
           inquire ef-mag, value in prg-cod-magazzino.
           move art-codice to       prg-cod-articolo.
           read progmag no lock
                invalid set errori to true
            not invalid
                if prg-bloccato or prg-disattivo
                   set errori to true
                end-if
           end-read.

           if errori
              add 1 to idx
              inquire form1-gd-1(riga, 1), cell-data in col-num
              move col-num to numero-riga(idx)
           end-if.

      *****     set trovato       to false.
      *****     set tutto-ok      to true.
      *****     move low-value    to prg-rec.
      *****     move mag-codice   to prg-cod-magazzino.
      *****     start progmag key is >= key01 invalid set errori to true 
      *****     end-start.
      *****
      *****     if tutto-ok        
      *****        read progmag next
      *****        if mag-codice not = prg-cod-magazzino
      *****           set errori to true
      *****        end-if
      *****        if tutto-ok
      *****           move low-value    to prg-chiave
      *****           move art-codice   to prg-cod-articolo
      *****           move mag-codice   to prg-cod-magazzino
      *****           start progmag key is >= key01 
      *****                 invalid set errori to true
      *****           end-start
      *****           if tutto-ok
      *****              perform until 1 = 2
      *****                 read progmag next at end exit perform end-read
      *****                 if art-codice not = prg-cod-articolo
      *****                    exit perform
      *****                 end-if
      *****                 if prg-attivo
      *****                    set trovato to true
      *****                    exit perform
      *****                 end-if
      *****              end-perform
      *****           end-if
      *****        end-if
      *****     end-if.
      *****
      *****     if trovato
      *****        perform VALORIZZA-CELLE-LABELS
      *****     end-if.
           move 0 to GiacenzaKey.

      ***---
       INIT.
           set NonCambiareTab   to false.
           set ArticoloSetFocus to false.
           set ControllaCampi   to true.
           move 1 to event-data-1 screen1-ta-1-tab-value.
           perform SCREEN1-TA-1-TABCHANGE.
           perform CLEAR-SCREEN.
           move 0 to StatusHelp LastPrg.
           move spaces to MagazzinoInUso VariazioneInUso.

           move 0 to RowCounter.
           set RigaCambiata to false.

           move 78-ID-ef-data to control-id.
           move 4 to accept-control.
      *
           |Riempio il tabellino contenente i riferimenti agli ID
           |relativi ad ogni pagina del tab-control
           |specifico per i pgm. aventi Tab-Control
           move 78-ID-ef-data       to min-id(1).
           move 78-ID-cbo-stato     to max-id(1).
           |*******
      *
           Perform RIEMPI-COMBO-STATO.

           if nome-pgm = "gmovmag"
              move space to lab-cau-buf lab-des-cau-buf 
              move 0     to lab-data-buf
              set NewRow to true
           end-if.
      *
           move "Attivo" to cbo-stato-buf.
           Modify  cbo-stato,   value cbo-stato-buf.

           move 0 to mod.

      ***---
       INIT-OLD-REC.
           initialize old-tmo-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           set attivo     to true.

           move stato     to old-tmo-stato.

LUBEXX*****           move data-oggi to tmo-data-movim tmo-data-doc.
LUBEXX     move data-ult-movim to tmo-data-movim tmo-data-doc.

      ***---
       INQUIRE-HIDDEN.
           inquire form1-gd-1(riga, 1), hidden-data in gruppo-hidden.
      *****     inquire form1-gd-1(riga, 78-NumCol + 1),
      *****             hidden-data in hid-cod-articolo.
      *****     inquire form1-gd-1(riga, 78-NumCol + 2),
      *****             hidden-data in HiddenKey.
      *****     inquire form1-gd-1(riga, 78-NumCol + 3),
      *****             hidden-data in hid-udm.
      *****     inquire form1-gd-1(riga, 78-NumCol + 4),
      *****             hidden-data in hid-desimb.
      *****     inquire form1-gd-1(riga, 78-NumCol + 5),
      *****             hidden-data in hid-coubat.
      *****     inquire form1-gd-1(riga, 78-NumCol + 6),
      *****             hidden-data in hid-utf.
      *****     inquire form1-gd-1(riga, 78-NumCol + 7),
      *****             hidden-data in hid-non-utf.
      *****     inquire form1-gd-1(riga, 78-NumCol + 8),
      *****             hidden-data in hid-old-qta.
      *****     inquire form1-gd-1(riga, 78-NumCol + 9),
      *****             hidden-data in hid-peso-articolo.

      ***---
       LEGGI-ANNO.
           move spaces to tge-codice.
           read tparamge no lock invalid continue end-read.

      ***---
       MAGAZZINO-CHANGED.
           display message box
                   "Il codice del magazzino in uso ", MagazzinoInUso, 
                   " è stato cambiato"
           x"0d0a""Confermi la scelta?"
                  title  titolo
                   icon  2
                   type  mb-yes-no
                 giving  scelta
           if scelta = mb-yes
              perform CHECK-RIGHE
              if errori
                 display message 
                         "Codice magazzino impossibile per "
                         "gli articoli già inseriti a riga:"
                  x"0d0a"numero-riga(1),  "  "
                         numero-riga(2),  "  "
                         numero-riga(3),  "  "
                         numero-riga(4),  "  "
                         numero-riga(5),  "  "
                         numero-riga(6),  "  "
                         numero-riga(7),  "  "
                         numero-riga(8),  "  "
                         numero-riga(9),  "  "
                         numero-riga(10), "  "
                         numero-riga(11), "  "
                         numero-riga(12), "  "
                         numero-riga(13), "  "
                         numero-riga(14), "  "
                         numero-riga(15), "  "
                         numero-riga(16), "  "
                         numero-riga(17), "  "
                         numero-riga(18), "  "
                         numero-riga(19), "  "
                         numero-riga(20)
                         title tit-err
                          icon 2
              else
                 move ef-mag-buf to MagazzinoInUso
                 |Metto nelle righe il nuovo magazzino
                 inquire form1-gd-1, last-row in tot-righe
                 perform varying riga from 2 by 1 
                           until riga > tot-righe
                    perform INQUIRE-HIDDEN
                    move MagazzinoInUso to hid-cod-magazzino
                    perform MODIFY-HIDDEN
      *****              modify form1-gd-1(riga, 78-NumCol + 2),
      *****                     hidden-data = HiddenKey
                 end-perform
              end-if
           else
              set errori to true
           end-if.

      ***---
       MODIFY-HIDDEN.
           modify form1-gd-1(riga, 1), hidden-data gruppo-hidden.
      *****     modify form1-gd-1(riga, 78-NumCol + 1),
      *****            hidden-data = hid-cod-articolo.
      *****     modify form1-gd-1(riga, 78-NumCol + 2),
      *****            hidden-data = HiddenKey.
      *****     modify form1-gd-1(riga, 78-NumCol + 3),
      *****            hidden-data = hid-udm.
      *****     modify form1-gd-1(riga, 78-NumCol + 4),
      *****            hidden-data = hid-desimb.
      *****     modify form1-gd-1(riga, 78-NumCol + 5),
      *****            hidden-data = hid-coubat.
      *****     modify form1-gd-1(riga, 78-NumCol + 6),
      *****            hidden-data = hid-utf.
      *****     modify form1-gd-1(riga, 78-NumCol + 7),
      *****            hidden-data = hid-non-utf.
      *****     modify form1-gd-1(riga, 78-NumCol + 8),
      *****            hidden-data = hid-old-qta.
      *****     modify form1-gd-1(riga, 78-NumCol + 9),
      *****            hidden-data = hid-peso-articolo.

      ***---
       MOVE-DATI.
           initialize old-tmo-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

LUBEXX*****           move data-oggi to como-data old-tmo-data-movim.
LUBEXX     move data-ult-movim to como-data 
                                  old-tmo-data-movim old-tmo-data-doc.
           perform DATE-TO-SCREEN.
           move como-data to ef-data-buf.
           display ef-data.

           set cod to true.
           display lab-clifor.

           move low-value to mag-chiave.
           start tmagaz key is >= mag-chiave 
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmagaz next at end exit perform end-read
                    if si-mag-principale 
                       set tutto-ok to true
                       exit perform 
                    end-if
                 end-perform
           end-start.
           if tutto-ok  move mag-codice      to ef-mag-buf
                        move mag-descrizione to lab-mag-buf
           else         move spaces          to ef-mag-buf
                        move spaces          to lab-mag-buf
           end-if.
           move ef-mag-buf  to lab-codmag-buf old-tmo-codmag.
           move lab-mag-buf to lab-desmag-buf.
           display lab-codmag lab-desmag.
              
           set old-tmo-attivo to true. |Attivo

      ***---
       PB-CODICI-PRESSED.
           inquire ef-art, value in ef-art-buf.
           move ef-art-buf to prg-cod-articolo.
           initialize link-wprogmag replacing numeric data by zeroes
                                         alphanumeric data by spaces.
           move prg-cod-articolo to link-articolo.
           set link-accept to true.
           move tca-cod-magaz   to link-magazzino.
           move user-codi       to link-user of link-wprogmag.
           call   "wprogmag" using link-wprogmag.
           cancel "wprogmag".

      ***---
       PULISCI-CAMPI.
           initialize HiddenKey.
           move zero   to ef-art-buf     ef-qta-buf    
                          ef-coubat-buf  ef-sconto-buf  
                          ef-listino-buf 
                          ef-cons-buf    
                          lab-tot-buf    lab-peso-udm-buf
                          lab-peso-kg-buf old-art-codice 
                          ef-imp-merce-buf.

           move spaces to lab-desart-buf lab-udm-buf
                          lab-imb-buf    lab-desimb-buf.
      
           display ef-art     ef-cons
                   ef-qta     ef-sconto
                   ef-coubat  
                   ef-listino 
                   lab-desart lab-udm
                   lab-imb    lab-desimb
                   lab-tot    lab-peso-udm
                   lab-peso-kg ef-imp-merce.   
                   
      ***---
       READ-TMARCHE.
           move art-marca-prodotto to mar-codice.
           read tmarche no lock invalid continue end-read.
      *****     move mar-ven-var-listino-meno to hid-var-piu.
      *****     move mar-ven-var-listino-piu  to hid-var-meno.

      ***---
       RESETTA-CLIFOR.
           move 0                 to ef-clifor-buf.
           move spaces            to lab-cli-buf.
           move tca-tipo          to SaveCliFor.
           if SaveCli set cod-cli to true
                      move 1 to v-destino
           else       set cod-for to true
                      move 0 to v-destino
           end-if.
           display lab-clifor ef-clifor lab-cli ef-destino lab-destino.

      ***---
       RESETTA-GRID.
           modify form1-gd-1, reset-grid = 1.
           perform FORM1-GD-1-CONTENT.

      ***---
      * Principalemte per VARIAZIONE, ma anche per riempimento 
      * partendo da un altro movimento di magazzino
       RIEMPI-GRIGLIA. 
           set tutto-ok to true.
           perform RESETTA-GRID.

           move low-value  to rmo-riga.
           move tmo-anno   to rmo-anno.
           move tmo-numero to rmo-movim.

           start rmovmag key is >= rmo-chiave 
                 invalid set errori to true 
           end-start.

           set CambioCausale to true.
           move 0 to righe-iniziali.
           if tutto-ok
              modify form1-gd-1, mass-update = 1
              move 1 to riga
              perform until 1 = 2
                 read rmovmag next no lock at end exit perform end-read

                 if tmo-anno   not = rmo-anno or
                    tmo-numero not = rmo-movim
                    exit perform
                 end-if
                 add 1 to righe-iniziali riga
                 move rmo-chiave-progmag to prg-chiave
                 read progmag  no lock invalid continue end-read
                 move rmo-riga      to col-num LastPrg
                 move rmo-articolo  to col-art art-codice
                 read articoli no lock 
                      invalid 
                      move "** NON TROVATO **" to art-descrizione
                 end-read
                 move art-descrizione to col-des
                 move rmo-qta         to col-qta
                 if rmo-qta = 0
                    move 1 to rmo-qta
                 end-if
                 move rmo-peso     to col-udm
                 move rmo-peso     to como-peso
                 compute numero = como-peso * rmo-qta
                 move numero       to col-kg
                 move rmo-listino  to col-listino
                 move rmo-sconto   to col-sconto
                 move rmo-imp-cons to col-cons
                 move rmo-coubat   to col-coubat
                 move rmo-netto    to col-netto
                 compute numero = 
                ( rmo-netto + rmo-imp-cons + rmo-coubat ) * rmo-qta
                 move numero to col-tot
                 modify form1-gd-1(riga, 1),  cell-data = col-num    
                 modify form1-gd-1(riga, 2),  cell-data = col-art    
                 modify form1-gd-1(riga, 3),  cell-data = col-des    
                 modify form1-gd-1(riga, 4),  cell-data = col-qta    
                 modify form1-gd-1(riga, 5),  cell-data = col-udm    
                 modify form1-gd-1(riga, 6),  cell-data = col-kg     
                 modify form1-gd-1(riga, 7),  cell-data = col-listino
                 modify form1-gd-1(riga, 8),  cell-data = col-sconto 
                 modify form1-gd-1(riga, 9),  cell-data = col-netto  
                 modify form1-gd-1(riga, 10), cell-data = col-cons   
                 modify form1-gd-1(riga, 11), cell-data = col-coubat 
                 modify form1-gd-1(riga, 12), cell-data = col-tot    

                 initialize gruppo-hidden
                 move col-art           to hid-cod-articolo
                 move rmo-codmag        to hid-cod-magazzino
                 move rmo-imballo       to hid-tipo-imballo
                 move rmo-peso          to hid-peso
                 move rmo-udm           to hid-udm
LUBEXX           move col-qta           to hid-old-qta

                 move rmo-imballo to imq-codice
                 read timbalqta   no lock
                  not invalid
                      move imq-tipo to imb-codice
                      read timballi no lock invalid continue end-read
                 end-read
                 move imb-descrizione to hid-desimb

                 if lab-imp-cou set hid-cou   to true
                 else           set hid-cobat to true
                 end-if

                 compute hid-utf     = rmo-peso-tot-utf / rmo-qta
                 compute hid-non-utf = rmo-peso-tot    / rmo-qta

                 perform MODIFY-HIDDEN

                 if CambioCausale
                    move rmo-chiave-eva to reva-chiave
                    read reva no lock 
                         invalid  continue
                     not invalid  set CambioCausale to false
                    end-read
                 end-if

              end-perform
              modify form1-gd-1, mass-update = 0
              move 2 to riga event-data-2
              perform SPOSTAMENTO
           end-if.

      ***--- 
       RIEMPI-GRIGLIA-FROM-EVA.
           set tutto-ok to true.
           perform RESETTA-GRID.

           move low-value   to reva-riga.
           move teva-chiave to reva-chiave-testa.

           start reva key is >= reva-chiave 
                 invalid set errori to true 
           end-start.

           move 0 to righe-iniziali.
           if tutto-ok
              modify form1-gd-1, mass-update = 1
              move 1 to riga
              perform until 1 = 2
                 read reva next no lock at end exit perform end-read

                 if reva-chiave-testa not = teva-chiave
                    exit perform
                 end-if
                 add 1 to righe-iniziali riga
                 move reva-chiave-progmag to prg-chiave
                 read progmag  no lock invalid continue end-read
                 move reva-riga          to col-num LastPrg
                 move reva-articolo  to col-art art-codice
                 read articoli no lock 
                      invalid 
                      move "** NON TROVATO **" to art-descrizione
                 end-read
                 move art-descrizione to col-des
                 move reva-qta        to col-qta
                 move reva-peso       to col-udm como-peso
                 compute numero = como-peso * reva-qta
                 move numero       to col-kg
                 
                 compute numero = reva-netto + reva-add-pb
                 if numero = 0
                    if reva-imp-cons >= 0,01
                       subtract 0,01 from reva-imp-cons
                       move 0,01 to numero
                    else
                       if reva-coubat >= 0,01
                          subtract 0,01 from reva-imp-cons
                          move 0,01 to numero
                       end-if
                   end-if
                 end-if

                 move reva-imp-cons  to col-cons
                 move reva-coubat    to col-coubat
                 move numero         to col-listino
                 move numero         to col-netto
                 compute numero =
                       ( reva-netto    +
                         reva-add-pb   +
                         reva-imp-cons + 
                         reva-coubat ) * reva-qta

                 move numero to col-tot
                 modify form1-gd-1(riga, 1),  cell-data = col-num    
                 modify form1-gd-1(riga, 2),  cell-data = col-art    
                 modify form1-gd-1(riga, 3),  cell-data = col-des    
                 modify form1-gd-1(riga, 4),  cell-data = col-qta    
                 modify form1-gd-1(riga, 5),  cell-data = col-udm    
                 modify form1-gd-1(riga, 6),  cell-data = col-kg     
                 modify form1-gd-1(riga, 7),  cell-data = col-listino
                 modify form1-gd-1(riga, 8),  cell-data = col-sconto 
                 modify form1-gd-1(riga, 9),  cell-data = col-netto  
                 modify form1-gd-1(riga, 10), cell-data = col-cons   
                 modify form1-gd-1(riga, 11), cell-data = col-coubat 
                 modify form1-gd-1(riga, 12), cell-data = col-tot    

                 initialize gruppo-hidden
                 move col-art           to hid-cod-articolo
                 move reva-codmag       to hid-cod-magazzino
                 move reva-imballo      to hid-tipo-imballo

                 move reva-peso         to hid-peso
                 move art-unita-di-misura to hid-udm
LUBEXX           move col-qta           to hid-old-qta

                 move reva-imballo to imq-codice
                 read timbalqta   no lock
                  not invalid
                      move imq-tipo to imb-codice
                      read timballi no lock invalid continue end-read
                 end-read
                 move imb-descrizione to hid-desimb

                 if lab-imp-cou set hid-cou   to true
                 else           set hid-cobat to true
                 end-if

                 compute hid-utf     = prg-peso-utf
                 compute hid-non-utf = prg-peso-non-utf

                 perform MODIFY-HIDDEN
              end-perform
              modify form1-gd-1, mass-update = 0
              move 2 to riga event-data-2
              perform SPOSTAMENTO
           end-if.

      ***--- 
       RIEMPI-GRIGLIA-FROM-BNOTA-ROTTA.
           set tutto-ok to true.
           perform RESETTA-GRID.

           move low-value   to brno-rec.
           move btno-chiave to brno-chiave.
           if bnota
              set brno-nota        to true
           else
              set brno-merce-rotta to true
           end-if.

           start brnotacr key is >= brno-chiave 
                 invalid set errori to true 
           end-start.

           move 0 to righe-iniziali.
           if tutto-ok
              modify form1-gd-1, mass-update = 1
              move 1 to riga
              perform until 1 = 2
                 read brnotacr next no lock at end exit perform end-read

                 if bnota 
                    if brno-anno   not = btno-anno   or
                       brno-numero not = btno-numero or
                       brno-addebito or brno-merce-rotta
                       exit perform
                    end-if
                 else
                    if brno-anno   not = btno-anno   or
                       brno-numero not = btno-numero or
                       brno-addebito or brno-nota
                       exit perform
                    end-if
                 end-if
                 add 1 to righe-iniziali riga
                 move brno-prg-chiave   to prg-chiave
                 read progmag      no lock invalid continue end-read
                 move brno-num-riga     to col-num LastPrg
                 move brno-cod-articolo to col-art art-codice
                 read articoli no lock 
                      invalid 
                      move "** NON TROVATO **" to art-descrizione
                 end-read
                 move art-descrizione to col-des
                 move brno-qta        to col-qta
                 move brno-prg-peso   to col-udm como-peso
                 compute numero = como-peso * brno-qta
                 move numero       to col-kg
                 
                 compute numero = brno-imponib-merce + brno-add-piombo
                 if numero = 0
                    if brno-imp-consumo >= 0,01
                       subtract 0,01 from brno-imp-consumo
                       move 0,01 to numero
                    else
                       if brno-imp-cou-cobat >= 0,01
                          subtract 0,01 from brno-imp-cou-cobat
                          move 0,01 to numero
                       end-if
                   end-if
                 end-if

                 move brno-imp-consumo   to col-cons
                 move brno-imp-cou-cobat to col-coubat
                 move numero             to col-listino
                 move numero             to col-netto
                 compute numero =
                       ( brno-imponib-merce +
                         brno-add-piombo    +
                         brno-imp-consumo   + 
                         brno-imp-cou-cobat ) * brno-qta

                 move numero to col-tot
                 modify form1-gd-1(riga, 1),  cell-data = col-num    
                 modify form1-gd-1(riga, 2),  cell-data = col-art    
                 modify form1-gd-1(riga, 3),  cell-data = col-des    
                 modify form1-gd-1(riga, 4),  cell-data = col-qta    
                 modify form1-gd-1(riga, 5),  cell-data = col-udm    
                 modify form1-gd-1(riga, 6),  cell-data = col-kg     
                 modify form1-gd-1(riga, 7),  cell-data = col-listino
                 modify form1-gd-1(riga, 8),  cell-data = col-sconto 
                 modify form1-gd-1(riga, 9),  cell-data = col-netto  
                 modify form1-gd-1(riga, 10), cell-data = col-cons   
                 modify form1-gd-1(riga, 11), cell-data = col-coubat 
                 modify form1-gd-1(riga, 12), cell-data = col-tot

                 initialize gruppo-hidden
                 move col-art                to hid-cod-articolo
                 move brno-prg-cod-magazzino to hid-cod-magazzino
                 move brno-prg-tipo-imballo  to hid-tipo-imballo

                 move brno-prg-peso       to hid-peso
                 move art-unita-di-misura to hid-udm
LUBEXX           move col-qta             to hid-old-qta

                 move brno-prg-tipo-imballo to imq-codice
                 read timbalqta   no lock
                  not invalid
                      move imq-tipo to imb-codice
                      read timballi no lock invalid continue end-read
                 end-read
                 move imb-descrizione to hid-desimb

                 if lab-imp-cou set hid-cou   to true
                 else           set hid-cobat to true
                 end-if

                 compute hid-utf     = prg-peso-utf
                 compute hid-non-utf = prg-peso-non-utf

                 perform MODIFY-HIDDEN
              end-perform
              modify form1-gd-1, mass-update = 0
              move 2 to riga event-data-2
              perform SPOSTAMENTO
           end-if.

      ***---
       SELEZIONA-ALFA.
      * LA SECONDA CHIAVE E' ALFANUMERICA E LA VALUE VARIABLE CONTIENE
      * ANCORA GLI ZERI DAVANTI CHE ELIMINO
           inspect ef-clifor-buf replacing leading zero by spaces.
           move    ef-clifor-buf to cli-ragsoc-1.
      
      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           move SaveCliFor to cli-tipo-CF.
           call "C$JUSTIFY" using cli-ragsoc-1, "L".
      
           start clienti     key >= cli-k1
                 invalid     continue
                 not invalid read clienti next 
           end-start.
      
           move "clienti-alfa-CF" to como-file.
           call "zoom-gt" using   como-file, cli-rec
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = ZERO
              move cli-codice    to codice-ed
              move codice-ed     to ef-clifor-buf   
              call "C$JUSTIFY" using ef-clifor-buf, "L"
              display ef-clifor
              move cli-ragsoc-1  to lab-cli-buf
           else
              set errori           to true
              move 78-ID-ef-clifor to control-id
           end-if.

      ***---
       SELEZIONA-ALFA-DESTINO.
      * LA SECONDA CHIAVE E' ALFANUMERICA E LA VALUE VARIABLE CONTIENE
      * ANCORA GLI ZERI DAVANTI CHE ELIMINO
           inspect ef-destino-buf replacing leading ZERO by spaces.
           move    ef-destino-buf to des-ragsoc-1.
      
      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           call "C$JUSTIFY" using des-ragsoc-1, "L".
      
           start destini     key >= k1
                 invalid     continue
                 not invalid read destini next 
           end-start.
      
           move "clienti-des-alf" to como-file.
           call "zoom-gt" using   como-file, des-rec
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = ZERO
              move des-prog    to    codice-ed
              move codice-ed   to    ef-destino-buf
              call "C$JUSTIFY" using ef-destino-buf, "L"
              display ef-destino
              move des-ragsoc-1  to lab-destino-buf
           else
              set errori            to true
              move 78-ID-ef-destino to control-id
           end-if.

      ***--
       SELEZIONA-NUMERICO.
           move SaveCliFor to cli-tipo-CF.
           read clienti no lock
                invalid
                set errori to true
                if SaveCli
                   display message box "Codice cliente NON valido"
                             title tit-err
                              icon 2
                else
                   display message box "Codice fornitore NON valido"
                             title tit-err
                              icon 2
                end-if                          
            not invalid
                if cli-tipo-F
                   if cli-attivo
                      move cli-ragsoc-1 to lab-cli-buf
                   else
                      set errori to true
                      display message "Fornitore NON attivo"
                                title tit-err
                                 icon 2
                      move spaces to lab-cli-buf
                   end-if
                else
                   move cli-ragsoc-1 to lab-cli-buf
                end-if
           end-read.
       
      ***---
       SELEZIONA-NUMERICO-DESTINO.
           read destini no lock
                invalid
                set errori to true
                display message box "Progressivo destino NON valido"
                          title tit-err
                           icon 2
            not invalid
                move des-ragsoc-1 to lab-destino-buf
           end-read.

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
                                    X"0d0a""Contattare assistenza"
                                     title tit-err
                                      icon 3
              end-read

              move art-marca-prodotto to mar-codice
              read tmarche no lock invalid continue end-read
      
           end-if.
           
           perform CANCELLA-COLORE.                      

           perform COLORE.
           set event-action     to event-action-terminate.
      *     set ControllaCampi   to false.
           set ArticoloSetFocus to true.

      ***---
       STATUS-BAR-MSG.  
           if mod = 1
              if nome-pgm = "gmovmag"
                 modify form1-st-1-handle, 
                        panel-index  3, 
                        panel-text  "INSERIMENTO"
              else
                 modify form1-st-1-handle, 
                        panel-index  3, 
                        panel-text  "MODIFICA"
              end-if
           else
              modify form1-st-1-handle, 
                     panel-index  3, 
                     panel-text  "VISUALIZZAZIONE"
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
           move StatusHelp to e-cerca.
           modify tool-cerca, enabled       = e-cerca.
           modify tool-cerca, bitmap-number = BitmapNumZoom.

      ***---
       STRING-DESCRIZIONE-IMBALLO.
           read articoli no lock.
           read timbalqta
                invalid continue
            not invalid
                move imq-tipo  to imb-codice
                read timballi no lock invalid continue end-read
           end-read.
           inspect imb-descrizione replacing trailing spaces 
                                                   by low-value.
           initialize lab-desimb-buf.
           move imq-qta-imb       to imballi-ed.
           call "C$JUSTIFY"    using imballi-ed, "L".
           string imb-descrizione delimited by low-value
                  " da "          delimited by size
                  imballi-ed      delimited by spaces
                  " x "           delimited by size
                  art-udm-imballo delimited by size
                  into lab-desimb-buf
           end-string.

      ***---
       TROVA-DESTINO.
           set trovato to false.
           inquire ef-clifor, value in ef-clifor-buf.
           move ef-clifor-buf to des-codice convert.
           move ef-clifor-buf to cli-codice convert.

           move low-value  to des-prog.
           start destini  key is >= des-chiave
                 invalid  continue
             not invalid  read destini next
                 if des-codice = cli-codice 
                    set trovato to true 
                 else 
                    move ef-clifor-buf  to des-codice convert
                    move ef-destino-buf to des-prog   convert
                 end-if
           end-start.

      ***---
       VALORIZZA-ARRAY-CAUSALI.
           set link-update-um      to true.
           set link-update-peso    to false.
           set link-update-valore  to false.
           move "0000000000000000" to link-array.
           move 1 to multiplyer(1).
           move 1 to multiplyer(15).
           move 1 to multiplyer(16).

      ***---
       VALORIZZA-CELLE-LABELS.
           move 0 to como-peso.
           move prg-peso to como-peso.
           if SaveCli
              move art-prezzo-vendita        to ef-listino-buf
              move art-perce-sconto-agente   to ef-sconto-buf
              compute rmo-listino = 
                      art-prezzo-vendita - art-perce-sconto-agente
      *****        move rmo-netto to ef-netto-buf
           else                                      
              move art-prezzo-acquisto       to ef-listino-buf
              move art-perce-sconto-acquisto to ef-sconto-buf
              compute rmo-listino = 
                      art-prezzo-acquisto - art-perce-sconto-acquisto
      *****        move rmo-netto to ef-netto-buf
           end-if.
           move como-peso           to hid-peso-articolo.
           move art-unita-di-misura to lab-udm-buf.
           move hid-tipo-imballo    to lab-imb-buf.
           move hid-peso-articolo   to lab-peso-udm-buf. 

           perform READ-TMARCHE.
           perform CALCOLA-IMPOSTE.

           evaluate true
           when ImpostaCou        
                set  lab-imp-cou   to true
                move imposta-cou   to risultato-imposte
           when ImpostaCobat      
                set lab-imp-cobat  to true
                move imposta-cobat to risultato-imposte
           when ImpostaCouCobat   
                set lab-imp-coubat to true
                add imposta-cou    to imposta-cobat 
                               giving risultato-imposte
           end-evaluate.

LUBEXX*****           move imposta-consumo   to ef-cons-buf.
LUBEXX*****           move risultato-imposte to ef-coubat-buf.

           move hid-tipo-imballo to imq-codice.
           perform STRING-DESCRIZIONE-IMBALLO.
      
           move art-descrizione to lab-desart-buf.
                                 
           perform CALCOLA-TOTALE.
      
      *****     move art-peso-standard to lab-peso-udm-buf.      
           move como-peso   to lab-peso-udm-buf.
      
           compute numero = como-peso * rmo-qta.
           move numero to lab-peso-kg-buf.

           perform DISPLAY-SCREEN.

      ***---
       VALORIZZA-OLD.
           move tmo-rec                 to old-tmo-rec.
           move tmo-codmag              to old-tmo-codmag.
           set vecchio                  to true.
           
           if old-tmo-stato = spaces
              set old-tmo-attivo to true |Attivo
           end-if.
      
           evaluate CONTROL-ID
           when 78-ID-ef-causale
           when 78-ID-ef-clifor
           when 78-ID-ef-pag
           when 78-ID-ef-mag     move 1 to StatusHelp
           when other            move 0 to StatusHelp
           end-evaluate.
           perform STATUS-HELP.
      
           move 0 to riga-nuova.

      ***---
       VALORIZZA-RIGA-ARTICOLO.
           if art-attivo

      *****        move art-codice to old-art-codice
              move art-codice to SaveArticolo
              move 0 to num-articoli
              if SaveArticolo not = 0
                 if CheckAfterZoom
                    perform FIND-PROGMAG
                    perform VALORIZZA-CELLE-LABELS
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
       ZOOM-SU-PROGMAG.
           move 0 to num-articoli.
           inquire ef-art, value in art-codice.
           read articoli no lock
                invalid move spaces to art-descrizione
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
           initialize prg-rec.
           if filtro-articoli
              move SaveArticolo to prg-cod-articolo
           end-if.
           read tmagaz no lock 
                invalid move spaces to mag-descrizione 
           end-read.
           start progmag key  is >= prg-chiave
                 invalid continue 
           end-start.
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
                    move mag-descrizione     to tmp-prg-z-mag-des
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

           end-perform

           close tmp-progmag-zoom.
           call "W$MOUSE" using set-mouse-shape, arrow-pointer.

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
                    if tmp-prg-z-giacenza > como-giacenza or
                                            como-record = spaces
                       move tmp-prg-z-giacenza to como-giacenza
                       move tmp-prg-z-rec      to como-record
                    end-if
                 end-perform
           end-start.
           if como-record not = spaces
              move como-record to tmp-prg-z-rec
           end-if.
           close tmp-progmag-zoom.

      ********---
      ***** POSITION-ON-FIRST-RECORD.
      *****     |Mi posiziono sul PRIMO record
      *****     open input tmp-progmag-zoom.
      *****     move low-value to tmp-prg-z-rec.
      *****     start tmp-progmag-zoom key is >= key-des
      *****           invalid continue
      *****     end-start.
      *****     read tmp-progmag-zoom next end-read.
      *****     close tmp-progmag-zoom.

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
       ZOOM-SU-PROGMAG-ARTICOLO.
           move low-value to prg-rec.
           inquire ef-art,   value in prg-cod-articolo
           move prg-cod-articolo  to como-articolo.
           move tca-cod-magaz     to prg-cod-magazzino 
                                        como-magazzino.
           move 0      to como-giacenza.
           start progmag key is >=  key01
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform 
                    end-read
                    if prg-cod-articolo  not = como-articolo or
                       prg-cod-magazzino not = como-magazzino
                       exit perform
                    end-if
                    if prg-giacenza > como-giacenza or
                                      como-giacenza = 0
                       move prg-chiave   to save-prg-chiave
                       move prg-giacenza to como-giacenza
                    end-if
                 end-perform
           end-start.
           move save-prg-chiave to prg-chiave.

           move "prg-artico-sons-a"  to como-file.
           inquire ef-art,   value in prg-cod-articolo
           move tca-cod-magaz      to prg-cod-magazzino.
           call "zoom-gt"       using como-file, prg-rec
                               giving stato-zoom
           end-call.
           cancel "zoom-gt".
           if stato-zoom = 0
              move prg-cod-articolo to ef-art-buf
              display ef-art
              set CheckAfterZoom to true
              perform CONTROLLO         
              set CheckAfterZoom to false
           else
              set errori to true
              move 4     to accept-control
           end-if.

LUBEXX***---
LUBEXX VALORIZZA-ULT-DATA-MOVIM.
LUBEXX     move 0 to data-ult-movim.
LUBEXX     move high-value to tmo-chiave.
LUBEXX     start tmovmag key is <= tmo-chiave
LUBEXX           invalid continue
LUBEXX       not invalid
LUBEXX           read tmovmag previous no lock
LUBEXX           move tmo-data-movim to data-ult-movim
LUBEXX     end-start.
LUBEXX     if data-ult-movim = 0
LUBEXX        accept data-ult-movim from century-date
LUBEXX     end-if.

      ***---
       FORZA-PESO-UGUALE.
           read progmag no lock 
                invalid 
                display message "ARTICOLO " prg-cod-articolo
                         x"0d0a""RIGA " rmo-riga
                         x"0d0a""DATI INCOERENTI!!!"
                         x"0d0a""CANCELLARE E REINSERIRE LA RIGA"
                          title tit-err
                           icon 2
            not invalid
                compute rmo-peso-tot-utf = prg-peso-utf     * rmo-qta
                compute rmo-peso-tot     = prg-peso-non-utf * rmo-qta
           end-read.

      ***---
       ORDINATO.
      *****     if rof-prg-chiave = rmo-chiave-progmag
      *****        move hid-chiave-ordf to rof-chiave rmo-chiave-ordf
      *****        set RecLocked to false
      *****        initialize geslock-linkage
      *****        string "L'ordine fornitore n. " rof-numero
      *****           x"0d0a""risulta in uso su altro terminale."
      *****           x"0d0a""Questo comporta l'impossibilità ad"
      *****           x"0d0a""aggiornare la quantità evasa." delimited size
      *****             into geslock-messaggio
      *****        end-string
      *****
      *****        read rordforn lock key rof-chiave 
      *****             invalid continue
      *****         not invalid
      *****             perform until 1 = 2
      *****                if not RecLocked
      *****                   exit perform
      *****                end-if
      *****                set RecLocked to false
      *****                move 1 to geslock-v-riprova
      *****                move 0 to geslock-v-ignora
      *****                move 0 to geslock-v-termina
      *****                call   "geslock" using geslock-linkage
      *****                cancel "geslock"
      *****                evaluate true
      *****                when riprova 
      *****                     read rordforn lock key rof-chiave
      *****                          invalid continue 
      *****                     end-read
      *****                when other continue
      *****                end-evaluate
      *****             end-perform
      *****        end-read
      *****        if not RecLocked
      *****           add rmo-qta to rof-qta-evasa
      *****           accept rof-data-ultima-modifica from century-date
      *****           accept rof-ora-ultima-modifica  from time
      *****           move user-codi to rof-utente-ultima-modifica
      *****           rewrite rof-rec
      *****           unlock rordforn all records
      *****        end-if
      *****     end-if.
      *****
      *****     |Se per un'evasione multipla, sottraendo comunque la 
      *****     |quantità dall'ordinato, esso è completamente coperto
      *****     |non devo stornare alcun ordinato
      *****     if ( rof-qta-evasa - rmo-qta ) < rof-qta-ord
      *****
      *****        |Adesso devo stornare l'ordinato
      *****        move rof-chiave-testa to tof-chiave
      *****        read tordforn no lock invalid continue end-read
      *****        move tge-causale-ord-forn to link-causale
      *****        set link-update      to true
      *****        move rof-prg-chiave  to link-key
      *****        move tof-mese-rif    to link-mese-rif
      *****
      *****        move user-codi to link-user of link-wprogmag
      ******    storno l'ordinato
      *****
      *****        |Evasione multipla. Devo stornare solamente la parte 
      *****        |che di quest'ordine incide sulla qta evasa
      *****        if rof-qta-evasa > rof-qta-ord
      *****           compute link-valore = 
      *****                   rof-qta-ord - ( rof-qta-evasa - rmo-qta)
      *****        else
      *****           move rmo-qta        to link-valore
      *****        end-if
      *****        set link-update-um     to true
      *****        set link-update-peso   to false
      *****        set link-update-valore to false
      *****        move "0000000000000"   to link-array
      *****        move -1                to multiplyer(3)
      *****        call   "wprogmag"   using link-wprogmag
      *****        cancel "wprogmag"
      *****     end-if.

      ***---
       BEFORE-ARTICOLO.
      * Gli articoli che provengono da un'evasione (ne controllo tutti
      * per valutarne almeno l'integrità) non posso essere cambiati
           if evasione |or rotta
              modify ef-art,    read-only
              move 0 to StatusHelp
              perform STATUS-HELP
           else
              modify ef-art not read-only
              move 1 to StatusHelp
              perform STATUS-HELP
           end-if.

      ***---
       BEFORE-QTA.
      * Gli articoli che provengono da un'evasione (ne controllo tutti
      * per valutarne almeno l'integrità) non posso essere cambiati
           if evasione or VariazioneDiValore
              modify ef-qta,    read-only
      *        move 0 to StatusHelp
      *        perform STATUS-HELP
           else
              modify ef-qta not read-only
      *        move 1 to StatusHelp
      *        perform STATUS-HELP
           end-if.

      ***---
       BLOCCA-EVASIONE.
           perform READ-TEVA-LOCK.
           if tutto-ok
              move low-value   to reva-rec
              move teva-chiave to reva-chiave-testa
              start reva key >= reva-chiave
                    invalid continue
              end-start
              move 0 to idx-ordf tot-ordf
              initialize tab-ordf
           end-if.

      ***---
       READ-TEVA-LOCK.           
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "teva" to geslock-nome-file.

           set tutto-ok to true.
           read teva lock key teva-chiave invalid continue end-read.

           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              set RecLocked to false
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   read teva lock key teva-chiave
                        invalid continue 
                   end-read
              when termina
                   set errori to true
                   display message "Operazione interrotta"
                             title tit-err
                              icon 2
              end-evaluate
           end-perform.

      ***---
       BLOCCA-BOZZA-NC.
           perform READ-BTNOTACR-LOCK.
           if tutto-ok
              move low-value   to brno-rec
              move btno-anno   to brno-anno
              move btno-numero to brno-numero
              if rotta
                 set brno-merce-rotta to true
              else
                 set brno-nota        to true
              end-if
              start brnotacr key >= brno-chiave
                    invalid continue
              end-start
              move 0 to idx-ordf tot-ordf
              initialize tab-ordf
           end-if.

      ***---
       READ-BTNOTACR-LOCK.           
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "btnotacr" to geslock-nome-file.

           set tutto-ok to true.
           read btnotacr lock key btno-chiave invalid continue end-read.

           perform until 1 = 2
              if not RecLocked
                 exit perform
              end-if
              set RecLocked to false
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   read btnotacr lock key btno-chiave
                        invalid continue 
                   end-read
              when termina
                   set errori to true
                   display message "Operazione interrotta"
                             title tit-err
                              icon 2
              end-evaluate
           end-perform.

      ***---
       READ-TORDFORN-LOCK.           
           set tutto-ok  to true.
           set RecLocked to false.
           initialize geslock-linkage.
           move "tordforn" to geslock-nome-file.

           set tutto-ok to true.
           read tordforn lock key tof-chiave invalid continue end-read.

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
                   read tordforn lock key tof-chiave
                        invalid continue 
                   end-read
              when other continue
              end-evaluate
           end-perform.

      ***---
       OPERAZIONI-EVASIONE.
           read reva next. 
           move reva-chiave-testa-ordf to tof-chiave.
           perform READ-TORDFORN-LOCK.

           move reva-chiave-ordf to rof-chiave
           read rordforn no lock 
                invalid continue
            not invalid
                move rof-qta-evasa to old-evasa
                move rof-qta-ord   to old-ord

                subtract reva-qta from rof-qta-evasa
                add rmo-qta to rof-qta-evasa

                move rmo-chiave-progmag to rof-prg-chiave

                accept reva-data-modifica from century-date
                accept reva-ora-modifica  from time
                move user-codi to reva-utente-modifica

                rewrite rof-rec invalid continue end-rewrite
           end-read.
      *
           if tof-causale not = spaces
              initialize link-wprogmag
              move "0000000000000000" to link-array
              move -1 to multiplyer(1)
              move -1 to multiplyer(15)
              set  link-update   to true
              move reva-chiave-progmag  to link-key

              move tof-causale   to link-causale
              move reva-qta      to link-valore
              move user-codi     to link-user of link-wprogmag

              set link-update-um     to true
              set link-update-peso   to false
              set link-update-valore to false
              move reva-qta to link-valore

              call   "wprogmag" using link-wprogmag
              cancel "wprogmag"

              |Coperto sia prima che dopo
              if old-evasa     >= old-ord and
                 rof-qta-evasa >= rof-qta-ord
                 continue |non ha influito
              else
                 |In caso contrario è già stato stornato dall'evasione
                 if reva-qta = rmo-qta
                    continue
                 else
                    |Scoperto sia prima che dopo
                    if old-ord     > old-evasa and
                       rof-qta-ord > rof-qta-evasa 
                       compute link-valore = reva-qta - rmo-qta
                    end-if
      
                    |nei due casi devo rettificare dell'eccedente
                    |Prima scoperto poi coperto
                    if old-evasa     <= old-ord and
                       rof-qta-evasa >= rof-qta-ord
                       compute link-valore = old-evasa - rof-qta-ord
                    end-if
      
                    |Prima coperto poi scoperto
                    if old-evasa     >= old-ord and
                       rof-qta-evasa <= rof-qta-ord
                       compute link-valore = rof-qta-ord - rof-qta-evasa
                    end-if
                 
                    move tof-mese-rif       to link-mese-rif
                    move tof-chiave         to link-chiave-origine
                    move "0010000000000000" to link-array
                    call   "wprogmag"    using link-wprogmag
                    cancel "wprogmag"

                 end-if
              end-if

           end-if.

           set trovato  to false.
           set idx-ordf to 1.
           search el-chiave-ordf
           when el-chiave-ordf(idx-ordf) = reva-chiave-testa-ordf
                set trovato to true
           end-search.

           if not trovato
              add 1 to tot-ordf
              move reva-chiave-testa-ordf to el-chiave-ordf(tot-ordf)
           end-if.

      ***---
       STATO-ORDINI-FORNITORI.
           perform varying idx-ordf from 1 by 1 
                     until idx-ordf > tot-ordf
              move el-chiave-ordf(idx-ordf) to tof-chiave
              perform AGGIORNA-STATO-ORDF
           end-perform.

PATCH      move low-value  to rmo-chiave.
PATCH      move tmo-anno   to rmo-anno.
PATCH      move tmo-numero to rmo-movim.
PATCH      start rmovmag key >= rmo-chiave
PATCH            invalid continue
PATCH        not invalid
PATCH            perform until 1 = 2
PATCH               read rmovmag next no lock
PATCH                    at end exit perform
PATCH               end-read
PATCH               if rmo-anno  not = tmo-anno or
PATCH                  rmo-movim not = tmo-numero
PATCH                  exit perform
PATCH               end-if
                    move rmo-chiave-eva to reva-chiave
                    read reva no lock invalid continue end-read
                    move rmo-qta            to reva-qta
                    move rmo-chiave-progmag to reva-chiave-progmag
                    accept reva-data-modifica from century-date
                    accept reva-ora-modifica  from time
                    move user-codi to reva-utente-modifica
                    rewrite reva-rec invalid continue end-rewrite
PATCH            end-perform
PATCH      end-start.

      ***---
       OPERAZIONI-MERCE-ROTTA.
       OPERAZIONI-BNOTA.
