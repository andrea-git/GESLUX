      ***---
       CANCELLA-RIGA.
           set InsertRow to false.
           inquire form1-gd-1, last-row in tot-righe, cursor-y in riga.
           if riga-nuova = 1 move riga to tot-righe end-if.
           
           if tot-righe > 1 and riga <= tot-righe
              inquire form1-gd-1(riga, 2), hidden-data in HiddenKey
              move HiddenKey to brno-prg-chiave
              inquire form1-gd-2(riga, 2), last-row in tot-righe2
              
              set trovato to false
              perform varying store-riga2 from 2 by 1 
                        until store-riga2 > tot-righe2
                 inquire form1-gd-2(store-riga2, 2), 
                         hidden-data in HiddenKey
                 if HiddenKey = brno-prg-chiave
                    set trovato to true
                    exit perform
                 end-if
              end-perform
              if not trovato
                 move 0 to tot-righe2
              else
                 move 1 to tot-righe2
              end-if

              inquire form1-gd-3(riga, 2), last-row in tot-righe3
              
              set trovato to false
              perform varying store-riga3 from 2 by 1 
                        until store-riga3 > tot-righe3
                 inquire form1-gd-3(store-riga3, 2), 
                         hidden-data in HiddenKey
                 if HiddenKey = brno-prg-chiave
                    set trovato to true
                    exit perform
                 end-if
              end-perform

              if not trovato
                 move 0 to tot-righe3
              else
                 move 1 to tot-righe3
              end-if

              evaluate tot-righe2 also tot-righe3
              when 0 also 0
                   display message "Cancellare la riga selezionata?"
                             title titolo
                           default mb-no
                              type mb-yes-no
                            giving scelta
                              icon 2
              when 1 also 0
                   display message "ATTENZIONE!!!"
                            x"0d0a""Articolo con addebito."
                            x"0d0a""Cancellare la riga selezionata?"
                             title titolo
                          default mb-no
                             type mb-yes-no
                            giving scelta
                              icon 2
              when 0 also 1
                   display message "ATTENZIONE!!!"
                            x"0d0a""Articolo con merce rotta."
                            x"0d0a""Cancellare la riga selezionata?"
                             title titolo
                           default mb-no
                              type mb-yes-no
                            giving scelta
                              icon 2
              when 1 also 1
                   display message "ATTENZIONE!!!"
                       x"0d0a""Articolo con addebito e merce rotta."
                       x"0d0a""Cancellare la riga selezionata?"
                             title titolo
                           default mb-no
                              type mb-yes-no
                            giving scelta
                              icon 2
              end-evaluate

              if scelta = mb-yes
                 set YesDeleted to true
                 modify  form1-gd-1, record-to-delete = riga

                 if tot-righe2 not = 0
                    modify  form1-gd-2, record-to-delete = store-riga2
                 end-if

                 if tot-righe3 not = 0
                    modify  form1-gd-3, record-to-delete = store-riga3
                 end-if


                 inquire form1-gd-1, last-row in tot-righe
                 if riga > tot-righe
                    move tot-righe to riga
                 end-if
                 move riga to event-data-2
                 move 0 to riga-nuova
                 if tot-righe > 1  perform SPOSTAMENTO
                 else              perform INITIALIZE-ENTRY
                                   set InsertRow to true
                 end-if
              end-if
           end-if.
           set FromSpostamento to false.


      ***---
       CERCA.
           evaluate control-id

           when 78-ID-ef-cli
                set cli-tipo-c        to true
                move "clienti-all"    to como-file         
                inquire ef-cli, value in cli-codice
                call "zoom-gt"  using como-file, cli-rec
                               giving stato-zoom
                end-call
                cancel "zoom-gt"

                if stato-zoom = 0
                   move cli-codice     to ef-cli-buf
                   inspect ef-cli-buf replacing leading x"30" by x"20"
                   call "C$JUSTIFY" using ef-cli-buf, "L"
                   move cli-ragsoc-1   to lab-cli-buf
                   move cli-indirizzo  to lab-ind-buf
                   move cli-localita   to lab-loca-buf
                   display ef-cli lab-cli lab-ind lab-loca
                end-if                  
      
           when 78-ID-ef-des
                inquire ef-cli, value in des-codice
                inquire ef-des, value in des-prog
                set cli-tipo-c        to true
                move "clienti-des"    to como-file
                call "zoom-gt"  using como-file, des-rec
                               giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0                  
                   move des-prog     to ef-des-buf      
                   inspect ef-des-buf replacing leading x"30" by x"20"
                   call "C$JUSTIFY" using ef-des-buf, "L"
                   move des-ragsoc-1   to lab-des-buf
                   move des-indirizzo  to lab-ind-d-buf
                   move des-localita   to lab-loca-d-buf
                   display ef-des lab-des lab-ind-d lab-loca-d
                end-if
      
           when 78-ID-ef-vet
                move "tvettori"    to como-file         
                inquire ef-vet,  value in vet-codice
                call "zoom-gt"   using como-file, vet-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move vet-codice      to ef-vet-buf
                   move vet-descrizione to lab-vet-buf
                   display ef-vet lab-vet
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
                   perform MOVE-DESCR-PAG
                   display ef-pag lab-pag
                end-if

           when 78-ID-ef-cli-fm
                set cli-tipo-c        to true
                move "clienti-all"    to como-file         
                inquire ef-cli-fm, value in cli-codice
                call "zoom-gt"  using como-file, cli-rec
                               giving stato-zoom
                end-call
                cancel "zoom-gt"

                if stato-zoom = 0
                   move cli-codice     to ef-cli-fm-buf
                   move cli-ragsoc-1   to lab-cli-fm-buf
                   move cli-indirizzo  to lab-ind-fm-buf
                   move cli-localita   to lab-loca-fm-buf
                   display ef-cli-fm lab-cli-fm lab-ind-fm lab-loca-fm
                end-if                  
      
           when 78-ID-ef-des-fm
                inquire ef-cli-fm, value in des-codice
                inquire ef-des-fm, value in des-prog
                set cli-tipo-c        to true
                move "clienti-des"    to como-file
                call "zoom-gt"  using como-file, des-rec
                               giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0                  
                   move des-prog      to ef-des-fm-buf
                   move des-ragsoc-1  to lab-des-fm-buf
                   move des-indirizzo to lab-ind-d-fm-buf
                   move des-localita  to lab-loca-d-fm-buf
                   display ef-des-fm lab-des-fm 
                           lab-ind-d-fm lab-loca-d-fm
                end-if
      
           when 78-ID-ef-art
                perform ZOOM-SU-PROGMAG
                move 4 to accept-control
      
           when 78-ID-ef-prz
                move Link-TipoNota to tca-codice
                read tcaumag no lock invalid continue end-read
                evaluate true
                when tca-tipo-nota-reso
                     perform ZOOM-SU-FATTURATI
                     move 4 to accept-control
                end-evaluate

           end-evaluate.

      ***---
       ZOOM-IVA.
           move "tivaese"   to como-file.
           move "IV"        to tbliv-codice1.
           call "zoom-gt"   using como-file, record-tbliv
                           giving stato-zoom
           end-call.
           cancel "zoom-gt" . 

      ***---
       CHANGE-TAB.
           set NonCambiareTab   to false.
           set ArticoloSetFocus to false.
           set ControllaCampi   to true.
           set CheckDestini     to true.
           set tutto-ok         to true.

      *****     if mod-k = 1
      *****        continue
      *****        set NonCambiareTab to true
      *****     else

           evaluate pagina
           when 1
                set CheckDestini to false
                perform CHECK-PAGE-1
                if tutto-ok
                   move 0 to v-fatt
                else
                   move store-id to CONTROL-ID
                   move 4        to ACCEPT-CONTROL
                   set NonCambiareTab to true
                end-if
           when 2
                perform CHECK-PAGE-2
                if errori
                   move store-riga   to event-data-2
                   perform SPOSTAMENTO
                   set errori to true
                   move 78-ID-ef-art to CONTROL-ID store-id
                   move 4            to ACCEPT-CONTROL
                   set NonCambiareTab to true
                end-if
           end-evaluate.

           if tutto-ok
              evaluate event-data-1
              when 1
                   move 0 to v-fatt v-fatt-2
                   if LinkNumero not = 0 move 1 to v-fatt end-if
                   set ControllaCampi to false
                   set environment "KEYSTROKE" to "DATA=44 44"
                   set environment "KEYSTROKE" to "DATA=46 46"

              when 2
                   if LinkNumero = 0 move 0 to v-fatt-2
                   else              move 1 to v-fatt-2
                   end-if
                   perform CANCELLA-COLORE
                   if inserimento
                      modify pb-genera,       bitmap-number = 1
                      modify pb-nuovo,        bitmap-number = 5
                      modify pb-elimina,      bitmap-number = 4
                      move 5 to NumBitmapNuovoGrid
                      move 4 to NumBitmapEliminaGrid
                   end-if
                   set ArticoloSetFocus    to true
                   |Se non vengo dalla prima pagina
                   if pagina not = 1
                      set ControllaCampi to false
                   end-if
                   inquire form1-gd-1, last-row in tot-righe
                   if tot-righe = 1
                      perform PB-NUOVO-LINKTO
                   end-if
                   inquire form1-gd-1, cursor-y in riga
                   move riga to event-data-2
                   perform SPOSTAMENTO
      *****             move spaces to HiddenKey
      *****             perform INITIALIZE-ENTRY

              when 3
      *****             move Link-TipoNota to tca-codice
      *****             read tcaumag no lock invalid continue end-read
      *****             if tca-cod-magaz not = "LBX"
      *****                set NonCambiareTab  to true
      *****                move pagina         to event-data-1
      *****             else
                      move 0 to v-fatt v-fatt-2
                      set environment "KEYSTROKE" to "DATA=44 44"
                      set environment "KEYSTROKE" to "DATA=46 46"
                      perform RIEMPI-GRID-ADDEBITO
                      |Se non vengo dalla prima pagina
                      if pagina not = 1
                         set ControllaCampi to false
                      end-if
                      inquire form1-gd-2, last-row in tot-righe2
                      move spaces to HiddenKey
      *****             end-if
              when 4
                   move Link-TipoNota to tca-codice
                   read tcaumag no lock invalid continue end-read
                   move tca-cod-magaz to mag-codice
                   perform VERIFICA-ABIL-ROTTA
                   if si-rotta
                      move 0 to v-fatt v-fatt-2
                      set environment "KEYSTROKE" to "DATA=44 44"
                      set environment "KEYSTROKE" to "DATA=46 46"
                      perform RIEMPI-GRID-MERCE-ROTTA
                      |Se non vengo dalla prima pagina
                      if pagina not = 1
                         set ControllaCampi to false
                      end-if
                      inquire form1-gd-2, last-row in tot-righe2
                      move spaces to HiddenKey
                   else
                      set NonCambiareTab  to true
                      move pagina         to event-data-1

                   end-if
              end-evaluate

              move event-data-1 to pagina
           end-if.

           display Screen1-Ta-1 . 

      ***---
       CHECK-PAGE-1.
           perform  varying control-id from 78-ID-ef-cli by 1
                      until control-id    > 78-ID-ef-note
              perform CONTROLLO
              if errori 
                 move control-id to store-id
                 exit perform 
              end-if
           end-perform.

      ***---
       CHECK-PAGE-2.
           set tutto-ok to true.
           |PRIMO CONTROLLO: valorizzo codice IVA
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 10), 
                      cell-data in col-cod-iva
              if col-cod-iva = spaces
                 set errori to true
                 display message "Valorizzare codice IVA"
                           title tit-err
                            icon 2
                 exit perform
              end-if
           end-perform.

      ***---
       CONTA-RIGHE.
           set trovato to false.

           inquire form1-gd-1, last-row in tot-righe.
           if tot-righe > 1 set trovato to true end-if . 

      ***---
       CONTA-ZERI.
           set trovato to false.

           inquire form1-gd-1, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire form1-gd-1(riga, 4), cell-data in brno-qta
              if brno-qta > 0
                 set trovato to true
                 exit perform
              end-if
           end-perform . 

      ***---
       CONTROLLA-PERCENTUALE-IVA.
           move "IV"         to tbliv-codice1.
           move brno-cod-iva to tbliv-codice2.
           read tivaese no lock 
                invalid continue
            not invalid
                if tbliv-percentuale not = 0
                   set EsisteIVA to true
                end-if
           end-read . 

      ***---
       CONTROLLA-TOTALE.
           move 0 to Sum.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 5), cell-data in SavePrezzo
              add SavePrezzo to Sum
           end-perform.
           if Sum = 0
              if TotaleNoZero set errori to true end-if
           else
              if TotaleSiZero set errori to true end-if
           end-if . 

      ***---
       CONTROLLO.
           set tutto-ok to true. 
           if not ControllaCampi
              set ControllaCampi to true
              exit paragraph
           end-if.
           if mod-campi = 0
              exit paragraph
           end-if.
           inquire form1-gd-1, last-row in tot-righe.
           if tot-righe = 1 and riga-nuova = 1
              move 2 to tot-righe
           end-if.
           
           inquire form1-gd-2, last-row in tot-righe2.
           if tot-righe2 = 1 and riga-nuova2 = 1
              move 2 to tot-righe2
           end-if.
           
           inquire form1-gd-3, last-row in tot-righe3.
           if tot-righe3 = 1 and riga-nuova3 = 1
              move 2 to tot-righe3
           end-if.

      * Paragrafo per la struttura dei controlli sulla screen Screen1
           evaluate control-id
           |78-ID-ef-cli è l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf
                move ef-cli-buf to cli-codice
                if ef-cli-buf not = 0
                   perform SELEZIONA-CLIENTE
                else
                   set errori to true  
                   move 78-ID-ef-cli to control-id
                   display message "Inserimento codice cliente mancante"
                             title tit-err
                              icon 2
                end-if
                if errori
                   move spaces to cli-ragsoc-1 
                                  cli-indirizzo 
                                  cli-localita
                end-if
                move cli-ragsoc-1    to lab-cli-buf
                move cli-indirizzo   to lab-ind-buf
                move cli-localita    to lab-loca-buf
                display lab-cli lab-ind lab-loca

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des
                inquire ef-des, value in ef-des-buf
                inquire ef-cli, value in ef-cli-buf
                move ef-cli-buf to des-codice
                move ef-des-buf to des-prog
                if des-prog not = 0
                   move 0 to des-no-bloc
                   perform SELEZIONA-DESTINO
                   display lab-des
                   display lab-ind-d
                   display lab-loca-d
                else
                   if CheckDestini
                      perform TROVA-DESTINO
                      initialize des-rec
                      if trovato and des-no-bloc not = cli-codice
                         display message "Esiste uno o più destini per"
                                          " il cliente specificato."
                                  x"0d0a""Procedere comunque con "
                                          "progressivo non valorizzato?"
                                 title titolo
                                  icon 2
                                  type mb-yes-no
                                giving scelta    
                         if scelta = mb-no
                            set errori to true
                            move 78-ID-ef-des to control-id       
                         else
                            move cli-codice to des-no-bloc
                         end-if
                      end-if
                   else
                      set CheckDestini to true
                   end-if
                end-if
                move des-ragsoc-1    to lab-des-buf
                move des-indirizzo   to lab-ind-d-buf
                move des-localita    to lab-loca-d-buf
                display lab-des lab-ind-d lab-loca-d

           |78-ID-ef-vet è l'ID del control ef-vet
           when 78-ID-ef-vet
                inquire ef-vet, value in ef-vet-buf
                move ef-vet-buf to vet-codice
                if vet-codice = 0
                   if form1-radio-1-buf = 2
                      set errori to true
                      display message "Vettore obbligatorio"
                                title tit-err
                                 icon 2
                   end-if
                   move spaces to vet-descrizione
                else
                   read tvettori no lock
                        invalid
                        move spaces to vet-descrizione
                        display message "Codice Vettore NON valido!"
                                  title tit-err
                                   icon 2
                        set errori to true
                   end-read
                end-if
                move vet-descrizione to lab-vet-buf
                display lab-vet

           |78-ID-ef-pag è l'ID del control ef-pag
           when 78-ID-ef-pag
                inquire ef-pag, value in ef-pag-buf
                move "PA"       to tblpa-codice1
                move ef-pag-buf to tblpa-codice2
                read tcodpag no lock
                     invalid
                     move spaces to tblpa-descrizione2 
                                    tblpa-descrizione2
                     display message box "Codice Pagamento NON valido"
                             title = tit-err
                             icon  2
                     set errori to true
                end-read
                perform MOVE-DESCR-PAG
                display lab-pag     

           |78-ID-ef-data-bolla è l'ID del control ef-data-bolla
           when 78-ID-ef-data-bolla
                inquire ef-data-bolla, value in ef-data-bolla-buf
                move ef-data-bolla-buf to como-data
                if como-data not = 0
                   perform DATE-FORMAT
                   move como-data to ef-data-bolla-buf
                   display ef-data-bolla
                end-if                  

                inquire ef-num-bolla,  value in ef-num-bolla-buf
                inquire ef-data-bolla, value in ef-data-bolla-buf
                move ef-data-bolla-buf to como-data
                if ( como-data            = 0 and 
                     ef-num-bolla-buf     = spaces ) or
                   ( como-data        not = 0 and 
                     ef-num-bolla-buf not = spaces )
                   continue
                else
                   set errori to true
                end-if

                if errori
                   display message "Inserire data e numero"
                             title tit-err
                              icon 2
                   move 78-ID-ef-num-bolla to control-id store-id
                end-if

           |78-ID-ef-num-reso è l'ID del control ef-num-reso
           when 78-ID-ef-num-reso
                inquire ef-num-reso, value in ef-num-reso-buf
                move ef-num-reso-buf to btno-num-reso
                if btno-num-reso not = 0 and 
                   btno-num-reso not = old-btno-num-reso
                   initialize chk-bz-cli-linkage
                              replacing numeric data by zeroes
                                   alphanumeric data by spaces
                   move btno-chiave   to chk-bz-chiave
                   move btno-num-reso to chk-bz-num-reso
                   move btno-data(5:2)to chk-bz-mese

                   call   "chk-reso-bozze" using chk-bz-cli-linkage
                   cancel "chk-reso-bozze"

                   if chk-bz-status = -1
                      display message "Esiste già la bozza con"
                               x"0d0a""Anno: " chk-bz-anno
                               x"0d0a""Numero: " chk-bz-numero
                               x"0d0a""Mese: " chk-bz-mese
                               x"0d0a""avente lo stesso numero reso."
                               x"0d0a""Proseguire?"
                                title titolo
                                 type mb-yes-no
                               giving scelta
                                 icon 2
                      if scelta = mb-no
                         set errori to true
                         move 78-ID-ef-num-reso to control-id
                      end-if
                   end-if
                end-if    

           when 78-ID-ef-data-ingresso
                if mod = 1
                   inquire ef-data-ingresso, 
                           value in ef-data-ingresso-buf
                   move ef-data-ingresso-buf to como-data
                   if como-data not = 0
                      perform DATE-FORMAT
                      move como-data to ef-data-ingresso-buf
                      display ef-data-ingresso
                      perform DATE-TO-FILE
                      if como-data <= tge-data-consolid-progmag          
                         set errori to true
                         display message 
                         "La data di ingresso dev'essere > "
                         "alla data di consolidamento magazzino ("
                         tge-data-consolid-progmag(7:2)"/" 
                         tge-data-consolid-progmag(5:2)"/"
                         tge-data-consolid-progmag(1:4)")"
                                   title tit-err
                                    icon 2
                         move 78-ID-ef-data-ingresso to control-id 
                                                        store-id
                      end-if
                   end-if
                end-if

           |78-ID-ef-cli-fm è l'ID del control ef-cli-fm
           when 78-ID-ef-cli-fm
                inquire ef-cli-fm, value in ef-cli-fm-buf 
                move ef-cli-fm-buf to cli-codice
                if cli-codice not = 0
                   perform SELEZIONA-CLIENTE
                   move cli-tipo to tcl-codice
                   read ttipocli no lock invalid continue end-read
                else
                   if form1-radio-1-buf = 1 and tot-righe2 > 1
                      set errori to true  
                      move 78-ID-ef-cli-fm to control-id
                      display message "Inserimento codice cliente mancan
      -    "te"
                                title tit-err
                                icon mb-warning-icon
                   else
                      move spaces to cli-ragsoc-1 
                      move spaces to cli-indirizzo 
                      move spaces to cli-localita
                      move 0 to ef-des-fm-buf
                      display ef-des-fm
                      move spaces to lab-des-fm-buf  
                      move spaces to lab-ind-d-fm-buf
                      move spaces to lab-loca-d-fm-buf
                      display lab-des-fm lab-ind-d-fm lab-loca-d-fm
                   end-if
                end-if
                move cli-ragsoc-1    to lab-cli-fm-buf
                move cli-indirizzo   to lab-ind-fm-buf
                move cli-localita    to lab-loca-fm-buf
                display lab-cli-fm lab-ind-fm lab-loca-fm

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des-fm
                inquire ef-des-fm, value in ef-des-fm-buf
                inquire ef-cli-fm, value in ef-cli-fm-buf
                move ef-cli-fm-buf to des-codice
                move ef-des-fm-buf to des-prog
                if des-prog not = 0    
                   move 0 to des-no-bloc
                   perform SELEZIONA-DESTINO
                else
                   if CheckDestini
                      perform TROVA-DESTINO   
                      if trovato and des-no-bloc not = cli-codice
                         display message "Esiste uno o più destini per"
                                          " il cliente specificato."
                                  x"0d0a""Procedere comunque con "
                                          "progressivo non valorizzato?"
                                 title titolo
                                  icon 2
                                  type mb-yes-no
                                giving scelta 
                         if scelta = mb-no
                            set errori to true
                            move 78-ID-ef-des-fm to control-id
                         else
                            move cli-codice to des-no-bloc
                         end-if
                      end-if
                   else
                      set CheckDestini to true
                   end-if
                end-if
                move des-ragsoc-1    to lab-des-fm-buf
                move des-indirizzo   to lab-ind-d-fm-buf
                move des-localita    to lab-loca-d-fm-buf
                display lab-des-fm lab-ind-d-fm lab-loca-d-fm

           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf
                move ef-art-buf to art-codice
                if art-codice = 0
                   set errori to true
                else
                   if tot-righe > 1
                      move spaces to lab-art-buf
                      if art-codice not = hid-cod-articolo
                         move spaces to HiddenKey
                      end-if 
                      read articoli no lock
                           invalid  set errori to true
                       not invalid
                           if art-attivo
                              move art-marca-prodotto to mar-codice
                              read tmarche no lock 
                                   invalid continue 
                              end-read
                              if HiddenKey = spaces
                                 perform FIND-MORE-ARTICOLI-ON-PROGMAG
                                 evaluate num-articoli
                                 when 0
                                      set errori to true
                                 when 1
                                      move GiacenzaKey to prg-chiave 
                                                          HiddenKey
                                      read progmag no lock 
                                           invalid continue 
                                      end-read
                                      move prg-peso-utf  
                                        to hid-peso-utf
                                      move prg-peso-non-utf 
                                        to hid-peso-non-utf
                                 when other             
                                      perform ZOOM-SU-PROGMAG-ARTICOLO
                                      move 4 to accept-control
                                 end-evaluate
                              end-if
                              move HiddenKey  to prg-chiave
                              if ef-iva-buf = spaces
                                 move cli-iva-ese    to ef-iva-buf
                              end-if
                              if ef-iva-buf = spaces
                                 move art-codice-iva  to ef-iva-buf
                              end-if
                              move "IV"       to tbliv-codice1
                              move ef-iva-buf to tbliv-codice2
                              read tivaese no lock 
                                   invalid continue 
                              end-read
                              move tbliv-descrizione1 to lab-iva-buf
                              display lab-iva
                              move art-descrizione 
                                to lab-art-buf col-des
                           else
                              set errori to true
                           end-if
                      end-read
                      display lab-art ef-qta ef-prz ef-iva
                      if errori
                         perform INITIALIZE-ENTRY
                         initialize art-stato
                         display message "Codice articolo NON valido"
                                 title = tit-err
                                 icon  2        
                      end-if
                   end-if
                end-if
                if tutto-ok 
                   move 78-ID-ef-qta to control-id
                   move 4            to accept-control
                end-if

           |78-ID-ef-qta è l'ID del control ef-qta
           when 78-ID-ef-qta
                if tot-righe > 1
                   inquire ef-qta, value in ef-qta-buf
                   move ef-qta-buf to ror-qta
                   if ror-qta = 0
                      set errori to true
                      display message "Quantità mancante"
                                title tit-err
                                 icon 2
                   end-if
                end-if
                if tutto-ok
                   move 78-ID-ef-prz to control-id
                   move 4            to accept-control
                end-if

           |78-ID-ef-prz è l'ID del control ef-prz
           when 78-ID-ef-prz
                if tot-righe > 1
                   inquire ef-prz, value in ef-prz-buf
LUBEXX             move ef-prz-buf to brno-prz-unitario
LUBEXX             if brno-prz-unitario = 0
LUBEXX                move tge-cod-iva-omag to ef-iva-buf
LUBEXX                display ef-iva
                      move 0 to brno-imp-consumo   ef-consumo-buf
                      move 0 to brno-imp-cou-cobat ef-coubat-buf
                      move 0 to brno-add-piombo    ef-add-buf
                      move 0 to brno-imponib-merce ef-imp-buf
                      display ef-consumo ef-coubat ef-add ef-imp
                   else
                      perform VALORIZZA-CELLE-CAMPI-MANUALE
LUBEXX             end-if
      *****             perform CANCELLA-COLORE
      *****             move 78-ID-ef-iva to control-id
      *****             move 4            to accept-control
                end-if
                if tutto-ok 
                   move Link-TipoNota to tca-codice
                   read tcaumag no lock invalid continue end-read
                   if tca-tipo-nota-reso
                      move 78-ID-ef-iva to control-id
                      move 4            to accept-control
                   end-if
                end-if

           when 78-ID-ef-consumo
                if tot-righe > 1
                   perform CALCOLA-IMPONIBILE
                   display ef-imp
                end-if
                perform RESETTA-VIRGOLA

           when 78-ID-ef-coubat
                if tot-righe > 1
                   perform CALCOLA-IMPONIBILE
                   display ef-imp
                end-if
                perform RESETTA-VIRGOLA

           when 78-ID-ef-add
                if tot-righe > 1             
                   perform CALCOLA-IMPONIBILE
                   display ef-imp
                end-if
                perform RESETTA-VIRGOLA

           |78-ID-ef-iva è l'ID del control ef-iva
           when 78-ID-ef-iva
                if tot-righe > 1
                   inquire ef-art,     value in brno-cod-articolo
                   inquire ef-consumo, value in brno-imp-consumo
                   inquire ef-coubat,  value in brno-imp-cou-cobat
                   inquire ef-add,     value in brno-add-piombo
                   inquire ef-prz,     value in brno-prz-unitario
                   inquire ef-qta,     value in brno-qta
                   inquire ef-iva,     value in ef-iva-buf
LUBEXX             if brno-prz-unitario = 0
LUBEXX                move tge-cod-iva-omag to ef-iva-buf
LUBEXX                display ef-iva
LUBEXX             end-if
                   if ef-iva-buf not = spaces
                      move ef-iva-buf to tbliv-codice2
                      move "IV"       to tbliv-codice1
                      read tivaese no lock
                           invalid
                           set errori to true
                           display message "Codice IVA NON valido"
                                   title = tit-err
                                   icon  2
                       not invalid
                           if brno-qta = 0
                              set errori        to true
                              move 78-ID-ef-qta to control-id
                           end-if
                      end-read
                   else
                      set errori           to true
                      if brno-qta = 0
                         move 78-ID-ef-qta to control-id
                      else
                         move 78-ID-ef-iva to control-id
                      end-if
                   end-if    
                   if tutto-ok and key-status = 13
                      perform ENTRY-TO-ROW
      *****                19/11/18:Richiesta di Walter. Posizionarsi sempre
      *****                sul tasto NUOVO per inserimento continuo da tastiera
      *****                if NotaNumero not = 0
      *****                   move 78-ID-ef-art to control-id
      *****                   move 4            to accept-control
      *****                end-if
                   end-if
                end-if

           end-evaluate.
           
           if errori
              perform CANCELLA-COLORE
              move control-id to store-id
              move 4          to accept-control
           end-if . 

      ***---
       DISPLAY-SCREEN.
           display form1 . 

      ***---
       FIND-ARTICOLO-ON-NOTA.
           move riga to store-riga.
           move 0 to num-articoli.
           move art-codice    to SaveArticolo.
           inquire gd-pag2, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire gd-pag2(riga, 2), cell-data in art-codice
              if SaveArticolo = art-codice
                 add 1 to num-articoli
                 evaluate true
                 when num-articoli = 1 
                      inquire gd-pag2(riga, 2), hidden-data HiddenKey
                      inquire gd-pag2(riga, 5), cell-data in col-prezzo
                      move HiddenKey to GiacenzaKey
                 when num-articoli > 1 
                      exit perform
                 end-evaluate
              end-if
           end-perform.
           move store-riga to riga . 

      ***---
       FIND-MORE-ARTICOLI-ON-PROGMAG.
           move link-tiponota to tca-codice.
           read tcaumag no lock invalid continue end-read.
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
                       if prg-tipo-imballo  not = spaces     and
                          prg-peso          not = 0          and
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
           end-start . 

      ***---
       INIT.
           set ArticoloSetFocus to false.
           set InsertRow        to false.
           set InsertRow2       to false.
           set PrimaVolta       to true.
           set ControllaCampi   to true.
           set CheckDestini     to true.
           move 0 to LastPrg.
           if inserimento
              if LinkNumero not = 0
                 move 22,00 to grid-line
                 move 15,54 to grid-lines
              else
                 move 31,30 to grid-lines
                 move  5,00 to grid-line
                 modify form1-gd-1 reset-grid = 1
                 perform FORM1-GD-1-CONTENT
                 perform INSERISCI-RIGA
              end-if
              modify form1-gd-1 line  grid-line
              modify form1-gd-1 lines grid-lines
           end-if.
           move spaces to HiddenKey.

           move 78-ID-ef-cli to control-id.
           move 4            to accept-control.
           move 1 to event-data-1 screen1-ta-1-tab-value.
           perform SCREEN1-TA-1-TABCHANGE.

           if KeyboardSaved
              set KeyboardReleased to true
              set environment "KEYSTROKE" to "DATA=44 44"
              set environment "KEYSTROKE" to "DATA=46 46"
           end-if . 

      ***---
       INITIALIZE-ENTRY.
           move 0 to ef-art-buf
                     ef-qta-buf
                     ef-prz-buf
                     ef-consumo-buf
                     ef-coubat-buf
                     ef-add-buf
                     ef-imp-buf.

           move spaces to lab-art-buf
                           ef-iva-buf
                          lab-iva-buf.

           perform DISPLAY-SCREEN.

           initialize HiddenValori replacing numeric data by zeroes
                                        alphanumeric data by spaces.

      ***---
       INSERISCI-RIGA.
           inquire form1-gd-1, last-row in tot-righe.
           inquire form1-gd-1, (tot-righe, 1), cell-data in LastPrg.
           add 1 to LastPrg.
           move LastPrg to col-riga.
           modify  form1-gd-1, insert-rows = 1.
           add 1 to tot-righe giving riga.
           modify form1-gd-1(riga, 1), cell-data = col-riga.
           move 1 to riga-nuova . 
           move "M" to hid-tipo-riga.
           set ControllaCampi to true.

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
       RESETTA-VIRGOLA.
           if KeyboardSaved
              set KeyboardReleased to true
      *       setto la tastiera originale
              set environment "KEYSTROKE" to "DATA=44 44"
              set environment "KEYSTROKE" to "DATA=46 46"
           end-if . 

      ***---
       RIEMPI-GRID-ADDEBITO.
           modify gd-pag2, reset-grid = 1.
           perform GD-PAG2-CONTENT.

           modify gd-pag2, mass-update = 1.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying riga from 2 by 1
                     until riga > tot-righe
              inquire form1-gd-1(riga, 1),  cell-data in col-riga
              inquire form1-gd-1(riga, 2),  cell-data in col-codice
              inquire form1-gd-1(riga, 3),  cell-data in col-descr
              inquire form1-gd-1(riga, 4),  cell-data in col-quantita
              inquire form1-gd-1(riga, 5),  cell-data in col-prezzo
              inquire form1-gd-1(riga, 6),  cell-data in col-imp-merce
              inquire form1-gd-1(riga, 7),  cell-data in col-consumo
              inquire form1-gd-1(riga, 8),  cell-data in col-coubat
              inquire form1-gd-1(riga, 9),  cell-data in col-add
              inquire form1-gd-1(riga, 10), cell-data in col-cod-iva

              inquire form1-gd-1(riga, 2), hidden-data in HiddenKey
              inquire form1-gd-1(riga, 3), hidden-data in HiddenPeso

              modify  gd-pag2(riga, 1),  cell-data col-riga
              modify  gd-pag2(riga, 2),  cell-data col-codice
              modify  gd-pag2(riga, 3),  cell-data col-descr
              modify  gd-pag2(riga, 4),  cell-data col-quantita
              modify  gd-pag2(riga, 5),  cell-data col-prezzo
              modify  gd-pag2(riga, 6),  cell-data col-imp-merce
              modify  gd-pag2(riga, 7),  cell-data col-consumo
              modify  gd-pag2(riga, 8),  cell-data col-coubat
              modify  gd-pag2(riga, 9),  cell-data col-add
              modify  gd-pag2(riga, 10), cell-data col-cod-iva
                                                             
              move 0 to hidden-sel
              modify gd-pag2(riga, 1), hidden-data hidden-sel
              modify gd-pag2(riga, 2), hidden-data HiddenKey
              modify gd-pag2(riga, 3), hidden-data HiddenPeso

           end-perform.
           modify gd-pag2, mass-update = 0 . 

      ***---
       RIEMPI-GRID-MERCE-ROTTA.
           modify gd-pag3, reset-grid = 1.
           perform GD-PAG3-CONTENT.

           modify gd-pag3, mass-update = 1.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying riga from 2 by 1
                     until riga > tot-righe
              inquire form1-gd-1(riga, 1),  cell-data in col-riga
              inquire form1-gd-1(riga, 2),  cell-data in col-codice
              inquire form1-gd-1(riga, 3),  cell-data in col-descr
              inquire form1-gd-1(riga, 4),  cell-data in col-quantita
              inquire form1-gd-1(riga, 5),  cell-data in col-prezzo
              inquire form1-gd-1(riga, 6),  cell-data in col-imp-merce
              inquire form1-gd-1(riga, 7),  cell-data in col-consumo
              inquire form1-gd-1(riga, 8),  cell-data in col-coubat
              inquire form1-gd-1(riga, 9),  cell-data in col-add
              inquire form1-gd-1(riga, 10), cell-data in col-cod-iva

              inquire form1-gd-1(riga, 2), hidden-data in HiddenKey
              inquire form1-gd-1(riga, 3), hidden-data in HiddenPeso

              modify  gd-pag3(riga, 1),  cell-data col-riga
              modify  gd-pag3(riga, 2),  cell-data col-codice
              modify  gd-pag3(riga, 3),  cell-data col-descr
              modify  gd-pag3(riga, 4),  cell-data col-quantita
              modify  gd-pag3(riga, 5),  cell-data col-prezzo
              modify  gd-pag3(riga, 6),  cell-data col-imp-merce
              modify  gd-pag3(riga, 7),  cell-data col-consumo
              modify  gd-pag3(riga, 8),  cell-data col-coubat
              modify  gd-pag3(riga, 9),  cell-data col-add
              modify  gd-pag3(riga, 10), cell-data col-cod-iva
                                                             
              move 0 to hidden-sel
              modify gd-pag3(riga, 1), hidden-data hidden-sel
              modify gd-pag3(riga, 2), hidden-data HiddenKey
              modify gd-pag3(riga, 3), hidden-data HiddenPeso

           end-perform.
           modify gd-pag3, mass-update = 0 . 

      ***---
       ROW-TO-ENTRY.
           inquire form1-gd-1(riga, 2), cell-data in col-codice.
           inquire form1-gd-1(riga, 3), cell-data in col-descr.
           inquire form1-gd-1(riga, 4), cell-data in col-quantita.
           inquire form1-gd-1(riga, 5), cell-data in col-prezzo.
           inquire form1-gd-1(riga, 6), cell-data in col-imp-merce.
           inquire form1-gd-1(riga, 7), cell-data in col-consumo.
           inquire form1-gd-1(riga, 8), cell-data in col-coubat.
           inquire form1-gd-1(riga, 9), cell-data in col-add.
           inquire form1-gd-1(riga, 10),cell-data in col-cod-iva.
           move col-codice    to ef-art-buf.
           move col-descr     to lab-art-buf.
           move col-quantita  to ef-qta-buf.
           move col-imp-merce to ef-imp-buf.
           move col-prezzo    to ef-prz-buf.
           move col-consumo   to ef-consumo-buf.
           move col-coubat    to ef-coubat-buf.
           move col-add       to ef-add-buf.
           move col-cod-iva   to ef-iva-buf.

           move "IV"          to tbliv-codice1.
           move col-cod-iva   to tbliv-codice2.
           read tivaese no lock
                invalid continue
            not invalid
                initialize lab-iva-buf
                inspect tbliv-descrizione1 replacing trailing 
                                           spaces by low-value
                string  tbliv-descrizione1 delimited low-value
                        " "                delimited size
                        tbliv-descrizione2 delimited size
                       into lab-iva-buf
                end-string
           end-read.

           display ef-art lab-art ef-qta ef-prz ef-iva
                   ef-consumo ef-coubat ef-add ef-imp lab-iva.

           inquire form1-gd-1(riga, 1), hidden-data in HiddenValori.
           inquire form1-gd-1(riga, 2), hidden-data in HiddenKey . 

      ***---
       SALVA.
           set tutto-ok to true.
      
           perform CHECK-PAGE-1.
      
           if tutto-ok
              perform CONTA-ZERI
              if not trovato
                 display message "Impossibile registrare la nota "
                                 "in quanto non sono presenti righe."
                         title = tit-err
                         icon mb-warning-icon
                 set errori to true
                 perform CANCELLA-COLORE
              end-if
           end-if.
      
           if errori
              move store-id to control-id
              move 4 to accept-control
           else
              perform CONTROLLA-TOTALE
              if errori
                 display message 
                  "Salvataggio NON effettuato: Il totale del documento "
            x"0d0a""non rispetta l'indicazione della causale utilizzata"
                           title tit-err
                            icon 2
              else
                 perform CONTROLLO-QUANTITA
                 if tutto-ok
                    perform SETTA-ADDEBITO-CORRETTO
                    if tutto-ok
                       if inserimento
                          perform VALORIZZA-NUMERO  
                       else
                          move NotaChiave to btno-chiave
                       end-if
                       if btno-numero = 0
                          display message "Bozza non caricata!!!"
                          x"0d0a""Errore interno. Contattare assistenza"
                                    title tit-err
                                     icon 2
                       else
                          perform FORM1-BUF-TO-FLD
                          if inserimento                     
                             move link-numero      to btno-numero
                             move link-TipoNota    to btno-causale 
                             move tor-num-fattura  to btno-num-fatt
                             move tor-anno-fattura to btno-anno-fatt 
                             move tor-data-fattura to btno-data-fatt
                          end-if
                          perform SCRIVI-RIGHE-N
                          perform SCRIVI-RIGHE-A
                          perform SCRIVI-RIGHE-R
                                  
                          unlock btnotacr all record

                          move btno-data-bolla(1:4) to btno-anno-bolla

                          write btno-rec
                                invalid rewrite btno-rec
                          end-write
                       end-if
                    end-if
                    if inserimento
                       move 27 to key-status
                    else
                       set vecchio to true
                       perform CARICA-GRID2
                       perform TORNA-IN-VISUA

                       if riga-nuova = 1
                          move 0 to riga-nuova
                          modify form1-gd-1,  
                                 record-to-delete = tot-righe + 1
                       end-if
                    end-if

                 end-if
              end-if
           end-if.
                    
           perform DISPLAY-SCREEN. 

      ***---
       SCRIVI-RIGHE-A.
           if modifica
              perform CANCELLA-RIGHE2
           end-if.

LUBEXX     set EsisteIVA to false.
           inquire form1-gd-2, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire form1-gd-2(riga, 4), cell-data in col-quantita
              move col-quantita to brno-qta
              if brno-qta > 0
                 initialize brno-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move    btno-chiave      to brno-chiave
                 move    btno-dati-comuni to brno-dati-comuni
                 inquire form1-gd-2(riga, 1), cell-data in col-riga
                 inquire form1-gd-2(riga, 2), cell-data in col-codice
                 inquire form1-gd-2(riga, 3), cell-data in col-descr
                 inquire form1-gd-2(riga, 4), cell-data in col-quantita
                 inquire form1-gd-2(riga, 5), cell-data in col-prezzo
                 inquire form1-gd-2(riga, 6), cell-data in col-imp-merce  
                 inquire form1-gd-2(riga, 7), cell-data in col-consumo
                 inquire form1-gd-2(riga, 8), cell-data in col-coubat
                 inquire form1-gd-2(riga, 9), cell-data in col-add
                 inquire form1-gd-2(riga, 10),cell-data in col-cod-iva

                 inquire form1-gd-2(riga, 1), hidden-data HiddenValori
                 inquire form1-gd-2(riga, 2), hidden-data HiddenKey
                 move col-riga      to brno-num-riga
                 move col-quantita  to brno-qta
                 move col-imp-merce to brno-imponib-merce
                 move col-prezzo    to brno-prz-unitario
                 move col-consumo   to brno-imp-consumo
                 move col-coubat    to brno-imp-cou-cobat
                 move col-add       to brno-add-piombo
                 move col-cod-iva   to brno-cod-iva

LUBEXX           if not EsisteIVA
LUBEXX              perform CONTROLLA-PERCENTUALE-IVA
LUBEXX           end-if
 
                 move col-codice to brno-cod-articolo

                 move HiddenKey        to brno-prg-chiave prg-chiave

                 read progmag no lock 
                      invalid 
                      display message "ARTICOLO " prg-cod-articolo
                               x"0d0a""RIGA " brno-num-riga
                               x"0d0a""DATI INCOERENTI!!!"
                               x"0d0a""CANCELLARE E REINSERIRE LA RIGA"
                                title tit-err
                                 icon 2
                  not invalid
                      move prg-peso-utf     to brno-peso-utf
                      move prg-peso-non-utf to brno-peso-non-utf
                 end-read
              
                 accept como-ora from time
                 move data-oggi  to brno-data-ultima-modifica
                 move como-ora   to brno-ora-ultima-modifica
                 move user-codi  to brno-utente-ultima-modifica

                 set brno-addebito to true         

                 write brno-rec invalid rewrite brno-rec end-write
              end-if

           end-perform . 

      ***---
       SCRIVI-RIGHE-N.
           if modifica
              perform CANCELLA-RIGHE
           end-if.

LUBEXX     set EsisteIVA to false.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              initialize brno-rec replacing numeric data by zeroes
                                      alphanumeric data by spaces
              move    btno-chiave      to brno-chiave
              move    btno-dati-comuni to brno-dati-comuni
              inquire form1-gd-1(riga, 1), cell-data in col-riga
              inquire form1-gd-1(riga, 2), cell-data in col-codice
              inquire form1-gd-1(riga, 3), cell-data in col-descr
              inquire form1-gd-1(riga, 4), cell-data in col-quantita
              inquire form1-gd-1(riga, 5), cell-data in col-prezzo
              inquire form1-gd-1(riga, 6), cell-data in col-imp-merce
              inquire form1-gd-1(riga, 7), cell-data in col-consumo
              inquire form1-gd-1(riga, 8), cell-data in col-coubat
              inquire form1-gd-1(riga, 9), cell-data in col-add
              inquire form1-gd-1(riga, 10), cell-data in col-cod-iva

              inquire form1-gd-1(riga, 1), hidden-data in HiddenValori
              inquire form1-gd-1(riga, 2), hidden-data in HiddenKey
              move col-riga      to brno-num-riga
              move col-quantita  to brno-qta
              move col-imp-merce to brno-imponib-merce
              move col-prezzo    to brno-prz-unitario
              move col-consumo   to brno-imp-consumo
              move col-coubat    to brno-imp-cou-cobat
              move col-add       to brno-add-piombo
              move col-cod-iva   to brno-cod-iva

LUBEXX        if not EsisteIVA
LUBEXX           perform CONTROLLA-PERCENTUALE-IVA
LUBEXX        end-if
 
              move col-codice to brno-cod-articolo

              move HiddenKey        to brno-prg-chiave prg-chiave

              read progmag no lock 
                   invalid 
                   display message "ARTICOLO " prg-cod-articolo
                            x"0d0a""RIGA " brno-num-riga
                            x"0d0a""DATI INCOERENTI!!!"
                            x"0d0a""CANCELLARE E REINSERIRE LA RIGA"
                             title tit-err
                              icon 2
               not invalid
                   move prg-peso-utf     to brno-peso-utf
                   move prg-peso-non-utf to brno-peso-non-utf
              end-read
              
              accept como-ora from time
              move data-oggi  to brno-data-ultima-modifica
              move como-ora   to brno-ora-ultima-modifica
              move user-codi  to brno-utente-ultima-modifica

              set brno-nota to true
                         
              move Link-TipoNota to tca-codice
              read tcaumag no lock invalid continue end-read
              if tca-tipo-nota-prz
                 set brno-manuale to true
              else
                 move hid-tipo-riga to brno-tipo-riga
              end-if

              write brno-rec invalid rewrite brno-rec end-write

           end-perform . 

      ***---
       SCRIVI-RIGHE-R.
           if modifica
              perform CANCELLA-RIGHE3
           end-if.

LUBEXX     set EsisteIVA to false.
           inquire form1-gd-3, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire form1-gd-3(riga, 4), cell-data in col-quantita
              move col-quantita to brno-qta
              if brno-qta > 0
                 initialize brno-rec replacing numeric data by zeroes
                                         alphanumeric data by spaces
                 move    btno-chiave      to brno-chiave
                 move    btno-dati-comuni to brno-dati-comuni
                 inquire form1-gd-3(riga, 1), cell-data in col-riga
                 inquire form1-gd-3(riga, 2), cell-data in col-codice
                 inquire form1-gd-3(riga, 3), cell-data in col-descr
                 inquire form1-gd-3(riga, 4), cell-data in col-quantita
                 inquire form1-gd-3(riga, 5), cell-data in col-prezzo
                 inquire form1-gd-3(riga, 6), cell-data in col-imp-merce  
                 inquire form1-gd-3(riga, 7), cell-data in col-consumo
                 inquire form1-gd-3(riga, 8), cell-data in col-coubat
                 inquire form1-gd-3(riga, 9), cell-data in col-add
                 inquire form1-gd-3(riga, 10),cell-data in col-cod-iva

                 inquire form1-gd-3(riga, 1), hidden-data HiddenValori
                 inquire form1-gd-3(riga, 2), hidden-data HiddenKey
                 move col-riga      to brno-num-riga
                 move col-quantita  to brno-qta
                 move col-imp-merce to brno-imponib-merce
                 move col-prezzo    to brno-prz-unitario
                 move col-consumo   to brno-imp-consumo
                 move col-coubat    to brno-imp-cou-cobat
                 move col-add       to brno-add-piombo
                 move col-cod-iva   to brno-cod-iva

LUBEXX           if not EsisteIVA
LUBEXX              perform CONTROLLA-PERCENTUALE-IVA
LUBEXX           end-if
 
                 move col-codice to brno-cod-articolo

                 move HiddenKey  to brno-prg-chiave prg-chiave

                 read progmag no lock 
                      invalid 
                      display message "ARTICOLO " prg-cod-articolo
                               x"0d0a""RIGA " brno-num-riga
                               x"0d0a""DATI INCOERENTI!!!"
                               x"0d0a""CANCELLARE E REINSERIRE LA RIGA"
                                title tit-err
                                 icon 2
                  not invalid
                      move prg-peso-utf     to brno-peso-utf
                      move prg-peso-non-utf to brno-peso-non-utf
                 end-read
              
                 accept como-ora from time
                 move data-oggi  to brno-data-ultima-modifica
                 move como-ora   to brno-ora-ultima-modifica
                 move user-codi  to brno-utente-ultima-modifica

                 set brno-merce-rotta to true         

                 write brno-rec invalid rewrite brno-rec end-write
              end-if

           end-perform.

      ***---
       SELEZIONA-CLIENTE.
           set tutto-ok to true.
           move spaces to lab-cli-buf.
           move spaces to lab-ind-buf.
           move spaces to lab-loca-buf.

           if cli-codice not > 0
              set errori to true
              display message msg-codice-obbligatorio
                        title tit-err
                         icon 2
           else          
              set cli-tipo-c to true
              read clienti no lock
                   invalid
                   display message "Codice cliente NON valido"
                             title tit-err
                              icon 2
                   set errori to true
               not invalid      
                   if cli-attivo
                      move 0 to cli-no-bloc
                   else
                      if cli-fuori-fido |and cli-fido-extra not = 0
                         |23/05/2012
                         or cli-prob-pag
                         |23/05/2012
                         continue
                      else
      *****                   if cli-fuori-fido
      *****                      initialize calfido-linkage 
      *****                                  sitfin-linkage
      *****                               replacing numeric data by zeroes
      *****                                    alphanumeric data by spaces
      *****                      move ef-cli-buf   to link-cli-codice
      *****                      call "C$JUSTIFY"  using 
      *****                                        link-cli-codice, "R"
      *****                      inspect link-cli-codice 
      *****                              replacing leading x"20" by x"30"
      *****                      call   "sitfin"  using sitfin-linkage
      *****                                            calfido-linkage
      *****                      cancel "sitfin"
      *****                   end-if
      *****                   if cli-no-angraf or cli-nuovo-ragsoc
      *****                      move 11 to Passwd-password
      *****                      call   "passwd" using Passwd-linkage
      *****                      cancel "passwd"
      *****
      *****                      if not Passwd-StatusOk
      *****                         set errori to true
      *****                      end-if
      *****                   else
                            if cli-no-bloc not = cli-codice
                               display message 
                                       "Cliente NON attivo. Confermi?"
                                         title tit-err
                                          icon 2      
                                          type mb-yes-no
                                        giving scelta
                                       default mb-no
                               set errori to true
                               if scelta = mb-yes
                                  move 11 to Passwd-password
                                  call   "passwd" using Passwd-linkage
                                  cancel "passwd"
                               
                                  if Passwd-StatusOk
                                     move cli-codice to cli-no-bloc
                                     set tutto-ok to true
                                  end-if
                               end-if
                            end-if
      *****                   end-if
                      end-if
                   end-if

                   if tutto-ok
                      if cli-codice-sdi = spaces and cli-pec = spaces
                         display message "Valorizzare PEC o SDI"
                                   title tit-err
                                    icon 2
                         set errori to true
                      end-if
                   end-if

                   move cli-tipo to tcl-codice
                   read ttipocli no lock invalid continue end-read
              end-read
           end-if  . 

      ***---
       SELEZIONA-DESTINO.
           move spaces to lab-des-buf.
           move spaces to lab-ind-d-buf.
           move spaces to lab-loca-d-buf.
           if des-codice not > 0
              set errori to true
              display message box msg-codice-obbligatorio
                      title tit-err
                      icon  mb-warning-icon
           else
              read destini no lock
                   invalid
                   display message "Progressivo destino NON valido"
                             title tit-err
                              icon 2
                   set errori to true
                   initialize des-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces
              end-read
           end-if.

      ***---
       SELEZIONA-RIGA-PAG-2.
           inquire gd-pag2, last-row in tot-righe.
           move event-data-2 to riga.

           if riga >= 2     and
              riga <= tot-righe
              inquire gd-pag2(riga, 1) hidden-data in hidden-sel
              inquire gd-pag2(riga, 5)   cell-data in brno-prz-unitario
              if hidden-sel = 1
                 move 0 to hidden-sel
                 modify gd-pag2(riga), row-color = 516
              else
                 move 1 to hidden-sel
                 modify gd-pag2(riga), row-color = 176
              end-if
              modify gd-pag2(riga, 1), hidden-data = hidden-sel
           end-if. 

      ***---
       SELEZIONA-RIGA-PAG-3.
           inquire gd-pag3, last-row in tot-righe.
           move event-data-2 to riga.

           if riga >= 2     and
              riga <= tot-righe
              inquire gd-pag3(riga, 1) hidden-data in hidden-sel
              inquire gd-pag3(riga, 5)   cell-data in brno-prz-unitario
              if hidden-sel = 1
                 move 0 to hidden-sel
                 modify gd-pag3(riga), row-color = 516
              else
                 move 1 to hidden-sel
                 modify gd-pag3(riga), row-color = 176
              end-if
              modify gd-pag3(riga, 1), hidden-data = hidden-sel
           end-if. 

      ***---
       SETTA-VIRGOLA.
           if KeyboardReleased
              set KeyboardSaved to true
      *       sostituisco il punto come virgola
              set environment "KEYSTROKE" to "DATA=44 46"
           end-if . 

      ***---
       SPOSTAMENTO.
           if mod-campi = 1
              move 5 to NumBitmapNuovoGrid
              move 4 to NumBitmapEliminaGrid
           else
              move 8 to NumBitmapNuovoGrid
              move 7 to NumBitmapEliminaGrid
           end-if.
           modify pb-nuovo,   bitmap-number = NumBitmapNuovoGrid.
           modify pb-elimina, bitmap-number = NumBitmapEliminaGrid.

           set tutto-ok to true.
           inquire form1-gd-1, last-row in tot-righe.
           move event-data-2 to riga.

           if riga < 2 move 2 to riga end-if.

           if riga > tot-righe
              if riga-nuova = 0
                 move tot-righe to riga
              else
                 move 78-ID-ef-art to control-id
                 move 4            to accept-control
                 set errori to true
              end-if
           else
              if riga-nuova = 1
                 move 0 to riga-nuova
                 modify form1-gd-1,  record-to-delete = tot-righe + 1
                 subtract 1 from LastPrg
              end-if
           end-if.

           modify form1-gd-1, start-y = riga, start-x =  2,
                                    y = riga,       x = 12,
                         region-color = 144.

           if tutto-ok
              perform ROW-TO-ENTRY
              set ArticoloSetFocus to true
              set Fromspostamento  to true
           end-if.
      *****     set event-action   to event-action-terminate.
           modify form1-gd-1, cursor-y = riga . 

      ***---
       SPOSTAMENTO2.
           set tutto-ok to true.
           inquire form1-gd-2, last-row in tot-righe.
           move  event-data-2 to riga.

           if riga < 2 move 2 to riga end-if.

           if riga > tot-righe
              if riga-nuova2 = 0
                 move tot-righe to riga
              else
                 set errori to true
              end-if
           else
              if riga-nuova2 = 1
                 move 0 to riga-nuova2
                 modify form1-gd-2,  record-to-delete = tot-righe + 1
                 subtract 1 from LastPrg
              end-if
           end-if.

           modify form1-gd-2, start-y = riga, start-x =  2,
                                    y = riga,       x = 12,
                         region-color = 144.

           if tutto-ok
              set Fromspostamento  to true
           end-if.
                                                        
           modify form1-gd-2, cursor-y = riga.
      *****     set event-action   to event-action-fail. 

      ***---
       SPOSTAMENTO-GD-PAG-2.
           inquire gd-pag2, last-row in tot-righe.
           move event-data-2 to riga. 

      ***---
       SPOSTAMENTO-GD-PAG-3.
           inquire gd-pag3, last-row in tot-righe.
           move event-data-2 to riga. 

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
           modify tool-cerca, bitmap-number = BitmapNumZoom . 

      ***---
       TROVA-DESTINO.
           set trovato to false.

           move low-value  to des-prog.
           start destini  key is >= des-chiave
                 invalid  continue
             not invalid  read destini next
                 if des-codice = cli-codice 
                    set trovato to true 
                 else 
                    move ef-cli-buf to des-codice
                    move ef-des-buf to des-prog
                 end-if
           end-start.

           if not trovato initialize des-rec end-if . 

      ***---
       VALORIZZA-CELLE-CAMPI-MANUALE.
      *****     move prg-peso-utf      to hid-utf.
      *****     move prg-peso-non-utf  to hid-non-utf.
      *****     move "N"               to hid-omaggio.

           move HiddenKey to prg-chiave.
           read progmag no lock 
                invalid display message "DATI INCOERENTI"
           end-read.
                    
           move Link-TipoNota to tca-codice.
           read tcaumag no lock invalid continue end-read.
           if tca-tipo-nota-reso
              if hid-tipo-riga = "F"
                 inquire ef-prz,     value in brno-prz-unitario
                 inquire ef-coubat,  value in brno-imp-cou-cobat
                 inquire ef-consumo, value in brno-imp-consumo
                 inquire ef-add,     value in brno-add-piombo
              else
                 perform CALCOLA-IMPOSTE
              end-if
           end-if.
           perform CALCOLA-IMPONIBILE.
           perform RECUPERA-IVA.
           display form1.

      ***---
       CALCOLA-IMPOSTE.
           move 0 to brno-imp-consumo brno-imp-cou-cobat brno-add-piombo
           move 0 to ef-consumo-buf col-cons.
           set lab-imp-coubat to true.

           if des-nazione = spaces
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

           move lab-anno-nc-buf to con-anno.
           read tcontat no lock.
           evaluate tcl-serie-bolle
           when 1 move con-ult-stampa-bolle-gdo to imp-data
           when 2 move con-ult-stampa-bolle-mv  to imp-data
           when 3 move con-ult-stampa-bolle-at  to imp-data
           end-evaluate.

           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

      ***---
       CALCOLO-IMPOSTE-ESTERO.
           move 0 to brno-imp-consumo brno-imp-cou-cobat brno-add-piombo
           move 0 to col-coubat  ef-coubat-buf  imposta-cou.
           move 0 to col-consumo ef-consumo-buf.
           move 0 to col-add     ef-add-buf add-piombo.

      ***---
       CALCOLO-IMPOSTE-STANDARD.
           move 0 to col-add ef-add-buf add-piombo.
           move 0 to col-cou ef-coubat-buf imposta-cou.
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
                 move como-imposta      to brno-imp-consumo
                 move brno-imp-consumo   to ef-consumo-buf col-cons
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
                 move como-imposta      to brno-imp-cou-cobat 
           imposta-cou
                 move brno-imp-cou-cobat to ef-coubat-buf col-cou
                 set lab-imp-cou to true
              else
                 move 0 to imposta-cou
              end-if

           else
              move 0 to brno-imp-consumo ef-consumo-buf col-cons
      
           end-if.
      *     
           if art-si-cobat
              set lab-imp-cobat to true
              perform CALCOLA-COBAT
           end-if.

      ***---
       CALCOLO-IMPOSTE-GDO.
           move 0 to col-add ef-add-buf add-piombo.
           move 0 to col-cou ef-coubat-buf imposta-cou.
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
              move como-imposta      to brno-imp-consumo
              move brno-imp-consumo   to ef-consumo-buf col-cons
           else
              move 0 to brno-imp-consumo ef-consumo-buf col-cons
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
           move como-imposta      to brno-imp-cou-cobat imposta-cou.
           move brno-imp-cou-cobat to ef-coubat-buf col-cou.

           set lab-imp-cou to true.
      *     
           if art-si-cobat
              set lab-imp-cobat to true
              perform CALCOLA-COBAT
              if tcl-si-piombo
                 perform CALCOLA-ADD-PIOMBO
              end-if
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
              move imposta-cobat     to ef-coubat-buf col-cou 
                                        brno-imp-cou-cobat

              if imposta-cou = 0
                 set lab-imp-cobat  to true
              else
                 set lab-imp-coubat to true
              end-if

           end-if.

      ***---
       CALCOLA-ADD-PIOMBO. 
           move brno-prz-unitario   to como-prz-unitario.
           move brno-imp-cou-cobat  to como-imp-cou-cobat.

           move imposta-cobat      to brno-imp-cou-cobat.
           move art-marca-prodotto to tpb-marca.
           accept como-data-ordine from century-date.
           move como-data-ordine to tpb-data.
           move ef-cli-buf to como-prm-cliente.
           move ef-des-buf to como-prm-destino.
           perform ADDIZIONALE-PIOMBO.
           move add-piombo to brno-add-piombo ef-add-buf col-add.

      ***---
       CALCOLA-IMPONIBILE. 
           move Link-TipoNota to tca-codice.
           read tcaumag no lock invalid continue end-read.
           evaluate true
           when tca-tipo-nota-prz
              inquire ef-prz,     value in brno-prz-unitario
              inquire ef-coubat,  value in brno-imp-cou-cobat
              inquire ef-consumo, value in brno-imp-consumo
              inquire ef-add,     value in brno-add-piombo
              if ( brno-imp-cou-cobat +
                   brno-imp-consumo   +
                   brno-add-piombo )  >=
                   brno-prz-unitario
                   set errori to true
                   display message 
                 "Il prezzo dev'essere > delle imposte. Rettificare!!!"
                             title tit-err
                              icon 2
                   move 78-ID-ef-prz to control-id
              end-if       
           end-evaluate.
           evaluate true
           when ttipocli-gdo
                compute brno-imponib-merce = 
                        brno-prz-unitario  - 
                        brno-imp-cou-cobat - 
                        brno-imp-consumo   -
                        brno-add-piombo
           when other
                if brno-prz-unitario = 9999999,99
                   move 9999999,99 to brno-imponib-merce
                else
                   compute brno-imponib-merce = 
                           brno-prz-unitario  
                end-if
           end-evaluate.
           move brno-imponib-merce to ef-imp-buf col-imp.

      ***---
       RECUPERA-IVA.
           if cli-iva-ese not = spaces
              move cli-iva-ese to ef-iva-buf 
                                  tbliv-codice2
                                  col-iva
           else
              move art-codice-iva to ef-iva-buf 
                                     tbliv-codice2
                                     col-iva
           end-if.
           move "IV"   to tbliv-codice1.
           move spaces to tbliv-descrizione1
                          tbliv-descrizione2.
           read tivaese no lock invalid continue end-read.           
           initialize lab-iva-buf.
           inspect tbliv-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tbliv-descrizione1 delimited by low-value
                   " "                delimited by size
                   tbliv-descrizione2 delimited by size
                   into lab-iva-buf
           end-string . 

      ***---
       VALORIZZA-OLD.
           move btno-rec to old-btno-rec.
           set old-btno-attivo to true . 

      ***---
       VALORIZZA-NUMERO.
           initialize link-nambar.
           move lab-anno-NC-buf to link-anno btno-anno.

           set  link-bnotacr  to true.
           set  link-crea     to true.

           call   "nambar" using link-nambar.
           cancel "nambar".
           
           if link-status-nambar = -1 set errori       to true
           else                       move link-numero to btno-numero
           end-if . 
           move link-TipoNota to btno-causale.

      ***---
       ZOOM-SU-FATTURATI.
           perform COMPONI-TMP-FATTURATI.
           if trovato
              move SaveRec               to tmp-f-rec
              move path-tmp-fatturati    to ext-file
              move "tmp-fatturati"       to como-file
              call "zoom-gt"          using como-file, tmp-f-rec
                                     giving stato-zoom
              end-call
              cancel "zoom-gt"
           else
              display message "Nessun ritiro per questo articolo"              
                        title titolo
                         icon 1
              move 1 to stato-zoom
           end-if.
           delete file tmp-fatturati.
           if stato-zoom = 0
              move tmp-f-uni  to ef-prz-buf
              move tmp-f-cons to ef-consumo-buf
              move tmp-f-cou  to ef-coubat-buf
              move tmp-f-add  to ef-add-buf
              move tmp-f-imp  to ef-imp-buf

              move "IV"       to tbliv-codice1
              move tmp-f-iva  to tbliv-codice2
              read tivaese no lock 
                   invalid continue
              end-read
              move tbliv-descrizione1 to lab-iva-buf
              move "F" to hid-tipo-riga
              display form1
              perform ENTRY-TO-ROW
              perform PB-NUOVO-LINKTO
              set tutto-ok to true
           else
              set errori to true
           end-if.
           |Altrimenti rimane "sporco" il numero di fattura
           move save-tor-rec to tor-rec.

      ***---
       COMPONI-TMP-FATTURATI.
           move 0 to tmp-f-id SavePrezzo.
           call "W$MOUSE" using set-mouse-shape, wait-pointer.
           inquire ef-art, value in SaveArticolo.
           accept  path-tmp-fatturati    from environment "PATH_ST".
           accept  como-data             from century-date.
           accept  como-ora              from time.
           inspect path-tmp-fatturati    replacing trailing
                                         spaces by low-value.
           string path-tmp-fatturati     delimited by low-value
                  "tmp-fatturati"        delimited by size
                  "_"                    delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  into path-tmp-fatturati
           end-string.

           if SavePrezzo = 0
              move 999999999 to SavePrezzo
           end-if.
           move 0         to SaveData.
           set trovato    to false.
           move 0         to tmp-f-id
           open output tmp-fatturati.
           move SaveArticolo to ror-cod-articolo.
           move low-value    to ror-chiave.
           move como-data(1:4) to como-anno.
           subtract 2 from como-anno giving ror-anno.

           |23/05/2012
           inquire ef-data-bolla, value in ef-data-bolla-buf.
           move ef-data-bolla-buf(7:4) to como-data-bolla(1:4).
           move ef-data-bolla-buf(4:2) to como-data-bolla(5:2).
           move ef-data-bolla-buf(1:2) to como-data-bolla(7:2).
           |23/05/2012

           if como-data-bolla = 0
              move como-data to como-data-bolla
           end-if.

           start rordini key >= ror-k-articolo
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-cod-articolo not = SaveArticolo
                       exit perform
                    end-if
                    if ror-prz-unitario not = 0
                       move ror-chiave to tor-chiave
                       read tordini no lock  
                       if tor-cod-cli = btno-cod-cli  and
                          tor-num-fattura  > 0        and
                          tor-data-fattura > 0        

                          |23/05/2012
                          and tor-data-bolla <= como-data-bolla
                          |23/05/2012
                          if btno-prg-destino = tor-prg-destino or
                             btno-prg-destino = 0
                             set trovato to true
                             add 1 to tmp-f-id
                             move tor-data-fattura  to tmp-f-data
                             move tor-num-fattura   to tmp-f-num
                             move ror-prz-unitario  to tmp-f-uni
                             move ror-imponib-merce to tmp-f-imp
                             move ror-add-piombo    to tmp-f-add
                             move ror-imp-consumo   to tmp-f-cons
                             move ror-imp-cou-cobat to tmp-f-cou
                             move ror-cod-iva       to tmp-f-iva
                             write tmp-f-rec
                             if tmp-f-uni < SavePrezzo and 
                                SavePrezzo not = 0
                                move tmp-f-uni to SavePrezzo
                                move tmp-f-rec to SaveRec
                             end-if
      *****                       if tmp-f-data > SaveData
      *****                          move tmp-f-data to SaveData
      *****                          move tmp-f-rec  to SaveRec
      *****                       end-if
                          end-if
                       end-if
                    end-if
                end-perform
           end-start.

           close tmp-fatturati.
           call "W$MOUSE" using set-mouse-shape, arrow-pointer.
              
      ***---
       AFTER-BUF-TO-FLD.
           if inserimento
              move lab-anno-nc-buf to btno-anno
              accept btno-data           from century-date
              accept btno-ora-creazione  from time
              accept btno-data-creazione from century-date
              move user-codi            to btno-utente-creazione
                                    
              initialize btno-rif-nc btno-rif-fm
                         replacing numeric data by zeroes
                               alphanumeric data by spaces
           else
              move NotaChiave to btno-chiave
              accept btno-ora-modifica  from time
              accept btno-data-modifica from century-date
              move user-codi            to btno-utente-modifica
           end-if.

           perform SCARICA-COMBO-COLPA.
           move errore-colpa to btno-errore-colpa.
                      
           move ef-cli-buf to btno-cod-cli. 
           move ef-des-buf to btno-prg-destino.
           
           move ef-data-bolla-buf to como-data.
           perform DATE-TO-FILE.
           move como-data       to btno-data-bolla.   

           move ef-data-ingresso-buf to como-data
           perform DATE-TO-FILE.
           move como-data to btno-data-ingresso.
      
      ***---
       AFTER-FLD-TO-BUF.
           inspect ef-cli-buf replacing leading x"30" by x"20".
           inspect ef-des-buf replacing leading x"30" by x"20".

           move btno-data-nc to como-data.
           perform DATE-TO-SCREEN.
           move como-data    to lab-data-nc-buf.   

           move btno-data-ingresso to como-data
           perform DATE-TO-SCREEN.
           move como-data to ef-data-ingresso-buf.
           
           move btno-data-bolla to como-data.
           perform DATE-TO-SCREEN.
           move como-data       to ef-data-bolla-buf.

           move btno-errore-colpa to errore-colpa.
           perform CARICA-COMBO-COLPA.

           set  cli-tipo-C  to true.
           move btno-cod-cli to cli-codice.
           read clienti no lock 
                invalid continue
            not invalid move cli-ragsoc-1  to lab-cli-buf
                        move cli-indirizzo to lab-ind-buf
                        move cli-localita  to lab-loca-buf
           end-read.

           if btno-prg-destino not = 0
              move btno-cod-cli     to des-codice
              move btno-prg-destino to des-prog
              read destini no lock 
                   invalid continue
               not invalid move des-ragsoc-1  to lab-des-buf
                           move des-indirizzo to lab-ind-d-buf
                           move des-localita  to lab-loca-d-buf
              end-read
           end-if.

           if btno-vettore not = 0
              move btno-vettore to vet-codice
              read tvettori no lock
                   invalid continue
               not invalid move vet-descrizione to lab-vet-buf
              end-read
           end-if.

      ***---
       BEFORE-ACCEPT.
           move link-tiponota to tca-codice.
           read tcaumag no lock invalid continue end-read.

           if modifica
              move 8 to NumBitmapNuovoGrid
              move 7 to NumBitmapEliminaGrid 
           else
              move 5 to NumBitmapNuovoGrid
              move 4 to NumBitmapEliminaGrid 
           end-if.
           modify pb-nuovo,   bitmap-number = NumBitmapNuovoGrid.
           modify pb-elimina, bitmap-number = NumBitmapEliminaGrid.
    
           accept data-oggi from century-date
           perform INIT
           move  43,50    to EF-LinesWithout
           perform RIEMPI-COMBO-COLPA
           move "Corriere"       to cbo-colpa-buf
           modify  cbo-colpa, value cbo-colpa-buf
           perform ABILITAZIONI
           move spaces        to tge-chiave
           read tparamge invalid continue end-read
           move 0  to v-fatt
           initialize tor-rec
           if LinkNumero not = 0 and LinkAnno not = 0
              move LinkAnno   to tor-anno-fattura
              move LinkNumero to tor-num-fattura
              read tordini no lock key k-fattura
                   invalid continue
              end-read
              move tor-rec to save-tor-rec
              move 1 to v-fatt
           end-if.
           perform CURRENT-RECORD.
           move 1 to pagina.

           perform DISPLAY-SCREEN.

           move 78-ID-ef-cli to control-id.
           move 4            to accept-control.

           string "Es. "       delimited size
                  esercizio    delimited size
                  " ("         delimited size
                  esercizio-G2 delimited size
                  ")"          delimited size
                  into lab-anno-buf
           end-string.


           display lab-anno.

      ***---
       AFTER-ENDACCEPT.
           if NonCambiareTab
              set NonCambiareTab to false
              move pagina to screen1-ta-1-tab-value event-data-1
              perform SCREEN1-TA-1-TABCHANGE
              move store-id to control-id
              move 4 to accept-control
           end-if

           if ArticoloSetFocus
              if not FromSpostamento
                 perform CANCELLA-COLORE
                 evaluate pagina
                 when 2
                      move 78-ID-ef-art      to control-id
                      move 4                 to accept-control
                 end-evaluate
              end-if
              set ArticoloSetFocus to false
           end-if

           if InsertRow
              move 0         to LastPrg
              set  InsertRow to false
              |aggiungo una riga simulando
              |pressione del tasto "nuovo"
              perform PB-NUOVO-LINKTO
           end-if

           if InsertRow2
              move 0          to LastPrg
              set  InsertRow2 to false
           end-if.
      
      ***---
       FORM1-GD-2-BEGIN-ENTRY.
           if event-data-1 = 1 or = 2 or = 3 or = 6
              set event-action to event-action-fail
           else
              if event-data-1 = 5 or = 7 or = 8 or = 9
                 perform SETTA-VIRGOLA
              end-if
           end-if.
      
      ***---
       FORM1-GD-2-FINISH-ENTRY.
           inquire form1-gd-2(event-data-2, 1),
                   hidden-data = HiddenValori.
           evaluate event-data-1
           when 4
                inquire form1-gd-2(event-data-2, event-data-1), 
                        cell-data in col-quantita
                move col-quantita to como-numero
      *****          if como-numero > hid-qta-nc
      *****             move hid-qta-nc  to col-quantita
      *****             move como-numero to col-qta
      *****             display message
      *****            "La quantità degli articoli addebitati non può essere"
      *****     X"0d0a""maggiore di quella degli articoli accreditati"
      *****     X"0D0A""Quantità accreditata: " col-quantita
      *****     X"0D0A""Quantità addebitata: "  col-qta
      *****                          title tit-err
      *****                           icon 2
      *****             set errori to true
      *****             set event-action to event-action-fail
      *****          end-if
                if como-numero not = hid-old-qta
                   set RigaCambiata to true
                end-if

           when 5
                inquire form1-gd-2(event-data-2, event-data-1), 
                        cell-data in col-prezzo
                move col-prezzo to ror-prz-unitario
                if ror-prz-unitario not = hid-old-prz
                   perform CALCOLA-IMPOSTE-ADDEBITO
                   set RigaCambiata to true
                end-if
                modify form1-gd-2(event-data-2, event-data-1), 
                       cell-data col-prezzo
                if ror-prz-unitario = 0
                   move tge-cod-iva-omag to col-iva
                   move 0 to col-imp-merce
                   move 0 to col-consumo
                   move 0 to col-coubat
                   move 0 to col-add
                   modify form1-gd-2(event-data-2, 6), 
                          cell-data col-imp-merce
                   modify form1-gd-2(event-data-2, 7), 
                          cell-data col-consumo
                   modify form1-gd-2(event-data-2, 8), 
                          cell-data col-coubat
                   modify form1-gd-2(event-data-2, 9), 
                          cell-data col-add
                else
                   move spaces to tge-codice
                   read tparamge no lock
                   move tge-cod-iva-std to col-iva
                end-if
                modify form1-gd-2(event-data-2, 10), 
                       cell-data col-iva
           when 7
                inquire form1-gd-2(event-data-2, event-data-1), 
                        cell-data in col-consumo
                move col-consumo to ror-imp-consumo
                if ror-imp-consumo not = hid-old-consumo
                   perform CALCOLA-IMPOSTE-ADDEBITO
                   set RigaCambiata to true
                end-if
                modify form1-gd-2(event-data-2, event-data-1), 
                       cell-data col-consumo
           when 8
                inquire form1-gd-2(event-data-2, event-data-1), 
                        cell-data in col-coubat
                move col-coubat to ror-imp-cou-cobat
                if ror-imp-cou-cobat not = hid-old-coubat
                   perform CALCOLA-IMPOSTE-ADDEBITO
                   set RigaCambiata to true
                end-if
                modify form1-gd-2(event-data-2, event-data-1), 
                       cell-data col-coubat
           when 9
                inquire form1-gd-2(event-data-2, event-data-1), 
                        cell-data in col-add
                move col-add to ror-add-piombo
                if ror-add-piombo not = hid-old-piombo
                   perform CALCOLA-IMPOSTE-ADDEBITO
                   set RigaCambiata to true
                end-if
                modify form1-gd-2(event-data-2, event-data-1), 
                       cell-data col-add
           when 10
                inquire form1-gd-2(event-data-2, event-data-1), 
                        cell-data in col-iva
                move "IV"    to tbliv-codice1
                move col-iva to tbliv-codice2
                read tivaese no lock
                     invalid
                     set event-action to event-action-fail
                     set errori to true
                     display message "Codice IVA non valido"
                               title tit-err
                                icon 2
                 not invalid
                     if col-iva = tge-cod-iva-omag
                        inquire form1-gd-2(event-data-2, 5), 
                                cell-data in col-prezzo
                        move col-prezzo to ror-prz-unitario
                        if ror-prz-unitario not = 0
                           display message "Confermi iva omaggio?"
                                     title titolo
                                      type mb-yes-no
                                      icon 2
                                    giving scelta
                           if scelta = mb-no
                              set errori to true
                              set event-action to event-action-fail
                           end-if
                        end-if
                     end-if
                end-read
           end-evaluate.
           perform RESETTA-VIRGOLA.
      
      ***---
       FORM1-GD-3-FINISH-ENTRY.
           inquire form1-gd-3(event-data-2, 1),
                   hidden-data = HiddenValori.
           evaluate event-data-1
           when 4
                inquire form1-gd-3(event-data-2, event-data-1), 
                        cell-data in col-quantita
                move col-quantita to como-numero
      *****          if como-numero > hid-qta-nc
      *****             move hid-qta-nc  to col-quantita
      *****             move como-numero to col-qta
      *****             display message
      *****            "La quantità degli articoli addebitati non può essere"
      *****     X"0d0a""maggiore di quella degli articoli accreditati"
      *****     X"0D0A""Quantità accreditata: " col-quantita
      *****     X"0D0A""Quantità addebitata: "  col-qta
      *****                          title tit-err
      *****                           icon 2
      *****             set errori to true
      *****             set event-action to event-action-fail
      *****          end-if
                if como-numero not = hid-old-qta
                   set RigaCambiata to true
                end-if
           when 5
                inquire form1-gd-3(event-data-2, event-data-1), 
                        cell-data in col-prezzo
                move col-prezzo to ror-prz-unitario
                if ror-prz-unitario not = hid-old-prz
                   perform CALCOLA-IMPOSTE-ADDEBITO-R
                   set RigaCambiata to true
                end-if
                modify form1-gd-3(event-data-2, event-data-1), 
                       cell-data col-prezzo
                if ror-prz-unitario = 0
                   move tge-cod-iva-omag to col-iva
                   move 0 to col-imp-merce
                   move 0 to col-consumo
                   move 0 to col-coubat
                   move 0 to col-add
                   modify form1-gd-3(event-data-2, 6), 
                          cell-data col-imp-merce
                   modify form1-gd-3(event-data-2, 7), 
                          cell-data col-consumo
                   modify form1-gd-3(event-data-2, 8), 
                          cell-data col-coubat
                   modify form1-gd-3(event-data-2, 9), 
                          cell-data col-add
                else
                   move spaces to tge-codice
                   read tparamge no lock
                   move tge-cod-iva-std to col-iva
                end-if
                modify form1-gd-3(event-data-2, 10), 
                       cell-data col-iva
           when 7
                inquire form1-gd-3(event-data-2, event-data-1), 
                        cell-data in col-consumo
                move col-consumo to ror-imp-consumo
                if ror-imp-consumo not = hid-old-consumo
                   perform CALCOLA-IMPOSTE-ADDEBITO-R
                   set RigaCambiata to true
                end-if
                modify form1-gd-3(event-data-2, event-data-1), 
                       cell-data col-consumo
           when 8
                inquire form1-gd-3(event-data-2, event-data-1), 
                        cell-data in col-coubat
                move col-coubat to ror-imp-cou-cobat
                if ror-imp-cou-cobat not = hid-old-coubat
                   perform CALCOLA-IMPOSTE-ADDEBITO-R
                   set RigaCambiata to true
                end-if
                modify form1-gd-2(event-data-2, event-data-1), 
                       cell-data col-coubat
           when 9
                inquire form1-gd-3(event-data-2, event-data-1), 
                        cell-data in col-add
                move col-add to ror-add-piombo
                if ror-add-piombo not = hid-old-piombo
                   perform CALCOLA-IMPOSTE-ADDEBITO-R
                   set RigaCambiata to true
                end-if
                modify form1-gd-3(event-data-2, event-data-1), 
                       cell-data col-add
           when 10
                inquire form1-gd-3(event-data-2, event-data-1), 
                        cell-data in col-iva
                move "IV"    to tbliv-codice1
                move col-iva to tbliv-codice2
                read tivaese no lock
                     invalid
                     set event-action to event-action-fail
                     set errori to true
                     display message "Codice IVA non valido"
                               title tit-err
                                icon 2
                 not invalid
                     if col-iva = tge-cod-iva-omag
                        inquire form1-gd-3(event-data-2, 5), 
                                cell-data in col-prezzo
                        move col-prezzo to ror-prz-unitario
                        if ror-prz-unitario not = 0
                           display message "Confermi IVA omaggio?"
                                     title titolo
                                      type mb-yes-no
                                      icon 2
                                    giving scelta
                           if scelta = mb-no
                              set errori to true
                              set event-action to event-action-fail
                           end-if
                        end-if
                     end-if
                end-read
           end-evaluate.
           perform RESETTA-VIRGOLA.

      ***---
       CALCOLA-IMPOSTE-ADDEBITO.
           set cli-tipo-C to true.
           inquire ef-cli, value in cli-codice.
           read clienti no lock invalid continue end-read.
           move cli-tipo to tcl-codice.
           read ttipocli no lock invalid continue end-read.
           inquire form1-gd-2(event-data-2, 5), 
                   cell-data in col-prezzo.
           if ttipocli-gdo
              inquire form1-gd-2(event-data-2, 7), 
                      cell-data in col-consumo
              inquire form1-gd-2(event-data-2, 8), 
                      cell-data in col-coubat
              inquire form1-gd-2(event-data-2, 9), 
                      cell-data in col-add
              move col-prezzo  to ror-prz-unitario
              move col-consumo to ror-imp-consumo
              move col-coubat  to ror-imp-cou-cobat
              move col-add     to ror-add-piombo
              compute ror-imponib-merce = 
                      ror-prz-unitario  - 
                      ror-imp-consumo   -
                      ror-imp-cou-cobat -
                      ror-add-piombo
              move ror-imponib-merce to col-imp-merce
           else
              move col-prezzo to col-imp-merce
           end-if.
           modify form1-gd-2(event-data-2, 6), 
                  cell-data col-imp-merce.

      ***---
       CALCOLA-IMPOSTE-ADDEBITO-R.
           set cli-tipo-C to true.
           inquire ef-cli, value in cli-codice.
           read clienti no lock invalid continue end-read.
           move cli-tipo to tcl-codice.
           read ttipocli no lock invalid continue end-read.
           inquire form1-gd-3(event-data-2, 5), 
                   cell-data in col-prezzo.
           if ttipocli-gdo
              inquire form1-gd-3(event-data-2, 7), 
                      cell-data in col-consumo
              inquire form1-gd-3(event-data-2, 8), 
                      cell-data in col-coubat
              inquire form1-gd-3(event-data-2, 9), 
                      cell-data in col-add
              move col-prezzo  to ror-prz-unitario
              move col-consumo to ror-imp-consumo
              move col-coubat  to ror-imp-cou-cobat
              move col-add     to ror-add-piombo
              compute ror-imponib-merce = 
                      ror-prz-unitario  - 
                      ror-imp-consumo   -
                      ror-imp-cou-cobat -
                      ror-add-piombo
              move ror-imponib-merce to col-imp-merce
           else
              move col-prezzo to col-imp-merce
           end-if.
           modify form1-gd-3(event-data-2, 6), 
                  cell-data col-imp-merce.

      ***---
       BEFORE-CAMPI-IMPOSTE.
           move Link-TipoNota to tca-codice.
           read tcaumag no lock invalid continue end-read.
           inquire ef-prz, value in brno-prz-unitario.
           if tca-tipo-nota-reso or brno-prz-unitario = 0 
              modify ef-consumo, read-only, color = 513
              modify ef-coubat,  read-only, color = 513
              modify ef-add,     read-only, color = 513
              perform CANCELLA-COLORE
              move 78-ID-ef-iva to control-id
              move 4            to accept-control
           else
              modify ef-consumo, not read-only
              modify ef-coubat,  not read-only
              modify ef-add,     not read-only
              perform SETTA-VIRGOLA
           end-if.
           if FromSpostamento
              perform CANCELLA-COLORE
              set ArticoloSetFocus to false
              move 78-ID-ef-art to control-id
              move 4            to accept-control
           end-if.

      ***---
       PB-NUOVO-PRESSED.
           initialize HiddenKey
                      HiddenValori replacing numeric data by zeroes
                                        alphanumeric data by spaces.
           move "M" to hid-tipo-riga.
           if riga-nuova = 0
              perform INSERISCI-RIGA
              modify form1-gd-1(riga, 1), hidden-data HiddenValori
              modify form1-gd-1(riga, 2), hidden-data HiddenKey
              move riga to event-data-2
              perform SPOSTAMENTO
              modify pb-nuovo,  bitmap-number = 5
              move 5 to NumBitmapNuovoGrid
           end-if.
                                 
           perform CANCELLA-COLORE.
           perform INITIALIZE-ENTRY.
           move 78-ID-ef-art to control-id.
           move 4            to accept-control.

      ***---
       ENTRY-TO-ROW.
           inquire ef-art,     value in brno-cod-articolo.
           inquire ef-consumo, value in brno-imp-consumo.
           inquire ef-coubat,  value in brno-imp-cou-cobat.
           inquire ef-add,     value in brno-add-piombo.
           inquire ef-prz,     value in brno-prz-unitario.
           inquire ef-qta,     value in brno-qta.
           inquire ef-imp,     value in brno-imponib-merce.
           inquire ef-iva,     value in ef-iva-buf.

           if ef-iva-buf            = tge-cod-iva-omag and
              brno-prz-unitario not = 0
              display message "Confermi IVA omaggio?"
                        title titolo
                         type mb-yes-no
                         icon 2
                       giving scelta
              if scelta = mb-no
                 set errori to true
                 exit paragraph
              end-if
           end-if.

           if hid-old-art      not = brno-cod-articolo  or
              hid-old-prz      not = brno-prz-unitario  or
              hid-old-consumo  not = brno-imp-consumo   or
              hid-old-coubat   not = brno-imp-cou-cobat or
              hid-old-piombo   not = brno-add-piombo    or
              hid-old-qta      not = brno-qta
              set RigaCambiata to true
           end-if.

           move ef-art-buf         to col-codice.
           move lab-art-buf        to col-descr.
           move brno-imp-consumo   to col-consumo.
           move brno-imp-cou-cobat to col-coubat.
           move brno-add-piombo    to col-add.
           move brno-prz-unitario  to col-prezzo.
           move brno-imponib-merce to col-imp-merce.
           move brno-qta           to col-quantita.
           move ef-iva-buf         to col-cod-iva.

           modify form1-gd-1(riga, 2),  cell-data =  col-codice.
           modify form1-gd-1(riga, 3),  cell-data =  col-descr.
           modify form1-gd-1(riga, 4),  cell-data =  col-quantita.
           modify form1-gd-1(riga, 5),  cell-data =  col-prezzo.
           modify form1-gd-1(riga, 6),  cell-data =  col-imp-merce.
           modify form1-gd-1(riga, 7),  cell-data =  col-consumo.
           modify form1-gd-1(riga, 8),  cell-data =  col-coubat.
           modify form1-gd-1(riga, 9),  cell-data =  col-add.
           modify form1-gd-1(riga, 10), cell-data =  col-cod-iva.

           move col-codice   to hid-old-art.
           move col-quantita to hid-old-qta hid-qta-nc.
           move col-prezzo   to hid-old-prz.
           move col-consumo  to hid-old-consumo.
           move col-coubat   to hid-old-coubat.
           move col-add      to hid-old-piombo.
           move col-cod-iva  to hid-old-iva.
                                         
           modify form1-gd-1(riga, 1),  hidden-data = HiddenValori.
           modify form1-gd-1(riga, 2),  hidden-data = HiddenKey.

           move 0 to riga-nuova.

           move HiddenKey  to brno-prg-chiave.
           inquire form1-gd-2, last-row in tot-righe.
           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe

               inquire form1-gd-2(store-riga, 2),
                       hidden-data = HiddenKey

               if HiddenKey = brno-prg-chiave
                  modify form1-gd-2(store-riga, 1),
                         hidden-data = HiddenValori
                  exit perform
               end-if

           end-perform.

      ***---
       BEFORE-ARTICOLO.
           if hid-tipo-riga = "F"
              modify ef-art,     read-only
           else
              modify ef-art, not read-only
           end-if.
           set FromSpostamento to false.

      ***---
       BEFORE-PREZZO.
           if FromSpostamento
              perform CANCELLA-COLORE
              set ArticoloSetFocus to false
              move 78-ID-ef-art to control-id
              move 4            to accept-control
           else
              if hid-tipo-riga = "F"
                 modify ef-prz, color = colore-or
                 move 78-ID-ef-iva to control-id
                 move 4            to accept-control
              else
                 modify  ef-prz, color = colore-nu
                 inquire ef-prz  value in como-prezzo
                 inquire ef-iva, value in ef-iva-buf
                 move Link-TipoNota to tca-codice
                 read tcaumag no lock invalid continue end-read
                 evaluate true
                 when tca-tipo-nota-reso
                    |Si tratta di una riga VOLUTAMENTE omaggio
                    |quindi non propongo il prezzo
                    if como-prezzo    = 0 and
                       ef-iva-buf not = tge-cod-iva-omag
                       perform ZOOM-SU-FATTURATI
                       |Torna così se esce lo zoom ma non 
                       |seleziono niente, quindi significa 
                       |che la riga dev'essere di tipo omaggio
                       if errori
                          set tutto-ok to true
                          move tge-cod-iva-omag to ef-iva-buf
                          display ef-iva
                       end-if
                    end-if
                 end-evaluate
              end-if
           end-if.

      ***---
       PB-GENERA2-PRESSED.           
           set trovato        to false.
           modify form1-gd-2, reset-grid = 1.
           perform FORM1-GD-2-CONTENT.

           modify form1-gd-2, mass-update = 1.
           inquire gd-pag2, last-row in tot-righe.
           move 0 to LastPrg.
           move 2 to store-riga.
           move 0 to riga-nuova2.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire gd-pag2(riga, 1), hidden-data in hidden-sel
              inquire gd-pag2(riga, 2), hidden-data in HiddenKey
              inquire gd-pag2(riga, 3), hidden-data in HiddenPeso
              if hidden-sel = 1
                 add 1 to LastPrg
                 move LastPrg to col-riga

                 inquire gd-pag2(riga, 2),  cell-data in col-art
                 inquire gd-pag2(riga, 3),  cell-data in col-des
                 inquire gd-pag2(riga, 4),  cell-data in col-qta
                 inquire gd-pag2(riga, 5),  cell-data in col-uni
                 inquire gd-pag2(riga, 6),  cell-data in col-imp
                 inquire gd-pag2(riga, 7),  cell-data in col-cons
                 inquire gd-pag2(riga, 8),  cell-data in col-cou 
                 inquire gd-pag2(riga, 9),  cell-data in col-add-pb
                 inquire gd-pag2(riga, 10), cell-data in col-iva

                 move col-art    to col-codice
                 move col-des    to col-descr
                 move col-qta    to col-quantita
                 move col-uni    to col-prezzo
                 move col-imp    to col-imp-merce
                 move col-cons   to col-consumo
                 move col-cou    to col-coubat
                 move col-add-pb to col-add

                 move col-qta to como-qta
                 compute como-prz = hid-peso * 6,2
                 move como-prz to col-prezzo

LUBEXX           move col-iva        to col-cod-iva
                 
                 modify form1-gd-2(store-riga, 1), cell-data col-riga
                 modify form1-gd-2(store-riga, 2), cell-data col-codice
                 modify form1-gd-2(store-riga, 3), cell-data col-descr
                 modify form1-gd-2(store-riga, 4), 
                        cell-data col-quantita
                 modify form1-gd-2(store-riga, 5), cell-data col-prezzo
                 modify form1-gd-2(store-riga, 6), 
                        cell-data col-imp-merce
                 modify form1-gd-2(store-riga, 7), cell-data col-consumo
                 modify form1-gd-2(store-riga, 8), cell-data col-coubat 
                 modify form1-gd-2(store-riga, 9), cell-data col-add
                 modify form1-gd-2(store-riga, 10),cell-data col-cod-iva

                 move store-riga to event-data-2

                 perform CALCOLA-IMPOSTE-ADDEBITO

                 move col-quantita  to hid-qta-nc
                 move col-prezzo    to hid-old-prz
                 move col-consumo   to hid-old-consumo
                 move col-coubat    to hid-old-coubat
                 move col-add       to hid-old-piombo
                 move col-cod-iva   to hid-old-iva
                 move HiddenUtf     to hid-peso-utf
                 move HiddenNonUtf  to hid-peso-non-utf

                 modify form1-gd-2(store-riga, 1),
                        hidden-data = HiddenValori
                 modify form1-gd-2(store-riga, 2),
                        hidden-data = HiddenKey
                 set trovato to true
                 add 1 to store-riga
                 set RigaCambiata to true
              end-if
           end-perform.
           modify form1-gd-2, mass-update = 0.

           if trovato            
              perform CANCELLA-COLORE
              perform SPOSTAMENTO2
           end-if.

      ***---
       PB-GENERA-R-PRESSED.           
           set trovato        to false.
           modify form1-gd-3, reset-grid = 1.
           perform FORM1-GD-3-CONTENT.

           modify form1-gd-3, mass-update = 1.
           inquire gd-pag3, last-row in tot-righe.
           move 0 to LastPrg.
           move 2 to store-riga.
           move 0 to riga-nuova3.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire gd-pag3(riga, 1), hidden-data in hidden-sel
              inquire gd-pag3(riga, 2), hidden-data in HiddenKey
              inquire gd-pag3(riga, 3), hidden-data in HiddenPeso
              if hidden-sel = 1
                 add 1 to LastPrg
                 move LastPrg to col-riga

                 inquire gd-pag3(riga, 2),  cell-data in col-art
                 inquire gd-pag3(riga, 3),  cell-data in col-des
                 inquire gd-pag3(riga, 4),  cell-data in col-qta
                 inquire gd-pag3(riga, 5),  cell-data in col-uni
                 inquire gd-pag3(riga, 6),  cell-data in col-imp
                 inquire gd-pag3(riga, 7),  cell-data in col-cons
                 inquire gd-pag3(riga, 8),  cell-data in col-cou 
                 inquire gd-pag3(riga, 9),  cell-data in col-add-pb
                 inquire gd-pag3(riga, 10), cell-data in col-iva

                 move col-art    to col-codice
                 move col-des    to col-descr
                 move col-qta    to col-quantita
                 move col-uni    to col-prezzo
                 move col-imp    to col-imp-merce
                 move col-cons   to col-consumo
                 move col-cou    to col-coubat
                 move col-add-pb to col-add

LUBEXX           move col-iva        to col-cod-iva
                 
                 modify form1-gd-3(store-riga, 1), cell-data col-riga
                 modify form1-gd-3(store-riga, 2), cell-data col-codice
                 modify form1-gd-3(store-riga, 3), cell-data col-descr
                 modify form1-gd-3(store-riga, 4), 
                        cell-data col-quantita
                 modify form1-gd-3(store-riga, 5), cell-data col-prezzo
                 modify form1-gd-3(store-riga, 6), 
                        cell-data col-imp-merce
                 modify form1-gd-3(store-riga, 7), cell-data col-consumo
                 modify form1-gd-3(store-riga, 8), cell-data col-coubat 
                 modify form1-gd-3(store-riga, 9), cell-data col-add
                 modify form1-gd-3(store-riga, 10),cell-data col-cod-iva

                 move col-quantita  to hid-qta-nc
                 move col-prezzo    to hid-old-prz
                 move col-consumo   to hid-old-consumo
                 move col-coubat    to hid-old-coubat
                 move col-add       to hid-old-piombo
                 move col-cod-iva   to hid-old-iva
                 move HiddenUtf     to hid-peso-utf
                 move HiddenNonUtf  to hid-peso-non-utf

                 modify form1-gd-3(store-riga, 1),
                        hidden-data = HiddenValori
                 modify form1-gd-3(store-riga, 2),
                        hidden-data = HiddenKey
                 set trovato to true
                 add 1 to store-riga
                 set RigaCambiata to true
              end-if
           end-perform.
           modify form1-gd-3, mass-update = 0.

           if trovato            
              perform CANCELLA-COLORE
              perform SPOSTAMENTO3
           end-if.

      ***---
       PB-SEL-TUTTO-N-PRESSED.
           inquire gd-pag2, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              move riga to event-data-2
              move 0 to hidden-sel
              modify gd-pag2(riga, 1), hidden-data = hidden-sel 
              perform SELEZIONA-RIGA-PAG-2
           end-perform.

      ***---
       PB-SEL-TUTTO-R-PRESSED.
           inquire gd-pag3, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              move riga to event-data-2
              move 0 to hidden-sel
              modify gd-pag3(riga, 1), hidden-data = hidden-sel 
              perform SELEZIONA-RIGA-PAG-3
           end-perform.

      ***---
       BEFORE-CAMPO-CLIENTE.
           |Se ci sono righe non cambio più il cliente altrimenti
           |dovrei ricontrollare tutti i vari assoggettamenti sia
           |per le imposte che per l'addizionale piombo
      *****     inquire form1-gd-1, last-row in tot-righe.
      *****     if tot-righe > 1
      *****        if btno-data-fatt > 0          or
      *****           btno-num-fatt  > 0          or  
      *****           btno-data-fm   > 0          or  
      *****           btno-num-fm    > 0          or  
      *****           btno-data-nc   > 0          or  
      *****           btno-num-nc    > 0
      *****           modify ef-cli, read-only
      *****           perform CANCELLA-COLORE
      *****           move 78-ID-ef-des to control-id
      *****           move 4            to accept-control
      *****        end-if
      *****     else
      *****        modify ef-cli, not read-only
      *****     end-if.

      ***---
       CONTROLLO-QUANTITA.
           inquire form1-gd-2, last-row in tot-righe.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire form1-gd-2(riga, 4), 
                      cell-data in col-quantita
              move col-quantita to como-numero
              inquire form1-gd-2(riga, 1),
                     hidden-data = HiddenValori
      *****        if como-numero > hid-qta-nc
      *****           move hid-qta-nc  to col-quantita
      *****           move como-numero to col-qta
      *****           subtract 1 from riga
      *****           move riga to riga-edit
      *****           display message
      *****            "La quantità degli articoli addebitati non può essere"
      *****     X"0d0a""maggiore di quella degli articoli accreditati."
      *****     x"0d0a""Riga ", riga-edit
      *****     X"0d0a""Quantità accreditata: " col-quantita
      *****     X"0d0a""Quantità addebitata: "  col-qta
      *****                     title tit-err
      *****                      icon 2
      *****           set errori to true
      *****           exit perform
      *****        end-if
           end-perform.

      ***---
       SETTA-ADDEBITO-CORRETTO.
      *****     inquire form1-gd-2, last-row in tot-righe.
      *****     inquire form1-gd-1, last-row in tot-righe2.
      *****     perform varying riga from 2 by 1 
      *****               until riga > tot-righe
      *****        inquire form1-gd-2(riga, 2), 
      *****                hidden-data in HiddenKey
      *****        move HiddenKey to brno-prg-chiave
      *****        set trovato to false
      *****        perform varying store-riga from 2 by 1 
      *****                  until store-riga > tot-righe2
      *****           inquire form1-gd-1(store-riga, 2),
      *****                   hidden-data = HiddenKey
      *****           if HiddenKey = brno-prg-chiave
      *****              |Setto i valori di prezzo uguali
      *****              inquire form1-gd-1(riga, 5), 
      *****                      cell-data in col-imp-merce
      *****              inquire form1-gd-1(riga, 6), 
      *****                      cell-data in col-prezzo
      *****              inquire form1-gd-1(riga, 7), 
      *****                      cell-data in col-consumo
      *****              inquire form1-gd-1(riga, 8), 
      *****                      cell-data in col-coubat
      *****              inquire form1-gd-1(riga, 9), 
      *****                      cell-data in col-add
      *****              inquire form1-gd-1(riga, 10),
      *****                      cell-data in col-cod-iva
      *****
      *****              modify form1-gd-2(riga, 5), cell-data col-prezzo
      *****              modify form1-gd-2(riga, 6), cell-data col-imp-merce
      *****              modify form1-gd-2(riga, 7), cell-data col-consumo
      *****              modify form1-gd-2(riga, 8), cell-data col-coubat
      *****              modify form1-gd-2(riga, 9), cell-data col-add
      *****              modify form1-gd-2(riga, 10),cell-data col-cod-iva
      *****              exit perform
      *****           end-if
      *****        end-perform
      *****     end-perform.

      ***---
       SPOSTAMENTO3.
           set tutto-ok to true.
           inquire form1-gd-3, last-row in tot-righe.
           move  event-data-2 to riga.

           if riga < 2 move 2 to riga end-if.

           if riga > tot-righe
              if riga-nuova3 = 0
                 move tot-righe to riga
              else
                 set errori to true
              end-if
           else
              if riga-nuova3 = 1
                 move 0 to riga-nuova3
                 modify form1-gd-3,  record-to-delete = tot-righe + 1
                 subtract 1 from LastPrg
              end-if
           end-if.

           modify form1-gd-3, start-y = riga, start-x =  2,
                                    y = riga,       x = 12,
                         region-color = 144.

           if tutto-ok
              set Fromspostamento  to true
           end-if.
                                                        
           modify form1-gd-3, cursor-y = riga.
      *****     set event-action   to event-action-fail. 

      ***---
       VERIFICA-ABIL-ROTTA.
           set si-rotta   to true
           read tmagaz no lock
                invalid set si-rotta   to false
            not invalid
                move mag-cau-carico-rot to tca-codice
                read tcaumag no lock
                     invalid set si-rotta   to false
                 not invalid
                     move mag-cau-scarico-rot to tca-codice
                     read tcaumag no lock
                          invalid set si-rotta   to false
                     end-read
                end-read
           end-read.
