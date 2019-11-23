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

           when 78-ID-ef-nota1
                inquire ef-anno1, value in como-anno
                perform RIEMPI-TMP-NOTE

                move "zoom-note"   to como-file

                if trovato
                   move path-zoom-tordini to ext-file
                   call   "zoom-gt"   using como-file, zoom-tor-rec
                                     giving stato-zoom

                   cancel "zoom-gt"
                else
                   move 1 to stato-zoom
                end-if
                delete file zoom-tordini

                if stato-zoom = 0
                   move zoom-tor-anno   to ef-anno1-buf
                   move zoom-tor-numero to ef-nota1-buf
                   display ef-anno1 ef-nota1
                end-if

           when 78-ID-ef-nota2
                inquire ef-anno2, value in como-anno
                perform RIEMPI-TMP-NOTE

                move "zoom-note"   to como-file

                if trovato
                   move path-zoom-tordini to ext-file
                   call   "zoom-gt"   using como-file, zoom-tor-rec
                                     giving stato-zoom

                   cancel "zoom-gt"
                else
                   move 1 to stato-zoom
                end-if
                delete file zoom-tordini

                if stato-zoom = 0
                   move zoom-tor-anno   to ef-anno2-buf
                   move zoom-tor-numero to ef-nota2-buf
                   display ef-anno2 ef-nota2
                end-if

           when 78-ID-ef-nota3
                inquire ef-anno3, value in como-anno
                perform RIEMPI-TMP-NOTE

                move "zoom-note"   to como-file

                if trovato
                   move path-zoom-tordini to ext-file
                   call   "zoom-gt"   using como-file, zoom-tor-rec
                                     giving stato-zoom

                   cancel "zoom-gt"
                else
                   move 1 to stato-zoom
                end-if
                delete file zoom-tordini

                if stato-zoom = 0
                   move zoom-tor-anno   to ef-anno3-buf
                   move zoom-tor-numero to ef-nota3-buf
                   display ef-anno3 ef-nota3
                end-if

           when 78-ID-ef-nota4
                inquire ef-anno4, value in como-anno
                perform RIEMPI-TMP-NOTE

                move "zoom-note"   to como-file

                if trovato
                   move path-zoom-tordini to ext-file
                   call   "zoom-gt"   using como-file, zoom-tor-rec
                                     giving stato-zoom

                   cancel "zoom-gt"
                else
                   move 1 to stato-zoom
                end-if
                delete file zoom-tordini

                if stato-zoom = 0
                   move zoom-tor-anno   to ef-anno4-buf
                   move zoom-tor-numero to ef-nota4-buf
                   display ef-anno4 ef-nota4
                end-if

           when 78-ID-ef-nota5
                inquire ef-anno5, value in como-anno
                perform RIEMPI-TMP-NOTE

                move "zoom-note"   to como-file

                if trovato
                   move path-zoom-tordini to ext-file
                   call   "zoom-gt"   using como-file, zoom-tor-rec
                                     giving stato-zoom

                   cancel "zoom-gt"
                else
                   move 1 to stato-zoom
                end-if
                delete file zoom-tordini

                if stato-zoom = 0
                   move zoom-tor-anno   to ef-anno5-buf
                   move zoom-tor-numero to ef-nota5-buf
                   display ef-anno5 ef-nota5
                end-if

           end-evaluate.

      ***---
       RIEMPI-TMP-NOTE.           
           call "W$MOUSE" using set-mouse-shape, wait-pointer.
           set record-ok to true

           accept  path-zoom-tordini      from environment "PATH_ST".
           accept  como-data              from century-date.
           accept  como-ora               from time.
           inspect path-zoom-tordini      replacing trailing
                                          spaces by low-value.
           string path-zoom-tordini       delimited by low-value
                  "zoom-tordini"          delimited by size
                  "_"                     delimited by size
                  como-data               delimited by size
                  "_"                     delimited by size
                  como-ora                delimited by size
                  into path-zoom-tordini
           end-string.

           set trovato to false.
           open output zoom-tordini.
           initialize tno-rec.
           move ef-cli-buf to como-cliente tno-cod-cli.
           move ef-des-buf to como-destino tno-prg-destino.

           start tnotacr key  is >= k1
                 invalid continue 
             not invalid
                 perform until 1 = 2
                    read tnotacr next no lock 
                         at end exit perform 
                    end-read

                    if tno-cod-cli not = como-cliente
                       exit perform
                    end-if

                    if tno-anno = como-anno

                       move tno-anno          to zoom-tor-anno
                       move tno-numero        to zoom-tor-numero
                       move tno-data          to zoom-tor-data-ordine
                       move tno-causale       to tca-codice
                       read tcaumag 
                            invalid initialize tca-caus-trasporto 
                       end-read
                       move tca-caus-trasporto to zoom-tor-causale
                       move tno-cod-cli        to zoom-tor-cod-cli
                                                  cli-codice
                                                  des-codice
                       set cli-tipo-C          to true
                       move tno-prg-destino    to zoom-tor-prg-destino
                                                  des-prog

                       read clienti no lock
                            invalid initialize cli-rec
                       end-read
                       move cli-ragsoc-1       to zoom-tor-cli-ragsoc-1

                       read destini no lock
                            invalid
                            initialize des-rec
                            move cli-localita to des-localita
                       end-read
                       move des-localita       to zoom-tor-des-localita

                       move tno-num-fattura  to zoom-tor-num-fat
                       move tno-data-fattura to zoom-tor-data-fat

                       write zoom-tor-rec invalid continue end-write
                       set trovato to true
                    end-if

                 end-perform
           end-start.

           close zoom-tordini.

           call "W$MOUSE" using set-mouse-shape, arrow-pointer.

           |Per posizionarmi sul primo
           if trovato
              initialize zoom-tor-rec replacing numeric data by zeroes
                                           alphanumeric data by spaces
           end-if.

      ***---
       CONTROLLO.
           set tutto-ok to true. 
      * Paragrafo per la struttura dei controlli sulla screen Screen1
           evaluate control-id
           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf
                move ef-data-buf to como-data
                if como-data = 0
                   accept como-data from century-date
                   perform DATE-TO-SCREEN
                else
                   perform DATE-FORMAT
                end-if
                move como-data to ef-data-buf
                display ef-data

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
                   perform SELEZIONA-DESTINO
                   display lab-des
                   display lab-ind-d
                   display lab-loca-d
                else
                   if CheckDestini
                      perform TROVA-DESTINO
                      if trovato
                         display message "Esiste uno o più destini per"
                                          " il cliente specificato."
                                  x"0d0a""Procedere comunque con "
                                          "progressivo non valorizzato?"
                                 title = titolo
                                  type mb-yes-no
                                giving scelta 
                         initialize des-rec
                         if scelta = mb-no
                            set errori to true
                            move 78-ID-ef-des to control-id
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

           |78-ID-ef-importo è l'ID del control ef-importo
           when 78-ID-ef-importo
                inquire ef-importo, value in cnt-importo
                if cnt-importo = 0
                   display message "Importo obbligatorio"
                             title tit-err
                              icon 2
                   set errori to true
                end-if

           |78-ID-ef-data-b è l'ID del control ef-data-b
           when 78-ID-ef-data-b
                inquire ef-data-b, value in como-data
                if como-data not = 0
                   perform DATE-FORMAT
                   move como-data to ef-data-b-buf
                   display ef-data-b
                end-if

           |78-ID-ef-data-rich è l'ID del control ef-data-rich
           when 78-ID-ef-data-rich
                if mod-rich = 1
                   inquire ef-data-rich, value in ef-data-rich-buf
                   move ef-data-rich-buf to como-data
                   perform DATE-FORMAT
                   move como-data to ef-data-rich-buf
                   display ef-data-rich
                end-if

           |78-ID-ef-data-rich è l'ID del control ef-data-rich
           when 78-ID-ef-data-ric
                if mod-rich = 1
                   inquire ef-data-ric, value in ef-data-ric-buf
                   move ef-data-ric-buf to como-data
                   if como-data not = 0
                      perform DATE-FORMAT
                      move como-data to ef-data-ric-buf
                      display ef-data-ric
                   end-if
                end-if

           |78-ID-ef-vet è l'ID del control ef-vet
           when 78-ID-ef-vet
                if mod-rich = 1
                   inquire ef-vet, value in ef-vet-buf
                   move ef-vet-buf to vet-codice
                   if vet-codice = 0
                      set errori to true
                      display message "Vettore obbligatorio"
                                title tit-err
                                 icon 2
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
                end-if

           |78-ID-ef-numero è l'ID del control ef-numero
           when 78-ID-ef-numero
                if mod-nota = 1
                   inquire ef-numero, value in ef-numero-buf
                   if ef-numero-buf = spaces
                      display message 
                      "Rif. Cliente per Nota Debito obbligatorio"
                                title tit-err
                                 icon 2
                      set errori to true
                   end-if
                end-if

           |78-ID-ef-data-nota è l'ID del control ef-data-nota
           when 78-ID-ef-data-nota
                if mod-nota = 1
                   inquire ef-data-nota, value in ef-data-nota-buf
                   move ef-data-nota-buf to como-data
                   perform DATE-FORMAT
                   move como-data to ef-data-nota-buf
                   display ef-data-nota
                end-if

           |78-ID-ef-anno1 è l'ID del control ef-anno1
           when 78-ID-ef-anno1
                inquire ef-anno1, value in como-anno
                if como-anno = 0
                   move 0 to ef-nota1-buf
                   move spaces to lab-data-fatt1-buf lab-cli1-buf   
                                  lab-n-fatt1-buf    lab-des1-buf 
                   display lab-cli1    lab-des1 
                           lab-n-fatt1 lab-data-fatt1 ef-nota1
                end-if

           |78-ID-ef-nota1 è l'ID del control ef-nota1
           when 78-ID-ef-nota1
                inquire ef-anno1, value in como-anno
                inquire ef-nota1, value in como-numero
                if como-anno not = 0 or como-numero not = 0
                   move como-chiave to tno-chiave
                   read tnotacr no lock
                        invalid
                        move 78-ID-ef-anno1 to control-id
                        set errori to true
                        display message "Nota inesistente"
                                  title tit-err
                                   icon 2
                        move spaces to lab-data-fatt1-buf lab-cli1-buf   
                                       lab-n-fatt1-buf    lab-des1-buf 
                    not invalid
                        perform CONTROLLO-NOTA-GIA-ASSOCIATA
                        if tutto-ok
                           set campo1 to true
                           perform VALORIZZA-CAMPI-NOTA
                        end-if
                   end-read
                else
                   move spaces to lab-data-fatt1-buf lab-cli1-buf   
                                  lab-n-fatt1-buf    lab-des1-buf 
                end-if
                display lab-cli1    lab-des1 
                        lab-n-fatt1 lab-data-fatt1

           |78-ID-ef-anno2 è l'ID del control ef-anno2
           when 78-ID-ef-anno2
                inquire ef-anno2, value in como-anno
                if como-anno = 0
                   move 0 to ef-nota2-buf
                   move spaces to lab-data-fatt2-buf lab-cli2-buf   
                                  lab-n-fatt2-buf    lab-des2-buf 
                   display lab-cli2    lab-des2 
                           lab-n-fatt2 lab-data-fatt2 ef-nota2
                end-if

           |78-ID-ef-nota2 è l'ID del control ef-nota2
           when 78-ID-ef-nota2
                inquire ef-anno2, value in como-anno
                inquire ef-nota2, value in como-numero
                if como-anno not = 0 or como-numero not = 0
                   move como-chiave to tno-chiave
                   read tnotacr no lock
                        invalid
                        set errori to true
                        move 78-ID-ef-anno2 to control-id
                        display message "Nota inesistente"
                                  title tit-err
                                   icon 2
                        move spaces to lab-data-fatt2-buf lab-cli2-buf   
                                       lab-n-fatt2-buf    lab-des2-buf 
                    not invalid
                        perform CONTROLLO-NOTA-GIA-ASSOCIATA
                        if tutto-ok
                           set campo2 to true
                           perform VALORIZZA-CAMPI-NOTA
                        end-if
                   end-read
                else
                   move spaces to lab-cli2-buf     lab-data-fatt2-buf
                                  lab-n-fatt2-buf  lab-des2-buf 
                end-if
                display lab-cli2    lab-des2
                        lab-n-fatt2 lab-data-fatt2

           |78-ID-ef-anno3 è l'ID del control ef-anno3
           when 78-ID-ef-anno3
                inquire ef-anno3, value in como-anno
                if como-anno = 0
                   move 0 to ef-nota3-buf
                   move spaces to lab-data-fatt3-buf lab-cli3-buf   
                                  lab-n-fatt3-buf    lab-des3-buf 
                   display lab-cli3    lab-des3 
                           lab-n-fatt3 lab-data-fatt3 ef-nota3
                end-if

           |78-ID-ef-nota3 è l'ID del control ef-nota3
           when 78-ID-ef-nota3
                inquire ef-anno3, value in como-anno
                inquire ef-nota3, value in como-numero
                if como-anno not = 0 or como-numero not = 0
                   move como-chiave to tno-chiave
                   read tnotacr no lock
                        invalid
                        set errori to true
                        move 78-ID-ef-anno3 to control-id
                        display message "Nota inesistente"
                                  title tit-err
                                   icon 2
                        move spaces to lab-data-fatt3-buf lab-cli3-buf   
                                       lab-n-fatt3-buf    lab-des3-buf 
                    not invalid
                        perform CONTROLLO-NOTA-GIA-ASSOCIATA
                        if tutto-ok
                           set campo3 to true
                           perform VALORIZZA-CAMPI-NOTA
                        end-if
                   end-read
                else
                   move spaces to lab-cli3-buf     lab-data-fatt3-buf
                                  lab-n-fatt3-buf  lab-des3-buf 
                end-if
                display lab-cli3    lab-des3
                        lab-n-fatt3 lab-data-fatt3

           |78-ID-ef-anno4 è l'ID del control ef-anno4
           when 78-ID-ef-anno4
                inquire ef-anno4, value in como-anno
                if como-anno = 0
                   move 0 to ef-nota4-buf
                   move spaces to lab-data-fatt4-buf lab-cli4-buf   
                                  lab-n-fatt4-buf    lab-des4-buf 
                   display lab-cli4    lab-des4 
                           lab-n-fatt4 lab-data-fatt4 ef-nota4
                end-if

           |78-ID-ef-nota4 è l'ID del control ef-nota4
           when 78-ID-ef-nota4
                inquire ef-anno4, value in como-anno
                inquire ef-nota4, value in como-numero
                if como-anno not = 0 or como-numero not = 0
                   move como-chiave to tno-chiave
                   read tnotacr no lock
                        invalid
                        set errori to true
                        move 78-ID-ef-anno4 to control-id
                        display message "Nota inesistente"
                                  title tit-err
                                   icon 2
                        move spaces to lab-data-fatt4-buf lab-cli4-buf   
                                       lab-n-fatt4-buf    lab-des4-buf 
                    not invalid
                        perform CONTROLLO-NOTA-GIA-ASSOCIATA
                        if tutto-ok
                           set campo4 to true
                           perform VALORIZZA-CAMPI-NOTA
                        end-if
                   end-read
                else
                   move spaces to lab-cli4-buf    lab-data-fatt4-buf
                                  lab-n-fatt4-buf lab-des4-buf 
                end-if
                display lab-cli4    lab-des4
                        lab-n-fatt4 lab-data-fatt4

           |78-ID-ef-anno5 è l'ID del control ef-anno5
           when 78-ID-ef-anno5
                inquire ef-anno5, value in como-anno
                if como-anno = 0
                   move 0 to ef-nota5-buf
                   move spaces to lab-cli5-buf lab-des5-buf  
                                  lab-n-fatt5-buf lab-data-fatt5-buf
                   display lab-cli5    lab-des5 ef-nota5
                           lab-n-fatt5 lab-data-fatt5
                end-if

           |78-ID-ef-nota5 è l'ID del control ef-nota5
           when 78-ID-ef-nota5
                inquire ef-anno5, value in como-anno
                inquire ef-nota5, value in como-numero
                if como-anno not = 0 or como-numero not = 0
                   move como-chiave to tno-chiave
                   read tnotacr no lock
                        invalid
                        set errori to true
                        move 78-ID-ef-anno5 to control-id
                        display message "Nota inesistente"
                                  title tit-err
                                   icon 2
                        move spaces to lab-data-fatt5-buf lab-cli5-buf   
                                       lab-n-fatt5-buf    lab-des5-buf 
                    not invalid
                        perform CONTROLLO-NOTA-GIA-ASSOCIATA
                        if tutto-ok
                           set campo5 to true
                           perform VALORIZZA-CAMPI-NOTA
                        end-if
                   end-read
                else
                   move spaces to lab-cli5-buf     lab-data-fatt5-buf
                                  lab-n-fatt5-buf  lab-des5-buf 
                end-if
                display lab-cli5    lab-des5
                        lab-n-fatt5 lab-data-fatt5

           end-evaluate.
           
           if errori
              perform CANCELLA-COLORE
              move control-id to store-id
              move 4          to accept-control
           end-if . 

      ***---
       DISPLAY-SCREEN.
           display form1.

      ***---
       INIT.
           set PrimaVolta       to true.
           set CheckDestini     to true.

           move 78-ID-ef-cli to control-id.
           move 4            to accept-control.

           if KeyboardSaved
              set KeyboardReleased to true
              set environment "KEYSTROKE" to "DATA=44 44"
              set environment "KEYSTROKE" to "DATA=46 46"
           end-if . 

           perform DISPLAY-SCREEN.

      ***---
       RESETTA-VIRGOLA.
           if KeyboardSaved
              set KeyboardReleased to true
      *       setto la tastiera originale
              set environment "KEYSTROKE" to "DATA=44 44"
              set environment "KEYSTROKE" to "DATA=46 46"
           end-if.

      ***---
       SALVA.
           set StoSalvando to true.
           set tutto-ok to true.
           perform  varying control-id from 78-ID-ef-data by 1
                      until control-id    > 78-ID-ef-nota5
              perform CONTROLLO
              if errori
                 exit perform 
              end-if
           end-perform.
           set StoSalvando to false.

           if tutto-ok
              move 78-ID-cbo-stato to control-id
              perform SCARICA-COMBO-STATO-CONT
              move stato-contest  to cnt-stato
              if cnt-stato not = old-cnt-stato
                 set StatusOk to false
                 display message "E' stato cambiato lo stato."
                          x"0d0a""Confermi?"
                           type mb-yes-no
                        default mb-no
                          title titolo
                           icon 2
                         giving scelta
                 if scelta = mb-no
                    if nuovo
                       set old-cnt-chiusa to true
                    end-if
                    move old-cnt-stato to stato-contest
                    perform CARICA-COMBO-STATO-CONT
                 else
                    if chiusa and modifica
                       move control-id to store-id
                       move 1 to mod
                       move 0 to mod-rich
                       move 0 to mod-nota
                       perform DISPLAY-SCREEN
                       move store-id to control-id
                    end-if
                 end-if
              end-if
              perform FORM1-BUF-TO-FLD
              if inserimento
                 perform VALORIZZA-NUMERO
                 if LinkFatt
                    move save-num-fattura  to cnt-num-fatt
                    move save-anno-fattura to cnt-anno-fatt
                    move save-data-fattura to cnt-data-fatt
                 else
                    move save-num-fattura  to cnt-num-nota
                    move save-anno-fattura to cnt-anno-nota
                    move save-data-fattura to cnt-data-nota
                 end-if
              else
                 move NotaChiave to cnt-chiave
              end-if

              unlock contestazioni all record

              if errori
                 move 27 to key-status
              else
                 write cnt-rec 
                       invalid rewrite cnt-rec 
                 end-write

                 perform ASSOCIA-CONTESTAZIONE-NOTA-CREDITO

                 if inserimento
                    move 27 to key-status
                 else
                    set vecchio to true
                    perform TORNA-IN-VISUA
                 end-if

                 perform DISPLAY-SCREEN
              end-if
           end-if.

      ***---
       ASSOCIA-CONTESTAZIONE-NOTA-CREDITO.
           close    note-cont.
           open i-o note-cont.

           if cnt-nota-anno-1 not = 0 and
              cnt-nota-1      not = 0
              set  trovato to false
              move cnt-nota-anno-1 to noco-anno
              move cnt-nota-1      to noco-numero
              set aggiungi to true
              perform BLOCCA-ASSOCIAZIONE
              perform UPDATE-ASSOCIAZIONE
           end-if.

           if cnt-nota-anno-2 not = 0 and
              cnt-nota-2      not = 0
              set  trovato to false
              move cnt-nota-anno-2 to noco-anno
              move cnt-nota-2      to noco-numero
              set aggiungi to true
              perform BLOCCA-ASSOCIAZIONE
              perform UPDATE-ASSOCIAZIONE
           end-if.

           if cnt-nota-anno-3 not = 0 and
              cnt-nota-3      not = 0
              set  trovato to false
              move cnt-nota-anno-3 to noco-anno
              move cnt-nota-3      to noco-numero
              set aggiungi to true
              perform BLOCCA-ASSOCIAZIONE
              perform UPDATE-ASSOCIAZIONE
           end-if.

           if cnt-nota-anno-4 not = 0 and
              cnt-nota-4      not = 0
              set  trovato to false
              move cnt-nota-anno-4 to noco-anno
              move cnt-nota-4      to noco-numero
              set aggiungi to true
              perform BLOCCA-ASSOCIAZIONE
              perform UPDATE-ASSOCIAZIONE
           end-if.

           if cnt-nota-anno-5 not = 0 and
              cnt-nota-5      not = 0
              set  trovato to false
              move cnt-nota-anno-5 to noco-anno
              move cnt-nota-5      to noco-numero
              set aggiungi to true
              perform BLOCCA-ASSOCIAZIONE
              perform UPDATE-ASSOCIAZIONE
           end-if.

           |cancello le eventuali associazioni precedenti
           if modifica
              if cnt-nota-anno-1 not = old-cnt-nota-anno-1 or
                 cnt-nota-1      not = old-cnt-nota-1
                 if old-cnt-nota-anno-1 not = 0 and
                    old-cnt-nota-1      not = 0
                    move old-cnt-nota-anno-1 to noco-anno
                    move old-cnt-nota-1      to noco-numero
                    set elimina to true
                    perform BLOCCA-ASSOCIAZIONE
                    perform UPDATE-ASSOCIAZIONE
                 end-if
              end-if  
              if cnt-nota-anno-2 not = old-cnt-nota-anno-2 or
                 cnt-nota-2      not = old-cnt-nota-2
                 if old-cnt-nota-anno-2 not = 0 and
                    old-cnt-nota-2      not = 0
                    move old-cnt-nota-anno-2 to noco-anno
                    move old-cnt-nota-2      to noco-numero
                    set elimina to true
                    perform BLOCCA-ASSOCIAZIONE
                    perform UPDATE-ASSOCIAZIONE
                 end-if
              end-if
              if cnt-nota-anno-3 not = old-cnt-nota-anno-3 or
                 cnt-nota-3      not = old-cnt-nota-3
                 if old-cnt-nota-anno-3 not = 0 and
                    old-cnt-nota-3      not = 0
                    move old-cnt-nota-anno-3 to noco-anno
                    move old-cnt-nota-3      to noco-numero
                    set elimina to true
                    perform BLOCCA-ASSOCIAZIONE
                    perform UPDATE-ASSOCIAZIONE
                 end-if
              end-if
              if cnt-nota-anno-4 not = old-cnt-nota-anno-4 or
                 cnt-nota-4      not = old-cnt-nota-4
                 if old-cnt-nota-anno-4 not = 0 and
                    old-cnt-nota-4      not = 0
                    move old-cnt-nota-anno-4 to noco-anno
                    move old-cnt-nota-4      to noco-numero
                    set elimina to true
                    perform BLOCCA-ASSOCIAZIONE
                    perform UPDATE-ASSOCIAZIONE
                 end-if
              end-if
              if cnt-nota-anno-5 not = old-cnt-nota-anno-5 or
                 cnt-nota-5      not = old-cnt-nota-5
                 if old-cnt-nota-anno-5 not = 0 and
                    old-cnt-nota-5      not = 0
                    move old-cnt-nota-anno-5 to noco-anno
                    move old-cnt-nota-5      to noco-numero
                    set elimina to true
                    perform BLOCCA-ASSOCIAZIONE
                    perform UPDATE-ASSOCIAZIONE
                 end-if
              end-if
           end-if.

           close      note-cont.
           open input note-cont.

      ***---
       BLOCCA-ASSOCIAZIONE.   
           perform until 1 = 2
              set tutto-ok  to true
              set RecLocked to false
              read note-cont lock 
                   invalid 
                   initialize noco-dati replacing numeric data by zeroes
                                             alphanumeric data by spaces
                   move user-codi to noco-utente-creazione
                   accept noco-data-creazione from century-date
                   accept noco-ora-creazione  from time
                   write noco-rec invalid continue end-write
              end-read
              if RecLocked
                 call   "geslock" using geslock-linkage
                 cancel "geslock"
                 evaluate true
                 when riprova continue
                 when ignora exit perform
                 end-evaluate
              else
                 exit perform
              end-if
           end-perform.

      ***---
       UPDATE-ASSOCIAZIONE.
           if tutto-ok
              move 0 to store-idx
              perform varying idx from 1 by 1
                        until idx > 78-TotIdx
                 if noco-anno-cont(idx)   not = 0 and
                    noco-numero-cont(idx) not = 0
                    if noco-anno-cont(idx)   = cnt-anno and
                       noco-numero-cont(idx) = cnt-numero
                       move idx to store-idx
                       set trovato to true
                       exit perform
                    end-if
                 else
                    if store-idx = 0
                       move idx to store-idx
                    end-if
                 end-if
              end-perform
              |23/05/2012
              if store-idx  > 0 and 
                 store-idx <= 5
                 if not trovato
                    move cnt-anno   to noco-anno-cont(store-idx)
                    move cnt-numero to noco-numero-cont(store-idx)
                    rewrite noco-rec invalid continue end-rewrite
                 else
                    if elimina
                       |la devo eliminare
                       move 0 to noco-anno-cont(store-idx)
                       move 0 to noco-numero-cont(store-idx)
                       rewrite noco-rec invalid continue end-rewrite
                    end-if
                 end-if
              end-if
              |23/05/2012
           end-if.

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
       SETTA-VIRGOLA.
           if KeyboardReleased
              set KeyboardSaved to true
      *       sostituisco il punto come virgola
              set environment "KEYSTROKE" to "DATA=44 46"
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
       VALORIZZA-NUMERO.
           initialize link-nambar.
           move tge-anno  to link-anno cnt-anno.

           set  link-gcontest to true.
           set  link-crea     to true.

           call   "nambar" using link-nambar.
           cancel "nambar".
           
           if link-status-nambar = -1 set errori       to true
           else                       move link-numero to cnt-numero
           end-if.
              
      ***---
       AFTER-BUF-TO-FLD.
           if inserimento
              move lab-anno-buf         to cnt-anno
              accept cnt-data           from century-date
              accept cnt-ora-creazione  from time
              accept cnt-data-creazione from century-date
              move user-codi            to cnt-utente-creazione
                                    
              initialize cnt-fattura cnt-nota
                         replacing numeric data by zeroes
                               alphanumeric data by spaces
           else
              move NotaChiave to cnt-chiave
              accept cnt-ora-modifica  from time
              accept cnt-data-modifica from century-date
              move user-codi            to cnt-utente-modifica
           end-if.

           perform SCARICA-COMBO-TIPO.
           move tipo-contest to cnt-tipo.

           perform SCARICA-COMBO-STATO-CONT.
           move stato-contest to cnt-stato.
                      
           move ef-cli-buf to cnt-cod-cli. 
           move ef-des-buf to cnt-prg-destino.

           move ef-data-buf to como-data.
           perform DATE-TO-FILE.
           move como-data   to cnt-data.

           move ef-data-b-buf to como-data.
           perform DATE-TO-FILE.
           move como-data   to cnt-data-bolla-cli.
      
      ***---
       AFTER-FLD-TO-BUF.
           inspect ef-cli-buf replacing leading x"30" by x"20".
           inspect ef-des-buf replacing leading x"30" by x"20".

           move cnt-data to como-data.
           perform DATE-TO-SCREEN.
           move como-data    to ef-data-buf.

           move cnt-data-bolla-cli to como-data.
           perform DATE-TO-SCREEN.
           move como-data    to ef-data-b-buf.

           move cnt-tipo to tipo-contest.
           perform CARICA-COMBO-TIPO.

           move cnt-stato to stato-contest.
           perform CARICA-COMBO-STATO-CONT.

           set  cli-tipo-C  to true.
           move cnt-cod-cli to cli-codice.
           read clienti no lock 
                invalid continue
            not invalid move cli-ragsoc-1  to lab-cli-buf
                        move cli-indirizzo to lab-ind-buf
                        move cli-localita  to lab-loca-buf
           end-read.

           if cnt-prg-destino not = 0
              move cnt-cod-cli     to des-codice
              move cnt-prg-destino to des-prog
              read destini no lock 
                   invalid continue
               not invalid move des-ragsoc-1  to lab-des-buf
                           move des-indirizzo to lab-ind-d-buf
                           move des-localita  to lab-loca-d-buf
              end-read
           end-if.

           if cnt-corriere not = 0
              move cnt-corriere to vet-codice
              read tvettori no lock
                   invalid continue
               not invalid move vet-descrizione to lab-vet-buf
              end-read
           end-if.

      ***---
       BEFORE-ACCEPT.
           accept data-oggi from century-date.
           perform INIT.
           perform RIEMPI-COMBO-TIPO.
           perform RIEMPI-COMBO-STATO-CONT.
           perform ABILITAZIONI.

           modify cbo-tipo,  value "Prezzo".
           modify cbo-stato, value "Aperta".
           move spaces        to tge-chiave.
           read tparamge invalid continue end-read.
           move 0  to v-fatt.
           initialize tor-rec.
           if modifica 
              perform CURRENT-RECORD
           end-if.
           if WK-LinkNumero not = 0 and 
              WK-LinkAnno   not = 0
              if inserimento
                 if LinkFatt
                    move WK-LinkAnno   to tor-anno-fattura
                    move WK-LinkNumero to tor-num-fattura
                    read tordini no lock key k-fattura
                         invalid continue 
                    end-read
                    move tor-num-fattura  to save-num-fattura
                    move tor-anno-fattura to save-anno-fattura
                    move tor-data-fattura to save-data-fattura
                 else
                    move WK-LinkAnno   to tno-anno-fattura
                    move WK-LinkNumero to tno-num-fattura
                    read tnotacr no lock key k-fattura
                         invalid continue 
                    end-read
                    move tno-num-fattura  to save-num-fattura
                    move tno-anno-fattura to save-anno-fattura
                    move tno-data-fattura to save-data-fattura
                 end-if
              end-if
              if LinkFatt or LinkNota
                 move 1 to v-fatt
              end-if
           end-if.
           if inserimento
              perform CURRENT-RECORD
           end-if.

           perform DISPLAY-SCREEN.

           move 78-ID-ef-data to control-id.
           move 4             to accept-control.

           string "Esercizio " delimited size
                  esercizio    delimited size
                  " ("         delimited size
                  esercizio-G2 delimited size
                  ")"          delimited size
                  into lab-esercizio-buf
           end-string.

           display lab-esercizio.

      ***---
       CHK-RICH-PRESSED.
           if mod-rich = 0
              if inserimento
                 if LinkFatt and tor-vettore not = 0
                    move tor-vettore to vet-codice ef-vet-buf
                    read tvettori no lock invalid continue end-read
                    move vet-descrizione to lab-vet-buf
                    display ef-vet lab-vet
                 end-if
              end-if
              move 1 to mod-rich
           else            
              move 0 to ef-data-rich-buf ef-data-ric-buf ef-vet-buf
              move spaces to lab-vet-buf
              move 0 to mod-rich
           end-if.
           display ef-data-rich ef-data-ric ef-vet lab-vet.

      ***---
       CHK-NOTA-PRESSED.
           if mod-nota = 0 
              move 1 to mod-nota
           else
              move spaces to ef-numero-buf   
              move 0 to ef-data-nota-buf 
                        ef-imp-nota-buf ef-reg-buf
              move 0 to mod-nota
           end-if.
           display ef-numero ef-data-nota ef-imp-nota ef-reg.

      ***---
      * Non posso andare sul campo numero se prima non ho valorizzato l'anno.
       BEFORE-CAMPO-NUMERO-NOTA.
           evaluate true
           when campo1
                inquire ef-anno1, value in como-anno
                if como-anno = 0
                   perform CANCELLA-COLORE
                   move 78-ID-ef-anno1 to control-id
                   move 4 to accept-control
                end-if
           when campo2
                inquire ef-anno2, value in como-anno
                if como-anno = 0
                   perform CANCELLA-COLORE
                   move 78-ID-ef-anno2 to control-id
                   move 4 to accept-control
                end-if
           when campo3
                inquire ef-anno3, value in como-anno
                if como-anno = 0
                   perform CANCELLA-COLORE
                   move 78-ID-ef-anno3 to control-id
                   move 4 to accept-control
                end-if
           when campo4
                inquire ef-anno4, value in como-anno
                if como-anno = 0
                   perform CANCELLA-COLORE
                   move 78-ID-ef-anno4 to control-id
                   move 4 to accept-control
                end-if
           when campo5
                inquire ef-anno5, value in como-anno
                if como-anno = 0
                   perform CANCELLA-COLORE
                   move 78-ID-ef-anno5 to control-id
                   move 4 to accept-control
                end-if
           end-evaluate.
      
      ***---
      ****** Non posso andare sul campo anno se prima non ho valorizzato il precedente.
       BEFORE-CAMPO-ANNO-NOTA.
      *****     evaluate true
      *****     when campo2
      *****          inquire ef-nota1, value in como-numero
      *****          if como-numero = 0
      *****             perform CANCELLA-COLORE
      *****             move 78-ID-ef-nota1 to control-id
      *****             move 4 to accept-control
      *****          end-if
      *****     when campo3
      *****          inquire ef-nota2, value in como-numero
      *****          if como-numero = 0
      *****             perform CANCELLA-COLORE
      *****             move 78-ID-ef-nota2 to control-id
      *****             move 4 to accept-control
      *****          end-if
      *****     when campo4
      *****          inquire ef-nota3, value in como-numero
      *****          if como-numero = 0
      *****             perform CANCELLA-COLORE
      *****             move 78-ID-ef-nota3 to control-id
      *****             move 4 to accept-control
      *****          end-if
      *****     when campo5
      *****          inquire ef-nota4, value in como-numero
      *****          if como-numero = 0
      *****             perform CANCELLA-COLORE
      *****             move 78-ID-ef-nota4 to control-id
      *****             move 4 to accept-control
      *****          end-if
      *****     end-evaluate.

      ***---
       CONTROLLO-NOTA-GIA-ASSOCIATA.
           if StoSalvando exit paragraph end-if.
           move 78-TotIdx to posti-liberi.
           set trovato to false.
           move tno-chiave to noco-chiave.
           read note-cont no lock 
                invalid continue 
            not invalid
                perform varying idx from 1 by 1 
                          until idx > 78-TotIdx
                   if noco-anno-cont(idx)   not = 0 or
                      noco-numero-cont(idx) not = 0
                      subtract 1 from posti-liberi
                      if inserimento
                         set trovato to true
                      else
                         if noco-anno-cont(idx)   not = cnt-anno or
                            noco-numero-cont(idx) not = cnt-numero
                            set trovato to true
                         end-if
                      end-if
                      exit perform
                   end-if
                end-perform
           end-read.
           if trovato
              if posti-liberi = 0
                 display message 
                "Raggiunto limite associazioni per questa contestazione"
                           title tit-err
                            icon 2
                 set errori to true
              else
                 display message "La nota indicata è già relazionata."
                          x"0d0a""Confermi?"
                           title titolo
                            type mb-yes-no
                            icon 2
                          giving scelta
                         default mb-no
                 if scelta = mb-no
                    set errori to true
                 end-if
              end-if
           end-if.

      ***---
       VALORIZZA-CAMPI-NOTA.
           move spaces  to des-localita.
           move 0       to como-data.

           set  cli-tipo-C to true.
           move tno-cod-cli to cli-codice.
           read clienti no lock invalid continue end-read.
           if tno-prg-destino not = 0
              move cli-codice      to des-codice
              move tno-prg-destino to des-prog
              read destini no lock invalid continue end-read
           else
              move cli-localita to des-localita
           end-if.
           if tno-data-fattura not = 0
              move tno-data-fattura to como-data
              perform DATE-TO-SCREEN
           end-if.
           move tno-num-fattura to como-numero.
           evaluate true
           when campo1
                move cli-ragsoc-1 to lab-cli1-buf
                move des-localita to lab-des1-buf
                move como-numero  to lab-n-fatt1-buf
                move como-data    to lab-data-fatt1-buf
           when campo2
                move cli-ragsoc-1 to lab-cli2-buf
                move des-localita to lab-des2-buf
                move como-numero  to lab-n-fatt2-buf
                move como-data    to lab-data-fatt2-buf
           when campo3
                move cli-ragsoc-1 to lab-cli3-buf
                move des-localita to lab-des3-buf
                move como-numero  to lab-n-fatt3-buf
                move como-data    to lab-data-fatt3-buf
           when campo4
                move cli-ragsoc-1 to lab-cli4-buf
                move des-localita to lab-des4-buf
                move como-numero  to lab-n-fatt4-buf
                move como-data    to lab-data-fatt4-buf
           when campo5
                move cli-ragsoc-1 to lab-cli5-buf
                move des-localita to lab-des5-buf
                move como-numero  to lab-n-fatt5-buf
                move como-data    to lab-data-fatt5-buf
           end-evaluate.

      ***---
       PB-RIFER-PRESSED.
           if LinkFatt
              initialize gordcvar-linkage
              move tor-numero to LinkNumero
              move tor-anno   to LinkAnno
              if LinkNumero not = 0
                 call   "gordcvar" using LK-BLOCKPGM, 
                                         USER-CODI, 
                                         1,
                                         gordcvar-linkage
                 cancel "gordcvar"
              end-if
           else
              move notacr-linkage to como-notacr-linkage
              initialize notacr-linkage
              move tno-numero  to NotaNumero
              move tno-anno    to NotaAnno
              move "NCPZ"      to Link-TipoNota |FITTIZIA
              if NotaNumero not = 0
                 call   "notavar" using LK-BLOCKPGM,
                                        USER-CODI,
                                        1,
                                        notacr-linkage
                 cancel "notavar"
              end-if
              move como-notacr-linkage to notacr-linkage
           end-if.
