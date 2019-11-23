      ***---
       CONTROLLA-CLIENTE.
           move low-value      to tor-chiave.
           move stordc-da-anno to tor-anno.
           move stordc-da-num  to tor-numero.
           start tordini key >= tor-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2

                    read tordini next at end exit perform end-read
                    if tor-anno   > stordc-a-anno or
                       tor-numero > stordc-a-num
                       exit perform
                    end-if
                    move low-value  to ror-rec
                    move tor-chiave to ror-chiave
                    perform LOOP-RIGHE-EVASIONE
                    
                    if omaggio
                       move tge-causale-omag to tor-causale
                       rewrite tor-rec invalid continue end-rewrite
                       if ok-messaggio
                          initialize geslock-linkage
                                     replacing numeric data by zeroes
                                          alphanumeric data by spaces
                          move 1 to geslock-v-salva
                          set geslock-omom to true
                          move tor-numero to tor-numero-x
                          inspect tor-numero-x replacing 
                                                 leading x"30" by x"20"
                          call "C$JUSTIFY" using tor-numero-x, "L"
                          inspect tor-numero-x replacing 
                                            trailing spaces by low-value
                          string "RICORDARSI D'INSERIRE " delimited size
                                 "DETTAGLI PROMOZIONE SU" delimited size
                                 " EVASIONE N.   "        delimited size
                                 tor-numero-x        delimited low-value
                                 into geslock-messaggio
                          end-string
                          perform 5 times
                             call   "geslock" using geslock-linkage
                             cancel "geslock"
                          end-perform
                       end-if
                    end-if

                    if mto-vettore not = 0
                       if tot-kg <= tge-limite-kg-OLD
                          if mto-ritira-no
                             set  cli-tipo-C  to true
                             move tor-cod-cli to cli-codice
                             read clienti no lock
                             if cli-nazione = "ITA"
                                move cli-tipo to tcl-codice
                                read ttipocli no lock 
                                     invalid continue 
                                end-read
                                if tcl-evasione-TRAD or
                                   tcl-evasione-ESTERO
                                   move tge-vettore-std-OLD 
                                     to tor-vettore
                                   rewrite tor-rec
                                end-if
                             end-if
                           end-if
                       else
                          if mto-ritira-no
                             move mto-vettore to tor-vettore
                          end-if
                       end-if
                    end-if

                    if piu-ordini
                       move "VARI" to tor-num-ord-cli
                       move 0      to tor-data-ordine
                       rewrite tor-rec
                    end-if

                    if bloccato
                       set tor-bloccato to true
                       rewrite tor-rec
                    end-if

                 end-perform
           end-start.

      ***---
       LOOP-RIGHE-EVASIONE.
           set omaggio    to true.
           move 0         to tot-kg.
           set piu-ordini to false.
           set bloccato   to false.
           move 0 to SaveAnno.

           move 0 to save-cli.
           move 0 to save-des.
           set tutto-ok to true.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if

                    if ( ror-prz-unitario  + ror-imp-consumo +
                         ror-imp-cou-cobat + ror-add-piombo  +
                         ror-imponib-merce ) > 0
                       set omaggio to false
                    end-if
                    
                    compute tot-kg  =
                            tot-kg  + 
                          ( ror-qta * 
                          ( ror-peso-utf + ror-peso-non-utf ))
           
                    if SaveAnno = 0
                       move ror-anno-master   to SaveAnno
                       move ror-numero-master to SaveNumero
                    end-if
           
                    if ror-anno-master   not = SaveAnno or
                       ror-numero-master not = SaveNumero
                       set piu-ordini to true
                    end-if

                    move ror-chiave-ordine-testa to mto-chiave
                    read mtordini no lock
                         invalid
                         set errori to true
                         perform 5 times
                            display message
                            "ERRORE GRAVE!!!"
                 x"0D0A""LA RIGA N. " ror-chiave
                 X"0D0A""HA PER CORRISPONDENZA IL MASTER N. " mto-chiave
                 X"0D0A""CHE RISULTA INESISTENTE!!!"
                 X"0D0A""CONTATTARE ASSISTENZA!!!"
                                      title tit-err
                                        icon 2
                         end-perform
                         exit perform
                     not invalid
                         if mto-bloccato
                            set bloccato to true
                         end-if
                    end-read
                    if save-cli = 0
                       move mto-cod-cli     to save-cli
                       move mto-prg-destino to save-des
                    end-if
                    if mto-cod-cli     not = save-cli or
                       mto-prg-destino not = save-des
                       set errori to true
                       perform 5 times
                          display message
                          "ERRORE GRAVE!!!"
              x"0D0A""LE RIGHE DELL'EVASIONE" tor-chiave
              X"0D0A""FANNO RIFERIMENTO A CLIENTE/DESTINO DIFFERENTI!!!"
              X"0D0A""CONTATTARE ASSISTENZA!!!"
                                    title tit-err
                                      icon 2
                       end-perform
                       exit perform
                    end-if
                 end-perform
           end-start.
           if tutto-ok
              if save-cli not = tor-cod-cli  or
                 save-des not = tor-prg-destino
                 move save-cli to tor-cod-cli
                 move save-des to tor-prg-destino
                 rewrite tor-rec invalid continue end-rewrite
              end-if
           end-if.       
           
      ***---
       DISPLAY-GIACENZA.
           perform TROVA-GIACENZA.
           compute giacenza = el-giacenza(idx-giac) + old-qta - como-qta
           move giacenza to el-giacenza(idx-giac).
              
           move giacenza to lab-giac-buf.
           display lab-giac.
           if giacenza > 0
              modify lab-giac, color 3
              modify lab1,     color 3
           else
              modify lab-giac, color 5
              modify lab1,     color 5
           end-if.
           
      ***---
        FROM-2-TO-3.
           modify gd3, mass-update 1.
           if old-qta not = como-qta
              inquire gd2(riga2, 1), hidden-data in g2-hidden

              move hid-chiave   to tmro-chiave
              read tmp-mrordini no lock
              move como-qta to tmro-qta
              rewrite tmro-rec
              
              perform DISPLAY-GIACENZA
              perform METTI-IN-GRIGLIA-3
           end-if.
           modify gd3, mass-update 0.                   
           
      ***---
        IMBALLI-QTA.
           if como-qta not = 0
              move 0 to resto
              if como-qta > tmro-qta-imballi
                 divide como-qta by tmro-qta-imballi giving ris
                                                  remainder resto
              else
                 move 1 to resto
                 move 0 to ris
              end-if

              if resto not = 0
                 add 1 to ris
              end-if

              compute tmro-qta = tmro-qta-imballi * ris
           else
              move como-qta to tmro-qta
           end-if.

           move tmro-qta to col2-qta como-qta.
           modify gd2(event-data-2, 7),  cell-data col2-qta.                   
           
      ***---
        METTI-IN-GRIGLIA-3.
           inquire gd2(riga2, 1), hidden-data in g2-hidden.
           move hid-chiave to tmro-chiave.
           inquire gd3, last-row in tot-righe3.
           move 0 to store-riga.
           move hid-chiave to tmeva-chiave.
           read tmp-evaart no lock
                invalid
                 
                 move tmro-chiave        to tmeva-chiave
                 move tmro-qta           to tmeva-qta
                 move tmro-cod-cli       to tmeva-cod-cli
                 move tmro-cliente       to tmeva-cliente
                 move tmro-prg-destino   to tmeva-prg-destino
                 move tmro-destino       to tmeva-destino
                 move tmro-ritira        to tmeva-ritira
                 move tmro-descr         to tmeva-articolo
                 move tmro-blister       to tmeva-blister
                 move tmro-qta-imballi   to tmeva-qta-imballi
                 move tmro-prg-chiave    to tmeva-prg-chiave
                 move tmro-bli-testa     to tmeva-bli-testa

                 move tmeva-chiave to mro-chiave
                 read mrordini no lock invalid continue end-read
                 move mro-omaggio to tmeva-omaggio
                 read mtordini no lock invalid continue end-read
                 if mto-bloccato set tmeva-bloccato-si to true
                 else            set tmeva-bloccato-no to true
                 end-if

                 write tmeva-rec invalid continue end-write
             not invalid
                 if como-qta = 0
                    delete tmp-evaart record
                 else
                    move tmro-descr to tmeva-articolo
                    move como-qta   to tmeva-qta
                    rewrite tmeva-rec
                 end-if
           end-read.

           perform GRID-EVASE.                   
           
      ***---
        READ-MRORDINI-LOCK.
           move mro-numero to numero-edit.
           initialize geslock-linkage.
           string "Ordine anno: " mro-anno, " - N. ", numero-edit
           x"0d0a""risulta in uso su altro terminale." delimited size
              into geslock-messaggio
           end-string. 

           perform until 1 = 2
              set RecLocked to false
              read mrordini with lock invalid continue end-read
              if not RecLocked
                 exit perform
              end-if

              move 1 to geslock-v-riprova
              move 1 to geslock-v-ignora
              move 0 to geslock-v-termina
              move "mtordini" to geslock-nome-file
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova continue
              when ignora  
                   read mrordini no lock
                   exit perform
              end-evaluate
           end-perform.                   
           
      ***---
       RIEMPI-GD2.
           move 2 to riga.
           modify gd2, mass-update = 1.
           move low-value  to tmro-rec.
           move mto-chiave to tmro-chiave-testa.
           start tmp-mrordini key >= tmro-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-mrordini next at end exit perform end-read

                    if tmro-chiave-testa not = mto-chiave
                       exit perform
                    end-if
                    set record-ok to false
                    perform VALIDA-RIGA

                    if record-ok

                       if tmro-si-blister
                          perform until 1 = 2
                             if tmro-qta-imballi not = 0
                                exit perform
                             end-if
                             read tmp-mrordini previous 
                                  at end exit perform
                             end-read
                          end-perform
                          perform AGGIUNGI-RIGA
                          perform until 1 = 2
                             read tmp-mrordini next 
                                  at end 
                                  read tmp-mrordini previous
                                  exit perform
                             end-read
                             if tmro-qta-imballi not = 0 or not 
                                tmro-si-blister
                                read tmp-mrordini previous
                                exit perform
                             end-if
                             perform AGGIUNGI-RIGA
                          end-perform
                       else
                          perform AGGIUNGI-RIGA
                       end-if

                    end-if
                 end-perform
           end-start.
           modify gd2, mass-update = 0.

      ***---
       AGGIUNGI-RIGA.
           move tmro-art     to col2-articolo.
           move tmro-descr   to col2-descrizione.
           if tmro-si-blister
              move "BLI"     to col2-imb
           else
              move tmro-imb  to col2-imb     
           end-if.
           move tmro-peso        to col2-peso.
           move tmro-qta-o       to col2-qta-o.
           move tmro-qta-e       to col2-qta-e.
           move tmro-qta         to col2-qta.

           
           move tmro-prg-chiave  to hid-prg-chiave.
           move tmro-chiave      to hid-chiave.
           move tmro-blister     to hid-blister.
           move tmro-qta-imballi to hid-qta-imballi.

           modify gd2(riga,  1), hidden-data g2-hidden.

           modify gd2(riga,  1), cell-data col2-articolo.
           modify gd2(riga,  2), cell-data col2-descrizione.
           modify gd2(riga,  3), cell-data col2-imb.
           modify gd2(riga,  4), cell-data col2-peso.
           modify gd2(riga,  5), cell-data col2-qta-o.
           modify gd2(riga,  6), cell-data col2-qta-e.
           modify gd2(riga,  7), cell-data col2-qta.

           move hid-chiave to mro-chiave.
           read mrordini no lock 
                invalid
                perform 5 times
                   display message
                            "ERRORE GRAVE!!!"
                 X"0D0A""MASTER CORRISPONDENZA " mro-chiave
                 X"0D0A""RISULTA INESISTENTE!!!"
                 X"0D0A""CONTATTARE ASSISTENZA!!!"
                           title tit-err
                            icon 2
                end-perform
                move 0 to lab-evadi-dal-buf
            not invalid
                if mro-evadi-dal > data-oggi
                   modify gd2(riga, 7) cell-color = 449
                else
                   modify gd2(riga, 7) cell-color = 481
                end-if
           end-read.

           add 1 to riga.                  
           

      ********---
      ***** EVADIBILITA-RIGA.
      *****     move art-scorta to sco-codice.
      *****     read tscorte no lock.
      *****
      *****     if mro-evadi-dal <= data-oggi
      *****        |VALORIZZAZIONE AUTOMATICA QUANTITA' ORDINATA!!!
      *****        if giacenza > 0 or sco-forzata-si
      *****           if sco-forzata-no
      *****              if giacenza > ( tmro-qta-o - tmro-qta-e )
      *****                 compute tmro-qta = tmro-qta-o - tmro-qta-e
      *****              else
      *****                 compute tmro-qta = giacenza
      *****              end-if
      *****           else
      *****              compute tmro-qta = tmro-qta-o - tmro-qta-e
      *****           end-if
      *****
      *****           if tmro-qta >= tmro-qta-imballi
      *****              compute ris = tmro-qta / tmro-qta-imballi
      *****              compute tmro-qta = ris * tmro-qta-imballi
      *****           else
      *****              move 0 to tmro-qta-imballi
      *****              move 0 to tmro-qta
      *****           end-if
      *****
      *****           if tmro-qta > 0
      *****              subtract tmro-qta from giacenza
      *****              rewrite tmro-rec  
      *****              move tmro-qta     to col2-qta
      *****              move giacenza     to el-giacenza(idx-giac)
      *****           
      *****              move tmro-chiave        to tmeva-chiave
      *****              move tmro-qta           to tmeva-qta
      *****              move tmro-cod-cli       to tmeva-cod-cli
      *****              move tmro-cliente       to tmeva-cliente
      *****              move tmro-prg-destino   to tmeva-prg-destino
      *****              move tmro-destino       to tmeva-destino
      *****              move tmro-ritira        to tmeva-ritira
      *****              move tmro-descr         to tmeva-articolo
      *****              move tmro-blister       to tmeva-blister
      *****              move tmro-qta-imballi   to tmeva-qta-imballi
      *****              move tmro-prg-chiave    to tmeva-prg-chiave
      *****
      *****              move tmeva-chiave to mro-chiave
      *****              read mrordini no lock invalid continue end-read
      *****              move mro-omaggio to tmeva-omaggio
      *****              read mtordini no lock invalid continue end-read
      *****              if mto-bloccato set tmeva-bloccato-si to true
      *****              else            set tmeva-bloccato-no to true
      *****              end-if
      *****              write tmeva-rec
      *****
      *****           end-if
      *****        end-if
      *****     end-if.            
           
      ***---
       SPOSTAMENTO-1.
           if event-data-2 > 1
              modify gd1, y = event-data-2, start-y = event-data-2,
                          x = 1,            start-x = 7,
                          region-color = 144

              inquire gd1(event-data-2, 1), cell-data in mto-anno
              inquire gd1(event-data-2, 2), cell-data in mto-numero

              modify  gd2, reset-grid 1
              perform GD2-CONTENT

              perform RIEMPI-GD2

              move 2 to event-data-2
              perform SPOSTAMENTO-2
           end-if.

      ***---
       SPOSTAMENTO-2.
           if event-data-2 > 1
              modify gd2, cursor-y event-data-2, cursor-x 7
              set event-action to event-action-fail
              modify gd2, y = event-data-2, start-y = event-data-2,
                          x = 1,            start-x = 6,
                          region-color = 144

              inquire gd2(event-data-2, 1), hidden-data in g2-hidden
              move hid-prg-chiave to tmro-prg-chiave
              move 0 to old-qta como-qta
              perform DISPLAY-GIACENZA

              initialize prg-chiave replacing numeric data by zeroes
                                         alphanumeric data by spaces
              move hid-art to prg-cod-articolo
              read progmag no lock invalid continue end-read
              move prg-ordinato-1 to lab-ord-buf
              display lab-ord

              move hid-chiave to mro-chiave
              read mrordini no lock 
                   invalid
                   perform 5 times
                      display message
                               "ERRORE GRAVE!!!"
                    X"0D0A""MASTER CORRISPONDENZA " mro-chiave
                    X"0D0A""RISULTA INESISTENTE!!!"
                    X"0D0A""CONTATTARE ASSISTENZA!!!"
                              title tit-err
                               icon 2
                   end-perform
                   move 0 to lab-evadi-dal-buf
               not invalid
                   move mro-evadi-dal to como-data
                   perform DATE-TO-SCREEN
                   move como-data to lab-evadi-dal-buf
              end-read
              display lab-evadi-dal
           end-if.
           
      ***---
       SPOSTAMENTO-ART.                  
           inquire gd-art, last-row in tot-righe.
           if event-data-2 > tot-righe
              add 1 to tot-righe giving riga
           else
              move event-data-2 to riga
           end-if.
                                    
           modify gd-art, cursor-y riga, cursor-x 1
           set event-action to event-action-fail.

           modify gd-art, start-x = 1, x = 2,
                          start-y = riga, y = riga,
                          region-color = 144.       
                          
      ***---
       VALIDA-RIGA.
           if tot-articoli not = 0
              set record-ok to false
              set idx-art to 1
              search el-articolo
              when tmro-art = el-articolo(idx-art)
                   set record-ok to true
              end-search
           else
              set record-ok to true
           end-if.             
           
      ***---
       VALORIZZA-NUMERO.
           move spaces to tge-codice
           read tparamge no lock

           move tge-anno     to link-anno.

           set  link-gordc    to true.
           set  link-crea     to true.

           call   "nambar" using link-nambar.
           cancel "nambar".
           
           if primo-numero = 0
              move link-numero to primo-numero
           end-if.
           move link-numero    to ultimo-numero.
           
           if link-status-nambar = -1 set errori       to true
           else                       move link-numero to tor-numero
           end-if.

           
      ***---
       AFTER-PROGRAM.
           set environment "KEYSTROKE" to "DATA=46   46".
           set environment "KEYSTROKE" to "DATA=44   44".

           if path-tmp-mrordini not = spaces
              close tmp-mrordini
              delete file tmp-mrordini
           end-if.

           if path-tmp-evaart not = spaces
              close tmp-evaart
              delete file tmp-evaart
           end-if.

           if path-tmp-blis-eva not = spaces
              close tmp-blis-eva
              delete file tmp-blis-eva
           end-if.

           destroy Verdana10I-Occidentale.
           destroy REGALO-BMP.
           destroy BLOCCATO-EVA-BMP.
           destroy REGALO2-BMP.
           destroy BLOCCATO-EVA2-BMP.
           
      ***---
       BEFORE-ACCEPT.
           set ApplicaFiltri  to false.
           perform CALCOLA-DATE.

           move data-evasione to como-data.
           perform DATE-TO-SCREEN.
           move como-data to lab-data-buf.
           display lab-data.
           |DATA EVASIONE: Confronto per "EVADI DAL" che dev'essere <=
                                   
           set prima-volta  to true.
           set primo-lancio to true.
           modify gd1, mass-update 1.
           modify gd2, mass-update 1.
           modify gd3, mass-update 1.
           perform PB-APPLICA-LINKTO.
           modify gd1, mass-update 0.
           modify gd2, mass-update 0.
           modify gd3, mass-update 0.
           set primo-lancio to false.
                                                     
           accept versione-evasione from environment "VERSIONE_EVASIONE"
           set ingresso to true.
           perform SCRIVI-LOCKFILE.                  

      ***---
       IMPEGNATO-RIGA.
           move tmeva-chiave  to mro-chiave.
           perform READ-MRORDINI-LOCK.

           initialize link-wprogmag.

           compute como-qta-e = mro-qta-e + tmeva-qta.
           |SE EVADO PIU' DELL'ORDINATO (SICURAMENTE LA QTA EVASA
           |POTRA' ESSERE SOLAMENTE AUMENTATA NON DIMINUITA DATO
           |CHE IN QUETO PROGRAMMA NON POSSO STORNARE NULLA MA 
           |SOLO EVADERE )
           |--->IMPEGNATO IN AUMENTO!!!!!
           if como-qta-e > mro-qta
              |SE ERA GIA' TUTTO EVASO NON ENTRA NEMMENO IN GIOCO
              |PERCIO' AUMENTO L'IMPEGNATO SE EVADO PIU' DI
              |QUANTO AVEVO ORDINATO
              compute link-valore = como-qta-e - mro-qta
              set link-update-um      to true
              set link-update         to true
              move mro-prg-chiave     to link-key
              move tor-causale        to link-causale
              move "0100000000000000" to link-array
              move user-codi          to link-user
              call   "wprogmag"   using link-wprogmag
              cancel "wprogmag" 
           end-if.

           perform DIREZIONA-IMPEGNATO.

           |IMPEGNATO GIA' LAVORATO
           move 0 to link-valore.   
              
           set link-update-um   to true.
           set link-update      to true.
           move mro-prg-chiave  to link-key.
           move tor-causale     to link-causale.

           |STORNO TUTTO
           move mro-qta-e          to link-impegnato.
           move "0100000000000000" to link-array.
           move user-codi          to link-user.
           call   "wprogmag"    using link-wprogmag.
           cancel "wprogmag".

           |DIMINUISCO LA NUOVA EVASIONE (MAX QTA ORDINATA)
           if como-qta-e > mro-qta
              move mro-qta    to link-impegnato
           else
              move como-qta-e to link-impegnato
           end-if.
           move "0000000000000000" to link-array.
           move -1                 to multiplyer(2).
           move user-codi          to link-user.
           call   "wprogmag"    using link-wprogmag.
           cancel "wprogmag".

           if not RecLocked
              add tmeva-qta to mro-qta-e
              rewrite mro-rec invalid continue end-rewrite
              unlock mrordini all records
           end-if.
      
      ***---
       VALORIZZA-RIGA-BLISTER.
           move tmeva-chiave to mro-chiave.
           read mrordini no lock.
           move tmeva-prg-chiave to prg-chiave.
           read progmag no lock.
           perform VALORIZZA-RIGA-EVASIONE.

      ***---
       SPLITTA-ORDINE-NORMALE.
           if collo > 0
              compute tmeva-qta = 
                      tmeva-qta-imballi * ( collo - colli-usati )
              perform VALORIZZA-RIGA-EVASIONE
           end-if.
           perform SCRIVI-TESTA.

      ***---
       SPLITTA-ORDINE-BLISTER.
           if collo > 0
              perform varying idx-c from 1 by 1 
                        until idx-c > componenti
                 move tab-prg-chiave(idx-c) to tmeva-prg-chiave
                 compute tmeva-qta = collo - colli-usati
                 if idx-c = 1
                    move tmeva-qta          to tmeva-qta-imballi
                    set  tmeva-bli-testa-si to true
                 else
                    move 0                  to tmeva-qta-imballi
                    set  tmeva-bli-testa-no to true
                 end-if
                 move tab-chiave-master(idx-c) to tmeva-chiave
                 perform VALORIZZA-RIGA-BLISTER
              end-perform
           end-if.
           perform SCRIVI-TESTA.

      ***---
       SCRIVI-TESTA.
           if idx = 1
              set modifica to true
              perform SCRIVI-LOCKFILE
              move 0 to primo-numero ultimo-numero
           end-if.

           initialize SaveChiave.
           initialize tor-rec replacing numeric data by zeroes
                                   alphanumeric data by spaces.
           perform VALORIZZA-NUMERO.

           if errori
              set contatore-lock to true
              set ingresso to true
              perform SCRIVI-LOCKFILE
           else
              move 0 to store-riga
              move tmeva-chiave-testa to mto-chiave
              read mtordini no lock

              move tge-anno        to tor-anno
              move mto-causale     to tor-causale tca-codice
              read tcaumag no lock invalid continue end-read
              move mto-cod-cli     to tor-cod-cli
              move mto-prg-destino to tor-prg-destino
              move mto-num-ord-cli to tor-num-ord-cli
              move mto-data-ordine to tor-data-ordine
              accept tor-data-passaggio-ordine from century-date
              move mto-cod-agente  to tor-cod-agente
              move mto-cod-pagamento to tor-cod-pagamento
              move mto-cod-ese-iva to tor-cod-ese-iva
              set tor-no-ric-ago   to true
              set tor-no-ric-dic   to true
              set tor-attivo       to true
              move mto-vettore     to tor-vettore
              if mto-data-note1 < data-oggi
                 move "URGENTE!!!"    to tor-note1
              else
                 move mto-note1       to tor-note1
                 move mto-data-note1  to tor-data-note1
              end-if
              move mto-note2       to tor-note2
              move mto-note3       to tor-note3
              move mto-note4       to tor-note4

              move mto-cod-cli to rec-codice
              read recapiti no lock invalid continue end-read
              move rec-invio to tor-invio
              if tor-invio = spaces
                 set tor-invio-manuale to true 
              end-if
                        
              set tor-no-ric-ago          to true
              set tor-no-ric-dic          to true                   
              set tor-bolla-no-prenotata  to true
              set tor-fatt-no-prenotata   to true
              set tor-manuale             to true
              set tor-no-agg-contab       to true
              set tor-ordine              to true
              set tor-da-ordine-si        to true
              move tca-contropartita      to tor-contropartita
              accept tor-data-creazione from century-date
              accept tor-ora-creazione  from time
              move user-codi              to tor-utente-creazione
              move mto-stato-attivazione  to tor-stato
              if EvasioneTradizionale
                 set tor-ev-auto-trad to true
              else
                 set tor-ev-auto-gdo  to true
              end-if
              write tor-rec invalid continue end-write
              move tor-chiave to el-chiave(idx)
              if stordc-da-anno = 0 or
                 stordc-da-num  = 0
                 move tor-anno   to stordc-da-anno
                 move tor-numero to stordc-da-num
              end-if
              move tor-anno      to stordc-a-anno
              move tor-numero    to stordc-a-num
              add  1 to idx
              move 0 to tot-utf
           end-if.

      ***---
       SCRIVI-RIGHE.
           perform CONTROLLO-500KG-UTF.
           if tutto-ok and not CreatoSplit
              perform VALORIZZA-RIGA-EVASIONE
           end-if. 

      ***---
       VALORIZZA-RIGA-EVASIONE.
           move tor-anno   to ror-anno.
           move tor-numero to ror-num-ordine.
           add 1 to store-riga.
           move store-riga to ror-num-riga.
           move tmeva-qta  to ror-qta.
           move tmeva-prg-chiave to ror-prg-chiave.

           move mro-prz-unitario  to ror-prz-unitario.
           move mro-imp-consumo   to ror-imp-consumo.
           move mro-imp-cou-cobat to ror-imp-cou-cobat.
           move mro-add-piombo    to ror-add-piombo.
           move mro-imponib-merce to ror-imponib-merce.

           move ror-prg-cod-articolo to ror-cod-articolo.
           move tmeva-blister     to ror-blister.

           move prg-peso-utf      to ror-peso-utf.
           move prg-peso-non-utf  to ror-peso-non-utf.

           if ror-si-blister
              if tmeva-bli-testa-si
                 move ror-qta          to ror-num-colli
                 compute ror-qta-imballi = 
                         ror-qta / ( mro-qta / mro-qta-imballi )
              else
                 move 0 to ror-num-colli
                           ror-qta-imballi
              end-if
              move mro-bli-codice to ror-bli-codice
           else
              move 0 to ror-bli-codice
              move tmeva-qta-imballi to ror-qta-imballi
              compute ror-num-colli = ror-qta / ror-qta-imballi
           end-if.
           move mro-cod-imballo  to ror-cod-imballo.
           move mro-des-imballo  to ror-des-imballo.
           move mro-cod-art-cli  to ror-cod-art-cli.
           move mro-cod-iva      to ror-cod-iva.
           set  ror-attivo       to true.
           accept ror-data-creazione from century-date.
           accept ror-ora-creazione  from time.
           move user-codi        to ror-utente-creazione.

           move mro-chiave-testa to ror-chiave-ordine-testa.
           move mro-progr        to ror-progr-master.
           if mro-prz-unitario > 99999
              set ror-bloccato to true
           end-if.
           if ( ror-prz-unitario  + ror-imp-consumo +
                ror-imp-cou-cobat + ror-add-piombo  +
                ror-imponib-merce ) = 0
              set ror-si-omaggio to true
           else
              set ror-no-omaggio to true
           end-if.
           write ror-rec invalid continue end-write.

      ***---
       CONTROLLO-500KG-UTF.
           set CreatoSplit to false.
           set tutto-ok    to true.
           move tmeva-prg-chiave to prg-chiave.
           read progmag no lock
                invalid
                set errori to true
                display message "ERRORE GRAVE!!!!"
                      x"0d0a""PROGRESSIVO NON VALIDO " prg-chiave
                      x"0d0a""CONTATTARE ASSISTENZA"
                          title tit-err
                          icon 3
           end-read.

           if tutto-ok
              perform IMPEGNATO-RIGA
              if des-deposito-utf not = "S"
                 if tmeva-si-blister
                    set CreatoSplit to true
                    |PRIMO GIRO CALCOLO IL PESO
                    move tmeva-qta    to tot-colli
                    move prg-peso-utf to como-utf
                    initialize tab-componenti
                    move 1 to componenti
                    move tmeva-qta         
                      to tab-qta(componenti)
                    move tmeva-qta-imballi 
                      to tab-qta-imballi(componenti)
                    move tmeva-prg-chiave  
                      to tab-prg-chiave(componenti)
                    move mro-chiave        
                      to tab-chiave-master(componenti)
                    perform until 1 = 2
                       read tmp-evaart next 
                            at end
                            set ExitPerform to true
                            exit perform
                       end-read
                       if tmeva-bli-testa-si or not tmeva-si-blister
                          read tmp-evaart previous
                          exit perform
                       end-if
                       perform IMPEGNATO-RIGA
                       add 1 to componenti
                       move tmeva-qta         
                         to tab-qta(componenti)
                       move tmeva-qta-imballi 
                         to tab-qta-imballi(componenti)
                       move tmeva-prg-chiave  
                         to tab-prg-chiave(componenti)
                       move mro-chiave        
                         to tab-chiave-master(componenti)

                       move tmeva-prg-chiave to prg-chiave
                       read progmag no lock
                       add prg-peso-utf to como-utf
                    end-perform
                    compute tot-utf = tot-utf + como-utf * tmeva-qta
                    if tot-utf > 500 
                       compute tot-utf = tot-utf - como-utf * tmeva-qta
                       move 0 to colli-usati
                       perform varying collo from 1 by 1 
                                 until collo > tot-colli
                          compute tot-utf = tot-utf + como-utf
                          if tot-utf > 500
                             subtract 1 from collo
                             set CreatoSplit to true
                             perform SPLITTA-ORDINE-BLISTER
                             move collo to colli-usati
                          end-if
                       end-perform
                       if tot-colli > colli-usati
                          perform varying idx-c from 1 by 1 
                                    until idx-c > componenti
                             move tab-prg-chiave(idx-c) 
                               to tmeva-prg-chiave
                             compute tmeva-qta = tot-colli - colli-usati
                             if idx-c = 1
                                move tmeva-qta to tmeva-qta-imballi
                                set  tmeva-bli-testa-si to true
                             else
                                move 0         to tmeva-qta-imballi
                                set  tmeva-bli-testa-no to true
                             end-if
                             move tab-chiave-master(idx-c) 
                               to tmeva-chiave
                             perform VALORIZZA-RIGA-BLISTER
                          end-perform
                       end-if
                    else
                       |CI STA TUTTO
                       subtract 1 from componenti
                       perform componenti times
                          read tmp-evaart previous
                       end-perform
                       perform VALORIZZA-RIGA-BLISTER
                       perform componenti times
                          read tmp-evaart next
                          perform VALORIZZA-RIGA-BLISTER
                       end-perform
                    end-if
                 else
                    move 0 to como-qta
                    compute tot-colli = tmeva-qta / tmeva-qta-imballi
                    compute como-utf  = tmeva-qta-imballi * prg-peso-utf

                    move 0 to colli-usati
                    perform varying collo from 1 by 1 
                              until collo > tot-colli
                       compute tot-utf = tot-utf + como-utf
                       if tot-utf > 500
                          subtract 1 from collo
                          set CreatoSplit to true
                          perform SPLITTA-ORDINE-NORMALE
                          move collo to colli-usati
                       end-if
                    end-perform
                    if CreatoSplit |LA PARTE RESTANTE INFERIORE a 500 KG
                       if tot-colli > colli-usati
                          compute tmeva-qta = 
                                  tmeva-qta-imballi * 
                                ( tot-colli - colli-usati)
                          perform VALORIZZA-RIGA-EVASIONE
                       end-if
                    end-if   
                 end-if
              end-if

           end-if. 

      ***---
       FINE-OPERAZIONE.
           if not contatore-lock and stordc-da-num not = 0
              modify screen2-handle, visible false 

              perform SCR-FINE-OPEN-ROUTINE
                              
              set ok-messaggio to true
              perform CONTROLLA-CLIENTE

              set stordc-evasioni to true
              call   "stordcp" using stordcp-limiti
              cancel "stordcp"
                                              
              set no-mail to true
              set ordine-evaso to true
              perform varying idx-m from 1 by 1 
                        until idx-m > tot-idx-m
                 move el-chiave-m(idx-m) to mto-chiave
                 perform AGGIORNA-STATO-MASTER
                 perform CHIUDI-MASTER
              end-perform

              set ok-messaggio to false
              perform CONTROLLA-CLIENTE
      
              perform DELETE-LOCKFILE
              move 27 to key-status
           end-if.

      ***---
       SCR-FINE-BEFORE-ACCEPT.
           compute tot-ordini = ultimo-numero - primo-numero + 1.
           move primo-numero  to primo-numero-z.
           move ultimo-numero to ultimo-numero-z.
           move tot-ordini    to tot-ordini-z.
           display scr-fine.

      ***---
       ASSOCIA-OMAGGIO.
           initialize occ-omaggio.
           inquire gd3, last-row in tot-righe.
           if tot-righe > 1
              perform varying riga from 2 by 1
                        until riga > tot-righe
                 inquire gd3(riga,  1), hidden-data in g2-hidden
                 if hid-no-omaggio
                    move 1 to el-sw-valore(hid-prog)
                 end-if
              end-perform
           end-if.

      ***---
       EVASIONE-INTERA.
           move low-value to tmeva-rec.
           start tmp-evaart key >= tmeva-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-evaart next at end exit perform end-read
                    if tmeva-promo-no
                       move low-value to mro-rec
                       move tmeva-chiave-testa to mto-chiave
                       read mtordini no lock
                       if mto-evasione-intera-si
                          if mto-chiave not = save-chiave-intera
                             add 1 to idx-intero
                             move mto-chiave 
                               to save-chiave-intera
                             move mto-chiave 
                               to el-chiave-intero(idx-intero)
                          end-if
                          move tmeva-chiave to mro-chiave
                          read mrordini no lock 
                               invalid continue 
                           not invalid
                               if mro-qta > ( mro-qta-e + tmeva-qta )
                                  move mro-chiave to tmro-chiave
                                  read tmp-mrordini no lock
                                       invalid continue
                                   not invalid 
                                       set idx-giac to 1
                                       search el-riga
                                       at end continue
                                       when tmeva-prg-chiave = 
                                            el-prg-chiave(idx-giac)
                                            add tmeva-qta 
                                             to el-giacenza(idx-giac)
                                       end-search
                                       move 0 to tmro-qta
                                       rewrite tmro-rec
                                  end-read
                                  delete tmp-evaart record
                                         invalid continue
                                  end-delete
                               else
                                  add 1 to el-righe-ok(idx-intero)
                               end-if
                          end-read
                       end-if
                    end-if
                 end-perform
           end-start.
           move 0 to righe-tot.
           move idx-intero to tot-idx-intero.
           perform varying idx-intero from 1 by 1 
                     until idx-intero > tot-idx-intero
              move low-value to mro-rec
              move el-chiave-intero(idx-intero) to mro-chiave-testa
              start mrordini key >= mro-chiave
              perform until 1 = 2
                 read mrordini next at end exit perform end-read
                 if mro-chiave-testa not = el-chiave-intero(idx-intero)
                    exit perform 
                 end-if
                 add 1 to righe-tot
              end-perform
              if righe-tot not = el-righe-ok(idx-intero)
                 move low-value                    to tmeva-rec
                 move el-chiave-intero(idx-intero) to tmeva-chiave-testa
                 start tmp-evaart key >= tmeva-chiave
                       invalid continue
                   not invalid
                       perform until 1 = 2
                          read tmp-evaart next 
                               at end exit perform 
                          end-read
                          if tmeva-chiave-testa not =
                             el-chiave-intero(idx-intero)
                             exit perform
                          end-if
                          delete tmp-evaart record
                                 invalid continue
                          end-delete
                       end-perform
                  end-start
               end-if
               move 0 to righe-tot
           end-perform.

      ***---
       OPEN-OUTPUT-TMP-EVAART.
           if path-tmp-evaart not = spaces
              close       tmp-evaart
              delete file tmp-evaart
           end-if.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-tmp-evaart.
           accept  path-tmp-evaart from environment "PATH_ST".
           inspect path-tmp-evaart replacing trailing 
                                   spaces by low-value.
           string  path-tmp-evaart delimited low-value
                   "TMP-EVAART_"   delimited size
                   como-data       delimited size
                   "_"             delimited size
                   como-ora        delimited size
                   ".tmp"          delimited size
                   into path-tmp-evaart
           end-string.
           open output tmp-evaart.
           close       tmp-evaart.
           open i-o    tmp-evaart.

      ***---
       OPEN-OUTPUT-TMP-BLIS-EVA.
           if path-tmp-blis-eva not = spaces
              close       tmp-blis-eva
              delete file tmp-blis-eva
           end-if.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-tmp-blis-eva.
           accept  path-tmp-blis-eva from environment "PATH_ST".
           inspect path-tmp-blis-eva replacing trailing 
                                     spaces by low-value.
           string  path-tmp-blis-eva delimited low-value
                   "TMP-BLIS-EVA_"   delimited size
                   como-data         delimited size
                   "_"               delimited size
                   como-ora          delimited size
                   ".tmp"            delimited size
                   into path-tmp-blis-eva
           end-string.
           open output tmp-blis-eva.
           close       tmp-blis-eva.
           open i-o    tmp-blis-eva.

      ***---
       CICLO-EVADIBILITA-NON-PROMO.
           move low-value to tmro-rec.
           set tmro-promo-no to true.
           start tmp-mrordini key >= k-promo
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-mrordini next at end exit perform end-read
                    if tmro-promo-si
                       exit perform
                    end-if
                    set record-ok to true
      *****              |Nel caso in cui un master abbia una riga non promo
      *****              |ma che faccia parte di un master promo non evadibile
      *****              set idx-eva-ko to 1
      *****              search el-master-eva-ko
      *****                at end set record-ok to true
      *****              when el-master-eva-ko(idx-eva-ko) = 
      *****                   tmro-chiave-testa
      *****                   set record-ok to false
      *****              end-search

                    |In questa fase non considero i blister non promo
                    |in quanto TUTTE le operazioni sui loro componenti
                    |(compreso storno della giacenza) vengono fatte
                    |nel paragrafo TRATTA-BLISTER
                    if record-ok
                       if not tmro-si-blister                  
                          set idx-giac to 1
                          search el-riga
                          at end continue
                          when tmro-prg-chiave = el-prg-chiave(idx-giac)
                               move el-giacenza(idx-giac) to giacenza
                          end-search
                          move tmro-art   to art-codice
                          read articoli   no lock
                          move art-scorta to sco-codice
                          read tscorte no lock
                          
                          if giacenza > 0 or sco-forzata-si
                             if sco-forzata-no
                                if giacenza > (tmro-qta-o - tmro-qta-e)
                                   compute tmro-qta = 
                                           tmro-qta-o - tmro-qta-e
                                else
                                   compute tmro-qta = giacenza
                                end-if
                             else
                                compute tmro-qta = 
                                        tmro-qta-o - tmro-qta-e
                             end-if
                          
                             if tmro-qta >= tmro-qta-imballi
                                compute ris = tmro-qta / 
                                              tmro-qta-imballi
                                compute tmro-qta = ris * 
                                                   tmro-qta-imballi
                             else
                                move 0 to tmro-qta-imballi
                                move 0 to tmro-qta
                             end-if
                          end-if
                       else
                          move tmro-qta to ris
                       end-if
                       
                       if tmro-qta > 0
                          if tmro-si-blister
                             move ris to tmro-bli-evasi
                          else
                             subtract tmro-qta 
                                 from el-giacenza(idx-giac)
                          end-if
                          move tmro-qta     to tmro-qta-evadibile
                          move tmro-qta     to col2-qta
                       
                          move tmro-chiave      to tmeva-chiave
                          move tmro-qta         to tmeva-qta
                          move tmro-cod-cli     to tmeva-cod-cli
                          move tmro-cliente     to tmeva-cliente
                          move tmro-prg-destino to tmeva-prg-destino
                          move tmro-destino     to tmeva-destino
                          move tmro-ritira      to tmeva-ritira
                          move tmro-descr       to tmeva-articolo
                          move tmro-blister     to tmeva-blister
                          move tmro-qta-imballi to tmeva-qta-imballi
                          move tmro-prg-chiave  to tmeva-prg-chiave
                          move tmro-promo       to tmeva-promo
                          move tmro-bli-testa   to tmeva-bli-testa
                       
                          move tmeva-chiave to mro-chiave
                          read mrordini no lock 
                               invalid continue 
                          end-read
                          move mro-omaggio to tmeva-omaggio
                          read mtordini no lock 
                               invalid continue 
                          end-read
                          if mto-bloccato 
                             set tmeva-bloccato-si to true
                          else            
                             set tmeva-bloccato-no to true
                          end-if
                          write tmeva-rec 
                                invalid rewrite tmeva-rec 
                          end-write
                          rewrite tmro-rec
                          
                          start tmp-mrordini key > k-promo
                                invalid exit perform
                          end-start 
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       PRF-COPY.
           copy "evaart-giang.cpy".
