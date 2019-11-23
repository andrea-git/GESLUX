      ***---
       ABILITA-COLLEGATI.
           if tor-anno-testa not = 0 or
              tor-num-testa  not = 0
              move 1 to v-reltor
              move 1 to NumBitmapDocColl
              display pb-colleg
              initialize path-tmp-tordini-zoom
              inspect path-tmp-tordini-zoom replacing trailing 
                                            spaces by low-value
              string  path-tmp-tordini-zoom delimited low-value
                      "tmp-tordini-zoom"  delimited size
                      como-data           delimited size
                      "_"                 delimited size
                      como-ora            delimited size
                      ".tmp"              delimited size
                      into path-tmp-tordini-zoom
              end-string
              open output tmp-tordini-zoom
              move tor-ordine-testa to rlt-chiave
              read reltor no lock invalid continue end-read
              if tor-chiave not = tor-ordine-testa
                 move rlt-anno        to tmp-tor-z-anno
                 move rlt-numero      to tmp-tor-z-numero
                 move tor-data-ordine to como-data
                 perform DATE-TO-SCREEN
                 move como-data       to tmp-tor-z-data-ordine
                 move tor-causale     to tmp-tor-z-causale
                 move tor-cod-cli     to tmp-tor-z-cod-cli       
                 move cli-ragsoc-1    to tmp-tor-z-cli-ragsoc-1  
                 move tor-prg-destino to tmp-tor-z-prg-destino   
                 move des-localita    to tmp-tor-z-des-localita  
                 move rlt-anno-bolla  to tmp-tor-z-anno-b        
                 move rlt-num-bolla   to tmp-tor-z-numero-b   
                 write tmp-tor-z-rec
              end-if
              perform varying idx from 1 by 1 until idx > 15
                 if rlt-anno-s(idx)   = 0 or
                    rlt-numero-s(idx) = 0
                    exit perform
                 end-if
                 if rlt-anno-s(idx)   not = tor-anno or
                    rlt-numero-s(idx) not = tor-numero
                    move rlt-anno-s(idx)   to tmp-tor-z-anno
                    move rlt-numero-s(idx) to tmp-tor-z-numero
                    move tor-data-ordine   to como-data
                    perform DATE-TO-SCREEN
                    move como-data         to tmp-tor-z-data-ordine
                    move tor-causale       to tmp-tor-z-causale
                    move tor-cod-cli       to tmp-tor-z-cod-cli       
                    move cli-ragsoc-1      to tmp-tor-z-cli-ragsoc-1  
                    move tor-prg-destino   to tmp-tor-z-prg-destino   
                    move des-localita      to tmp-tor-z-des-localita  
                    move rlt-anno-b-s(idx) to tmp-tor-z-anno-b        
                    move rlt-num-b-s(idx)  to tmp-tor-z-numero-b   
                    write tmp-tor-z-rec
                 end-if
              end-perform
              close tmp-tordini-zoom
           end-if.

      ***---
       CANCELLA-COLLEGATI.
           |Se l'ordine è relazionato ad altri dopo
           |lo split cancello tutte le evasioni
           move tor-ordine-testa to rlt-chiave
           read reltor no lock
                invalid continue
            not invalid
                |Sono su una bolla collegata ma non di
                |testa per cui troverò la bolla di testa in 
                |chiave e non tra gli elementi dell'occurs
                if tor-ordine-testa not = tor-chiave
                   move rlt-chiave to tor-chiave
                   delete tordini record
                          invalid continue 
                   end-delete
                   perform CANCELLA-RIGHE-COLLEGATE
                end-if   

                perform varying idx from 1 by 1
                          until idx > 15
                   move rlt-chiave-split(idx) to tor-chiave
                   delete tordini record
                          invalid continue 
                   end-delete
                   perform CANCELLA-RIGHE-COLLEGATE
                end-perform
                delete reltor record invalid continue end-delete
           end-read.

      ***---
       CANCELLA-RIGHE-COLLEGATE.
           move low-value  to ror-rec.
           move tor-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next 
                         at end exit perform 
                    end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    delete rordini record 
                           invalid continue 
                    end-delete
                 end-perform
           end-start.

      ***---
       SCRIVI-COLLEGATI.
           |Se l'ordine è relazionato ad altri dopo lo split
           |riporto i dati di testata in tutte le evasioni
           move tor-ordine-testa to rlt-chiave
           read reltor no lock
                invalid continue
            not invalid
                |Sono su una bolla collegata ma non di
                |testa per cui troverò la bolla di testa in 
                |chiave e non tra gli elementi dell'occurs
                if tor-ordine-testa not = tor-chiave
                   move rlt-chiave to tor-chiave
                   rewrite tor-rec 
                           invalid continue 
                   end-rewrite
                end-if 

                perform varying idx from 1 by 1
                          until idx > 15
                   if rlt-anno-s(idx)   = 0 or
                      rlt-numero-s(idx) = 0
                      exit perform
                   end-if
                   move rlt-chiave-split(idx) to tor-chiave
                   rewrite tor-rec 
                           invalid continue 
                   end-rewrite
                end-perform
           end-read.

      ***---
       PB-COLLEGATI-PRESSED.
           move path-tmp-tordini-zoom to ext-file.
           move "tmp-tordini-zoom"    to como-file.
           call "zoom-gt"          using como-file, 
                                         tmp-prg-z-rec
                                  giving stato-zoom
           end-call.
           cancel "zoom-gt".
           if stato-zoom = 0
              perform SALV-MOD
              if tutto-ok
                 
                 move 1 to NumBitmapDocColl
                 move 3 to NumBitmapDatiBolla
                 move tmp-tor-z-anno   to LinkAnno   of gordcvar-linkage 
                 move tmp-tor-z-anno   to tor-anno
                 move tmp-tor-z-numero to LinkNumero of gordcvar-linkage 
                 move tmp-tor-z-numero to tor-numero
                 set RicaricaGrid        to false
                 set MousePressed        to false
                 set SystemErrorOccurred to false
      
                 accept  data-oggi from century-date
                 perform INIT
                 perform LEGGI-ANNO

                 move 0 to e-modifica
                 modify tool-modifica, value e-modifica
                 unlock tordini all records
                 move LinkChiave  to tor-chiave
                 read tordini no  lock invalid continue end-read
                 if tor-manuale move 1 to e-man
                 else           move 1 to e-gui
                 end-if
                 move tor-cod-cli to cli-codice
                 set cli-tipo-C   to true
                 read clienti no  lock invalid continue end-read
                 if SiAssortimento
                    move spaces          to link-path-tmp-assorcli 
                    move cli-gdo         to link-gdo
                    move cli-codice      to link-cliente
                    move tor-prg-destino to link-destino
      
                    call   "wassorcli" using link-wassorcli
                    cancel "wassorcli"
      
                    if link-path-tmp-assorcli not = spaces
                       move link-path-tmp-assorcli to path-tmp-assorcli
                       open input tmp-assorcli
                    end-if
                 end-if
      
                 perform INIT-OLD-REC
                 perform ABILITA-TOOLBAR
                 move 1 to e-stampa
                 modify tool-stampa, enabled e-stampa, bitmap-number = 7
                 if tor-data-fattura not = 0 or tor-anno-fattura not = 0
                    move 0 to e-modifica
                    move BitmapEditDisabled to BitmapNumEdit
                    modify tool-modifica, enabled = e-modifica
                                          bitmap-number = BitmapNumEdit
                 end-if
                 move 0 to e-nuovo
                 modify tool-nuovo, enabled = e-nuovo
                 perform ABILITAZIONI
                 set StatusVisua to true
                 perform STATUS-BAR-MSG
      
                 move LinkChiave to tor-chiave
                 perform CANCELLA-FLAG
                 perform CURRENT-RECORD
                 perform DISPLAY-SCREEN
                 move 2 to save-riga event-data-2 riga
                 move 4 to event-data-1 colonna
                 perform SETTA-RIGA
                 move LinkNumero to numero-edit
                 display lab-numero
                 if livello-abil < 2 
                    set NonCambiareTab   to false
                 end-if

                 move 0        to imp-data
                 read timposte no lock invalid continue end-read
      
                 evaluate PgmChiamante 
                 when "selnota"
                      modify pb-notacr, bitmap-number = 1, 
                                        title "Creazione &Nota Credito"
                      move 1 to NumBitmapNotaCr
                 when "selbozza"
                      modify pb-notacr, bitmap-number = 3, 
                                        title "Creazione &Bozza"
                      move 3 to NumBitmapNotaCr
                 when "selcont"
                      modify pb-notacr, bitmap-number = 7, 
                                        title "Creazione Contestazioni"
                      move 7 to NumBitmapNotaCr
                 end-evaluate
                 perform ABILITA-COLLEGATI
              end-if
           end-if.

