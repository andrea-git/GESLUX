      ***---
       READ-RECORD-LOCK.
           set RecLocked to false.
           initialize geslock-linkage.
           move "tedi" to geslock-nome-file.

           set tutto-ok to true.
           read tedi with lock 
              invalid 
                 continue 
           end-read.

           if RecLocked
              move 1 to geslock-v-riprova
              move 0 to geslock-v-ignora
              move 1 to geslock-v-termina
              call   "geslock" using geslock-linkage
              cancel "geslock"
              evaluate true
              when riprova 
                   perform READ-RECORD-LOCK
              when ignora  
              when termina 
                   set errori to true
                   unlock tedi all records
              end-evaluate
           end-if.

      ***---
       SCREEN2-BEFOREACCEPT.
           move space  to pae-codice
           read PARAMEDI
              invalid
                 continue
           end-read

           perform LOAD-REDI

           perform SCREEN2-IUD-DISPLAY.
      
           modify Screen2-Handle, visible true.

      ***---
       SPOSTAMENTO.
           inquire gb-rec-b, last-row in tot-righe.
           if event-data-2 < 2 
              move 2 to event-data-2 
              move 2 to colonna
              modify gb-rec-b, cursor-x colonna
           end-if.
           if event-data-2 > tot-righe 
              move tot-righe to event-data-2 
              move 2 to colonna
              modify gb-rec-b, cursor-x colonna
           end-if.
           move event-data-1 to colonna
           if riga not = event-data-2 
              perform CAMBIA-FONT-RIGA
           end-if.

      ***--
       CAMBIA-FONT-RIGA.
           modify gb-rec-b(riga), ROW-FONT = small-font.

           modify gb-rec-b(riga, 1),
                  bitmap        = elemento-bmp,
                  bitmap-number = 1,
                  bitmap-width  = 16.

           move event-data-2 to riga.
           modify gb-rec-b(riga), ROW-FONT = font-evidenzia-griglia..

      *    cambio la label legenda
           inquire gb-rec-b(riga, 1), hidden-data = como-tipo-mov
           evaluate true
           when como-carico
                move 78-colore-ingresso  to col-stato-ordine
                move 78-tit-ingresso     to tit-stato-ordine
           when como-scarico
                move 78-colore-uscita    to col-stato-ordine
                move 78-tit-uscita       to tit-stato-ordine
           end-evaluate

           modify gb-rec-b, (riga, 1),
                  bitmap        = elemento-bmp,
                  bitmap-number = 2,
                  bitmap-width  = 16.

           modify lbl-stato title tit-stato-ordine
                            color col-stato-ordine.


      ***---
       LOAD-REDI.
           move 0 to tot-importi tot-carico tot-scarico.
           move 1 to riga.
           modify gb-rec-b mass-update = 1
           modify gb-rec-b reset-grid = 1
           perform GB-REC-B-CONTENT

           move tedi-chiave  to redi-tedi-chiave
           move low-value    to redi-tipo-movim 
                                redi-data-movimento
                                redi-tmo-chiave

           start redi key not < redi-k-dt-mov|redi-k-tipo-mov
              invalid
                 continue
              not invalid
                 perform until 1 = 2
                    read redi next no lock
                       at end
                          exit perform
                    end-read
                    if tedi-chiave not = redi-tedi-chiave
                       exit perform
                    end-if
                    if filtro-nc = zero
                       set rec-ok  to true
                    else
                       perform CONFRONTO-NC
                    end-if
                    if rec-ok
                       perform METTI-IN-GRIGLIA
                    end-if
                 end-perform
           end-start.

           modify gb-rec-b mass-update = zero

           move 1   to riga
           move 2   to event-data-2
           perform SPOSTAMENTO.

           move tot-importi  to tot-importi-ed.
           move tot-carico   to lab-carico-buf.
           move tot-scarico  to lab-scarico-buf.
           display lbl-tot-importi.
           display lab-carico
           display lab-scarico.

      ***---
       METTI-IN-GRIGLIA.
      *     move redi-mov-anno         to col-anno
      *     move redi-num-reg          to col-registro
      *     move redi-mov-prog-reg     to col-progressivo

           move redi-ragsoc           to col-ragsoc
           move redi-cpa              to col-cpa  
           move redi-nc               to col-nc   
           move redi-taric            to col-taric
           move redi-dac              to col-dac  

           move redi-qta-kg           to col-kg

           evaluate true
           when redi-carico
                set como-carico   to true
                add redi-qta-kg   to tot-carico
           when redi-scarico
                set como-scarico  to true
                add redi-qta-kg   to tot-scarico
           end-evaluate

           move redi-tipo-stoc        to col-stoccaggio
           move redi-mov-num-doc      to col-ddt

           move redi-data-movimento(1:4)  to como-data(5:4)
           move redi-data-movimento(5:2)  to como-data(3:2)
           move redi-data-movimento(7:2)  to como-data(1:2)
           move como-data             to col-data-mov


           move redi-mod-dt-doc(1:4)  to como-data(5:4)
           move redi-mod-dt-doc(5:2)  to como-data(3:2)
           move redi-mod-dt-doc(7:2)  to como-data(1:2)
           move como-data             to col-data-ddt
           move redi-naz              to col-nazione
           move redi-pos-fis          to col-pos-fiscale
           move redi-cau-movim        to col-cau
           move redi-imp-consumo      to col-consumo
           move redi-stato            to col-stato

           if redi-spedito
              move 2   to num-bitmap
           else
              move 1   to num-bitmap
           end-if

           move redi-tmo-anno         to col-anno-tmo
           move redi-tmo-numero       to col-num-tmo

           evaluate true
           when como-carico
                move 78-colore-ingresso  to col-stato-ordine
                move 78-tit-ingresso     to tit-stato-ordine
           when como-scarico
                move 78-colore-uscita    to col-stato-ordine
                move 78-tit-uscita       to tit-stato-ordine
           end-evaluate

           add 1 to riga

           modify gb-rec-b, record-to-add rec-grid-b.
           modify gb-rec-b(riga), row-color = col-stato-ordine.
           modify gb-rec-b(riga, 1) hidden-data = como-tipo-mov.
           modify gb-rec-b(riga, 2) hidden-data = redi-chiave.

           modify gb-rec-b(riga, 78-col-invio) hidden-data = num-bitmap.
           modify gb-rec-b(riga, 78-col-invio) bitmap = conferma-handle
                                               bitmap-width = 19
                                              bitmap-number = num-bitmap 

           

           modify gb-rec-b(riga, 1),
                  bitmap        = elemento-bmp,
                  bitmap-number = 1,
                  bitmap-width  = 16.

           add redi-imp-consumo to tot-importi.


      ***---
       X-Y.
           inquire gb-rec-b, cursor-y in riga,
                        cursor-x in colonna,
                        last-row in tot-righe.
 
      ***---
       VALORE-RIGA.
           inquire gb-rec-b(riga), record-data in rec-grid-b.
           inquire gb-rec-b(riga, 2) hidden-data = redi-chiave.

           read redi
              invalid
                 continue
           end-read

      *     move col-anno        to redi-mov-anno
      *     move col-registro    to redi-num-reg
      *     move col-progressivo to redi-mov-prog-reg 
           move col-cpa         to redi-cpa  
           move col-nc          to redi-nc   
           move col-taric       to redi-taric
           move col-dac         to redi-dac  

           inquire gb-rec-b(riga, 78-col-kg) cell-data = col-kg.
           move col-kg          to redi-qta-kg 
           move col-stoccaggio  to redi-tipo-stoc
           move col-ddt         to redi-mov-num-doc
           move col-data-ddt    to como-data
           move como-data(1:2)  to redi-mod-dt-doc(7:2)
           move como-data(3:2)  to redi-mod-dt-doc(5:2)
           move como-data(5:4)  to redi-mod-dt-doc(1:4)

           move col-nazione     to redi-naz
           move col-pos-fiscale to redi-pos-fis
           inquire gb-rec-b(riga, 78-col-consumo) 
                       cell-data = col-consumo.
           move col-consumo     to redi-imp-consumo.

      ***---
       CONTROLLO-RIGA.
           set tutto-ok to true.

           evaluate colonna
           when 78-col-kg
                if redi-qta-kg = zero
                   set errori to true
                   display message "Peso NON valido"
                           title tit-err
                           icon 2
                end-if
                move redi-qta-kg   to col-kg
                modify gb-rec-b(riga, 78-col-kg) cell-data col-kg

           when 78-col-stoccaggio
                if redi-tipo-stoc not = pae-tipo-sfuso and 
                   redi-tipo-stoc not = pae-tipo-conf
                   set errori to true
                   display message "Tipo stoccaggio NON valido"
                           title tit-err
                           icon 2
                end-if

           when 78-col-pos-fiscale
                if redi-pos-fis not = pae-imp-assolta and 
                   redi-pos-fis not = pae-imp-non-soggetta
                   set errori to true
                   display message "Posizione fiscale NON valida"
                           title tit-err
                           icon 2
                end-if

           when 78-col-data-ddt
                move col-data-ddt  to como-data
                perform DATE-FORMAT
                move como-data     to col-data-ddt
           when 78-col-ddt
           when 78-col-consumo
                continue
           end-evaluate.           

      ***---
       SALVA-RIGA.
           perform VALORE-RIGA
           move user-codi to redi-utente-ultima-modifica.
           accept redi-data-ultima-modifica from century-date
           accept redi-ora-ultima-modifica  from time.

           rewrite redi-rec
              invalid
                 continue
           end-rewrite.

      ***---
       EXP-DATI.
           move zero   to cont-exp
           accept como-data from century-date
      *    apro il file di export
           move pae-path-file-invio   to path-exp-edi
           open output exp-edi

           if status-exp-edi = "00"

              inquire gb-rec-b last-row tot-righe

              perform varying riga from 2 by 1 until riga > tot-righe
                 inquire gb-rec-b(riga, 78-col-invio) 
                                            hidden-data = num-bitmap
                 if num-bitmap = 1
                    inquire gb-rec-b(riga, 2) hidden-data = redi-chiave

                    read redi no lock
                       invalid
                          continue
                       not invalid
                          perform EXP-RECORD
                    end-read
                 end-if
              end-perform

              close exp-edi

              move cont-exp  to tot-mov

              perform SCR-FINE-OPEN-ROUTINE

           else
                 display message 
                         "Impossibile procedere!"
                  X"0d0a""Impossibile creare il file di export."
                  X"0d0a""Controllare la tabella dei parametri."
                          title titolo
                           icon 2
           end-if.
      

      ***---
       EXP-RECORD.
           move " |" to exe-pipe-1
                        exe-pipe-2
                        exe-pipe-3
                        exe-pipe-4
                        exe-pipe-5
                        exe-pipe-6
                        exe-pipe-7
                        exe-pipe-8
                        exe-pipe-9
                        exe-pipe-10
                        exe-pipe-11
                        exe-pipe-12
                        exe-pipe-13
                        exe-pipe-14
                        exe-pipe-15
                        exe-pipe-16
                        exe-pipe-17
                        exe-pipe-18
                        exe-pipe-19
                        exe-pipe-20
                        exe-pipe-21
                        exe-pipe-22
                        exe-pipe-23
                        exe-pipe-24
                        exe-pipe-25
                        exe-pipe-26

           move "olluda_b"   to exe-val-fisso


           move redi-data-movimento   to exe-data-rif

      *     3 SI TA01 Tipo richiesta X(1)
           move "I"          to exe-tipo-ope

      *     4 SI TA02 Identificativo registro: Tipo registro X(1)
           move pae-tipo-reg to exe-tipo-registro
      *     5 SI TA03 Identificativo registro: Codice Ufficio X(8)
           move pae-cod-ufficio to exe-cod-ufficio

      *     6 SI Identificativo registro: Anno protocollo (formato "AAAA") 9(4)
           move redi-mov-anno   to exe-anno-prot
                                   utf-anno.
      *     7 SI Identificativo registro: Numero protocollo X(10)
           initialize exe-num-registro
           inspect pae-registro replacing trailing space by low-value
           string pae-registro  delimited by low-value
                  "-"           delimited by size
                  redi-suf-reg  delimited by size
                  into exe-num-registro.


      *     8 COND CN11 Numero identificativo record 9(7)
      *     move zero   to exe-prog-registro
           move redi-prog       to exe-prog-registro

      *     9 COND CN01, TA13 Codice prodotto X(18)
           move redi-codi-prodotto to exe-cod-prod
           initialize exe-cod-prod

           if redi-cpa not = space
              move redi-cpa  to exe-cod-prod
           end-if
           if redi-nc not = zero
              inspect exe-cod-prod replacing trailing space by low-value
              if redi-nc(5:4) = "0000"
                 string exe-cod-prod  delimited by low-value
                        redi-nc(1:4)  delimited by size
                        into exe-cod-prod
              else
                 string exe-cod-prod  delimited by low-value
                        redi-nc       delimited by size
                        into exe-cod-prod
              end-if
              inspect exe-cod-prod replacing trailing low-value by space 
           end-if
           if redi-taric not = zero
              inspect exe-cod-prod replacing trailing space by low-value
              string exe-cod-prod  delimited by low-value
                     redi-taric       delimited by size
                     into exe-cod-prod
              inspect exe-cod-prod replacing trailing low-value by space 
           end-if
           if redi-dac not = space
              inspect exe-cod-prod replacing trailing space by low-value
              string exe-cod-prod  delimited by low-value
                     redi-dac      delimited by size
                     into exe-cod-prod
              inspect exe-cod-prod replacing trailing low-value by space 
           end-if

      *     10 SI Quantità in chilogrammi 9(11)V9(3)
           move redi-qta-kg  to exe-qta-kg.
      *     11 NO Quantità in litri a 15°C 9(11)V9(3)
           move zero   to exe-qta-litri.
      *     12 NO Densità a 15°C in aria (chilogrammi/litro) 9(1)V 9(4)
           move zero to exe-qta-densita.

      *     13 COND CN01, TA04 Tipo stoccaggio X(1)
           move redi-tipo-stoc  to exe-tipo-stoc
      *     14 Quantità nominale confezioni 9(4)V9(3)
           move zero   to exe-qta-conf
      *     15 COND Numero delle confezioni 9(7)
           move zero   to exe-num-conf

      *     16 COND CN11, TA05 Tipo documento / verbale X(3)
      *     move pae-tipo-doc to exe-tipo-doc
           move redi-tipo-doc   to exe-tipo-doc
      *     17 COND CN03, CN11 Numero documento / verbale X(21)
           move redi-mov-num-doc   to exe-num-doc

      *     18 COND CN11 9(8) Data emissione documento / verbale (nel formato "AAAAMMGG")
           move redi-mod-dt-doc to exe-data-doc

      *     19 COND CN17 Numero del DAS collettivo X(21)

           move redi-das  to exe-das

      *     20 COND CN09, TA06 X(2) Provenienza / Destinazione della merce (sigla paese comunitario)
           move redi-naz  to exe-naz
      *     21 COND CN05 X(30) P.IVA Mittente / Destinatario del prodotto. 
           move redi-dest to exe-piva.
      *     22 SI TA07 Tipo movimentazione X(1)
           move redi-tipo-movim to exe-tipo-mov
      *     23 COND Causale di movimentazione 9(3)
           move redi-cau-movim  to exe-cau-mov.
      *     24 COND Codice posizione fiscale 9(3)
           move redi-pos-fis to exe-pos-fis.
      *     25 COND CN10, CN16 Importo tributi erariali a debito (Imposta di Consumo) 9(9)V9(2)
           move redi-imp-consumo   to exe-imp-tributi.

           write rec-exe.

           set redi-spedito  to true

           rewrite redi-rec invalid continue end-rewrite.

           add 1 to cont-exp.

      ***---
       CONTROLLA-MOD.
           set procedi to false.

           perform X-Y.
           perform VALORE-RIGA.

           if redi-spedito
              display message "ATTENZIONE!!! Dato già esportato."
                       x"0D0A""Modificare comunque?"
                         type mb-yes-no
                        title titolo
                       giving scelta
                         icon 2
              if scelta = mb-yes
      *           perform MSG-SINCRO
                 set procedi to true
              end-if

           else
              set procedi to true
           end-if.

      ***---
       CONTROLLA-CANC.
           set procedi to false.

           perform X-Y.
           perform VALORE-RIGA.

           if redi-spedito
              display message "ATTENZIONE!!! Dato già esportato."
                       x"0D0A""Cancellare comunque?"
                         type mb-yes-no
                      default mb-no
                        title titolo
                       giving scelta
                         icon 2
           else
              display message "Confermi la cancellazione?"
                         type mb-yes-no
                      default mb-no
                        title titolo
                       giving scelta
                         icon 2
           end-if.

           if scelta = mb-yes
              perform MSG-SINCRO
              set procedi to true
           end-if.
           
                 
      ***---
       MSG-SINCRO.
           if redi-mov-prog-reg = zero
              display message "ATTENZIONE!!!"
                  x"0d0a""Sincronizzare: "
              x"0D0A""il movimento di magazzino " redi-tmo-numero
                        title titolo
                       giving scelta
                         icon 2
           else
              display message "ATTENZIONE!!!"
                  x"0d0a""Sincronizzare: "
              x"0D0A""il movimento di magazzino " redi-tmo-numero
              x"0D0A""il movimento sul registro " redi-mov-prog-reg
                        title titolo
                       giving scelta
                         icon 2
           end-if.
           


      ***---
       CAMBIA-BITMAP.
      *     inquire gb-rec-b(riga, 78-col-stato) cell-data = redi-stato
      *
      *     if redi-spedito
              inquire gb-rec-b(riga, 78-col-invio) 
                             hidden-data = num-bitmap
              if num-bitmap = 1
                 move 2   to num-bitmap
              else
                 move 1   to num-bitmap
              end-if

              modify gb-rec-b(riga, 78-col-invio) 
                                            hidden-data = num-bitmap
              modify gb-rec-b(riga, 78-col-invio) 
                                            bitmap = conferma-handle
                                            bitmap-width = 19
                                            bitmap-number = num-bitmap. 
      *     end-if.

      ***---
       CONFRONTO-NC.
           move filtro-nc    to filtro-nc-z
           move filtro-nc-z  to filtro-nc-x
           call "C$JUSTIFY" using filtro-nc-x, "L"
           inspect filtro-nc-x replacing trailing space by low-value
           move zero   to cont
           inspect filtro-nc-x 
                    tallying cont for characters before low-value
           move redi-nc   to redi-nc-x

           set rec-ok  to false
           if filtro-nc-x(1:cont) = redi-nc-x(1:cont)
              set rec-ok  to true
           end-if.

      ***---
       SALVA-EXCELL.
           move riga   to save-riga
           inspect opnsav-filename 
                             replacing trailing spaces by low-value
           initialize wstampa
           string opnsav-filename delimited low-value
                  ".csv"          delimited size
                  into wstampa
           end-string
           perform ACCETTA-SEPARATORE
           open output lineseq
           string |"Anno"             delimited size
      *            separatore         delimited size
      *            "Registro"         delimited size
      *            separatore         delimited size
      *            "Progressivo"      delimited size
      *            separatore         delimited size
                  "Anno"             delimited size
                  separatore         delimited size
                  "Num. Mov."        delimited size
                  separatore         delimited size
                  "Dt. Mov."         delimited size
                  separatore         delimited size
                  "Ragione Sociale"  delimited size
                  separatore         delimited size
                  "CPA"              delimited size
                  separatore         delimited size
                  "NC"               delimited size
                  separatore         delimited size
                  "TARIC"            delimited size
                  separatore         delimited size
                  "DAC"              delimited size
                  separatore         delimited size
                  "Qta Kg"           delimited size
                  separatore         delimited size
                  "Stocc."           delimited size
                  separatore         delimited size
                  "DDT"              delimited size
                  separatore         delimited size
                  "del"              delimited size
                  separatore         delimited size
                  "Naz."             delimited size
                  separatore         delimited size
                  "Pos. Fiscale"     delimited size
                  separatore         delimited size
                  "Causale"          delimited size
                  separatore         delimited size
                  "Imp.Consumo"      delimited size
                  separatore         delimited size
                  "Stato"            delimited size
                  separatore         delimited size
                  "Tipo"            delimited size
                  into line-riga
           end-string
           write line-riga
           inquire gb-rec-b, last-row in tot-righe
           perform varying riga from 2 by 1 
                     until riga > tot-righe
      *        inquire gb-rec-b(riga,  2), cell-data in col-anno
      *        inquire gb-rec-b(riga,  3), cell-data in col-registro
      *        inquire gb-rec-b(riga,  4), cell-data in col-progressivo
              inquire gb-rec-b(riga,  2), cell-data in col-anno-tmo   
              inquire gb-rec-b(riga,  3), cell-data in col-num-tmo    
              inquire gb-rec-b(riga,  4), cell-data in col-data-mov   
              inquire gb-rec-b(riga,  5), cell-data in col-ragsoc     
              inquire gb-rec-b(riga,  6), cell-data in col-cpa        
              inquire gb-rec-b(riga,  7), cell-data in col-nc         
              inquire gb-rec-b(riga,  8), cell-data in col-taric      
              inquire gb-rec-b(riga,  9), cell-data in col-dac        
              inquire gb-rec-b(riga, 10), cell-data in col-kg         
              inquire gb-rec-b(riga, 11), cell-data in col-stoccaggio 
              inquire gb-rec-b(riga, 12), cell-data in col-ddt        
              inquire gb-rec-b(riga, 13), cell-data in col-data-ddt   
              inquire gb-rec-b(riga, 14), cell-data in col-nazione    
              inquire gb-rec-b(riga, 15), cell-data in col-pos-fiscale
              inquire gb-rec-b(riga, 16), cell-data in col-cau        
              inquire gb-rec-b(riga, 17), cell-data in col-consumo    
              inquire gb-rec-b(riga, 18), cell-data in col-stato      

              inquire gb-rec-b(riga, 1), hidden-data = como-tipo-mov
              string |col-anno          delimited size
      *               separatore        delimited size
      *               col-registro      delimited size
      *               separatore        delimited size
      *               col-progressivo   delimited size
      *               separatore        delimited size
                     col-anno-tmo      delimited size
                     separatore        delimited size
                     col-num-tmo       delimited size
                     separatore        delimited size
                     col-data-mov      delimited size
                     separatore        delimited size
                     col-ragsoc        delimited size
                     separatore        delimited size
                     col-cpa           delimited size
                     separatore        delimited size
                     col-nc            delimited size
                     separatore        delimited size
                     col-taric         delimited size
                     separatore        delimited size
                     col-dac           delimited size
                     separatore        delimited size
                     col-kg            delimited size
                     separatore        delimited size
                     col-stoccaggio    delimited size
                     separatore        delimited size
                     col-ddt           delimited size
                     separatore        delimited size
                     col-data-ddt      delimited size
                     separatore        delimited size
                     col-nazione       delimited size
                     separatore        delimited size
                     col-pos-fiscale   delimited size
                     separatore        delimited size
                     col-cau           delimited size
                     separatore        delimited size
                     col-consumo       delimited size
                     separatore        delimited size
                     col-stato         delimited size
                     separatore        delimited size
                     como-tipo-mov     delimited size
                  into line-riga
              end-string
              write line-riga
           end-perform
           close lineseq
           display message "Esportazione conclusa!"
                     title titolo.

           move save-riga to riga.

