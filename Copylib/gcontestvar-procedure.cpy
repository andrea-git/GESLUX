      ***---
       BEFORE-PROGRAM.
           move NotaChiave       to WK-LinkChiave.

           accept esercizio-g2 from environment "ESERCIZIO_G2".
           accept esercizio-x  from environment "ESERCIZIO".
           move   esercizio-x  to esercizio.
           set modifica to true.

           move "GESLUX - Modifica Contestazioni" to titolo.

      ***---
       VALORIZZA-SCREEN.
           if cnt-num-fatt   not = 0 or
              cnt-anno-fatt  not = 0 or
              cnt-data-fatt  not = 0
              move cnt-anno-fatt to tor-anno-fattura
              move cnt-num-fatt  to tor-num-fattura
              read tordini key k-fattura invalid continue end-read

              move cnt-num-fatt  to lab-num-buf
              move cnt-data-fatt to como-data
              perform DATE-TO-SCREEN
              move como-data    to lab-data-buf
              move "Fattura n." to lab-docum-buf
              set LinkFatt      to true
           end-if.

           if cnt-num-nota   not = 0 or
              cnt-anno-nota  not = 0 or
              cnt-data-nota  not = 0
              move cnt-anno-nota to tno-anno-fattura
              move cnt-num-nota  to tno-num-fattura
              read tnotacr key k-fattura invalid continue end-read

              move cnt-num-nota  to lab-num-buf
              move cnt-data-nota to como-data
              perform DATE-TO-SCREEN
              move como-data    to lab-data-buf
              move "Nota cr n." to lab-docum-buf
              set LinkNota      to true
           end-if.
           move cnt-anno    to lab-anno-buf.
           move cnt-numero  to numero-cont.
           set cli-tipo-C   to true.
           move cnt-cod-cli to cli-codice.
           read clienti no lock invalid continue end-read.

           move cnt-cod-cli     to des-codice.
           move cnt-prg-destino to des-prog.
           read destini no lock invalid continue end-read.

           move cnt-corriere to vet-codice.
           read tvettori no lock invalid continue end-read.

           string "Esercizio " delimited size
                  esercizio    delimited size
                  " ("         delimited size
                  esercizio-G2 delimited size
                  ")"          delimited size
                  into lab-esercizio-buf
           end-string.

           move cnt-cod-cli    to codice-ed.
           move codice-ed       to ef-cli-buf.
           call "C$JUSTIFY"  using ef-cli-buf, "L".

           move cnt-prg-destino to codice-ed.
           move codice-ed        to ef-des-buf.
           call "C$JUSTIFY"   using ef-des-buf, "L".

           move cnt-corriere    to ef-vet-buf.

           move cli-ragsoc-1    to lab-cli-buf.
           move cli-indirizzo   to lab-ind-buf.
           move cli-localita    to lab-loca-buf.

           move des-ragsoc-1    to lab-des-buf.
           move des-indirizzo   to lab-ind-d-buf.
           move des-localita    to lab-loca-d-buf.

           move vet-descrizione to lab-vet-buf.
                                     
           move cnt-tipo to tipo-contest.
           perform CARICA-COMBO-TIPO.
                                           
           move cnt-stato to stato-contest.
           perform CARICA-COMBO-STATO-CONT.
                                         
           move spaces to lab-data-fatt1-buf lab-cli1-buf   
                          lab-n-fatt1-buf    lab-des1-buf
                          lab-data-fatt2-buf lab-cli2-buf   
                          lab-n-fatt2-buf    lab-des2-buf
                          lab-data-fatt3-buf lab-cli3-buf   
                          lab-n-fatt3-buf    lab-des3-buf
                          lab-data-fatt4-buf lab-cli4-buf   
                          lab-n-fatt4-buf    lab-des4-buf
                          lab-data-fatt5-buf lab-cli5-buf   
                          lab-n-fatt5-buf    lab-des5-buf.

           if cnt-nota-anno-1 not = 0 and
              cnt-nota-1      not = 0
              move cnt-nota-anno-1 to tno-anno
              move cnt-nota-1      to tno-numero
              read tnotacr no lock
                   invalid continue
               not invalid
                   set campo1 to true
                   perform VALORIZZA-CAMPI-NOTA
              end-read
           end-if.

           if cnt-nota-anno-2 not = 0 and
              cnt-nota-2      not = 0
              move cnt-nota-anno-2 to tno-anno
              move cnt-nota-2      to tno-numero
              read tnotacr no lock
                   invalid continue
               not invalid
                   set campo2 to true
                   perform VALORIZZA-CAMPI-NOTA
              end-read
           end-if.

           if cnt-nota-anno-3 not = 0 and
              cnt-nota-3      not = 0
              move cnt-nota-anno-3 to tno-anno
              move cnt-nota-3      to tno-numero
              read tnotacr no lock
                   invalid continue
               not invalid
                   set campo3 to true
                   perform VALORIZZA-CAMPI-NOTA
              end-read
           end-if.

           if cnt-nota-anno-4 not = 0 and
              cnt-nota-4      not = 0
              move cnt-nota-anno-4 to tno-anno
              move cnt-nota-4      to tno-numero
              read tnotacr no lock
                   invalid continue
               not invalid
                   set campo4 to true
                   perform VALORIZZA-CAMPI-NOTA
              end-read
           end-if.
           
           if cnt-nota-anno-5 not = 0 and
              cnt-nota-5      not = 0
              move cnt-nota-anno-5 to tno-anno
              move cnt-nota-5      to tno-numero
              read tnotacr no lock
                   invalid continue
               not invalid
                   set campo5 to true
                   perform VALORIZZA-CAMPI-NOTA
              end-read
           end-if.

           display form1.

      ***---
       CANCELLA.
           display message "Cancellare il documento corrente?"
                     title titolo
                      type mb-yes-no
                   default mb-no
                    giving scelta
                      icon 2

           if scelta = mb-yes
              move WK-LinkChiave to cnt-chiave
              delete contestazioni  record invalid continue end-delete
              display message "Cancellazione avenuta con successo!"
                        title titolo
              move 27 to key-status
              initialize cnt-rec 
                         old-cnt-rec replacing numeric data by zeroes
                                          alphanumeric data by spaces
           end-if.

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
                 move 1 to mod
                 set StatusModifica to true
                 perform STATUS-BAR-MSG      
                 move 78-ID-ef-data to control-id
              end-if
           else
              move 1 to mod
              perform SALV-MOD            
              move 0 to mod
              if errori
                 move 1 to mod
              else
                 unlock contestazioni all records
                 set NoMessage to true
                 perform CURRENT-RECORD
                 set StatusVisua to true
                 perform STATUS-BAR-MSG
              end-if
           end-if.
                                
           perform DISPLAY-SCREEN.
           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.

           move 4 to accept-control.
           move 0 to StatusHelp.
           perform STATUS-HELP.

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
                   set errori to true
                   move store-id to CONTROL-ID       
                   move 4        to ACCEPT-CONTROL   
              end-evaluate

           end-if.

      ***---
       CURRENT-RECORD.
           move spaces to Link-TipoDoc.
           move WK-LinkAnno   to cnt-anno.
           move WK-LinkNumero to cnt-numero.

           set tutto-ok  to true.
           set ReadSecca to true.
           if mod = 1
              read contestazioni lock invalid
                   set errori to true
              end-read
           else
              read contestazioni no lock invalid
                   set errori to true
              end-read
           end-if.
           set ReadSecca to false.
           
           if RecLocked
              set RecLocked to false
              set errori    to true
           end-if.

           if tutto-ok
              move "00"          to status-contestazioni
              move riga          to store-riga
              perform FORM1-FLD-TO-BUF
              perform VALORIZZA-SCREEN
           else
              move 0 to mod
              move 0 to mod-k
              modify tool-modifica, value = mod
           end-if.
           perform ABILITAZIONI.
           perform VALORIZZA-OLD.   

      ***---
       VALORIZZA-OLD.
           move cnt-rec to old-cnt-rec. 

      ***---
       STATUS-BAR-MSG.
           evaluate true
           when StatusIns
           when StatusModifica
                modify form1-st-1-handle, 
                       panel-index  3,
                       panel-text  "MODIFICA"
           when StatusVisua
                modify form1-st-1-handle, 
                       panel-index  3,
                       panel-text  "VISUALIZZAZIONE"
                move 0 to StatusHelp
                perform STATUS-HELP
           when other
                modify form1-st-1-handle, 
                       panel-index  2,
                       panel-text   spaces
           end-evaluate.

      ***---
       ABILITAZIONI.
           move livello-abil to sav-livello-abil.
      *****     if btno-data-fm not = 0 or
      *****        btno-num-fm  not = 0 or
      *****        btno-data-nc not = 0 or
      *****        btno-num-nc  not = 0 or livello-abil = 1
      *****        move BitmapEditDisabled to BitmapNumEdit
      *****        move 0 to e-modifica
      *****     else
           if livello-abil = 1
              move BitmapEditDisabled to BitmapNumEdit
              move 0 to e-modifica
           else
              move BitmapEditEnabled to BitmapNumEdit
              move 1 to e-modifica
           end-if.

           if cnt-chiusa
              move 2 to livello-abil
           end-if.

      *****     end-if.

           if livello-abil <= 2
              move 0 to e-cancella
              move BitmapDeleteDisabled to BitmapNumDelete
           else
              move 1 to e-cancella
              move BitmapDeleteEnabled to BitmapNumDelete
           end-if.

           modify tool-modifica,
                  enabled e-modifica,
                  bitmap-number = BitmapNumEdit.
                                    
           move 0 to mod-campi mod-rich mod-nota.
           if mod = 1
              move BitmapSaveEnabled   to BitmapNumSave
              move 1 to e-salva

              if cnt-aperta
                 move 1 to mod-campi

                 if cnt-si-richiesta
                    move 1 to mod-rich
                 end-if

                 if cnt-si-nota
                    move 1 to mod-nota
                 end-if
              end-if

           else
              move BitmapDeleteDisabled to BitmapNumDelete
              move BitmapSaveDisabled   to BitmapNumSave
              move 0 to e-salva e-cancella
           end-if.
           move BitmapNewDisabled     to BitmapNumNew.

           modify tool-nuovo,    enabled       = 0
                                 bitmap-number = BitmapNumNew.
           modify tool-cancella, enabled       = e-cancella,
                                 bitmap-number = BitmapNumDelete.
           modify tool-salva,    enabled       = e-salva,
                                 bitmap-number = BitmapNumSave.
           move sav-livello-abil to livello-abil.

      ***---
       TORNA-IN-VISUA.
           move 0 to mod.
           move 78-ID-ef-cli to control-id.
           set NoMessage to true.
           perform ABILITAZIONI.
           set StatusVisua to true.
           perform STATUS-BAR-MSG.
           unlock contestazioni all records.

           modify tool-modifica,  value mod.
           perform CANCELLA-COLORE.
           move 4 to ACCEPT-CONTROL.

           perform INIT.
           perform DISPLAY-SCREEN.
           perform VALORIZZA-OLD.
