      ***---
       RIEMPI-TMP.
           move spaces to G2-codice.
           read G2 no lock invalid continue end-read.

           move "DO"           to tbldo-codice1.
           move G2-cod-fatture to tbldo-codice2
           read tbldo no lock
                invalid
                set errori to true
                display message
                        "Record tipologie documenti NON PRESENTE"
                 x"0d0a""Impossibile procedere"
                          title titolo
                           icon 2
            not invalid
                move "CO"            to tblco-codice1
                move tbldo-codice-co to tblco-codice2
                |MI SERVE LA VARIABILE "TBLCO-TIPO-DOCUMENTO"
                read tblco no lock
                     invalid
                     set errori to true
                     display message
                            "Record causale contabile NON PRESENTE"
                     x"0d0a""Impossibile procedere"
                               title titolo
                                icon 2
                end-read
           end-read.
           move tblco-tipo-documento to fatture-tipo-documento.

           move "DO"      to tbldo-codice1.
           move G2-cod-nc to tbldo-codice2.
           read tbldo no  lock
                invalid
                set errori to true
                display message
                        "Record tipologie documenti NON PRESENTE"
                 x"0d0a""Impossibile procedere"
                          title titolo
                           icon 2
            not invalid
                move "CO"            to tblco-codice1
                move tbldo-codice-co to tblco-codice2
                |MI SERVE LA VARIABILE "TBLCO-TIPO-DOCUMENTO"
                read tblco no lock
                     invalid
                     set errori to true
                     display message
                            "Record causale contabile NON PRESENTE"
                     x"0d0a""Impossibile procedere"
                               title titolo
                                icon 2
                end-read
           end-read.
           move tblco-tipo-documento to nc-tipo-documento.

           move 0 to counter counter2.
           move 0 to RecCounter.
           set tutto-ok to true.
           set trovato  to false.
           initialize path-tmp-cont.
           accept  path-tmp-cont from environment "PATH-ST".
           inspect path-tmp-cont
                   replacing trailing spaces by low-value.
           accept como-data from century-date.
           accept como-ora  from time.
           string path-tmp-cont   delimited low-value
                  "tmp-cont"      delimited size
                  "_"             delimited size
                  como-data       delimited size
                  "_"             delimited size
                  como-ora        delimited size
                  ".tmp"          delimited size
                  into path-tmp-cont
           end-string.
           open output tmp-cont.
           perform CICLO-LETTURA-CONTESTAZIONI.

      ***---
       CICLO-LETTURA-CONTESTAZIONI.
           move 0 to tot-cont tot-tratt.
           move low-value    to cnt-chiave.
           move como-cliente to cnt-cod-cli.
           move como-destino to cnt-prg-destino.

           start contestazioni key is >= k1
LUBEXX           invalid set errori to true
LUBEXX     end-start.
      *
           if tutto-ok
              perform until 1 = 2
                 set record-ok to true
                 read contestazioni next at end exit perform end-read

                 add 1 to counter
                 add 1 to counter2
                 if counter2 = 100
                    move counter to counter-edit
                    display counter-edit
                       upon form3-handle at column 22
                                              line 03
                    move 0 to counter2
                 end-if

LUBEXX           if como-cliente not = 0
LUBEXX              if cnt-cod-cli not = como-cliente
LUBEXX                 exit perform
LUBEXX              end-if
LUBEXX              if como-destino not = 0
LUBEXX                 if cnt-prg-destino not = como-destino
LUBEXX                    exit perform
LUBEXX                 end-if
LUBEXX              end-if
LUBEXX           end-if

LUBEXX           if cnt-data >= como-data-from and
LUBEXX              cnt-data <= como-data-to

                    perform VALIDA-RECORD

LUBEXX              if record-ok
LUBEXX                 perform MOVE-DATI
LUBEXX                 inquire ef-cod,  value in cli-codice
LUBEXX                 inquire ef-des,  value in des-prog
LUBEXX              end-if

LUBEXX           end-if

              end-perform
           end-if.

      ***---
       MOVE-DATI.
           set trovato          to true.
           move spaces          to tmp-cnt-rec.

           move cnt-anno        to tmp-cnt-anno.
           move cnt-numero      to tmp-cnt-numero.
           move cnt-data        to como-data.
           perform DATE-TO-SCREEN.
           move como-data       to tmp-cnt-data.
           evaluate true
           when cnt-reso         move "RESO"        to tmp-cnt-tipo
           when cnt-prezzo       move "PREZZO"      to tmp-cnt-tipo
           when cnt-merce        move "MERCE"       to tmp-cnt-tipo
           when cnt-altro        move "ALTRO"       to tmp-cnt-tipo
           when cnt-add-corriere move "Addebito C." to tmp-cnt-tipo
           end-evaluate.
           set  cli-tipo-C  to true.
           move cnt-cod-cli to cli-codice.
           read clienti no lock invalid continue end-read.

           move spaces to tmp-cnt-gdo.
           move spaces to tmp-cnt-gdo-descrizione.
           if cli-gdo not = spaces
              move cli-gdo to gdo-codice tmp-cnt-gdo
              read tgrupgdo no lock invalid continue end-read
              move gdo-intestazione to tmp-cnt-gdo-descrizione
           end-if.
           if cnt-prg-destino not = 0
              move cnt-cod-cli     to des-codice
              move cnt-prg-destino to des-prog
              read destini no lock invalid continue end-read
           else
              move cli-localita to des-localita
           end-if.
           move cli-ragsoc-1    to tmp-cnt-cli-ragsoc.
           move cli-codice      to tmp-cnt-cod-cli.
           move cnt-prg-destino to tmp-cnt-prg-destino.
           move des-localita    to tmp-cnt-des-localita.
           move cnt-importo     to tmp-cnt-importo.
           inspect cnt-appunti replacing all x"0d0a" by x"2020".
           move " "            to tmp-cnt-appunti(1:1).
           move cnt-appunti    to tmp-cnt-appunti(2:).

           move 0 to tmp-cnt-num-doc 
                     tmp-cnt-data-doc 
                     tmp-cnt-importo-doc.

           if cnt-anno-fatt not = 0 and
              cnt-num-fatt  not = 0
              move "F"           to tmp-cnt-riferimento
              move cnt-num-fatt  to tmp-cnt-num-doc
              move cnt-data-fatt to como-data
              perform DATE-TO-SCREEN
              move como-data     to tmp-cnt-data-doc
              move cnt-anno-fatt to tor-anno-fattura
              move cnt-num-fatt  to tor-num-fattura
              read tordini key k-fattura invalid continue end-read
              move tor-causale to tca-codice
              read tcaumag no lock invalid continue end-read
              move tca-descrizione to tmp-cnt-causale
              perform CALCOLA-TOTALE-IVATO-FATTURA
              move como-tot-ivato  to tmp-cnt-importo-doc
              move fatture-tipo-documento to tblco-tipo-documento
              move cnt-num-fatt    to como-num-doc
              move cnt-data-fatt   to como-data
              
              move "PA"              to tblpa-codice1
              move tor-cod-pagamento to tblpa-codice2
              read tcodpag no lock invalid continue end-read
                                                 
              set PagamentoBancario to false
              perform varying idx from 1 by 1 until idx > 36
                 if tblpa-codice-tr(idx) = "W"
                    set PagamentoBancario to true
                    exit perform
                 end-if
              end-perform
              
              if PagamentoBancario
                 move "RI.BA."    to tmp-cnt-pagamento-doc
                 move cnt-importo to tmp-cnt-da-pagare
              else
                 perform CONTROLLA-PAGAMENTO
              end-if

           end-if.

           if cnt-anno-nota not = 0 and
              cnt-num-nota  not = 0
              move "N"           to tmp-cnt-riferimento
              move cnt-num-nota  to tmp-cnt-num-doc
              move cnt-data-nota to como-data
              perform DATE-TO-SCREEN
              move como-data     to tmp-cnt-data-doc
              move cnt-anno-nota to tno-anno-fattura
              move cnt-num-nota  to tno-num-fattura
              read tnotacr key k-fattura invalid continue end-read
              move tno-causale to tca-codice
              read tcaumag no lock invalid continue end-read
              move tca-descrizione to tmp-cnt-causale
              perform CALCOLA-TOTALE-IVATO-NOTA
              move como-tot-ivato to tmp-cnt-importo-doc
              move nc-tipo-documento to tblco-tipo-documento
              move cnt-num-nota      to como-num-doc
              move cnt-data-nota     to como-data
              
              move "PA"              to tblpa-codice1
              move tno-cod-pagamento to tblpa-codice2
              read tcodpag no lock invalid continue end-read
                                            
              set PagamentoBancario to false
              perform varying idx from 1 by 1 until idx > 36
                 if tblpa-codice-tr(idx) = "W"
                    set PagamentoBancario to true
                    exit perform
                 end-if
              end-perform
              
              if PagamentoBancario
                 move "RI.BA."    to tmp-cnt-pagamento-doc
                 move cnt-importo to tmp-cnt-da-pagare
              else
                 perform CONTROLLA-PAGAMENTO
              end-if

           end-if.

           if cnt-nota-anno-1 not = 0
              move cnt-nota-anno-1 to tmp-cnt-anno-nc
           end-if.
           if cnt-nota-1 not = 0
              move cnt-nota-1 to tmp-cnt-numero-nc
           end-if.

           move cnt-data-rich to como-data.
           perform DATE-TO-SCREEN.
           move como-data to tmp-cnt-data-rich.

           if cnt-corriere not = 0
              move cnt-corriere to tmp-cnt-corriere vet-codice
              read tvettori no lock invalid continue end-read
              move vet-descrizione to tmp-cnt-vet-descrizione
           else
              move spaces to tmp-cnt-vet-descrizione
           end-if.

           move cnt-data-ricev to como-data.
           perform DATE-TO-SCREEN.
           move como-data      to tmp-cnt-data-ricev.

           move cnt-numero-nota-debito to tmp-cnt-numero-nota-debito.

           move cnt-data-nota-deb  to como-data.
           perform DATE-TO-SCREEN.
           move como-data to tmp-cnt-data-nota-deb.
           move cnt-importo-nota   to tmp-cnt-importo-nota-deb.
           move cnt-num-reg-contab to tmp-cnt-num-reg-contab.
           move 1 to tmp-cnt-prog.

           write tmp-cnt-rec invalid continue end-write.
           compute tot-tratt = tot-tratt + tmp-cnt-da-pagare.
           compute tot-cont  = tot-cont  + tmp-cnt-importo.

           if cnt-nota-anno-2 not = 0 or
              cnt-nota-2      not = 0
              perform INIT-TMP-CNT-REC
              move cnt-nota-anno-2 to tmp-cnt-anno-nc
              move cnt-nota-2      to tmp-cnt-numero-nc
              move 2               to tmp-cnt-prog
              write tmp-cnt-rec invalid continue end-write
           end-if.

           if cnt-nota-anno-3 not = 0 or
              cnt-nota-3      not = 0
              perform INIT-TMP-CNT-REC
              move cnt-nota-anno-3 to tmp-cnt-anno-nc
              move cnt-nota-3      to tmp-cnt-numero-nc
              move 3               to tmp-cnt-prog
              write tmp-cnt-rec invalid continue end-write
           end-if.

           if cnt-nota-anno-4 not = 0 or
              cnt-nota-4      not = 0
              perform INIT-TMP-CNT-REC
              move cnt-nota-anno-4 to tmp-cnt-anno-nc
              move cnt-nota-4      to tmp-cnt-numero-nc
              move 4               to tmp-cnt-prog
              write tmp-cnt-rec invalid continue end-write
           end-if.

           if cnt-nota-anno-5 not = 0 or
              cnt-nota-5      not = 0
              perform INIT-TMP-CNT-REC
              move cnt-nota-anno-5 to tmp-cnt-anno-nc
              move cnt-nota-5      to tmp-cnt-numero-nc
              move 5               to tmp-cnt-prog
              write tmp-cnt-rec invalid continue end-write
           end-if.

           perform BUG-FIX-THIN-CLIENT.

      ***---
       INIT-TMP-CNT-REC.
           move spaces to tmp-cnt-dati.

      ***---
       GENERA-FILE-EXCEL.
           perform ACCETTA-SEPARATORE.
           perform COMPONI-PATH-STAMPA.
           if tutto-ok
              perform SCRIVI-RIGHE-EXCEL
           end-if.

      ***---
       CONTROLLA-PAGAMENTO.
           move spaces to tmp-cnt-pagamento-doc.
           move 0      to tmp-cnt-da-pagare.

           set trovato-pagamento to false.
           move como-num-doc             to num-fattura-x.
           inspect num-fattura-x  replacing leading x"30" by x"20".
           call "C$JUSTIFY"           using num-fattura-x, "L".
           move num-fattura-x            to num-fattura-x6.
           call "C$JUSTIFY"           using num-fattura-x6, "R".
           inspect num-fattura-x6 replacing leading x"20" by x"30".
           
           initialize como-numero-rif.
           string num-fattura-x6       delimited size
                  tblco-tipo-documento delimited size
                  into como-numero-rif
           end-string.
           set pat-tipo-cfm-cli to true.

           move cnt-cod-cli            to cli-codice-x.
           call "C$JUSTIFY"         using cli-codice-x, "R".
           inspect cli-codice-x replacing leading x"20" by x"30".
           move cli-codice-x    to pat-codice-cfm.
           move como-data       to pat-data-riferimento.
           move como-numero-rif to pat-numero-riferimento.
           move low-value       to pat-codice.
           start pat key >= pat-codice1
                 invalid move 0 to pat-importo-saldo
             not invalid
                 read pat next 
                      at end continue
                  not at end
                      if pat-numero-riferimento = como-numero-rif and
                         pat-data-riferimento   = como-data       and
                         pat-codice-cfm         = cli-codice-x    and
                         pat-tipo-cfm-cli
                         set trovato-pagamento to true
                         if pat-importo-saldo = 0
                            set pagato-totale  to true
                         else
                            set pagato-parziale to true
                         end-if
                      end-if
                 end-read
           end-start.

           if trovato-pagamento
              move 0 to tot-importo-dare
              move 0 to tot-importo-avere

              if pagato-totale
                 move "COMPLETO" to tmp-cnt-pagamento-doc
              else
                 move pat-progressivo to pas-progressivo
                 move 0               to pas-riga
                 start PAS key >= pas-codice
                       invalid continue
                 end-start
                 perform until 1 = 2
                    read PAS next at end exit perform end-read
                    if pas-progressivo not = pat-progressivo
                       exit perform
                    end-if
                    if PAS-DATA-SCADENZA > data-scadenza
                       exit perform
                    end-if
                    add pas-importo-dare  to tot-importo-dare
                    add pas-importo-avere to tot-importo-avere
                 end-perform
                 if tot-importo-dare  = 0 and
                    tot-importo-avere = 0
                    move "NON SCADUTO" to tmp-cnt-pagamento-doc
                    move cnt-importo   to tmp-cnt-da-pagare
                 else
                    compute tmp-cnt-da-pagare =
                            tot-importo-dare  -
                            tot-importo-avere
                    move "SCADUTO"            to tmp-cnt-pagamento-doc
                 end-if
              end-if
           else
              move "NON TROVATO" to tmp-cnt-pagamento-doc 
           end-if.


      ***---
       COMPONI-PATH-STAMPA.
           move user-codi to como-user.
           initialize wstampa.
           accept  wstampa   from environment "PATH_ST".
           inspect wstampa   replacing trailing spaces by low-value.
           inspect como-user replacing trailing spaces by low-value.
           string  wstampa       delimited low-value
                   "excel_cont"  delimited size
                   "_"           delimited size
                   como-user     delimited low-value
                   ".csv"        delimited size
                   into wstampa
           end-string.
           open output lineseq.

      ***---
       SCRIVI-RIGHE-EXCEL.
           initialize tmp-cnt-rec
           set prima-volta to true.
           set tutto-ok    to true.
           perform until 1 = 2
              read tmp-cont next at end exit perform end-read
              if prima-volta
                 set   prima-volta to false
                 initialize line-riga
                 string "Anno"                     delimited size
                        separatore                 delimited size
                        "Numero"                   delimited size
                        separatore                 delimited size
                        "Data"                     delimited size
                        separatore                 delimited size
                        "Tipologia"                delimited size
                        separatore                 delimited size
                        "GDO"                      delimited size
                        separatore                 delimited size
                        "Descrizione"              delimited size
                        separatore                 delimited size
                        "Cliente"                  delimited size
                        separatore                 delimited size
                        "Ragione Sociale"          delimited size
                        separatore                 delimited size
                        "Destino"                  delimited size
                        separatore                 delimited size
                        "Localita"                 delimited size
                        separatore                 delimited size
                        "Contestato"               delimited size
                        separatore                 delimited size
                        "Causale riferimento"      delimited size
                        separatore                 delimited size
                        "N. Fatt/N.C."             delimited size
                        separatore                 delimited size
                        "Data Fatt/N.C."           delimited size
                        separatore                 delimited size
                        "Totale (IVA compresa)"    delimited size
                        separatore                 delimited size
                        "Stato Pagamento"          delimited size
                        separatore                 delimited size
                        "Trattenuto"               delimited size
                        separatore                 delimited size
                        "Anno N.C. collegata"      delimited size
                        separatore                 delimited size
                        "Numero N.C. collegata"    delimited size
                        separatore                 delimited size
                        "Data richiesta bolla"     delimited size
                        separatore                 delimited size
                        "Corriere"                 delimited size
                        separatore                 delimited size
                        "Descrizione"              delimited size
                        separatore                 delimited size
                        "Data ricevimento bolla"   delimited size
                        separatore                 delimited size
                        "N. nota debito"           delimited size
                        separatore                 delimited size
                        "Data nota debito"         delimited size
                        separatore                 delimited size
                        "Importo nota debito"      delimited size
                        separatore                 delimited size
                        "Reg. contabilità"         delimited size
                        separatore                 delimited size
                        "Appunti"                  delimited size
                        into line-riga
                 end-string
                 write line-riga
              end-if
              move tmp-cnt-importo          to imp-edit
              move tmp-cnt-importo-doc      to imp-doc-edit
              move tmp-cnt-da-pagare        to imp-da-pag-edit
              move tmp-cnt-importo-nota-deb to imp-nd-edit

              initialize line-riga
              string tmp-cnt-anno               delimited size
                     separatore                 delimited size
                     tmp-cnt-numero             delimited size
                     separatore                 delimited size
                     tmp-cnt-data               delimited size
                     separatore                 delimited size
                     tmp-cnt-tipo               delimited size
                     separatore                 delimited size
                     tmp-cnt-gdo                delimited size
                     separatore                 delimited size
                     tmp-cnt-gdo-descrizione    delimited size
                     separatore                 delimited size
                     tmp-cnt-cod-cli            delimited size
                     separatore                 delimited size
                     tmp-cnt-cli-ragsoc         delimited size
                     separatore                 delimited size
                     tmp-cnt-prg-destino        delimited size
                     separatore                 delimited size
                     tmp-cnt-des-localita       delimited size
                     separatore                 delimited size
                     imp-edit                   delimited size
                     separatore                 delimited size
                     tmp-cnt-causale            delimited size
                     separatore                 delimited size
                     tmp-cnt-num-doc            delimited size
                     separatore                 delimited size
                     tmp-cnt-data-doc           delimited size
                     separatore                 delimited size
                     imp-doc-edit               delimited size
                     separatore                 delimited size
                     tmp-cnt-pagamento-doc      delimited size
                     separatore                 delimited size
                     imp-da-pag-edit            delimited size
                     separatore                 delimited size
                     tmp-cnt-anno-nc            delimited size
                     separatore                 delimited size
                     tmp-cnt-numero-nc          delimited size
                     separatore                 delimited size
                     tmp-cnt-data-rich          delimited size
                     separatore                 delimited size
                     tmp-cnt-corriere           delimited size
                     separatore                 delimited size
                     tmp-cnt-vet-descrizione    delimited size
                     separatore                 delimited size
                     tmp-cnt-data-ricev         delimited size
                     separatore                 delimited size
                     tmp-cnt-numero-nota-debito delimited size
                     separatore                 delimited size
                     tmp-cnt-data-nota-deb      delimited size
                     separatore                 delimited size
                     imp-nd-edit                delimited size
                     separatore                 delimited size
                     tmp-cnt-num-reg-contab     delimited size
                     separatore                 delimited size
                     tmp-cnt-appunti            delimited size
                     into line-riga
              end-string        
              write line-riga
           end-perform.
           close lineseq.

           if trovato
              perform CALL-EXCEL
           end-if.

      ***---
       BUG-FIX-THIN-CLIENT.
           |MODIFICA PER THIN CLIENT (BUG-FIX)
           add 1 to RecCounter.
           if RecCounter = num-rec-thin-client
              move 0 to RecCounter
              display form3
           end-if.

      ***---
       CALCOLA-TOTALE-IVATO-FATTURA.
           move 0 to como-tot-ivato.
           initialize ror-rec.
           move tor-chiave to ror-chiave.
           start rordini key >= ror-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rordini next at end exit perform end-read
                    if ror-anno       not = tor-anno or
                       ror-num-ordine not = tor-numero
                       exit perform
                    end-if
                    compute SavePrezzo = ror-imponib-merce +
                                         ror-imp-cou-cobat +
                                         ror-add-piombo    +
                                         ror-imp-consumo
                    compute SavePrezzo = 
                            SavePrezzo * ( ror-qta - ror-qta-omaggi)
                    move "IV"        to tbliv-codice1
                    move ror-cod-iva to tbliv-codice2
                    read tivaese no lock invalid continue end-read
                    compute como-iva =
                            SavePrezzo * tbliv-percentuale / 100
                    add 0,005           to como-iva
                    move como-iva       to como-iva-2dec
                    compute como-tot-ivato =
                            como-tot-ivato +
                            SavePrezzo     +
                            como-iva-2dec
                 end-perform
           end-start.

      ***---
       CALCOLA-TOTALE-IVATO-NOTA.
           move 0 to como-tot-ivato.
           initialize rno-rec.
           move tno-chiave to rno-chiave.
           start rnotacr key >= rno-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rnotacr  next at end exit perform end-read
                    if rno-anno   not = tno-anno or
                       rno-numero not = tno-numero
                       exit perform
                    end-if
                    compute SavePrezzo = rno-prz-unitario  +
                                         rno-imp-consumo   +
                                         rno-imp-cou-cobat +
                                         rno-add-piombo
                    compute SavePrezzo = 
                            SavePrezzo * rno-qta
                    move "IV"        to tbliv-codice1
                    move rno-cod-iva to tbliv-codice2
                    read tivaese no lock invalid continue end-read
                    compute como-iva =
                            SavePrezzo * tbliv-percentuale / 100
                    add 0,005           to como-iva
                    move como-iva       to como-iva-2dec
                    compute como-tot-ivato =
                            como-tot-ivato +
                            SavePrezzo     +
                            como-iva-2dec
                 end-perform
           end-start.

      ***---
       VALIDA-RECORD.

           if tcl-codice not = space
              move cnt-cod-cli to cli-codice
              read clienti no lock 
                 invalid 
                    initialize cli-tipo
              end-read
              if cli-tipo not = tcl-codice
                 set record-ok  to false
              end-if
           end-if


           if record-ok           
              if como-cliente = 0
                 if ef-gdo-buf not = spaces
                    set  cli-tipo-C  to true
                    move cnt-cod-cli to cli-codice
                    read clienti no lock invalid continue end-read
                    if cli-gdo not = ef-gdo-buf
                       set record-ok to false
                    end-if
                 end-if
              end-if
           end-if

           if record-ok
              set record-ok to false
              if tutte
                 set record-ok to true
              else
              evaluate true also true
              when reso     also cnt-reso         set record-ok to true
              when prezzo   also cnt-prezzo       set record-ok to true
              when merce    also cnt-merce        set record-ok to true
              when corriere also cnt-add-corriere set record-ok to true
              when altro    also cnt-altro        set record-ok to true
              end-evaluate
              end-if
           end-if

           if record-ok
              set record-ok to false
              if tutte-s
                 set record-ok to true
              else
              evaluate true also true
              when aperte also cnt-aperta set record-ok to true
              when chiuse also cnt-chiusa set record-ok to true
              end-evaluate
              end-if
           end-if.

