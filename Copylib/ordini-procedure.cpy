      *** COPY DI PROCEDURE PER EVASIONI

      ***---
       INTESTAZIONE.
           perform FORM1-GD-1-CONTENT.

      ***---
       VALORIZZA-RIGA-ARTICOLO.
           if art-attivo

      *****        move prg-chiave to save-prg-chiave
      *****        move art-scorta to sco-codice
      *****        read tscorte no lock invalid continue end-read
      *****        if sco-chiu-forzata-si
      *****           initialize prg-chiave
      *****           move art-codice to prg-cod-articolo
      *****           read progmag no lock invalid continue end-read
      *****           if prg-giacenza < prg-impegnato
      *****              set errori to true
      *****              display message 
      *****                      "Articolo non valido:"
      *****               x"0d0a""Scorta forzata con impegnato scoperto"
      *****                        title tit-err
      *****                         icon 2
      *****           end-if
      *****        end-if
      *****        move save-prg-chiave to prg-chiave

      *****        if tutto-ok
      *****           if SiAssortimento
      *****              perform ASSORCLI-IN-LINE
      *****              if not trovato
      *****                 set errori to true
      *****              else
      *****                 move art-codice to SaveArticolo
      *****              end-if
      *****           else
      *****              move art-codice to SaveArticolo
      *****              if tutto-ok and not CheckAfterZoom
      *****                 perform TROVA-LISTINO
      *****                 if no-prg-listino
      *****                    perform TROVA-CLI-PRG
      *****                 end-if
      *****              end-if
      *****           end-if                  
      *    Luciano 09/06/2010
              if tutto-ok and not CheckAfterZoom
                 perform TROVA-LISTINO
                 if no-prg-listino
                    perform TROVA-CLI-PRG
                 end-if
              end-if
      *    Luciano fine     
              if tutto-ok
                 move art-codice to SaveArticolo
                 move 0 to num-articoli

                 if SaveArticolo not = 0
                    if CheckAfterZoom
                       perform FIND-PROGMAG
                       perform READ-TMARCHE
                       perform VALORIZZA-CELLE-CAMPI-MANUALE
                    else
                       perform FIND-MORE-ARTICOLI-ON-PROGMAG
                       if num-articoli = 0
                          set errori to true
                       end-if
                    end-if
                 else
                    set errori to true
                 end-if
              end-if
           else
              set errori to true
           end-if.

      ***---
       VALORIZZA-CELLE-CAMPI-MANUALE.
           inquire form1-gd-1, last-row in tot-righe.
           move art-codice     to col-art.
           initialize lab-art-buf.
           inspect art-descrizione replacing trailing spaces 
                                             by low-value.
           inspect imb-descrizione replacing trailing spaces 
                                             by low-value.
           inspect hid-des-imballo replacing trailing spaces 
                                             by low-value.
           move hid-imballi to imballi-ed.
           call "C$JUSTIFY" using imballi-ed, "L".
           initialize lab-art-buf.
BLISTR     if hid-blister = 1
BLISTR        string art-descrizione delimited low-value
BLISTR               " - "           delimited size
BLISTR               hid-des-imballo delimited low-value
BLISTR               " ("            delimited size
BLISTR               imballi-ed      delimited spaces
BLISTR               ")"             delimited size
BLISTR               into lab-art-buf
BLISTR        end-string
           else
              string  art-descrizione delimited low-value
                      " - "           delimited size
                      hid-des-imballo delimited low-value
                      " da "          delimited size
                      imballi-ed      delimited spaces
                      " x "           delimited size
                      art-udm-imballo delimited size
                      into lab-art-buf
              end-string
           end-if.

           inspect hid-des-imballo replacing trailing low-value
                                             by spaces.
           inspect art-descrizione replacing trailing low-value
                                             by spaces.
           move lab-art-buf       to col-des.
           move prg-peso-utf      to hid-utf.
           move prg-peso-non-utf  to hid-non-utf.
           move "N"               to hid-omaggio.

           move 0                 to ef-qta-buf.

           if OrdineTradizionale and pgm-name = "gordc"
              move 9999999,99  to ror-prz-unitario
           else
              if tca-prezzo-reso-si
                 perform RECUPERA-DATI-FROM-RMOVMAG-FORN
              else
                 perform RECUPERA-DATI-FROM-RMOVMAG
              end-if
           end-if.
           perform RECUPERA-PREZZO.
           perform RECUPERA-SCONTO.
           perform CALCOLA-IMPOSTE-ORDINE.
           perform CALCOLA-IMPONIBILE.
           perform RECUPERA-IVA.
           perform DISPLAY-SCREEN.

      *    Luciano
           move ef-uni-buf  to como-prezzo-proposto
           move ef-sconto-buf   to como-sconto-proposto.
      *    Luciano fine


      ***---
       RECUPERA-DATI-FROM-RMOVMAG.
           set  trovato-movim  to false.
           |DEVO FARE POI IL RECUPERO DA LISTINO PER
           |CUI NON M'INTERESSA L'ULTIMO MOVIMENTO
LABLAB     if tcl-si-recupero  exit paragraph end-if.
           move low-value      to rmo-rec.
           set  rmo-cliente    to true.
           move ef-cli-buf     to rmo-cod-clifor convert.
           move ef-cau-buf     to rmo-causale    save-causale.
           move art-codice     to rmo-articolo   save-articolo.
           move high-value     to rmo-data-movim.
           move rmo-cod-clifor to save-cliente.
           start rmovmag key   is < rmo-chiave-ricerca
                 invalid continue
             not invalid
                 read rmovmag previous at end continue end-read
                 if rmo-cod-clifor = save-cliente  and
                    rmo-causale    = save-causale  and
                    rmo-articolo   = save-articolo and
LUBEXX              rmo-qta    not = 0
                    set trovato-movim to true
LUBEXX              if ttipocli-gdo
LUBEXX                 compute ror-prz-unitario = 
LUBEXX                         rmo-netto + rmo-coubat + rmo-imp-cons
LUBEXX              else
LUBEXX                 move rmo-netto    to ror-prz-unitario
LUBEXX              end-if
                 end-if
           end-start.

      ***---
       RECUPERA-DATI-FROM-RMOVMAG-FORN.
           move 0 to prezzo-movim.
           set  trovato-movim  to false.
           accept como-data-oggi from century-date.
           subtract 2 from como-anno-oggi.
           move low-value      to rmo-rec.
           set  rmo-fornitore  to true.
           move ef-forn-buf    to rmo-cod-clifor.
           move ef-cau-buf     to rmo-causale    save-causale.
           move art-codice     to rmo-articolo   save-articolo.
           move 1              to rmo-destino.
           move como-data-oggi to rmo-data-movim.
           move rmo-cod-clifor to save-cliente.
           start rmovmag key >= k-art-data
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read rmovmag next at end exit perform end-read
                    if rmo-articolo   not = save-articolo
                       exit perform
                    end-if
                    if rmo-fornitore and
                       rmo-cod-clifor = save-cliente
                       set trovato-movim to true
                       if prezzo-movim < 
                        ( rmo-netto + rmo-coubat + rmo-imp-cons )
                          compute prezzo-movim =
                                  rmo-netto    + 
                                  rmo-coubat   + 
                                  rmo-imp-cons
                          
                          compute rmo-netto =
                                  prezzo-movim - 
                                  rmo-coubat   - 
                                  rmo-imp-cons

                          move rmo-netto    to netto-movim
                          move rmo-coubat   to coubat-movim
                          move rmo-imp-cons to cons-movim
                       end-if
                    end-if
                 end-perform
           end-start.

           if trovato-movim
LUBEXX        move prezzo-movim to ror-prz-unitario
              move netto-movim  to rmo-netto
              move coubat-movim to rmo-coubat
              move cons-movim   to rmo-imp-cons
           end-if.

      ***---
       RECUPERA-PREZZO.
           set hid-bloccato to false.
      *****     if SaveGDO = spaces
      *****        move art-prezzo-vendita   to ef-uni-buf 
      *****                                     hid-prezzo
      *****                                     ror-prz-unitario
      *****     else
      *****        if SiAssortimento           
      *****           if si-listino
      *****              continue
      *****              |move asc-cod-listino-vendita to codice listino
      *****              |LEGGI LISTINO
      *****           else
      *****              move asc-prezzo-finito    to ef-uni-buf
      *****                                           hid-prezzo
      *****                                           ror-prz-unitario
      *****           end-if
      *****        else
           set prezzo-sp    to false.
LABLAB     if tcl-si-recupero and tca-prezzo-reso-no
              if hid-prz-commle = 0
                 if volantino-forzato = 0
                    set promo-future to false
                    perform CERCA-PROMO-LISTINO
                 else
                    perform FORZA-PREZZO-VOLANTINO
                    set prezzo-sp to false
                 end-if
              end-if
              if not trovato
                 move 0 to ror-prz-commle
                 move 9999999,99         to ef-uni-buf 
                                            ror-prz-unitario
                 move art-prezzo-vendita to hid-prezzo
                 set  hid-bloccato       to true
                 move 0                  to hid-promo
              else
                 if not prezzo-sp
                    move rpr-codice      to ror-promo
                    move rpr-codice      to hid-promo
                 end-if
                 move rpr-prz-acq        to ef-uni-buf hid-prezzo
                                            ror-prz-unitario
                                            ror-prz-commle
              end-if
           else
              if trovato-movim
LUBEXX           if ttipocli-gdo or tca-prezzo-reso-si
LUBEXX              compute ror-prz-unitario = 
LUBEXX                      rmo-netto + rmo-coubat + rmo-imp-cons
LUBEXX              move ror-prz-unitario to ef-uni-buf
LUBEXX                                       hid-prezzo
LUBEXX           else
LUBEXX              move rmo-netto    to ef-uni-buf
LUBEXX                                   hid-prezzo
LUBEXX                                   ror-prz-unitario
LUBEXX           end-if
              else
                 move 0            to ef-sconto-buf
                                      hid-sconto
                                      ror-perce-sconto
                 move 9999999,99 to ef-uni-buf
                                      ror-prz-unitario
                 move art-prezzo-vendita to hid-prezzo
              end-if
           end-if.
      *****        end-if
      *****     end-if. 

      ***---
       RECUPERA-SCONTO.
      *****     if SaveGDO = spaces
      *****        move 0 to ef-sconto-buf
      *****     else
           if SiAssortimento
              move asc-perc-sconto-listino to ef-sconto-buf
           else
              move 0 to ef-sconto-buf
           end-if.
      *****     end-if.   
           
      ***---
       CALCOLA-IMPOSTE-ORDINE.
           move 0 to ef-cons-buf col-cons.
           set lab-imp-coubat to true.
LUBEXX     perform IMPOSTE.

      ***---
       IMPOSTE.      
           inquire ef-anno value in ef-anno-buf.
           move ef-anno-buf   to con-anno.
           open input tcontat.
           read tcontat no lock.
           close tcontat.

           if tor-data-bolla not = 0
              move tor-data-bolla to imp-data
           else
              evaluate tcl-serie-bolle
              when 1 move con-ult-stampa-bolle-gdo to imp-data
              when 2 move con-ult-stampa-bolle-mv  to imp-data
              when 3 move con-ult-stampa-bolle-at  to imp-data
              end-evaluate
           end-if.

           start timposte key <= imp-chiave
                 invalid continue
             not invalid
                 read timposte previous
           end-start.

           move 0 to imposta-cobat imposta-cou add-piombo
                     ror-imp-cou-cobat ror-imp-consumo 
                     ror-add-piombo.
           |L'ho dovuta searare in quanto, se TROVO il movimento di
           |magazzino ma poi cambio il prezzo devo rifare il calcolo
           |delle imposte. Viene richiamato sull'after del campo
           |prezzo ossia quando viene cambiato
           if SiAssortimento
              move asc-imposta-consumo          to ef-cons-buf
                                                   col-cons
              move asc-imposta-ecologica-coubat to ef-cou-buf
                                                   col-cou
              subtract asc-imposta-ecologica-coubat from
                       ror-prz-unitario
              move ror-prz-unitario to ef-uni-buf
           else           
              if des-prog = 0
                 move cli-nazione to naz-codice
              else
                 move des-nazione to naz-codice
              end-if
              read tnazioni no lock

              if naz-imp-esenti-si
                 perform CALCOLO-IMPOSTE-ESTERO
              else
                 evaluate true
                 when ttipocli-standard perform CALCOLO-IMPOSTE-STANDARD
                 when ttipocli-gdo      perform CALCOLO-IMPOSTE-GDO
                 end-evaluate
              end-if
           end-if.

      ***---
       CALCOLO-IMPOSTE-ESTERO.
           move 0 to ror-imp-consumo ror-imp-cou-cobat ror-add-piombo.
           move 0 to col-cou  ef-cou-buf  imposta-cou.
           move 0 to col-cons ef-cons-buf imposta-consumo.
           move 0 to col-add  ef-add-buf add-piombo.

      ***---
       CALCOLO-IMPOSTE-STANDARD.
           move 0 to col-add  ef-add-buf add-piombo.
           move 0 to col-cou  ef-cou-buf imposta-cou.
           move 0 to col-cons ef-cons-buf.
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
                 move como-imposta      to ror-imp-consumo
                 move ror-imp-consumo   to ef-cons-buf col-cons
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
                 move como-imposta      to ror-imp-cou-cobat imposta-cou
                 move ror-imp-cou-cobat to ef-cou-buf col-cou
                 set lab-imp-cou to true
              else
                 move 0 to imposta-cou
              end-if

           else
              move 0 to ror-imp-consumo ef-cons-buf col-cons
      
           end-if.
      *           
           inquire ef-art, value in art-codice.
           read articoli no lock
                invalid
                set errori to true
                display message "ERRORE"
           end-read
           if art-si-cobat and tutto-ok
              set lab-imp-cobat to true
              perform CALCOLA-COBAT
           end-if.

      ***---
       CALCOLO-IMPOSTE-GDO.
           move 0 to col-add ef-add-buf add-piombo.
           move 0 to col-cou ef-cou-buf imposta-cou.
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
              move como-imposta      to ror-imp-consumo
              move ror-imp-consumo   to ef-cons-buf col-cons
           else
              move 0 to ror-imp-consumo ef-cons-buf col-cons
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
           move como-imposta      to ror-imp-cou-cobat imposta-cou.
           move ror-imp-cou-cobat to ef-cou-buf col-cou.

           set lab-imp-cou to true.
      *     
           inquire ef-art, value in art-codice.
           read articoli no lock
                invalid
                set errori to true
                display message "ERRORE"
           end-read
           if art-si-cobat and tutto-ok
              set lab-imp-cobat to true
              perform CALCOLA-COBAT
              if tcl-si-piombo
                 perform CALCOLA-ADD-PIOMBO
              end-if
           end-if.

      ***---
       CALCOLA-COBAT.
           if ttipocli-gdo or mar-si-cobat
              perform SCAGLIONI-COBAT

              if imposta-cou = 0
                 set lab-imp-cobat  to true
              else
                 set lab-imp-coubat to true
              end-if
           end-if.

      ***---
       SCAGLIONI-COBAT.
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
           end-evaluate.
           
           add 0,005              to imposta-cobat.
           add  imposta-cou       to imposta-cobat.
           move imposta-cobat     to ef-cou-buf col-cou 
                                     ror-imp-cou-cobat.

      ***---
       CALCOLA-ADD-PIOMBO.                     
           move ef-cli-buf to como-prm-cliente convert.
           move ef-des-buf to como-prm-destino convert.

           move ror-prz-unitario   to como-prz-unitario.
           move ror-imp-cou-cobat  to como-imp-cou-cobat.

           move imposta-cobat      to ror-imp-cou-cobat.
           move art-marca-prodotto to tpb-marca. 
           move tor-data-ordine    to como-data-ordine tpb-data.
           |Se devo ricalcolare l'addizionale piombo su un
           |ordine senza data, ossia creato da piu master
           if tor-data-ordine = 0 and tor-da-ordine-si
              if ror-anno-master    not = 0 and
                 ror-numero-master  not = 0
                 move ror-chiave-ordine-testa to mto-chiave
                 read mtordini no lock
                      invalid continue
                  not invalid move mto-data-ordine 
                                to como-data-ordine tpb-data
                 end-read
              end-if
           end-if.
           perform ADDIZIONALE-PIOMBO.
           move add-piombo to ror-add-piombo ef-add-buf col-add.

      ***---
       CALCOLA-IMPONIBILE.
           if trovato-movim
              if ttipocli-gdo or tca-prezzo-reso-si
                 compute ror-imponib-merce =
                         ror-prz-unitario  -
                         ror-imp-consumo   -
                         ror-imp-cou-cobat -
                         ror-add-piombo
              else
                 compute ror-imponib-merce =
                         ror-prz-unitario  
              end-if
           else   
              evaluate true
              when ttipocli-gdo
                   compute ror-imponib-merce = 
                           ror-prz-unitario  - 
                           ror-imp-cou-cobat - 
                           ror-imp-consumo   -
                           ror-add-piombo
              when other
                   if ror-prz-unitario >= 9999999,99
                      move 9999999,99 to ror-imponib-merce
                   else
                      compute ror-imponib-merce = 
                              ror-prz-unitario  
                   end-if
              end-evaluate
           end-if.
           move ror-imponib-merce to ef-imp-buf col-imp.
      *****     if SaveGDO = spaces
      *****        compute ror-imponib-merce = ror-prz-unitario - 
      *****        (( ror-prz-unitario * ror-perce-sconto ) / 100)
      *****        move ror-imponib-merce to ef-imp-buf col-imp
      *****     else
      *****        if SiAssortimento
      *****           if si-listino
      *****              compute ror-imponib-merce = ror-prz-unitario - 
      *****              (( ror-prz-unitario * asc-perc-sconto-listino )/100)
      *****           else
      *****              compute ror-imponib-merce =
      *****                      asc-prezzo-finito   - 
      *****                      asc-imposta-consumo - 
      *****                      asc-imposta-ecologica-coubat
      *****           end-if
      *****        else
      *****           if trovato-movim
      *****              compute ror-imponib-merce = 
      *****                      rmo-netto    -
      *****                      rmo-imp-cons -
      *****                      rmo-coubat
      *****           else
      *****              move 0 to ror-imponib-merce
      *****           end-if
      *****        end-if
      *****        move ror-imponib-merce to ef-imp-buf col-imp
      *****     end-if.

      ***---
       RECUPERA-IVA.          
           if ef-iva-buf not = spaces
              move ef-iva-buf     to ef-cod-iva-buf 
                                     tbliv-codice2
                                     col-iva
           else
              move art-codice-iva to ef-cod-iva-buf 
                                     tbliv-codice2
                                     col-iva
           end-if.
           move "IV"   to tbliv-codice1.
           move spaces to tbliv-descrizione1
                          tbliv-descrizione2.
           read tivaese no lock invalid continue end-read.
           perform MOVE-DESCR-IVA-2.

      ***---
       IMBALLI-QTA.
           move 0 to resto.
           if ror-qta >= imq-qta-imb|hid-imballi
              divide ror-qta by imq-qta-imb giving ris
                                         remainder resto
              |divide ror-qta by hid-imballi giving ris
              |                           remainder resto
           else
              move 1 to resto
              move 0 to ris
           end-if.

           if resto > 0
              move imq-qta-imb|hid-imballi 
                to imballi-ed
              display message "Imballo standard pezzi ", imballi-ed, "."
                       x"0d0a""Correzione automatica?"
                        title titolo
                         type mb-yes-no-cancel
                       giving scelta
                         icon mb-warning-icon
                      default mb-cancel
              compute ror-qta = ( imq-qta-imb * ( ris + 1 ) )
                                 |hid-imballi * ( ris + 1 ) )
           else
              move mb-yes to scelta
           end-if.

           evaluate scelta
           when mb-yes
                |MODIFICA RICHIESTA DA TRIVELLA 080306
                 move hid-des-imballo to link-des
                 move imq-qta-imb     to imballi-ed hid-imballi
                 perform DESCRIZIONE-IMBALLO
                |FINE MODIFICA
                set tutto-ok to true
                move ror-qta to ef-qta-buf col-qta
                display ef-qta
           when mb-no
                set  link-blister    to false
                move hid-des-imballo to link-des
                move ef-qta-buf      to link-qta
                move art-udm-imballo to link-udm
                call   "imballo" using imballo-linkage
                cancel "imballo"
                if link-imballo-saved = 1
                   move link-qta       to imballi-ed
                   perform DESCRIZIONE-IMBALLO
                else
                   set errori to true
                end-if
           when mb-cancel
                set errori to true
                move 78-ID-ef-qta to control-id
                move 4 to accept-control
           end-evaluate.

      ***---
       DESCRIZIONE-IMBALLO.
           call "C$JUSTIFY" using imballi-ed, "L".
           inspect art-descrizione replacing trailing spaces 
                                          by low-value.
           inspect link-des        replacing trailing spaces 
                                          by low-value.
           initialize lab-art-buf.
BLISTR     if chk-blister-buf = 1
BLISTR        string  art-descrizione delimited by low-value
BLISTR                " - "           delimited by size
BLISTR                link-des        delimited by low-value
BLISTR               " ("            delimited size
BLISTR               imballi-ed      delimited spaces
BLISTR               ")"             delimited size
BLISTR                into lab-art-buf
BLISTR        end-string
           else
              string  art-descrizione delimited by low-value
                      " - "           delimited by size
                      link-des        delimited by low-value
                      " da "          delimited by size
                      imballi-ed      delimited by spaces
                      " x "           delimited by size
                      art-udm-imballo delimited by size
                      into lab-art-buf
              end-string
           end-if.
           inspect art-descrizione replacing trailing low-value by space
           inspect link-des        replacing trailing low-value by space
           display lab-art.
           move lab-art-buf to col-des.

      ***--- 
       VALUTA-CAMBIO-PREZZO.
           if hid-prezzo = 0
              move art-prezzo-vendita to hid-prezzo
           end-if.
           set tutto-ok to true.
           move ror-prz-unitario to SavePrezzo.
           evaluate true
           when ror-prz-unitario = hid-prezzo
                move SavePrezzo to ror-prz-unitario
                perform IMPOSTE
           when ror-prz-unitario > hid-prezzo
                compute como-prezzo =
                ( hid-prezzo + ( ( hid-prezzo * hid-var-piu ) / 100 ) )
                if SavePrezzo > como-prezzo
LUBEXX             if si-controlla-scostamento |CBLCONFI
LUBEXX                display message "Prezzo non nei limiti previsti."
LUBEXX                             x"0d0a""Proseguire comunque?"
LUBEXX                          title titolo
LUBEXX                           icon mb-warning-icon
LUBEXX                           type mb-yes-no
LUBEXX                         giving scelta
LUBEXX             else
LUBEXX                move mb-yes to scelta
LUBEXX             end-if
LUBEXX          else
LUBEXX             move mb-yes to scelta
LUBEXX          end-if

                if scelta = mb-no 
                   set errori to true 
                else
                   move SavePrezzo to ror-prz-unitario
                   perform IMPOSTE
                end-if

           when ror-prz-unitario < hid-prezzo
                compute como-prezzo =
                ( hid-prezzo - ( ( hid-prezzo * hid-var-meno ) / 100 ) )
                if SavePrezzo < como-prezzo
LUBEXX             if si-controlla-scostamento |CBLCONFI
LUBEXX                display message "Prezzo non nei limiti previsti."
LUBEXX                         x"0d0a""Proseguire comunque?"
LUBEXX                          title titolo
LUBEXX                           icon mb-warning-icon
LUBEXX                           type mb-yes-no
LUBEXX                         giving scelta
LUBEXX             else
LUBEXX                move mb-yes to scelta
LUBEXX             end-if
LUBEXX          else
LUBEXX             move mb-yes to scelta
                end-if

                if scelta = mb-no 
                   set errori to true 
                else
                   move SavePrezzo to ror-prz-unitario
                   perform IMPOSTE
                end-if

           end-evaluate.

      ***---
       PREZZO-ZERO.
LUBEXX     if tca-si-speciale
LUBEXX        move mb-yes to scelta
LUBEXX     else
              display message "Articolo omaggio?"
                        title titolo
                         type mb-yes-no
                      default mb-no
                       giving scelta
LUBEXX     end-if.

           if scelta = mb-no 
              set errori to true 
           else
              move 0 to ef-imp-buf ef-sconto-buf
                        col-imp    col-sconto
              move iva-omaggio    to ef-cod-iva-buf col-iva
              move save-descr-iva to lab-iva-2-buf
              display ef-cod-iva lab-iva-2
                      ef-imp ef-sconto
           end-if. 
 
      ***---
       FIND-MORE-ARTICOLI-ON-PROGMAG.
           move 0             to num-articoli.
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
                          prg-peso          not = 0

                          if prg-attivo
                             add 1 to num-articoli
                             evaluate true
                             when num-articoli = 1 
                                  move prg-chiave to GiacenzaKey
                             when num-articoli > 1 
                                  exit perform
                             end-evaluate
                          end-if
                       end-if
                    else
                       exit perform
                    end-if
                 end-perform
           end-start.

      ***---
       FIND-PROGMAG.
      *****     move 0 to sum      SumKey.
      *****     move 0 to giacenza GiacenzaKey.
           move 0 to imq-qta-imb hid-imballi.
           
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
           if prg-bloccato
              move spaces  to hid-tipo-imballo
           end-if.

           if hid-tipo-imballo not = spaces
              move hid-tipo-imballo     to imq-codice
           else
              move art-imballo-standard to imq-codice
           end-if.

           read timbalqta no lock invalid continue end-read.
           move imq-qta-imb     to hid-imballi imballi-ed.
           move imq-tipo        to imb-codice.
           read timballi no lock 
                invalid  initialize imb-descrizione
           end-read.               
           inspect imb-descrizione replacing trailing spaces 
                                                   by low-value.
           move imb-descrizione to hid-des-imballo.

      *****     if pgm-name = "gordcvar"
      *****        move ror-qta-imballi to hid-imballi
      *****        move ror-des-imballo to hid-des-imballo
      *****     end-if.

           perform SOMMA-DINAMICI.

           perform LABEL-VALORI.

           move HiddenKey to prg-chiave.
           read progmag no lock.

      *    luciano 
           move hid-imballi  to ef-qta-buf.

      ***---
       SOMMA-DINAMICI.
           move art-codice     to prg-cod-articolo.
           move art-codice     to prg-cod-articolo.
           move StoreMagazzino to prg-cod-magazzino.
           move low-value      to prg-tipo-imballo prg-peso.
           start progmag key >= prg-chiave 
                 invalid continue 
             not invalid
                 move 0 to hid-giacenza hid-impegnato hid-ordinato
                 perform until 1 = 2
                    read progmag next no lock 
                         at end exit perform 
                    end-read
                    if prg-cod-articolo  not = art-codice or
                       prg-cod-magazzino not = StoreMagazzino
                       exit perform
                    end-if
                    add prg-giacenza   to hid-giacenza
                    add prg-impegnato  to hid-impegnato
                    add prg-ordinato-1 to hid-ordinato
                 end-perform
           end-start.

      ***---
       READ-TMARCHE.
           move art-marca-prodotto to mar-codice.
           read tmarche no lock invalid continue end-read.
           move mar-ven-var-listino-meno to hid-var-meno.
           move mar-ven-var-listino-piu  to hid-var-piu.

      ***--- 
       SOMMA-PESO-UTF-MAN.           
           inquire form1-gd-1, last-row in tot-righe.
           move 0 to tot-peso.

           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 4), cell-data in como-qta
              inquire form1-gd-1(store-riga, 5), 
                      cell-data in como-prz-unitario

      *****        if como-prz-unitario not = 0

                 inquire form1-gd-1(store-riga, 1), 
                         hidden-data in gruppo-hidden
                   
                 move hid-utf to como-peso

                 compute tot-peso = (tot-peso + ( como-qta * como-peso))
                 if tot-peso > 500 
                    set errori to true
                    exit perform
                 end-if
      *****        end-if

           end-perform. 

      ***---
       SUPERAMENTO-500-UTF.
           if not deposito-utf
              perform SOMMA-PESO-UTF-MAN
              if errori
                 move tot-peso to tot-peso-edit
                 display message "Superamento 500 Kg. UTF."
                     x"0d0a""Peso raggiunto: ", tot-peso-edit, " Kg."
                           title tit-err
                            icon 2
                          giving scelta
                 perform CANCELLA-COLORE
              end-if
           end-if.
 
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
                                    X"0d0a""PRG-CHIAVE: " prg-chiave
                                    X"0d0a""Contattare assistenza"
                                     title tit-err
                                      icon 3
              end-read

              move art-marca-prodotto to mar-codice
              read tmarche no lock invalid continue end-read

              if VenditaAlDettaglio
                 move ef-qta-buf  to ror-qta
                 move ef-imp-buf  to ror-imponib-merce
                 move ef-cou-buf  to ror-imp-cou-cobat
                 move ef-add-buf  to ror-add-piombo
                 move ef-cons-buf to ror-imp-consumo
                 compute SavePrezzo = ror-imponib-merce +
                                      ror-imp-cou-cobat +
                                      ror-add-piombo    +
                                      ror-imp-consumo
      *****           compute SavePrezzo = SavePrezzo * ror-qta
OMAGGI           compute SavePrezzo = 
OMAGGI                   SavePrezzo * ( ror-qta - ror-qta-omaggi)
                 compute como-imposta =
                         SavePrezzo * hid-perce-iva / 100
                 add 0,005           to como-imposta
                 add SavePrezzo      to como-imposta
                 move como-imposta   to lab-ivato-buf
                 display lab-ivato
              end-if

           end-if.           
                     
           perform CANCELLA-COLORE.      
           perform COLORE. 

      *****     set ArticoloSetFocus to true.
           set event-action     to event-action-terminate.
           set FromSpostamento  to true.
           set ControllaCampi   to false.

      ***---
       LABEL-VALORI.
           if hid-giacenza >= 0 move 515 to ColorGiacenza
           else                 move 525 to ColorGiacenza
           end-if.
           move hid-giacenza  to giacenza-ed.

           if hid-impegnato >= 0 move 515 to ColorImpegnato
           else                  move 525 to ColorImpegnato
           end-if.
           move hid-impegnato to impegnato-ed.

           if hid-ordinato >= 0 move 515 to ColorOrdinato
           else                 move 525 to ColorOrdinato
           end-if.
           move hid-ordinato to ordinato-ed. 

      ***---
       ASSORCLI-IN-LINE.
           if link-path-tmp-assorcli not = spaces
              move tmp-asc-cod-articolo-per-cliente to hid-cod-art-cli
              move SaveGDO    to tmp-asc-cod-gruppo-gdo
              move ef-cli-buf to tmp-asc-cod-cliente         convert
              move ef-des-buf to tmp-asc-progressivo-destino convert
              move art-codice to tmp-asc-cod-articolo
              read tmp-assorcli
                   invalid move low-value  to tmp-asc-chiave
                           move SaveGDO    to tmp-asc-cod-gruppo-gdo

                           move art-codice to tmp-asc-cod-articolo
                           start tmp-assorcli key is >= key01
                                 invalid set trovato to false
                             not invalid
                                 read tmp-assorcli next
                                 if art-codice = tmp-asc-cod-articolo
                                    move art-codice to SaveArticolo
                                    set trovato to true
                                 else
                                    set trovato to false
                                 end-if
                           end-start
               not invalid set trovato to true
              end-read
              if trovato perform METTI-IN-CELLS end-if
           end-if.

      ***---
       METTI-IN-CELLS.
           set trovato to true.
           move tmp-asc-prezzo-finito             to asc-prezzo-finito.
           move tmp-asc-perc-sconto-listino       to col-sconto
                                                      ef-sconto-buf
                                                 asc-perc-sconto-listino
           move tmp-asc-imposta-consumo           to col-cons
                                                      ef-cons-buf
                                                     asc-imposta-consumo
           move tmp-asc-imposta-ecologica-coubat  to col-cou
                                                      ef-cou-buf
                                           asc-imposta-ecologica-coubat.
           move tmp-asc-cod-articolo-per-cliente to hid-cod-art-cli.

      ***---
       CHANGE-TAB.
           move 0 to v-master v-reltor v-evasione.
           set NonCambiareTab   to false.
      *****     set ArticoloSetFocus to false.
           set ControllaCampi   to true.

           if mod-k = 1
              set NonCambiareTab to true
           else
              evaluate EVENT-DATA-1
              when 1
                   if control-id >= 78-ID-ef-art
                      set ControllaCampi   to false
                   end-if
                   if DatiBollaManuale  move 1 to v-bolla
                   else                 move 0 to v-bolla
                   end-if
                   move 0 to v-dett v-blister v-evasione
                   perform ABILITA-GEST-PLUS
                   if tor-da-ordine-si
                      if OrdineAnno = 0 and OrdineNumero = 0
                         move 1 to v-master
                      end-if
                   else
                      if tor-anno-testa not = 0 or
                         tor-num-testa  not = 0
                         move 1 to v-reltor
                      end-if
                   end-if

                   if tca-prezzo-reso-si
                      move 1 to v-forn
                   end-if

              when 2
                   perform CHECK-PAGE-1
                   if tutto-ok
                      move 0 to v-gest-plus v-reltor v-master v-forn
                      display lab-gest ef-gest
                      perform CANCELLA-COLORE
                      evaluate form1-radio-1-buf
                      when 1 |MANUALE
                           if FirstTime
                              if pgm-name = "gordc"
                                |Per gordc non è ancora stato
                                |settato il tipo di caricamento
                                |sul file di testata
                                 set tor-manuale to true
                              end-if
                              perform DISPLAY-SCREEN-MANUALE
                              |Rimane a TRUE solo se no ci sono righe,
                              |ossia in caso di transazione in corso...
                              if FirstTime
                                 set errori to true
                                 set ControllaCampi to false
                                 move store-id to CONTROL-ID
                                 move 4        to ACCEPT-CONTROL
                                 set NonCambiareTab to true
                              else
                                 move 2 to save-riga
                                 perform SETTA-RIGA
                                 set ControllaCampi to true
                              end-if
                           end-if
                           |Settato a errori da "if FirstTime" sopra
                           if tutto-ok
      *****                     set ArticoloSetFocus to true
                              if ttipocli-gdo
                                 modify ef-sconto,     read-only
                              else
                                 if pgm-name = "gordc" and 
                                    OrdineTradizionale
                                    modify ef-sconto, read-only
                                 else
                                    modify ef-sconto, not read-only
                                 end-if
                              end-if
                              if tcl-si-recupero
                                 move 1 to v-blister
                              end-if
                              if pgm-name = "gordc"
                                 move 1 to v-evasione
                              else
                                 if tor-da-ordine-si
                                    move 0 to v-evasione
                                 else
                                    move 1 to v-evasione
                                 end-if
                              end-if
                           end-if
                      end-evaluate
                      move 0 to v-bolla
                      if VenditaAlDettaglio
                         move 1 to v-dett
                      end-if
                   else
                      set ControllaCampi to false
                      move store-id to CONTROL-ID
                      move 4        to ACCEPT-CONTROL
                      set NonCambiareTab to true
                   end-if
              end-evaluate
           end-if.
                              
           display Screen1-Ta-1.

      ***---
       DISPLAY-CAMPI-BOLLA.
           display frame-bolla ef-num-bolla ef-data-bolla lab1 lab2.

      ***---
       MOVE-DATI.
           |Non sono sulla chiave perciò il cliente l'ho già confermato
           |e se ci passassi sul salvataggio sovrascriverei i dati
           |già magari cambiati dall'utente
           if pgm-name = "gordcvar" and mod-cliente-destino = 0
              exit paragraph
           end-if.
           initialize lab-iva-buf.
           move des-prov to SaveProvincia.
           move spaces to lab-tipo-iva-buf

      *****     if ef-iva-buf = spaces
              move spaces to ef-iva-buf tbliv-codice2
              if cli-iva not = spaces
                 move cli-iva to ef-iva-buf tbliv-codice2
                 move "Codice IVA" to lab-tipo-iva-buf
              end-if
              if cli-iva-ese not = spaces
                 move cli-iva-ese  to ef-iva-buf tbliv-codice2
                 move "Codice Esenzione IVA" to lab-tipo-iva-buf
              end-if
              move "IV"         to tbliv-codice1
              read tivaese invalid continue
                       not invalid perform MOVE-DESCR-IVA
              end-read
              display lab-iva ef-iva lab-tipo-iva
      *****     end-if.

           if cli-utf = "S"
              move cli-utf to flag-deposito-utf
           else
              move des-deposito-utf to flag-deposito-utf
           end-if.

           move ef-vet-buf to vet-codice.
      *****     if vet-codice = 0
              move des-vettore         to ef-vet-buf vet-codice
              move spaces to lab-vet-buf
              read tvettori invalid continue
                        not invalid move vet-descrizione to lab-vet-buf
              end-read
              display lab-vet ef-vet
      *****     end-if.

      *****     move cli-gdo     to SaveGDO.
           move ef-cli-buf  to not-codice convert.
           move ef-des-buf  to not-prog   convert.
           read note invalid continue
                 not invalid move not-note-1 to ef-note-1-buf
                             move not-note-2 to ef-note-2-buf
                             move not-note-3 to ef-note-3-buf
                             move not-note-4 to ef-note-4-buf
                             move not-data   to como-data
                             perform DATE-TO-SCREEN
                             move como-data  to ef-data-cons-buf
                             display ef-note-1
                                     ef-note-2
                                     ef-note-3
                                     ef-note-4
                                     ef-data-cons
           end-read.
           
           if ef-pag-buf = spaces
              move spaces  to lab-pag-buf
              move "PA"    to tblpa-codice1
              move cli-pag to tblpa-codice2 ef-pag-buf
              read tcodpag invalid continue
                       not invalid perform MOVE-DESCR-PAG
              end-read
              display lab-pag ef-pag
           end-if.

           move ef-pag-buf to SavePag.
           move ef-iva-buf to SaveIva.
           move ef-vet-buf to SaveVet.

           move ef-age-buf to age-codice.
      *****     if age-codice = 0
              move spaces      to lab-age-buf
              move cli-agente  to age-codice ef-age-buf
              read agenti invalid continue
                      not invalid move age-ragsoc-1 to lab-age-buf
              end-read
              display lab-age ef-age
      *****     end-if.

           initialize tcl-rec.                                                  
           move cli-tipo to tcl-codice
           read ttipocli no lock
                invalid  
                move spaces to TrattamentoInUso SaveGDO
            not invalid  
                move tcl-tipologia-tratt-imposte to TrattamentoInuso
                evaluate true
                when tcl-gdo-no  move spaces  to SaveGDO
                when other       move cli-gdo to SaveGDO
                end-evaluate
           end-read
           perform ABILITA-GEST-PLUS.

           if mod-campi = 0 |Solo la prima volta quando sono ancora in chiave
              move des-note-bolla-1 to ef-note-bolla-1-buf
              move des-note-bolla-2 to ef-note-bolla-2-buf
           end-if.

      ***---
       CONTA-ZERI-MAN.
           set trovato to false.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 4), cell-data in col-qta
              move col-qta to como-qta
              if como-qta > 0
                 set trovato to true
                 exit perform
              end-if
           end-perform.

      ***---
       BEFORE-CAMPI.
LABLAB     move 0 to e-cerca.
LABLAB     modify tool-cerca, enabled e-cerca.
           evaluate control-id
           when 78-ID-ef-cau
           when 78-ID-ef-cli
           when 78-ID-ef-des
                continue |lascio posare il fuoco su quei campi
                         |altrimenti mi risulta impossibile cambiare
                         |la causale, il cliente o il destino se mi
                         |trovo nella seconda pagina
           when other
                if FromSpostamento
                   set FromSpostamento to false
                   move 78-ID-ef-art   to control-id
                   move 4              to accept-control
                   perform CANCELLA-COLORE
                else
                   inquire ef-art, value in art-codice
                   if art-codice = 0
                      perform CANCELLA-COLORE
                      move 78-ID-ef-art to control-id
                      move 4            to accept-control
                   end-if
                end-if
                if pgm-name = "gordc" and 
                   OrdineTradizionale
LABLAB             modify ef-uni, read-only
                else
LABLAB             modify ef-uni, not read-only
                end-if
LABLAB          if control-id = 78-ID-ef-uni
LABLAB             if tcl-si-recupero and tca-prezzo-reso-no
LABLAB                move BitmapZoomEnabled to BitmapNumZoom
LABLAB                move 1 to e-cerca
LABLAB                display tool-cerca
                      |Solo in INSERIMENTO
LABLAB                if pgm-name = "gordc"
LABLAB                   if ef-uni-buf not = 0
LABLAB                      modify ef-uni, read-only
LABLAB                   end-if
LABLAB                end-if
LABLAB             end-if
LABLAB          end-if
                if ror-anno-master not = 0
                   modify ef-uni,    read-only
                   modify ef-sconto, read-only
                end-if
           end-evaluate.
           set CheckAfterZoom to false.

      ***---
       CONTROLLO.
           if mod   = 0 and
              mod-k = 0 
              exit paragraph 
           end-if.
           set tutto-ok to true.
           if not ControllaCampi
              set ControllaCampi to true
              exit paragraph
           end-if.
      *
      * Elenco degli ID sui quali fare il CONTROLLO nel programma gclienti
      * paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-anno è l'ID del control ef-anno
           when 78-ID-ef-anno
                inquire ef-anno, value in ef-anno-buf
                if ef-anno-buf = 0
                   set errori to true
                   display message box "Inserimento anno mancante"
                           title = tit-err
                           icon mb-warning-icon
                   move 78-ID-ef-anno to control-id
                end-if
                move ef-anno-buf to como-anno

           |78-ID-ef-cau è l'ID del control ef-cau
           when 78-ID-ef-cau
                move 0 to v-forn v-inviato
                modify ef-age, not read-only
                modify ef-pag, not read-only
                modify ef-iva, not read-only
                modify ef-vet, not read-only
                set VenditaAlDettaglio to false
                inquire ef-cau, value in ef-cau-buf
                move ef-cau-buf to tca-codice
                move 0 to v-bolla
                set DatiBollaManuale to false
                read tcaumag
                     invalid
                     move spaces to tca-descrizione
                     set errori to true
                     display message "Causale di magazzino NON valida"
                             title = tit-err
                             icon mb-warning-icon
                     move 78-ID-ef-cau to control-id
                 not invalid
                     if tca-si-emissione
                        move 1 to v-contras
                     else
                        move 0 to v-contras
                     end-if
                     display lab-contras chk-contrassegno
                     if tca-blocco-modeva-si
                        move 1 to v-inviato
                     end-if
                     if tca-no-stampa
                        move 1               to v-bolla
                        set DatiBollaManuale to true
                     end-if
                     if ef-cau-buf = tge-causale-corrisp
                        set VenditaAlDettaglio to true
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

                     if tca-fornitore
                        move spaces to tca-descrizione
                        set errori to true
                        display message
                                "Utilizzare una causale di tipo Cliente"
                                  title tit-err
                                   icon 2
                        move 78-ID-ef-cau to control-id
                     end-if

                     move tca-cod-magaz   to mag-codice
                     read tmagaz no lock  invalid continue end-read
                     move mag-descrizione to StoreDesMagazzino

                     if old-magazzino not = tca-cod-magaz and
                        pgm-name = "gordcvar"
                        set CambiatoMagazzino to true
                     end-if

                     if tca-prezzo-reso-si
                        move 1 to v-forn
                        display LabFor ef-forn lab-forn
                     end-if
                     move tca-cod-magaz to StoreMagazzino

                end-read
                display lab-inviato
                move tca-descrizione to lab-cau-buf
                display lab-cau
                perform DISPLAY-CAMPI-BOLLA
                if VenditaAlDettaglio
                   perform TESTATA-DETTAGLIO
                else
                   move 0 to v-dett
                end-if
                if tutto-ok
                   inquire ef-pag, value in ef-pag-buf
                   if ef-pag-buf            = spaces and
                      tca-cod-pagamento not = spaces
                      move tca-cod-pagamento to ef-pag-buf
                      display ef-pag
                      move "PA"       to tblpa-codice1
                      move ef-pag-buf to tblpa-codice2
                      read tcodpag
                           invalid continue
                       not invalid 
                           perform MOVE-DESCR-PAG
                           display lab-pag
                      end-read
                   end-if

LUBEXX             if not StoSalvando
                      move 78-ID-ef-cli to control-id store-id
                      move 4 to accept-control
LUBEXX             end-if
                end-if

           |78-ID-ef-cli è l'ID del control ef-cli
           when 78-ID-ef-cli
                inquire ef-cli, value in ef-cli-buf
                if ef-cli-buf not = spaces
                   inspect ef-cli-buf replacing trailing spaces
                                                         by low-values
                   initialize CountChar
                   inspect ef-cli-buf tallying CountChar for characters
                                                      before low-value
                   inspect ef-cli-buf replacing trailing low-values
                                                         by spaces

                   | SE E' NUMERICO LEGGO IL RECORD NORMALMENTE
                   if ef-cli-buf(1:CountChar) is numeric
                      perform SELEZIONA-NUMERICO
                   else
                      | HO DIGITATO UNA SIGLA ALFANUMERICA QUINDI
                      | APRO LO ZOOM (CASE SENSITIVE)
                      perform SELEZIONA-ALFA
                   end-if
                   display lab-cli
                   display lab-ind-cli
                   display lab-loc-cli
                   move cli-codice to rec-codice
                   read recapiti no lock invalid continue end-read

                else
                   set errori to true
                   move 78-ID-ef-cli to control-id
                   display message "Inserimento codice cliente mancante"
                           title = tit-err
                           icon mb-warning-icon
                end-if
                if tutto-ok
      *****             if cli-gdo    = spaces
      *****                move 0 to e-gui
      *****             else
                      if SiAssortimento
                         move low-value to asc-chiave
                         move cli-gdo   to asc-cod-gruppo-gdo
                         start assorcli key is >= asc-chiave
                               invalid set errori to true
                           not invalid
                               read assorcli next
                               if asc-cod-gruppo-gdo not = cli-gdo
                                  set errori to true
      *****                         else
      *****                            move 1 to e-gui
                               end-if
                         end-start
      *****                else
      *****                   move 0 to e-gui
      *****                end-if
      *****                display rb-gui
                   end-if
                   if tutto-ok

                      if cli-codice      not = old-tor-cod-cli and
                         pgm-name            = "gordcvar"      and
                         mod-cliente-destino = 1 |sono sulla chiave
                         if not StoSalvando
                            move spaces to ef-des-buf
                                           lab-des-buf
                                           lab-ind-des-buf
                                           lab-loc-des-buf
                            display lab-des lab-loc-des 
                                    lab-ind-des ef-des
                         end-if
                      end-if

                      if cli-codice      not = old-tor-cod-cli and
                         ef-des-buf          = spaces
                         move cli-vettore      to des-vettore
                         move cli-utf          to des-deposito-utf
                         move cli-prov         to des-prov
                         perform MOVE-DATI

      *****                   if tcl-bloc-auto-si
      *****                      move cli-piva to sf-piva
      *****                      read sitfin no lock
      *****                           invalid continue
      *****                       not invalid
      *****                           if not sf-verifica-1-ok
      *****                              display message 
      *****              "Impossibile inserire ordini per questo cliente:"
      *****       x"0d0a""la situazione finanziaria attuale non lo consente!"
      *****                                        title tit-err
      *****                                         icon 2
      *****                              set errori to true
      *****                           end-if
      *****                      end-read
      *****                   end-if
                      end-if

LUBEXX                if not StoSalvando and tutto-ok
                         move 78-ID-ef-des to control-id store-id
                         move 4            to accept-control
LUBEXX                end-if

                   else
                      display message box "Impossibile procedere."
                                   X"0d0a""Assortimento non presente"
                                          " per gruppo GDO"
                              title = tit-err
                              icon mb-warning-icon
                   end-if
                end-if
                if tutto-ok
                   move cli-codice   to save-cli-codice
                   if tca-prezzo-reso-si and ef-forn-buf = 0
                      move cli-ragsoc-1 to save-cli-ragsoc-1
                      move low-value to cli-rec
                      set cli-tipo-F    to true
                      move save-cli-ragsoc-1 to cli-ragsoc-1
                      start clienti key >= cli-K1 of clienti
                            invalid continue
                        not invalid
                            read clienti next
                            if cli-ragsoc-1 = save-cli-ragsoc-1 and
                               cli-tipo-F
                               move cli-codice   to ef-forn-buf
                               move cli-ragsoc-1 to lab-forn-buf
                               display ef-forn lab-forn
                            end-if 
                      end-start

                      perform RIALLINEA-CLIENTE
                   end-if
                end-if

           |78-ID-ef-des è l'ID del control ef-des
           when 78-ID-ef-des
                inquire ef-des, value in ef-des-buf
                if ef-des-buf not = spaces
                   inspect ef-des-buf replacing trailing spaces
                                                         by low-values
                   initialize CountChar
                   inspect ef-des-buf tallying CountChar for characters
                                                       before low-value
                   inspect ef-des-buf replacing trailing low-values
                                                         by spaces

                   | SE E' NUMERICO LEGGO IL RECORD NORMALMENTE
                   if ef-des-buf(1:CountChar) is numeric
                      perform SELEZIONA-DESTINO-NUMERICO
                   else
                      | HO DIGITATO UNA SIGLA ALFANUMERICA QUINDI
                      | APRO LO ZOOM (CASE SENSITIVE)
                      perform SELEZIONA-DESTINO-ALFA
                   end-if
                   display lab-des
                   display lab-ind-des
                   display lab-loc-des
                else
                   if not StoSalvando
                      perform TROVA-DESTINO
                      if trovato
                         display message "Esiste uno o più destini per"
                                         " il cliente specificato."
                                  x"0d0a""Procedere comunque con "
                                         "progressivo non valorizzato?"
                                 title = titolo
                                 type mb-yes-no
                                 giving scelta
                         if scelta = mb-no
                            set errori to true
                            move 78-ID-ef-cli to control-id
                         else
                            move spaces to lab-des-buf
                                           lab-ind-des-buf
                                           lab-loc-des-buf
                            display lab-des lab-loc-des lab-ind-des
                         end-if
                      else
                         move spaces to lab-des-buf
                                        lab-ind-des-buf
                                        lab-loc-des-buf
                         display lab-des lab-loc-des lab-ind-des
                      end-if
                   end-if
                end-if
                if tutto-ok
                   if des-codice not = old-tor-cod-cli or
                      des-prog   not = old-tor-prg-destino
                      if ef-des-buf not = spaces
                         perform MOVE-DATI
                      end-if
                      move 0 to mod-k
                      move 1 to mod mod-campi e-man |e-gui
                      perform DISPLAY-SCREEN               
                      move 78-ID-ef-num-ord to control-id
                      move 4                to accept-control
                      inquire ef-des,    value ef-des-buf
                      move ef-des-buf       to des-codice convert
                      if SiAssortimento
                         move spaces           to link-path-tmp-assorcli  
                         move cli-codice       to link-cliente
                         move cli-gdo          to link-gdo
                         move des-codice       to link-destino
                         call   "wassorcli" using link-wassorcli
                         cancel "wassorcli"
                         if link-path-tmp-assorcli not = spaces
                           move link-path-tmp-assorcli 
                             to path-tmp-assorcli
                           open input tmp-assorcli
                         end-if
                      end-if
                   end-if
                   if pgm-name = "gordcvar"
                      move 0 to mod-cliente-destino
                      display ef-cau ef-cli ef-des

                      move 1 to e-pb-grid
                      move 5 to BitmapNumGridNuovo
                      move 4 to BitmapNumGridElimina
                      display pb-grid-elimina pb-grid-nuovo
                   end-if
                end-if

           when 78-ID-ef-num-ord

                inquire ef-num-ord, value in chk-num-ord-cli

                if chk-num-ord-cli not = spaces
                   initialize chk-ord-cli-linkage
                              replacing numeric data by zeroes
                                   alphanumeric data by spaces

                   move tor-chiave to chk-ord-chiave

                   inquire ef-cli,     value in chk-cod-cli
                   inquire ef-des,     value in chk-prg-destino
                   inquire ef-num-ord, value in chk-num-ord-cli
                   
      *****             if tor-da-ordine-si
      *****                call   "check-ord-cli" using chk-ord-cli-linkage
      *****                cancel "check-ord-cli"
      *****             else
      *****                call   "check-eva-cli" using chk-ord-cli-linkage
      *****                cancel "check-eva-cli"
      *****             end-if

                   if chk-ord-status = -1
                      display message "Esiste già l'ordine con"
                               x"0d0a""Anno: " chk-ord-anno
                               x"0d0a""Numero: " chk-ord-numero
                               x"0d0a""avente lo stesso numero ordine."
                               x"0d0a""Proseguire?"
                                title titolo
                                 type mb-yes-no
                               giving scelta
                                 icon 2
                      if scelta = mb-no
                         set errori to true
                         move 78-ID-ef-num-ord to control-id
                      end-if
                   end-if
                end-if

           |78-ID-ef-data è l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf
                move ef-data-buf to como-data
                if como-data = 0
                   if tor-da-ordine-no
                      move data-oggi to como-data
                      perform DATE-TO-SCREEN
                      move como-data to ef-data-buf
                      move data-oggi to como-data
                   end-if
                else
                   |Prima ne controllo la validita...

                   move ef-data-buf to como-data
                   perform DATE-FORMAT
                   move como-data to ef-data-buf

                   perform DATE-TO-FILE
      *****             |...poi che non sia > alla data di oggi
      *****             move ef-data-buf to como-data
      *****             perform DATE-TO-FILE
      *****             if como-data > data-oggi
      *****                set errori to true
      *****                display message "Data dell'ordine > data odierna"
      *****                        title = tit-err
      *****                        icon mb-warning-icon
      *****                move 78-ID-ef-anno to control-id
      *****             end-if
                end-if
                display ef-data
                move como-data to tor-data-ordine

           |78-ID-ef-data-pass è l'ID del control ef-data-pass
           when 78-ID-ef-data-pass
                inquire ef-data-pass, value in ef-data-pass-buf
                move ef-data-pass-buf to como-data
                if como-data = 0
                   if not VenditaAlDettaglio
                      move data-oggi to como-data
                      perform DATE-TO-SCREEN
                      move como-data to ef-data-pass-buf
                   end-if
                else
                   perform DATE-FORMAT
                   move como-data to ef-data-pass-buf
                end-if
                display ef-data-pass

           |78-ID-ef-age è l'ID del control ef-age
           when 78-ID-ef-age
                inquire ef-age, value in ef-age-buf  
                move spaces to lab-age-buf
                move ef-age-buf to age-codice
                read agenti 
                     invalid
                        set errori to true        
                        display message "Codice agente NON valido"
                                title = tit-err
                                icon mb-warning-icon
                        move 78-ID-ef-age to control-id
                 not invalid move age-ragsoc-1 to lab-age-buf
                end-read
                display lab-age

           |78-ID-ef-pag è l'ID del control ef-pag
           when 78-ID-ef-pag
                inquire ef-pag, value in ef-pag-buf  
                if ef-pag-buf not = SavePag
                   move 1 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"

                   if not Passwd-StatusOk
                      move SavePag to ef-pag-buf
                      display ef-pag
                      move "PA"       to tblpa-codice1
                      move ef-pag-buf to tblpa-codice2
                      read tcodpag
                           invalid continue
                       not invalid 
                           perform MOVE-DESCR-PAG
                           display lab-pag
                      end-read
                   else
                      move "PA"       to tblpa-codice1
                      move ef-pag-buf to tblpa-codice2
                      read tcodpag
                           invalid
                           set errori to true
                           display message "Codice pagamento NON valido"
                                     title tit-err
                                      icon mb-warning-icon
                           move 78-ID-ef-pag to control-id
                           move SavePag      to ef-pag-buf
                           display ef-pag
                       not invalid 
                           perform MOVE-DESCR-PAG
                           move ef-pag-buf to SavePag
                           display lab-pag
                      end-read
                   end-if
                end-if  
                if tutto-ok
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

           |78-ID-ef-iva è l'ID del control ef-iva
           when 78-ID-ef-iva
                inquire ef-iva, value in ef-iva-buf  
                if ef-iva-buf not = SaveIva
                   move 1 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"

                   if not Passwd-StatusOk
                      move SaveIva to ef-iva-buf
                      display ef-iva
                      move "IV"       to tbliv-codice1
                      move ef-iva-buf to tbliv-codice2
                      read tivaese
                           invalid continue
                       not invalid 
                           perform MOVE-DESCR-IVA
                      end-read
                   else
                      move "IV"       to tbliv-codice1
                      move ef-iva-buf to tbliv-codice2
                      read tivaese
                           invalid
                           if ef-iva-buf = spaces
                              move spaces to lab-iva-buf
                           else
                              set errori to true        
                              display message "Codice IVA NON valido"
                                        title tit-err
                                         icon mb-warning-icon
                              move 78-ID-ef-iva to control-id
                              move SaveIva      to ef-iva-buf
                              display ef-iva
                           end-if
                       not invalid 
      *****                     if tbliv-percentuale not = 0
      *****                        set errori to true        
      *****                        display message"Codice IVA NON valido "
      *****                                       "in quanto non esente"
      *****                                  title tit-err
      *****                                   icon mb-warning-icon
      *****                        move 78-ID-ef-iva to control-id
      *****                        move SaveIva      to ef-iva-buf
      *****                        display ef-iva
      *****                     else                   
                              perform MOVE-DESCR-IVA
                              move ef-iva-buf to SaveIva
      *****                     end-if
                      end-read
                   end-if
                end-if
                display lab-iva
                if ef-iva-buf = spaces
                   move spaces to lab-tipo-iva-buf
                else
                   if tbliv-percentuale not = 0
                      move "Codice IVA" to lab-tipo-iva-buf
                   else
                      move "Codice Esenzione IVA" to lab-tipo-iva-buf
                   end-if
                end-if
                display lab-tipo-iva

           |78-ID-ef-vet è l'ID del control ef-vet
           when 78-ID-ef-vet
                inquire ef-vet, value in ef-vet-buf  
                if ef-vet-buf = 0
                   move 0 to SaveVet
                   move spaces to lab-vet-buf
                   display lab-vet
                end-if
                if ef-vet-buf not = SaveVet 
      *****             move 21 to Passwd-password
      *****             call   "passwd" using Passwd-linkage
      *****             cancel "passwd"
      *****             if not Passwd-StatusOk
      *****                move SaveVet to ef-vet-buf
      *****                display ef-vet
      *****                move ef-vet-buf to vet-codice
      *****                read tvettori 
      *****                     invalid continue
      *****                 not invalid move vet-descrizione to lab-vet-buf
      *****                             display lab-vet
      *****                end-read
      *****             else
                      move ef-vet-buf to vet-codice
                      read tvettori
                           invalid continue
                       not invalid 
                           if vet-su-autorizz-si
                              move 12 to Passwd-password
                              call   "passwd" using Passwd-linkage
                              cancel "passwd"
                           else
                              set Passwd-StatusOk to true
                           end-if
                      end-read
                   
                      if not Passwd-StatusOk
                         move SaveVet to ef-vet-buf
                         display ef-vet
                         move ef-vet-buf to vet-codice
                         read tvettori 
                              invalid continue
                          not invalid 
                              move vet-descrizione to lab-vet-buf
                              display lab-vet
                         end-read
                      else
                         move ef-vet-buf to vet-codice
                         read tvettori
                              invalid
                              set errori to true
                              display message 
                                        "Codice vettore NON valido"
                                        title tit-err
                                         icon mb-warning-icon
                              move 78-ID-ef-vet to control-id
                              move SaveVet      to ef-vet-buf
                              display ef-vet
                          not invalid 
                              move vet-descrizione to lab-vet-buf
                              move ef-vet-buf to SaveVet
                              display lab-vet
                         end-read
                      end-if
      *****             end-if
                end-if

           |78-ID-ef-forn è l'ID del control ef-forn
           when 78-ID-ef-forn
                if tca-prezzo-reso-si
                   inquire ef-forn, value in ef-forn-buf
                   move spaces to lab-forn-buf
                   set cli-tipo-F to true
                   move ef-forn-buf to cli-codice
                   read clienti no lock
                        invalid
                        set errori to true        
                        display message "Codice fornitore NON valido"
                                  title tit-err
                                   icon mb-warning-icon
                        move 78-ID-ef-forn to control-id
                        move spaces to cli-ragsoc-1
                   end-read
                   move cli-ragsoc-1 to lab-forn-buf
                   display lab-forn

                   perform RIALLINEA-CLIENTE
                end-if

           |78-ID-ef-data-cons è l'ID del control ef-data-cons
           when 78-ID-ef-data-cons
                inquire ef-data-cons, value in ef-data-cons-buf
                move ef-data-cons-buf to como-data
                if como-data not = 0
                   perform DATE-FORMAT
                   move como-data to ef-data-cons-buf
                   display ef-data-cons
                end-if

           
           |78-ID-ef-data-bolla è l'ID del control ef-data-bolla
           when 78-ID-ef-data-bolla
                inquire ef-data-bolla, value in ef-data-bolla-buf
                if DatiBollaManuale and pgm-name = "gordc"
                   move ef-data-bolla-buf to como-data
                   perform DATE-FORMAT
                   move como-data         to ef-data-bolla-buf
                   display ef-data-bolla
                end-if

           |78-ID-ef-num-bolla è l'ID del control ef-num-bolla
           when 78-ID-ef-num-bolla
                inquire ef-num-bolla, value in ef-num-bolla-buf  
                if DatiBollaManuale and pgm-name = "gordc"
                   move ef-num-bolla-buf to tor-num-bolla
                   if tor-num-bolla = 0
                      display message box "Numero Bolla obbligatorio"
                                title tit-err
                                 icon mb-warning-icon
                      move 78-ID-ef-num-bolla to control-id
                      set errori to true
      *****             else
      *****                perform TROVA-BOLLA
      *****                if trovato
      *****                   display message box "Bolla già caricata!"
      *****                             title tit-err
      *****                              icon mb-warning-icon
      *****                   move 78-ID-ef-num-bolla to control-id
      *****                   set errori to true
      *****                end-if
                   end-if
                end-if

           |78-ID-cbo-stato è l'ID del control cbo-stato
           when 78-ID-cbo-stato
                inquire cbo-stato, value in cbo-stato-buf
                perform SCARICA-COMBO-STATO
                move stato   to  tor-stato
                if tor-stato not = old-tor-stato
                   move 1 to Passwd-password
                   call   "passwd" using Passwd-linkage
                   cancel "passwd"

                   if not Passwd-StatusOk
                      move old-tor-stato to stato
                      perform CARICA-COMBO-STATO
                      set errori to true
                   else
                      if disattivo
                         move control-id to store-id
                         move "clienti" to nome-file
                         perform CHANGE-STATUS
                         move store-id to control-id
                      end-if
                   end-if
                end-if

           when 78-ID-rb-man
                if key-status = 13
                   move 2 to screen1-ta-1-tab-value event-data-1      
                   perform CHANGE-TAB
                   if NonCambiareTab
                      set NonCambiareTab to false
                      move 1 to screen1-ta-1-tab-value event-data-1     
            
                   else
                      perform SCREEN1-TA-1-TABCHANGE
                      move 78-ID-ef-art    to control-id
                      move 4               to accept-control
      *****                set ArticoloSetFocus to false
                   end-if
                end-if

           when 78-ID-rb-gui
                if key-status = 13
                   move 2 to screen1-ta-1-tab-value event-data-1
                   perform CHANGE-TAB
                   if NonCambiareTab
                      set NonCambiareTab to false
                      move 1 to screen1-ta-1-tab-value event-data-1
                   else
                      perform SCREEN1-TA-1-TABCHANGE
                      move 78-ID-ef-art    to control-id
                      move 4               to accept-control
      *****                set ArticoloSetFocus to false
                   end-if
                end-if

           |78-ID-ef-art è l'ID del control ef-art
           when 78-ID-ef-art
                inquire ef-art, value in ef-art-buf
                move ef-art-buf to art-codice
                if art-codice not = 0
                   if art-codice        not = old-art-codice    or
                      prg-peso          not = hid-peso          or
                      prg-tipo-imballo  not = hid-tipo-imballo  or
                      prg-cod-magazzino not = hid-cod-magazzino
                      read articoli no lock
                           invalid  set errori to true
                                    initialize art-stato
                       not invalid
                           if art-attivo
                              if old-art-codice not = art-codice
                                 move 0 to hid-prz-commle
                              end-if
                              perform VALORIZZA-RIGA-ARTICOLO
                           else
                              set errori to true
                           end-if
                      end-read
                   else
                      if CambiatoMagazzino
                         |La prima volta che riconfermo la riga
                         |l'hidden viene aggiornato col valore nuovo.
                         |Testandolo con il codice in uso, se è uguale
                         |significa che è una riga già riconfermata.
                         if art-codice        not = old-art-codice    or
                            prg-peso          not = hid-peso          or
                            prg-tipo-imballo  not = hid-tipo-imballo  or
                            tca-cod-magaz     not = hid-cod-magazzino 
                            move tca-cod-magaz to prg-cod-magazzino
                            read progmag no lock
                                 invalid set errori to true
                             not invalid 
                                 if prg-attivo
                                    set CheckAfterZoom to true
                                    perform VALORIZZA-RIGA-ARTICOLO
                                 else
                                    set errori to true
                                 end-if
                            end-read
                         end-if
                      else
                         if CambiatoTrattamento and
                            pgm-name = "gordcvar"
                            perform CALCOLA-IMPOSTE-ORDINE
                            perform DISPLAY-SCREEN
                         end-if
                      end-if
                      move 0 to num-articoli
                      if cli-codice not = old-tor-cod-cli and
                         pgm-name = "gordcvar"
                         perform RECUPERA-IVA
                         if des-prog = 0
                            move cli-nazione to naz-codice
                         else
                            move des-nazione to naz-codice
                         end-if
                         read tnazioni no lock

                         if naz-imp-esenti-si
                            move 0 to ef-cons-buf ef-cou-buf 
                                      ef-add-buf
                                      col-cons col-cou col-add
                            display ef-cons ef-cou ef-add
                         else
                             if ttipocli-gdo
                                move 0 to ef-sconto-buf
                                display ef-sconto
                             end-if
                         end-if
                      end-if
                   end-if
         
                   if errori                                         
                      evaluate true
                      when art-bloccato
                           display message "Articolo BLOCCATO"
                                   title = tit-err
                                   icon mb-warning-icon
                      when art-disattivo
                           display message "Articolo SOSPESO"
                                   title = tit-err
                                   icon mb-warning-icon
                      when other
                           display message "Codice articolo NON valido"
                                   title = tit-err
                                   icon mb-warning-icon
                      end-evaluate
                      move 78-ID-ef-art to control-id
                      move art-codice   to SaveArticolo
                      perform PULISCI-CAMPI-LABELS
                      move SaveArticolo to ef-art-buf
                      display ef-art
                   else
                      if CheckAfterZoom
                         perform CANCELLA-COLORE
                      end-if
                      set FromSpostamento to false
                      move 78-ID-ef-qta to control-id
                      move 4 to accept-control
                   end-if
                   perform DISPLAY-SCREEN
                end-if

           |78-ID-ef-qta è l'ID del control ef-qta
           when 78-ID-ef-qta                      
                inquire ef-qta, value in ef-qta-buf

                move ef-qta-buf to como-qta
                if como-qta = 0
                   set errori to true        
                   display message "Inserimento quantità mancante"
                             title tit-err
                              icon 2
                else
BLISTR             if chk-blister-buf = 0
                      |nessun contgrollo sui pezzi/imballo se
                      |sono in bolla e non può essere modificata
                      if bollettata
                         continue
                      else
                         perform IMBALLI-QTA
                      end-if
BLISTR             else
                      if hid-bli-codice = zero
      *                   blister vecchio stile       
BLISTR                   if como-qta < hid-imballi
BLISTR                      set errori to true
BLISTR                      display message 
BLISTR                      "Quantità < colli Blister ("hid-imballi,")"
BLISTR                             title tit-err
BLISTR                              icon 2
BLISTR                   else
BLISTR                      move como-qta to col-qta
                      else
BLISTR                   move como-qta to col-qta

      *    Luciano
                         divide como-qta by hid-bli-qta 
                                         giving num-blist
                                         remainder rest-blist
                         if rest-blist not = zero
                            set errori to true
                            display message 
                                "Quantità non compatibile col Blister."
                                x"0D0A"
                                "Deve essere multiplo di " hid-bli-qta,
                                   title tit-err
                                   icon 2
                         end-if
      *    Luciano fine


BLISTR                end-if
BLISTR             end-if

      *             if tutto-ok
      *                perform AGGIORNA-NUM-COLLI
      *             end-if
                end-if


                if errori
                   move 78-ID-ef-qta     to control-id
                else
                   move 78-ID-ef-qta-oma to control-id
                end-if                            
                move 4 to accept-control

OMAGGI     |78-ID-ef-qta-oma è l'ID del control ef-qta-oma
OMAGGI     when 78-ID-ef-qta-oma
OMAGGI          inquire ef-qta-oma, value in ef-qta-oma-buf
OMAGGI
OMAGGI          move ef-qta-oma-buf to ror-qta-omaggi
OMAGGI          if ror-qta-omaggi not = 0
OMAGGI             inquire ef-qta value in como-qta
OMAGGI             evaluate true
OMAGGI             when ror-qta-omaggi > como-qta
OMAGGI                  set errori to true
OMAGGI                  display message
OMAGGI                          "Quantità omaggi > quantità totale"
OMAGGI                            title tit-err
OMAGGI                             icon 2
OMAGGI             when ror-qta-omaggi = como-qta
OMAGGI                  set errori to true
OMAGGI                  display message "Quantità omaggio incoerente"
OMAGGI                            title tit-err
OMAGGI                             icon 2
OMAGGI             when other
OMAGGI                  move ror-qta-omaggi to hid-qta-omaggi
OMAGGI             end-evaluate
OMAGGI          end-if
OMAGGI
OMAGGI          if errori
OMAGGI             move 78-ID-ef-qta-oma to control-id
OMAGGI          else
OMAGGI             move 78-ID-ef-uni     to control-id
OMAGGI          end-if
OMAGGI          move 4 to accept-control
                if OrdineTradizionale and 
                   pgm-name = "gordc" and 
                   tutto-ok
                   move 0 to col-uni  col-sconto col-imp
                             col-cons col-cou col-add
                   move spaces to col-iva
                   perform ENTRY-TO-ROW
                   perform PULISCI-CAMPI-LABELS
                end-if

           |78-ID-ef-uni è l'ID del control ef-uni
           when 78-ID-ef-uni
                inquire ef-uni, value in ef-uni-buf
                move ef-uni-buf to ror-prz-unitario col-uni
                if ror-prz-unitario = 0
                   perform PREZZO-ZERO
                else
                   if TotaleSiZero
                      display message "Impossibile attribuire un prezzo"
                                      " con la causale inserita"
                                title tit-err
                                 icon 2
                      set errori to true
                   else
LUBEXX                if ef-cod-iva-buf = tge-cod-iva-omag
LUBEXX                   perform RECUPERA-IVA
                         display ef-cod-iva
LUBEXX                end-if
LUBEXX             end-if

                   if tutto-ok
                      if ttipocli-gdo
                         if NoAssortimento
                            perform VALUTA-CAMBIO-PREZZO
                            move ef-uni-buf  to ror-prz-unitario
                            move ef-cons-buf to ror-imp-consumo
                            move ef-cou-buf  to ror-imp-cou-cobat
                            if tcl-si-piombo and art-si-cobat and 
                               naz-imp-esenti-no
                               perform CALCOLA-ADD-PIOMBO
                               display ef-add
                            end-if
                            if ror-prz-unitario   <=
                             ( ror-imp-consumo   +
                               ror-imp-cou-cobat +
                               ror-add-piombo )
                               perform DISPLAY-SCREEN
                               display message
                      "Prezzo errato: dev'essere maggiore delle imposte"
                                         title tit-err
                                          icon 2
                               set errori to true
                            else
                               perform CALCOLA-IMPONIBILE
                            end-if
                         end-if
                      else
                         if tcl-agente-si
                            move ef-age-buf to age-codice
                            if age-codice not = 0
                               move ef-cli-buf to como-prm-cliente
                               move ef-des-buf to como-prm-destino
                               perform RECUPERA-PRZ-LISTINO
                               if ror-prz-unitario < prezzo-listino
                                  move prezzo-listino to como-edit
                                  display message
                                  "Prezzo inserito < prezzo agente!!!"
                           x"0d0a""Prezzo listino: " como-edit
                           x"0d0a""Confermi?"
                                            title titolo
                                             type mb-yes-no
                                             icon 2
                                          default mb-no
                                           giving scelta
                                  if scelta= mb-no
                                     set errori to true
                                  end-if
                               end-if
                            end-if
                         end-if
                         if tutto-ok
                            perform VALUTA-CAMBIO-PREZZO
                            move ef-uni-buf  to rmo-netto
                            move ef-cons-buf to rmo-imp-cons
                            move ef-cou-buf  to rmo-coubat  

                            move ef-uni-buf  to ror-prz-unitario
                            move ef-cons-buf to ror-imp-consumo
                            move ef-cou-buf  to ror-imp-cou-cobat  

                            if tcl-si-piombo and art-si-cobat and 
                               naz-imp-esenti-no
                               perform CALCOLA-ADD-PIOMBO
                               display ef-add
                            end-if
                            move ef-add-buf  to ror-add-piombo
      *****                      if ror-prz-unitario   <=
      *****                       ( ror-imp-consumo   +
      *****                         ror-imp-cou-cobat +
      *****                         ror-add-piombo )
      *****                         perform DISPLAY-SCREEN
      *****                         display message
      *****                "Prezzo errato: dev'essere maggiore delle imposte"
      *****                                   title tit-err
      *****                                    icon 2
      *****                         set errori to true
      *****                      else
                               perform CALCOLA-IMPONIBILE
      *****                      end-if
                         end-if
                      end-if
      *****                if SaveGDO = spaces
      *****                   perform VALUTA-CAMBIO-PREZZO
      *****                else
      *****                   if NoAssortimento
      *****                      perform SCORPORO-IMPOSTE-IMPONIBILE
      *****                   ||||
      *****                   else
      *****                      perform VALUTA-CAMBIO-PREZZO
      *****                   ||||
      *****                   end-if
      *****                end-if
                   end-if
                end-if
                
                if errori
                   move 78-ID-ef-uni    to control-id
                else
                   move ef-uni-buf to old-prezzo
                   move 78-ID-ef-sconto to control-id
                   perform DISPLAY-SCREEN
                end-if                               
                move 4 to accept-control

           |78-ID-ef-sconto è l'ID del control ef-sconto
           when 78-ID-ef-sconto
                inquire ef-sconto, value in ef-sconto-buf
                move ef-sconto-buf to col-sconto ror-perce-sconto
                move ef-uni-buf    to ror-prz-unitario
      *****          if SaveGDO = spaces
                if ttipocli-gdo continue
                else
                   if ror-prz-unitario not = 0 |Articolo omaggio
                      compute ror-prz-unitario =
                              ror-prz-unitario -
                           (( ror-prz-unitario *
                              ror-perce-sconto ) / 100)
                      perform CALCOLA-IMPONIBILE
                      move ror-imponib-merce to ef-imp-buf col-imp
                   else
                      move 0 to ef-sconto-buf
                      display ef-sconto
                   end-if
                end-if

                display ef-imp
                if key-status = 13

                   if ror-prz-unitario = 0 |Articolo omaggio
                      move 0 to ef-cons-buf ef-cou-buf ef-add-buf
                                col-cons col-cou col-add
                      display ef-cons ef-cou
                   end-if
                            
                   set tutto-ok to true
                   if pgm-name = "gordcvar"
                      move col-qta     to como-qta
                      move old-col-qta to como-old-qta
                      if como-qta     <  como-old-qta and
                         prg-giacenza >= como-qta     and not NewRow
                         display message "Si sta tagliando "
                                         "merce con giacenza."
                                  x"0d0a""Confermare?"
                                   title titolo
                                    icon 2
                                    type mb-yes-no
                                  giving scelta
                                 default mb-no
                         if scelta = mb-no
                            set errori to true
                         end-if
                      end-if
                   end-if

                   if tutto-ok
                      perform ENTRY-TO-ROW
      *    se è un blister e ho cambiato la qta vado ad aggiornare la qta
      *    sulele righe collegate
                      if hid-blister = 1
                         if col-qta not = old-col-qta
                            perform AGGIORNA-QTA-CORRELATE
                         end-if
                      end-if

                      if VenditaAlDettaglio
                         perform CALCOLA-TOTALE-IVATO
                      end-if

      *****             move 0 to e-gui
      *****             modify rb-gui, enabled = e-gui
                      perform PULISCI-CAMPI-LABELS
                   end-if
                else
                   move 78-ID-ef-art to control-id
                   move 4            to accept-control
                end-if

           |78-ID-ef-cons è l'ID del control ef-cons
           when 78-ID-ef-cons
                inquire ef-cons, value in ef-cons-buf
                if errori continue
                else
                   move 78-ID-ef-sconto to control-id
                   move 4 to accept-control
                end-if

           |78-ID-ef-cou è l'ID del control ef-cou
           when 78-ID-ef-cou
                inquire ef-cou, value in ef-cou-buf
                if errori continue
                else
                   move 78-ID-ef-sconto to control-id
                   move 4 to accept-control
                end-if

           |78-ID-ef-imp è l'ID del control ef-imp
           when 78-ID-ef-imp
                inquire ef-imp, value in ef-imp-buf
                if errori continue
                else
                   move 78-ID-ef-sconto to control-id
                   move 4 to accept-control
                end-if

           end-evaluate.
      *
           if errori
              perform CANCELLA-COLORE
              move CONTROL-ID to store-id
              move 4          to ACCEPT-CONTROL
           |SPECIFICO PER GESTIONE ORDINI
           else    
              evaluate true        
              when num-articoli = 1
                   perform UN-SOLO-ARTICOLO-SU-PROGMAG
                   move art-codice to old-art-codice
              when num-articoli > 1                
                   move 78-ID-ef-art  to control-id
                   perform ZOOM-SU-PROGMAG-ARTICOLO
                   if stato-zoom = 0
                      move art-codice to old-art-codice
                   end-if
              end-evaluate
           end-if.
           
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
       MOVE-DESCR-IVA.
           initialize lab-iva-buf.
           inspect tbliv-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tbliv-descrizione1 delimited by low-value
                   " "                delimited by size
                   tbliv-descrizione2 delimited by size
                   into lab-iva-buf
           end-string.

      ***---
       MOVE-DESCR-IVA-2.
           initialize lab-iva-2-buf.
           inspect tbliv-descrizione1 replacing trailing 
                                         spaces by low-value.
           string  tbliv-descrizione1 delimited by low-value
                   " "                delimited by size
                   tbliv-descrizione2 delimited by size
                   into lab-iva-2-buf
           end-string.

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
       INIT-OLD-REC.
           initialize |old-ror-rec  
                      old-tor-rec replacing numeric data by zeroes
                                       alphanumeric data by spaces.

           set attivo       to true

           move stato       to old-tor-stato.

           move "N" to old-tor-spostam-ric-ago.
           move "N" to old-tor-spostam-ric-dic.

           move data-oggi to tor-data-ordine tor-data-passaggio-ordine.

      ***---
       PULISCI-CAMPI-LABELS.
           initialize HiddenKey hid-saldo.

           move zero   to ef-art-buf  ef-cons-buf   ef-add-buf
                          ef-cou-buf  ef-sconto-buf ef-qta-oma-buf 
                          ef-qta-buf  ef-uni-buf    ef-imp-buf 
                          chk-blister-buf hid-blister
                          hid-flag-bloccato hid-promo hid-prz-commle
                          hid-bli-codice
                          hid-bli-qta
                          hid-bli-perce.
           set hid-no-prz-manuale  to true.
           move spaces to lab-art-buf lab-iva-2-buf ef-cod-iva-buf.

           display ef-art  ef-cons   ef-qta-oma ef-add
                   ef-cou  ef-sconto
                   ef-qta  ef-cod-iva
                   ef-uni  ef-imp
                   lab-art lab-iva-2 chk-blister.
                   
           move 0 to old-art-codice old-prezzo save-ordinato.

           move 0 to hid-giacenza hid-ordinato hid-impegnato 
                     lab-ivato-buf.
           perform LABEL-VALORI.
           display lab-giacenza lab-impegnato lab-ordinato
                   lab-gia      lab-imp       lab-ord      lab-ivato.

           move zero   to num-colli-riga-old
                          hid-imballi-old.

      ***---
       SELEZIONA-ALFA.
           set tutto-ok to true.  
           move spaces to lab-cli-buf.
           move spaces to lab-ind-cli-buf.
           move spaces to lab-loc-cli-buf.
      * LA SECONDA CHIAVE E' ALFANUMERICA E LA VALUE VARIABLE CONTIENE
      * ANCORA GLI ZERI DAVANTI CHE ELIMINO
           inspect ef-cli-buf replacing leading ZERO by SPACES.
           move    ef-cli-buf to cli-ragsoc-1.
      
      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           set cli-tipo-c to true.
           call "C$JUSTIFY" using cli-ragsoc-1, "L".
      
           start clienti key >= cli-k1
                 invalid continue
             not invalid read clienti next 
           end-start.
      

           move "clienti-alfa"    to como-file.

           call "zoom-gt"  using   como-file, cli-rec
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = 0
              if cli-disattivo or cli-bloccato
                 if not tmp
                    if cli-fuori-fido and cli-fido-extra not = 0
                       continue
                    else
                       if cli-fuori-fido
                          initialize calfido-linkage 
                                      sitfin-linkage
                                     replacing numeric data by zeroes
                                          alphanumeric data by spaces
                          move ef-cli-buf   to link-cli-codice
                          call "C$JUSTIFY"  using link-cli-codice, "R"
                          inspect link-cli-codice 
                                  replacing leading x"20" by x"30"
                          call   "sitfin"  using sitfin-linkage
                                                calfido-linkage
                          cancel "sitfin"
                          display message "Cliente NON attivo"
                                    title tit-err
                                     icon 2
                          |31/05/2012
                          if pgm-name = "gordc"
                             set errori to true
                          end-if
                          |31/05/2012
                          move spaces to lab-cli-buf
                          move spaces to lab-ind-cli-buf
                          move spaces to lab-loc-cli-buf
                       end-if
                    end-if
                 end-if   
              end-if
              if tutto-ok
                 if pgm-name = "gordcvar"
                    perform VALUTA-CAMBIO-TIPOLOGIA
                 end-if
                 move cli-codice    to codice-ed
                 move codice-ed     to ef-cli-buf   
                 call "C$JUSTIFY" using ef-cli-buf, "L"
                 display ef-cli   
                 move cli-ragsoc-1  to lab-cli-buf
                 move cli-indirizzo to lab-ind-cli-buf
                 move cli-localita  to lab-loc-cli-buf
              end-if
           else
              set errori         to true
              move 78-ID-ef-cli  to CONTROL-ID
           end-if.

      ***---
       SELEZIONA-NUMERICO.
           set tutto-ok to true.
           move spaces to lab-cli-buf.
           move spaces to lab-ind-cli-buf.
           move spaces to lab-loc-cli-buf.

           inquire ef-cli, value in cli-codice.
           if cli-codice not > 0
              set errori to true
              move 78-ID-ef-cli to CONTROL-ID
              display message box msg-codice-obbligatorio

                      title tit-err
                      icon  MB-WARNING-ICON
           else          
              set cli-tipo-c to true
              read clienti no lock
                   invalid
                   display message box "Codice cliente NON valido"
                           title = tit-err
                           icon MB-WARNING-ICON
                   set errori to true
                   move 78-ID-ef-cli to CONTROL-ID
               not invalid
                   if cli-disattivo or cli-bloccato
                      if not tmp
                         if cli-fuori-fido and cli-fido-extra not = 0
                            continue
                         else
                            if cli-fuori-fido
                               initialize calfido-linkage 
                                           sitfin-linkage
                                       replacing numeric data by zeroes
                                            alphanumeric data by spaces
                               move ef-cli-buf   to link-cli-codice
                               call "C$JUSTIFY"  using 
                                                 link-cli-codice, "R"
                               inspect link-cli-codice 
                                       replacing leading x"20" by x"30"
                               call   "sitfin"  using sitfin-linkage
                                                     calfido-linkage
                               cancel "sitfin"
                               display message "Cliente NON attivo"
                                         title tit-err
                                          icon 2
                              |31/05/2012
                               if pgm-name = "gordc"
                                  set errori to true
                               end-if
                              |31/05/2012
                               move spaces to lab-cli-buf
                               move spaces to lab-ind-cli-buf
                               move spaces to lab-loc-cli-buf
                            end-if
                         end-if
                      end-if
                   end-if
                   if tutto-ok
                      if OrdineTradizionale
                         move cli-tipo to tcl-codice
                         read ttipocli no lock invalid continue end-read
                         if tcl-gdo-si or tcl-gdo-opz
      *****                   if cli-gdo not = spaces
                            set errori to true
                            display message "Impossibile tipologia GDO"
                                      title tit-err
                                       icon 2
                         end-if
      *****                else
      *****                   if cli-gdo = spaces
      *****                      set errori to true
      *****                      display message 
      *****                      "Impossibile tipologia non GDO"
      *****                                title tit-err
      *****                                 icon 2
      *****                   end-if
                      end-if
                   end-if
                   if tutto-ok
                      if pgm-name = "gordcvar"
                         perform VALUTA-CAMBIO-TIPOLOGIA
                      end-if
                      move cli-ragsoc-1  to lab-cli-buf
                      move cli-indirizzo to lab-ind-cli-buf
                      move cli-localita  to lab-loc-cli-buf
                   end-if
                   if tutto-ok
                      move cli-ragsoc-1  to lab-cli-buf
                      move cli-indirizzo to lab-ind-cli-buf
                      move cli-localita  to lab-loc-cli-buf
                   end-if
              end-read
           end-if .

      ***---
       SELEZIONA-DESTINO-NUMERICO.
           move spaces to lab-des-buf.
           move spaces to lab-ind-des-buf.
           move spaces to lab-loc-des-buf.
           inquire ef-cli, value in des-codice.
           inquire ef-des, value in des-prog.
           if des-codice not > 0
              set errori to true
              move 78-ID-ef-des to control-id
              display message box msg-codice-obbligatorio
                      title tit-err
                      icon  mb-warning-icon
           else          
              inquire ef-cli, value in des-codice
              read destini no lock
                   invalid
                   display message "Progressivo destino NON valido"
                             title tit-err
                             icon mb-warning-icon
                   set errori to true
                   move 78-ID-ef-des to control-id
               not invalid
                   if des-disattivo or des-bloccato
                      if not tmp
                         set errori to true
                         display message "Destino NON attivo"
                                   title tit-err
                                    icon 2
                         move spaces to lab-des-buf
                         move spaces to lab-ind-des-buf
                         move spaces to lab-loc-des-buf
                      end-if
                   end-if
                   if tutto-ok
                      move des-ragsoc-1  to lab-des-buf
                      move des-indirizzo to lab-ind-des-buf
                      move des-localita  to lab-loc-des-buf
                   end-if
              end-read
           end-if.

      ***---


       SELEZIONA-DESTINO-ALFA.
      * LA SECONDA CHIAVE E' ALFANUMERICA E LA VALUE VARIABLE CONTIENE
      * ANCORA GLI ZERI DAVANTI CHE ELIMINO
           move    ef-cli-buf to des-codice.
           inspect ef-des-buf replacing leading ZERO by SPACES.
           move    ef-des-buf to des-ragsoc-1.
      
      * POI GIUSTIFICO A SINISTRA E MI POSIZIONO SULLO ZOOM PER CHIAVE
      * ALTERNATA
           call "C$JUSTIFY" using des-ragsoc-1, "L".
      
           start destini     key >= K1
                 invalid     continue
                 not invalid read destini next 
           end-start.   
           set cli-tipo-c to true.
      
           move "clienti-des-alf-A" to como-file.
           call "zoom-gt"  using  como-file, des-rec
                          giving  stato-zoom.
           cancel "zoom-gt".
           if stato-zoom = 0
              if des-disattivo or des-bloccato
                 if not tmp
                    set errori to true
                    display message "Destino NON attivo"
                              title tit-err
                               icon 2
                    move spaces to lab-des-buf
                    move spaces to lab-ind-des-buf
                    move spaces to lab-loc-des-buf
                 end-if
              end-if
              if tutto-ok
                 move des-prog       to codice-ed
                 move codice-ed      to ef-des-buf   
                 call "C$JUSTIFY" using ef-des-buf, "L"
                 display ef-des   
                 move des-ragsoc-1  to lab-des-buf
                 move des-indirizzo to lab-ind-des-buf
                 move des-localita  to lab-loc-des-buf
              end-if
           else
              set errori         to true
              move 78-ID-ef-des  to CONTROL-ID
           end-if.


      ***---
       TROVA-DESTINO.
           set trovato to false.
           inquire ef-cli, value in ef-cli-buf.
           move ef-cli-buf to des-codice convert.
           move ef-cli-buf to cli-codice convert.

           move low-value  to des-prog.
           start destini  key is >= des-chiave
                 invalid  continue
             not invalid  read destini next
                 if des-codice = cli-codice 
                    set trovato to true 
                 else 
                    move ef-cli-buf to des-codice convert
                    move ef-des-buf to des-prog   convert
                 end-if
           end-start.

      ***---
       CHECK-PAGE-1.
           set tutto-ok to true.
           perform  varying control-id from 78-ID-ef-num-ord by 1
                      until control-id    > 78-ID-ef-data-cons
              perform CONTROLLO
              if errori 
                 exit perform 
              end-if
           end-perform.
           if tutto-ok
              perform  varying control-id from 78-ID-ef-data-bolla by 1
                         until control-id    > 78-ID-ef-num-bolla
                 perform CONTROLLO
                 if errori 
                    exit perform 
                 end-if
              end-perform
           end-if.



      ***---
       VALUTA-CAMBIO-TIPOLOGIA.
           if e-gui = 1
              display message "IMPOSSIBILE" |assortimento
           else
              move cli-tipo to tcl-codice
              read ttipocli no lock
                   invalid  move spaces to tcl-tipologia-tratt-imposte
              end-read
              if tcl-tipologia-tratt-imposte not = TrattamentoInUso
LUBEXX           display message "ATTENZIONE!!"
LUBEXX                    x"0d0a""E' stato cambiata la tipologia "
LUBEXX                           "trattamento imposte del cliente."
LUBEXX                    x"0d0a""Annullare e ricreare l'ordine"
LUBEXX                           " col nuovo Cliente!!!"
LUBEXX                     title tit-err
LUBEXX                      icon 2
LUBEXX           set errori to true
LUBEXX*****
LUBEXX*****                 move tcl-tipologia-tratt-imposte to TrattamentoInUso
LUBEXX*****
LUBEXX*****                 if ttipocli-gdo modify ef-sconto,     read-only
LUBEXX*****                 else            modify ef-sconto, not read-only
LUBEXX*****                 end-if

LUBEXX*****                 display message "ATTENZIONE!!"
LUBEXX*****                          x"0d0a""E' stato cambiata la "
LUBEXX*****                                 "tipologia di cliente."
LUBEXX*****                          x"0d0a""Occorre ripassare TUTTE le "
LUBEXX*****                                 "righe per prezzi e imposte"
LUBEXX*****                           title tit-err
LUBEXX*****                            icon 2
LUBEXX*****
LUBEXX*****                 move tcl-tipologia-tratt-imposte to TrattamentoInUso
LUBEXX*****
LUBEXX*****                 if ttipocli-gdo modify ef-sconto,     read-only
LUBEXX*****                 else            modify ef-sconto, not read-only
LUBEXX*****                 end-if
LUBEXX*****
LUBEXX*****                 set CambiatoTrattamento to true

              end-if
           end-if.

      ***---
       SECCA-TMP-ASSORCLI.
           if link-path-tmp-assorcli not = spaces
              close tmp-assorcli
              call "C$DELETE" using link-path-tmp-assorcli, "I"
              move spaces to link-path-tmp-assorcli
           end-if.

      ***---
       STATUS-BAR-MSG.
           if pgm-name = "gordc"
              modify form1-st-1-handle, 
                     panel-index  3,
                     panel-text  "INSERIMENTO"
           else
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
              end-evaluate
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
           modify tool-cerca, bitmap-number = BitmapNumZoom.

      ***---
       ORDINI-BEFORE-PROGRAM.
           set tmp                 to false.
           set CambiatoTrattamento to false.
           set CambiatoMagazzino   to false.
           set StoSalvando         to false.

LUBEXX     initialize sw-controlla-scostamento.
LUBEXX     accept sw-controlla-scostamento 
LUBEXX            from environment "CONTROLLO_SCOSTAMENTO".

           initialize FlagAssortimento.
           accept FlagAssortimento from environment "ASSORTIMENTO"
                  on exception
                     set NoAssortimento to true
              not on exception
                     if FlagAssortimento = spaces
                        set NoAssortimento to true
                     end-if
           end-accept.
           if SiAssortimento move 0 to e-gui
           else              move 1 to e-gui
           end-if.
           move 0       to v-bolla v-dett.
           set tutto-ok to true.
           open input tparamge.
           move spaces to tge-chiave.
           read tparamge no lock invalid continue end-read.
           move tge-anno to como-anno.
           close tparamge.

           open input tcaumag.
           move tge-causale-ordini-std to tca-codice.
           read  tcaumag no lock invalid set errori to true end-read.

           if tutto-ok
              move tca-cod-magaz to mag-codice
              open input tmagaz
              read tmagaz no lock invalid set errori to true end-read
              close tmagaz
              if errori
                 display message box 
                          "Magazzino per Bolla ordinaria mancante!!!"
                  x"0d0da""Impossibile procedere con il programma."
                         title = tit-err
                         icon MB-ERROR-ICON
              end-if
           else
              display message box 
                       "Causale ordinaria di magazzino mancante!!!"
               x"0d0da""Impossibile procedere con il programma."
                      title = tit-err
                      icon MB-ERROR-ICON
           end-if.

           close tcaumag.

           if tutto-ok
              open input timposte
              move 0        to imp-data
              read timposte no lock invalid set errori to true end-read
           
              if errori
                 display message box 
                          "Record imposte mancante!!!"
                  x"0d0da""Impossibile procedere con il programma."
                         title = tit-err
                         icon MB-ERROR-ICON
              end-if
              close timposte
           end-if.

           copy resource "conferma.bmp".
           if errori
              perform ORDINI-AFTER-PROGRAM
              SET LK-BL-CANCELLAZIONE TO TRUE 
              MOVE COMO-PROG-ID       TO LK-BL-PROG-ID
              CALL "BLOCKPGM"  USING LK-BLOCKPGM
              goback 
           else
              call "W$BITMAP" using wbitmap-load "conferma.bmp"
                             giving conferma-bmp
           end-if.

           accept como-art-no-colli from environment "ART_NO_COLLI".
           unstring como-art-no-colli delimited by ";"
                    into art-no-colli(1)
                         art-no-colli(2)
                         art-no-colli(3)
                         art-no-colli(4)
                         art-no-colli(5)
                         art-no-colli(6)
                         art-no-colli(7)
                         art-no-colli(8)
                         art-no-colli(9)
                         art-no-colli(10)
                         art-no-colli(11)
                         art-no-colli(12)
                         art-no-colli(13)
                         art-no-colli(14)
                         art-no-colli(15)
                         art-no-colli(16)
                         art-no-colli(17)
                         art-no-colli(18)
                         art-no-colli(19)
                         art-no-colli(20).

      ***---
       ORDINI-AFTER-PROGRAM.
           call "W$BITMAP" using wbitmap-destroy, conferma-bmp.
           perform SECCA-TMP-ASSORCLI.

           if HoSalvato
              call   "tprev-elab" using user-codi
              cancel "tprev-elab"
           end-if.

      ***---
       ORDINI-AFTER-END-ACCEPT.
           if NonCambiareTab
              set NonCambiareTab to false
              move 1 to screen1-ta-1-tab-value event-data-1
              perform SCREEN1-TA-1-TABCHANGE
              move store-id to control-id
              move 4 to accept-control
              set ControllaCampi to true
           end-if.

      *****     if ArticoloSetFocus
      *****        inquire screen1-ta-1, value in screen1-ta-1-tab-value
      *****        if screen1-ta-1-tab-value = 2
      *****           move 78-ID-ef-art    to control-id
      *****           move 4               to accept-control
      *****        end-if
      *****        if not FromSpostamento
      *****           set ControllaCampi   to true
      *****        end-if
      *****        set ArticoloSetFocus to false
      *****        initialize key-status
      *****     end-if.

      ***---
       SETTA-RIGA.
           move save-riga to event-data-2 riga.
           move 4         to event-data-1 colonna.

           if tor-manuale
              modify form1-gd-1, cursor-y = riga
              perform SPOSTAMENTO
           end-if.

      ***---
       CERCA.
           evaluate control-id
           when 78-ID-ef-cau
                if not bollettata and tor-da-ordine-no
                   perform RIEMPI-TMP-CAUSALI
                   move "zoom-tcaumag"   to como-file
                   inquire ef-cau, value in zoom-tca-codice
                   call "zoom-gt"  using como-file, zoom-tca-rec                                
                                  giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      move zoom-tca-codice      to ef-cau-buf
                      move zoom-tca-descrizione to lab-cau-buf
                      display ef-cau lab-cau
                   end-if
                   close       zoom-tcaumag
                   delete file zoom-tcaumag
                end-if

           when 78-ID-ef-cli
                if not bollettata and tor-da-ordine-no
                   set cli-tipo-c  to true
                   inquire ef-cli, value in cli-codice
                   if OrdineTradizionale
                      move "clienti-no-gdo"  to como-file
                      call "zoom-gt"      using como-file, cli-rec
                                         giving stato-zoom
                      end-call
                      cancel "zoom-gt"
                   else
                      move "clienti" to como-file
                      move spaces to cli-gdo
                      call "zoom-gt"  using como-file, cli-rec
                                     giving stato-zoom
                      end-call
                      cancel "zoom-gt"
                   end-if

                   if stato-zoom = 0
                      move cli-codice     to ef-cli-buf
                     inspect ef-cli-buf replacing leading x"30" by x"20"
                      call "C$JUSTIFY" using ef-cli-buf, "L"
                      move cli-ragsoc-1   to lab-cli-buf

                      move cli-indirizzo  to lab-ind-cli-buf
                      move cli-localita   to lab-loc-cli-buf
                      display ef-cli lab-cli lab-ind-cli lab-loc-cli
                   end-if
                end-if

           when 78-ID-ef-des
                if not bollettata and tor-da-ordine-no
                   inquire ef-cli, value in des-codice
                   set cli-tipo-c to true
                   move "clienti-des-A"    to como-file
                   inquire ef-des, value in des-prog
                   call "zoom-gt"  using como-file, des-rec
                                   giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      move des-prog     to ef-des-buf
                     inspect ef-des-buf replacing leading x"30" by x"20"
                      call "C$JUSTIFY" using ef-des-buf, "L"
                      move des-ragsoc-1   to lab-des-buf
                      move des-indirizzo  to lab-ind-des-buf
                      move des-localita   to lab-loc-des-buf
                      display ef-des lab-des lab-ind-des lab-loc-des
                   end-if
                end-if

           when 78-ID-ef-vet
                if not bollettata and tor-da-ordine-no
                   move "tvettori"  to como-file
                   inquire ef-vet, value in vet-codice
                   call "zoom-gt"   using como-file, vet-rec
                                   giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      move vet-codice      to ef-vet-buf
                      move vet-descrizione to lab-vet-buf
                      display ef-vet lab-vet
                   end-if
                end-if
   
           when 78-ID-ef-forn
                set cli-tipo-F      to true
                move "clienti-CF"   to como-file
                inquire ef-forn, value cli-codice
                call "zoom-gt"   using como-file, cli-rec
                                giving stato-zoom
                end-call
                cancel "zoom-gt"
                if stato-zoom = 0
                   move cli-codice   to ef-forn-buf
                   move cli-ragsoc-1 to lab-forn-buf
                   display ef-forn lab-forn
                end-if
   
           when 78-ID-ef-age
                if tor-da-ordine-no
                   move "agenti"      to como-file
                   inquire ef-age, value age-codice
                   call "zoom-gt"  using como-file, age-rec
                                  giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      move age-codice   to ef-age-buf
                      move age-ragsoc-1 to lab-age-buf
                      display ef-age lab-age
                   end-if
                end-if

           when 78-ID-ef-iva
                if tor-da-ordine-no
                   move "tivaese-ese" to como-file
                   move "IV"          to tbliv-codice1
                   inquire ef-iva,  value in tbliv-codice2

                   call "zoom-gt"   using como-file, record-tbliv
                                   giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      if tbliv-codice2 not = SaveIva
                         move tbliv-codice2   to ef-iva-buf
                         perform MOVE-DESCR-IVA
                         display ef-iva lab-iva
                     end-if
                   end-if
                end-if

           when 78-ID-ef-pag
      *****          if tor-da-ordine-no
                   move "tcodpag"   to como-file
                   move "PA"        to tblpa-codice1
                   inquire ef-pag,  value in tblpa-codice2
                   call "zoom-gt"   using como-file, record-tblpa
                                   giving stato-zoom
                   end-call
                   cancel "zoom-gt"
                   if stato-zoom = 0
                      if tblpa-codice2 not = SavePag
                         move tblpa-codice2   to ef-pag-buf
                         perform MOVE-DESCR-PAG
                         display ef-pag lab-pag
                      end-if
                   end-if
      *****          end-if

           when 78-ID-ef-art
      *****          if SaveGdo = spaces
      *****             perform ZOOM-SU-PROGMAG
      *****          else
                   if bollettata or tor-da-ordine-si
                      continue
                   else
                      if NoAssortimento  
                         perform ZOOM-SU-PROGMAG
                      else
                         perform ZOOM-SU-TMP-ASSORCLI
                      end-if
                   end-if
      *****          end-if
                move 4 to ACCEPT-CONTROL

           when 78-ID-ef-uni
LABLAB          if tcl-si-recupero       and 
                   volantino-forzato = 0 and
                   tca-prezzo-reso-no
LABLAB             set promo-future to true
LABLAB             perform CERCA-PROMO-LISTINO
                   if trovato
                      if not prezzo-sp
                         move rpr-codice         to ror-promo
                         move rpr-codice         to hid-promo
                      end-if
                      set PrezzoCambiato      to true
                      move rpr-prz-acq        to ef-uni-buf hid-prezzo
                                                 ror-prz-unitario
                                                 ror-prz-commle
                      perform CALCOLA-IMPONIBILE
                      perform DISPLAY-SCREEN
                   end-if
LABLAB          end-if

           end-evaluate.

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
                    perform TROVA-LISTINO
                    if no-prg-listino
                       perform TROVA-CLI-PRG
                    end-if
                    if si-prg-listino
                       perform FIND-PROGMAG-LISTINO
                       perform UN-SOLO-ARTICOLO-SU-PROGMAG
                    else
                       perform POSITION-ON-MAJOR-GIACENZA
LUBEXX*****              perform POSITION-ON-FIRST-RECORD
                       move path-tmp-progmag-zoom to ext-file
                       move "tmp-progmag-zoom-o"  to como-file
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
                          move 0 to key-status
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
       ZOOM-SU-PROGMAG-ARTICOLO.
           set filtro-articoli to true.
           perform COMPONI-TMP.
           if trovato
              perform POSITION-ON-MAJOR-GIACENZA
              move path-tmp-progmag-zoom to ext-file
              move "tmp-progmag-zoom-o"  to como-file
              call "zoom-gt"          using como-file, tmp-prg-z-rec
                                     giving stato-zoom
              end-call
              cancel "zoom-gt"
           else
              move 1 to stato-zoom
           end-if.
           delete file tmp-progmag-zoom.
           if stato-zoom = 0
              move tmp-prg-z-chiave to prg-chiave
              read progmag no lock invalid continue end-read
              move prg-cod-articolo to ef-art-buf
              display ef-art
              set CheckAfterZoom to true
              perform CONTROLLO
              set CheckAfterZoom to false
           else
              set errori to true
              move 4     to accept-control
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
                  "tmp-progmag-zoom-o"   delimited by size
                  "_"                    delimited by size
                  como-data              delimited by size
                  "_"                    delimited by size
                  como-ora               delimited by size
                  into path-tmp-progmag-zoom
           end-string.

           set trovato to false.
           open output tmp-progmag-zoom.
           if status-tmp-progmag-zoom = "00"
              initialize prg-rec
              if filtro-articoli
                 move SaveArticolo to prg-cod-articolo
                 read progmag no lock invalid continue end-read
                 move prg-ordinato-1 to save-ordinato
              end-if
              move tca-cod-magaz to prg-cod-magazzino
              start progmag key  is >= prg-chiave
                    invalid continue 
                not invalid perform CICLO-READ
              end-start

              close tmp-progmag-zoom
              call "W$MOUSE" using set-mouse-shape, arrow-pointer
           end-if.

      ***---
       CICLO-READ.
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
                 prg-cod-magazzino     = tca-cod-magaz

                 if prg-attivo
                    move prg-cod-articolo  to art-codice
                    read articoli no lock invalid continue end-read
                    if art-attivo
                       set record-ok to true 
                    end-if
                 
                    if record-ok
                 
                       move prg-tipo-imballo to imb-codice
                                                imq-codice
                       read timballi  no lock invalid continue end-read
                       read timbalqta no lock invalid continue end-read
                 
                       move imq-qta-imb      to hid-imballi imballi-ed
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
                 
                       move art-codice      to tmp-prg-z-cod-articolo
                       move art-descrizione to tmp-prg-z-art-des
                       move tca-cod-magaz   to tmp-prg-z-cod-magazzino
                       move StoreDesMagazzino  to tmp-prg-z-mag-des
                       move prg-tipo-imballo   to tmp-prg-z-tipo-imballo
                       move imballo-descrizione to tmp-prg-z-imb-des
                       move prg-peso            to tmp-prg-z-peso
                       move prg-giacenza        to tmp-prg-z-giacenza
                       move prg-impegnato       to tmp-prg-z-impegnato
                       move save-ordinato       to tmp-prg-z-ordinato
                 
                       write tmp-prg-z-rec invalid continue end-write
                       set trovato to true
                       add 1 to num-articoli
                       if num-articoli = 1
                          move prg-chiave to GiacenzaKey
                       end-if
                    end-if
                 end-if
              end-if

           end-perform.

      ***---
       POSITION-ON-MAJOR-GIACENZA.
           open input tmp-progmag-zoom.

           move low-value to tmp-prg-z-rec.
           inquire ef-art,   value in tmp-prg-z-cod-articolo
           move tmp-prg-z-cod-articolo  to como-articolo.
           move tca-cod-magaz           to tmp-prg-z-cod-magazzino
                                           como-magazzino.
           move 0      to giacenza-maggiore giacenza-fisica.
           move spaces to como-record.
           start tmp-progmag-zoom key is >=  key-des
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tmp-progmag-zoom next 
                         at end exit perform 
                    end-read
                    if tmp-prg-z-cod-articolo  not = como-articolo or
                       tmp-prg-z-cod-magazzino not = como-magazzino
                       exit perform
                    end-if

                    compute giacenza-fisica = 
                            tmp-prg-z-giacenza - 
                            tmp-prg-z-impegnato
                          
                    if giacenza-fisica > giacenza-maggiore or 
                       como-record = spaces
                       move giacenza-fisica to giacenza-maggiore
                       move tmp-prg-z-rec   to como-record
                    end-if
                 end-perform
           end-start.
           if como-record not = spaces
              move como-record to tmp-prg-z-rec
           end-if.
           close tmp-progmag-zoom.

      ***---
       POSITION-ON-FIRST-RECORD.
           |Mi posiziono sul PRIMO record
           open input tmp-progmag-zoom.
           move low-value to tmp-prg-z-rec.
           start tmp-progmag-zoom key is >= key-des
                 invalid continue
           end-start.
           read tmp-progmag-zoom next end-read.
           close tmp-progmag-zoom.

      ***---
       ZOOM-SU-TMP-ASSORCLI.
           if path-tmp-assorcli not = spaces
              move SaveGdo    to asc-cod-gruppo-gdo
              move ef-cli-buf to asc-cod-cliente    with convert
              inquire ef-art, value in asc-cod-articolo
              inquire ef-des, value in ef-des-buf
              move path-tmp-assorcli to ext-file
              move "tmp-assorcli" to como-file
              call "zoom-gt"   using como-file, tmp-asc-rec
                              giving stato-zoom
              end-call
              cancel "zoom-gt"
              if stato-zoom = 0
                 move tmp-asc-cod-articolo      to ef-art-buf
                 display ef-art
              end-if
           end-if.

      ***---
       VALORIZZA-ARRAY-CAUSALI.
           set link-update-um      to true.
           set link-update-peso    to false.
           set link-update-valore  to false.
           move "0000000000000000" to link-array.
           move link-causale       to tca-codice.
           read tcaumag no lock invalid continue end-read.
           |In caso sia permessa la stampa della bolla
           |agisco sull'impegnato, altrimenti l'ordine
           |è da considerarsi già bollettato ed agisco
           |direttamente sulla giacenza e non sull'impegnato
           if tca-si-stampa
              move 1 to multiplyer(2)
           else
              move 1 to multiplyer(1)
              move 1 to multiplyer(15)
           end-if.

      ***---
       TESTATA-DETTAGLIO.
           move tge-cliente-corrisp to cli-codice.
           set cli-tipo-C to true.
           read clienti no lock
                invalid
                   set errori to true
                   display message "Vendita al dettaglio impossibile:"
                            x"0d0a""Cliente per corrispettivi assente"
                           title = tit-err
                           icon mb-warning-icon
                   move 78-ID-ef-cli to control-id
            not invalid
                if pgm-name = "gordc"
                   move cli-codice     to ef-cli-buf
                   inspect ef-cli-buf replacing leading x"30" by x"20"
                   call "C$JUSTIFY" using ef-cli-buf, "L"
                   move cli-ragsoc-1   to lab-cli-buf
                   move cli-indirizzo  to lab-ind-cli-buf
                   move cli-localita   to lab-loc-cli-buf
                   move spaces         to ef-des-buf
                   move 0              to ef-data-pass-buf
                   accept como-data  from century-date
                   perform DATE-TO-SCREEN
                   move como-data to ef-data-buf
                   display ef-data
                   move cli-vettore  to des-vettore
                   move cli-utf      to des-deposito-utf
                   move cli-prov     to des-prov
                   perform MOVE-DATI
                   move 0 to mod-k
                   move 1 to mod mod-campi e-man  |e-gui
LUBEXX             perform ABILITA-GEST-PLUS
                   perform DISPLAY-SCREEN
                   modify ef-age, read-only
                   modify ef-pag, read-only
                   modify ef-iva, read-only
                   modify ef-vet, read-only
                   move 2 to event-data-1 screen1-ta-1-tab-value
                   perform CHANGE-TAB
                   perform SCREEN1-TA-1-TABCHANGE
                   move 78-ID-ef-art    to control-id
                   move 4               to accept-control
      *****             set ArticoloSetFocus to false
                end-if
           end-read.

      ***---
       CALCOLA-TOTALE-IVATO.
           inquire form1-gd-1, last-row in tot-righe.
           move 0 to como-tot-ivato como-imposta SavePrezzo.
           perform varying riga from 2 by 1 
                     until riga > tot-righe
              inquire form1-gd-1(riga, 4), 
                      cell-data in ror-qta
              inquire form1-gd-1(riga, 7), 
                      cell-data in ror-imp-consumo
              inquire form1-gd-1(riga, 8), 
                      cell-data in ror-imp-cou-cobat
              inquire form1-gd-1(riga, 9), 
                      cell-data in ror-add-piombo
              inquire form1-gd-1(riga, 10), 
                      cell-data in ror-imponib-merce
              if pgm-name = "gordc"
                 inquire form1-gd-1(riga, 1),
                         hidden-data in gruppo-hidden
              else
                 inquire form1-gd-1(riga, 1),
                         hidden-data in gruppo-hidden
              end-if
              compute SavePrezzo = ror-imponib-merce +
                                   ror-imp-cou-cobat +
                                   ror-add-piombo    +
                                   ror-imp-consumo
      *****        compute SavePrezzo = SavePrezzo * ror-qta
OMAGGI        compute SavePrezzo = 
OMAGGI                SavePrezzo * ( ror-qta - ror-qta-omaggi)
              compute como-iva =
                      SavePrezzo * hid-perce-iva / 100
              add 0,005           to como-iva
              move como-iva       to como-iva-2dec
              compute como-tot-ivato =
                      como-tot-ivato +
                      SavePrezzo     +
                      como-iva-2dec
           end-perform.
           move como-tot-ivato to lab-tot-ivato-buf.
           display lab-tot-ivato.
           move 0 to lab-ivato-buf.
           display lab-ivato.

      ***---
       CONTROLLA-TOTALE-MAN.
LUBEXX*****Se è attiva la causale di movimento "speciale" non
LUBEXX*****devo fare nessun controllo sul totale del documento
LUBEXX     if tca-si-speciale exit paragraph end-if.
           move cli-tipo to tcl-codice.
           read ttipocli no lock invalid continue end-read.
           if ttipocli-gdo set TrattamentoGDO to true
           else            set TrattamentoGDO to false
           end-if.
           move 0 to Sum.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 10), 
                      cell-data in como-valore   
              inquire form1-gd-1(store-riga, 9), 
                      cell-data in como-pb
              inquire form1-gd-1(store-riga, 8), 
                      cell-data in como-coubat
              inquire form1-gd-1(store-riga, 7), 
                      cell-data in como-cons
              compute SavePrezzo = como-valore + 
                                   como-pb     + 
                                   como-coubat + 
                                   como-cons     
              if tcl-fido-nuovo-si    
                 inquire form1-gd-1(store-riga, 11), 
                         cell-data in ror-cod-iva
                 move "IV"        to tbliv-codice1
                 move ror-cod-iva to tbliv-codice2
                 read tivaese 
                 compute mult = 1 + tbliv-percentuale / 100
                 compute SavePrezzo =
                         SavePrezzo * mult
              end-if
              inquire form1-gd-1(store-riga, 4), cell-data in como-qta
              if SavePrezzo > 999999
                 inquire form1-gd-1(store-riga, 1),
                         hidden-data = gruppo-hidden
                 move HiddenKey to prg-chiave
                 read progmag no lock
                 perform CALCOLA-COSTO-MP-COMPLETO
                 |Altrimenti righe solo 999999,99 senza costo mp
                 |darebbero come risultato un master prezzo ZERO
                 if costo-mp = 0
                    compute Sum = Sum + 0,01
                 else
                    compute Sum = Sum + ( costo-mp * como-qta )
                 end-if
              else
                 compute Sum = Sum + ( SavePrezzo * como-qta )
              end-if
           end-perform.
           if Sum = 0 if TotaleNoZero set errori to true end-if
           else       if TotaleSiZero set errori to true end-if
           end-if.
           if errori
              display message 
               "Salvataggio NON effettuato: Il totale del documento "
           x"0d0a""non rispetta l'indicazione della causale utilizzata "
                        title tit-err
                         icon 2
           end-if.

      ********---
      ***** CONTROLLA-FUORI-FIDO.
LUBEXX*****     if tutto-ok
LUBEXX*****        initialize calfido-linkage 
LUBEXX*****                   replacing numeric data by zeroes
LUBEXX*****                        alphanumeric data by spaces
LUBEXX*****        move cli-codice to link-cli-codice
LUBEXX*****        call   "calfido" using calfido-linkage
LUBEXX*****        cancel "calfido"
LUBEXX*****        compute scoperto = saldo + Sum     +
LUBEXX*****                           effetti-rischio + ordini-in-essere
      *****        if cli-gestione-fido-si
      *****           move cli-piva to sf-piva
      *****           read sitfin no lock 
      *****                invalid move 0 to sf-fido-max
      *****           end-read
      *****           compute tot-fido = sf-lince
      *****        else
      *****           compute tot-fido = cli-fido + 
      *****                              cli-pfa  + 
      *****                              cli-fidejussione
      *****        end-if
LUBEXX*****        if scoperto > tot-fido
      *****           if Sum <= cli-fido-extra
LUBEXX*****              close    clienti
LUBEXX*****              open i-o clienti
LUBEXX*****              read clienti no lock invalid continue end-read
LUBEXX*****              subtract Sum from cli-fido-extra
LUBEXX*****              rewrite cli-rec invalid continue end-rewrite
LUBEXX*****              close clienti
LUBEXX*****              open input clienti
LUBEXX*****              read clienti no lock invalid continue end-read
      *****           else
      *****              compute como-numero = scoperto - tot-fido
      *****              if como-numero > cli-fido-extra
LUBEXX*****                 set errori to true
      *****                 subtract cli-fido-extra from como-numero
LUBEXX*****                 move como-numero to como-edit
LUBEXX*****                 display message
LUBEXX*****                         "ATTENZIONE!!!!"
LUBEXX*****                  x"0d0a""CLIENTE FUORI FIDO DI:  " como-edit
LUBEXX*****                  x"0d0a""IMPOSSIBILE REGISTRARE L'ORDINE!"
LUBEXX*****                           title tit-err
LUBEXX*****                            icon 2
LUBEXX*****              else
LUBEXX*****                 close    clienti
LUBEXX*****                 open i-o clienti
LUBEXX*****                 read clienti no lock invalid continue end-read
LUBEXX*****                 subtract como-numero from cli-fido-extra
LUBEXX*****                 rewrite cli-rec invalid continue end-rewrite
LUBEXX*****                 close clienti
LUBEXX*****                 open input clienti
LUBEXX*****                 read clienti no lock invalid continue end-read
      *****              end-if
LUBEXX*****           end-if
LUBEXX*****        end-if
LUBEXX*****     end-if. 

      ***---
       CONTA-CODICI-IVA-MAN.
           inquire form1-gd-1, last-row in tot-righe.
           move 0 to num-codici-iva.
           move 1 to idx.
           move spaces to cod-iva(1).
           move spaces to cod-iva(2).
           move spaces to cod-iva(3).
           perform varying riga from 2 by 1
                     until riga > tot-righe
              set trovato to false
              if pgm-name = "gordcvar"           
                 inquire form1-gd-1(riga, 12),
                         cell-data in col-iva    
              else
                 inquire form1-gd-1(riga, 11),
                         cell-data in col-iva    
              end-if
              perform varying idx from 1 by 1
                        until idx > 3
                 if col-iva = cod-iva(idx)
                    set trovato to true
                    exit perform
                 end-if
              end-perform
              if not trovato
                 add 1 to num-codici-iva
                 perform varying idx from 1 by 1
                           until idx > 3
                    if cod-iva(idx) = spaces
                       move col-iva to cod-iva(idx)
                       exit perform
                    end-if
                 end-perform
              end-if
              if num-codici-iva > 3 
                 set errori to true
                 exit perform 
              end-if
           end-perform.

      ***---
       CONTROLLA-RIGHE.
           |Viene richiamato per controllare che TUTTE le righe
           |abbiano il nuovo magazzino (in caso la NUOVA causale) ne
           |apporti il cambiamento che di conseguenza influisce sui
           |progressivi di magazzino ossia sulle righe (MODIFICA)
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 1),
                      hidden-data = gruppo-hidden
              if hid-cod-magazzino not = tca-cod-magaz
                 move store-riga to save-riga
                 subtract 1 from store-riga
                 move store-riga to riga-ed
                 perform SETTA-RIGA
                 display message "Cambio di magazzino!"
                          X"0d0a""Ricaricare la riga: " riga-ed, "."
                           title tit-err
                            icon 2
                 set errori to true
                 exit perform
              end-if
           end-perform.

      ***---
       RIEMPI-TMP-CAUSALI.
           accept como-data from century-date.
           accept como-ora  from time.
           initialize path-zoom-tcaumag.
           accept  path-zoom-tcaumag from environment "PATH_ST".
           inspect path-zoom-tcaumag replacing trailing spaces 
                                                     by low-value.
           string  path-zoom-tcaumag delimited low-value
                   "zoom-tcaumag"    delimited size
                   "_"               delimited size
                   como-data         delimited size
                   "_"               delimited size
                   como-ora          delimited size
                   ".tmp"            delimited size
                   into path-zoom-tcaumag
           end-string.
           open  output zoom-tcaumag.
           move  low-value to tca-rec.
           start tcaumag key is >= tca-chiave
                 invalid continue
             not invalid
                 open input tmagaz
                 perform until 1 = 2
                    read tcaumag next no lock
                         at end exit perform
                    end-read
                    if tca-si-stampa and tca-cliente or 
                       tca-codice = "AEXD"
                       initialize zoom-tca-rec
                       move tca-codice      to zoom-tca-codice
                       move tca-descrizione to zoom-tca-descrizione
                       move tca-cod-magaz   to zoom-tca-cod-magaz 
                       move tca-cod-magaz   to mag-codice
                       read tmagaz no lock
                            invalid continue
                        not invalid 
                            move mag-descrizione to zoom-mag-descrizione
                       end-read
                       write zoom-tca-rec invalid continue end-write
                    end-if
                 end-perform
           end-start.
           close zoom-tcaumag.
           open input zoom-tcaumag.
           move path-zoom-tcaumag to ext-file.

      ***---
       CONTROLLA-PERCENTUALE-IVA.
           move "IV"        to tbliv-codice1.
           move ror-cod-iva to tbliv-codice2.
           read tivaese no lock 
                invalid continue
            not invalid
                if tbliv-percentuale not = 0
                   set EsisteIVA to true
                end-if
           end-read.

LABLAB***---
       PB-FORZA-PRESSED.
           set trovato to false.
           move low-value to tpr-rec.
           move cli-gdo     to tpr-gdo.
           move ef-data-buf to como-data.
           perform DATE-TO-FILE.
           add 1 to como-data.
           move como-data   to tpr-ini-dpo.
           start tpromo key >= tpr-chiave-ricerca
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read tpromo next at end  exit perform end-read
                    if tpr-gdo not = cli-gdo exit perform end-if

                    if not trovato
                       perform APRI-TMP-PROMO-PRZ
                       set trovato to true
                    end-if
                    move tpr-codice      to tprz-codice
                    move cli-gdo         to tprz-gdo
                    move tpr-descrizione to tprz-descr
                    move tpr-ini-dpo     to tprz-ini-dpo
                    move tpr-fine-dpo    to tprz-fine-dpo
                    write tprz-rec invalid continue end-write
                 end-perform
                 if tor-prg-destino not = 0
                    move low-value       to loc-rec
                    move cli-gdo         to loc-gdo
                    move cli-codice      to loc-cliente
                    move tor-prg-destino to loc-destino
                    move como-data       to loc-ini-dpo
                    start locali key >= loc-chiave-ricerca
                          invalid continue
                      not invalid
                          perform until 1 = 2
                             read locali next 
                                  at end exit perform 
                             end-read
                             if loc-gdo     not = cli-gdo    or
                                loc-cliente not = cli-codice or
                                loc-destino not = tor-prg-destino
                                exit perform
                             end-if
                             move loc-codice to tpr-codice
                             if not trovato
                                perform APRI-TMP-PROMO-PRZ
                                set trovato to true
                             end-if
                             move loc-codice      to tprz-codice
                             move cli-gdo         to tprz-gdo
                             move tpr-descrizione to tprz-descr
                             move loc-ini-dpo     to tprz-ini-dpo
                             move loc-fine-dpo    to tprz-fine-dpo
                             write tprz-rec invalid continue end-write
                          end-perform
                    end-start      
                 end-if

                 if trovato
                    close tmp-promo-prz
                    move path-tmp-promo-prz to ext-file
                    move "tmp-promo-prz2"   to como-file
                    call "zoom-gt"       using como-file, 
                                               tprz-rec
                                        giving stato-zoom
                    end-call
                    cancel "zoom-gt"
                    if stato-zoom = 0
                       move tprz-codice to volantino-forzato
                       move tprz-descr  to lab-forzato-buf
                       display lab-forzato
                    end-if
                    delete file tmp-promo-prz
                 end-if
           end-start.

      ***---
BLISTR CHK-BLISTER-PRESSED.
           inquire chk-blister, value in chk-blister-buf.
           if mod-cliente-destino = 1 or bollettata
              if  chk-blister-buf = 1
                 move 0 to chk-blister-buf
              else
                 move 1 to chk-blister-buf
              end-if
              display chk-blister
           else
              if chk-blister-buf = 1
                 set  link-blister   to true
                 move "BLISTER"      to link-des
                 move 0              to link-qta
                 move spaces         to link-udm
                 call   "imballo" using imballo-linkage
                 cancel "imballo"
                 if link-imballo-saved = 1
                    move link-qta       to imballi-ed hid-imballi
                    perform DESCRIZIONE-IMBALLO
                    move 78-ID-ef-qta to control-id
                 else
                    move 0 to chk-blister-buf
                    display chk-blister
                    move 78-ID-chk-blister to control-id
                 end-if
              else
                 if ror-qta not = 0
                    move hid-tipo-imballo to imq-codice
                    read timbalqta no lock 
                         invalid 
                         display message "Imballo non congruo"
                                   title tit-err
                                    icon 3
                     not invalid
                         move imq-tipo to imb-codice
                         read timballi no lock 
                              invalid 
                              display message "Imballo non congruo"
                                        title tit-err
                                         icon 3
                          not invalid
                              move imb-descrizione to link-des 
                                                      hid-des-imballo
                              move imq-qta-imb     to imballi-ed 
                                                      hid-imballi
                              perform DESCRIZIONE-IMBALLO
                              move 78-ID-ef-qta to control-id
                         end-read
                    end-read
                 end-if
              end-if
              perform CANCELLA-COLORE
              move 4                to accept-control
              set  FromSpostamento  to false
              set  ArticoloSetFocus to false
              set  ControllaCampi   to true
           end-if.

LABLAB***---
       CERCA-PROMO-LISTINO.
           set trovato      to false.
           |Inserimento ordini
           move ef-data-buf to como-data.
           perform DATE-TO-FILE.
           move como-data to tor-data-ordine.

           initialize path-tmp-promo-prz.
           move 0 to num-promo.

           |Provo con la promo locale
           if tor-prg-destino not = 0
              if promo-future
                 move cli-gdo          to loc-gdo
                 move cli-codice       to loc-cliente
                 move tor-prg-destino  to loc-destino
                 move tor-data-ordine  to loc-ini-dpo
                 move low-value        to loc-fine-dpo
                 start locali key >= loc-chiave-ricerca
                       invalid set record-ok to false
                   not invalid set record-ok to true
                 end-start
              else
                 move cli-gdo          to loc-gdo
                 move cli-codice       to loc-cliente
                 move tor-prg-destino  to loc-destino
                 move tor-data-ordine  to loc-fine-dpo
                 move low-value        to loc-ini-dpo
                 start locali key >= loc-chiave-gdo-fine
                       invalid set record-ok to false
                   not invalid set record-ok to true
                 end-start
              end-if
              if record-ok
                 perform until 1 = 2
                    read locali next at end exit perform end-read
                    if loc-gdo     not = cli-gdo    or
                       loc-cliente not = cli-codice or
                       loc-destino not = tor-prg-destino
                       exit perform
                    end-if

                    if loc-fine-dpo >= tor-data-ordine and
                       loc-ini-dpo  <= tor-data-ordine or
                       promo-future

                       move loc-codice to tpr-codice
                       read tpromo no lock
                            invalid continue
                        not invalid
                            if tpr-gdo      = loc-gdo     and
                               tpr-ini-dpo  = loc-ini-dpo and
                               tpr-fine-dpo = loc-fine-dpo
                               move tpr-codice to rpr-codice
                               move art-codice to rpr-articolo
                               read rpromo no lock 
                                    invalid continue
                                not invalid
                                    if path-tmp-promo-prz = spaces
                                       perform APRI-TMP-PROMO-PRZ
                                    end-if
                                    set trovato to true
                                    add 1 to num-promo
                                    move tpr-codice to tprz-codice
                                    move cli-gdo    to tprz-gdo
                                    move tpr-descrizione  
                                      to tprz-descr
                                    move tpr-ini-dpo      
                                      to tprz-ini-dpo
                                    move tpr-fine-dpo     
                                      to tprz-fine-dpo
                                    move rpr-prz-acq      
                                      to tprz-prz-acq
                                    write tprz-rec 
                                          invalid continue 
                                    end-write
                               end-read
                            end-if
                       end-read
                    end-if
                 end-perform
              end-if
           end-if.

           |PROMO GDO
           if not trovato
              if promo-future
                 move cli-gdo         to tpr-gdo
                 move tor-data-ordine to tpr-ini-dpo
                 move low-value       to tpr-fine-dpo
                 start tpromo key >= tpr-chiave-ricerca
                       invalid set record-ok to false
                   not invalid set record-ok to true
                 end-start
              else
                 move cli-gdo         to tpr-gdo
                 move tor-data-ordine to tpr-fine-dpo
                 move low-value       to tpr-ini-dpo
                 start tpromo key >= tpr-chiave-gdo-fine
                       invalid set record-ok to false
                   not invalid set record-ok to true
                 end-start
              end-if

              if record-ok
                 perform until 1 = 2
                    read tpromo next at end exit perform end-read
                    if tpr-gdo not = cli-gdo
                       exit perform
                    end-if

                    if tpr-fine-dpo >= tor-data-ordine and
                       tpr-ini-dpo  <= tor-data-ordine or promo-future

                       if tpr-nazionale
                          move tpr-codice to rpr-codice
                          move art-codice to rpr-articolo
                          read rpromo no lock 
                               invalid continue
                           not invalid
                               set trovato to true
                               if path-tmp-promo-prz = spaces
                                  perform APRI-TMP-PROMO-PRZ
                               end-if
                               add 1 to num-promo
                               move tpr-codice       to tprz-codice
                               move cli-gdo          to tprz-gdo
                               move tpr-descrizione  to tprz-descr
                               move tpr-ini-dpo      to tprz-ini-dpo
                               move tpr-fine-dpo     to tprz-fine-dpo
                               move rpr-prz-acq      to tprz-prz-acq
                               write tprz-rec invalid continue end-write
                          end-read
                       end-if
                    end-if
                 end-perform
              end-if

           end-if.

           if not trovato 
              if not promo-future
                 move 0 to rpr-codice
                 move cli-gdo          to lst-gdo
                 move tor-data-ordine  to lst-data
                 move art-codice       to lst-articolo
                 start listini key <= lst-k-gdo-articolo
                       invalid continue
                   not invalid
                       read listini previous
                       if lst-gdo      = cli-gdo          and
                          lst-data    <= tor-data-ordine  and
                          lst-articolo = art-codice
                          |In caso di "FA" non cerco promo 
                          |né successive né precedenti
                          if lst-prezzo >= 999999,99
                             set hid-bloccato to true
                             set trovato      to false
                          else
                             if lst-prezzo not = 0
                                set trovato to true
                                move lst-prezzo   to rpr-prz-acq
                                set  hid-bloccato to false
                             else
                                set prezzo-sp to false
      *****                          set  hid-bloccato to true (levato in data 12/05/09)
                                |CASO "SP"
                                |1. Cerco la promo immediatamente dopo
                                perform CERCA-PROMO-DOPO
                                if not trovato
                                   |2. Cerco la promo immediatamente prima
                                   perform CERCA-PROMO-PRIMA
                                end-if
                             end-if
                          end-if
                       end-if
                 end-start
              end-if
           else
              if num-promo > 1 or promo-future
                 move path-tmp-promo-prz to ext-file
                 move "tmp-promo-prz"    to como-file
                 call "zoom-gt"       using como-file, 
                                            tprz-rec
                                     giving stato-zoom
                 end-call
                 cancel "zoom-gt"
                 if stato-zoom = 0
                    move tprz-codice  to rpr-codice
                    move tprz-prz-acq to rpr-prz-acq
                    set trovato to true
                 else
                    set trovato to false
                 end-if
              else
                 move tprz-codice  to rpr-codice
                 move tprz-prz-acq to rpr-prz-acq
              end-if
              close       tmp-promo-prz
              delete file tmp-promo-prz
           end-if.

LABLAB***---
      * Usato quando è già stato scelto il volantino dalla funzione
      * apposita di forzatura. A questo punto si sa già qual è
       FORZA-PREZZO-VOLANTINO.
           |Inserimento ordini
           move ef-data-buf to como-data.
           perform DATE-TO-FILE.
           move como-data to tor-data-ordine.

           set promo-future to false.
           move volantino-forzato to rpr-codice.
           move art-codice        to rpr-articolo.
           read rpromo no lock
                invalid set trovato to false
            not invalid set trovato to true
           end-read.

           if not trovato 
              move 0 to rpr-codice
              move cli-gdo          to lst-gdo
              move tor-data-ordine  to lst-data
              move art-codice       to lst-articolo
              start listini key <= lst-k-gdo-articolo
                    invalid continue
                not invalid
                    read listini previous
                    if lst-gdo      = cli-gdo          and
                       lst-data    <= tor-data-ordine  and
                       lst-articolo = art-codice
                       if lst-prezzo not = 0
                          set trovato to true
                          move lst-prezzo  to rpr-prz-acq
                          set hid-bloccato to false
                       end-if
                    end-if
              end-start
           end-if.

LABLAB***---
       APRI-TMP-PROMO-PRZ.
           accept  como-data from century-date.
           accept  como-ora  from time.
           accept  path-tmp-promo-prz from environment "PATH_ST".
           inspect path-tmp-promo-prz replacing trailing 
                                      spaces by low-value.
           string  path-tmp-promo-prz delimited low-value
                   "TMP-PROMO-PRZ"    delimited size
                   "_"                delimited size
                   como-data          delimited size
                   "_"                delimited size


                   como-ora           delimited size
                   ".tmp"             delimited size
                   into path-tmp-promo-prz
           end-string.
           open output tmp-promo-prz.

      ***---
       CERCA-PROMO-DOPO.
           if tor-prg-destino not = 0
              move cli-gdo         to loc-gdo
              move cli-codice      to loc-cliente
              move tor-prg-destino to loc-destino
              move tor-data-ordine to loc-ini-dpo
              move low-value       to loc-fine-dpo
              start locali key >= loc-chiave-ricerca
                    invalid set record-ok to false
                not invalid set record-ok to true
              end-start

              if record-ok
                 perform until 1 = 2
                    read locali next at end exit perform end-read
                    if loc-gdo      not =  cli-gdo         or
                       loc-cliente  not =  cli-codice      or
                       loc-destino  not =  tor-prg-destino
                       exit perform
                    end-if

                    if loc-ini-dpo  >= tor-data-ordine and
                       loc-fine-dpo >= tor-data-ordine
                       move loc-codice to tpr-codice
                       read tpromo no lock
                            invalid continue
                        not invalid
                            if tpr-gdo      = loc-gdo     and
                               tpr-ini-dpo  = loc-ini-dpo and
                               tpr-fine-dpo = loc-fine-dpo
                               move tpr-codice to rpr-codice
                               move art-codice to rpr-articolo
                               read rpromo no lock 
                                    invalid continue
                                not invalid set trovato to true
                                            exit perform
                               end-read
                            end-if
                       end-read
                    end-if

                 end-perform
              end-if
           end-if.

           if not trovato
              move cli-gdo         to tpr-gdo
              move tor-data-ordine to tpr-ini-dpo
              move low-value       to tpr-fine-dpo
              start tpromo key >= tpr-chiave-ricerca
                    invalid set record-ok to false
                not invalid set record-ok to true
              end-start

              if record-ok
                 perform until 1 = 2
                    read tpromo next at end exit perform end-read
                    if tpr-gdo not = cli-gdo
                       exit perform
                    end-if


                    if tpr-fine-dpo > tor-data-ordine and
                       tpr-ini-dpo  > tor-data-ordine
                       move tpr-codice to rpr-codice
                       move art-codice to rpr-articolo
                       read rpromo no lock 
                            invalid continue
                        not invalid set trovato to true
                                    exit perform
                       end-read
                    end-if
                 end-perform
              end-if
           end-if.                   

      ***---
       CERCA-PROMO-PRIMA.
           if tor-prg-destino not = 0
              move cli-gdo         to loc-gdo
              move cli-codice      to loc-cliente
              move tor-prg-destino to loc-destino
              move tor-data-ordine to loc-fine-dpo
              move low-value       to loc-ini-dpo
              start locali key <= loc-chiave-gdo-fine
                    invalid set record-ok to false
                not invalid set record-ok to true
              end-start

              if record-ok
                 perform until 1 = 2
                    read locali previous at end exit perform end-read
                    if loc-gdo     not =  cli-gdo      or
                       loc-cliente not =  cli-codice   or
                       loc-destino not =  tor-prg-destino
                       exit perform
                    end-if

                    if loc-ini-dpo  >= tor-data-ordine and
                       loc-fine-dpo >= tor-data-ordine
                       move loc-codice to tpr-codice
                       read tpromo no lock
                            invalid continue
                        not invalid
                            if tpr-gdo      = loc-gdo     and
                               tpr-ini-dpo  = loc-ini-dpo and
                               tpr-fine-dpo = loc-fine-dpo
                               move tpr-codice to rpr-codice
                               move art-codice to rpr-articolo
                               read rpromo no lock 
                                    invalid continue
                                not invalid set trovato to true
                                            exit perform
                               end-read
                            end-if
                       end-read
                    end-if
                 end-perform
              end-if
           end-if.

           if not trovato
              move cli-gdo         to tpr-gdo
              move tor-data-ordine to tpr-fine-dpo
              move low-value       to tpr-ini-dpo
              start tpromo key <= tpr-chiave-gdo-fine
                    invalid set record-ok to false
                not invalid set record-ok to true
              end-start

              if record-ok
                 perform until 1 = 2
                    read tpromo previous at end exit perform end-read
                    if tpr-gdo not = cli-gdo
                       exit perform
                    end-if
                    if tpr-fine-dpo < tor-data-ordine and
                       tpr-ini-dpo  < tor-data-ordine
                       move tpr-codice to rpr-codice

                       move art-codice to rpr-articolo
                       read rpromo no lock 
                            invalid continue
                        not invalid set trovato to true
                                    exit perform
                       end-read
                    end-if
                 end-perform
              end-if

           end-if.

      ***---
LABLAB PB-BLISTER-PRESSED.
           if mod-cliente-destino = 1 exit paragraph end-if.
           move tca-cod-magaz  to ins-mag.
           move 0              to ins-qta.
           move 0              to ins-codice.
           move cli-gdo        to ins-gdo.
           move cli-codice     to ins-cliente.
           move des-prog       to ins-destino.
           move 0              to TotPrzBlister Sum.

           call   "ins-blister"  using ins-linkage.
           cancel "ins-blister".

           if ins-codice not = 0
              move ins-codice to bli-codice
              read blister no lock invalid continue end-read
              move 0 to idx 
              perform PULISCI-CAMPI-LABELS
              perform CANCELLA-COLORE 
              set NewRow          to true
              set ControllaCampi  to true
              set PromoGiaCercato to false
              perform varying idx from 1 by 1 
                        until idx > 50
                 if bli-el-articolo(idx) = 0
                    exit perform
                 end-if

                    set tutto-ok to true
                    set trovato  to false
                    move bli-el-articolo(idx) to art-codice 
                    perform TROVA-LISTINO
                    if no-prg-listino
                       perform TROVA-CLI-PRG
                    end-if
                    if tutto-ok
                       if si-prg-listino
                          perform FIND-PROGMAG-LISTINO
                          set trovato to true
                       else
                          perform VALORIZZA-MAGGIOR-GIACENZA
                       end-if
                    end-if
                 if trovato
                    move bli-el-articolo(idx) to art-codice col-art
                                                 ef-art-buf
      **    Luciano
                    move bli-codice        to hid-bli-codice
                    move bli-el-qta(idx)   to hid-bli-qta
                    move bli-el-perce(idx) to hid-bli-perce
      **    Luciano fine

                    perform SOMMA-DINAMICI
                    move GiacenzaKey to prg-chiave HiddenKey
                    read progmag  no lock invalid continue end-read
                    move 0 to hid-prz-commle
                    display ef-art
                    read articoli no lock invalid continue end-read
                    if idx = 1
                       move "BLISTER" to link-des
                       move ins-qta   to link-qta
                    else
                       move "BLISTER" to link-des
                       move 0         to link-qta
                    end-if
                    set prezzo-sp to false
                    move link-qta to hid-old-qta imballi-ed
      *    Luciano
                    compute ef-qta-buf = ins-qta * bli-el-qta(idx)
      *    Luciano fine

                    move 1 to link-imballo-saved
                    move 1 to chk-blister-buf
                    perform DESCRIZIONE-IMBALLO
                    move 0 to hid-qta-omaggi ef-qta-oma-buf

                    if not PromoGiaCercato
                       move 0 to rpr-prz-acq
                       move art-codice  to SaveArticolo
                       move bli-codice  to art-codice
                       set promo-future to false
                       if volantino-forzato = 0
                          perform CERCA-PROMO-LISTINO
                          move SaveArticolo to art-codice
                          set PromoGiaCercato to true
                          set prezzo-sp to true
                       else
                          perform FORZA-PREZZO-VOLANTINO
                          set prezzo-sp to false
                       end-if
                       if trovato set trovato-promo to true
                       else       set trovato-promo to false
                       end-if
                    end-if
      
                    if bli-el-perce(idx) not = 0 and
                       rpr-prz-acq       not = 0

                       if not trovato-promo
                          |Non ho trovato promo né prima né dopo,
                          |e il listino esiste ma con "SP"
                          if bli-prezzo not = 0
                             if idx = 1 |la prima volta
                                move bli-prezzo to TotPrzBlister
                             end-if

      *    Luciano
      *                       compute como-numero =
      *                               bli-prezzo * 
      *                               bli-el-perce(idx)/ 100
                             compute como-numero =
                                     bli-prezzo * 
                                     bli-el-perce(idx)/ 100 / 
                                     bli-el-qta(idx) 
      *    Luciano fine              
                             perform ARROTONDA-PRZ-BLISTER

                          else
                             move 9999999,99 to como-prezzo
                          end-if
                          move 0                  to ror-prz-commle
                          move como-prezzo        to ef-uni-buf 
                                                     ror-prz-unitario
                          move como-prezzo        to hid-prezzo
                          set  hid-bloccato       to true
                          move 0                  to hid-promo
                       else
                          if idx = 1 |la prima volta
                             move rpr-prz-acq to TotPrzBlister
                          end-if

      *    Luciano              
      *                    compute como-numero =
      *                            rpr-prz-acq * 
      *                            bli-el-perce(idx) / 100
                          compute como-numero =
                                  rpr-prz-acq * 
                                  bli-el-perce(idx) / 100 / 
                                  bli-el-qta(idx) 
      *    Luciano fine              

                          perform ARROTONDA-PRZ-BLISTER

                          if not prezzo-sp
                             move rpr-codice         to ror-promo
                             move rpr-codice         to hid-promo
                          end-if
                          move como-prezzo        to ef-uni-buf 
                                                     hid-prezzo
                                                     ror-prz-unitario
                                                     ror-prz-commle
                          set  hid-bloccato       to false
                       end-if
                       if ef-iva-buf = spaces
                          move art-codice-iva to col-iva
                       else
                          move ef-iva-buf to col-iva
                       end-if
                       perform IMPOSTE
                       move como-prezzo  to ror-prz-unitario hid-prezzo
                                            col-uni
                       move ror-imp-consumo to col-cons
                       if imposta-cobat not = 0
                          move imposta-cobat to ror-imp-cou-cobat 
                                                col-cou
                       else
                          move imposta-cou   to ror-imp-cou-cobat 
                                                col-cou
                       end-if
                       if add-piombo not = 0
                          move add-piombo to ror-add-piombo col-add
                       end-if
                       perform CALCOLA-IMPONIBILE
                       move ror-imponib-merce to col-imp

                       compute como-iva = como-prezzo * 
                                          hid-perce-iva / 100
                    else 
              
                          move 0                  to como-prezzo
                                                     ror-prz-commle
                                                     ef-uni-buf 
                                                     ror-prz-unitario
                                                     hid-prezzo
                                                     col-uni
                                                     ror-imponib-merce 
                                                     col-imp
                          set  hid-bloccato       to false
              
                          move iva-omaggio to col-iva
                          move 0 to ror-imp-consumo 
                                    col-cons
                                    ror-imp-cou-cobat 
                                    col-cou
                                    ror-add-piombo
                                    col-add
              
                          move 0 to como-iva
              
                          if trovato-promo
                             if not prezzo-sp
                                move rpr-codice to hid-promo
                             end-if
                          else
                             move 0          to hid-promo
                             move 9999999,99 to como-prezzo
                                                ror-prz-commle
                                                ef-uni-buf 
                                                ror-prz-unitario
                                                hid-prezzo
                                                col-uni
                                                ror-imponib-merce 
                                                col-imp
                             if ef-iva-buf = spaces
                                move art-codice-iva to col-iva
                             else
                                move ef-iva-buf to col-iva
                             end-if
                          end-if
                    end-if

                    perform READ-TMARCHE

                    move prg-peso-utf     to hid-utf
                    move prg-peso-non-utf to hid-non-utf

                    if hid-prezzo = 0 move "S" to hid-omaggio
                    else              move "N" to hid-omaggio
                    end-if

                    perform ENTRY-TO-ROW
                 end-if
              end-perform

              add ins-qta   to num-colli
              move num-colli to lab-colli-buf
              modify lab-colli title lab-colli-buf


              perform PULISCI-CAMPI-LABELS
           end-if.          

LABLAB***---
       VALORIZZA-MAGGIOR-GIACENZA.
           |Valorizzo prg-chiave con il record avente > giacenza
           move 0 to giacenza.
           set  trovato              to false.
           move low-value            to prg-chiave.
           move bli-el-articolo(idx) to prg-cod-articolo.
           move bli-magazzino        to prg-cod-magazzino.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read progmag next at end exit perform end-read
                    if prg-cod-articolo  not = bli-el-articolo(idx) or
                       prg-cod-magazzino not = bli-magazzino
                       exit perform
                    end-if
                    if prg-attivo
                       if not trovato
                          set trovato to true
                          |Se la prima ed unica volta ho una giacenza di -1

                          move prg-giacenza to giacenza
                          move prg-giacenza to giacenza
                          move prg-chiave to GiacenzaKey
                       end-if
                       if prg-giacenza > giacenza
                          move prg-giacenza to giacenza
                          move prg-chiave to GiacenzaKey
                       end-if
                    end-if
                 end-perform
           end-start.

      ***---
       CONTROLLA-SE-PESO-SUP-24000-KG.
           accept controlla-24000 from environment "CONTROLLA_24000".
           if controlla-24000 not = "S"
              exit paragraph
           end-if.
LUBEXX*****Blocco il salvataggio di un ordine con peso > 24000 Kg
           set tutto-ok to true.
           if cli-si-blocco
              move 0 to tot-peso
              inquire form1-gd-1, last-row in tot-righe
              perform varying store-riga from 2 by 1 
                        until store-riga > tot-righe
      
                 inquire form1-gd-1(store-riga, 1),
                         hidden-data in gruppo-hidden
                      
                 move hid-peso to como-peso
      
                 inquire form1-gd-1(store-riga, 4), 
                         cell-data in como-qta
                 compute tot-peso = 
                       ( tot-peso + ( como-qta * como-peso) )
                 if tot-peso > 24000 
                    display message "Salvataggio impossibile!!!"
                             x"0d0a""Peso superiore a 24.000 Kg."
                              title tit-err
                               icon 2
                    set errori to true
                    exit perform
                 end-if
              end-perform
           end-if.

LUBEXX***---
       VALORIZZA-PROGRESSIVO-CORRETTO.
           move low-value        to prg-chiave.
           move ror-cod-articolo to prg-cod-articolo.
           move tca-cod-magaz    to prg-cod-magazzino.
           start progmag key >= prg-chiave
                 invalid continue
             not invalid
                 read progmag next no lock at end continue end-read
                 if prg-cod-articolo  = ror-cod-articolo and
                    prg-cod-magazzino = tca-cod-magaz
                    move prg-chiave       to HiddenKey
                    move prg-peso-non-utf to ror-peso-non-utf
                    move prg-peso-utf     to ror-peso-utf
                 end-if
           end-start.

LUBEXX***---
       FORZA-PESO-UGUALE.
           read progmag no lock 
                invalid 
                perform 5 times
                   display message "ARTICOLO " prg-cod-articolo
                            x"0d0a""RIGA " ror-num-riga
                            x"0d0a""DATI INCOERENTI!!!"
                            x"0d0a""CANCELLARE E REINSERIRE LA RIGA"
                             title tit-err
                              icon 2
                end-perform
            not invalid
                move prg-peso-utf     to ror-peso-utf
                move prg-peso-non-utf to ror-peso-non-utf
           end-read.

      ***---
       ABILITA-GEST-PLUS.
LUBEXX     inquire Screen1-Ta-1, value in pagina.
LUBEXX     if pagina = 1
LUBEXX        if tcl-si-gest-plus
LUBEXX           move 1 to v-gest-plus
LUBEXX        else
LUBEXX           move 0 to v-gest-plus tor-gest-plus
LUBEXX        end-if
LUBEXX     else
LUBEXX        move 0 to v-gest-plus
LUBEXX     end-if.
LUBEXX     display lab-gest ef-gest.

      ***---
       ARROTONDA-PRZ-BLISTER.             
           add 0,005 to como-numero giving como-prezzo.
      *    Luciano
      *    devo moltiplicare il prezzo singolo per la qta presente 
      *    all'interno del blister
      *     add como-prezzo to Sum.
           compute como-prezzo2 = como-prezzo * hid-bli-qta
           add como-prezzo2 to Sum.
      *    Luciano fine

           if idx = ins-idx|sono sull'ultimo
              if Sum not = TotPrzBlister
      *    Luciano
      *           if Sum > TotPrzBlister
      *              compute como-prezzo = 
      *                      como-prezzo - (Sum - TotPrzBlister)
      *           else
      *
      *              compute como-prezzo = 
      *                      como-prezzo + (TotPrzBlister - Sum)
      *           end-if

                 if Sum > TotPrzBlister
                    compute como-prezzo2 = 
                            como-prezzo2 - (Sum - TotPrzBlister)
                 else
      
                    compute como-prezzo2 = 
                            como-prezzo2 + (TotPrzBlister - Sum)
                 end-if

      *    devo dividere il prezzo ottenuto per la qta dell'articolo
      *    all'interno del blister
                 compute como-prezzo = como-prezzo2 / hid-bli-qta
      *    Luciano fine
              end-if
           end-if.

      ***---
       CONTROLLA-QTA-BLISTER.
           |Nessun controllo: le qta blister da 
           |master non possono essere cambiate
           if tor-da-ordine-si exit paragraph end-if.
           set tutto-ok to true.
           inquire form1-gd-1, last-row in tot-righe.
           perform varying store-riga from 2 by 1 
                     until store-riga > tot-righe
              inquire form1-gd-1(store-riga, 1), 
                      hidden-data in gruppo-hidden
      *    il controllo lo faccio solo per i vecchi blister
              if hid-blister = 1 and hid-bli-codice = zero
                 inquire form1-gd-1(store-riga, 4), cell-data in col-qta
                 move col-qta  to ror-qta
                 if hid-imballi not = 0 |Sono sul primo elemento
                    move ror-qta  to qta-blis-check
                 else
                    if ror-qta not = qta-blis-check
                       set errori to true
                       subtract 1 from store-riga 
                       move store-riga to riga-ed
                       exit perform
                    end-if
                 end-if
              end-if
           end-perform.
                    
           if errori
              display message"Quantità blister errata. "
             x"0d0a""Dev'essere uguale su tutti gli articoli."
             x"0d0a""Controllare quantità riga ", riga-ed
                        title tit-err
                         icon 2
              set errori to true
              perform CANCELLA-COLORE
           end-if.


      ***--- DUMMY NON TOCCARE
       AGGIORNA-IMPEGNATO-MASTER.

      ***---
       AGGIORNA-NUM-COLLI.
      **    tolgo prima la vecchia qta dal totale dei colli
           if chk-blister-buf = 0
              subtract num-colli-riga-old   from num-colli
           else
      *    tolgo solo per i blister vecchia maniera
              if hid-bli-codice = zero
                 subtract hid-imballi-old   from num-colli
              end-if
           end-if

      *    calcolo il numero di colli della riga
           perform CALC-COLLI-RIGA.

      *    aggiungo il numero dei colli e visualizzo
           add  num-colli-riga  to num-colli.
           move num-colli-riga  to num-colli-riga-old.

           move num-colli      to lab-colli-buf.
           modify lab-colli title lab-colli-buf.

      ***---
       CALC-COLLI-RIGA.
           set trovato-art to false.
           move 0 to idx.
           perform 20 times
              add 1 to idx
              if art-codice = art-no-colli(idx)
                 set trovato-art to true
                 exit perform
              end-if
           end-perform.
           if trovato-art
              move 0 to num-colli-riga
           else
              inquire ef-qta value como-qta
              if chk-blister-buf = 0
                 compute num-colli-riga = como-qta / imq-qta-imb
              else
                 if hid-bli-codice = zero
      *    sommo solo per i blister vecchia maniera
                    move hid-imballi  to num-colli-riga
                 else
      *           compute num-colli-riga = como-qta / hid-bli-qta
                    move zero         to num-colli-riga
                 end-if
              end-if
           end-if.

      ***---
       AGGIORNA-NUM-COLLI-DEL.
           if chk-blister-buf = 0
              subtract num-colli-riga-old from num-colli
           else              
              subtract hid-imballi from num-colli
           end-if.

           move num-colli to lab-colli-buf
           modify lab-colli title lab-colli-buf.


      ***---
       RIALLINEA-CLIENTE.
           set cli-tipo-C to true.
           move save-cli-codice to cli-codice.
           read clienti no lock.

      ***---
       AGGIORNA-QTA-CORRELATE.
      *    se è il vecchio sistema di blister non faccio niente
           if hid-bli-codice = zero
              exit paragraph
           end-if

           divide como-qta by hid-bli-qta 
                       giving num-blist
                    remainder rest-blist


           move riga to store-riga

           inquire form1-gd-1, last-row in tot-righe
           perform until 1 = 2
              inquire form1-gd-1(store-riga, 1),
                       hidden-data in gruppo-hidden
              inquire form1-gd-1(store-riga, 3), 
                       hidden-data hidden-modifiche
              if hid-imballi not = 0
                 perform MODIFICA-QTA
                 exit perform
              end-if
              subtract 1 from store-riga
           end-perform
           perform until 1 = 2
              add 1 to store-riga
              inquire form1-gd-1(store-riga, 1),
                       hidden-data in gruppo-hidden
              inquire form1-gd-1(store-riga, 3), 
                       hidden-data hidden-modifiche
              if hid-imballi not = 0 or 
                 store-riga > tot-righe
                 exit perform
              end-if
              if store-riga not = riga
                 perform MODIFICA-QTA
              end-if
           end-perform.

      ***---
       MODIFICA-QTA.
           compute col-qta = hid-bli-qta * num-blist
           modify form1-gd-1(store-riga, 4),  cell-data = col-qta.
           if hid-imballi not = zero
              subtract hid-imballi   from num-colli
              move num-blist    to hid-imballi
                                   imballi-ed
              add hid-imballi   to num-colli

              modify form1-gd-1(store-riga, 1), 
                          hidden-data = gruppo-hidden


              move num-colli to lab-colli-buf
              modify lab-colli title lab-colli-buf


      *    aggiorno anche la descrizione dell'articolo

              move hid-des-imballo to link-des
              move hid-cod-articolo   to art-codice
              read articoli no lock
                 invalid
                    continue
              end-read

              perform DESCRIZIONE-IMBALLO
              modify form1-gd-1(store-riga, 3), 
                          cell-data = col-des

           end-if.


      ******    Luciano
      *****     if mail-modifica-SHI or mail-modifica-GET
      *****        perform MOD-MAIL
      *****     end-if.
      ******    Luciano fine

      *    Luciano 09/06/2010
      ***---
       TROVA-LISTINO.
           set no-prg-listino to true
      *****     if cli-gdo = space
           if tcl-gdo-no
              exit paragraph
           end-if

           move ef-data-buf to como-data.
           perform DATE-TO-FILE.
           move como-data to mto-data-ordine.

           move cli-gdo          to lst-gdo
           move mto-data-ordine  to lst-data
           move art-codice       to lst-articolo
           start listini key <= lst-k-gdo-articolo
              invalid 
                 continue
              not invalid
                 read listini previous
                 if lst-gdo      = cli-gdo          and
                    lst-data    <= mto-data-ordine  and
                    lst-articolo = art-codice

                    if lst-prg-cod-articolo = space
                       move zero   to lst-prg-cod-articolo
                    end-if

                    if lst-prg-cod-articolo not = zero
                       set si-prg-listino   to true
                    end-if
                       
                 end-if
           end-start.

           if si-prg-listino
              move lst-prg-chiave  to prg-chiave
              read progmag no lock
                 invalid
                    set nv-prg-listino   to true
              end-read

              if not prg-attivo
                 set bl-prg-listino   to true
              end-if

              if tca-cod-magaz not = prg-cod-magazzino
                 set nc-prg-listino   to true
              end-if
           end-if.

           evaluate true
           when si-prg-listino
                move lst-prg-chiave  to como-prg-chiave
           when no-prg-listino
                continue
           when bl-prg-listino
                display message box 
                    "Progressivo associato al listino bloccato."
                    title titolo
                    icon 2
                set errori   to true
           when nv-prg-listino
                display message box 
                    "Progressivo associato al listino inesistente."
                    title titolo
                    icon 2
                set errori   to true
           when nc-prg-listino
                display message box 
                    "Progressivo associato al listino"
                    x"0D0A"
                    "non compatibile con la causale scelta."
                    title titolo
                    icon 2
                set errori   to true
           end-evaluate.

      ***---
       TROVA-CLI-PRG.
           set  cp-tipo-C        to true.
           move cli-codice       to cp-clifor.
           move art-codice       to cp-articolo.
           read cli-prg no lock
                invalid continue
            not invalid set si-prg-listino   to true
           end-read.

           if si-prg-listino
              move cp-prg-chiave  to prg-chiave
              read progmag no lock
                 invalid
                    set nv-prg-listino   to true
              end-read

              if not prg-attivo
                 set bl-prg-listino   to true
              end-if

              if tca-cod-magaz not = prg-cod-magazzino
                 set nc-prg-listino   to true
              end-if
           end-if.

           evaluate true
           when si-prg-listino
                move cp-prg-chiave  to como-prg-chiave
           when no-prg-listino
                continue
           when bl-prg-listino
                display message box 
                    "Progressivo associato al listino bloccato."
                    title titolo
                    icon 2
                set errori   to true
           when nv-prg-listino
                display message box 
                    "Progressivo associato al listino inesistente."
                    title titolo
                    icon 2
                set errori   to true
           when nc-prg-listino
                display message box 
                    "Progressivo associato al listino"
                    x"0D0A"
                    "non compatibile con la causale scelta."
                    title titolo
                    icon 2
                set errori   to true
           end-evaluate.

      ***---
       FIND-PROGMAG-LISTINO.
           move 1               to num-articoli
           move como-prg-chiave to GiacenzaKey.

           copy "controlla-fuori-fido.cpy".
