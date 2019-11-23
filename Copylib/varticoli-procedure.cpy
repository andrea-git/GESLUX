      ***---
       CONTROLLO.
           set tutto-ok to true.

      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate CONTROL-ID
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
           read articoli no lock invalid 
              set errori to true 
           end-read
           set ReadSecca to false.

           perform FORM1-IUD-DISPLAY.

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
           move 78-ID-ef-des     to min-id(1).
           move 78-ID-cbo-stato  to max-id(1).
      *     move 78-ID-form1-gd-1        to min-id(2).
      *     move 78-ID-ef-note-4         to max-id(2).
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
           end-evaluate.

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
           end-evaluate.

      ***---
       RIEMPI-CHIAVE.
           move ef-codice-buf to art-codice of articoli with convert.

      *******************************************************************
      *                  SEZIONE ENTRY_POINT & EVENTS                   *
      *******************************************************************
      ***---
       GARTICOLI-BEFORE-ACCEPT.
           perform INIT.

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

           move vart-codice  to ef-codice-buf
           modify ef-codice value ef-codice-buf.
           perform CURRENT-RECORD.


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
           if art-scorta of articoli = 5 or
              art-scorta of articoli = 7
              move 1 to v-limite
           else
              move 0 to v-limite
           end-if.
           display lab-limite ef-limite.
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
           when attivo    move 1 to mod-campi
           when disattivo move 0 to mod-campi
           end-evaluate.              
           
           move art-gestione-utf of articoli        to gestione-utf.
           perform CARICA-COMBO-UTF.
           
           move art-auto-moto-per-cobat of articoli        to cobat.
           perform CARICA-COMBO-COBAT.
                                  
           move art-codice of articoli    to  codice-ed.
           move codice-ed     to  ef-codice-buf.   
           call "C$JUSTIFY" using ef-codice-buf, "L".
           display ef-codice.       
           
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

      *     if vecchio
      *        move art-codice of articoli to dis-articolo-finale
      *        move 0          to dis-codice
      *        start distinteb key is >= k-articolo
      *              invalid continue
      *          not invalid
      *              read distinteb next 
      *                   at end continue
      *               not at end
      *                   if dis-articolo-finale = art-codice of articoli
      *                      move 1 to BitmapNumDistinta
      *                      move 1 to e-distinta
      *                   end-if
      *               end-read
      *        end-start
      *     end-if.
      *     modify pb-distinta, enabled = e-distinta,
      *                   bitmap-number = BitmapNumDistinta.

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
           move zero   to e-campo.
           perform APRI-NOTE.

      ***---
       PB-FOTO-PRESSED.
           inquire ef-foto, value in LinkImage.
           move zero to LinkAbil.
           call   "browser" using LinkBrowser.
           cancel "browser".
           move LinkImage to ef-foto-buf.
           display ef-foto.

      ***---
       PB-BRAND-PRESSED.
           inquire ef-brand, value in LinkImage.
           move zero to LinkAbil.
           call   "browser" using LinkBrowser.
           cancel "browser".
           move LinkImage to ef-brand-buf.
           display ef-brand.

      ***---
       PB-SCHEDA-PRESSED.
           move 1 to num-campo.
           move zero to e-campo.
           inquire ef-scheda, value in path-note.
           perform APRI-NOTE.
           move opnsav-filename to ef-scheda-buf.
           display ef-scheda.

      ***---
       PB-TOSS-PRESSED.
           move 1 to num-campo.
           move zero to e-campo.
           inquire ef-toss, value in path-note.
           perform APRI-NOTE.
           move opnsav-filename to ef-toss-buf.
           display ef-toss.

      ****---
      * PB-DISTINTA-PRESSED.
      *     set tutto-ok    to true.
      *     move 0          to num-articoli.
      *     move 0          to dis-codice.
      *     move art-codice of articoli to dis-articolo-finale.
      *
      *     start distinteb key is >= k-articolo
      *           invalid continue
      *       not invalid
      *           perform until 1 = 2
      *              read distinteb next at end exit perform end-read
      *              if dis-articolo-finale not = art-codice of articoli
      *                 exit perform
      *              end-if
      *              add 1 to num-articoli
      *              evaluate true
      *              when num-articoli = 1
      *                   move dis-codice to save-codice
      *              when num-articoli > 1
      *                   exit perform
      *              end-evaluate
      *           end-perform
      *     end-start.

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
      *     if tutto-ok
      *        move save-codice to dis-codice
      *        call   "vdistinta" using dis-codice
      *        cancel "vdistinta"
      *     end-if.

      ****---
      * PB-CODICI-PRESSED.
      *     set link-accept to true.
      *     perform SCRIVI-PROGMAG.

      ****---
      * PB-DETTAGLI-PRESSED.
      *     move "prg-artico-sons" to como-file.
      *     inquire ef-codice, value in prg-cod-articolo
      *     call "zoom-gt"   using como-file, prg-rec
      *                     giving stato-zoom
      *     end-call.
      *     cancel "zoom-gt".
      *     if stato-zoom = 0
      *        call   "vprogmag" using prg-chiave
      *        cancel "vprogmag"
      *     end-if.

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

      ****---
      * PB-STAMPA-PRESSED.
      *     perform CANCELLA-COLORE.
      *     if nuovo
      *        display message "Occorre confermare l'articolo"
      *                  title titolo
      *                   icon 2
      *     else
      *        call "W$MOUSE" using set-mouse-shape, wait-pointer
      *        call   "st-art-det" using art-codice of articoli
      *        cancel "st-art-det"
      *        call "W$MOUSE" using set-mouse-shape, arrow-pointer
      *     end-if.
      *     move 760 to control-id.
      *     move 4   to accept-control.
      *     modify pb-stampa, bitmap-number = 2.

      ****---
      * SELEZIONE-COMBO-UTF.
      *     perform SCARICA-COMBO-UTF.
      *     evaluate true
      *     when soggetto     move 1 to mod-peso-utf
      *                       move 0 to mod-peso-non-utf
      *                       move 0 to ef-peso-non-utf-buf
      *     when misto        move 1 to mod-peso-utf
      *                       move 1 to mod-peso-non-utf
      *     when non-soggetto move 0 to mod-peso-utf
      *                       move 1 to mod-peso-non-utf   
      *                       move 0 to ef-peso-utf-buf
      *     end-evaluate.
      *
      *     display ef-peso-utf ef-peso-non-utf.

      ****---
      * PB-OK-EAN-PRESSED.
      *     move 0 to art-codice of articoli.
      *     inquire ef-ean, value in ef-ean-buf.
      *     if ef-ean-buf not = 0
      *        call   "cerca-ean" using art-codice of articoli, 
      *                                 ef-ean-buf
      *        cancel "cerca-ean"
      *        if art-codice of articoli = 0
      *           display message "Nessun articolo trovato"
      *                     title titolo
      *                      icon 2
      *           modify pb-ok, bitmap-number = 1
      *           move 78-ID-ef-ean to control-id
      *           move 4            to accept-control
      *        else
      *           move 27 to key-status
      *        end-if
      *     else
      *        move 4 to accept-control
      *     end-if.
      *
      ****---
      * PB-EAN-PRESSED.
      *     perform  CANCELLA-COLORE.
      *     perform FORM-EAN-OPEN-ROUTINE.
      *     if art-codice of articoli not = 0
      *        if old-art-chiave not = art-chiave of articoli
      *           move art-chiave of articoli to  save-chiave
      *           perform SALV-MOD
      *           if tutto-ok
      *              move save-chiave  to art-chiave of articoli
      *              modify ef-codice, value = art-codice of articoli
      *              move  art-codice of articoli to ef-codice-buf
      *              set   ReadSecca  to true
      *              perform CANCELLA-COLORE
      *              perform CURRENT-RECORD
      *              modify pb-ean, bitmap-number = 1
      *              move 78-ID-ef-codice to control-id
      *              move 4 to accept-control
      *           end-if
      *        else
      *           move 1761 to control-id
      *           move 4    to accept-control
      *        end-if
      *     else
      *        move 1761 to control-id
      *        move 4    to accept-control
      *     end-if.

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

      ****---
      * CBO-STATO-SELCHANGE.
      *     perform NO-SOSPESO.

      ****---
      * NO-SOSPESO.
      *     |Una volta tolto lo stato di sospeso
      *     |non può più essere impostato!!!
      *     if vecchio
      *        if old-art-bloccato or old-art-attivo
      *           inquire cbo-stato value cbo-stato-buf
      *           if cbo-stato-buf = "Sospeso"
      *              move old-art-stato to stato
      *              perform CARICA-COMBO-STATO
      *           end-if
      *        end-if
      *     end-if.

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

                       move tlis-trasp        to como-trasporto
                       perform CALCOLA-PRZ-FINITO
           
                       move prz-reale         to sart-prz-fint
                       move prz-confronto     to sart-prz-conf 

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

      ****---
      * CONTROLLA-PESO.
      *     inquire ef-altezza,    value in como-altezza.
      *     inquire ef-larghezza,  value in como-larghezza.
      *     inquire ef-profondita, value in como-profondita.
      *     inquire ef-qta-EPAL,   value in como-qta-EPAL.
      *     inquire ef-qta-STD,    value in como-qta-STD.
      *     if como-qta-EPAL = 0
      *        move como-qta-STD to como-qta-EPAL
      *     end-if.
      *
      *     inquire ef-peso-utf,     
      *             value in art-peso-utf     of articoli.
      *     inquire ef-peso-non-utf, 
      *             value in art-peso-non-utf of articoli.
      *     compute peso-inserito =  art-peso-utf     of articoli + 
      *                              art-peso-non-utf of articoli.
      *
      *     compute como-peso =
      *         ( ( como-altezza * como-larghezza * como-profondita ) / 
      *             1000000 * 200 ) / como-qta-EPAL.
      *     if como-peso > peso-inserito
      *        move 9 to ef-scorta-buf
      *        display ef-scorta
      *        move peso-inserito to ef-reale-buf
      *        move 1 to v-reale
      *        display ef-reale
      *        if art-peso-utf of articoli > 0
      *           move como-peso to ef-peso-utf-buf
      *           display ef-peso-utf
      *        end-if
      *        if art-peso-non-utf of articoli > 0
      *           move como-peso to ef-peso-non-utf-buf
      *           display ef-peso-non-utf
      *        end-if
      *        move 0 to ef-limite-buf
      *        move 0 to v-limite
      *        display ef-limite
      *     end-if.

      ***---
      * CONTROLLA-PRESENZA-LISTINI-F.
      *     move low-value to rlis-rec.
      *     move art-codice of articoli to rlis-articolo.
      *     start rlistini key >= rlis-k-art
      *           invalid 
      *           set errori to true
      *           display message 
      *            "Nessun listino fornitore relativo presente."
      *     x"0d0a""Impossibile attivare l'articolo"
      *                     title tit-err
      *                      icon 2
      *       not invalid
      *           read rlistini next
      *           if rlis-articolo not = art-codice of articoli
      *              set errori to true
      *              display message 
      *            "Nessun listino fornitore relativo presente."
      *     x"0d0a""Impossibile attivare l'articolo"
      *                        title tit-err
      *                         icon 2
      *           end-if
      *     end-start.

      ****---
      * AGGIORNA-CATENA.
      *     perform FORM3-OPEN-ROUTINE.

