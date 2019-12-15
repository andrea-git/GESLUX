       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      EDI-confid-p.
       AUTHOR.                          Andrea.
       REMARKS.
           Controllo fido da numero a numero

      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       copy "EDI-mtordini.sl".
       copy "EDI-mrordini.sl".
       copy "mtordini.sl".
       copy "mrordini.sl".
       copy "blister.sl".
       copy "clienti.sl".
       copy "ttipocli.sl".
       copy "progmag.sl".
       copy "tmarche.sl".
       copy "listini.sl".
       copy "cli-prg.sl".
       copy "articoli.sl".
       copy "tparamge.sl".
       copy "tivaese.sl".
       copy "timposte.sl".
       copy "tnazioni.sl".
       copy "tpiombo.sl".
       copy "destini.sl".
       copy "tcontat.sl".
       copy "param.sl.".  
       copy "grade.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.                        
       copy "EDI-mtordini.fd".
       copy "EDI-mrordini.fd".
       copy "mtordini.fd".
       copy "mrordini.fd".
       copy "blister.fd".
       copy "clienti.fd".
       copy "ttipocli.fd".
       copy "progmag.fd".
       copy "tmarche.fd".
       copy "listini.fd".
       copy "cli-prg.fd". 
       copy "articoli.fd".
       copy "tparamge.fd".
       copy "tivaese.fd". 
       copy "timposte.fd".
       copy "tnazioni.fd".
       copy "tpiombo.fd".
       copy "destini.fd".
       copy "tcontat.fd".
       copy "param.fd.".
       copy "grade.fd".

       WORKING-STORAGE SECTION.      
           copy "costo-medio.def".   
           copy "imposte.def". 
           copy "trova-parametro.def".
           copy "link-calfido.def".
      *    FILE-STATUS
       77  status-EDI-mtordini   pic xx.
       77  status-EDI-mrordini   pic xx.
       77  status-mtordini       pic xx.
       77  status-mrordini       pic xx.
       77  status-blister        pic xx.
       77  status-clienti        pic xx.
       77  status-ttipocli       pic xx.
       77  status-progmag        pic xx.
       77  status-tmarche        pic xx.
       77  status-listini        pic xx.
       77  status-cli-prg        pic xx.
       77  status-articoli       pic xx.   
       77  status-tparamge       pic xx.
       77  status-tivaese        pic xx.
       77  status-timposte       pic xx.
       77  status-tnazioni       pic xx.
       77  status-tpiombo        pic xx.
       77  status-destini        pic xx.
       77  status-tcontat        pic xx.
       77  status-param          pic xx.
       77  status-grade          pic xx.

      *    COSTANTI
       78  titolo                value "Controllo fido EDI". 
                                          
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).  
       77  como-qta              pic 9(8). 
       77  Sum                   pic s9(9)v99.  
       77  TotPrzBlister         pic 9(13)v99. 
       77  SavePrezzo            pic s9(9)v99.
       77  idx                   pic 9(5).
       77  mult                  pic 9v99.  
       77  como-numero           pic 9(9)v99999.      
       77  scoperto              pic s9(9)v99.
       77  fido-tmp              pic s9(13)v99.
       77  fido-usato            pic s9(13)v99.
       77  tot-fido              pic s9(9)v99.   
       77  giacenza              pic s9(8).

       01 GiacenzaKey.
         10 gia-prg-cod-articolo  pic 9(6).
         10 gia-prg-cod-magazzino pic x(3).
         10 gia-prg-tipo-imballo  pic x(3).
         10 gia-prg-peso          pic 9(5)v9(3).  
 
       01 como-prg-chiave.
           10 como-prg-cod-articolo        PIC  9(6).
           10 como-prg-cod-magazzino       PIC  X(3).
           10 como-prg-tipo-imballo        PIC  X(3).
           10 como-prg-peso    PIC  9(5)v9(3).

       01 filler                 pic x.
         88 si-prg-listino       value "S". 
         88 no-prg-listino       value "N".  
                                               
       01  filler                 pic 9    value 0.
         88 record-ok             value 1, false 0.   

       01  filler                 pic 9    value 0.
         88 trovato               value 1, false 0.

       01  controlli              pic xx.    
         88 tutto-ok              value "OK".
         88 errori                value "ER".

       LINKAGE SECTION.
       01 cfid-linkage.
         05 cfid-anno           pic 9(4).
         05 cfid-da-num         pic 9(8).
         05 cfid-a-num          pic 9(8).

      ******************************************************************
       PROCEDURE DIVISION USING cfid-linkage.
       MAIN.
           perform INIT-PGM.
           perform OPEN-FILES.
           perform ELABORAZIONE.
           perform CLOSE-FILES.
           perform EXIT-PGM.

      ***---
       INIT-PGM.

      ***---
       OPEN-FILES.                 
           open i-o   edi-mtordini.
           open input edi-mrordini mtordini mrordini blister listini
                      clienti ttipocli progmag tmarche cli-prg articoli
                      tparamge tivaese timposte tnazioni tpiombo 
                      destini tcontat param grade.

      ***---
       ELABORAZIONE.
           move spaces to tge-chiave.
           read tparamge.

           move cfid-anno    to emto-anno.
           move cfid-da-num  to emto-numero.
                                            
           start EDI-mtordini key >= emto-chiave
                 invalid continue
             not invalid perform SCORRI-ORDINI
           end-start.

      ***---
       SCORRI-ORDINI.      
           move emto-anno to con-anno.
           read tcontat no lock.

           perform until 1 = 2
              read EDI-mtordini next at end exit perform end-read
              if cfid-anno not = emto-anno
                 exit perform
              end-if
              if emto-numero > cfid-a-num
                 exit perform
              end-if        

              if emto-ordine-anno   not = 0 or 
                 emto-ordine-numero not = 0
                 if emto-attivo or emto-bloccato
                    set emto-caricato to true
                    rewrite emto-rec
                 end-if
                 exit perform cycle
              end-if

              perform VALIDA-RECORD

              if record-ok 
                 set emto-cliente-fido-ok    to true
              else                               
                 set emto-cliente-fuori-fido to true
              end-if                         

              if emto-cliente-valido  and 
                 emto-cliente-fido-ok and
                 emto-destino-valido  and
                 emto-righe-presenti  and
                 emto-qta-ok          and 
                 emto-art-ok          and
                 emto-prg-ok          and
                 emto-esistente-si
                 set emto-attivo   to true
              else
                 set emto-bloccato to true
              end-if              
                 
              rewrite emto-rec

           end-perform.

      ***---
       VALIDA-RECORD.   
           set record-ok  to false.
           set cli-tipo-C to true.
           move emto-cod-cli to cli-codice.
           read clienti no lock
                invalid continue
            not invalid
                move cli-codice       to des-codice
                move emto-prg-destino to des-prog
                read destini no lock

                move cli-tipo to tcl-codice
                read ttipocli no lock
                     invalid continue
                 not invalid 
           
                     evaluate tcl-serie-bolle
                     when 1 move con-ult-stampa-bolle-gdo to imp-data
                     when 2 move con-ult-stampa-bolle-mv  to imp-data
                     when 3 move con-ult-stampa-bolle-at  to imp-data
                     end-evaluate
                     
                     start timposte key <= imp-chiave
                           invalid continue
                       not invalid
                           read timposte previous
                     end-start

                     if ttipocli-gdo set TrattamentoGDO to false
                     else            set TrattamentoGDO to true
                     end-if
                     move 0 to Sum
                     move low-value   to emro-chiave
                     move emto-chiave to emro-chiave
                     start EDI-mrordini key >= emro-chiave
                           invalid continue
                       not invalid
                           perform until 1 = 2
                              read EDI-mrordini next 
                                   at end exit perform 
                              end-read
                              if emro-chiave-testa not = emto-chiave
                                 exit perform
                              end-if
                              move emro-qta         to como-qta
                              move emro-prz-GESLUX  to SavePrezzo
                              move emro-prg-chiave  to prg-chiave
                              move emto-cod-cli     to mto-cod-cli
                              move emto-prg-destino to mto-prg-destino
                              move emto-data-ordine to mto-data-ordine
                              perform AGGIUNGI-VALORI
                           end-perform
                     end-start
                     perform CONTROLLA-FUORI-FIDO
                end-read
           end-read.        

      ***---
       AGGIUNGI-VALORI.
           move prg-cod-articolo to art-codice
           read articoli no lock
                invalid
                move art-codice to bli-codice
                read blister no lock
                     invalid continue
                 not invalid    
                     move SavePrezzo to TotPrzBlister
                     move 0 to idx
                     perform varying idx from 1 by 1
                               until idx > 50
                        if bli-el-articolo(idx) = 0
                           exit perform
                        end-if        
                        set trovato  to false
                        move bli-el-articolo(idx) to art-codice
                        if tcl-gdo-si or tcl-gdo-opz
                           perform TROVA-LISTINO
                           if no-prg-listino
                              perform TROVA-CLI-PRG
                           end-if     
                           if si-prg-listino
                              move como-prg-chiave to GiacenzaKey
                              set trovato to true
                           else
                              perform VALORIZZA-MAGGIOR-GIACENZA
                           end-if
                        else
                           perform VALORIZZA-MAGGIOR-GIACENZA
                        end-if

                        if trovato             
                           move bli-el-articolo(idx) to art-codice
                           move bli-el-qta(idx)      to mro-bli-qta
                           move bli-el-perce(idx)    to mro-bli-perce
                           move GiacenzaKey          to prg-chiave
                           read progmag  no lock 
                                invalid continue 
                           end-read
                           read articoli no lock 
                                invalid continue 
                           end-read  
                                                                  
                           compute mro-qta = como-qta * bli-el-qta(idx)
           
                           if bli-el-perce(idx) not = 0
           
                              compute como-numero =
                                      TotPrzBlister * 
                                      bli-el-perce(idx) / 100 / 
                                      bli-el-qta(idx) 
                              move como-numero to SavePrezzo
                              perform AGGIUNGI-VALORE
                           end-if
                        end-if
                     end-perform
                end-read
            not invalid
                perform AGGIUNGI-VALORE
           end-read.

      ***---
       AGGIUNGI-VALORE.          
           move art-marca-prodotto to mar-codice.
           read tmarche.
           read progmag no lock.
           if SavePrezzo > 999999    
              perform CALCOLA-COSTO-MP-COMPLETO
              |Altrimenti righe solo 999999,99 senza costo mp
              |darebbero come risultato un master prezzo ZERO
              if costo-mp = 0
                 compute Sum = Sum + 0,01
              else
                 compute Sum = Sum + ( costo-mp * como-qta )
              end-if
           else                  
              if SavePrezzo > 0 

                 move SavePrezzo to como-prz-unitario mro-prz-unitario
                 perform CALCOLA-IMPOSTE-ORDINE
                 perform CALCOLA-IMPONIBILE    
                 
                 compute SavePrezzo =
                         mro-imponib-merce + 
                         mro-imp-cou-cobat + 
                         mro-imp-consumo   +
                         mro-add-piombo         

                 if tcl-fido-nuovo-si    
                       
                    if cli-iva not = spaces
                       move cli-iva to tbliv-codice2
                    else
                       if cli-iva-ese not = spaces
                          move cli-iva-ese to tbliv-codice2
                       else
                          move tge-cod-iva-std to tbliv-codice2
                       end-if
                    end-if

                    move "IV"        to tbliv-codice1
                    read tivaese 
                    compute mult = 1 + tbliv-percentuale / 100
                    compute SavePrezzo =
                            SavePrezzo * mult
                 end-if

                 compute Sum = Sum + ( SavePrezzo * como-qta )
              end-if
           end-if.                         

      ***---
       TROVA-LISTINO.
           initialize como-prg-chiave replacing numeric data by zeroes
                                           alphanumeric data by spaces.
           set no-prg-listino to true.

           move cli-gdo          to lst-gdo.
           move emto-data-ordine to lst-data.
           move art-codice       to lst-articolo.
           start listini key <= lst-k-articolo
                 invalid continue
             not invalid
                 read listini previous
                 if lst-gdo      = cli-gdo          and
                    lst-data    <= emto-data-ordine and
                    lst-articolo = art-codice

                    if lst-prg-cod-articolo = space
                       move zero   to lst-prg-cod-articolo
                    end-if

                    if lst-prg-cod-articolo not = 0
                       set si-prg-listino   to true
                    end-if
                       
                 end-if
           end-start.

           if si-prg-listino
              move lst-prg-chiave  to prg-chiave
              read progmag no lock
                   invalid set emro-progressivo-non-forzato to true
               not invalid 
                   if prg-bloccato or prg-disattivo or
                      prg-cod-magazzino not = "LBX"
                      move 0 to como-prg-cod-articolo
                   else
                      move lst-prg-chiave  to como-prg-chiave
                   end-if
              end-read
           end-if.

      ***---
       TROVA-CLI-PRG.
           set  cp-tipo-C        to true.
           move cli-codice       to cp-clifor.
           move art-codice       to cp-articolo.
           read cli-prg no lock
                invalid continue
            not invalid 
                set si-prg-listino   to true
                read progmag no lock
                     invalid set emro-progressivo-non-forzato to true
                 not invalid 
                     if prg-bloccato or prg-disattivo or
                        prg-cod-magazzino not = "LBX"
                        move 0 to como-prg-cod-articolo
                     else
                        move cp-prg-chiave  to como-prg-chiave
                     end-if
                end-read
           end-read.
                            
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

           |Se non ho nemmeno un progerssivo attivo prendo il primo
           if not trovato
              set  trovato              to true
              move low-value            to prg-chiave
              move bli-el-articolo(idx) to prg-cod-articolo
              move bli-magazzino        to prg-cod-magazzino
              start progmag key >= prg-chiave
                    invalid continue
                not invalid
                    read progmag next
                    move prg-chiave to GiacenzaKey
              end-start
           end-if.                        
           
      ***---
       CALCOLA-IMPOSTE-ORDINE.
           move 0 to imposta-cobat imposta-cou add-piombo
                     mro-imp-cou-cobat mro-imp-consumo 
                     mro-add-piombo.
           |L'ho dovuta searare in quanto, se TROVO il movimento di
           |magazzino ma poi cambio il prezzo devo rifare il calcolo
           |delle imposte. Viene richiamato sull'after del campo
           |prezzo ossia quando viene cambiato

           if emto-prg-destino = 0
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
           move 0 to mro-imp-consumo mro-imp-cou-cobat mro-add-piombo
                     imposta-cou imposta-consumo add-piombo.

      ***---
       CALCOLO-IMPOSTE-STANDARD.
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
                 move como-imposta      to mro-imp-consumo
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
                 move como-imposta      to mro-imp-cou-cobat
              end-if

           else
              move 0 to mro-imp-consumo
           end-if.

           if art-si-cobat
              perform CALCOLA-COBAT  
              if tcl-si-piombo
                 perform CALCOLA-ADD-PIOMBO
              end-if
           end-if.

      ***---
       CALCOLO-IMPOSTE-GDO.
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
              move como-imposta      to mro-imp-consumo
           else
              move 0 to mro-imp-consumo
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
           move como-imposta      to mro-imp-cou-cobat.
                                                        
           if art-si-cobat
              perform CALCOLA-COBAT
              if tcl-si-piombo
                 perform CALCOLA-ADD-PIOMBO
              end-if
           end-if.

      ***---
       CALCOLA-COBAT.
           if ttipocli-gdo or mar-si-cobat
              perform SCAGLIONI-COBAT
           end-if.

      ***---
       SCAGLIONI-COBAT.
           evaluate true
           when art-auto-cobat
                evaluate true
                when art-amperaggio >= imp-cb-auto-sca-1-da and
                     art-amperaggio <= imp-cb-auto-sca-1-a
                     move imp-cb-auto-sca-1-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-auto-sca-2-da and
                     art-amperaggio <= imp-cb-auto-sca-2-a
                     move imp-cb-auto-sca-2-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-auto-sca-3-da and
                     art-amperaggio <= imp-cb-auto-sca-3-a
                     move imp-cb-auto-sca-3-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-auto-sca-4-da and
                     art-amperaggio <= imp-cb-auto-sca-4-a
                     move imp-cb-auto-sca-4-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-auto-sca-5-da and
                     art-amperaggio <= imp-cb-auto-sca-5-a
                     move imp-cb-auto-sca-5-euro 
                       to imposta-cobat
                end-evaluate
           
           when art-moto-cobat
                evaluate true
                when art-amperaggio >= imp-cb-scooter-sca-1-da 
                 and art-amperaggio <= imp-cb-scooter-sca-1-a
                     move imp-cb-scooter-sca-1-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-scooter-sca-2-da 
                 and art-amperaggio <= imp-cb-scooter-sca-2-a
                     move imp-cb-scooter-sca-2-euro 
                       to imposta-cobat
                when art-amperaggio >= imp-cb-scooter-sca-3-da 
                 and art-amperaggio <= imp-cb-scooter-sca-3-a
                     move imp-cb-scooter-sca-3-euro 
                       to imposta-cobat
                end-evaluate
           end-evaluate.
           
           add 0,005              to imposta-cobat.
           add  imposta-cou       to imposta-cobat.
           move imposta-cobat     to mro-imp-cou-cobat.

      ***---
       CALCOLA-ADD-PIOMBO. 
           move mro-prz-unitario   to como-prz-unitario.
           move mro-imp-cou-cobat  to como-imp-cou-cobat.

           move imposta-cobat      to mro-imp-cou-cobat.
           move art-marca-prodotto to tpb-marca.
           move mto-data-ordine    to como-data-ordine tpb-data.
           move mto-cod-cli        to como-prm-cliente.
           move mto-prg-destino    to como-prm-destino.
           perform ADDIZIONALE-PIOMBO.
           move add-piombo to mro-add-piombo.

      ***---
       CALCOLA-IMPONIBILE.
           evaluate true
           when ttipocli-gdo
                compute mro-imponib-merce = 
                        mro-prz-unitario  - 
                        mro-imp-cou-cobat - 
                        mro-imp-consumo   -
                        mro-add-piombo
           when other
                if mro-prz-unitario >= 9999999,99
                   move 9999999,99 to mro-imponib-merce
                else
                   compute mro-imponib-merce = 
                           mro-prz-unitario  
                end-if
           end-evaluate.        
      
      ***---
       CONTROLLA-FUORI-FIDO.
           if cli-escludi-fido-si exit paragraph end-if.

LUBEXX        initialize calfido-linkage 
LUBEXX                   replacing numeric data by zeroes
LUBEXX                        alphanumeric data by spaces
LUBEXX        move cli-codice to link-cli-codice
LUBEXX        call   "calfido" using calfido-linkage
LUBEXX        cancel "calfido"
LUBEXX        compute scoperto = saldo + Sum     +
LUBEXX                           effetti-rischio + ordini-in-essere
              if cli-gestione-fido-si
      *****           move cli-piva to sf-piva
      *****           read sitfin no lock                   
      *****                invalid move 0 to sf-fido-max
      *****           end-read                   
                 if tcl-fido-nuovo-si            
                    compute fido-tmp = cli-fido |sf-lince
                 else
                    compute tot-fido = cli-fido |sf-lince
                 end-if
              else
                 if tcl-fido-nuovo-si
                    compute fido-tmp = cli-fido
                 else
                    compute tot-fido = cli-fido + 
                                       cli-pfa  + 
                                       cli-fidejussione
                 end-if
              end-if  
              if tcl-fido-nuovo-si  
                 move 0 to fido-usato
                 if cli-fidejussione > 0

                    if cli-grade > 0
                       move spaces to gra-codice
                       read grade no lock
                            invalid move 0 to Sum
                        not invalid
                            perform varying idx from 1 by 1 
                                      until idx > 20
                               if gra-da(idx) <= cli-grade and
                                  gra-a(idx)  >= cli-grade
                                  move gra-perce(idx) to Sum
                                  exit perform
                               end-if
                            end-perform
                        end-read
                        if Sum > 0
                           compute cli-fidejussione = 
                                   cli-fidejussione * Sum / 100
                        end-if
                    end-if

                    compute fido-usato = 
                            cli-fidejussione +
                            cli-fido-extra   +
                            cli-pfa
                 else
                    if tge-blocco-fido < cli-fido
                       compute fido-usato = tge-blocco-fido + cli-pfa
                    else
                       compute fido-usato = fido-tmp + cli-pfa
                    end-if
                 end-if
                 if scoperto > fido-usato
                    subtract fido-usato from scoperto giving como-numero
LUBEXX*****              move como-numero to como-edit
                    set errori to true
LUBEXX*****              display message
LUBEXX*****                      "ATTENZIONE!!!!"
LUBEXX*****               x"0d0a""CLIENTE FUORI FIDO DI:  " como-edit
LUBEXX*****               x"0d0a""IMPOSSIBILE REGISTRARE L'ORDINE!"
LUBEXX*****                        title tit-err
LUBEXX*****                         icon 2
LUBEXX           end-if
              else
LUBEXX           if scoperto > tot-fido
                    if Sum <= cli-fido-extra
LUBEXX                 close    clienti
LUBEXX                 open i-o clienti
LUBEXX                 read clienti no lock invalid continue end-read
LUBEXX                 subtract Sum from cli-fido-extra
LUBEXX                 rewrite cli-rec invalid continue end-rewrite
LUBEXX                 close clienti
LUBEXX                 open input clienti
LUBEXX                 read clienti no lock invalid continue end-read
                    else
                       compute como-numero = scoperto - tot-fido
                       if como-numero > cli-fido-extra
LUBEXX                    set errori to true
                          subtract cli-fido-extra from como-numero
LUBEXX*****                    move como-numero to como-edit
LUBEXX*****                    display message
LUBEXX*****                            "ATTENZIONE!!!!"
LUBEXX*****                     x"0d0a""CLIENTE FUORI FIDO DI:  " como-edit
LUBEXX*****                     x"0d0a""IMPOSSIBILE REGISTRARE L'ORDINE!"
LUBEXX*****                              title tit-err
LUBEXX*****                               icon 2
LUBEXX                 else
LUBEXX                    close    clienti
LUBEXX                    open i-o clienti
LUBEXX                    read clienti no lock invalid continue end-read
LUBEXX                    subtract como-numero from cli-fido-extra
LUBEXX                    rewrite cli-rec invalid continue end-rewrite
LUBEXX                    close clienti
LUBEXX                    open input clienti
LUBEXX                    read clienti no lock invalid continue end-read
                       end-if
LUBEXX              end-if
LUBEXX           end-if
              end-if.

      ***---
       CLOSE-FILES.
           close edi-mtordini edi-mrordini mtordini mrordini blister 
                 clienti ttipocli progmag tmarche listini cli-prg
                 articoli tivaese tparamge timposte tnazioni tpiombo
                 destini tcontat param grade.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.                
           copy "costo-medio.cpy".
           copy "recupero-anagrafica.cpy".
           copy "addizionale-piombo.cpy".
           copy "trova-parametro.cpy".
