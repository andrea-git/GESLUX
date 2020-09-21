       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      tprev-p.
       AUTHOR.                          Andrea.
       REMARKS. Motore di elaborazione promo per evasione
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "tpromo.sl". 
           copy "rpromo.sl".
           copy "promoeva.sl".
           copy "progmag.sl".
           copy "tmagaz.sl".
           copy "blister.sl".
           copy "articoli.sl".
           copy "mrordini.sl".
           copy "tparamge.sl".
           copy "param.sl".
           copy "tgrupgdo.sl".
           copy "mtordini.sl".
           copy "clienti.sl". |Solo per la copy non serve aprirlo

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "tpromo.fd". 
           copy "rpromo.fd".
           copy "promoeva.fd".
           copy "progmag.fd".
           copy "tmagaz.fd".
           copy "blister.fd".
           copy "articoli.fd".
           copy "mrordini.fd".
           copy "tparamge.fd".
           copy "param.fd".
           copy "tgrupgdo.fd".
           copy "mtordini.fd".
           copy "clienti.fd".

       WORKING-STORAGE SECTION.
           COPY "acucobol.def".
           copy "link-geslock.def".
           copy "comune.def".
           copy "versione-evasione.def".
           copy "trova-parametro.def".

       78  titolo    value "Promo per evasione".
       78  78-clear              value 
           "                                                          ".

       77  status-tpromo    pic xx.
       77  status-tpromo2   pic xx.
       77  status-rpromo    pic xx.
       77  status-promoeva  pic xx.  
       77  status-progmag   pic xx.
       77  status-tmagaz    pic xx.
       77  status-blister   pic xx.
       77  status-articoli  pic xx.
       77  status-mrordini  pic xx.
       77  status-tparamge  pic xx.
       77  status-param     pic xx.
       77  status-mtordini  pic xx.
       77  status-clienti   pic xx.
       77  status-tgrupgdo  pic xx.

       77  PgmChiamante     pic x(20).
       77  data-oggi        pic 9(8).
       77  como-data        pic 9(8).
       77  counter          pic 9(10).
       77  counter2         pic 9(10).
       77  counter-edit     pic z(10).
       77  como-giacenza    pic s9(8).
       77  como-qta         pic 9(8).
       77  qta-promo        pic 9(8).
       77  idx-b            pic 9(3).

       01  tab-giacenza     occurs 30000.
         05 el-giacenza     pic s9(8).
         05 el-giacenza-tot pic s9(8).
         05 el-prenotata    pic s9(8).
         05 el-evasa        pic s9(8).
         05 el-articolo     pic 9(6).

       77  esubero          pic 9(8).
       77  rimanenza        pic s9(8).
       77  como-valore      pic 9(10).

       01 GdoInUsoFlag      pic x.
           88 GdoInUso      value "S". 
           88 GdoNonInUso   value " ". 
                                      
       01  filler           pic 9.
           88 AumentaGiacenza value 1, false 0.

       01  filler           pic 9.
           88 trovato-blister value 1, false 0.

       LINKAGE SECTION.
       copy "link-tprev-p.def".

      ******************************************************************
       PROCEDURE DIVISION USING tprev-linkage.

       DECLARATIVES.
       PROMOEVA-ERR SECTION.
           use after error procedure on promoeva.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-promoeva
           when "39"
                set errori to true
                display message "File [PROMOEVA] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROMOEVA] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [PROMOEVA] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       TPROMO-ERR SECTION.
           use after error procedure on tpromo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-tpromo
           when "39"
                set errori to true
                display message "File [TPROMO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[TPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [TPROMO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  
                           
       RPROMO-ERR SECTION.
           use after error procedure on rpromo.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-rpromo
           when "39"
                set errori to true
                display message "File [RPROMO] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[RPROMO] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [RPROMO] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

      ***---
       PROGMAG-ERR SECTION.
           use after error procedure on progmag.
           set RecLocked to false.
           set tutto-ok  to true.
           evaluate status-progmag
           when "39"
                set errori to true
                display message "File [PROGMAG] mismatch size!"
                          title titolo
                           icon 3
           when "98"
                set errori to true
                display message "[PROGMAG] Indexed file corrupt!"
                          title titolo
                           icon 3
           when "35"
                display message "File [PROGMAG] inesistente"
                        title = titolo
                        icon 2
                set errori to true
           when "93"
           when "99" set RecLocked to true
           end-evaluate.  

       END DECLARATIVES.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              move spaces to tge-chiave
              read tparamge no lock invalid continue end-read
              accept data-oggi  from century-date
              perform ELABORAZIONE
              perform ELIMINA-FITTIZIE-EVASE-CHIUSE
              perform CLOSE-FILES
              perform EXIT-PGM
           end-if.

      ***---
       INIT.
           call "C$CALLEDBY" using PgmChiamante.
           set  RecLocked to false.
           set  tutto-ok  to true.
           accept versione-evasione from environment "VERSIONE_EVASIONE"
           accept GdoInUsoFlag      from environment "GDO_IN_USO".

      ***---
       OPEN-FILES.
           perform OPEN-IO-PROMOEVA-LOCK.
           if tutto-ok
              close       promoeva
              open output promoeva
              close       promoeva
              perform OPEN-IO-PROMOEVA-LOCK
           end-if.
           if tutto-ok
              open i-o rpromo allowing all
              open input tpromo progmag tmagaz blister articoli
                         mrordini tparamge param tgrupgdo mtordini
           else
              goback
           end-if.

      ***---
       OPEN-IO-PROMOEVA-LOCK.
      *****     perform until 1 = 2
      *****        move "promoeva" to geslock-nome-file
      *****        initialize geslock-messaggio
      *****        string   "Il file delle quantità promo per evasione" 
      *****          x"0d0a""è in uso su altro terminale." delimited size
      *****                 into geslock-messaggio
      *****        end-string

              set tutto-ok  to true
              set RecLocked to false
              open i-o promoeva allowing readers
              if RecLocked
                 set errori to true
              end-if
      *****        if RecLocked
      *****           set errori to true
      *****           move 1     to geslock-v-termina
      *****           move 1     to geslock-v-riprova
      *****           move 0     to geslock-v-ignora
      *****           call   "geslock" using geslock-linkage
      *****           cancel "geslock"
      *****
      *****           evaluate true
      *****           when riprova continue
      *****           when other   display message "Operazione interrotta!"
      *****                                  title titolo
      *****                                   icon 2
      *****                        exit perform
      *****           end-evaluate
      *****        else
      *****           exit perform
      *****        end-if
      *****     end-perform.
           .

      ***---
       ELABORAZIONE.
           perform VALORIZZA-ARTICOLI-FROM-LISTINI.
           if trovato
              perform AGGIORNA-IMP-BOLL-EVASA
              perform ASSEGNA-QUANTITA
              perform STORNA-QUANTITA
              perform AGGIORNA-RIMAN-GIACENZA
           end-if.

      ***---
       VALORIZZA-ARTICOLI-FROM-LISTINI.
      *****     move low-value to pev-rec.
      *****     start promoeva key >= pev-chiave
      *****           invalid continue
      *****       not invalid
      *****           perform until 1 = 2
      *****              read promoeva next at end exit perform end-read
      *****              delete promoeva record
      *****           end-perform
      *****     end-start.
           |Creazione base dati (progressivo e ordinamento nuovi)
           |senza assegnazione della quantità
           set  trovato        to false.
           move low-value      to tpr-rec.
           if vecchia-evasione
              compute como-data = function integer-of-date(data-oggi)
              add tge-gg-inizio-vol-pren-OLD to como-data
              compute como-data = function date-of-integer(como-data)
              start tpromo key >= tpr-k-data-ins
                    invalid
                    close       promoeva
                    open output promoeva
                    close       promoeva
                    open i-o    promoeva
                not invalid
                    perform until 1 = 2
              
                       read tpromo next at end exit perform end-read
              
                       if tpr-fine-volantino >= 20100601   and
                          tpr-ini-volantino  <  como-data  and
                          tpr-fine-volantino >= data-oggi
                          perform VALORIZZA-OK
                       end-if
                    end-perform
              end-start
           else
              start tpromo key >= tpr-k-data-ins
                    invalid
                    close       promoeva
                    open output promoeva
                    close       promoeva
                    open i-o    promoeva
                not invalid
                    perform until 1 = 2 
                       read tpromo next at end exit perform end-read
                       move tpr-gdo  to cli-gdo gdo-codice
                       read tgrupgdo no lock
                            invalid  
                            if Pgmchiamante not = "ricaldin-bat"
                               display message "Cancellare promo " 
                                                tpr-codice 
                                         title titolo
                                          icon 2
                            end-if
                       end-read
                       move gdo-tipocli to cli-tipo                  
                       perform TROVA-PARAMETRO-GDO                    
                       compute como-data = 
                               function integer-of-date(data-oggi)
                       add prm-gg-inizio-vol to como-data
                       compute como-data = 
                               function date-of-integer(como-data)
                       if tpr-fine-volantino >= 20100601   and
                          tpr-ini-volantino  <  como-data  and
                          tpr-fine-volantino >= data-oggi
                          perform VALORIZZA-OK
                       end-if
                    end-perform
              end-start
           end-if.

      ***---
       VALORIZZA-OK.
           set trovato to true.
           move low-value   to rpr-rec.
           move tpr-codice  to rpr-codice.
           start rpromo key >= rpr-chiave
                 invalid continue
             not invalid
                 perform until 1 = 2
                    if Pgmchiamante not = "ricaldin-bat"
                       add 1 to counter
                       add 1 to counter2
                       if counter2 = 50
                          move counter to counter-edit
                          display counter-edit
                             upon link-tprev-handle at column 35
                                                         line 04
                          move 0 to counter2
                       end-if
                    end-if
           
                    read rpromo next
                         at end exit perform
                    end-read
                    if rpr-codice not = tpr-codice
                       exit perform
                    end-if
                    if rpr-qta not = 0          
                       move rpr-articolo to bli-codice
                       read blister no lock 
                            invalid 
                            move 0 to bli-codice
                            perform AGGIUNGI-ARTICOLO
                        not invalid
                            move 0 to idx
                            perform until 1 = 2
                               add 1 to idx
                               if bli-el-articolo(idx) = 0
                                  exit perform
                               end-if
                               move bli-el-articolo(idx) 
                                 to rpr-articolo 
                               perform AGGIUNGI-ARTICOLO
                            end-perform
                       end-read
                    end-if       
                 end-perform
           end-start.

      ***---
       AGGIUNGI-ARTICOLO.
           move rpr-articolo to pev-articolo
                                art-codice.              
           read articoli
                invalid initialize art-descrizione
           end-read.

           move tpr-codice  to pev-tpr-codice. 
           read promoeva no lock key pev-k-art
                invalid 
                move 0 to pev-prog
                initialize pev-dati replacing numeric data by zeroes
                                         alphanumeric data by spaces
                move art-descrizione    to pev-descr-art
                move tpr-data-creazione to pev-data-ins
                move tpr-codice         to pev-tpr-codice
                move tpr-descrizione    to pev-tpr-descrizione
                move tpr-gdo            to pev-gdo
                move rpr-prenotazioni   to pev-rpr-prenotazioni
                if bli-codice not = 0
                   move 1 to pev-n-blister
                   move bli-codice to pev-bli-codice(pev-n-blister)
                end-if
                move tpr-fittizia       to pev-fittizia
                move link-tprev-user    to pev-utente-creazione
                accept pev-data-creazione from century-date
                accept pev-ora-creazione  from time
                perform until 1 = 2
                   add 1 to pev-prog
                   write pev-rec
                         invalid continue
                     not invalid exit perform
                   end-write
                end-perform
            not invalid
                |Nella stessa promo due volte lo stesso 
                |articolo in due blister diversi
                if bli-codice not = 0
                   set trovato-blister to false
                   perform varying idx-b from 1 by 1 
                             until idx-b > 20
                      if bli-codice = pev-bli-codice(idx-b)
                         set trovato-blister to true
                         exit perform
                      end-if
                   end-perform
      *             if idx-b > pev-n-blister and idx-b <= 20
                   if not trovato-blister
                      add 1 to pev-n-blister
                      move bli-codice to pev-bli-codice(pev-n-blister)
                      rewrite pev-rec
                   end-if
                end-if
           end-read.

           |Uso l'articolo come idx in quanto cumulativo
           if el-articolo(pev-articolo) = 0
              move pev-articolo to el-articolo(pev-articolo)
              move low-value    to prg-rec
              move pev-articolo to prg-cod-articolo
              start progmag key >= prg-chiave 
                    invalid continue 
              end-start
              perform until 1 = 2
                 read progmag next at end exit perform end-read
                 if prg-cod-articolo not = pev-articolo
                    exit perform
                 end-if
                 move prg-cod-magazzino to mag-codice
                 read tmagaz no lock 
                      invalid continue 
                  not invalid
                      if mag-per-promo-si
                         if GdoInUso
                            compute como-giacenza = prg-giacenza  -
                                                  ( prg-impegnato -
                                                  ( prg-imp-TRAD +
                                                    prg-imp-GDO ) )
                         else
                            compute como-giacenza = prg-giacenza  -
                                                  ( prg-impegnato -
                                                    prg-imp-master )
                         end-if
                         add como-giacenza 
                          to el-giacenza(pev-articolo)
                             el-giacenza-tot(pev-articolo)
                      end-if
                 end-read
              end-perform
           end-if.      

      ***---
       AGGIORNA-IMP-BOLL-EVASA.
           |Aggiorno i valori di rimanenza e giac-utile.
           move low-value to pev-rec.
           start promoeva key >= pev-chiave 
                 invalid exit paragraph 
           end-start.
           perform until 1 = 2
              read promoeva next no lock at end exit perform end-read
              move 0 to pev-impegnato
              move 0 to pev-evasa
              move 0 to pev-boll
              move low-value to mro-rec
              move pev-articolo   to mro-prg-cod-articolo
              move pev-tpr-codice to mro-promo
              start mrordini key >= mro-k-tprev
                    invalid continue
                not invalid
                    perform until 1 = 2
                       read mrordini next at end exit perform end-read
                       if mro-prg-cod-articolo not = pev-articolo or
                          mro-promo            not = pev-tpr-codice
                          exit perform
                       end-if
                       if mro-chiuso
                          move mro-qta-e to mro-qta
                       end-if
                       add mro-qta   to pev-impegnato
                       add mro-qta-e to pev-evasa
                       add mro-qta-b to pev-boll
                    end-perform
              end-start  
              move pev-tpr-codice to rpr-codice 
              move pev-articolo   to rpr-articolo
      *****        read rpromo no lock 
      *****             |E' UN BLISTER
      *****             invalid               
      *****             move 0 to como-qta
      *****             perform varying idx-b from 1 by 1 
      *****                       until ( idx-b > pev-n-blister or
      *****                               idx-b > 20 )
      *****                move pev-bli-codice(idx-b) to rpr-articolo
      *****                read rpromo no lock invalid continue end-read
      *****                move pev-bli-codice(idx-b) to bli-codice
      *****                read blister no lock invalid continue end-read
      *****                perform varying idx from 1 by 1 
      *****                          until idx > 50
      *****                   if bli-el-articolo(idx) = 0
      *****                      exit perform
      *****                   end-if
      *****                   if bli-el-articolo(idx) = pev-articolo
      *****                      compute como-qta = 
      *****                              como-qta + rpr-qta * bli-el-qta(idx)
      **********                   exit perform
      *****                   end-if
      *****                end-perform
      *****             end-perform   
      *****             move como-qta to rpr-qta
      *****        end-read 
      *****        move rpr-qta to pev-rpr-qta
      *****        if pev-evasa < pev-rpr-qta
      *****           move pev-evasa to pev-prenotata
      *****        else
      *****           move rpr-qta   to pev-prenotata
      *****        end-if
      *****        move pev-evasa to pev-prenotata
      *****        rewrite pev-rec                               
              read rpromo no lock 
                   |E' UN BLISTER
                   invalid move 0 to rpr-qta
              end-read      
              if pev-n-blister > 0     
                 |E' UN BLISTER     
                 move 0       to como-qta
                 move rpr-qta to qta-promo
                 perform varying idx-b from 1 by 1 
                           until ( idx-b > pev-n-blister or
                                   idx-b > 20 )
                    move pev-bli-codice(idx-b) to rpr-articolo
                    read rpromo no lock invalid continue end-read
                    move pev-bli-codice(idx-b) to bli-codice
                    read blister no lock invalid continue end-read
                    perform varying idx from 1 by 1 
                              until idx > 50
                       if bli-el-articolo(idx) = 0
                          exit perform
                       end-if
                       if bli-el-articolo(idx) = pev-articolo
                          compute como-qta = 
                                  como-qta + rpr-qta * bli-el-qta(idx)
      *****                 exit perform
                       end-if
                    end-perform
                 end-perform   
                 add  como-qta  to qta-promo
                 move qta-promo to rpr-qta
              end-if
              
              move rpr-qta to pev-rpr-qta
              if pev-impegnato > pev-rpr-qta
                 move pev-impegnato to pev-rpr-qta
              end-if
              if pev-impegnato < pev-rpr-qta
                 move pev-tpr-codice to tpr-codice    
                 read tpromo no lock
                      invalid continue
                  not invalid
                      compute como-data = function
                              integer-of-date(tpr-ini-volantino)
                      subtract tge-gg-vol-qta from como-data    
                      compute como-data = function
                              date-of-integer(como-data)
                      if como-data < data-oggi
                         move pev-impegnato to pev-rpr-qta
                      end-if
                 end-read                                     
      *           Al cambio di pev-rpr-qta <> da rpr-qta,
      *           aggiornare rpr-qta
                 if pev-rpr-qta not = rpr-qta and pev-rpr-qta not = 0
                    perform until 1 = 2
                       set RecLocked to false
                       read rpromo lock
                            invalid exit perform
                       end-read
                       if RecLocked
                          move "rpromo" to geslock-nome-file
                          initialize geslock-messaggio
                          string "Il record:"
                          x"0d0a""PROMO: " rpr-codice
                          x"0d0a""ARTICOLO: " rpr-articolo
                          x"0d0a""Risulta in uso."
                          x"0d0a""Riprovare?"
                                 delimited size
                            into geslock-messaggio
                          end-string
                          move 0     to geslock-v-termina
                          move 1     to geslock-v-riprova
                          move 1     to geslock-v-ignora
                          call   "geslock" using geslock-linkage
                          cancel "geslock"       
                          if ignora
                             exit perform
                          end-if
                       else
                          move pev-rpr-qta to rpr-qta
                          rewrite rpr-rec
                          exit perform
                       end-if
                    end-perform
                 end-if
              end-if
                                
              if pev-rpr-prenotazioni-si
                 if pev-evasa < pev-rpr-qta
                    move pev-evasa to pev-prenotata
                 else
                    move rpr-qta   to pev-prenotata
                 end-if
                 move pev-evasa to pev-prenotata
              else
                 move 0 to pev-prenotata
              end-if
              rewrite pev-rec


           end-perform.

      ***---
       ASSEGNA-QUANTITA.
           |Assegno le quantità
           move low-value to pev-rec.
           start promoeva key >= pev-chiave 
                 invalid exit paragraph
           end-start.
           perform until 1 = 2
              read promoeva next no lock at end exit perform end-read 
              if pev-rpr-prenotazioni-si
                 |Quantità prenotata minore della quantità
                 |promo piena altrimenti la diminuisco  
                 if pev-evasa >= pev-rpr-qta
                    move pev-rpr-qta to pev-prenotata
                 else
                    if el-giacenza(pev-articolo) > 0
      *****                 if pev-prenotata >= pev-boll
                       if pev-prenotata >= pev-evasa
                          compute rimanenza =
                                  el-giacenza(pev-articolo) -
      *****                          ( pev-prenotata - pev-boll )
                                ( pev-prenotata - pev-evasa )
                       else
                          compute rimanenza =
                                  el-giacenza(pev-articolo) - 
                                  pev-prenotata
                       end-if
                       if rimanenza > 0
                          add rimanenza to pev-prenotata
                          set AumentaGiacenza to true
                          subtract pev-prenotata
                              from el-giacenza(pev-articolo)
                       end-if
                    end-if
                                                      
                    add pev-prenotata to el-prenotata(pev-articolo)
                    add pev-evasa     to el-evasa(pev-articolo)
                    if pev-prenotata > pev-rpr-qta
                       compute  como-valore = pev-prenotata - 
                                              pev-rpr-qta
                       subtract como-valore 
                           from el-prenotata(pev-articolo)
                       add como-valore  to el-giacenza(pev-articolo)
                       move pev-rpr-qta to pev-prenotata
                    end-if
                                                        
                    if AumentaGiacenza
                       set AumentaGiacenza to false
                       if pev-evasa < pev-prenotata
                          add pev-evasa     to el-giacenza(pev-articolo)
                       else
                          add pev-prenotata to el-giacenza(pev-articolo)
                       end-if
                    end-if
                 end-if
                
                 rewrite pev-rec
              end-if
           end-perform.

      ***---
       STORNA-QUANTITA.
           |Se la qta prenotata è maggiore della giacenza
           |svuoto la qta prenotata partendo dal fondo
           perform varying rpr-articolo from 1 by 1
                     until rpr-articolo > 20000
              if el-giacenza-tot(rpr-articolo) < 0
                 move 0 to el-giacenza-tot(rpr-articolo)
              end-if
              if ( el-prenotata(rpr-articolo) -
                   el-evasa(rpr-articolo) )   >
                   el-giacenza-tot(rpr-articolo)
                 move high-value   to pev-rec
                 move rpr-articolo to pev-articolo
                 start promoeva key <= pev-chiave
                       invalid continue
                   not invalid
                       compute esubero = el-prenotata(rpr-articolo) -
                                      el-giacenza-tot(rpr-articolo) 
                       perform until 1 = 2
                          read promoeva previous 
                               at end exit perform 
                          end-read
                          if pev-articolo not = rpr-articolo
                             exit perform
                          end-if
                          if esubero = 0
                             exit perform
                          end-if
                          if esubero > pev-prenotata
                             subtract  pev-prenotata from esubero
                             move 0 to pev-prenotata
                          else
                             subtract  esubero from pev-prenotata
                             move 0 to esubero
                          end-if
                          rewrite pev-rec
                       end-perform
                 end-start
              end-if
           end-perform.

      ***---
       AGGIORNA-RIMAN-GIACENZA. 
           |Aggiorno i valori di rimanenza e giac-utile.
           move low-value to pev-rec.
           start promoeva key >= pev-chiave 
                 invalid exit paragraph
           end-start.
           perform until 1 = 2
              read promoeva next no lock at end exit perform end-read
              compute pev-giac-utile = pev-prenotata - pev-evasa
              if pev-giac-utile < 0
                 move 0 to pev-giac-utile
              end-if
              compute pev-rimanenza  = pev-prenotata - pev-boll
              if pev-rimanenza < 0
                 move 0 to pev-rimanenza
              end-if
              rewrite pev-rec
           end-perform.

      ***---
       ELIMINA-FITTIZIE-EVASE-CHIUSE.
           move low-value to pev-rec.
           set pev-fittizia-si to true.
           start promoeva key >= pev-k-fittizia
                 invalid continue
             not invalid
                 perform until 1 = 2
                    read promoeva next at end exit perform end-read
                    if pev-boll >= pev-prenotata and
                       pev-boll > 0
                       delete promoeva record
                    else
                       move pev-tpr-codice to tpr-codice
                       read tpromo no lock
                            invalid continue
                        not invalid
                            move tpr-chiave-master to mto-chiave
                            read mtordini no lock
                                 invalid continue
                             not invalid
                                 if mto-chiuso
                                    delete promoeva record
                                 end-if
                            end-read
                       end-read
                    end-if
                 end-perform
           end-start.

      ***---
       CLOSE-FILES.
           close tpromo rpromo promoeva progmag tmagaz blister articoli
                 mrordini tparamge tgrupgdo mtordini.

      ***---
       EXIT-PGM.
           goback.

      ***---
       PARAGRAFO-COPY.
           copy "trova-parametro.cpy".
